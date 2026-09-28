/**
 * @file TierNode - one cloud / fog / edge node of the AtomVM continuum.
 *
 * A TierNode boots a real AtomVM runtime, proves it is alive by running its
 * tier witness program, and only then starts listening. Over HTTP it serves:
 *
 *   GET  /health            liveness + counters
 *   POST /ingest            FIRST MILE : data travelling up   (browser/device -> edge -> fog -> cloud)
 *   GET  /deliver/:digest   LAST MILE  : data travelling down (cloud -> fog -> edge -> browser)
 *
 * Every hop executes the tier's AtomVM program through AtomVMSwarmCluster /
 * AtomVMProcessBroker (a receipted actuation) and seals a receipt linked to the
 * previous hop, so the consumer can verify the whole journey with
 * receipt-chain.mjs. Nothing is acknowledged before the cloud (system of
 * record) acknowledged it, and nothing an upstream returns is trusted before
 * it is re-verified.
 */
import { createServer } from 'node:http';
import { readFile } from 'node:fs/promises';
import { createHash } from 'node:crypto';
import { AtomVMNodeRuntime } from '../node-runtime.mjs';
import { AtomVMSwarmCluster } from '../swarm-cluster.mjs';
import { AtomVMProcessBroker } from '../process-broker.mjs';
import { parseAvm } from '../avm-packer.mjs';
import { ROOT_TIER, TIER_LEVEL, assertTier, upstreamOf, witnessMarker } from './tiers.mjs';
import { canonical, payloadDigestOf, sealReceipt, verifyChain } from './receipt-chain.mjs';

const MAX_BODY_BYTES = 1_000_000;
const DIGEST_PATTERN = /^[0-9a-f]{64}$/;
const SERVED_TIERS = Object.freeze(['edge', 'fog', 'cloud']);

export class TierRefusal extends Error {
  constructor(code, message, status = 422, details = {}) {
    super(`[${code}] ${message}`);
    this.name = 'TierRefusal';
    this.code = code;
    this.status = status;
    this.details = Object.freeze({ ...details });
  }
}

const sha256 = value => createHash('sha256').update(value).digest('hex');

async function readBody(req) {
  const chunks = [];
  let size = 0;
  for await (const chunk of req) {
    size += chunk.length;
    if (size > MAX_BODY_BYTES)
      throw new TierRefusal('BODY_TOO_LARGE_REFUSED', `body exceeds ${MAX_BODY_BYTES} bytes`, 413);
    chunks.push(chunk);
  }
  const text = Buffer.concat(chunks).toString('utf8');
  try {
    return JSON.parse(text);
  } catch {
    throw new TierRefusal('INVALID_JSON_REFUSED', 'request body is not valid JSON', 400);
  }
}

function sendJson(res, status, body) {
  const text = JSON.stringify(body);
  res.writeHead(status, {
    'content-type': 'application/json',
    'content-length': Buffer.byteLength(text),
  });
  res.end(text);
}

/** @typedef {'Unbooted'|'Bootstrapping'|'Alive'|'Refused'|'Stopped'} TierNodeState */

export class TierNode {
  #server = null;
  #cluster = null;
  #broker = null;
  #store = new Map();

  /**
   * @param {object} options
   * @param {'edge'|'fog'|'cloud'} options.tier
   * @param {string} options.avmPath - tier witness application (.avm)
   * @param {string} [options.nodeId]
   * @param {string} [options.upstreamUrl] - required for edge/fog, forbidden for cloud
   * @param {string} [options.atomvmBinary] - explicit binary; default: native AtomVM on PATH, else bundled wasm launcher
   */
  constructor({
    tier,
    avmPath,
    nodeId = `${tier}-1`,
    upstreamUrl = null,
    atomvmBinary,
    host = '127.0.0.1',
    port = 0,
    allowedOrigins = [],
    clock = () => new Date().toISOString(),
    executionTimeoutMs = 10_000,
    upstreamTimeoutMs = 15_000,
    maxRecords = 10_000,
  } = {}) {
    assertTier(tier);
    if (!SERVED_TIERS.includes(tier)) {
      throw new TypeError('the browser tier is a client (see browser-client.mjs), not a TierNode');
    }
    if (typeof avmPath !== 'string' || avmPath.length === 0)
      throw new TypeError('avmPath is required');
    this.tier = tier;
    this.level = TIER_LEVEL[tier];
    this.nodeId = nodeId;
    this.avmPath = avmPath;
    this.upstreamUrl = upstreamUrl;
    this.atomvmBinary = atomvmBinary;
    this.host = host;
    this.port = port;
    /** Browser origins allowed to call this node (default deny). Needed for the browser tier. */
    this.allowedOrigins = Object.freeze([...allowedOrigins]);
    this.clock = clock;
    this.executionTimeoutMs = executionTimeoutMs;
    this.upstreamTimeoutMs = upstreamTimeoutMs;
    this.maxRecords = maxRecords;
    /** @type {TierNodeState} */
    this.state = 'Unbooted';
    this.backend = null;
    this.bootstrap = null;
    this.counters = {
      ingests: 0,
      duplicates: 0,
      deliveries: 0,
      cacheHits: 0,
      upstreamCalls: 0,
      executions: 0,
      refusals: 0,
    };
  }

  get url() {
    return this.#server ? `http://${this.host}:${this.#server.address().port}` : null;
  }

  /** Receipted AtomVM executions performed by this node (cluster receipts). */
  executionReceipts() {
    return this.#cluster ? this.#cluster.snapshot().receipts : [];
  }

  verifyExecutionReceipt(receipt) {
    return this.#cluster.verifyReceipt(receipt);
  }

  hasRecord(digest) {
    return this.#store.has(digest);
  }

  recordCount() {
    return this.#store.size;
  }

  /**
   * Bootstrap: topology law -> AVM validity -> runtime boot -> witness alive
   * as THIS tier -> upstream alive as the NEXT tier -> listen. Any failure
   * leaves the node 'Refused' and closed; it never serves half-booted.
   */
  async start() {
    if (this.state !== 'Unbooted') {
      throw new TierRefusal(
        'BOOTSTRAP_STATE_REFUSED',
        `cannot start from state ${this.state}`,
        409
      );
    }
    this.state = 'Bootstrapping';
    try {
      const isRoot = this.tier === ROOT_TIER;
      if (isRoot && this.upstreamUrl) {
        throw new TierRefusal(
          'ROOT_HAS_UPSTREAM_REFUSED',
          `${ROOT_TIER} is the root of trust and cannot have an upstream`
        );
      }
      if (!isRoot && !this.upstreamUrl) {
        throw new TierRefusal(
          'UPSTREAM_REQUIRED_REFUSED',
          `${this.tier} requires an upstream ${upstreamOf(this.tier)} node`
        );
      }

      let avmBytes;
      try {
        avmBytes = await readFile(this.avmPath);
      } catch (error) {
        throw new TierRefusal(
          'AVM_NOT_FOUND_REFUSED',
          `cannot read ${this.avmPath}: ${error.message}`
        );
      }
      try {
        parseAvm(new Uint8Array(avmBytes));
      } catch (error) {
        throw new TierRefusal('AVM_INVALID_REFUSED', `${this.avmPath}: ${error.message}`);
      }

      const runtime = new AtomVMNodeRuntime({
        atomvmBinary: this.atomvmBinary,
        log: () => {},
        errorLog: () => {},
      });
      let boot;
      try {
        await runtime.load();
        boot = await runtime.execute(this.avmPath);
      } catch (error) {
        throw new TierRefusal('RUNTIME_BOOT_REFUSED', error.message, 503, { cause: error.message });
      }
      const marker = witnessMarker(this.tier);
      if (!`${boot.stdout}\n${boot.stderr}`.includes(marker)) {
        throw new TierRefusal(
          'TIER_IDENTITY_MISMATCH_REFUSED',
          `witness did not report ${marker}; wrong tier program or incorrect computation`,
          422,
          { stdout: boot.stdout }
        );
      }
      this.backend = runtime.backend;
      const binary = runtime.atomvmPath;
      runtime.destroy();

      this.#cluster = new AtomVMSwarmCluster({ clusterId: `continuum.${this.nodeId}` });
      this.#cluster.admitSwarm({
        id: this.nodeId,
        gatewayNode: `${this.nodeId}-gateway`,
        cookieRef: `authority://atomvm/continuum/${this.nodeId}`,
        endpoint: `atomvm://${this.nodeId}`,
        metadata: { tier: this.tier },
      });
      this.#broker = new AtomVMProcessBroker({
        atomvmBinary: binary,
        runtimeRef: this.backend,
        timeoutMs: this.executionTimeoutMs,
        swarms: { [this.nodeId]: { avmPath: this.avmPath, expectedMarker: marker } },
      });

      if (this.upstreamUrl) await this.#verifyUpstream();
      await this.#listen();

      this.bootstrap = Object.freeze({
        tier: this.tier,
        nodeId: this.nodeId,
        backend: this.backend,
        marker,
        avmDigest: sha256(avmBytes),
        upstream: this.upstreamUrl,
      });
      this.bootstrap = Object.freeze({
        ...this.bootstrap,
        digest: sha256(canonical(this.bootstrap)),
      });
      this.state = 'Alive';
      return this.bootstrap;
    } catch (error) {
      this.state = 'Refused';
      await this.#close();
      throw error;
    }
  }

  async stop() {
    await this.#close();
    this.state = 'Stopped';
  }

  // ---------------------------------------------------------------- wiring

  async #verifyUpstream() {
    const expected = upstreamOf(this.tier);
    const { status, json } = await this.#upstream('GET', '/health');
    if (status !== 200 || json?.state !== 'Alive') {
      throw new TierRefusal(
        'UPSTREAM_NOT_ALIVE_REFUSED',
        `upstream ${this.upstreamUrl} is not Alive`,
        503,
        { status }
      );
    }
    if (json.tier !== expected) {
      throw new TierRefusal(
        'UPSTREAM_TIER_MISMATCH_REFUSED',
        `${this.tier} must sit under ${expected}, but upstream reports ${json.tier}`
      );
    }
  }

  async #listen() {
    this.#server = createServer((req, res) => {
      this.#handle(req, res).catch(error => this.#fail(res, error));
    });
    this.#server.requestTimeout = 30_000;
    this.#server.headersTimeout = 10_000;
    await new Promise((resolve, reject) => {
      this.#server.once('error', reject);
      this.#server.listen(this.port, this.host, resolve);
    });
  }

  async #close() {
    const server = this.#server;
    this.#server = null;
    if (!server) return;
    server.closeAllConnections?.();
    await new Promise(resolve => server.close(resolve));
  }

  #fail(res, error) {
    this.counters.refusals++;
    const refusal =
      error instanceof TierRefusal
        ? error
        : new TierRefusal('INTERNAL_REFUSED', error?.message ?? String(error), 500);
    if (!res.headersSent) {
      // An unread request body poisons a keep-alive connection; close it after answering.
      if (refusal.status === 413) res.setHeader('connection', 'close');
      sendJson(res, refusal.status, {
        error: refusal.code,
        message: refusal.message,
        details: refusal.details,
      });
    } else {
      res.destroy();
    }
  }

  async #handle(req, res) {
    const { pathname } = new URL(req.url, 'http://tier.local');
    this.#applyCors(req, res);
    if (req.method === 'OPTIONS') {
      res.writeHead(res.getHeader('access-control-allow-origin') ? 204 : 403, {
        'content-length': 0,
      });
      return res.end();
    }
    if (req.method === 'GET' && pathname === '/health') return sendJson(res, 200, this.#health());
    if (req.method === 'POST' && pathname === '/ingest')
      return sendJson(res, 200, await this.#ingest(await readBody(req)));
    if (req.method === 'GET' && pathname.startsWith('/deliver/')) {
      const digest = pathname.slice('/deliver/'.length);
      if (!DIGEST_PATTERN.test(digest))
        throw new TierRefusal(
          'INVALID_DIGEST_REFUSED',
          'digest must be 64 lowercase hex chars',
          400
        );
      return sendJson(res, 200, await this.#deliver(digest));
    }
    throw new TierRefusal('ROUTE_NOT_FOUND_REFUSED', `${req.method} ${pathname}`, 404);
  }

  /**
   * Default-deny CORS. Only listed origins get access-control-* headers; under
   * COEP (required for AtomVM WASM) the browser also needs CORP on the response.
   */
  #applyCors(req, res) {
    const origin = req.headers.origin;
    res.setHeader('vary', 'Origin');
    if (!origin || !this.allowedOrigins.includes(origin)) return;
    res.setHeader('access-control-allow-origin', origin);
    res.setHeader('cross-origin-resource-policy', 'cross-origin');
    res.setHeader('access-control-allow-methods', 'GET, POST, OPTIONS');
    res.setHeader('access-control-allow-headers', 'content-type');
    res.setHeader('access-control-max-age', '600');
  }

  #health() {
    return {
      tier: this.tier,
      level: this.level,
      nodeId: this.nodeId,
      state: this.state,
      backend: this.backend,
      bootstrapDigest: this.bootstrap?.digest ?? null,
      records: this.#store.size,
      counters: { ...this.counters },
    };
  }

  async #upstream(method, path, body) {
    this.counters.upstreamCalls++;
    let response;
    try {
      response = await fetch(new URL(path, this.upstreamUrl), {
        method,
        headers: body === undefined ? {} : { 'content-type': 'application/json' },
        body: body === undefined ? undefined : JSON.stringify(body),
        signal: AbortSignal.timeout(this.upstreamTimeoutMs),
      });
    } catch (error) {
      throw new TierRefusal(
        'UPSTREAM_UNAVAILABLE_REFUSED',
        `upstream ${this.upstreamUrl} unreachable: ${error.cause?.code ?? error.message}`,
        502
      );
    }
    return { status: response.status, json: await response.json().catch(() => null) };
  }

  /** One receipted AtomVM execution of this tier's witness. */
  async #execute(phase, payloadDigest) {
    const intent = this.#cluster.constructIntent({
      sourceId: this.nodeId,
      targetId: this.nodeId,
      operation: 'atomvm.execute',
      payload: { phase, payloadDigest },
    });
    const receipt = await this.#cluster.actuate(intent, this.#broker);
    this.counters.executions++;
    if (receipt.status !== 'ALIVE') {
      throw new TierRefusal(
        'TIER_EXECUTION_BLOCKED_REFUSED',
        receipt.error?.message ?? 'AtomVM execution blocked',
        503
      );
    }
    return receipt.receiptDigest;
  }

  // ------------------------------------------------------------ first mile

  async #ingest(body) {
    if (!body || typeof body !== 'object' || Array.isArray(body)) {
      throw new TierRefusal('INVALID_INGEST_REFUSED', 'body must be an object');
    }
    const { payload, origin, payloadDigest: claimed, chain = [] } = body;
    if (typeof origin !== 'string' || origin.length === 0 || origin.length > 256) {
      throw new TierRefusal(
        'INVALID_INGEST_REFUSED',
        'origin must be a non-empty string of at most 256 chars'
      );
    }
    if (payload === undefined)
      throw new TierRefusal('INVALID_INGEST_REFUSED', 'payload is required');
    if (!Array.isArray(chain))
      throw new TierRefusal('INVALID_INGEST_REFUSED', 'chain must be an array');

    let digest;
    try {
      digest = await payloadDigestOf(payload);
    } catch (error) {
      throw new TierRefusal(
        'INVALID_INGEST_REFUSED',
        `payload is not canonicalisable: ${error.message}`
      );
    }
    if (claimed !== undefined && claimed !== digest) {
      throw new TierRefusal(
        'DIGEST_MISMATCH_REFUSED',
        'payload does not hash to the digest the sender attested (corrupted in transit)'
      );
    }
    if (chain.length > 0) {
      const verdict = await verifyChain(chain, { payloadDigest: digest, phase: 'ingest' });
      if (!verdict.valid)
        throw new TierRefusal('CHAIN_INVALID_REFUSED', verdict.reason, 422, {
          chainCode: verdict.code,
        });
      const previous = TIER_LEVEL[chain.at(-1).tier];
      if (previous !== this.level - 1) {
        throw new TierRefusal(
          'TIER_SKIP_REFUSED',
          `${this.tier} accepts ingest only from ${Object.keys(TIER_LEVEL)[this.level - 1]}, not ${chain.at(-1).tier}`
        );
      }
    }

    const existing = this.#store.get(digest);
    if (existing) {
      this.counters.duplicates++;
      return { status: 'duplicate', tier: this.tier, digest, chain: existing.ingestChain };
    }
    if (this.#store.size >= this.maxRecords) {
      throw new TierRefusal(
        'STORE_FULL_REFUSED',
        `record store is at capacity (${this.maxRecords})`,
        507
      );
    }

    this.counters.ingests++;
    const executionDigest = await this.#execute('ingest', digest);
    const receipt = await sealReceipt({
      tier: this.tier,
      nodeId: this.nodeId,
      phase: 'ingest',
      payloadDigest: digest,
      prev: chain.at(-1)?.digest ?? null,
      executionDigest,
      at: this.clock(),
    });

    let finalChain = [...chain, receipt];
    if (this.upstreamUrl) {
      const { status, json } = await this.#upstream('POST', '/ingest', {
        payload,
        origin,
        payloadDigest: digest,
        chain: finalChain,
      });
      if (status !== 200 || !Array.isArray(json?.chain)) {
        throw new TierRefusal(
          'UPSTREAM_REFUSED',
          `upstream did not acknowledge ingest (HTTP ${status})`,
          502,
          { upstream: json }
        );
      }
      // Trust nothing upstream returns: it must extend exactly what we sent.
      const verdict = await verifyChain(json.chain, { payloadDigest: digest, phase: 'ingest' });
      // A fresh store must extend exactly what we sent; an upstream that already
      // held this payload (via another path) answers 'duplicate' with its own chain.
      const extendsOurs = json.chain[finalChain.length - 1]?.digest === receipt.digest;
      if (!verdict.valid || (json.status !== 'duplicate' && !extendsOurs)) {
        throw new TierRefusal(
          'UPSTREAM_TAMPER_REFUSED',
          'upstream acknowledgement does not extend our receipt chain',
          502
        );
      }
      finalChain = json.chain;
    }

    this.#store.set(digest, { payload, origin, payloadDigest: digest, ingestChain: finalChain });
    return { status: 'stored', tier: this.tier, digest, chain: finalChain };
  }

  // ------------------------------------------------------------- last mile

  async #deliver(digest) {
    this.counters.deliveries++;
    let record = this.#store.get(digest);
    let chain = [];
    let servedBy = this.tier;

    if (record) {
      if (this.tier !== ROOT_TIER) this.counters.cacheHits++;
    } else if (this.upstreamUrl) {
      const { status, json } = await this.#upstream('GET', `/deliver/${digest}`);
      if (status === 404) throw new TierRefusal('NOT_FOUND_REFUSED', `no record ${digest}`, 404);
      if (status !== 200 || !json?.record || !Array.isArray(json?.chain)) {
        throw new TierRefusal(
          'UPSTREAM_REFUSED',
          `upstream did not deliver (HTTP ${status})`,
          502,
          { upstream: json }
        );
      }
      if ((await payloadDigestOf(json.record.payload)) !== digest) {
        throw new TierRefusal(
          'UPSTREAM_TAMPER_REFUSED',
          'upstream returned a payload that does not hash to the requested digest',
          502
        );
      }
      const verdict = await verifyChain(json.chain, { payloadDigest: digest, phase: 'deliver' });
      if (!verdict.valid)
        throw new TierRefusal('UPSTREAM_TAMPER_REFUSED', verdict.reason, 502, {
          chainCode: verdict.code,
        });
      if (TIER_LEVEL[json.chain.at(-1).tier] !== this.level + 1) {
        throw new TierRefusal(
          'TIER_SKIP_REFUSED',
          `${this.tier} accepts delivery only from the adjacent upstream tier`,
          502
        );
      }
      record = { ...json.record, payloadDigest: digest };
      chain = json.chain;
      servedBy = json.servedBy;
      if (this.#store.size < this.maxRecords) this.#store.set(digest, record); // warm this tier for the next consumer
    } else {
      throw new TierRefusal('NOT_FOUND_REFUSED', `no record ${digest}`, 404);
    }

    const executionDigest = await this.#execute('deliver', digest);
    const receipt = await sealReceipt({
      tier: this.tier,
      nodeId: this.nodeId,
      phase: 'deliver',
      payloadDigest: digest,
      prev: chain.at(-1)?.digest ?? null,
      executionDigest,
      at: this.clock(),
    });
    return {
      record: {
        payload: record.payload,
        origin: record.origin,
        payloadDigest: digest,
        ingestChain: record.ingestChain,
      },
      chain: [...chain, receipt],
      servedBy,
    };
  }
}
