/**
 * @file Browser tier of the AtomVM continuum. Browser-only (uses document,
 * fetch, WebCrypto); the counterpart of TierNode for the low end of the chain.
 *
 * The browser boots a REAL AtomVM (the WASM build) inside the page, proves it is
 * alive by running its tier witness, then acts as the producer of the first
 * mile (POST to the edge) and the consumer of the last mile (GET from the edge),
 * verifying every receipt in the page with WebCrypto.
 *
 * The WASM build runs one application per page load (window.Module is a
 * singleton), so the browser attests its runtime once at bootstrap and every
 * later receipt carries that boot digest as its executionDigest.
 */
import { TIER_LEVEL, witnessMarker } from './tiers.mjs';
import {
  canonical,
  payloadDigestOf,
  sealReceipt,
  sha256Hex,
  verifyChain,
} from './receipt-chain.mjs';

const EMSCRIPTEN_WASM_NAME = 'AtomVM.wasm';

export class ContinuumClientError extends Error {
  constructor(code, message, details = {}) {
    super(`[${code}] ${message}`);
    this.name = 'ContinuumClientError';
    this.code = code;
    this.details = details;
  }
}

/**
 * Run an .avm on the AtomVM WASM build in this page.
 * @returns {Promise<{output: string[], returnValue: string}>}
 */
export async function runAvmInBrowser({ scriptUrl, wasmUrl, avmUrl, timeoutMs = 20_000 }) {
  if (typeof SharedArrayBuffer === 'undefined' || !globalThis.crossOriginIsolated) {
    throw new ContinuumClientError(
      'CROSS_ORIGIN_ISOLATION_REFUSED',
      'AtomVM WASM needs SharedArrayBuffer (COOP/COEP headers)'
    );
  }
  if (globalThis.Module) {
    throw new ContinuumClientError(
      'ATOMVM_ALREADY_BOOTED_REFUSED',
      'AtomVM WASM runs once per page load'
    );
  }
  const response = await fetch(avmUrl);
  if (!response.ok)
    throw new ContinuumClientError('AVM_FETCH_REFUSED', `HTTP ${response.status} for ${avmUrl}`);
  const avmBytes = new Uint8Array(await response.arrayBuffer());

  return new Promise((resolve, reject) => {
    const output = [];
    const timer = setTimeout(
      () =>
        reject(
          new ContinuumClientError('ATOMVM_TIMEOUT_REFUSED', `no result within ${timeoutMs} ms`, {
            output,
          })
        ),
      timeoutMs
    );
    const settle = (error, value) => {
      clearTimeout(timer);
      if (error) reject(error);
      else resolve(value);
    };
    globalThis.Module = {
      arguments: ['/tier.avm'],
      locateFile: (path, directory) =>
        path === EMSCRIPTEN_WASM_NAME ? wasmUrl : `${directory}${path}`,
      print: line => {
        output.push(line);
        const match = /^Return value: (.*)$/.exec(line);
        if (match) settle(null, { output, returnValue: match[1] });
      },
      printErr: line => output.push(line),
      preRun: [() => globalThis.FS.createDataFile('/', 'tier.avm', avmBytes, true, false)],
      onAbort: reason =>
        settle(new ContinuumClientError('ATOMVM_ABORT_REFUSED', String(reason), { output })),
    };
    const script = document.createElement('script');
    script.src = scriptUrl;
    script.onerror = () =>
      settle(new ContinuumClientError('ATOMVM_SCRIPT_LOAD_REFUSED', `cannot load ${scriptUrl}`));
    document.head.append(script);
  });
}

export class BrowserTier {
  /**
   * @param {object} options
   * @param {string} options.edgeUrl
   * @param {{scriptUrl: string, wasmUrl: string, avmUrl: string}} options.assets
   */
  constructor({ edgeUrl, assets, nodeId = 'browser-1', clock = () => new Date().toISOString() }) {
    this.tier = 'browser';
    this.level = TIER_LEVEL.browser;
    this.edgeUrl = edgeUrl;
    this.assets = assets;
    this.nodeId = nodeId;
    this.clock = clock;
    this.state = 'Unbooted';
    this.bootDigest = null;
    this.bootOutput = null;
  }

  /** Boot AtomVM in the page, prove the browser witness, and confirm the edge is Alive. */
  async bootstrap() {
    if (this.state !== 'Unbooted')
      throw new ContinuumClientError('BOOTSTRAP_STATE_REFUSED', `state is ${this.state}`);
    this.state = 'Bootstrapping';
    try {
      const boot = await runAvmInBrowser(this.assets);
      const marker = witnessMarker('browser');
      if (boot.returnValue !== 'ok' || !boot.output.includes(marker)) {
        throw new ContinuumClientError('TIER_IDENTITY_MISMATCH_REFUSED', `expected ${marker}`, {
          output: boot.output,
        });
      }
      const health = await this.#edge('GET', '/health');
      if (health.status !== 200 || health.json?.state !== 'Alive' || health.json?.tier !== 'edge') {
        throw new ContinuumClientError(
          'EDGE_NOT_ALIVE_REFUSED',
          `edge at ${this.edgeUrl} is not an Alive edge`,
          { health: health.json }
        );
      }
      this.bootOutput = boot.output;
      this.bootDigest = await sha256Hex(canonical({ nodeId: this.nodeId, output: boot.output }));
      this.state = 'Alive';
      return { marker, bootDigest: this.bootDigest, edge: health.json };
    } catch (error) {
      this.state = 'Refused';
      throw error;
    }
  }

  /** FIRST MILE: browser -> edge -> fog -> cloud. Resolves only once the cloud acknowledged. */
  async ingest(payload, { origin = this.nodeId } = {}) {
    this.#requireAlive();
    const digest = await payloadDigestOf(payload);
    const receipt = await sealReceipt({
      tier: 'browser',
      nodeId: this.nodeId,
      phase: 'ingest',
      payloadDigest: digest,
      prev: null,
      executionDigest: this.bootDigest,
      at: this.clock(),
    });
    const { status, json } = await this.#edge('POST', '/ingest', {
      payload,
      origin,
      payloadDigest: digest,
      chain: [receipt],
    });
    if (status !== 200) {
      throw new ContinuumClientError('FIRST_MILE_REFUSED', `edge answered HTTP ${status}`, {
        response: json,
      });
    }
    const verdict = await verifyChain(json.chain, { payloadDigest: digest, phase: 'ingest' });
    if (!verdict.valid)
      throw new ContinuumClientError('FIRST_MILE_CHAIN_INVALID_REFUSED', verdict.reason, {
        code: verdict.code,
      });
    if (json.chain[0].digest !== receipt.digest) {
      throw new ContinuumClientError(
        'FIRST_MILE_CHAIN_INVALID_REFUSED',
        'acknowledgement does not start at our receipt'
      );
    }
    if (json.chain.at(-1).tier !== 'cloud') {
      throw new ContinuumClientError(
        'FIRST_MILE_NOT_DURABLE_REFUSED',
        'the cloud (system of record) did not acknowledge'
      );
    }
    return { digest, chain: json.chain, tiers: verdict.tiers, status: json.status };
  }

  /** LAST MILE: cloud -> fog -> edge -> browser. Verifies payload + journey before returning it. */
  async fetchDelivery(digest) {
    this.#requireAlive();
    const { status, json } = await this.#edge('GET', `/deliver/${digest}`);
    if (status === 404) throw new ContinuumClientError('NOT_FOUND_REFUSED', `no record ${digest}`);
    if (status !== 200)
      throw new ContinuumClientError('LAST_MILE_REFUSED', `edge answered HTTP ${status}`, {
        response: json,
      });

    if ((await payloadDigestOf(json.record.payload)) !== digest) {
      throw new ContinuumClientError(
        'PAYLOAD_TAMPER_REFUSED',
        'delivered payload does not hash to the requested digest'
      );
    }
    const delivery = await verifyChain(json.chain, { payloadDigest: digest, phase: 'deliver' });
    if (!delivery.valid)
      throw new ContinuumClientError('LAST_MILE_CHAIN_INVALID_REFUSED', delivery.reason, {
        code: delivery.code,
      });
    if (json.chain.at(-1).tier !== 'edge') {
      throw new ContinuumClientError(
        'TIER_SKIP_REFUSED',
        'delivery must reach the browser through the edge'
      );
    }
    const provenance = await verifyChain(json.record.ingestChain, {
      payloadDigest: digest,
      phase: 'ingest',
    });
    if (!provenance.valid || json.record.ingestChain.at(-1).tier !== 'cloud') {
      throw new ContinuumClientError(
        'PROVENANCE_REFUSED',
        provenance.reason ?? 'record was never acknowledged by the cloud'
      );
    }

    const receipt = await sealReceipt({
      tier: 'browser',
      nodeId: this.nodeId,
      phase: 'deliver',
      payloadDigest: digest,
      prev: json.chain.at(-1).digest,
      executionDigest: this.bootDigest,
      at: this.clock(),
    });
    const chain = [...json.chain, receipt];
    const complete = await verifyChain(chain, { payloadDigest: digest, phase: 'deliver' });
    return {
      payload: json.record.payload,
      origin: json.record.origin,
      digest,
      servedBy: json.servedBy,
      chain,
      tiers: complete.tiers,
      ingestTiers: provenance.tiers,
    };
  }

  #requireAlive() {
    if (this.state !== 'Alive')
      throw new ContinuumClientError(
        'NOT_ALIVE_REFUSED',
        `browser tier is ${this.state}; bootstrap() first`
      );
  }

  async #edge(method, path, body) {
    const response = await fetch(new URL(path, this.edgeUrl), {
      method,
      headers: body === undefined ? {} : { 'content-type': 'application/json' },
      body: body === undefined ? undefined : JSON.stringify(body),
    });
    return { status: response.status, json: await response.json().catch(() => null) };
  }
}
