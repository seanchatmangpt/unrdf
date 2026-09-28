/**
 * Chicago-school end-to-end suite: cloud <-> fog <-> edge on real AtomVM.
 *
 * Collaborators are real: three TierNodes on loopback sockets, each running its
 * own AtomVM tier witness (native binary if installed, otherwise the bundled
 * WebAssembly build) through the swarm cluster / process broker. Assertions are
 * on observable state (HTTP answers, stores, receipts, counters), never on
 * interactions. The only "fakes" are impostor HTTP servers used as falsifiers.
 */
import { afterEach, beforeAll, afterAll, describe, expect, it } from 'vitest';
import { mkdtempSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { TierNode } from '../../src/continuum/tier-node.mjs';
import { payloadDigestOf, sealReceipt, verifyChain } from '../../src/continuum/receipt-chain.mjs';
import { witnessMarker } from '../../src/continuum/tiers.mjs';
import { httpServer, json, request, tierAvm, tierFleet } from './helpers.mjs';

const TEST_TIMEOUT = 60_000;

/** A real device-side client: produces a first-mile request with a correct attestation. */
async function deviceIngest(edge, payload, extra = {}) {
  return request(edge.url, 'POST', '/ingest', {
    payload,
    origin: 'sensor-17',
    payloadDigest: await payloadDigestOf(payload),
    ...extra,
  });
}

describe('bootstrapping', () => {
  const fleet = tierFleet();
  afterEach(() => fleet.stopAll());

  it(
    'brings cloud, fog and edge Alive in dependency order on real AtomVM',
    async () => {
      const { cloud, fog, edge } = await fleet.bootstrapAll();

      for (const [node, level] of [
        [cloud, 3],
        [fog, 2],
        [edge, 1],
      ]) {
        const health = await request(node.url, 'GET', '/health');
        expect(health.status).toBe(200);
        expect(health.json).toMatchObject({ tier: node.tier, level, state: 'Alive' });
        expect(['native', 'wasm-node']).toContain(health.json.backend);
        expect(health.json.bootstrapDigest).toMatch(/^[0-9a-f]{64}$/);
        expect(node.bootstrap.marker).toBe(witnessMarker(node.tier));
      }
      const digests = new Set([cloud, fog, edge].map(node => node.bootstrap.digest));
      expect(digests.size).toBe(3); // each tier attests a distinct identity
      expect(edge.bootstrap.upstream).toBe(fog.url);
      expect(fog.bootstrap.upstream).toBe(cloud.url);
      expect(cloud.bootstrap.upstream).toBeNull();
    },
    TEST_TIMEOUT
  );

  it(
    'refuses to bootstrap out of order: an edge cannot start before its fog exists',
    async () => {
      const edge = new TierNode({
        tier: 'edge',
        avmPath: tierAvm('edge'),
        upstreamUrl: 'http://127.0.0.1:1',
      });
      await expect(edge.start()).rejects.toMatchObject({ code: 'UPSTREAM_UNAVAILABLE_REFUSED' });
      expect(edge.state).toBe('Refused');
      expect(edge.url).toBeNull(); // never listened
    },
    TEST_TIMEOUT
  );

  it(
    'refuses a tier stacked on the wrong tier (edge directly on cloud skips fog)',
    async () => {
      const cloud = await fleet.start({ tier: 'cloud' });
      const edge = new TierNode({ tier: 'edge', avmPath: tierAvm('edge'), upstreamUrl: cloud.url });
      await expect(edge.start()).rejects.toMatchObject({ code: 'UPSTREAM_TIER_MISMATCH_REFUSED' });
      expect(edge.state).toBe('Refused');
    },
    TEST_TIMEOUT
  );

  it(
    "refuses a node whose AtomVM program is another tier's program",
    async () => {
      const cloud = await fleet.start({ tier: 'cloud' });
      const impostor = new TierNode({
        tier: 'fog',
        avmPath: tierAvm('edge'),
        upstreamUrl: cloud.url,
      });
      await expect(impostor.start()).rejects.toMatchObject({
        code: 'TIER_IDENTITY_MISMATCH_REFUSED',
      });
      expect(impostor.state).toBe('Refused');
    },
    TEST_TIMEOUT
  );

  it(
    'enforces the topology law: the cloud is the root, everything else needs an upstream',
    async () => {
      const cloud = await fleet.start({ tier: 'cloud' });
      await expect(
        new TierNode({ tier: 'cloud', avmPath: tierAvm('cloud'), upstreamUrl: cloud.url }).start()
      ).rejects.toMatchObject({ code: 'ROOT_HAS_UPSTREAM_REFUSED' });
      await expect(
        new TierNode({ tier: 'fog', avmPath: tierAvm('fog') }).start()
      ).rejects.toMatchObject({ code: 'UPSTREAM_REQUIRED_REFUSED' });
      expect(() => new TierNode({ tier: 'browser', avmPath: tierAvm('browser') })).toThrow(
        /client/
      );
      expect(() => new TierNode({ tier: 'mist', avmPath: 'x' })).toThrow(/unknown tier/);
    },
    TEST_TIMEOUT
  );

  it(
    'refuses a corrupt, missing, or unrunnable AtomVM application before serving anything',
    async () => {
      const dir = mkdtempSync(join(tmpdir(), 'tier-bad-'));
      const fake = join(dir, 'tier_cloud.avm');
      writeFileSync(fake, '404: Not Found');
      const corrupt = new TierNode({ tier: 'cloud', avmPath: fake });
      await expect(corrupt.start()).rejects.toMatchObject({ code: 'AVM_INVALID_REFUSED' });

      const missing = new TierNode({ tier: 'cloud', avmPath: join(dir, 'absent.avm') });
      await expect(missing.start()).rejects.toMatchObject({ code: 'AVM_NOT_FOUND_REFUSED' });

      const noBinary = new TierNode({
        tier: 'cloud',
        avmPath: tierAvm('cloud'),
        atomvmBinary: '/nonexistent/AtomVM',
      });
      await expect(noBinary.start()).rejects.toMatchObject({ code: 'RUNTIME_BOOT_REFUSED' });
      for (const node of [corrupt, missing, noBinary]) {
        expect(node.state).toBe('Refused');
        expect(node.url).toBeNull();
      }
    },
    TEST_TIMEOUT
  );

  it(
    'cannot be started twice',
    async () => {
      const cloud = await fleet.start({ tier: 'cloud' });
      await expect(cloud.start()).rejects.toMatchObject({ code: 'BOOTSTRAP_STATE_REFUSED' });
      expect(cloud.state).toBe('Alive');
    },
    TEST_TIMEOUT
  );
});

describe('first mile: device -> edge -> fog -> cloud', () => {
  const fleet = tierFleet();
  let cloud;
  let fog;
  let edge;
  beforeAll(async () => {
    ({ cloud, fog, edge } = await fleet.bootstrapAll());
  }, TEST_TIMEOUT);
  afterAll(() => fleet.stopAll());

  it(
    'is durable end to end: every tier stores it, and every tier ran its own AtomVM program for it',
    async () => {
      const payload = { reading: 21.5, unit: 'C', seq: 1 };
      const response = await deviceIngest(edge, payload);
      expect(response.status).toBe(200);
      expect(response.json.status).toBe('stored');

      const digest = await payloadDigestOf(payload);
      expect(response.json.digest).toBe(digest);
      expect([cloud, fog, edge].map(node => node.hasRecord(digest))).toEqual([true, true, true]);
      expect([cloud, fog, edge].map(node => node.recordCount())).toEqual([1, 1, 1]);

      const chain = response.json.chain;
      expect(await verifyChain(chain, { payloadDigest: digest, phase: 'ingest' })).toMatchObject({
        valid: true,
        tiers: ['edge', 'fog', 'cloud'],
      });

      for (const [node, receipt] of [
        [edge, chain[0]],
        [fog, chain[1]],
        [cloud, chain[2]],
      ]) {
        const executions = node.executionReceipts();
        expect(executions).toHaveLength(1);
        const [execution] = executions;
        expect(execution.status).toBe('ALIVE');
        expect(execution.result.stdout).toContain(witnessMarker(node.tier)); // the tier's own program really ran
        expect(node.verifyExecutionReceipt(execution)).toBe(true);
        expect(receipt.executionDigest).toBe(execution.receiptDigest); // chain receipt is bound to that run
      }
    },
    TEST_TIMEOUT
  );

  it('a tampered AtomVM execution receipt fails verification', () => {
    const [execution] = cloud.executionReceipts();
    expect(
      cloud.verifyExecutionReceipt({ ...execution, status: 'ALIVE', completedAt: 'forged' })
    ).toBe(false);
    expect(cloud.verifyExecutionReceipt({ ...execution, route: ['someone-else'] })).toBe(false);
  });

  it(
    'is idempotent: replaying the same reading stores nothing new and runs no new AtomVM work',
    async () => {
      const payload = { reading: 22.0, unit: 'C', seq: 2 };
      await deviceIngest(edge, payload);
      const before = [cloud, fog, edge].map(node => node.executionReceipts().length);
      const replay = await deviceIngest(edge, payload);
      expect(replay.status).toBe(200);
      expect(replay.json.status).toBe('duplicate');
      expect([cloud, fog, edge].map(node => node.executionReceipts().length)).toEqual(before);
      expect(cloud.recordCount()).toBe(2); // seq 1 and seq 2, not 3
    },
    TEST_TIMEOUT
  );

  it(
    'canonicalises payloads: key order does not change identity',
    async () => {
      const a = await deviceIngest(edge, { x: 1, y: { p: 1, q: 2 } });
      const b = await deviceIngest(edge, { y: { q: 2, p: 1 }, x: 1 });
      expect(a.json.digest).toBe(b.json.digest);
      expect(b.json.status).toBe('duplicate');
    },
    TEST_TIMEOUT
  );

  describe('refuses bad first-mile input and leaves every store untouched', () => {
    const snapshot = () => [cloud, fog, edge].map(node => node.recordCount());

    it('malformed JSON -> 400', async () => {
      const before = snapshot();
      const response = await request(edge.url, 'POST', '/ingest', '{not json');
      expect(response.status).toBe(400);
      expect(response.json.error).toBe('INVALID_JSON_REFUSED');
      expect(snapshot()).toEqual(before);
    });

    it.each([
      ['missing origin', { payload: { a: 1 } }],
      ['missing payload', { origin: 'x' }],
      ['non-object body', [1, 2, 3]],
      ['chain that is not an array', { payload: { a: 1 }, origin: 'x', chain: 'nope' }],
      ['oversized origin', { payload: { a: 1 }, origin: 'o'.repeat(300) }],
    ])('%s -> 422', async (_label, body) => {
      const before = snapshot();
      const response = await request(edge.url, 'POST', '/ingest', body);
      expect(response.status).toBe(422);
      expect(response.json.error).toBe('INVALID_INGEST_REFUSED');
      expect(snapshot()).toEqual(before);
    });

    it('payload corrupted in transit (digest attested by sender no longer matches) -> 422', async () => {
      const before = snapshot();
      const response = await request(edge.url, 'POST', '/ingest', {
        payload: { reading: 99 },
        origin: 'sensor-17',
        payloadDigest: await payloadDigestOf({ reading: 21 }),
      });
      expect(response.status).toBe(422);
      expect(response.json.error).toBe('DIGEST_MISMATCH_REFUSED');
      expect(snapshot()).toEqual(before);
    });

    it('a body over the size limit -> 413', async () => {
      const response = await request(edge.url, 'POST', '/ingest', {
        payload: 'x'.repeat(1_100_000),
        origin: 'big',
      });
      expect(response.status).toBe(413);
      expect(response.json.error).toBe('BODY_TOO_LARGE_REFUSED');
    });

    it('a forged receipt chain (hash does not match contents) -> 422', async () => {
      const payload = { forged: true };
      const digest = await payloadDigestOf(payload);
      const genuine = await sealReceipt({
        tier: 'browser',
        nodeId: 'b',
        phase: 'ingest',
        payloadDigest: digest,
        prev: null,
        executionDigest: 'e',
        at: 't',
      });
      const forged = { ...genuine, nodeId: 'someone-else' }; // contents changed, digest kept
      const before = snapshot();
      const response = await request(edge.url, 'POST', '/ingest', {
        payload,
        origin: 'b',
        payloadDigest: digest,
        chain: [forged],
      });
      expect(response.status).toBe(422);
      expect(response.json).toMatchObject({
        error: 'CHAIN_INVALID_REFUSED',
        details: { chainCode: 'RECEIPT_DIGEST_MISMATCH' },
      });
      expect(snapshot()).toEqual(before);
    });

    it('a chain that attests a different payload -> 422', async () => {
      const other = await payloadDigestOf({ other: 1 });
      const receipt = await sealReceipt({
        tier: 'browser',
        nodeId: 'b',
        phase: 'ingest',
        payloadDigest: other,
        prev: null,
        executionDigest: 'e',
        at: 't',
      });
      const response = await request(edge.url, 'POST', '/ingest', {
        payload: { mine: 1 },
        origin: 'b',
        chain: [receipt],
      });
      expect(response.status).toBe(422);
      expect(response.json.details.chainCode).toBe('PAYLOAD_DIGEST_MISMATCH');
    });

    it('a browser receipt sent straight to the fog skips the edge -> 422 TIER_SKIP', async () => {
      const payload = { skip: 'edge' };
      const digest = await payloadDigestOf(payload);
      const receipt = await sealReceipt({
        tier: 'browser',
        nodeId: 'b',
        phase: 'ingest',
        payloadDigest: digest,
        prev: null,
        executionDigest: 'e',
        at: 't',
      });
      const before = snapshot();
      const response = await request(fog.url, 'POST', '/ingest', {
        payload,
        origin: 'b',
        chain: [receipt],
      });
      expect(response.status).toBe(422);
      expect(response.json.error).toBe('TIER_SKIP_REFUSED');
      expect(snapshot()).toEqual(before);
    });

    it('a delivery-phase chain cannot be replayed as ingest', async () => {
      const payload = { phase: 'confusion' };
      const digest = await payloadDigestOf(payload);
      const receipt = await sealReceipt({
        tier: 'browser',
        nodeId: 'b',
        phase: 'deliver',
        payloadDigest: digest,
        prev: null,
        executionDigest: 'e',
        at: 't',
      });
      const response = await request(edge.url, 'POST', '/ingest', {
        payload,
        origin: 'b',
        chain: [receipt],
      });
      expect(response.status).toBe(422);
      expect(response.json.details.chainCode).toBe('PHASE_MISMATCH');
    });

    it('unknown routes and methods -> 404', async () => {
      expect((await request(edge.url, 'GET', '/ingest')).status).toBe(404);
      expect((await request(edge.url, 'GET', '/nope')).json.error).toBe('ROUTE_NOT_FOUND_REFUSED');
    });
  });
});

describe('first mile when the network above is broken', () => {
  const fleet = tierFleet();
  afterEach(() => fleet.stopAll());

  it(
    'does not acknowledge - or keep - data the cloud never acknowledged',
    async () => {
      const { cloud, fog, edge } = await fleet.bootstrapAll();
      await fog.stop(); // the fog dies after bootstrap
      const payload = { reading: 1, seq: 100 };
      const response = await deviceIngest(edge, payload);
      expect(response.status).toBe(502);
      expect(response.json.error).toBe('UPSTREAM_UNAVAILABLE_REFUSED');
      expect(edge.recordCount()).toBe(0); // no write-behind: a refused write leaves no trace
      expect(cloud.recordCount()).toBe(0);
      expect(edge.state).toBe('Alive'); // and the edge stays up to retry
    },
    TEST_TIMEOUT
  );

  it(
    'refuses when the upstream acknowledges with a chain that does not extend ours (impostor fog)',
    async () => {
      const cloud = await fleet.start({ tier: 'cloud' });
      const impostor = await httpServer((req, res) => {
        if (req.url === '/health') return json(res, 200, { tier: 'fog', state: 'Alive' });
        return json(res, 200, { status: 'stored', chain: [] }); // claims success, proves nothing
      });
      try {
        const edge = await fleet.start({ tier: 'edge', upstreamUrl: impostor.url });
        const response = await deviceIngest(edge, { reading: 5 });
        expect(response.status).toBe(502);
        expect(['UPSTREAM_TAMPER_REFUSED', 'UPSTREAM_REFUSED']).toContain(response.json.error);
        expect(edge.recordCount()).toBe(0);
        expect(cloud.recordCount()).toBe(0);
      } finally {
        await impostor.close();
      }
    },
    TEST_TIMEOUT
  );
});

describe('last mile: cloud -> fog -> edge -> consumer', () => {
  const fleet = tierFleet();
  let cloud;
  let fog;
  let edge;
  beforeAll(async () => {
    ({ cloud, fog, edge } = await fleet.bootstrapAll());
  }, TEST_TIMEOUT);
  afterAll(() => fleet.stopAll());

  it(
    'descends through every tier for cold data, then serves it from the edge without touching upstream again',
    async () => {
      // Data that exists only at the system of record (e.g. produced by a cloud batch job).
      const payload = { report: 'quarterly', rows: [1, 2, 3] };
      const digest = await payloadDigestOf(payload);
      const stored = await request(cloud.url, 'POST', '/ingest', {
        payload,
        origin: 'batch-job',
        payloadDigest: digest,
      });
      expect(stored.json.status).toBe('stored');
      expect([cloud.hasRecord(digest), fog.hasRecord(digest), edge.hasRecord(digest)]).toEqual([
        true,
        false,
        false,
      ]);

      const cold = await request(edge.url, 'GET', `/deliver/${digest}`);
      expect(cold.status).toBe(200);
      expect(cold.json.servedBy).toBe('cloud');
      expect(cold.json.record.payload).toEqual(payload);
      expect(await payloadDigestOf(cold.json.record.payload)).toBe(digest);
      expect(
        await verifyChain(cold.json.chain, { payloadDigest: digest, phase: 'deliver' })
      ).toMatchObject({
        valid: true,
        tiers: ['cloud', 'fog', 'edge'],
      });
      for (const [node, receipt] of [
        [cloud, cold.json.chain[0]],
        [fog, cold.json.chain[1]],
        [edge, cold.json.chain[2]],
      ]) {
        expect(
          node
            .executionReceipts()
            .some(execution => execution.receiptDigest === receipt.executionDigest)
        ).toBe(true);
      }
      // Every tier on the way down is now warm.
      expect([cloud.hasRecord(digest), fog.hasRecord(digest), edge.hasRecord(digest)]).toEqual([
        true,
        true,
        true,
      ]);

      const upstreamBefore = [
        (await request(fog.url, 'GET', '/health')).json.counters.deliveries,
        cloud.executionReceipts().length,
      ];
      const warm = await request(edge.url, 'GET', `/deliver/${digest}`);
      expect(warm.json.servedBy).toBe('edge');
      expect(warm.json.chain.map(receipt => receipt.tier)).toEqual(['edge']);
      const upstreamAfter = [
        (await request(fog.url, 'GET', '/health')).json.counters.deliveries,
        cloud.executionReceipts().length,
      ];
      expect(upstreamAfter).toEqual(upstreamBefore); // fog and cloud were not consulted
    },
    TEST_TIMEOUT
  );

  it(
    'delivers data that arrived through the first mile, with its full ingest provenance to the cloud',
    async () => {
      const payload = { reading: 30, seq: 7 };
      const ingested = await deviceIngest(edge, payload);
      const digest = ingested.json.digest;
      const delivered = await request(edge.url, 'GET', `/deliver/${digest}`);
      expect(delivered.json.record.origin).toBe('sensor-17');
      expect(delivered.json.record.ingestChain.map(receipt => receipt.tier)).toEqual([
        'edge',
        'fog',
        'cloud',
      ]);
      expect(delivered.json.record.ingestChain).toEqual(ingested.json.chain);
    },
    TEST_TIMEOUT
  );

  it(
    'a second edge starts cold and is served by the fog (regional cache), then warms up',
    async () => {
      const payload = { region: 'eu', doc: 'price-list' };
      const digest = await payloadDigestOf(payload);
      await request(cloud.url, 'POST', '/ingest', {
        payload,
        origin: 'cms',
        payloadDigest: digest,
      });
      await request(fog.url, 'GET', `/deliver/${digest}`); // fog warmed by some earlier consumer
      const edge2 = await fleet.start({ tier: 'edge', nodeId: 'edge-2', upstreamUrl: fog.url });

      const first = await request(edge2.url, 'GET', `/deliver/${digest}`);
      expect(first.json.servedBy).toBe('fog');
      expect(first.json.chain.map(receipt => receipt.tier)).toEqual(['fog', 'edge']);
      expect(edge2.hasRecord(digest)).toBe(true);
      expect((await request(edge2.url, 'GET', `/deliver/${digest}`)).json.servedBy).toBe('edge');
    },
    TEST_TIMEOUT
  );

  it(
    'reports a missing record as 404 all the way down, and a malformed digest as 400',
    async () => {
      const missing = 'a'.repeat(64);
      const response = await request(edge.url, 'GET', `/deliver/${missing}`);
      expect(response.status).toBe(404);
      expect(response.json.error).toBe('NOT_FOUND_REFUSED');
      expect((await request(edge.url, 'GET', '/deliver/not-a-digest')).status).toBe(400);
      expect((await request(edge.url, 'GET', `/deliver/${'A'.repeat(64)}`)).status).toBe(400);
    },
    TEST_TIMEOUT
  );
});

describe('last mile falsifiers and degradation', () => {
  const fleet = tierFleet();
  afterEach(() => fleet.stopAll());

  it(
    'keeps serving warm data when everything above the edge is gone (edge autonomy)',
    async () => {
      const { fog, edge } = await fleet.bootstrapAll();
      const ingested = await deviceIngest(edge, { reading: 3, seq: 42 });
      await fog.stop();
      const delivered = await request(edge.url, 'GET', `/deliver/${ingested.json.digest}`);
      expect(delivered.status).toBe(200);
      expect(delivered.json.servedBy).toBe('edge');
      // ...but cold data cannot be conjured: it fails loudly, it is not invented.
      const cold = await request(edge.url, 'GET', `/deliver/${'b'.repeat(64)}`);
      expect(cold.status).toBe(502);
      expect(cold.json.error).toBe('UPSTREAM_UNAVAILABLE_REFUSED');
    },
    TEST_TIMEOUT
  );

  const payload = { secret: 'v1' };

  /** An impostor fog that answers /health correctly, then lies on the last mile. */
  async function edgeUnderImpostor(deliver) {
    const impostor = await httpServer((req, res) => {
      if (req.url === '/health') return json(res, 200, { tier: 'fog', state: 'Alive' });
      return deliver(req, res);
    });
    const edge = await fleet.start({ tier: 'edge', upstreamUrl: impostor.url });
    return { impostor, edge, digest: await payloadDigestOf(payload) };
  }

  it(
    'refuses a payload altered by the upstream',
    async () => {
      const digest = await payloadDigestOf(payload);
      const receipt = await sealReceipt({
        tier: 'fog',
        nodeId: 'evil',
        phase: 'deliver',
        payloadDigest: digest,
        prev: null,
        executionDigest: 'e',
        at: 't',
      });
      const { impostor, edge } = await edgeUnderImpostor((_req, res) =>
        json(res, 200, {
          record: { payload: { secret: 'v2-TAMPERED' }, origin: 'x', ingestChain: [] },
          chain: [receipt],
          servedBy: 'fog',
        })
      );
      try {
        const response = await request(edge.url, 'GET', `/deliver/${digest}`);
        expect(response.status).toBe(502);
        expect(response.json.error).toBe('UPSTREAM_TAMPER_REFUSED');
        expect(edge.hasRecord(digest)).toBe(false); // poison is not cached
      } finally {
        await impostor.close();
      }
    },
    TEST_TIMEOUT
  );

  it(
    'refuses a correct payload whose delivery receipts were forged',
    async () => {
      const digest = await payloadDigestOf(payload);
      const genuine = await sealReceipt({
        tier: 'fog',
        nodeId: 'evil',
        phase: 'deliver',
        payloadDigest: digest,
        prev: null,
        executionDigest: 'e',
        at: 't',
      });
      const { impostor, edge } = await edgeUnderImpostor((_req, res) =>
        json(res, 200, {
          record: { payload, origin: 'x', ingestChain: [] },
          chain: [{ ...genuine, at: 'edited' }],
          servedBy: 'fog',
        })
      );
      try {
        const response = await request(edge.url, 'GET', `/deliver/${digest}`);
        expect(response.status).toBe(502);
        expect(response.json).toMatchObject({
          error: 'UPSTREAM_TAMPER_REFUSED',
          details: { chainCode: 'RECEIPT_DIGEST_MISMATCH' },
        });
        expect(edge.hasRecord(digest)).toBe(false);
      } finally {
        await impostor.close();
      }
    },
    TEST_TIMEOUT
  );

  it(
    'refuses a delivery that skipped a tier (a cloud receipt handed straight to the edge)',
    async () => {
      const digest = await payloadDigestOf(payload);
      const receipt = await sealReceipt({
        tier: 'cloud',
        nodeId: 'evil',
        phase: 'deliver',
        payloadDigest: digest,
        prev: null,
        executionDigest: 'e',
        at: 't',
      });
      const { impostor, edge } = await edgeUnderImpostor((_req, res) =>
        json(res, 200, {
          record: { payload, origin: 'x', ingestChain: [] },
          chain: [receipt],
          servedBy: 'cloud',
        })
      );
      try {
        const response = await request(edge.url, 'GET', `/deliver/${digest}`);
        expect(response.status).toBe(502);
        expect(response.json.error).toBe('TIER_SKIP_REFUSED');
        expect(edge.hasRecord(digest)).toBe(false);
      } finally {
        await impostor.close();
      }
    },
    TEST_TIMEOUT
  );
});

describe('cross-origin access for the browser tier', () => {
  const fleet = tierFleet();
  afterEach(() => fleet.stopAll());
  const APP = 'https://app.example';

  it(
    'grants only listed origins, answers preflight, and denies everyone else by default',
    async () => {
      const { edge } = await fleet.bootstrapAll({ edge: { allowedOrigins: [APP] } });

      const allowed = await request(edge.url, 'GET', '/health', undefined, { origin: APP });
      expect(allowed.headers.get('access-control-allow-origin')).toBe(APP);
      expect(allowed.headers.get('cross-origin-resource-policy')).toBe('cross-origin');

      const preflight = await request(edge.url, 'OPTIONS', '/ingest', undefined, {
        origin: APP,
        'access-control-request-method': 'POST',
        'access-control-request-headers': 'content-type',
      });
      expect(preflight.status).toBe(204);
      expect(preflight.headers.get('access-control-allow-methods')).toContain('POST');

      const evil = await request(edge.url, 'GET', '/health', undefined, {
        origin: 'https://evil.example',
      });
      expect(evil.headers.get('access-control-allow-origin')).toBeNull();
      const evilPreflight = await request(edge.url, 'OPTIONS', '/ingest', undefined, {
        origin: 'https://evil.example',
      });
      expect(evilPreflight.status).toBe(403);

      const noOrigin = await request(edge.url, 'GET', '/health');
      expect(noOrigin.headers.get('access-control-allow-origin')).toBeNull();
    },
    TEST_TIMEOUT
  );
});
