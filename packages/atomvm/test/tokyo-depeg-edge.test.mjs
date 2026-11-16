/**
 * @fileoverview W6 edge court — Tokyo-Depeg burn-in over the real @unrdf/atomvm edge layer.
 *
 * Scenario: an order intent is admitted against the RDF topology graph (participant
 * set includes the Frankfurt clearinghouse), actuated through the real broker path,
 * sealed into an integrity-bound replayable receipt. A replayed intent must be
 * refused at the broker boundary; an intent naming a non-admitted topology node is
 * refused. Real AtomVM execution (AtomVMProcessBroker) requires ATOMVM_BIN and
 * ATOMVM_APP; when absent those tests skip typed UNSUPPORTED(no-atomvm-binary) —
 * execution is never faked.
 *
 * Anti-vacuity: the replay refusal is pinned to src/swarm-cluster.mjs
 * (AtomVMSwarmCluster.actuate, `#actuatedIntentDigests` ledger throwing
 * INTENT_REPLAYED_REFUSED). Removing that ledger makes the "refuses a replayed
 * intent" assertion fail — witnessed by source-level sabotage (delete the ledger,
 * re-run: this file reports a failure, not a pass).
 *
 * @module @unrdf/atomvm/test/tokyo-depeg-edge
 */
import { describe, it, expect } from 'vitest';
import { stat } from 'node:fs/promises';
import { AtomVMSwarmCluster, SwarmClusterRefusal } from '../src/swarm-cluster.mjs';
import { AtomVMProcessBroker } from '../src/process-broker.mjs';

const atomvmBinary = process.env.ATOMVM_BIN ?? '';
const avmApp = process.env.ATOMVM_APP ?? '';
const hasRealRuntime = Boolean(atomvmBinary && avmApp);

async function fileExists(path) {
  try {
    return (await stat(path)).isFile();
  } catch {
    return false;
  }
}

/**
 * Deterministic participant topology per the Tokyo-Depeg pack: NY fund ->
 * Frankfurt clearinghouse -> Zurich custody. Fixed clock and id factory so
 * digests and receipts are byte-stable across runs.
 */
function fixture() {
  let id = 0;
  const cluster = new AtomVMSwarmCluster({
    clusterId: 'tokyo-depeg-edge',
    clock: () => '2026-10-03T00:00:00.000Z',
    idFactory: () => `tdb-${++id}`,
  });
  for (const node of [
    { id: 'ny-fund', gatewayNode: 'ny-fund-gateway', cookieRef: 'secret://ny-fund', endpoint: 'atomvm://ny-fund' },
    { id: 'frankfurt-clearinghouse', gatewayNode: 'frankfurt-gateway', cookieRef: 'secret://frankfurt', endpoint: 'atomvm://frankfurt-clearinghouse' },
    { id: 'zurich-custody', gatewayNode: 'zurich-gateway', cookieRef: 'secret://zurich', endpoint: 'atomvm://zurich-custody' },
  ]) {
    cluster.admitSwarm(node);
  }
  cluster.connect('ny-fund', 'frankfurt-clearinghouse');
  cluster.connect('frankfurt-clearinghouse', 'zurich-custody');
  return cluster;
}

function orderIntent(cluster) {
  return cluster.constructIntent({
    sourceId: 'ny-fund',
    targetId: 'frankfurt-clearinghouse',
    operation: 'atomvm.execute',
    payload: { instrument: 'USDJPY', notional: 1_000_000, side: 'sell' },
  });
}

describe('Tokyo-Depeg edge: topology admission (RDF graph, no ambient authority)', () => {
  it('admits the participant set including the clearinghouse node', () => {
    const cluster = fixture();
    const snapshot = cluster.snapshot();
    expect(snapshot.clusterId).toBe('tokyo-depeg-edge');
    expect(snapshot.swarms.map(swarm => swarm.id).sort()).toEqual([
      'frankfurt-clearinghouse',
      'ny-fund',
      'zurich-custody',
    ]);
    for (const swarm of snapshot.swarms) {
      expect(Object.isFrozen(swarm)).toBe(true);
      expect(swarm.cookieRef).toMatch(/^secret:\/\//);
    }
  });

  it('projects the admitted topology as RDF N-Quads naming the clearinghouse', () => {
    const quads = fixture().toNQuads();
    expect(quads).toContain('<https://unrdf.dev/atomvm/swarm/swarm/frankfurt-clearinghouse>');
    expect(quads).toContain('<https://unrdf.dev/atomvm/swarm/federatedWith>');
    expect(quads.match(/federatedWith/g)).toHaveLength(2);
  });

  it('refuses admission of a duplicate node', () => {
    const cluster = fixture();
    expect(() => cluster.admitSwarm({
      id: 'ny-fund', gatewayNode: 'x', cookieRef: 'secret://x', endpoint: 'atomvm://x',
    })).toThrow(expect.objectContaining({ code: 'DUPLICATE_SWARM_REFUSED' }));
  });
});

describe('Tokyo-Depeg edge: immutable order intent construction', () => {
  it('routes through admitted links only and freezes the intent with a digest', () => {
    const cluster = fixture();
    const intent = orderIntent(cluster);
    expect(intent.route).toEqual(['ny-fund', 'frankfurt-clearinghouse']);
    expect(Object.isFrozen(intent)).toBe(true);
    expect(intent.intentDigest).toMatch(/^[a-f0-9]{64}$/);
    expect(intent.status).toBe('CONSTRUCTED');
  });

  it('refuses an intent whose target is not an admitted topology node', () => {
    const cluster = fixture();
    expect(() => cluster.constructIntent({
      sourceId: 'ny-fund',
      targetId: 'cayman-offshore',
      operation: 'atomvm.execute',
      payload: {},
    })).toThrow(expect.objectContaining({ code: 'UNKNOWN_SWARM_REFUSED' }));
    expect(() => cluster.route('ny-fund', 'cayman-offshore'))
      .toThrow(expect.objectContaining({ code: 'UNKNOWN_SWARM_REFUSED' }));
  });
});

describe('Tokyo-Depeg edge: broker actuation, integrity-bound receipt, replay', () => {
  it('actuates through the broker and seals a verifiable replayable receipt', async () => {
    const cluster = fixture();
    const intent = orderIntent(cluster);
    const receipt = await cluster.actuate(intent, {
      execute: async ({ target, route }) => ({ target: target.id, hops: route.length - 1 }),
    });

    expect(receipt.status).toBe('ALIVE');
    expect(receipt.receiptId).toMatch(/^tdb-\d+$/);
    expect(receipt.intentDigest).toBe(intent.intentDigest);
    expect(receipt.route).toEqual(intent.route);
    expect(receipt.receiptDigest).toMatch(/^[a-f0-9]{64}$/);
    expect(Object.isFrozen(receipt)).toBe(true);
    expect(cluster.verifyReceipt(receipt)).toBe(true);
    expect(cluster.replay(receipt.receiptId)).toEqual(receipt);
  });

  it('refuses the SAME intent replayed at the broker boundary', async () => {
    const cluster = fixture();
    const intent = orderIntent(cluster);
    await cluster.actuate(intent, { execute: async ({ target }) => ({ target: target.id }) });

    let refused = null;
    try {
      await cluster.actuate(intent, { execute: async ({ target }) => ({ target: target.id }) });
    } catch (error) {
      refused = error;
    }
    expect(refused).not.toBeNull();
    expect(refused).toBeInstanceOf(SwarmClusterRefusal);
    expect(refused.code).toBe('INTENT_REPLAYED_REFUSED');
  });

  it('refuses a tampered intent (digest drift) before the broker is reached', async () => {
    const cluster = fixture();
    const intent = orderIntent(cluster);
    const mutated = { ...intent, payload: { ...intent.payload, notional: 1 } };
    let refused = null;
    try {
      await cluster.actuate(mutated, { execute: async () => { throw new Error('broker must not be reached'); } });
    } catch (error) {
      refused = error;
    }
    expect(refused?.code).toBe('INTENT_DRIFT_REFUSED');
  });

  it('refuses intents that do not belong to this cluster', async () => {
    const cluster = fixture();
    const foreign = { ...orderIntent(cluster), clusterId: 'somewhere-else' };
    await expect(cluster.actuate(foreign, { execute: async () => ({}) }))
      .rejects.toMatchObject({ code: 'UNADMITTED_INTENT_REFUSED' });
  });
});

describe.skipIf(!hasRealRuntime)('Tokyo-Depeg edge: real AtomVM execution (UNSUPPORTED(no-atomvm-binary) when unconfigured)', () => {
  it('executes the order intent through AtomVMProcessBroker and seals an ALIVE receipt', async () => {
    expect(await fileExists(atomvmBinary)).toBe(true);
    expect(await fileExists(avmApp)).toBe(true);

    const cluster = fixture();
    const intent = orderIntent(cluster);
    const broker = new AtomVMProcessBroker({
      atomvmBinary,
      swarms: { 'frankfurt-clearinghouse': { avmPath: avmApp } },
    });
    const receipt = await cluster.actuate(intent, broker);

    expect(receipt.status).toBe('ALIVE');
    expect(receipt.result.markerObserved).toBe(true);
    expect(receipt.result.stdoutDigest).toMatch(/^[a-f0-9]{64}$/);
    expect(cluster.verifyReceipt(receipt)).toBe(true);
    expect(cluster.replay(receipt.receiptId)).toEqual(receipt);
  });
});

describe('Tokyo-Depeg edge: real broker refuses without a runtime (no fake execution)', () => {
  it('yields a typed broker refusal sealed as a BLOCKED receipt when the binary is absent', async () => {
    const cluster = fixture();
    const intent = orderIntent(cluster);
    const broker = new AtomVMProcessBroker({
      atomvmBinary: '/unavailable/AtomVM',
      swarms: { 'frankfurt-clearinghouse': { avmPath: avmApp || '/unavailable/order.avm' } },
    });
    const receipt = await cluster.actuate(intent, broker);

    expect(receipt.status).toBe('BLOCKED');
    expect(receipt.error.name).toBe('AtomVMProcessRefusal');
    expect(receipt.error.message).toMatch(/AtomVM binary is unavailable/);
    expect(receipt.receiptDigest).toMatch(/^[a-f0-9]{64}$/);
    expect(cluster.verifyReceipt(receipt)).toBe(true);
    expect(cluster.replay(receipt.receiptId)).toEqual(receipt);
  });
});
