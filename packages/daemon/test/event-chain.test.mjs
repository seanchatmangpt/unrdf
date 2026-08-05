import { describe, expect, it } from 'vitest';
import { blake3 } from 'hash-wasm';
import {
  buildMerkleTree,
  getMerkleProofPath,
  verifyMerkleProof,
} from '../src/integrations/kgc-4d-merkle.mjs';
import { DaemonEventStore } from '../src/integrations/kgc-4d-sourcing.mjs';

const logger = { log() {}, error() {} };

describe('DaemonEventStore append-only transition law', () => {
  it('preserves immutable current objects and appends status history', async () => {
    const store = new DaemonEventStore({ logger });
    const enqueued = await store.appendEvent('task', { input: 1 });
    const started = await store.updateEventStatus(enqueued.operationId, 'started');
    const success = await store.updateEventStatus(enqueued.operationId, 'success', { output: 2 });

    expect(enqueued.status).toBe('enqueued');
    expect(started.status).toBe('started');
    expect(success.status).toBe('success');
    expect(store.eventLog).toHaveLength(1);
    expect(store.transitionLog).toHaveLength(3);
    expect(started.previousEventId).toBe(enqueued.id);
    expect(success.previousEventId).toBe(started.id);
    expect(started.previousHash).toBe(enqueued.currentHash);
    expect(success.previousHash).toBe(started.currentHash);
    expect(await store.verifyTransitionChain()).toEqual({
      valid: true,
      count: 3,
      operationCount: 1,
      head: success.currentHash,
      reason: null,
    });
  });

  it('retains status history for queries and time reconstruction', async () => {
    const store = new DaemonEventStore({ logger });
    const enqueued = await store.appendEvent('task', {});
    const started = await store.updateEventStatus(enqueued.operationId, 'started');
    await store.updateEventStatus(enqueued.operationId, 'success', 'done');

    expect(await store.queryEvents({ operationId: enqueued.operationId })).toHaveLength(1);
    expect(await store.queryEvents({ operationId: enqueued.operationId, includeHistory: true })).toHaveLength(3);
    const state = await store.reconstructState(started.timestamp);
    expect(state.eventCount).toBe(1);
    expect(state.transitionCount).toBe(2);
    expect(state.events[0].status).toBe('started');
  });

  it('refuses post-terminal mutation before actuation', async () => {
    const store = new DaemonEventStore({ logger });
    const event = await store.appendEvent('task', {});
    await store.updateEventStatus(event.operationId, 'success');
    const before = store.getStats();

    await expect(store.updateEventStatus(event.operationId, 'failure')).rejects.toThrow(/terminal/);
    expect(store.getStats()).toEqual(before);
  });

  it('refuses ambiguous evidence without changing state', async () => {
    const store = new DaemonEventStore({ logger });
    await store.initialize();
    const before = store.getStats();
    const cyclic = {};
    cyclic.self = cyclic;
    const sparse = [];
    sparse.length = 1;

    for (const payload of [
      { value: Number.NaN },
      { value: undefined },
      { value: new Date() },
      cyclic,
      sparse,
    ]) {
      await expect(store.appendEvent('task', payload)).rejects.toThrow(TypeError);
      expect(store.getStats()).toEqual(before);
    }
  });

  it('refuses non-plain result values before a status transition', async () => {
    const store = new DaemonEventStore({ logger });
    const event = await store.appendEvent('task', { admitted: 1n });
    const before = store.getStats();

    await expect(store.updateEventStatus(event.operationId, 'success', new Map())).rejects.toThrow(TypeError);
    expect(store.getStats()).toEqual(before);
    expect(store.eventLog[0].status).toBe('enqueued');
  });

  it('detects replacement tampering in the transition ledger', async () => {
    const store = new DaemonEventStore({ logger });
    await store.appendEvent('task', { input: 1 });
    store.transitionLog[0] = { ...store.transitionLog[0], payload: { input: 2 } };

    const receipt = await store.verifyTransitionChain();
    expect(receipt.valid).toBe(false);
    expect(receipt.reason).toBe('CURRENT_HASH_MISMATCH');
  });

  it('binds per-operation ancestry and current views', async () => {
    const ancestryStore = new DaemonEventStore({ logger });
    const first = await ancestryStore.appendEvent('task', {});
    await ancestryStore.updateEventStatus(first.operationId, 'started');
    ancestryStore.transitionLog[1] = { ...ancestryStore.transitionLog[1], previousEventId: 'wrong' };
    expect((await ancestryStore.verifyTransitionChain()).reason).toBe('PREVIOUS_EVENT_ID_MISMATCH');

    const viewStore = new DaemonEventStore({ logger });
    const current = await viewStore.appendEvent('task', {});
    viewStore.eventLog[0] = { ...current, id: 'wrong' };
    expect((await viewStore.verifyTransitionChain()).reason).toBe('CURRENT_VIEW_MISMATCH');
  });
});

describe('Daemon Merkle proof law', () => {
  it('binds every leaf count including zero and one', async () => {
    const leaves = [await blake3('leaf-0'), await blake3('leaf-1')];
    const roots = await Promise.all([
      buildMerkleTree([]),
      buildMerkleTree([leaves[0]]),
      buildMerkleTree(leaves),
    ]);

    expect(new Set(roots).size).toBe(3);
    expect(roots[1]).not.toBe(leaves[0]);
  });

  it('refuses non-canonical positions and surplus path steps', async () => {
    const leaves = await Promise.all([0, 1, 2].map(index => blake3(`leaf-${index}`)));
    const proof = {
      leafIndex: 2,
      leafCount: leaves.length,
      leafHash: leaves[2],
      proof: await getMerkleProofPath(leaves, 2),
      merkleRoot: await buildMerkleTree(leaves),
    };

    expect(await verifyMerkleProof(proof)).toBe(true);
    expect(await verifyMerkleProof({
      ...proof,
      proof: [{ ...proof.proof[0], position: 'left' }, ...proof.proof.slice(1)],
    })).toBe(false);
    expect(await verifyMerkleProof({
      ...proof,
      proof: [...proof.proof, proof.proof.at(-1)],
    })).toBe(false);
  });
});
