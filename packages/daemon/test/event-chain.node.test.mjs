import test from 'node:test';
import assert from 'node:assert/strict';
import { blake3 } from 'hash-wasm';
import {
  buildMerkleTree,
  getMerkleProofPath,
  verifyMerkleProof,
} from '../src/integrations/kgc-4d-merkle.mjs';
import { DaemonEventStore } from '../src/integrations/kgc-4d-sourcing.mjs';

const logger = { log() {}, error() {} };

test('status transitions preserve immutable current objects and append history', async () => {
  const store = new DaemonEventStore({ logger });
  const enqueued = await store.appendEvent('task', { input: 1 });
  const started = await store.updateEventStatus(enqueued.operationId, 'started');
  const success = await store.updateEventStatus(enqueued.operationId, 'success', { output: 2 });

  assert.equal(enqueued.status, 'enqueued');
  assert.equal(started.status, 'started');
  assert.equal(success.status, 'success');
  assert.equal(store.eventLog.length, 1);
  assert.equal(store.transitionLog.length, 3);
  assert.equal(started.previousEventId, enqueued.id);
  assert.equal(success.previousEventId, started.id);
  assert.equal(started.previousHash, enqueued.currentHash);
  assert.equal(success.previousHash, started.currentHash);
  assert.deepEqual(await store.verifyTransitionChain(), {
    valid: true,
    count: 3,
    head: success.currentHash,
    reason: null,
  });
});

test('historical queries and reconstruction retain every status transition', async () => {
  const store = new DaemonEventStore({ logger });
  const enqueued = await store.appendEvent('task', {});
  const started = await store.updateEventStatus(enqueued.operationId, 'started');
  await store.updateEventStatus(enqueued.operationId, 'success', 'done');

  assert.equal((await store.queryEvents({ operationId: enqueued.operationId })).length, 1);
  assert.equal((await store.queryEvents({ operationId: enqueued.operationId, includeHistory: true })).length, 3);
  const state = await store.reconstructState(started.timestamp);
  assert.equal(state.eventCount, 1);
  assert.equal(state.transitionCount, 2);
  assert.equal(state.events[0].status, 'started');
});

test('terminal operations refuse later mutation before actuation', async () => {
  const store = new DaemonEventStore({ logger });
  const event = await store.appendEvent('task', {});
  await store.updateEventStatus(event.operationId, 'success');
  const before = store.getStats();
  await assert.rejects(() => store.updateEventStatus(event.operationId, 'failure'), /terminal/);
  assert.deepEqual(store.getStats(), before);
});

test('transition-chain verification detects replacement tampering', async () => {
  const store = new DaemonEventStore({ logger });
  await store.appendEvent('task', { input: 1 });
  store.transitionLog[0] = { ...store.transitionLog[0], payload: { input: 2 } };
  const receipt = await store.verifyTransitionChain();
  assert.equal(receipt.valid, false);
  assert.equal(receipt.reason, 'CURRENT_HASH_MISMATCH');
});

test('Merkle roots bind every leaf count including zero and one', async () => {
  const leaves = [await blake3('leaf-0'), await blake3('leaf-1')];
  const roots = await Promise.all([
    buildMerkleTree([]),
    buildMerkleTree([leaves[0]]),
    buildMerkleTree(leaves),
  ]);
  assert.equal(new Set(roots).size, 3);
  assert.notEqual(roots[1], leaves[0]);
});

test('Merkle verification refuses non-canonical positions and surplus path steps', async () => {
  const leaves = await Promise.all([0, 1, 2].map(index => blake3(`leaf-${index}`)));
  const proof = {
    leafIndex: 2,
    leafCount: leaves.length,
    leafHash: leaves[2],
    proof: await getMerkleProofPath(leaves, 2),
    merkleRoot: await buildMerkleTree(leaves),
  };
  assert.equal(await verifyMerkleProof(proof), true);
  assert.equal(await verifyMerkleProof({
    ...proof,
    proof: [{ ...proof.proof[0], position: 'left' }, ...proof.proof.slice(1)],
  }), false);
  assert.equal(await verifyMerkleProof({
    ...proof,
    proof: [...proof.proof, proof.proof.at(-1)],
  }), false);
});
