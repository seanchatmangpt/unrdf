import test from 'node:test';
import assert from 'node:assert/strict';
import { createPolicyEngine } from '../src/policy-engine.mjs';
import { createSnapshot, verifySnapshot, diffSnapshots } from '../src/snapshot.mjs';
import { createBoundedCache } from '../src/bounded-cache.mjs';
import { createEventLog } from '../src/event-log.mjs';
import { createLeaseRegistry } from '../src/lease.mjs';

test('policy engine returns first priority decision', async () => {
  const engine = createPolicyEngine().add({ id: 'permit', priority: 1, decide: () => ({ effect: 'PERMIT' }) });
  assert.equal((await engine.evaluate({})).effect, 'PERMIT');
});

test('snapshots verify and diff', () => {
  const before = createSnapshot('x', { n: 1 });
  const after = createSnapshot('x', { n: 2 });
  assert.equal(verifySnapshot(before).valid, true);
  assert.equal(diffSnapshots(before, after).changes.length, 1);
});

test('bounded cache expires deterministically', () => {
  let now = 0;
  const cache = createBoundedCache({ maxSize: 1, ttlMs: 5, now: () => now });
  cache.set('a', 1);
  now = 6;
  assert.equal(cache.get('a'), undefined);
});

test('event log verifies chain', () => {
  const log = createEventLog();
  log.append('A', {});
  log.append('B', {});
  assert.equal(log.verify().valid, true);
});

test('leases use fencing tokens', () => {
  let now = 0;
  const leases = createLeaseRegistry({ now: () => now });
  const first = leases.acquire('r', 'a', 5);
  now = 6;
  const second = leases.acquire('r', 'b', 5);
  assert.ok(second.token > first.token);
});
