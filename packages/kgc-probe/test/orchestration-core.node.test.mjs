import test from 'node:test';
import assert from 'node:assert/strict';
import {
  canonicalJson,
  digest,
  deterministicId,
  observationIdentity,
  mergeObservations,
  sealArtifact,
  verifyArtifact,
  replayArtifacts,
  withTimeout,
  runAgentPool,
} from '../src/orchestration-core.mjs';

test('canonical JSON is key-order independent and handles rich values', () => {
  const a = canonicalJson({ b: 2, a: 1, set: new Set(['z', 'a']), bigint: 2n });
  const b = canonicalJson({ bigint: 2n, set: new Set(['a', 'z']), a: 1, b: 2 });
  assert.equal(a, b);
  assert.equal(digest({ a: 1, b: 2 }), digest({ b: 2, a: 1 }));
});

test('canonicalization refuses cycles', () => {
  const value = {};
  value.self = value;
  assert.throws(() => canonicalJson(value), /cyclic/);
});

test('deterministic IDs are stable and namespace separated', () => {
  assert.equal(deterministicId('probe', { x: 1 }), deterministicId('probe', { x: 1 }));
  assert.notEqual(deterministicId('probe', { x: 1 }), deterministicId('guard', { x: 1 }));
});

test('observation identity excludes volatile timestamps and IDs', () => {
  const left = { id: '1', timestamp: '2020-01-01', agent: 'a', kind: 'k', severity: 'info', subject: 's', evidence: { query: 'q' } };
  const right = { ...left, id: '2', timestamp: '2021-01-01' };
  assert.equal(observationIdentity(left), observationIdentity(right));
});

test('merge deduplicates exact observations and resolves conflicts deterministically', () => {
  const base = { id: '1', timestamp: '2020-01-01', agent: 'a', kind: 'k', severity: 'info', subject: 's', object: 'o' };
  const later = { ...base, id: '2', timestamp: '2021-01-01', tags: ['new'] };
  const merged = mergeObservations([{ observations: [base] }], [base, later]);
  assert.equal(merged.observations.length, 1);
  assert.equal(merged.observations[0].id, '2');
  assert.equal(merged.conflicts.length, 1);
});

test('merge can refuse conflicts', () => {
  const base = { id: '1', timestamp: '2020-01-01', agent: 'a', kind: 'k', severity: 'info', subject: 's' };
  assert.throws(() => mergeObservations([{ observations: [base] }], [{ ...base, id: '2', tags: ['x'] }], { conflictPolicy: 'error' }), /Conflicting observation/);
});

test('artifact manifests detect observation and metadata tampering', () => {
  const artifact = sealArtifact({ observations: [{ agent: 'a', kind: 'k', severity: 'info', subject: 's' }], summary: { total: 1 }, metadata: { run: 1 } });
  assert.equal(verifyArtifact(artifact).valid, true);
  const tampered = { ...artifact, metadata: { run: 2 } };
  const verification = verifyArtifact(tampered);
  assert.equal(verification.valid, false);
  assert.ok(verification.differences.some(item => item.path.includes('metadata')));
});

test('replay compares semantic roots independent of object key order', () => {
  const left = { observations: [], summary: { b: 2, a: 1 }, metadata: {} };
  const right = { metadata: {}, summary: { a: 1, b: 2 }, observations: [] };
  assert.equal(replayArtifacts(left, right).state, 'REPLAY_MATCH');
  assert.equal(replayArtifacts(left, { ...right, summary: { a: 2 } }).state, 'REPLAY_DIFFERENCE');
});

test('withTimeout returns values and types timeout failures', async () => {
  assert.equal(await withTimeout(async () => 42, 50), 42);
  await assert.rejects(withTimeout(() => new Promise(() => {}), 5), error => error.code === 'ETIMEDOUT');
});

test('withTimeout propagates external abort', async () => {
  const controller = new AbortController();
  const promise = withTimeout(() => new Promise(() => {}), 1000, controller.signal);
  controller.abort(new Error('stop'));
  await assert.rejects(promise, error => error.code === 'ABORT_ERR');
});

test('agent pool preserves input order under concurrency', async () => {
  const tasks = [30, 5, 15].map((delay, index) => ({
    id: `a${index}`,
    run: () => new Promise(resolve => setTimeout(() => resolve(index), delay)),
  }));
  const result = await runAgentPool(tasks, { concurrency: 3 });
  assert.deepEqual(result.results.map(item => item.value), [0, 1, 2]);
});

test('agent pool captures failures and supports fail-fast', async () => {
  const tasks = [
    { id: 'bad', run: async () => { throw new Error('boom'); } },
    { id: 'later', run: async () => 2 },
  ];
  const result = await runAgentPool(tasks, { concurrency: 1, failFast: true });
  assert.equal(result.errors.length, 1);
  assert.equal(result.stoppedEarly, true);
  assert.deepEqual(result.results.map(item => item.id), ['bad']);
});
