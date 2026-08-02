import test from 'node:test';
import assert from 'node:assert/strict';
import { createEvidenceStore } from '../src/evidence-store.mjs';
import { createCapabilityGraph } from '../src/capability-graph.mjs';
import { createAdmissionBoundary, rules, AdmissionRefusal } from '../src/admission-boundary.mjs';
import { verifyCommand } from '../src/command-verifier.mjs';
import { requireReplayMatch } from '../src/replay-runner.mjs';

test('evidence is content addressed and deduplicated', () => {
  const store = createEvidenceStore();
  const first = store.add({ claim: 'x', subject: 's', source: 'sha', state: 'ALIVE' });
  const second = store.add({ source: 'sha', subject: 's', claim: 'x', state: 'ALIVE' });
  assert.equal(first.digest, second.digest);
  assert.equal(store.verify().count, 1);
});

test('capability graph computes transitive impact', () => {
  const graph = createCapabilityGraph().addNode('storage').addNode('query').addNode('api');
  graph.addDependency('query', 'storage').addDependency('api', 'query');
  assert.deepEqual(graph.impact(['storage']), ['storage', 'query', 'api']);
});

test('admission boundary returns typed refusal', async () => {
  const boundary = createAdmissionBoundary().rule(rules.required('id'));
  await assert.rejects(() => boundary.admit({}), AdmissionRefusal);
});

test('command verifier observes real execution', async () => {
  const result = await verifyCommand(process.execPath, ['--eval', 'process.stdout.write("ok")']);
  assert.equal(result.state, 'ALIVE');
  assert.equal(result.stdout, 'ok');
});

test('replay runner requires deterministic output', async () => {
  const result = await requireReplayMatch(async () => ({ value: 1 }));
  assert.equal(result.state, 'REPLAY_MATCH');
});
