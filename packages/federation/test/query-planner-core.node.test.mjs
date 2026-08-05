import test from 'node:test';
import assert from 'node:assert/strict';
import {
  parseBasicSparql,
  buildFederationPlan,
  executeFederationPlan,
  joinBindings,
  FederationPlanError,
  FederationExecutionError,
} from '../src/federation/query-planner-core.mjs';

const knows = 'urn:knows';
const name = 'urn:name';

function source(id, predicates, query, metadata = {}) {
  return { id, metadata: { predicates, ...metadata }, query };
}

test('parser extracts projection, triples, optional and filter', () => {
  const parsed = parseBasicSparql(`SELECT DISTINCT ?s ?name WHERE { ?s <${knows}> ?o . OPTIONAL { ?s <${name}> ?name . } FILTER(?s != ?o) }`);
  assert.equal(parsed.distinct, true);
  assert.deepEqual(parsed.variables, ['s', 'name']);
  assert.equal(parsed.patterns.length, 1);
  assert.equal(parsed.optionals.length, 1);
  assert.equal(parsed.filters.length, 1);
});

test('parser refuses non-select and malformed patterns', () => {
  assert.throws(() => parseBasicSparql('ASK { ?s ?p ?o }'), FederationPlanError);
  assert.throws(() => parseBasicSparql('SELECT * WHERE { ?s ?p }'), FederationPlanError);
});

test('planner routes concrete predicates to matching sources', () => {
  const plan = buildFederationPlan(`SELECT * WHERE { ?s <${knows}> ?o . ?s <${name}> ?name . }`, [
    source('graph', [knows], async () => []),
    source('labels', [name], async () => []),
  ]);
  assert.equal(plan.steps[0].candidates[0].sourceId, 'graph');
  assert.equal(plan.steps[1].candidates[0].sourceId, 'labels');
});

test('planner orders selective connected patterns first', () => {
  const plan = buildFederationPlan(`SELECT * WHERE { ?s ?p ?o . ?s <${name}> ?name . }`, [
    source('all', [], async () => [], { cardinality: 100000 }),
    source('labels', [name], async () => [], { predicateCardinality: { [name]: 10 } }),
  ]);
  assert.equal(plan.steps[0].pattern.predicate.value, name);
});

test('plan hash is deterministic', () => {
  const sources = [source('a', [name], async () => [])];
  const one = buildFederationPlan(`SELECT ?s WHERE { ?s <${name}> ?n . }`, sources);
  const two = buildFederationPlan(`SELECT ?s WHERE { ?s <${name}> ?n . }`, sources);
  assert.equal(one.planHash, two.planHash);
});

test('binding joins merge compatible rows and preserve optional left rows', () => {
  assert.deepEqual(joinBindings([{ s: 'a' }, { s: 'b' }], [{ s: 'a', n: 'A' }], true), [{ s: 'a', n: 'A' }, { s: 'b' }]);
});

test('executor performs bind joins across sources', async () => {
  const plan = buildFederationPlan(`SELECT ?s ?name WHERE { ?s <${knows}> ?o . ?s <${name}> ?name . }`, [
    source('graph', [knows], async () => [{ s: 'alice', o: 'bob' }, { s: 'carol', o: 'dan' }], { cardinality: 10 }),
    source('labels', [name], async (_query, { bindings }) => [{ s: bindings.s, name: bindings.s.toUpperCase() }], { cardinality: 20 }),
  ]);
  const result = await executeFederationPlan(plan);
  assert.deepEqual(result.rows, [{ s: 'alice', name: 'ALICE' }, { s: 'carol', name: 'CAROL' }]);
  assert.equal(result.trace.length, 3);
});

test('executor fails over to replicas', async () => {
  const plan = buildFederationPlan(`SELECT * WHERE { ?s <${name}> ?name . }`, [
    source('primary', [name], async () => { throw new Error('down'); }, { latencyMs: 1 }),
    source('replica', [name], async () => [{ s: 'alice', name: 'Alice' }], { latencyMs: 2 }),
  ]);
  const result = await executeFederationPlan(plan);
  assert.equal(result.rows.length, 1);
  assert.equal(result.trace[0].sourceId, 'replica');
});

test('executor can refuse failover', async () => {
  const plan = buildFederationPlan(`SELECT * WHERE { ?s <${name}> ?name . }`, [
    source('primary', [name], async () => { throw new Error('down'); }),
    source('replica', [name], async () => []),
  ]);
  await assert.rejects(executeFederationPlan(plan, { failover: false }));
});

test('executor enforces intermediate row bounds', async () => {
  const plan = buildFederationPlan(`SELECT * WHERE { ?s <${name}> ?name . }`, [source('a', [name], async () => Array.from({ length: 5 }, (_, i) => ({ s: i }))) ]);
  await assert.rejects(executeFederationPlan(plan, { maxIntermediateRows: 2 }), FederationExecutionError);
});

test('distinct projection deduplicates canonical rows', async () => {
  const plan = buildFederationPlan(`SELECT DISTINCT ?s WHERE { ?s <${name}> ?name . }`, [source('a', [name], async () => [{ s: 'a', name: 'A' }, { name: 'B', s: 'a' }])]);
  const result = await executeFederationPlan(plan);
  assert.deepEqual(result.rows, [{ s: 'a' }]);
});
