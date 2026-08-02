import assert from 'node:assert/strict';
import { test } from 'node:test';
import {
  createIncrementalGraphIndex,
  quadKey,
  termKey,
} from '../src/incremental-index.mjs';

const nn = value => ({ termType: 'NamedNode', value });
const lit = (value, language = '', datatype = nn('http://www.w3.org/2001/XMLSchema#string')) => ({
  termType: 'Literal', value, language, datatype,
});
const dg = () => ({ termType: 'DefaultGraph', value: '' });
const quad = (s, p, o, g = dg()) => ({ subject: nn(s), predicate: nn(p), object: o, graph: g });

const aliceName = quad('urn:alice', 'urn:name', lit('Alice'));
const bobName = quad('urn:bob', 'urn:name', lit('Bob'));
const aliceKnows = quad('urn:alice', 'urn:knows', nn('urn:bob'), nn('urn:social'));

test('termKey distinguishes RDF term types and literal metadata', () => {
  assert.notEqual(termKey(nn('x')), termKey({ termType: 'BlankNode', value: 'x' }));
  assert.notEqual(termKey(lit('hello', 'en')), termKey(lit('hello', 'fr')));
  assert.notEqual(termKey(lit('1', '', nn('xsd:int'))), termKey(lit('1', '', nn('xsd:string'))));
});

test('quadKey includes graph identity', () => {
  const left = quad('s', 'p', lit('o'));
  const right = quad('s', 'p', lit('o'), nn('g'));
  assert.notEqual(quadKey(left), quadKey(right));
});

test('index add is idempotent and updates statistics', () => {
  const index = createIncrementalGraphIndex();
  assert.equal(index.add(aliceName), true);
  assert.equal(index.add(aliceName), false);
  assert.deepEqual(index.stats(), {
    size: 1, version: 1, subjects: 1, predicates: 1, objects: 1, graphs: 1, subscriptions: 0,
  });
});

test('index remove is idempotent', () => {
  const index = createIncrementalGraphIndex();
  index.add(aliceName);
  assert.equal(index.remove(aliceName), true);
  assert.equal(index.remove(aliceName), false);
  assert.equal(index.stats().size, 0);
});

test('match supports all RDF positions', () => {
  const index = createIncrementalGraphIndex();
  index.applyBatch([
    { type: 'add', quad: aliceName },
    { type: 'add', quad: bobName },
    { type: 'add', quad: aliceKnows },
  ]);
  assert.deepEqual(index.match({ subject: nn('urn:alice') }).map(quadKey), [aliceName, aliceKnows].map(quadKey).sort());
  assert.deepEqual(index.match({ predicate: nn('urn:name') }).map(quadKey), [aliceName, bobName].map(quadKey).sort());
  assert.deepEqual(index.match({ object: nn('urn:bob') }).map(quadKey), [aliceKnows].map(quadKey));
  assert.deepEqual(index.match({ graph: nn('urn:social') }).map(quadKey), [aliceKnows].map(quadKey));
});

test('planner chooses longest bound prefix deterministically', () => {
  const index = createIncrementalGraphIndex();
  assert.equal(index.plan({ subject: nn('s'), predicate: nn('p') }).index, 'SPO');
  assert.equal(index.plan({ predicate: nn('p'), object: nn('o') }).index, 'POS');
  assert.equal(index.plan({ object: nn('o'), subject: nn('s') }).index, 'OSP');
  assert.equal(index.plan({ graph: nn('g'), subject: nn('s') }).index, 'GRAPH');
});

test('applyBatch validates every operation before mutating', () => {
  const index = createIncrementalGraphIndex();
  assert.throws(() => index.applyBatch([
    { type: 'add', quad: aliceName },
    { type: 'invalid', quad: bobName },
  ]), /Invalid operation/);
  assert.equal(index.stats().size, 0);
});

test('applyBatch applies mixed changes and reports version', () => {
  const index = createIncrementalGraphIndex();
  index.add(aliceName);
  const result = index.applyBatch([
    { type: 'remove', quad: aliceName },
    { type: 'add', quad: bobName },
    { type: 'add', quad: aliceKnows },
  ]);
  assert.deepEqual(result, { applied: 3, version: 4 });
  assert.deepEqual(index.match().map(quadKey), [aliceKnows, bobName].map(quadKey).sort());
});

test('estimate reports current matching cardinality', () => {
  const index = createIncrementalGraphIndex();
  index.add(aliceName);
  index.add(bobName);
  assert.equal(index.estimate({ predicate: nn('urn:name') }), 2);
  assert.equal(index.estimate({ subject: nn('urn:missing') }), 0);
});

test('subscriptions receive only matching changes and can unsubscribe', () => {
  const index = createIncrementalGraphIndex();
  const events = [];
  const unsubscribe = index.subscribe({ predicate: nn('urn:name') }, event => events.push(event));
  index.add(aliceName);
  index.add(aliceKnows);
  index.remove(aliceName);
  unsubscribe();
  index.add(bobName);
  assert.deepEqual(events.map(event => event.type), ['add', 'remove']);
  assert.deepEqual(events.map(event => event.version), [1, 3]);
});

test('listener failures do not roll back index mutations', () => {
  const index = createIncrementalGraphIndex();
  index.subscribe({}, () => { throw new Error('listener failed'); });
  assert.equal(index.add(aliceName), true);
  assert.equal(index.stats().size, 1);
});

test('snapshot is deterministic across insertion order', () => {
  const left = createIncrementalGraphIndex();
  left.add(aliceName);
  left.add(bobName);
  const right = createIncrementalGraphIndex();
  right.add(bobName);
  right.add(aliceName);
  assert.equal(left.snapshot().digest, right.snapshot().digest);
});

test('snapshot restore rebuilds all indexes', () => {
  const index = createIncrementalGraphIndex();
  index.add(aliceName);
  index.add(aliceKnows);
  const snapshot = index.snapshot();
  const restored = createIncrementalGraphIndex().restore(snapshot);
  assert.deepEqual(restored.match().map(quadKey), index.match().map(quadKey));
  assert.deepEqual(restored.verify(), { valid: true, errors: [] });
  assert.equal(restored.version, snapshot.version);
});

test('restore refuses tampered snapshots', () => {
  const index = createIncrementalGraphIndex();
  index.add(aliceName);
  const snapshot = structuredClone(index.snapshot());
  snapshot.quads[0].object.value = 'Tampered';
  assert.throws(() => createIncrementalGraphIndex().restore(snapshot), /Invalid index snapshot/);
});

test('diff reports added and removed quads', () => {
  const index = createIncrementalGraphIndex();
  index.add(aliceName);
  const before = index.snapshot();
  index.remove(aliceName);
  index.add(bobName);
  const result = index.diff(before);
  assert.equal(result.added[0].object.value, 'Bob');
  assert.equal(result.removed[0].object.value, 'Alice');
  assert.equal(result.fromVersion, 1);
  assert.equal(result.toVersion, 3);
});

test('verify detects index corruption and compact repairs normal indexes', () => {
  const index = createIncrementalGraphIndex();
  index.add(aliceName);
  assert.deepEqual(index.verify(), { valid: true, errors: [] });
  index.spo.clear();
  assert.equal(index.verify().valid, false);
  index.compact();
  assert.deepEqual(index.verify(), { valid: true, errors: [] });
});
