import test from 'node:test';
import assert from 'node:assert/strict';
import {
  MemoryQuadStore,
  beginTransaction,
  quadKey,
  verifyTransactionReceipt,
  TransactionConflict,
  TransactionRefusal,
} from '../src/utils/transaction-core.mjs';

const nn = value => ({ termType: 'NamedNode', value });
const lit = (value, datatype = 'http://www.w3.org/2001/XMLSchema#string') => ({ termType: 'Literal', value: String(value), language: '', datatype: nn(datatype) });
const dg = { termType: 'DefaultGraph', value: '' };
const q = (s, p, o, g = dg) => ({ subject: nn(s), predicate: nn(p), object: typeof o === 'string' ? lit(o) : o, graph: g });

const aliceName = q('urn:alice', 'urn:name', 'Alice');
const bobName = q('urn:bob', 'urn:name', 'Bob');

test('quad keys distinguish datatype, language and graph', () => {
  const a = q('s', 'p', lit('1', 'x:int'), nn('g1'));
  const b = q('s', 'p', lit('1', 'x:string'), nn('g1'));
  const c = q('s', 'p', lit('1', 'x:int'), nn('g2'));
  assert.notEqual(quadKey(a), quadKey(b));
  assert.notEqual(quadKey(a), quadKey(c));
});

test('transaction commits additions and deletions atomically', () => {
  const store = new MemoryQuadStore([aliceName]);
  const tx = beginTransaction(store, { actor: 'test' });
  tx.delete(aliceName).add(bobName);
  const receipt = tx.commit();
  assert.equal(store.has?.(bobName), undefined);
  assert.deepEqual(store.values().map(x => x.subject.value), ['urn:bob']);
  assert.equal(receipt.committedVersion, 1);
  assert.equal(verifyTransactionReceipt(receipt).valid, true);
});

test('read-your-writes and delete visibility work before commit', () => {
  const store = new MemoryQuadStore([aliceName]);
  const tx = beginTransaction(store);
  tx.delete(aliceName).add(bobName);
  assert.equal(tx.has(aliceName), false);
  assert.equal(tx.has(bobName), true);
  assert.deepEqual(tx.read({ predicate: nn('urn:name') }).map(x => x.subject.value), ['urn:bob']);
});

test('savepoints restore writes, reads and assertions', () => {
  const store = new MemoryQuadStore();
  const tx = beginTransaction(store);
  tx.add(aliceName);
  const point = tx.savepoint('before-bob');
  tx.add(bobName).assert(() => false, 'should disappear');
  tx.rollbackTo(point);
  const receipt = tx.commit();
  assert.equal(receipt.operations.length, 1);
  assert.deepEqual(store.values().map(x => x.subject.value), ['urn:alice']);
});

test('snapshot isolation rejects write-write conflicts', () => {
  const store = new MemoryQuadStore([aliceName]);
  const first = beginTransaction(store);
  const second = beginTransaction(store);
  first.delete(aliceName).commit();
  second.delete(aliceName);
  assert.throws(() => second.commit(), TransactionConflict);
});

test('snapshot isolation permits stale reads of unrelated writes', () => {
  const store = new MemoryQuadStore([aliceName]);
  const first = beginTransaction(store, { isolation: 'snapshot' });
  first.read({ subject: nn('urn:alice') });
  beginTransaction(store).add(bobName).commit();
  first.add(q('urn:carol', 'urn:name', 'Carol'));
  assert.doesNotThrow(() => first.commit());
});

test('serializable isolation detects read-write conflicts', () => {
  const store = new MemoryQuadStore([aliceName]);
  const first = beginTransaction(store, { isolation: 'serializable' });
  first.read({ subject: nn('urn:alice') });
  beginTransaction(store).delete(aliceName).commit();
  assert.throws(() => first.add(bobName).commit(), TransactionConflict);
});

test('assertions refuse commit without partial mutation', () => {
  const store = new MemoryQuadStore();
  const tx = beginTransaction(store).add(aliceName).assert(view => view.length < 1, 'graph must stay empty');
  assert.throws(() => tx.commit(), TransactionRefusal);
  assert.equal(store.values().length, 0);
  assert.equal(store.version, 0);
});

test('idempotency keys return the original receipt', () => {
  const store = new MemoryQuadStore();
  const first = beginTransaction(store, { idempotencyKey: 'request-1' }).add(aliceName).commit();
  const second = beginTransaction(store, { idempotencyKey: 'request-1' }).add(bobName).commit();
  assert.equal(second.replayed, true);
  assert.equal(second.receiptHash, first.receiptHash);
  assert.deepEqual(store.values().map(x => x.subject.value), ['urn:alice']);
});

test('explicit rollback prevents later commit', () => {
  const store = new MemoryQuadStore();
  const tx = beginTransaction(store).add(aliceName);
  assert.equal(tx.rollback().state, 'ROLLED_BACK');
  assert.throws(() => tx.commit(), TransactionRefusal);
});
