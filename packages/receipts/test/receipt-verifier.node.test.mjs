import test from 'node:test';
import assert from 'node:assert/strict';
import {
  canonicalJson,
  createReceipt,
  verifyReceipt,
  ReceiptLedger,
  buildReceiptMerkleTree,
  createReceiptMerkleProof,
  verifyReceiptMerkleProof,
  createSelectiveDisclosure,
  verifySelectiveDisclosure,
  verifyReplay,
} from '../src/receipt-verifier.mjs';

test('canonical JSON ignores object key order', () => {
  assert.equal(canonicalJson({ b: 2, a: 1 }), canonicalJson({ a: 1, b: 2 }));
});

test('receipt binds inputs, outputs and evidence', () => {
  const receipt = createReceipt({ subject: 'pkg', action: 'test', inputs: { a: 1 }, outputs: { ok: true }, evidence: ['log'], timestamp: 1 });
  assert.equal(verifyReceipt(receipt, { inputs: { a: 1 }, outputs: { ok: true }, evidence: ['log'] }).valid, true);
  assert.equal(verifyReceipt(receipt, { outputs: { ok: false } }).valid, false);
});

test('receipt tampering is detected', () => {
  const receipt = createReceipt({ subject: 'pkg', action: 'build', outputs: 1 });
  assert.equal(verifyReceipt({ ...receipt, actor: 'attacker' }).valid, false);
});

test('HMAC signatures verify and reject wrong keys', () => {
  const receipt = createReceipt({ subject: 'pkg', action: 'release' }, { hmacKey: 'secret' });
  assert.equal(verifyReceipt(receipt, { hmacKey: 'secret' }).valid, true);
  assert.equal(verifyReceipt(receipt, { hmacKey: 'wrong' }).valid, false);
});

test('ledger constructs a verified hash chain', () => {
  const ledger = new ReceiptLedger({ hmacKey: 'key' });
  const first = ledger.append({ subject: 'a', action: 'build', timestamp: 1 });
  const second = ledger.append({ subject: 'b', action: 'test', timestamp: 2 });
  assert.equal(second.previousReceiptHash, first.receiptHash);
  assert.equal(ledger.verify().valid, true);
});

test('ledger idempotently returns an existing receipt ID', () => {
  const ledger = new ReceiptLedger();
  const first = ledger.append({ receiptId: 'fixed', subject: 'a', action: 'x' });
  const second = ledger.append({ receiptId: 'fixed', subject: 'different', action: 'y' });
  assert.equal(second.receiptHash, first.receiptHash);
  assert.equal(ledger.export().length, 1);
});

test('chain verification detects reordered receipts', () => {
  const ledger = new ReceiptLedger();
  ledger.append({ subject: 'a', action: 'x' });
  ledger.append({ subject: 'b', action: 'y' });
  const reversed = ledger.export().reverse();
  const other = new ReceiptLedger();
  other.receipts = reversed;
  assert.equal(other.verify().valid, false);
});

test('Merkle proofs verify membership for odd leaf counts', () => {
  const receipts = ['a', 'b', 'c'].map(subject => createReceipt({ subject, action: 'test' }));
  const tree = buildReceiptMerkleTree(receipts);
  for (let index = 0; index < receipts.length; index++) assert.equal(verifyReceiptMerkleProof(receipts[index], createReceiptMerkleProof(tree, index)), true);
});

test('Merkle proofs reject a different receipt', () => {
  const receipts = ['a', 'b'].map(subject => createReceipt({ subject, action: 'test' }));
  const proof = createReceiptMerkleProof(buildReceiptMerkleTree(receipts), 0);
  assert.equal(verifyReceiptMerkleProof(createReceipt({ subject: 'x', action: 'test' }), proof), false);
});

test('selective disclosures reveal only selected fields', () => {
  const disclosure = createSelectiveDisclosure({ user: { email: 'a@b.test', role: 'admin' }, secret: 'hidden' }, ['user.role'], { salt: 'salt' });
  assert.deepEqual(disclosure.disclosed, { 'user.role': 'admin' });
  assert.equal(verifySelectiveDisclosure(disclosure).valid, true);
});

test('selective disclosure tampering is detected', () => {
  const disclosure = createSelectiveDisclosure({ role: 'admin' }, ['role'], { salt: 'salt' });
  disclosure.disclosed.role = 'guest';
  assert.equal(verifySelectiveDisclosure(disclosure).valid, false);
});

test('replay verification distinguishes match and difference', () => {
  const receipt = createReceipt({ subject: 'pkg', action: 'run', inputs: [1], outputs: [2], exitCode: 0 });
  assert.equal(verifyReplay(receipt, { inputs: [1], outputs: [2], exitCode: 0 }).state, 'REPLAY_MATCH');
  assert.equal(verifyReplay(receipt, { inputs: [1], outputs: [3], exitCode: 0 }).state, 'REPLAY_DIFFERENCE');
});
