#!/usr/bin/env node
/**
 * Smoke test for receipts-kernel
 * Verifies basic functionality without full test framework
 */

import {
  createReceipt,
  verifyReceipt,
  chainReceipts,
  merkleBatch,
} from './src/receipts-kernel.mjs';

console.log('=== Receipts Kernel Smoke Test ===\n');

let passed = 0;
let failed = 0;

async function test(name, fn) {
  try {
    await fn();
    console.log(`✅ ${name}`);
    passed++;
  } catch (error) {
    console.error(`❌ ${name}: ${error.message}`);
    failed++;
  }
}

// Test 1: Create receipt
await test('Create receipt', async () => {
  const receipt = await createReceipt('test-event', { value: 42 });

  if (!receipt.id) throw new Error('Missing id');
  if (!receipt.hash) throw new Error('Missing hash');
  if (!receipt.timestamp) throw new Error('Missing timestamp');
  if (!receipt.eventType) throw new Error('Missing eventType');
  if (receipt.eventType !== 'test-event') throw new Error('Wrong eventType');
  if (!receipt.payload || receipt.payload.value !== 42) throw new Error('Wrong payload');
});

// Test 2: Verify valid receipt
await test('Verify valid receipt', async () => {
  const receipt = await createReceipt('verify-test', { data: 'test' });
  const result = await verifyReceipt(receipt);

  if (!result.valid) throw new Error(`Verification failed: ${result.reason}`);
  if (result.receiptId !== receipt.id) throw new Error('Wrong receipt ID');
});

// Test 3: Detect tampered receipt
await test('Detect tampered receipt', async () => {
  const receipt = await createReceipt('tamper-test', { value: 100 });
  const tampered = { ...receipt, payload: { value: 999 } };
  const result = await verifyReceipt(tampered);

  if (result.valid) throw new Error('Should have detected tampering');
  if (!result.reason?.includes('Hash mismatch')) throw new Error('Wrong error reason');
});

// Test 4: Deterministic hashing
await test('Deterministic hashing', async () => {
  process.env.DETERMINISTIC = '1';

  const r1 = await createReceipt('deterministic', { value: 42 });
  const r2 = await createReceipt('deterministic', { value: 42 });

  if (r1.hash !== r2.hash) throw new Error('Hashes should match in deterministic mode');

  delete process.env.DETERMINISTIC;
});

// Test 5: Chain receipts
await test('Chain receipts', async () => {
  const receipts = await Promise.all([
    createReceipt('chain-1', { step: 1 }),
    createReceipt('chain-2', { step: 2 }),
    createReceipt('chain-3', { step: 3 }),
  ]);

  const chain = await chainReceipts(receipts);

  if (!chain.valid) throw new Error(`Chain invalid: ${chain.reason}`);
  if (!chain.root) throw new Error('Missing root');
  if (!chain.root.startsWith('0x')) throw new Error('Root should be hex with 0x prefix');
  if (chain.proofs.length !== 3) throw new Error('Wrong number of proofs');
  if (chain.count !== 3) throw new Error('Wrong count');
});

// Test 6: Merkle batch
await test('Merkle batch', async () => {
  const receipts = await Promise.all([
    createReceipt('batch-1', { value: 1 }),
    createReceipt('batch-2', { value: 2 }),
    createReceipt('batch-3', { value: 3 }),
    createReceipt('batch-4', { value: 4 }),
  ]);

  const batch = await merkleBatch(receipts);

  if (!batch.root) throw new Error('Missing root');
  if (!batch.tree) throw new Error('Missing tree');
  if (batch.tree.leafCount !== 4) throw new Error('Wrong leaf count');
  if (batch.proofs.length !== 4) throw new Error('Wrong number of proofs');
});

// Test 7: Different receipt types
await test('Different receipt types', async () => {
  const kgcReceipt = await createReceipt('snapshot', { hash: 'abc' }, { receiptType: 'kgc' });
  const blockchainReceipt = await createReceipt('anchor', { tx: '0x1' }, { receiptType: 'blockchain' });
  const hookReceipt = await createReceipt('validate', { valid: true }, { receiptType: 'hook' });

  if (kgcReceipt.receiptType !== 'kgc') throw new Error('Wrong KGC receipt type');
  if (blockchainReceipt.receiptType !== 'blockchain') throw new Error('Wrong blockchain receipt type');
  if (hookReceipt.receiptType !== 'hook') throw new Error('Wrong hook receipt type');

  // Verify all
  const results = await Promise.all([
    verifyReceipt(kgcReceipt),
    verifyReceipt(blockchainReceipt),
    verifyReceipt(hookReceipt),
  ]);

  for (const result of results) {
    if (!result.valid) throw new Error(`Receipt invalid: ${result.reason}`);
  }
});

// Test 8: Chain with proof
await test('Chain with optional proof', async () => {
  const proof = { merkleProof: { leaf: '0xabc', proof: [], root: '0xdef' } };
  const receipt = await createReceipt('with-proof', { data: 'test' }, { proof });

  if (!receipt.proof) throw new Error('Missing proof');
  const result = await verifyReceipt(receipt);
  if (!result.valid) throw new Error('Receipt with proof should be valid');
  if (!result.details.hasProof) throw new Error('Should indicate proof present');
});

// Summary
console.log(`\n=== Summary ===`);
console.log(`Passed: ${passed}`);
console.log(`Failed: ${failed}`);
console.log(`Total: ${passed + failed}`);

if (failed > 0) {
  process.exit(1);
} else {
  console.log('\n✅ All smoke tests passed!');
  process.exit(0);
}
