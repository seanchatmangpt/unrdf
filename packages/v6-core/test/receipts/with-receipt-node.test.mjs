/**
 * P0-001: withReceipt HOF Tests (Node.js test runner)
 *
 * Validates deterministic receipt generation and idempotency
 */

import { describe, it } from 'node:test';
import assert from 'node:assert';
import { withReceipt, createReceiptChain, verifyIdempotency } from '../../src/receipts/with-receipt.mjs';

describe('P0-001: withReceipt HOF', () => {
  it('wraps function and generates receipt', async () => {
    const double = withReceipt(
      (x) => x * 2,
      {
        operation: 'double',
        getTimestamp: () => 1704110400000000000n,
      }
    );

    const { result, receipt } = await double(5);

    assert.strictEqual(result, 10);
    assert.ok(receipt);
    assert.ok(receipt.id);
    assert.ok(receipt.receiptHash);
    assert.strictEqual(receipt.operation_name, 'double');
    assert.ok(receipt.inputs_hash);
    assert.ok(receipt.outputs_hash);
    assert.strictEqual(receipt.timestamp_provided, 'injected');

    console.log('✅ withReceipt generates receipts correctly');
  });

  it('is deterministic with injected timestamp', async () => {
    const fixedTimestamp = () => 1704110400000000000n;

    const processData = withReceipt(
      (data) => data.map((x) => x * 2),
      {
        operation: 'processData',
        getTimestamp: fixedTimestamp,
      }
    );

    const run1 = await processData([1, 2, 3]);
    const run2 = await processData([1, 2, 3]);

    // Same inputs + same timestamp → same hashes
    assert.strictEqual(run1.receipt.inputs_hash, run2.receipt.inputs_hash);
    assert.strictEqual(run1.receipt.outputs_hash, run2.receipt.outputs_hash);
    assert.strictEqual(run1.receipt.receiptHash, run2.receipt.receiptHash);
    assert.deepStrictEqual(run1.result, run2.result);

    console.log('✅ Deterministic: Same inputs → Same receipt hash');
    console.log(`   Receipt hash: ${run1.receipt.receiptHash}`);
  });

  it('verifies idempotency', async () => {
    const wrapped = withReceipt(
      (x) => x * 2,
      {
        operation: 'double',
        getTimestamp: () => 1704110400000000000n,
      }
    );

    const check = await verifyIdempotency(wrapped, [5]);

    assert.strictEqual(check.idempotent, true);
    assert.strictEqual(check.hashMatch, true);
    assert.strictEqual(check.receipt1.receiptHash, check.receipt2.receiptHash);

    console.log('✅ Idempotency verified');
    console.log(`   Run 1 hash: ${check.receipt1.receiptHash}`);
    console.log(`   Run 2 hash: ${check.receipt2.receiptHash}`);
    console.log(`   Match: ${check.idempotent}`);
  });

  it('creates receipt chains', async () => {
    const fixedTimestamp = () => 1704110400000000000n;

    const chain = await createReceiptChain([
      {
        fn: () => ({ step: 1, value: 10 }),
        context: { operation: 'step1', getTimestamp: fixedTimestamp },
      },
      {
        fn: () => ({ step: 2, value: 20 }),
        context: { operation: 'step2', getTimestamp: fixedTimestamp },
      },
      {
        fn: () => ({ step: 3, value: 30 }),
        context: { operation: 'step3', getTimestamp: fixedTimestamp },
      },
    ]);

    assert.strictEqual(chain.length, 3);

    // Verify chain links
    assert.strictEqual(chain[0].receipt.previousHash, null); // Genesis
    assert.strictEqual(chain[1].receipt.previousHash, chain[0].receipt.receiptHash);
    assert.strictEqual(chain[2].receipt.previousHash, chain[1].receipt.receiptHash);

    console.log('✅ Receipt chain created successfully');
    console.log(`   Chain length: ${chain.length}`);
    console.log(`   Step 1 hash: ${chain[0].receipt.receiptHash}`);
    console.log(`   Step 2 previous: ${chain[1].receipt.previousHash}`);
    console.log(`   Step 3 previous: ${chain[2].receipt.previousHash}`);
  });

  it('includes all P0-001 required fields', async () => {
    const wrapped = withReceipt(
      (x) => x,
      {
        operation: 'identity',
        getTimestamp: () => 1704110400000000000n,
      }
    );

    const { receipt } = await wrapped('test');

    // P0-001 required fields
    assert.ok(receipt.id, 'id missing');
    assert.ok(receipt.receiptHash, 'receiptHash missing');
    assert.ok(receipt.merkle_proof !== undefined, 'merkle_proof missing');
    assert.strictEqual(receipt.timestamp_provided, 'injected');
    assert.strictEqual(receipt.operation_name, 'identity');
    assert.ok(receipt.inputs_hash, 'inputs_hash missing');
    assert.ok(receipt.outputs_hash, 'outputs_hash missing');

    // Base receipt fields
    assert.ok(receipt.t_ns, 't_ns missing');
    assert.ok(receipt.timestamp_iso, 'timestamp_iso missing');
    assert.ok(receipt.payloadHash, 'payloadHash missing');

    console.log('✅ All P0-001 required fields present');
    console.log(`   id: ${receipt.id}`);
    console.log(`   hash: ${receipt.receiptHash}`);
    console.log(`   merkle_proof: ${receipt.merkle_proof}`);
    console.log(`   timestamp_provided: ${receipt.timestamp_provided}`);
    console.log(`   operation_name: ${receipt.operation_name}`);
    console.log(`   inputs_hash: ${receipt.inputs_hash}`);
    console.log(`   outputs_hash: ${receipt.outputs_hash}`);
  });

  it('throws error if fn is not a function', () => {
    assert.throws(
      () => withReceipt('not a function'),
      /withReceipt requires a function/
    );

    console.log('✅ Error handling works correctly');
  });
});

console.log('\n📊 P0-001 Test Summary: withReceipt HOF');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
