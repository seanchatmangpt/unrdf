/**
 * Tests for P0-001: withReceipt HOF
 *
 * Validates:
 * - Deterministic receipt generation
 * - Idempotency (same inputs → same receipt)
 * - No Date.now() usage
 * - Hash chain correctness
 * - Input/output hashing
 */

import { describe, it, expect } from 'vitest';
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

    expect(result).toBe(10);
    expect(receipt).toBeDefined();
    expect(receipt.id).toBeDefined();
    expect(receipt.receiptHash).toBeDefined();
    expect(receipt.operation_name).toBe('double');
    expect(receipt.inputs_hash).toBeDefined();
    expect(receipt.outputs_hash).toBeDefined();
    expect(receipt.timestamp_provided).toBe('injected');
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
    expect(run1.receipt.inputs_hash).toBe(run2.receipt.inputs_hash);
    expect(run1.receipt.outputs_hash).toBe(run2.receipt.outputs_hash);
    expect(run1.receipt.receiptHash).toBe(run2.receipt.receiptHash);
    expect(run1.result).toEqual(run2.result);
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

    expect(check.idempotent).toBe(true);
    expect(check.hashMatch).toBe(true);
    expect(check.receipt1.receiptHash).toBe(check.receipt2.receiptHash);
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

    expect(chain).toHaveLength(3);

    // Verify chain links
    expect(chain[0].receipt.previousHash).toBeNull(); // Genesis
    expect(chain[1].receipt.previousHash).toBe(chain[0].receipt.receiptHash);
    expect(chain[2].receipt.previousHash).toBe(chain[1].receipt.receiptHash);

    // Verify all have required fields
    chain.forEach((item) => {
      expect(item.receipt.operation_name).toBeDefined();
      expect(item.receipt.inputs_hash).toBeDefined();
      expect(item.receipt.outputs_hash).toBeDefined();
      expect(item.receipt.timestamp_provided).toBe('injected');
    });
  });

  it('hashes inputs and outputs deterministically', async () => {
    const wrapped = withReceipt(
      (a, b) => a + b,
      {
        operation: 'add',
        getTimestamp: () => 1704110400000000000n,
      }
    );

    const { receipt: r1 } = await wrapped(1, 2);
    const { receipt: r2 } = await wrapped(1, 2);

    expect(r1.inputs_hash).toBe(r2.inputs_hash);
    expect(r1.outputs_hash).toBe(r2.outputs_hash);
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
    expect(receipt.id).toBeDefined();
    expect(receipt.receiptHash).toBeDefined();
    expect(receipt.merkle_proof).toBeDefined(); // null initially
    expect(receipt.timestamp_provided).toBe('injected');
    expect(receipt.operation_name).toBe('identity');
    expect(receipt.inputs_hash).toBeDefined();
    expect(receipt.outputs_hash).toBeDefined();

    // Base receipt fields
    expect(receipt.t_ns).toBeDefined();
    expect(receipt.timestamp_iso).toBeDefined();
    expect(receipt.payloadHash).toBeDefined();
  });

  it('throws error if fn is not a function', () => {
    expect(() => withReceipt('not a function')).toThrow(
      'withReceipt requires a function'
    );
  });
});
