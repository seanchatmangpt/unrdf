/**
 * Minimal v6-core test suite - <500ms execution
 * Only critical paths: Receipt creation, Delta validation, Contract basics
 * @module @unrdf/v6-core/test/implementations
 */

import { test } from 'node:test';
import assert from 'node:assert/strict';
import {
  createContext,
  withReceipt,
  ReceiptProfileSchema,
  DeterministicContextSchema
} from '../src/receipt-pattern.mjs';

// 1. Receipt Creation - Basic Operation
await test('Receipt Creation - Basic Operation', async () => {
  const fn = async (x, y) => x + y;
  const wrapped = withReceipt(fn, {
    operation: 'add',
    profile: 'execution'
  });

  const ctx = createContext({ nodeId: 'test-node' });
  const { result, receipt } = await wrapped(ctx, 5, 3);

  assert.equal(result, 8);
  assert.ok(receipt.id);
  assert.equal(receipt.profile, 'execution');
  assert.equal(receipt.receiptHash.length, 64);
});

// 2. Receipt Creation - Chain Verification
await test('Receipt Creation - Chain Verification', async () => {
  const fn = async (x) => x * 2;
  const wrapped = withReceipt(fn, { operation: 'double', profile: 'execution' });

  const ctx1 = createContext({ nodeId: 'node-1' });
  const { receipt: r1 } = await wrapped(ctx1, 10);
  assert.equal(r1.previousReceiptHash, null);

  const ctx2 = createContext({
    nodeId: 'node-1',
    previousReceiptHash: r1.receiptHash
  });
  const { receipt: r2 } = await wrapped(ctx2, 20);
  assert.equal(r2.previousReceiptHash, r1.receiptHash);
});

// 3. Delta Validation - Receipt Schema
await test('Delta Validation - Receipt Schema', () => {
  const validReceipt = {
    id: '123e4567-e89b-12d3-a456-426614174000',
    profile: 'delta',
    previousReceiptHash: null,
    payloadHash: 'a'.repeat(64),
    receiptHash: 'b'.repeat(64),
    t_ns: BigInt(Date.now()) * 1000000n,
    timestamp_iso: new Date().toISOString(),
    context: { nodeId: 'node-1' },
    payload: { operation: 'test' }
  };

  assert.doesNotThrow(() => ReceiptProfileSchema.parse(validReceipt));
});

// 4. Delta Validation - Context Schema
await test('Delta Validation - Context Schema', () => {
  const ctx = {
    t_ns: BigInt(Date.now()) * 1000000n,
    timestamp_iso: new Date().toISOString(),
    nodeId: 'node-1',
    previousReceiptHash: null
  };

  assert.doesNotThrow(() => DeterministicContextSchema.parse(ctx));
});

// 5. Contract Basics - Valid Format
await test('Contract Basics - Valid Format', () => {
  const ctx = createContext({ nodeId: 'test-node' });
  assert.ok(ctx.t_ns);
  assert.ok(ctx.timestamp_iso);
  assert.equal(ctx.nodeId, 'test-node');
  assert.equal(ctx.previousReceiptHash, null);
});

// 6. Contract Basics - Invalid Hash Rejection
await test('Contract Basics - Invalid Hash Rejection', () => {
  const invalidReceipt = {
    id: '123e4567-e89b-12d3-a456-426614174000',
    profile: 'delta',
    previousReceiptHash: 'invalid',
    payloadHash: 'a'.repeat(64),
    receiptHash: 'b'.repeat(64),
    t_ns: BigInt(Date.now()) * 1000000n,
    timestamp_iso: new Date().toISOString(),
    context: { nodeId: 'node-1' },
    payload: { operation: 'test' }
  };

  assert.throws(() => ReceiptProfileSchema.parse(invalidReceipt));
});

// 7. Smoke Test - E2E Receipt Generation
await test('Smoke Test - E2E Receipt Generation', async () => {
  const operation = async (data) => ({ processed: true, ...data });
  const wrapped = withReceipt(operation, { operation: 'process', profile: 'delta' });

  const ctx = createContext({ nodeId: 'smoke-test' });
  const { result, receipt } = await wrapped(ctx, { value: 42 });

  assert.deepEqual(result, { processed: true, value: 42 });
  assert.ok(receipt.receiptHash);
  assert.ok(receipt.id);
  assert.ok(receipt.t_ns);
});
