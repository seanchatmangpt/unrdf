/**
 * Minimal v6-core test suite - <500ms execution
 * Only critical paths: Receipt creation, Delta validation, Contract basics
 * @module @unrdf/v6-core/test/implementations
 */

import { describe, it, expect } from 'vitest';
import {
  createContext,
  withReceipt,
  ReceiptProfileSchema,
  DeterministicContextSchema
} from '../src/receipt-pattern.mjs';

describe('v6-core implementations', () => {
  it('Receipt Creation - Basic Operation', async () => {
    const fn = async (x, y) => x + y;
    const wrapped = withReceipt(fn, {
      operation: 'add',
      profile: 'execution'
    });

    const ctx = createContext({ nodeId: 'test-node' });
    const { result, receipt } = await wrapped(ctx, 5, 3);

    expect(result).toBe(8);
    expect(receipt.id).toBeDefined();
    expect(receipt.profile).toBe('execution');
    expect(receipt.receiptHash.length).toBe(64);
  });

  it('Receipt Creation - Chain Verification', async () => {
    const fn = async (x) => x * 2;
    const wrapped = withReceipt(fn, { operation: 'double', profile: 'execution' });

    const ctx1 = createContext({ nodeId: 'node-1' });
    const { receipt: r1 } = await wrapped(ctx1, 10);
    expect(r1.previousReceiptHash).toBeNull();

    const ctx2 = createContext({
      nodeId: 'node-1',
      previousReceiptHash: r1.receiptHash
    });
    const { receipt: r2 } = await wrapped(ctx2, 20);
    expect(r2.previousReceiptHash).toBe(r1.receiptHash);
  });

  it('Delta Validation - Receipt Schema', () => {
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

    expect(() => ReceiptProfileSchema.parse(validReceipt)).not.toThrow();
  });

  it('Delta Validation - Context Schema', () => {
    const ctx = {
      t_ns: BigInt(Date.now()) * 1000000n,
      timestamp_iso: new Date().toISOString(),
      nodeId: 'node-1',
      previousReceiptHash: null
    };

    expect(() => DeterministicContextSchema.parse(ctx)).not.toThrow();
  });

  it('Contract Basics - Valid Format', () => {
    const ctx = createContext({ nodeId: 'test-node' });
    expect(ctx.t_ns).toBeDefined();
    expect(ctx.timestamp_iso).toBeDefined();
    expect(ctx.nodeId).toBe('test-node');
    expect(ctx.previousReceiptHash).toBeNull();
  });

  it('Contract Basics - Invalid Hash Rejection', () => {
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

    expect(() => ReceiptProfileSchema.parse(invalidReceipt)).toThrow();
  });

  it('Smoke Test - E2E Receipt Generation', async () => {
    const operation = async (data) => ({ processed: true, ...data });
    const wrapped = withReceipt(operation, { operation: 'process', profile: 'delta' });

    const ctx = createContext({ nodeId: 'smoke-test' });
    const { result, receipt } = await wrapped(ctx, { value: 42 });

    expect(result).toEqual({ processed: true, value: 42 });
    expect(receipt.receiptHash).toBeDefined();
    expect(receipt.id).toBeDefined();
    expect(receipt.t_ns).toBeDefined();
  });
});
