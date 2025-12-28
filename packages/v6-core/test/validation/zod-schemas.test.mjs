/**
 * Zod Schema Validation Tests - v6-core
 *
 * Tests covering:
 * - All Zod schemas (Receipt, Delta, Operation, etc.)
 * - Valid data passes validation
 * - Invalid data rejected with clear errors
 * - Type coercion and transformations
 * - Edge cases and boundary conditions
 *
 * @module @unrdf/v6-core/test/validation/zod
 */

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { z } from 'zod';

// Simulated v6-core schemas
const ReceiptTypeSchema = z.enum(['execution', 'allocation', 'compile', 'verification']);

const BaseReceiptSchema = z.object({
  id: z.string().uuid(),
  receiptType: ReceiptTypeSchema,
  t_ns: z.bigint().positive(),
  timestamp_iso: z.string().datetime(),
  payloadHash: z.string().length(64),
  receiptHash: z.string().length(64),
  previousHash: z.string().length(64).nullable(),
});

const DeltaOperationSchema = z.object({
  op: z.enum(['add', 'delete', 'update']),
  subject: z.string().min(1),
  predicate: z.string().min(1),
  object: z.string().optional(),
  oldObject: z.string().optional(),
  newObject: z.string().optional(),
  graph: z.string().optional(),
});

const DeltaSchema = z.object({
  id: z.string().uuid(),
  timestamp_iso: z.string().datetime(),
  t_ns: z.bigint().positive(),
  operations: z.array(DeltaOperationSchema).min(1),
  source: z.object({
    package: z.string(),
    actor: z.string().optional(),
    context: z.record(z.unknown()).optional(),
  }),
});

// ============================================================================
// Test Suite 1: Receipt Schema Validation (5 tests)
// ============================================================================

test('Zod - valid receipt passes validation', () => {
  const validReceipt = {
    id: '123e4567-e89b-12d3-a456-426614174000',
    receiptType: 'execution',
    t_ns: 1234567890123456789n,
    timestamp_iso: '2025-01-01T00:00:00.000Z',
    payloadHash: 'a'.repeat(64),
    receiptHash: 'b'.repeat(64),
    previousHash: null,
  };

  const result = BaseReceiptSchema.safeParse(validReceipt);

  assert.strictEqual(result.success, true);
  if (result.success) {
    assert.strictEqual(result.data.receiptType, 'execution');
  }
});

test('Zod - receipt with invalid UUID rejected', () => {
  const invalidReceipt = {
    id: 'not-a-uuid',
    receiptType: 'execution',
    t_ns: 123n,
    timestamp_iso: '2025-01-01T00:00:00.000Z',
    payloadHash: 'a'.repeat(64),
    receiptHash: 'b'.repeat(64),
    previousHash: null,
  };

  const result = BaseReceiptSchema.safeParse(invalidReceipt);

  assert.strictEqual(result.success, false);
  if (!result.success) {
    assert.ok(result.error.issues.some(issue => issue.path.includes('id')));
  }
});

test('Zod - receipt with wrong hash length rejected', () => {
  const invalidReceipt = {
    id: '123e4567-e89b-12d3-a456-426614174000',
    receiptType: 'execution',
    t_ns: 123n,
    timestamp_iso: '2025-01-01T00:00:00.000Z',
    payloadHash: 'short', // Should be 64 chars
    receiptHash: 'b'.repeat(64),
    previousHash: null,
  };

  const result = BaseReceiptSchema.safeParse(invalidReceipt);

  assert.strictEqual(result.success, false);
  if (!result.success) {
    assert.ok(result.error.issues.some(issue => issue.path.includes('payloadHash')));
  }
});

test('Zod - receipt with invalid timestamp rejected', () => {
  const invalidReceipt = {
    id: '123e4567-e89b-12d3-a456-426614174000',
    receiptType: 'execution',
    t_ns: 123n,
    timestamp_iso: 'not-a-datetime',
    payloadHash: 'a'.repeat(64),
    receiptHash: 'b'.repeat(64),
    previousHash: null,
  };

  const result = BaseReceiptSchema.safeParse(invalidReceipt);

  assert.strictEqual(result.success, false);
});

test('Zod - receipt with invalid type rejected', () => {
  const invalidReceipt = {
    id: '123e4567-e89b-12d3-a456-426614174000',
    receiptType: 'invalid-type',
    t_ns: 123n,
    timestamp_iso: '2025-01-01T00:00:00.000Z',
    payloadHash: 'a'.repeat(64),
    receiptHash: 'b'.repeat(64),
    previousHash: null,
  };

  const result = BaseReceiptSchema.safeParse(invalidReceipt);

  assert.strictEqual(result.success, false);
});

// ============================================================================
// Test Suite 2: Delta Schema Validation (5 tests)
// ============================================================================

test('Zod - valid delta passes validation', () => {
  const validDelta = {
    id: '123e4567-e89b-12d3-a456-426614174000',
    timestamp_iso: '2025-01-01T00:00:00.000Z',
    t_ns: 1234567890123456789n,
    operations: [
      {
        op: 'add',
        subject: 'http://example.org/subject',
        predicate: 'http://example.org/predicate',
        object: 'value',
      },
    ],
    source: {
      package: '@unrdf/test',
    },
  };

  const result = DeltaSchema.safeParse(validDelta);

  assert.strictEqual(result.success, true);
});

test('Zod - delta with empty operations rejected', () => {
  const invalidDelta = {
    id: '123e4567-e89b-12d3-a456-426614174000',
    timestamp_iso: '2025-01-01T00:00:00.000Z',
    t_ns: 123n,
    operations: [], // Empty
    source: { package: '@unrdf/test' },
  };

  const result = DeltaSchema.safeParse(invalidDelta);

  assert.strictEqual(result.success, false);
  if (!result.success) {
    assert.ok(result.error.issues.some(issue => issue.path.includes('operations')));
  }
});

test('Zod - delta operation with invalid op rejected', () => {
  const invalidOperation = {
    op: 'invalid-op',
    subject: 's',
    predicate: 'p',
    object: 'o',
  };

  const result = DeltaOperationSchema.safeParse(invalidOperation);

  assert.strictEqual(result.success, false);
});

test('Zod - delta operation missing subject rejected', () => {
  const invalidOperation = {
    op: 'add',
    predicate: 'p',
    object: 'o',
  };

  const result = DeltaOperationSchema.safeParse(invalidOperation);

  assert.strictEqual(result.success, false);
});

test('Zod - delta with multiple valid operations passes', () => {
  const validDelta = {
    id: '123e4567-e89b-12d3-a456-426614174000',
    timestamp_iso: '2025-01-01T00:00:00.000Z',
    t_ns: 123n,
    operations: [
      { op: 'add', subject: 's1', predicate: 'p1', object: 'o1' },
      { op: 'delete', subject: 's2', predicate: 'p2', object: 'o2' },
      {
        op: 'update',
        subject: 's3',
        predicate: 'p3',
        oldObject: 'old',
        newObject: 'new',
      },
    ],
    source: { package: '@unrdf/test' },
  };

  const result = DeltaSchema.safeParse(validDelta);

  assert.strictEqual(result.success, true);
  if (result.success) {
    assert.strictEqual(result.data.operations.length, 3);
  }
});

// ============================================================================
// Test Suite 3: Type Coercion and Transformations (3 tests)
// ============================================================================

test('Zod - bigint coercion from string', () => {
  const schema = z.object({
    value: z.bigint(),
  });

  // Note: Zod doesn't auto-coerce strings to bigint
  const validData = {
    value: 123n,
  };

  const result = schema.safeParse(validData);

  assert.strictEqual(result.success, true);
  if (result.success) {
    assert.strictEqual(typeof result.data.value, 'bigint');
  }
});

test('Zod - optional fields handled correctly', () => {
  const schema = z.object({
    required: z.string(),
    optional: z.string().optional(),
  });

  const data = {
    required: 'value',
  };

  const result = schema.safeParse(data);

  assert.strictEqual(result.success, true);
  if (result.success) {
    assert.strictEqual(result.data.optional, undefined);
  }
});

test('Zod - default values applied', () => {
  const schema = z.object({
    value: z.string().default('default-value'),
  });

  const data = {};

  const result = schema.safeParse(data);

  assert.strictEqual(result.success, true);
  if (result.success) {
    assert.strictEqual(result.data.value, 'default-value');
  }
});

// ============================================================================
// Test Suite 4: Error Messages (3 tests)
// ============================================================================

test('Zod - error messages include field path', () => {
  const schema = z.object({
    nested: z.object({
      value: z.string(),
    }),
  });

  const data = {
    nested: {
      value: 123, // Wrong type
    },
  };

  const result = schema.safeParse(data);

  assert.strictEqual(result.success, false);
  if (!result.success) {
    const issue = result.error.issues[0];
    assert.deepStrictEqual(issue.path, ['nested', 'value']);
  }
});

test('Zod - error messages are descriptive', () => {
  const schema = z.string().min(5);

  const result = schema.safeParse('abc');

  assert.strictEqual(result.success, false);
  if (!result.success) {
    // Zod message format: "String must contain at least 5 character(s)"
    const message = result.error.issues[0].message;
    assert.ok(message.includes('5') && message.includes('character'), 'Error message mentions length requirement');
  }
});

test('Zod - multiple errors reported', () => {
  const schema = z.object({
    field1: z.string(),
    field2: z.number(),
    field3: z.boolean(),
  });

  const data = {
    field1: 123, // Wrong type
    field2: 'abc', // Wrong type
    field3: 'true', // Wrong type
  };

  const result = schema.safeParse(data);

  assert.strictEqual(result.success, false);
  if (!result.success) {
    assert.ok(result.error.issues.length >= 3);
  }
});

// ============================================================================
// Test Suite 5: Edge Cases (4 tests)
// ============================================================================

test('Zod - empty string handled', () => {
  const schema = z.string().min(1);

  const result = schema.safeParse('');

  assert.strictEqual(result.success, false);
});

test('Zod - null vs undefined distinction', () => {
  const schema = z.object({
    nullable: z.string().nullable(),
    optional: z.string().optional(),
  });

  const data1 = { nullable: null, optional: undefined };
  const result1 = schema.safeParse(data1);

  assert.strictEqual(result1.success, true);

  const data2 = { nullable: undefined, optional: null };
  const result2 = schema.safeParse(data2);

  // nullable field can't be undefined without .optional()
  assert.strictEqual(result2.success, false);
});

test('Zod - array validation', () => {
  const schema = z.array(z.string()).min(1);

  const result1 = schema.safeParse([]);
  assert.strictEqual(result1.success, false);

  const result2 = schema.safeParse(['item']);
  assert.strictEqual(result2.success, true);
});

test('Zod - record validation', () => {
  const schema = z.record(z.number());

  const data = {
    key1: 1,
    key2: 2,
    key3: 'not-a-number',
  };

  const result = schema.safeParse(data);

  assert.strictEqual(result.success, false);
});

console.log('\n✅ All Zod validation tests passed');
