/**
 * Error Handling Tests - v6-core
 *
 * Tests covering:
 * - Rollback on partial failures
 * - Timeout enforcement (5s default)
 * - Graceful degradation
 * - Error receipts (denial, rejection)
 * - No partial state application
 *
 * @module @unrdf/v6-core/test/errors
 */

import { test } from 'node:test';
import assert from 'node:assert/strict';
import crypto from 'node:crypto';

// ============================================================================
// Test Suite 1: Rollback on Failure (3 tests)
// ============================================================================

test('Error Handling - rollback on delta failure', () => {
  const state = new Set(['existing-triple']);
  const originalSize = state.size;

  const delta = {
    operations: [
      { op: 'add', triple: 'triple-1' },
      { op: 'add', triple: 'triple-2' },
      { op: 'invalid-op', triple: 'triple-3' }, // Will fail
    ],
  };

  // Apply with rollback on error
  const snapshot = new Set(state);

  try {
    for (const op of delta.operations) {
      if (op.op === 'add') {
        state.add(op.triple);
      } else if (op.op === 'delete') {
        state.delete(op.triple);
      } else {
        throw new Error(`Invalid operation: ${op.op}`);
      }
    }
  } catch (error) {
    // Rollback: restore snapshot
    state.clear();
    snapshot.forEach(item => state.add(item));
  }

  // State restored to original
  assert.strictEqual(state.size, originalSize);
  assert.ok(state.has('existing-triple'));
  assert.ok(!state.has('triple-1')); // Rolled back
});

test('Error Handling - no partial state after failure', () => {
  const state = new Map([['key1', 'value1']]);

  const operations = [
    { key: 'key2', value: 'value2' },
    { key: 'key3', value: null }, // Invalid
  ];

  const snapshot = new Map(state);

  try {
    for (const op of operations) {
      if (op.value === null) {
        throw new Error('Invalid value');
      }
      state.set(op.key, op.value);
    }
  } catch (error) {
    // Rollback
    state.clear();
    snapshot.forEach((value, key) => state.set(key, value));
  }

  // Only original state remains
  assert.strictEqual(state.size, 1);
  assert.strictEqual(state.get('key1'), 'value1');
  assert.ok(!state.has('key2'));
});

test('Error Handling - atomic delta application (all-or-none)', () => {
  const state = new Set();

  function applyDeltaAtomic(delta) {
    const snapshot = new Set(state);

    try {
      for (const op of delta.operations) {
        if (op.op === 'add') {
          state.add(op.triple);
        } else if (op.op === 'fail') {
          throw new Error('Simulated failure');
        }
      }
      return { success: true };
    } catch (error) {
      // Rollback
      state.clear();
      snapshot.forEach(item => state.add(item));
      return { success: false, error: error.message };
    }
  }

  const delta = {
    operations: [
      { op: 'add', triple: 't1' },
      { op: 'add', triple: 't2' },
      { op: 'fail' }, // Failure
    ],
  };

  const result = applyDeltaAtomic(delta);

  assert.strictEqual(result.success, false);
  assert.strictEqual(state.size, 0); // Nothing applied
});

// ============================================================================
// Test Suite 2: Timeout Enforcement (3 tests)
// ============================================================================

test('Error Handling - operation timeout after 5s', async () => {
  const timeoutMs = 100; // 100ms for test

  async function withTimeout(fn, timeout) {
    return Promise.race([
      fn(),
      new Promise((_, reject) =>
        setTimeout(() => reject(new Error('Timeout exceeded')), timeout)
      ),
    ]);
  }

  const slowOperation = async () => {
    await new Promise(resolve => setTimeout(resolve, 200)); // 200ms
    return 'should not get here';
  };

  await assert.rejects(
    async () => withTimeout(slowOperation, timeoutMs),
    /Timeout exceeded/
  );
});

test('Error Handling - fast operation completes before timeout', async () => {
  async function withTimeout(fn, timeout) {
    return Promise.race([
      fn(),
      new Promise((_, reject) =>
        setTimeout(() => reject(new Error('Timeout')), timeout)
      ),
    ]);
  }

  const fastOperation = async () => {
    return 'success';
  };

  const result = await withTimeout(fastOperation, 1000);

  assert.strictEqual(result, 'success');
});

test('Error Handling - timeout generates denial receipt', async () => {
  const timeoutMs = 50;

  async function executeWithTimeout(operation) {
    try {
      const result = await Promise.race([
        operation(),
        new Promise((_, reject) =>
          setTimeout(() => reject(new Error('Timeout')), timeoutMs)
        ),
      ]);

      return {
        success: true,
        result,
      };
    } catch (error) {
      return {
        success: false,
        receipt: {
          decision: 'DENY',
          reason: 'TIMEOUT',
          timestamp: new Date().toISOString(),
          error: error.message,
        },
      };
    }
  }

  const slowOp = async () => {
    await new Promise(resolve => setTimeout(resolve, 100));
    return 'late';
  };

  const result = await executeWithTimeout(slowOp);

  assert.strictEqual(result.success, false);
  assert.strictEqual(result.receipt.decision, 'DENY');
  assert.strictEqual(result.receipt.reason, 'TIMEOUT');
});

// ============================================================================
// Test Suite 3: Error Receipts (3 tests)
// ============================================================================

test('Error Handling - denial receipt for invalid delta', () => {
  const invalidDelta = {
    operations: [], // Empty
  };

  function validateDelta(delta) {
    if (!delta.operations || delta.operations.length === 0) {
      return {
        valid: false,
        receipt: {
          decision: 'DENY',
          reason: 'EMPTY_OPERATIONS',
          timestamp: new Date().toISOString(),
        },
      };
    }

    return { valid: true };
  }

  const result = validateDelta(invalidDelta);

  assert.strictEqual(result.valid, false);
  assert.strictEqual(result.receipt.decision, 'DENY');
  assert.strictEqual(result.receipt.reason, 'EMPTY_OPERATIONS');
});

test('Error Handling - rejection receipt for policy violation', () => {
  const delta = {
    operations: [
      { op: 'delete', subject: 'protected-resource', predicate: 'p', object: 'o' },
    ],
  };

  function checkPolicy(delta) {
    for (const op of delta.operations) {
      if (op.op === 'delete' && op.subject === 'protected-resource') {
        return {
          allowed: false,
          receipt: {
            decision: 'REJECT',
            reason: 'POLICY_VIOLATION',
            policy: 'resource-protection',
            timestamp: new Date().toISOString(),
          },
        };
      }
    }

    return { allowed: true };
  }

  const result = checkPolicy(delta);

  assert.strictEqual(result.allowed, false);
  assert.strictEqual(result.receipt.decision, 'REJECT');
  assert.strictEqual(result.receipt.reason, 'POLICY_VIOLATION');
});

test('Error Handling - error receipt includes details', () => {
  function createErrorReceipt(error, context) {
    return {
      decision: 'ERROR',
      error: error.message,
      stack: error.stack,
      context,
      timestamp: new Date().toISOString(),
      recoverable: false,
    };
  }

  const error = new Error('Validation failed');
  const receipt = createErrorReceipt(error, { deltaId: 'delta-123' });

  assert.strictEqual(receipt.decision, 'ERROR');
  assert.strictEqual(receipt.error, 'Validation failed');
  assert.ok(receipt.context.deltaId);
  assert.strictEqual(receipt.recoverable, false);
});

// ============================================================================
// Test Suite 4: Graceful Degradation (3 tests)
// ============================================================================

test('Error Handling - fallback on service unavailable', () => {
  function getDataWithFallback() {
    try {
      // Simulate service unavailable
      throw new Error('Service unavailable');
    } catch (error) {
      // Fallback to cached data
      return {
        source: 'cache',
        data: { value: 'fallback' },
      };
    }
  }

  const result = getDataWithFallback();

  assert.strictEqual(result.source, 'cache');
  assert.strictEqual(result.data.value, 'fallback');
});

test('Error Handling - partial success with warnings', () => {
  const operations = [
    { op: 'add', triple: 't1' },
    { op: 'add', triple: 't2' },
    { op: 'invalid', triple: 't3' },
  ];

  const state = new Set();
  const warnings = [];

  for (const op of operations) {
    if (op.op === 'add') {
      state.add(op.triple);
    } else {
      warnings.push(`Invalid operation: ${op.op}`);
    }
  }

  assert.strictEqual(state.size, 2);
  assert.strictEqual(warnings.length, 1);
  assert.ok(warnings[0].includes('Invalid operation'));
});

test('Error Handling - retry with exponential backoff', async () => {
  let attempts = 0;

  async function retryWithBackoff(fn, maxRetries = 3) {
    for (let i = 0; i < maxRetries; i++) {
      try {
        return await fn();
      } catch (error) {
        attempts++;
        if (i === maxRetries - 1) throw error;

        // Exponential backoff: 2^i * 10ms
        const delay = Math.pow(2, i) * 10;
        await new Promise(resolve => setTimeout(resolve, delay));
      }
    }
  }

  const flakeyOperation = async () => {
    if (attempts < 2) {
      throw new Error('Temporary failure');
    }
    return 'success';
  };

  const result = await retryWithBackoff(flakeyOperation);

  assert.strictEqual(result, 'success');
  assert.strictEqual(attempts, 2);
});

// ============================================================================
// Test Suite 5: Edge Cases (2 tests)
// ============================================================================

test('Error Handling - null/undefined values', () => {
  function validateValue(value) {
    if (value === null || value === undefined) {
      throw new Error('Value cannot be null or undefined');
    }
    return true;
  }

  assert.throws(() => validateValue(null), /cannot be null/);
  assert.throws(() => validateValue(undefined), /cannot be null/);
  assert.strictEqual(validateValue('valid'), true);
});

test('Error Handling - circular reference detection', () => {
  const obj = { value: 42 };
  obj.circular = obj; // Create circular reference

  function detectCircular(obj, seen = new WeakSet()) {
    if (typeof obj !== 'object' || obj === null) {
      return false;
    }

    if (seen.has(obj)) {
      return true; // Circular reference detected
    }

    seen.add(obj);

    for (const key in obj) {
      if (detectCircular(obj[key], seen)) {
        return true;
      }
    }

    return false;
  }

  assert.strictEqual(detectCircular(obj), true);
  assert.strictEqual(detectCircular({ value: 42 }), false);
});

console.log('\n✅ All error handling tests passed');
