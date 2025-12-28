/**
 * @file Custom validators tests
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { blake3 } from 'hash-wasm';
import {
  validateReceiptChainIntegrity,
  ReceiptChainSchema,
  validateTemporalConsistency,
  TemporallyOrderedSchema,
  validateArtifactHash,
  ArtifactSchema,
  validateDependencyDAG,
  WorkItemDependencySchema,
  detectCycle,
  validateAsyncPolicy,
  createAsyncPolicySchema,
  validateTimeRange,
  RunCapsuleTimeRangeSchema,
  combineValidators,
  createValidationResult,
} from '../src/validators.mjs';

// =============================================================================
// Receipt Chain Integrity Tests
// =============================================================================

describe('Receipt Chain Integrity Validator', () => {
  it('should validate valid receipt chain with proper linkage', () => {
    const receipts = [
      {
        id: 'receipt-1',
        hash: 'abc123',
        parentHash: null,
        timestamp: 1000,
        operation: 'add',
      },
      {
        id: 'receipt-2',
        hash: 'def456',
        parentHash: 'abc123',
        timestamp: 2000,
        operation: 'update',
      },
      {
        id: 'receipt-3',
        hash: 'ghi789',
        parentHash: 'def456',
        timestamp: 3000,
        operation: 'delete',
      },
    ];

    const isValid = validateReceiptChainIntegrity(receipts);
    assert.equal(isValid, true);
  });

  it('should reject chain with broken parentHash linkage', () => {
    const receipts = [
      {
        id: 'receipt-1',
        hash: 'abc123',
        parentHash: null,
        timestamp: 1000,
        operation: 'add',
      },
      {
        id: 'receipt-2',
        hash: 'def456',
        parentHash: 'wrong-hash', // Should be 'abc123'
        timestamp: 2000,
        operation: 'update',
      },
    ];

    const isValid = validateReceiptChainIntegrity(receipts);
    assert.equal(isValid, false);
  });

  it('should reject chain where first receipt has parent', () => {
    const receipts = [
      {
        id: 'receipt-1',
        hash: 'abc123',
        parentHash: 'should-be-null', // First receipt should have null parent
        timestamp: 1000,
        operation: 'add',
      },
    ];

    const isValid = validateReceiptChainIntegrity(receipts);
    assert.equal(isValid, false);
  });

  it('should validate empty chain', () => {
    const isValid = validateReceiptChainIntegrity([]);
    assert.equal(isValid, true);
  });

  it('should validate chain using ReceiptChainSchema', async () => {
    const receipts = [
      {
        id: 'receipt-1',
        hash: 'abc123',
        parentHash: null,
        timestamp: 1000,
        operation: 'add',
      },
      {
        id: 'receipt-2',
        hash: 'def456',
        parentHash: 'abc123',
        timestamp: 2000,
        operation: 'update',
      },
    ];

    const result = ReceiptChainSchema.parse(receipts);
    assert.equal(result.length, 2);
  });
});

// =============================================================================
// Temporal Consistency Tests
// =============================================================================

describe('Temporal Consistency Validator', () => {
  it('should validate monotonically increasing timestamps', () => {
    const items = [
      { timestamp: 1000 },
      { timestamp: 2000 },
      { timestamp: 3000 },
      { timestamp: 4000 },
    ];

    const isValid = validateTemporalConsistency(items);
    assert.equal(isValid, true);
  });

  it('should reject non-increasing timestamps', () => {
    const items = [
      { timestamp: 1000 },
      { timestamp: 2000 },
      { timestamp: 1500 }, // Goes backwards
    ];

    const isValid = validateTemporalConsistency(items);
    assert.equal(isValid, false);
  });

  it('should reject equal timestamps', () => {
    const items = [
      { timestamp: 1000 },
      { timestamp: 2000 },
      { timestamp: 2000 }, // Duplicate
    ];

    const isValid = validateTemporalConsistency(items);
    assert.equal(isValid, false);
  });

  it('should validate single item', () => {
    const items = [{ timestamp: 1000 }];

    const isValid = validateTemporalConsistency(items);
    assert.equal(isValid, true);
  });

  it('should validate using TemporallyOrderedSchema', () => {
    const items = [
      { timestamp: 1000, data: 'test1' },
      { timestamp: 2000, data: 'test2' },
    ];

    const result = TemporallyOrderedSchema.parse(items);
    assert.equal(result.length, 2);
  });
});

// =============================================================================
// Artifact Hash Validation Tests
// =============================================================================

describe('Artifact Hash Validator', () => {
  it('should validate artifact with correct hash', async () => {
    const content = 'Hello World';
    const hash = await blake3(content);

    const artifact = {
      type: 'file',
      content,
      hash,
    };

    const isValid = await validateArtifactHash(artifact);
    assert.equal(isValid, true);
  });

  it('should reject artifact with incorrect hash', async () => {
    const artifact = {
      type: 'file',
      content: 'Hello World',
      hash: 'incorrect-hash',
    };

    const isValid = await validateArtifactHash(artifact);
    assert.equal(isValid, false);
  });

  it('should validate artifact without content or hash', async () => {
    const artifact = {
      type: 'file',
      path: '/test/file.txt',
    };

    const isValid = await validateArtifactHash(artifact);
    assert.equal(isValid, true);
  });

  it('should validate using ArtifactSchema', async () => {
    const content = 'Test Content';
    const hash = await blake3(content);

    const artifact = {
      type: 'file',
      path: '/test.txt',
      content,
      hash,
      size: 12,
    };

    const result = await ArtifactSchema.parseAsync(artifact);
    assert.equal(result.type, 'file');
  });
});

// =============================================================================
// Dependency DAG Validation Tests
// =============================================================================

describe('Dependency DAG Validator', () => {
  it('should validate acyclic dependency graph', () => {
    const workItems = [
      { id: 'A', dependencies: ['B', 'C'] },
      { id: 'B', dependencies: ['D'] },
      { id: 'C', dependencies: ['D'] },
      { id: 'D', dependencies: [] },
    ];

    const result = validateDependencyDAG(workItems);
    assert.equal(result.valid, true);
    assert.equal(result.cycle.length, 0);
  });

  it('should detect simple cycle', () => {
    const workItems = [
      { id: 'A', dependencies: ['B'] },
      { id: 'B', dependencies: ['C'] },
      { id: 'C', dependencies: ['A'] }, // Cycle: A -> B -> C -> A
    ];

    const result = validateDependencyDAG(workItems);
    assert.equal(result.valid, false);
    assert.ok(result.cycle.length > 0);
  });

  it('should detect self-loop', () => {
    const workItems = [
      { id: 'A', dependencies: ['A'] }, // Self-loop
    ];

    const result = validateDependencyDAG(workItems);
    assert.equal(result.valid, false);
  });

  it('should validate empty graph', () => {
    const result = validateDependencyDAG([]);
    assert.equal(result.valid, true);
  });

  it('should detect non-existent dependency', () => {
    const workItems = [
      { id: 'A', dependencies: ['B'] },
      // 'B' does not exist
    ];

    const result = validateDependencyDAG(workItems);
    assert.equal(result.valid, false);
    assert.ok(result.error);
  });

  it('should validate using WorkItemDependencySchema', async () => {
    const workItems = [
      { id: 'A', dependencies: ['B'] },
      { id: 'B', dependencies: [] },
    ];

    const result = WorkItemDependencySchema.parse(workItems);
    assert.equal(result.length, 2);
  });

  it('should reject cycle using WorkItemDependencySchema', async () => {
    const workItems = [
      { id: 'A', dependencies: ['B'] },
      { id: 'B', dependencies: ['A'] },
    ];

    assert.throws(() => {
      WorkItemDependencySchema.parse(workItems);
    }, /cycle detected/i);
  });
});

describe('Cycle Detection', () => {
  it('should detect cycle in graph', () => {
    const graph = new Map([
      ['A', ['B']],
      ['B', ['C']],
      ['C', ['A']],
    ]);

    const result = detectCycle(graph);
    assert.equal(result.hasCycle, true);
    assert.ok(result.cycle.length > 0);
  });

  it('should return no cycle for DAG', () => {
    const graph = new Map([
      ['A', ['B', 'C']],
      ['B', ['D']],
      ['C', ['D']],
      ['D', []],
    ]);

    const result = detectCycle(graph);
    assert.equal(result.hasCycle, false);
  });
});

// =============================================================================
// Async Validator Tests
// =============================================================================

describe('Async Policy Validator', () => {
  it('should validate operation allowed by policy', async () => {
    const policyCheck = async (op) => op.type === 'safe_operation';

    const operation = { type: 'safe_operation', data: 'test' };
    const isAllowed = await validateAsyncPolicy(operation, policyCheck);

    assert.equal(isAllowed, true);
  });

  it('should reject operation denied by policy', async () => {
    const policyCheck = async (op) => op.type !== 'forbidden_operation';

    const operation = { type: 'forbidden_operation', data: 'test' };
    const isAllowed = await validateAsyncPolicy(operation, policyCheck);

    assert.equal(isAllowed, false);
  });

  it('should handle async policy check errors', async () => {
    const policyCheck = async () => {
      throw new Error('Policy service unavailable');
    };

    const operation = { type: 'test', data: 'test' };
    const isAllowed = await validateAsyncPolicy(operation, policyCheck);

    assert.equal(isAllowed, false);
  });

  it('should validate using createAsyncPolicySchema', async () => {
    const policyCheck = async (op) => op.type === 'allowed';

    const schema = createAsyncPolicySchema(policyCheck);
    const result = await schema.parseAsync({ type: 'allowed', data: 'test' });

    assert.equal(result.type, 'allowed');
  });

  it('should reject using createAsyncPolicySchema', async () => {
    const policyCheck = async (op) => op.type === 'allowed';

    const schema = createAsyncPolicySchema(policyCheck);

    await assert.rejects(async () => {
      await schema.parseAsync({ type: 'denied', data: 'test' });
    }, /denied by policy/i);
  });
});

// =============================================================================
// Time Range Validation Tests
// =============================================================================

describe('Time Range Validator', () => {
  it('should validate valid time range', () => {
    const capsule = {
      startTime: 1000,
      endTime: 2000,
    };

    const isValid = validateTimeRange(capsule);
    assert.equal(isValid, true);
  });

  it('should reject invalid time range', () => {
    const capsule = {
      startTime: 2000,
      endTime: 1000, // End before start
    };

    const isValid = validateTimeRange(capsule);
    assert.equal(isValid, false);
  });

  it('should validate using RunCapsuleTimeRangeSchema', () => {
    const capsule = {
      startTime: 1000,
      endTime: 2000,
      other: 'data',
    };

    const result = RunCapsuleTimeRangeSchema.parse(capsule);
    assert.equal(result.startTime, 1000);
  });
});

// =============================================================================
// Validator Utilities Tests
// =============================================================================

describe('Validator Utilities', () => {
  it('should combine multiple validators', () => {
    const validators = [
      (data) => data.value > 0 || 'Value must be positive',
      (data) => data.value < 100 || 'Value must be less than 100',
    ];

    const validator = combineValidators(validators);

    const result1 = validator({ value: 50 });
    assert.equal(result1.valid, true);
    assert.equal(result1.errors.length, 0);

    const result2 = validator({ value: -10 });
    assert.equal(result2.valid, false);
    assert.ok(result2.errors.length > 0);
  });

  it('should create validation result', () => {
    const result = createValidationResult(true, 'Success', { detail: 'test' });

    assert.equal(result.valid, true);
    assert.equal(result.message, 'Success');
    assert.equal(result.context.detail, 'test');
    assert.ok(result.timestamp);
  });
});
