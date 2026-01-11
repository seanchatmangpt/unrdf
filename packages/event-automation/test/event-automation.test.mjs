/**
 * @file Event Automation Tests
 * @description Comprehensive test suite for event-automation package
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  EventAutomationEngine,
  createEventAutomationEngine,
  DeltaProcessor,
  createDeltaProcessor,
  ReceiptTracker,
  createReceiptTracker,
  PolicyEnforcer,
  createPolicyEnforcer,
} from '../src/index.mjs';

// ============================================================================
// Delta Processor Tests
// ============================================================================

describe('DeltaProcessor', () => {
  let processor;

  beforeEach(() => {
    processor = createDeltaProcessor();
  });

  afterEach(() => {
    processor.reset();
  });

  it('should create a delta processor instance', () => {
    expect(processor).toBeInstanceOf(DeltaProcessor);
    expect(processor.metrics.totalProcessed).toBe(0);
  });

  it('should process a valid delta successfully', async () => {
    const delta = {
      id: 'delta-1',
      operations: [
        {
          operation: 'insert',
          subject: 'http://example.org/s1',
          predicate: 'http://example.org/p1',
          object: 'http://example.org/o1',
        },
      ],
      timestamp: Date.now(),
    };

    const result = await processor.processDelta(delta);

    expect(result.success).toBe(true);
    expect(result.deltaId).toBe('delta-1');
    expect(result.hash).toBeDefined();
    expect(result.operationCount).toBe(1);
    expect(result.duration).toBeGreaterThanOrEqual(0);
  });

  it('should reject invalid delta structure', async () => {
    const invalidDelta = {
      id: 'delta-2',
      // Missing required operations field
      timestamp: Date.now(),
    };

    const result = await processor.processDelta(invalidDelta);

    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
  });

  it('should detect duplicate delta processing', async () => {
    const delta = {
      id: 'delta-3',
      operations: [
        {
          operation: 'delete',
          subject: 'http://example.org/s2',
          predicate: 'http://example.org/p2',
          object: 'http://example.org/o2',
        },
      ],
      timestamp: Date.now(),
    };

    const result1 = await processor.processDelta(delta);
    const result2 = await processor.processDelta(delta);

    expect(result1.success).toBe(true);
    expect(result2.success).toBe(true);
    expect(result2.duplicate).toBe(true);
  });

  it('should validate operation fields', async () => {
    const delta = {
      id: 'delta-4',
      operations: [
        {
          operation: 'insert',
          subject: '',  // Empty subject should fail
          predicate: 'http://example.org/p1',
          object: 'http://example.org/o1',
        },
      ],
      timestamp: Date.now(),
    };

    const result = await processor.processDelta(delta);

    expect(result.success).toBe(false);
    expect(result.error).toContain('subject');
  });

  it('should batch process deltas sequentially', async () => {
    const deltas = [
      {
        id: 'delta-5',
        operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
        timestamp: Date.now(),
      },
      {
        id: 'delta-6',
        operations: [{ operation: 'delete', subject: 's2', predicate: 'p2', object: 'o2' }],
        timestamp: Date.now(),
      },
    ];

    const results = await processor.batchProcess(deltas);

    expect(results).toHaveLength(2);
    expect(results[0].success).toBe(true);
    expect(results[1].success).toBe(true);
  });

  it('should batch process deltas in parallel', async () => {
    const deltas = [
      {
        id: 'delta-7',
        operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
        timestamp: Date.now(),
      },
      {
        id: 'delta-8',
        operations: [{ operation: 'update', subject: 's2', predicate: 'p2', object: 'o2' }],
        timestamp: Date.now(),
      },
    ];

    const results = await processor.batchProcess(deltas, { parallel: true });

    expect(results).toHaveLength(2);
    expect(results[0].success).toBe(true);
    expect(results[1].success).toBe(true);
  });

  it('should track processing metrics', async () => {
    const delta = {
      id: 'delta-9',
      operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
      timestamp: Date.now(),
    };

    await processor.processDelta(delta);

    const metrics = processor.getMetrics();

    expect(metrics.totalProcessed).toBe(1);
    expect(metrics.totalSucceeded).toBe(1);
    expect(metrics.totalFailed).toBe(0);
    expect(metrics.averageDuration).toBeGreaterThanOrEqual(0);
  });
});

// ============================================================================
// Receipt Tracker Tests
// ============================================================================

describe('ReceiptTracker', () => {
  let tracker;

  beforeEach(() => {
    tracker = createReceiptTracker();
  });

  afterEach(() => {
    tracker.reset();
  });

  it('should create a receipt tracker instance', () => {
    expect(tracker).toBeInstanceOf(ReceiptTracker);
    expect(tracker.metrics.totalCreated).toBe(0);
  });

  it('should create a receipt for a delta', async () => {
    const delta = {
      id: 'delta-10',
      operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
      timestamp: Date.now(),
    };

    const receipt = await tracker.createReceipt(delta);

    expect(receipt).toBeDefined();
    expect(receipt.id).toBeDefined();
    expect(receipt.deltaId).toBe('delta-10');
    expect(receipt.operation).toBe('process');
    expect(receipt.entityType).toBe('Delta');
    expect(receipt.hash).toBeDefined();
  });

  it('should verify a valid receipt', async () => {
    const delta = {
      id: 'delta-11',
      operations: [{ operation: 'delete', subject: 's1', predicate: 'p1', object: 'o1' }],
      timestamp: Date.now(),
    };

    const receipt = await tracker.createReceipt(delta);
    const isValid = await tracker.verifyReceipt(receipt);

    expect(isValid).toBe(true);
  });

  it('should reject tampered receipt', async () => {
    const delta = {
      id: 'delta-12',
      operations: [{ operation: 'update', subject: 's1', predicate: 'p1', object: 'o1' }],
      timestamp: Date.now(),
    };

    const receipt = await tracker.createReceipt(delta);
    receipt.hash = 'tampered-hash';

    const isValid = await tracker.verifyReceipt(receipt);

    expect(isValid).toBe(false);
  });

  it('should get receipt by ID', async () => {
    const delta = {
      id: 'delta-13',
      operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
      timestamp: Date.now(),
    };

    const receipt = await tracker.createReceipt(delta);
    const retrieved = tracker.getReceipt(receipt.id);

    expect(retrieved).toEqual(receipt);
  });

  it('should get all receipts for a delta', async () => {
    const delta = {
      id: 'delta-14',
      operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
      timestamp: Date.now(),
    };

    const receipt1 = await tracker.createReceipt(delta, { operation: 'create' });
    const receipt2 = await tracker.createReceipt(delta, { operation: 'update' });

    const receipts = tracker.getReceiptsForDelta('delta-14');

    expect(receipts).toHaveLength(2);
    expect(receipts).toContainEqual(receipt1);
    expect(receipts).toContainEqual(receipt2);
  });

  it('should enforce max receipts limit', async () => {
    const smallTracker = new ReceiptTracker({ maxReceipts: 5 });

    for (let i = 0; i < 10; i++) {
      const delta = {
        id: `delta-${i}`,
        operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
        timestamp: Date.now(),
      };
      await smallTracker.createReceipt(delta);
    }

    const metrics = smallTracker.getMetrics();
    expect(metrics.totalReceipts).toBe(5);
  });

  it('should track receipt metrics', async () => {
    const delta = {
      id: 'delta-15',
      operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
      timestamp: Date.now(),
    };

    const receipt = await tracker.createReceipt(delta);
    await tracker.verifyReceipt(receipt);

    const metrics = tracker.getMetrics();

    expect(metrics.totalCreated).toBe(1);
    expect(metrics.totalVerified).toBe(1);
    expect(metrics.totalInvalid).toBe(0);
  });
});

// ============================================================================
// Policy Enforcer Tests
// ============================================================================

describe('PolicyEnforcer', () => {
  let enforcer;

  beforeEach(() => {
    enforcer = createPolicyEnforcer({ failOnPolicyViolation: false });
  });

  afterEach(() => {
    enforcer.reset();
  });

  it('should create a policy enforcer instance', () => {
    expect(enforcer).toBeInstanceOf(PolicyEnforcer);
    expect(enforcer.metrics.totalEvaluations).toBe(0);
  });

  it('should register a policy', () => {
    const policy = {
      id: 'policy-1',
      name: 'Test Policy',
      trigger: 'before:delta',
      action: async () => {},
    };

    const policyId = enforcer.registerPolicy(policy);

    expect(policyId).toBe('policy-1');
    expect(enforcer.getPolicy('policy-1')).toBeDefined();
  });

  it('should unregister a policy', () => {
    const policy = {
      id: 'policy-2',
      name: 'Test Policy',
      trigger: 'after:delta',
      action: async () => {},
    };

    enforcer.registerPolicy(policy);
    const removed = enforcer.unregisterPolicy('policy-2');

    expect(removed).toBe(true);
    expect(enforcer.getPolicy('policy-2')).toBeUndefined();
  });

  it('should evaluate policies for a trigger', async () => {
    const policy = {
      id: 'policy-3',
      name: 'Test Policy',
      trigger: 'before:delta',
      action: async (context) => {
        context.validated = true;
      },
    };

    enforcer.registerPolicy(policy);

    const context = { delta: {} };
    const results = await enforcer.evaluatePolicies('before:delta', context);

    expect(results).toHaveLength(1);
    expect(results[0].passed).toBe(true);
    expect(context.validated).toBe(true);
  });

  it('should skip disabled policies', async () => {
    const policy = {
      id: 'policy-4',
      name: 'Test Policy',
      trigger: 'after:receipt',
      action: async () => {
        throw new Error('Should not execute');
      },
      enabled: false,
    };

    enforcer.registerPolicy(policy);

    const results = await enforcer.evaluatePolicies('after:receipt', {});

    expect(results).toHaveLength(1);
    expect(results[0].skipped).toBe(true);
  });

  it('should evaluate policies by priority', async () => {
    const executionOrder = [];

    const policy1 = {
      id: 'policy-5',
      name: 'Low Priority',
      trigger: 'before:delta',
      priority: 10,
      action: async () => {
        executionOrder.push('low');
      },
    };

    const policy2 = {
      id: 'policy-6',
      name: 'High Priority',
      trigger: 'before:delta',
      priority: 90,
      action: async () => {
        executionOrder.push('high');
      },
    };

    enforcer.registerPolicy(policy1);
    enforcer.registerPolicy(policy2);

    await enforcer.evaluatePolicies('before:delta', {});

    expect(executionOrder).toEqual(['high', 'low']);
  });

  it('should handle policy failures gracefully', async () => {
    const policy = {
      id: 'policy-7',
      name: 'Failing Policy',
      trigger: 'before:delta',
      action: async () => {
        throw new Error('Policy failed');
      },
    };

    enforcer.registerPolicy(policy);

    const results = await enforcer.evaluatePolicies('before:delta', {});

    expect(results).toHaveLength(1);
    expect(results[0].passed).toBe(false);
    expect(results[0].message).toContain('Policy failed');
  });

  it('should track policy metrics', async () => {
    const policy = {
      id: 'policy-8',
      name: 'Test Policy',
      trigger: 'after:delta',
      action: async () => {},
    };

    enforcer.registerPolicy(policy);
    await enforcer.evaluatePolicies('after:delta', {});

    const metrics = enforcer.getMetrics();

    expect(metrics.totalEvaluations).toBe(1);
    expect(metrics.totalPassed).toBe(1);
    expect(metrics.totalFailed).toBe(0);
  });

  it('should enable and disable policies', () => {
    const policy = {
      id: 'policy-9',
      name: 'Test Policy',
      trigger: 'before:delta',
      action: async () => {},
    };

    enforcer.registerPolicy(policy);

    const disabled = enforcer.disablePolicy('policy-9');
    expect(disabled).toBe(true);
    expect(enforcer.getPolicy('policy-9').enabled).toBe(false);

    const enabled = enforcer.enablePolicy('policy-9');
    expect(enabled).toBe(true);
    expect(enforcer.getPolicy('policy-9').enabled).toBe(true);
  });
});

// ============================================================================
// Event Automation Engine Tests
// ============================================================================

describe('EventAutomationEngine', () => {
  let engine;

  beforeEach(() => {
    engine = createEventAutomationEngine({ autoStart: false });
  });

  afterEach(async () => {
    if (engine.isRunning) {
      await engine.stop();
    }
    engine.reset();
  });

  it('should create an engine instance', () => {
    expect(engine).toBeInstanceOf(EventAutomationEngine);
    expect(engine.isRunning).toBe(false);
  });

  it('should start and stop the engine', async () => {
    engine.start();
    expect(engine.isRunning).toBe(true);

    await engine.stop();
    expect(engine.isRunning).toBe(false);
  });

  it('should process a delta with full automation', async () => {
    engine.start();

    const delta = {
      id: 'delta-20',
      operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
      timestamp: Date.now(),
    };

    const result = await engine.processDelta(delta);

    expect(result.success).toBe(true);
    expect(result.deltaId).toBe('delta-20');
    expect(result.receipts).toHaveLength(1);
    expect(result.duration).toBeGreaterThanOrEqual(0);
  });

  it('should enforce policies during delta processing', async () => {
    engine.start();

    let policyExecuted = false;
    const policy = {
      id: 'policy-10',
      name: 'Test Policy',
      trigger: 'before:delta',
      action: async (context) => {
        policyExecuted = true;
        expect(context.delta).toBeDefined();
      },
    };

    engine.registerPolicy(policy);

    const delta = {
      id: 'delta-21',
      operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
      timestamp: Date.now(),
    };

    await engine.processDelta(delta);

    expect(policyExecuted).toBe(true);
  });

  it('should batch process deltas', async () => {
    engine.start();

    const deltas = [
      {
        id: 'delta-22',
        operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
        timestamp: Date.now(),
      },
      {
        id: 'delta-23',
        operations: [{ operation: 'delete', subject: 's2', predicate: 'p2', object: 'o2' }],
        timestamp: Date.now(),
      },
    ];

    const results = await engine.batchProcess(deltas);

    expect(results).toHaveLength(2);
    expect(results[0].success).toBe(true);
    expect(results[1].success).toBe(true);
  });

  it('should add events to replay buffer', async () => {
    engine.start();

    const delta = {
      id: 'delta-24',
      operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
      timestamp: Date.now(),
    };

    await engine.processDelta(delta);

    const stats = engine.getStatistics();
    expect(stats.replayBufferSize).toBe(1);
  });

  it('should replay events from buffer', async () => {
    engine.start();

    const delta1 = {
      id: 'delta-25',
      operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
      timestamp: Date.now(),
    };

    const delta2 = {
      id: 'delta-26',
      operations: [{ operation: 'delete', subject: 's2', predicate: 'p2', object: 'o2' }],
      timestamp: Date.now(),
    };

    await engine.processDelta(delta1);
    await engine.processDelta(delta2);

    // Reset processor to test replay
    engine.deltaProcessor.reset();

    const replayResults = await engine.replay({ batchSize: 10 });

    expect(replayResults).toHaveLength(2);
    expect(replayResults[0].success).toBe(true);
    expect(replayResults[1].success).toBe(true);
  });

  it('should collect comprehensive statistics', async () => {
    engine.start();

    const delta = {
      id: 'delta-27',
      operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
      timestamp: Date.now(),
    };

    await engine.processDelta(delta);

    const stats = engine.getStatistics();

    expect(stats.totalProcessed).toBe(1);
    expect(stats.totalSucceeded).toBe(1);
    expect(stats.totalFailed).toBe(0);
    expect(stats.totalReceipts).toBe(1);
    expect(stats.averageDuration).toBeGreaterThanOrEqual(0);
    expect(stats.uptimeMs).toBeGreaterThanOrEqual(0);
  });

  it('should respect max concurrent limit', async () => {
    const limitedEngine = new EventAutomationEngine({
      autoStart: true,
      maxConcurrent: 2,
    });

    const deltas = Array.from({ length: 5 }, (_, i) => ({
      id: `delta-${30 + i}`,
      operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
      timestamp: Date.now(),
    }));

    const results = await limitedEngine.batchProcess(deltas, { parallel: true });

    expect(results).toHaveLength(5);
    expect(results.every((r) => r.success)).toBe(true);

    await limitedEngine.stop();
  });

  it('should handle processing failures gracefully', async () => {
    engine.start();

    const invalidDelta = {
      id: 'delta-35',
      // Missing operations field
      timestamp: Date.now(),
    };

    const result = await engine.processDelta(invalidDelta);

    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
  });

  it('should emit events during processing', async () => {
    engine.start();

    const events = [];
    engine.on('delta:processing', (data) => events.push({ type: 'processing', data }));
    engine.on('delta:processed', (data) => events.push({ type: 'processed', data }));
    engine.on('receipt:created', (data) => events.push({ type: 'receipt', data }));

    const delta = {
      id: 'delta-36',
      operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
      timestamp: Date.now(),
    };

    await engine.processDelta(delta);

    expect(events.length).toBeGreaterThanOrEqual(2);
    expect(events.some((e) => e.type === 'processing')).toBe(true);
    expect(events.some((e) => e.type === 'processed')).toBe(true);
  });

  it('should measure performance within targets', async () => {
    engine.start();

    const delta = {
      id: 'delta-37',
      operations: [{ operation: 'insert', subject: 's1', predicate: 'p1', object: 'o1' }],
      timestamp: Date.now(),
    };

    const result = await engine.processDelta(delta);

    // Performance targets:
    // - Delta processing: <5ms P95
    // - Receipt creation: <1ms P95
    // - Total should be well under 10ms for simple case
    expect(result.duration).toBeLessThan(10);
  });
});
