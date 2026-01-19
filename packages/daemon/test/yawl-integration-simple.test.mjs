/**
 * @file YAWL Daemon Bridge Integration Tests (Simplified)
 * @module test/yawl-integration-simple
 * @description Focused tests for YawlDaemonBridge core functionality
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  YawlDaemonBridge,
  YawlDaemonBridgeConfigSchema,
  YawlRetryPolicySchema,
  YawlTimeoutConfigSchema,
  DistributionStrategySchema,
  createYawlBridge,
} from '../src/integrations/yawl.mjs';

// =============================================================================
// Schema Validation Tests
// =============================================================================

describe('YawlDaemonBridge - Schema Validation', () => {
  it('should validate retry policy schema with defaults', () => {
    const config = { maxAttempts: 3, backoffMs: 1000 };
    const result = YawlRetryPolicySchema.parse(config);
    expect(result.maxAttempts).toBe(3);
    expect(result.backoffMultiplier).toBe(2);
  });

  it('should validate empty retry policy with defaults', () => {
    const result = YawlRetryPolicySchema.parse({});
    expect(result.maxAttempts).toBe(3);
    expect(result.backoffMs).toBe(1000);
    expect(result.jitterFactor).toBe(0.1);
  });

  it('should validate timeout config schema', () => {
    const config = { taskTimeoutMs: 30000 };
    const result = YawlTimeoutConfigSchema.parse(config);
    expect(result.taskTimeoutMs).toBe(30000);
    expect(result.caseTimeoutMs).toBe(3600000);
  });

  it('should validate distribution strategy', () => {
    const strategies = ['round-robin', 'least-loaded', 'random', 'affinity'];
    for (const strategy of strategies) {
      const result = DistributionStrategySchema.parse(strategy);
      expect(result).toBe(strategy);
    }
  });

  it('should validate bridge config schema', () => {
    const config = {
      daemonNodeId: 'node-1',
      maxConcurrentCases: 50,
    };
    const result = YawlDaemonBridgeConfigSchema.parse(config);
    expect(result.daemonNodeId).toBe('node-1');
    expect(result.maxConcurrentCases).toBe(50);
    expect(result.enableAutoRetry).toBe(true);
  });

  it('should validate bridge config with custom logger', () => {
    const mockLogger = { info: () => {}, error: () => {} };
    const config = {
      daemonNodeId: 'node-1',
      logger: mockLogger,
    };
    const result = YawlDaemonBridgeConfigSchema.parse(config);
    expect(result.logger).toBe(mockLogger);
  });

  it('should reject invalid retry attempts', () => {
    expect(() => YawlRetryPolicySchema.parse({ maxAttempts: 0 })).toThrow();
    expect(() => YawlRetryPolicySchema.parse({ maxAttempts: 11 })).toThrow();
  });

  it('should reject invalid backoff multiplier', () => {
    expect(() => YawlRetryPolicySchema.parse({ backoffMultiplier: 0.9 })).toThrow();
  });

  it('should reject invalid concurrency', () => {
    expect(() =>
      YawlDaemonBridgeConfigSchema.parse({
        daemonNodeId: 'node-1',
        maxConcurrentCases: 0,
      })
    ).toThrow();
  });
});

// =============================================================================
// Mock Classes
// =============================================================================

class MockDaemon {
  constructor() {
    this.operations = new Map();
    this.scheduled = [];
  }

  schedule(operation) {
    this.operations.set(operation.id, operation);
    this.scheduled.push(operation.id);
  }

  unschedule(operationId) {
    if (this.operations.has(operationId)) {
      this.operations.delete(operationId);
      const idx = this.scheduled.indexOf(operationId);
      if (idx > -1) this.scheduled.splice(idx, 1);
      return true;
    }
    return false;
  }

  listOperations() {
    return Array.from(this.operations.values());
  }
}

class MockYawlEngine {
  constructor() {
    this._handlers = new Map();
  }

  on(eventType, handler) {
    if (!this._handlers.has(eventType)) {
      this._handlers.set(eventType, new Set());
    }
    this._handlers.get(eventType).add(handler);
    return () => {
      this._handlers.get(eventType).delete(handler);
    };
  }

  emit(eventType, data) {
    const handlers = this._handlers.get(eventType);
    if (handlers) {
      for (const handler of handlers) {
        try {
          handler(data);
        } catch (e) {
          // Silently ignore
        }
      }
    }
  }

  async createCase(config) {
    return { case_id: config.caseId };
  }

  async enableTask(config) {
    return { task_id: config.taskId };
  }

  async cancelTask(_config) {
    return { cancelled: true };
  }
}

// =============================================================================
// YawlDaemonBridge Constructor Tests
// =============================================================================

describe('YawlDaemonBridge - Constructor', () => {
  let daemon;
  let yawlEngine;

  beforeEach(() => {
    daemon = new MockDaemon();
    yawlEngine = new MockYawlEngine();
  });

  it('should create bridge with valid configuration', () => {
    const bridge = new YawlDaemonBridge(daemon, yawlEngine, {
      daemonNodeId: 'node-1',
      maxConcurrentCases: 50,
    });

    expect(bridge.id).toBeDefined();
    expect(bridge.daemon).toBe(daemon);
    expect(bridge.yawlEngine).toBe(yawlEngine);
    expect(bridge.isRunning).toBe(false);
    expect(bridge.config.daemonNodeId).toBe('node-1');
  });

  it('should throw on invalid daemon', () => {
    expect(() => {
      new YawlDaemonBridge(null, yawlEngine, { daemonNodeId: 'node-1' });
    }).toThrow(TypeError);
  });

  it('should throw on daemon without schedule method', () => {
    expect(() => {
      new YawlDaemonBridge({}, yawlEngine, { daemonNodeId: 'node-1' });
    }).toThrow(TypeError);
  });

  it('should throw on invalid yawl engine', () => {
    expect(() => {
      new YawlDaemonBridge(daemon, null, { daemonNodeId: 'node-1' });
    }).toThrow(TypeError);
  });

  it('should throw on engine without event subscription', () => {
    expect(() => {
      new YawlDaemonBridge(daemon, {}, { daemonNodeId: 'node-1' });
    }).toThrow(TypeError);
  });

  it('should create bridge with factory function', () => {
    const bridge = createYawlBridge(daemon, yawlEngine, {
      daemonNodeId: 'node-1',
    });

    expect(bridge).toBeInstanceOf(YawlDaemonBridge);
  });
});

// =============================================================================
// Bridge Lifecycle Tests
// =============================================================================

describe('YawlDaemonBridge - Lifecycle', () => {
  let daemon;
  let yawlEngine;
  let bridge;

  beforeEach(() => {
    daemon = new MockDaemon();
    yawlEngine = new MockYawlEngine();
    bridge = new YawlDaemonBridge(daemon, yawlEngine, {
      daemonNodeId: 'node-1',
    });
  });

  it('should start and stop bridge', async () => {
    expect(bridge.isRunning).toBe(false);
    await bridge.start();
    expect(bridge.isRunning).toBe(true);
    await bridge.stop();
    expect(bridge.isRunning).toBe(false);
  });

  it('should emit lifecycle events', async () => {
    const events = [];
    bridge.on('bridge:started', (e) => events.push(e));
    bridge.on('bridge:stopped', (e) => events.push(e));

    await bridge.start();
    await bridge.stop();

    expect(events).toHaveLength(2);
    expect(events[0].bridgeId).toBe(bridge.id);
  });

  it('should be idempotent on start', async () => {
    await bridge.start();
    await bridge.start();
    expect(bridge.isRunning).toBe(true);
  });

  it('should be idempotent on stop', async () => {
    await bridge.stop();
    await bridge.stop();
    expect(bridge.isRunning).toBe(false);
  });
});

// =============================================================================
// Case Scheduling Tests
// =============================================================================

describe('YawlDaemonBridge - Case Scheduling', () => {
  let daemon;
  let yawlEngine;
  let bridge;

  beforeEach(() => {
    daemon = new MockDaemon();
    yawlEngine = new MockYawlEngine();
    bridge = new YawlDaemonBridge(daemon, yawlEngine, {
      daemonNodeId: 'node-1',
    });
  });

  it('should schedule recurring case', async () => {
    const result = await bridge.scheduleRecurringCase('workflow-1', '0 * * * *');

    expect(result.operationId).toBeDefined();
    expect(result.workflowId).toBe('workflow-1');
    expect(result.success).toBe(true);
  });

  it('should throw on invalid workflow ID', async () => {
    await expect(
      bridge.scheduleRecurringCase(null, '0 * * * *')
    ).rejects.toThrow(TypeError);
  });

  it('should store case schedule in map', async () => {
    await bridge.scheduleRecurringCase('workflow-1', '0 * * * *');
    expect(bridge.caseSchedules.has('workflow-1')).toBe(true);
  });

  it('should register with daemon', async () => {
    const result = await bridge.scheduleRecurringCase('workflow-1', '0 * * * *');
    expect(daemon.operations.has(result.operationId)).toBe(true);
  });
});

// =============================================================================
// Task Timeout Tests
// =============================================================================

describe('YawlDaemonBridge - Task Timeout', () => {
  let daemon;
  let yawlEngine;
  let bridge;

  beforeEach(() => {
    daemon = new MockDaemon();
    yawlEngine = new MockYawlEngine();
    bridge = new YawlDaemonBridge(daemon, yawlEngine, {
      daemonNodeId: 'node-1',
      timeoutDefaults: { caseTimeoutMs: 30000 },
    });
  });

  it('should watch task timeout', async () => {
    const result = await bridge.watchTaskTimeout('case-1', 'task-1', 5000);

    expect(result.operationId).toBeDefined();
    expect(result.caseId).toBe('case-1');
    expect(result.taskId).toBe('task-1');
    expect(result.timeoutMs).toBe(5000);
    expect(result.success).toBe(true);
  });

  it('should throw on invalid timeout', async () => {
    await expect(
      bridge.watchTaskTimeout('case-1', 'task-1', 100)
    ).rejects.toThrow(TypeError);
  });

  it('should store timeout tracking', async () => {
    await bridge.watchTaskTimeout('case-1', 'task-1', 5000);
    expect(bridge.taskTimeouts.has('case-1:task-1')).toBe(true);
  });
});

// =============================================================================
// Task Retry Tests
// =============================================================================

describe('YawlDaemonBridge - Task Retry', () => {
  let daemon;
  let yawlEngine;
  let bridge;

  beforeEach(() => {
    daemon = new MockDaemon();
    yawlEngine = new MockYawlEngine();
    bridge = new YawlDaemonBridge(daemon, yawlEngine, {
      daemonNodeId: 'node-1',
    });
  });

  it('should schedule retry', async () => {
    const result = await bridge.scheduleRetry('case-1', 'task-1', {
      maxAttempts: 3,
    });

    expect(result.operationId).toBeDefined();
    expect(result.caseId).toBe('case-1');
    expect(result.success).toBe(true);
  });

  it('should throw on invalid parameters', async () => {
    await expect(bridge.scheduleRetry(null, 'task-1')).rejects.toThrow(TypeError);
    await expect(bridge.scheduleRetry('case-1', null)).rejects.toThrow(TypeError);
  });

  it('should store retry state', async () => {
    await bridge.scheduleRetry('case-1', 'task-1');
    expect(bridge.taskRetries.has('case-1:task-1')).toBe(true);
  });

  it('should use provided backoff policy', async () => {
    const policy = { maxAttempts: 5, backoffMs: 2000 };
    const _result = await bridge.scheduleRetry('case-1', 'task-1', policy);

    const state = bridge.taskRetries.get('case-1:task-1');
    expect(state.maxAttempts).toBe(5);
  });
});

// =============================================================================
// Deferred Choice Tests
// =============================================================================

describe('YawlDaemonBridge - Deferred Choice', () => {
  let daemon;
  let yawlEngine;
  let bridge;

  beforeEach(() => {
    daemon = new MockDaemon();
    yawlEngine = new MockYawlEngine();
    bridge = new YawlDaemonBridge(daemon, yawlEngine, {
      daemonNodeId: 'node-1',
      timeoutDefaults: { caseTimeoutMs: 30000 },
    });
  });

  it('should wait for choice trigger', async () => {
    const promise = bridge.waitForChoiceTrigger('case-1', 'task-1', {
      eventName: 'user:approved',
      timeoutMs: 5000,
    });

    expect(bridge.choiceTriggers.has('case-1:task-1')).toBe(true);
    expect(promise).toBeInstanceOf(Promise);
  });

  it('should throw on missing event name', async () => {
    await expect(
      bridge.waitForChoiceTrigger('case-1', 'task-1', {})
    ).rejects.toThrow(TypeError);
  });

  it('should timeout if not triggered', async () => {
    const promise = bridge.waitForChoiceTrigger('case-1', 'task-1', {
      eventName: 'user:approved',
      timeoutMs: 1000,
    });

    await expect(promise).rejects.toThrow('Deferred choice timeout');
  });
});

// =============================================================================
// Task Distribution Tests
// =============================================================================

describe('YawlDaemonBridge - Task Distribution', () => {
  let daemon;
  let yawlEngine;
  let bridge;

  beforeEach(() => {
    daemon = new MockDaemon();
    yawlEngine = new MockYawlEngine();
    bridge = new YawlDaemonBridge(daemon, yawlEngine, {
      daemonNodeId: 'node-1',
    });
  });

  it('should distribute tasks', async () => {
    const result = await bridge.distributeAndSplitTasks('case-1', [
      'task-a',
      'task-b',
    ]);

    expect(result.operationId).toBeDefined();
    expect(result.caseId).toBe('case-1');
    expect(result.taskIds).toHaveLength(2);
    expect(result.success).toBe(true);
  });

  it('should throw on empty task list', async () => {
    await expect(
      bridge.distributeAndSplitTasks('case-1', [])
    ).rejects.toThrow(TypeError);
  });

  it('should support multiple strategies', async () => {
    const strategies = ['round-robin', 'least-loaded', 'random', 'affinity'];

    for (const strategy of strategies) {
      const result = await bridge.distributeAndSplitTasks('case-1', ['task-a'], {
        strategy,
      });
      expect(result.strategy).toBe(strategy);
    }
  });

  it('should store distribution state', async () => {
    const result = await bridge.distributeAndSplitTasks('case-1', [
      'task-a',
      'task-b',
    ]);

    expect(result.operationId).toBeDefined();
    expect(result.caseId).toBe('case-1');
    expect(result.taskIds).toHaveLength(2);
  });
});

// =============================================================================
// Statistics Tests
// =============================================================================

describe('YawlDaemonBridge - Statistics', () => {
  let daemon;
  let yawlEngine;
  let bridge;

  beforeEach(() => {
    daemon = new MockDaemon();
    yawlEngine = new MockYawlEngine();
    bridge = new YawlDaemonBridge(daemon, yawlEngine, {
      daemonNodeId: 'node-1',
    });
  });

  it('should return initial statistics', () => {
    const stats = bridge.getStats();

    expect(stats.bridgeId).toBe(bridge.id);
    expect(stats.isRunning).toBe(false);
    expect(stats.caseSchedules).toBe(0);
    expect(stats.activeTimeouts).toBe(0);
    expect(stats.activeRetries).toBe(0);
    expect(stats.activeTriggers).toBe(0);
  });

  it('should update statistics after scheduling', async () => {
    await bridge.scheduleRecurringCase('workflow-1', '0 * * * *');
    await bridge.watchTaskTimeout('case-1', 'task-1', 5000);
    await bridge.scheduleRetry('case-1', 'task-2');

    const stats = bridge.getStats();

    expect(stats.caseSchedules).toBe(1);
    expect(stats.activeTimeouts).toBe(1);
    expect(stats.activeRetries).toBe(1);
  });
});

// =============================================================================
// Event Listener Tests
// =============================================================================

describe('YawlDaemonBridge - Event Listeners', () => {
  let daemon;
  let yawlEngine;
  let bridge;

  beforeEach(() => {
    daemon = new MockDaemon();
    yawlEngine = new MockYawlEngine();
    bridge = new YawlDaemonBridge(daemon, yawlEngine, {
      daemonNodeId: 'node-1',
      enableAutoRetry: true,
      enableTimeoutTracking: true,
    });
  });

  it('should clean up on case completion', async () => {
    await bridge.watchTaskTimeout('case-1', 'task-1', 5000);
    await bridge.scheduleRetry('case-1', 'task-2');

    expect(bridge.taskTimeouts.size).toBe(1);
    expect(bridge.taskRetries.size).toBe(1);

    // Simulate case completion
    yawlEngine.emit('case:completed', { caseId: 'case-1' });

    // Cleanup happens after event processing
    expect(bridge.taskTimeouts.size).toBe(0);
    expect(bridge.taskRetries.size).toBe(0);
  });

  it('should not affect other cases', async () => {
    await bridge.watchTaskTimeout('case-1', 'task-1', 5000);
    await bridge.watchTaskTimeout('case-2', 'task-2', 5000);

    yawlEngine.emit('case:completed', { caseId: 'case-1' });

    expect(bridge.taskTimeouts.has('case-2:task-2')).toBe(true);
    expect(bridge.taskTimeouts.has('case-1:task-1')).toBe(false);
  });
});
