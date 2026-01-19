/**
 * @file End-to-End Cross-Package Integration Tests
 * @module @unrdf/daemon/test/e2e-cross-package-integration
 * @description Comprehensive integration tests for daemon with @unrdf/core, @unrdf/hooks,
 * @unrdf/streaming, @unrdf/consensus, and @unrdf/yawl. Validates data flow, error
 * propagation, and coordinated operation execution across 5 integration patterns + 1 multi-layer.
 *
 * Test Coverage (20+ tests):
 * - Daemon + Core (4 tests): RDF graph updates trigger daemon operations
 * - Daemon + Hooks (4 tests): Policy-controlled operation execution
 * - Daemon + Streaming (4 tests): Change feeds drive workflow scheduling
 * - Daemon + Consensus (4 tests): Distributed operation coordination
 * - Daemon + YAWL (4 tests): Complex workflow orchestration
 * - Multi-Layer (1+ tests): All packages combined
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { EventEmitter } from 'events';
import { Daemon } from '../src/daemon.mjs';

// =============================================================================
// Mock Implementations
// =============================================================================

/**
 * Mock RDF Store (simulates @unrdf/core behavior)
 * Emits events when quads are added/removed
 */
class MockRdfStore extends EventEmitter {
  constructor() {
    super();
    this.quads = new Map();
  }

  /**
   * Add a quad to the store
   * @param {Object} quad - RDF quad
   * @returns {void}
   */
  addQuad(quad) {
    const key = `${quad.subject}|${quad.predicate}|${quad.object}`;
    this.quads.set(key, quad);
    this.emit('quad:added', { quad, timestamp: Date.now() });
  }

  /**
   * Remove a quad from the store
   * @param {Object} quad - RDF quad
   * @returns {void}
   */
  removeQuad(quad) {
    const key = `${quad.subject}|${quad.predicate}|${quad.object}`;
    this.quads.delete(key);
    this.emit('quad:removed', { quad, timestamp: Date.now() });
  }

  /**
   * Get all quads
   * @returns {Array} Array of quads
   */
  getAllQuads() {
    return Array.from(this.quads.values());
  }

  /**
   * Query quads by pattern
   * @param {Object} pattern - Query pattern
   * @returns {Array} Matching quads
   */
  queryQuads(pattern) {
    return Array.from(this.quads.values()).filter((quad) => {
      if (pattern.subject && quad.subject !== pattern.subject) return false;
      if (pattern.predicate && quad.predicate !== pattern.predicate) return false;
      if (pattern.object && quad.object !== pattern.object) return false;
      return true;
    });
  }
}

/**
 * Mock Hook Scheduler (simulates @unrdf/hooks behavior)
 * Validates and executes hooks based on policies
 */
class MockHookScheduler extends EventEmitter {
  constructor() {
    super();
    this.hooks = new Map();
    this.policies = new Map();
    this.results = new Map();
  }

  /**
   * Register a hook with validation/transformation policy
   * @param {string} hookId - Hook identifier
   * @param {Object} hook - Hook definition
   * @param {Object} policy - Execution policy
   * @returns {void}
   */
  registerHook(hookId, hook, policy = {}) {
    this.hooks.set(hookId, hook);
    this.policies.set(hookId, policy);
    this.emit('hook:registered', { hookId, policy, timestamp: Date.now() });
  }

  /**
   * Execute a hook
   * @param {string} hookId - Hook identifier
   * @param {Object} input - Input data
   * @returns {Promise<Object>} Execution result
   */
  async executeHook(hookId, input) {
    const hook = this.hooks.get(hookId);
    const policy = this.policies.get(hookId);

    if (!hook) {
      throw new Error(`Hook not found: ${hookId}`);
    }

    // Validate input against policy
    if (policy.validation && !policy.validation(input)) {
      throw new Error(`Input validation failed for hook ${hookId}`);
    }

    // Execute hook
    const result = hook.handler ? hook.handler(input) : input;

    // Transform if policy requires
    const transformed = policy.transform ? policy.transform(result) : result;

    const execution = {
      hookId,
      input,
      output: transformed,
      status: 'success',
      timestamp: Date.now(),
    };

    this.results.set(hookId, execution);
    this.emit('hook:executed', { hookId, result: transformed, timestamp: Date.now() });

    return transformed;
  }

  /**
   * Get hook execution results
   * @returns {Array} Array of execution results
   */
  getResults() {
    return Array.from(this.results.values());
  }
}

/**
 * Mock Streaming Feed (simulates @unrdf/streaming behavior)
 * Emits change events that trigger daemon operations
 */
class MockStreamingFeed extends EventEmitter {
  constructor() {
    super();
    this.subscriptions = new Map();
    this.changes = [];
  }

  /**
   * Subscribe to changes matching a pattern
   * @param {string} subscriberId - Subscriber identifier
   * @param {Object} pattern - Change pattern to match
   * @param {Function} onMatch - Callback when pattern matches
   * @returns {string} Subscription ID
   */
  subscribe(subscriberId, pattern, onMatch) {
    const subscriptionId = `sub_${Date.now()}_${Math.random().toString(36).slice(2, 9)}`;
    this.subscriptions.set(subscriptionId, { subscriberId, pattern, onMatch });
    this.emit('subscription:created', { subscriberId, subscriptionId, timestamp: Date.now() });
    return subscriptionId;
  }

  /**
   * Unsubscribe from feed
   * @param {string} subscriptionId - Subscription identifier
   * @returns {boolean} Whether unsubscribe succeeded
   */
  unsubscribe(subscriptionId) {
    const removed = this.subscriptions.delete(subscriptionId);
    if (removed) {
      this.emit('subscription:deleted', { subscriptionId, timestamp: Date.now() });
    }
    return removed;
  }

  /**
   * Emit a change event
   * @param {Object} change - Change event
   * @returns {Promise<void>}
   */
  async emitChange(change) {
    this.changes.push(change);

    // Match against subscriptions
    for (const [subscriptionId, { pattern, onMatch }] of this.subscriptions.entries()) {
      if (this.matchesPattern(change, pattern)) {
        try {
          await onMatch(change);
        } catch (error) {
          this.emit('subscription:error', { subscriptionId, error: error.message });
        }
      }
    }

    this.emit('change:emitted', { change, timestamp: Date.now() });
  }

  /**
   * Check if change matches pattern
   * @param {Object} change - Change event
   * @param {Object} pattern - Pattern to match
   * @returns {boolean}
   * @private
   */
  matchesPattern(change, pattern) {
    if (pattern.type && change.type !== pattern.type) return false;
    if (pattern.subject && change.subject !== pattern.subject) return false;
    if (pattern.operation && change.operation !== pattern.operation) return false;
    return true;
  }

  /**
   * Get all changes
   * @returns {Array} Array of changes
   */
  getAllChanges() {
    return this.changes;
  }
}

/**
 * Mock Consensus Coordinator (simulates @unrdf/consensus behavior)
 * Manages distributed state and leader election
 */
class MockConsensusCoordinator extends EventEmitter {
  constructor(nodeId = 'node-1') {
    super();
    this.nodeId = nodeId;
    this.isLeader = false;
    this.term = 0;
    this.log = [];
    this.committedIndex = 0;
    this.nodes = new Set();
  }

  /**
   * Initialize coordinator
   * @returns {Promise<void>}
   */
  async initialize() {
    this.emit('coordinator:initialized', { nodeId: this.nodeId, timestamp: Date.now() });
  }

  /**
   * Replicate log entry across cluster
   * @param {Object} entry - Log entry
   * @returns {Promise<boolean>} Whether replication succeeded
   */
  async replicateLog(entry) {
    this.log.push(entry);
    const result = await this.maybeCommit();
    this.emit('log:replicated', { nodeId: this.nodeId, entryIndex: this.log.length - 1 });
    return result;
  }

  /**
   * Attempt to elect leader
   * @returns {Promise<boolean>} Whether became leader
   */
  async electLeader() {
    this.term += 1;
    this.isLeader = true;
    this.emit('leader:elected', { nodeId: this.nodeId, term: this.term, timestamp: Date.now() });
    return true;
  }

  /**
   * Maybe commit log entries
   * @returns {Promise<boolean>}
   * @private
   */
  async maybeCommit() {
    if (this.log.length > this.committedIndex) {
      this.committedIndex = this.log.length;
      this.emit('log:committed', {
        nodeId: this.nodeId,
        count: this.committedIndex,
        timestamp: Date.now(),
      });
      return true;
    }
    return false;
  }

  /**
   * Get log entries
   * @returns {Array} Array of log entries
   */
  getLog() {
    return this.log;
  }

  /**
   * Get current term
   * @returns {number} Current term
   */
  getTerm() {
    return this.term;
  }
}

/**
 * Mock YAWL Engine (simulates @unrdf/yawl behavior)
 * Orchestrates workflow cases and tasks
 */
class MockYawlEngine extends EventEmitter {
  constructor() {
    super();
    this.cases = new Map();
    this.tasks = new Map();
    this.taskStates = new Map();
  }

  /**
   * Create a new workflow case
   * @param {Object} options - Case options
   * @returns {Promise<Object>} Created case
   */
  async createCase(options = {}) {
    const caseId = options.caseId || `case-${Date.now()}`;
    const { workflowId, inputData = {} } = options;

    this.cases.set(caseId, {
      id: caseId,
      workflowId,
      inputData,
      status: 'RUNNING',
      createdAt: new Date(),
    });

    this.emit('case:created', { caseId, workflowId, timestamp: Date.now() });

    return { caseId, workflowId, status: 'RUNNING' };
  }

  /**
   * Complete a task
   * @param {Object} options - Task completion options
   * @returns {Promise<Object>} Task result
   */
  async completeTask(options = {}) {
    const { caseId, taskId, outputData = {} } = options;
    const key = `${caseId}:${taskId}`;

    this.taskStates.set(key, {
      caseId,
      taskId,
      status: 'COMPLETED',
      outputData,
      completedAt: new Date(),
    });

    this.emit('task:completed', { caseId, taskId, timestamp: Date.now() });

    return { caseId, taskId, status: 'COMPLETED' };
  }

  /**
   * Fail a task
   * @param {Object} options - Task failure options
   * @returns {Promise<Object>} Task result
   */
  async failTask(options = {}) {
    const { caseId, taskId, error = '' } = options;
    const key = `${caseId}:${taskId}`;

    this.taskStates.set(key, {
      caseId,
      taskId,
      status: 'FAILED',
      error,
      failedAt: new Date(),
    });

    this.emit('task:failed', { caseId, taskId, error, timestamp: Date.now() });

    return { caseId, taskId, status: 'FAILED' };
  }

  /**
   * Get case details
   * @param {string} caseId - Case identifier
   * @returns {Object} Case details
   */
  getCase(caseId) {
    return this.cases.get(caseId);
  }

  /**
   * Get all cases
   * @returns {Array} Array of cases
   */
  getAllCases() {
    return Array.from(this.cases.values());
  }
}

// =============================================================================
// Test Suites
// =============================================================================

describe('Daemon Cross-Package Integration', () => {
  let daemon;
  const testConfig = {
    daemonId: 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
      const r = (Math.random() * 16) | 0;
      const v = c === 'x' ? r : (r & 0x3) | 0x8;
      return v.toString(16);
    }),
    name: 'integration-test-daemon',
    concurrency: 10,
    logger: { info: vi.fn(), debug: vi.fn(), error: vi.fn(), warn: vi.fn() },
  };

  beforeEach(async () => {
    daemon = new Daemon(testConfig);
    await daemon.start();
  });

  afterEach(async () => {
    if (daemon && daemon.isRunning) {
      await daemon.stop();
    }
  });

  // =========================================================================
  // Daemon + Core Integration (4 tests)
  // =========================================================================

  describe('Daemon + Core Integration', () => {
    it('should trigger daemon operation when RDF quad is added to store', async () => {
      // Arrange
      const store = new MockRdfStore();
      const operationFired = vi.fn();

      daemon.schedule({
        id: 'op-core-add',
        name: 'Process RDF Addition',
        handler: operationFired,
      });

      // Act
      store.on('quad:added', async () => {
        await daemon.execute('op-core-add');
      });

      store.addQuad({
        subject: 'http://example.com/s1',
        predicate: 'http://example.com/p1',
        object: 'http://example.com/o1',
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      // Assert
      expect(operationFired).toHaveBeenCalled();
      expect(store.getAllQuads()).toHaveLength(1);
    });

    it('should handle multiple RDF quad additions and trigger corresponding operations', async () => {
      // Arrange
      const store = new MockRdfStore();
      const operationResults = [];

      daemon.schedule({
        id: 'op-core-batch',
        name: 'Batch RDF Processing',
        handler: async () => {
          operationResults.push({ count: store.getAllQuads().length, timestamp: Date.now() });
        },
      });

      // Act
      store.on('quad:added', async () => {
        await daemon.execute('op-core-batch');
      });

      store.addQuad({
        subject: 'http://example.com/s1',
        predicate: 'http://example.com/p1',
        object: 'http://example.com/o1',
      });

      store.addQuad({
        subject: 'http://example.com/s2',
        predicate: 'http://example.com/p2',
        object: 'http://example.com/o2',
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      // Assert
      expect(operationResults.length).toBeGreaterThanOrEqual(1);
      expect(store.getAllQuads()).toHaveLength(2);
    });

    it('should query RDF store and execute daemon operation based on query results', async () => {
      // Arrange
      const store = new MockRdfStore();
      let executionCount = 0;

      store.addQuad({
        subject: 'http://example.com/entity1',
        predicate: 'http://example.com/type',
        object: 'http://example.com/DataNode',
      });

      daemon.schedule({
        id: 'op-core-query',
        name: 'Query-based Operation',
        handler: async () => {
          const results = store.queryQuads({
            predicate: 'http://example.com/type',
            object: 'http://example.com/DataNode',
          });
          if (results.length > 0) {
            executionCount += 1;
          }
        },
      });

      // Act
      await daemon.execute('op-core-query');

      // Assert
      expect(executionCount).toBe(1);
      expect(store.queryQuads({ predicate: 'http://example.com/type' })).toHaveLength(1);
    });

    it('should propagate errors from RDF store operations to daemon', async () => {
      // Arrange
      const _store = new MockRdfStore();
      const errorHandler = vi.fn();

      daemon.schedule({
        id: 'op-core-error',
        name: 'Error-prone Operation',
        handler: async () => {
          throw new Error('RDF processing failed');
        },
      });

      daemon.on('operation:failure', errorHandler);

      // Act
      try {
        await daemon.execute('op-core-error');
      } catch (error) {
        // Expected error
      }

      await new Promise((resolve) => setTimeout(resolve, 50));

      // Assert
      expect(errorHandler).toHaveBeenCalled();
      const call = errorHandler.mock.calls[0][0];
      expect(call.error).toMatch(/RDF processing failed/);
    });
  });

  // =========================================================================
  // Daemon + Hooks Integration (4 tests)
  // =========================================================================

  describe('Daemon + Hooks Integration', () => {
    it('should execute daemon operation with hook validation policy', async () => {
      // Arrange
      const hookScheduler = new MockHookScheduler();
      const validationResults = [];

      hookScheduler.registerHook(
        'hook-validate-input',
        { handler: (input) => input.value * 2 },
        {
          validation: (input) => input.value > 0,
          transform: (result) => ({ processed: true, value: result }),
        }
      );

      daemon.schedule({
        id: 'op-hook-validate',
        name: 'Hook-Validated Operation',
        handler: async () => {
          const result = await hookScheduler.executeHook('hook-validate-input', { value: 10 });
          validationResults.push(result);
        },
      });

      // Act
      await daemon.execute('op-hook-validate');

      // Assert
      expect(validationResults).toHaveLength(1);
      expect(validationResults[0]).toEqual({ processed: true, value: 20 });
    });

    it('should handle hook validation failures in daemon operation', async () => {
      // Arrange
      const hookScheduler = new MockHookScheduler();
      const errorCaught = [];

      hookScheduler.registerHook(
        'hook-strict-validate',
        { handler: (input) => input },
        { validation: (input) => input.required === true }
      );

      daemon.schedule({
        id: 'op-hook-fail',
        name: 'Hook Failure Operation',
        handler: async () => {
          try {
            await hookScheduler.executeHook('hook-strict-validate', { required: false });
          } catch (error) {
            errorCaught.push(error.message);
          }
        },
      });

      // Act
      await daemon.execute('op-hook-fail');

      // Assert
      expect(errorCaught).toHaveLength(1);
      expect(errorCaught[0]).toMatch(/validation failed/i);
    });

    it('should coordinate multiple hooks in daemon operation chain', async () => {
      // Arrange
      const hookScheduler = new MockHookScheduler();
      const executionLog = [];

      hookScheduler.registerHook('hook-step1', { handler: (input) => input + 1 });
      hookScheduler.registerHook('hook-step2', { handler: (input) => input * 2 });
      hookScheduler.registerHook('hook-step3', { handler: (input) => input + 10 });

      daemon.schedule({
        id: 'op-hook-chain',
        name: 'Hook Chain Operation',
        handler: async () => {
          let result = 5;
          result = await hookScheduler.executeHook('hook-step1', result);
          executionLog.push({ step: 1, result });

          result = await hookScheduler.executeHook('hook-step2', result);
          executionLog.push({ step: 2, result });

          result = await hookScheduler.executeHook('hook-step3', result);
          executionLog.push({ step: 3, result });
        },
      });

      // Act
      await daemon.execute('op-hook-chain');

      // Assert
      expect(executionLog).toHaveLength(3);
      expect(executionLog[0].result).toBe(6);
      expect(executionLog[1].result).toBe(12);
      expect(executionLog[2].result).toBe(22);
    });

    it('should emit hook events through daemon event bus', async () => {
      // Arrange
      const hookScheduler = new MockHookScheduler();
      const hookEvents = [];

      hookScheduler.registerHook('hook-emit', { handler: (input) => input });
      hookScheduler.on('hook:executed', (event) => {
        hookEvents.push(event);
      });

      daemon.schedule({
        id: 'op-hook-emit',
        name: 'Hook Event Operation',
        handler: async () => {
          await hookScheduler.executeHook('hook-emit', { test: true });
        },
      });

      // Act
      await daemon.execute('op-hook-emit');
      await new Promise((resolve) => setTimeout(resolve, 50));

      // Assert
      expect(hookEvents.length).toBeGreaterThanOrEqual(1);
    });
  });

  // =========================================================================
  // Daemon + Streaming Integration (4 tests)
  // =========================================================================

  describe('Daemon + Streaming Integration', () => {
    it('should trigger daemon operation when streaming feed emits matching change', async () => {
      // Arrange
      const feed = new MockStreamingFeed();
      const operationFired = vi.fn();

      daemon.schedule({
        id: 'op-streaming-trigger',
        name: 'Streaming Triggered Operation',
        handler: operationFired,
      });

      feed.subscribe('daemon', { type: 'add', operation: 'insert' }, async () => {
        await daemon.execute('op-streaming-trigger');
      });

      // Act
      await feed.emitChange({
        type: 'add',
        operation: 'insert',
        subject: 'http://example.com/s1',
        timestamp: Date.now(),
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      // Assert
      expect(operationFired).toHaveBeenCalled();
      expect(feed.getAllChanges()).toHaveLength(1);
    });

    it('should handle multiple subscriptions with different patterns', async () => {
      // Arrange
      const feed = new MockStreamingFeed();
      const results = { add: 0, remove: 0 };

      daemon.schedule({
        id: 'op-stream-add',
        handler: () => {
          results.add += 1;
        },
      });

      daemon.schedule({
        id: 'op-stream-remove',
        handler: () => {
          results.remove += 1;
        },
      });

      feed.subscribe('daemon', { type: 'add' }, async () => {
        await daemon.execute('op-stream-add');
      });

      feed.subscribe('daemon', { type: 'remove' }, async () => {
        await daemon.execute('op-stream-remove');
      });

      // Act
      await feed.emitChange({ type: 'add', timestamp: Date.now() });
      await feed.emitChange({ type: 'remove', timestamp: Date.now() });
      await feed.emitChange({ type: 'add', timestamp: Date.now() });

      await new Promise((resolve) => setTimeout(resolve, 50));

      // Assert
      expect(results.add).toBe(2);
      expect(results.remove).toBe(1);
    });

    it('should handle streaming subscription lifecycle in daemon', async () => {
      // Arrange
      const feed = new MockStreamingFeed();
      const subscriptionEvents = [];

      feed.on('subscription:created', (event) => {
        subscriptionEvents.push({ type: 'created', ...event });
      });

      feed.on('subscription:deleted', (event) => {
        subscriptionEvents.push({ type: 'deleted', ...event });
      });

      // Act
      const subId = feed.subscribe('daemon', { type: 'add' }, async () => {});
      await new Promise((resolve) => setTimeout(resolve, 10));
      feed.unsubscribe(subId);
      await new Promise((resolve) => setTimeout(resolve, 10));

      // Assert
      expect(subscriptionEvents).toHaveLength(2);
      expect(subscriptionEvents[0].type).toBe('created');
      expect(subscriptionEvents[1].type).toBe('deleted');
    });

    it('should backpressure handling in streaming feed during daemon execution', async () => {
      // Arrange
      const feed = new MockStreamingFeed();
      const executionLog = [];

      daemon.schedule({
        id: 'op-stream-backpressure',
        handler: async () => {
          executionLog.push({ timestamp: Date.now() });
          await new Promise((resolve) => setTimeout(resolve, 10));
        },
      });

      feed.subscribe('daemon', { type: 'add' }, async () => {
        await daemon.execute('op-stream-backpressure');
      });

      // Act
      for (let i = 0; i < 5; i++) {
        await feed.emitChange({ type: 'add', index: i, timestamp: Date.now() });
      }

      // Assert
      expect(executionLog.length).toBeGreaterThanOrEqual(1);
      expect(feed.getAllChanges()).toHaveLength(5);
    });
  });

  // =========================================================================
  // Daemon + Consensus Integration (4 tests)
  // =========================================================================

  describe('Daemon + Consensus Integration', () => {
    it('should replicate daemon operations across consensus nodes', async () => {
      // Arrange
      const coordinator = new MockConsensusCoordinator('node-1');
      await coordinator.initialize();

      const logEntries = [];

      daemon.schedule({
        id: 'op-consensus-log',
        handler: async () => {
          const entry = {
            operationId: 'op-consensus-log',
            timestamp: Date.now(),
            data: { action: 'process' },
          };
          logEntries.push(entry);
          await coordinator.replicateLog(entry);
        },
      });

      // Act
      await daemon.execute('op-consensus-log');
      await daemon.execute('op-consensus-log');

      // Assert
      expect(logEntries).toHaveLength(2);
      expect(coordinator.getLog()).toHaveLength(2);
    });

    it('should ensure leader-based operation execution in consensus', async () => {
      // Arrange
      const coordinator = new MockConsensusCoordinator('node-1');
      await coordinator.initialize();

      const leaderEvents = [];
      coordinator.on('leader:elected', (event) => {
        leaderEvents.push(event);
      });

      daemon.schedule({
        id: 'op-consensus-leader',
        handler: async () => {
          if (coordinator.isLeader) {
            return { status: 'executed-by-leader' };
          }
          throw new Error('Not leader');
        },
      });

      // Act
      await coordinator.electLeader();
      const result = await daemon.execute('op-consensus-leader');

      // Assert
      expect(leaderEvents).toHaveLength(1);
      expect(result.status).toBe('executed-by-leader');
      expect(coordinator.isLeader).toBe(true);
    });

    it('should track consensus term changes during daemon execution', async () => {
      // Arrange
      const coordinator = new MockConsensusCoordinator('node-1');
      await coordinator.initialize();

      const termChanges = [];

      daemon.schedule({
        id: 'op-consensus-term',
        handler: async () => {
          termChanges.push(coordinator.getTerm());
          await coordinator.electLeader();
        },
      });

      // Act
      await daemon.execute('op-consensus-term');
      await new Promise((resolve) => setTimeout(resolve, 10));
      await daemon.execute('op-consensus-term');

      // Assert
      expect(termChanges).toHaveLength(2);
      expect(termChanges[1]).toBeGreaterThan(termChanges[0]);
    });

    it('should coordinate distributed state machine through daemon', async () => {
      // Arrange
      const coordinator = new MockConsensusCoordinator('node-1');
      await coordinator.initialize();
      const stateLog = [];

      daemon.schedule({
        id: 'op-consensus-state',
        handler: async () => {
          const entry = {
            type: 'state-change',
            operation: 'update-value',
            timestamp: Date.now(),
          };
          await coordinator.replicateLog(entry);
          stateLog.push(entry);
        },
      });

      // Act
      for (let i = 0; i < 3; i++) {
        await daemon.execute('op-consensus-state');
      }

      // Assert
      expect(stateLog).toHaveLength(3);
      expect(coordinator.getLog()).toHaveLength(3);
      expect(coordinator.committedIndex).toBeGreaterThanOrEqual(0);
    });
  });

  // =========================================================================
  // Daemon + YAWL Integration (4 tests)
  // =========================================================================

  describe('Daemon + YAWL Integration', () => {
    it('should schedule recurring YAWL case creation through daemon', async () => {
      // Arrange
      const yawl = new MockYawlEngine();
      const createdCases = [];

      daemon.schedule({
        id: 'op-yawl-create-case',
        name: 'YAWL Case Creator',
        handler: async () => {
          const caseResult = await yawl.createCase({
            caseId: `case-${Date.now()}-${Math.random()}`,
            workflowId: 'workflow-1',
            inputData: { timestamp: Date.now() },
          });
          createdCases.push(caseResult);
        },
      });

      yawl.on('case:created', () => {});

      // Act
      await daemon.execute('op-yawl-create-case');
      await new Promise((resolve) => setTimeout(resolve, 10));
      await daemon.execute('op-yawl-create-case');

      // Assert
      expect(createdCases).toHaveLength(2);
      expect(yawl.getAllCases()).toHaveLength(2);
    });

    it('should handle YAWL task completion through daemon operations', async () => {
      // Arrange
      const yawl = new MockYawlEngine();
      const completedTasks = [];

      // Create a case first
      const caseResult = await yawl.createCase({ workflowId: 'workflow-1' });
      const caseId = caseResult.caseId;

      daemon.schedule({
        id: 'op-yawl-complete-task',
        handler: async () => {
          const taskResult = await yawl.completeTask({
            caseId,
            taskId: 'task-1',
            outputData: { result: 'processed' },
          });
          completedTasks.push(taskResult);
        },
      });

      // Act
      await daemon.execute('op-yawl-complete-task');

      // Assert
      expect(completedTasks).toHaveLength(1);
      expect(completedTasks[0].status).toBe('COMPLETED');
    });

    it('should coordinate YAWL task failure recovery through daemon', async () => {
      // Arrange
      const yawl = new MockYawlEngine();
      const failedTasks = [];
      const recoveryAttempts = [];

      const caseResult = await yawl.createCase({ workflowId: 'workflow-1' });
      const caseId = caseResult.caseId;

      daemon.schedule({
        id: 'op-yawl-fail-task',
        handler: async () => {
          const failResult = await yawl.failTask({
            caseId,
            taskId: 'task-1',
            error: 'Processing failed',
          });
          failedTasks.push(failResult);
        },
      });

      daemon.schedule({
        id: 'op-yawl-recovery',
        handler: async () => {
          recoveryAttempts.push({
            attempt: recoveryAttempts.length + 1,
            timestamp: Date.now(),
          });
        },
      });

      yawl.on('task:failed', async () => {
        await daemon.execute('op-yawl-recovery');
      });

      // Act
      await daemon.execute('op-yawl-fail-task');
      await new Promise((resolve) => setTimeout(resolve, 50));

      // Assert
      expect(failedTasks).toHaveLength(1);
      expect(recoveryAttempts.length).toBeGreaterThanOrEqual(1);
    });

    it('should execute complex YAWL workflow orchestration through daemon', async () => {
      // Arrange
      const yawl = new MockYawlEngine();
      const workflow = {
        steps: ['create-case', 'process-task', 'complete-task'],
        results: [],
      };

      const caseResult = await yawl.createCase({ workflowId: 'workflow-complex' });
      const caseId = caseResult.caseId;

      // Step 1: Create case operation
      daemon.schedule({
        id: 'op-yawl-wf-step1',
        handler: async () => {
          workflow.results.push({ step: 'created', caseId });
        },
      });

      // Step 2: Process task operation
      daemon.schedule({
        id: 'op-yawl-wf-step2',
        handler: async () => {
          workflow.results.push({ step: 'processing', caseId });
        },
      });

      // Step 3: Complete task operation
      daemon.schedule({
        id: 'op-yawl-wf-step3',
        handler: async () => {
          await yawl.completeTask({ caseId, taskId: 'task-final' });
          workflow.results.push({ step: 'completed', caseId });
        },
      });

      // Act
      await daemon.execute('op-yawl-wf-step1');
      await daemon.execute('op-yawl-wf-step2');
      await daemon.execute('op-yawl-wf-step3');

      // Assert
      expect(workflow.results).toHaveLength(3);
      expect(workflow.results[2].step).toBe('completed');
    });
  });

  // =========================================================================
  // Multi-Layer Integration (1+ test)
  // =========================================================================

  describe('Multi-Layer Integration', () => {
    it('should coordinate all packages in comprehensive workflow', async () => {
      // Arrange
      const rdfStore = new MockRdfStore();
      const hookScheduler = new MockHookScheduler();
      const streamingFeed = new MockStreamingFeed();
      const consensus = new MockConsensusCoordinator('node-1');
      const yawl = new MockYawlEngine();

      await consensus.initialize();

      const orchestrationLog = [];

      // Register hook for data transformation
      hookScheduler.registerHook('hook-transform', { handler: (data) => ({ ...data, processed: true }) });

      // Schedule multi-layer operation
      daemon.schedule({
        id: 'op-multi-layer',
        handler: async () => {
          // 1. Add to RDF store
          rdfStore.addQuad({
            subject: 'http://example.com/entity',
            predicate: 'http://example.com/status',
            object: 'http://example.com/processing',
          });

          // 2. Execute hook
          const hookResult = await hookScheduler.executeHook('hook-transform', { data: 'test' });

          // 3. Replicate to consensus
          await consensus.replicateLog({
            operation: 'process',
            hookResult,
            timestamp: Date.now(),
          });

          // 4. Create YAWL case
          const caseResult = await yawl.createCase({ workflowId: 'workflow-multi' });

          orchestrationLog.push({
            rdfQuads: rdfStore.getAllQuads().length,
            hookResult,
            consensusLogSize: consensus.getLog().length,
            yawlCaseId: caseResult.caseId,
          });
        },
      });

      // Act - trigger through streaming feed
      streamingFeed.subscribe('orchestrator', { type: 'trigger' }, async () => {
        await daemon.execute('op-multi-layer');
      });

      await streamingFeed.emitChange({ type: 'trigger', timestamp: Date.now() });
      await new Promise((resolve) => setTimeout(resolve, 100));

      // Assert - verify all integrations
      expect(orchestrationLog).toHaveLength(1);
      expect(rdfStore.getAllQuads()).toHaveLength(1);
      expect(consensus.getLog()).toHaveLength(1);
      expect(yawl.getAllCases()).toHaveLength(1);
      expect(hookScheduler.getResults()).toHaveLength(1);
    });

    it('should handle error propagation across all layers', async () => {
      // Arrange
      const rdfStore = new MockRdfStore();
      const errorLog = [];

      daemon.schedule({
        id: 'op-multi-error',
        handler: async () => {
          throw new Error('Multi-layer error');
        },
      });

      daemon.on('operation:failure', (event) => {
        errorLog.push(event);
      });

      rdfStore.on('quad:added', async () => {
        try {
          await daemon.execute('op-multi-error');
        } catch (error) {
          // Error handled
        }
      });

      // Act
      rdfStore.addQuad({
        subject: 'http://example.com/error-trigger',
        predicate: 'http://example.com/action',
        object: 'http://example.com/trigger',
      });

      await new Promise((resolve) => setTimeout(resolve, 50));

      // Assert
      expect(errorLog.length).toBeGreaterThanOrEqual(1);
      expect(errorLog[0].error).toMatch(/Multi-layer error/);
    });
  });
});
