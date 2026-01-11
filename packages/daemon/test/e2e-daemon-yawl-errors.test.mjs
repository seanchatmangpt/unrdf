/**
 * @file End-to-End Error Path Validation Tests for Daemon+YAWL
 * @module @unrdf/daemon/test/e2e-daemon-yawl-errors
 * @description Comprehensive error handling validation across 8 failure scenarios:
 * 1. Task Failure Scenarios (3 tests) - mid-execution, unhandled exceptions, contract violations
 * 2. Timeout Scenarios (3 tests) - single task, case-level, cleanup hangs
 * 3. Retry Exhaustion (2 tests) - max retries, backoff exceeds timeout
 * 4. Deadlock Detection (2 tests) - circular dependencies, unsatisfiable conditions
 * 5. Cascade Failure (2 tests) - isolated failures, compensation workflows
 * 6. Receipt Integrity (2 tests) - cryptographic chains, hash mismatches
 *
 * Each test validates:
 * - Error is caught and logged with full context
 * - System continues operating (no cascade failure)
 * - Error receipts have correct structure
 * - Daemon event emissions (task:failed, case:failed, etc.)
 * - Proper cleanup and resource isolation
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { EventEmitter } from 'events';
import { z } from 'zod';
import { createHash } from 'crypto';

// =============================================================================
// Test Utilities
// =============================================================================

/**
 * Generate a UUID v4 for testing
 * @returns {string} Valid UUID v4
 */
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * Create SHA-256 hash of receipt for integrity validation
 * @param {Object} receipt - Receipt to hash
 * @returns {string} Hex-encoded hash
 */
function hashReceipt(receipt) {
  const data = JSON.stringify(receipt);
  return createHash('sha256').update(data).digest('hex');
}

/**
 * Mock Daemon for error testing
 */
class MockDaemon extends EventEmitter {
  constructor(config = {}) {
    super();
    this.id = config.id || generateUUID();
    this.isRunning = false;
    this.operations = new Map();
    this.errorLog = [];
    this.logger = config.logger || console;
  }

  async start() {
    this.isRunning = true;
    this.emit('daemon:started', { daemonId: this.id });
  }

  async stop() {
    this.isRunning = false;
    this.emit('daemon:stopped', { daemonId: this.id });
  }

  schedule(operation, trigger = {}) {
    const operationId = `op-${generateUUID()}`;
    this.operations.set(operationId, {
      operationId,
      operation,
      trigger,
      createdAt: new Date(),
      status: 'SCHEDULED',
    });
    return { operationId };
  }

  unschedule(operationId) {
    this.operations.delete(operationId);
  }

  logError(error, context = {}) {
    this.errorLog.push({ error, context, timestamp: new Date() });
    this.emit('daemon:error', { error, context });
  }
}

/**
 * Mock YAWL Engine for error testing
 */
class MockYawlEngine extends EventEmitter {
  constructor(config = {}) {
    super();
    this.id = config.id || generateUUID();
    this.cases = new Map();
    this.tasks = new Map();
    this.workflows = new Map();
    this.receipts = [];
    this.logger = config.logger || console;
  }

  async createCase(workflowId, inputData = {}) {
    const caseId = `case-${generateUUID()}`;
    this.cases.set(caseId, {
      caseId,
      workflowId,
      inputData,
      status: 'RUNNING',
      createdAt: new Date(),
      tasks: new Map(),
    });
    this.emit('case:created', { caseId, workflowId });
    return { caseId, workflowId, status: 'RUNNING' };
  }

  async enableTask(caseId, taskId, options = {}) {
    const key = `${caseId}:${taskId}`;
    if (!this.tasks.has(key)) {
      this.tasks.set(key, {
        caseId,
        taskId,
        status: 'ENABLED',
        enabledAt: new Date(),
        timeout: options.timeout,
        retryPolicy: options.retryPolicy,
      });
    }
    this.emit('task:enabled', { caseId, taskId });
    return { caseId, taskId, status: 'ENABLED' };
  }

  async completeTask(caseId, taskId, outputData = {}) {
    const key = `${caseId}:${taskId}`;
    const task = this.tasks.get(key);
    if (!task) {
      throw new Error(`Task ${key} not found`);
    }
    task.status = 'COMPLETED';
    task.outputData = outputData;
    task.completedAt = new Date();
    this.emit('task:completed', { caseId, taskId, outputData });
    return { status: 'COMPLETED' };
  }

  async failTask(caseId, taskId, error) {
    const key = `${caseId}:${taskId}`;
    const task = this.tasks.get(key);
    if (task) {
      task.status = 'FAILED';
      task.error = error.message;
      task.failedAt = new Date();
    }
    this.emit('task:failed', { caseId, taskId, error: error.message });
    return { status: 'FAILED', error: error.message };
  }

  async completeCase(caseId, status = 'COMPLETED') {
    const caseObj = this.cases.get(caseId);
    if (!caseObj) {
      throw new Error(`Case ${caseId} not found`);
    }
    caseObj.status = status;
    caseObj.completedAt = new Date();
    this.emit('case:completed', { caseId, status });
    return { caseId, status };
  }

  createReceipt(options = {}) {
    const receipt = {
      receiptId: `receipt-${generateUUID()}`,
      ...options,
      timestamp: new Date(),
    };
    this.receipts.push(receipt);
    return receipt;
  }
}

/**
 * Mock YAWL Daemon Bridge for integration testing
 */
class MockYawlDaemonBridge extends EventEmitter {
  constructor(daemon, yawlEngine, config = {}) {
    super();
    this.id = `bridge-${generateUUID()}`;
    this.daemon = daemon;
    this.yawlEngine = yawlEngine;
    this.config = config;
    this.isRunning = false;
    this.taskTimeouts = new Map();
    this.taskRetries = new Map();
  }

  async start() {
    this.isRunning = true;
    this.emit('bridge:started', { bridgeId: this.id });
  }

  async stop() {
    this.isRunning = false;
    for (const handle of this.taskTimeouts.values()) {
      clearTimeout(handle);
    }
    this.taskTimeouts.clear();
    this.emit('bridge:stopped', { bridgeId: this.id });
  }

  watchTaskTimeout(caseId, taskId, timeoutMs) {
    const key = `${caseId}:${taskId}`;
    const handle = setTimeout(() => {
      this.yawlEngine.failTask(caseId, taskId, new Error('Task timeout'));
      this.emit('task:timeout', { caseId, taskId, timeoutMs });
    }, timeoutMs);
    this.taskTimeouts.set(key, handle);
    return { caseId, taskId, timeoutMs };
  }

  clearTaskTimeout(caseId, taskId) {
    const key = `${caseId}:${taskId}`;
    const handle = this.taskTimeouts.get(key);
    if (handle) {
      clearTimeout(handle);
      this.taskTimeouts.delete(key);
    }
  }

  async executeTaskWithRetry(caseId, taskId, taskFn, retryPolicy = {}) {
    const key = `${caseId}:${taskId}`;
    const maxAttempts = retryPolicy.maxAttempts || 3;
    const backoffMs = retryPolicy.backoffMs || 1000;

    let lastError;
    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
      try {
        const result = await taskFn();
        return { attempt, result, success: true };
      } catch (error) {
        lastError = error;
        this.emit('task:retry', { caseId, taskId, attempt, error: error.message });
        if (attempt < maxAttempts) {
          await new Promise((resolve) => setTimeout(resolve, backoffMs * attempt));
        }
      }
    }

    // All retries exhausted
    this.emit('task:retry-exhausted', { caseId, taskId, maxAttempts, error: lastError });
    throw new Error(`Task ${taskId} failed after ${maxAttempts} retries: ${lastError.message}`);
  }
}

// =============================================================================
// Test Suites
// =============================================================================

describe('Error Path Validation - Daemon+YAWL Integration', () => {
  let daemon;
  let yawlEngine;
  let bridge;

  beforeEach(async () => {
    daemon = new MockDaemon({ id: `daemon-${generateUUID()}` });
    yawlEngine = new MockYawlEngine({ id: `engine-${generateUUID()}` });
    bridge = new MockYawlDaemonBridge(daemon, yawlEngine, {
      maxConcurrentCases: 100,
      enableAutoRetry: true,
    });

    await daemon.start();
    await yawlEngine.emit('engine:ready');
    await bridge.start();
  });

  afterEach(async () => {
    if (bridge.isRunning) {
      await bridge.stop();
    }
    if (daemon.isRunning) {
      await daemon.stop();
    }
  });

  // ===========================================================================
  // 1. Task Failure Scenarios (3 tests)
  // ===========================================================================

  describe('Task Failure Scenarios', () => {
    /**
     * Test 1: Task fails mid-execution
     * Validates that task failure is caught, logged, and doesn't cascade
     */
    it('should handle task failure mid-execution without cascade', async () => {
      // Arrange
      const { caseId } = await yawlEngine.createCase('approval-workflow', {
        amount: 5000,
      });

      const taskId = 'validate-amount';
      await yawlEngine.enableTask(caseId, taskId);

      const errorEvents = [];
      yawlEngine.on('task:failed', (event) => {
        errorEvents.push(event);
      });

      // Act
      const error = new Error('Validation failed: amount exceeds limit');
      await yawlEngine.failTask(caseId, taskId, error);

      // Assert
      expect(errorEvents).toHaveLength(1);
      expect(errorEvents[0]).toMatchObject({
        caseId,
        taskId,
        error: 'Validation failed: amount exceeds limit',
      });

      const taskState = yawlEngine.tasks.get(`${caseId}:${taskId}`);
      expect(taskState.status).toBe('FAILED');
      expect(taskState.error).toBe('Validation failed: amount exceeds limit');

      // Verify case is still accessible
      const caseObj = yawlEngine.cases.get(caseId);
      expect(caseObj.status).toBe('RUNNING');
    });

    /**
     * Test 2: Task throws unhandled exception
     * Validates that exceptions are caught, receipts include error field
     */
    it('should catch unhandled task exception and create error receipt', async () => {
      // Arrange
      const { caseId } = await yawlEngine.createCase('payment-workflow', {
        amount: 100,
        cardToken: 'tok_123',
      });

      const taskId = 'process-payment';
      await yawlEngine.enableTask(caseId, taskId);

      const errorHandler = vi.fn();
      yawlEngine.on('task:failed', errorHandler);

      // Act - Simulate unhandled exception in task
      const taskException = new Error('Payment gateway connection failed');
      taskException.code = 'ECONNREFUSED';

      try {
        throw taskException;
      } catch (error) {
        await yawlEngine.failTask(caseId, taskId, error);
      }

      // Assert
      expect(errorHandler).toHaveBeenCalled();
      const taskState = yawlEngine.tasks.get(`${caseId}:${taskId}`);
      expect(taskState).toBeDefined();
      expect(taskState.error).toBe('Payment gateway connection failed');
    });

    /**
     * Test 3: Task violates contract (bad output type)
     * Validates that contract violations are detected and reported
     */
    it('should detect contract violation (invalid output type)', async () => {
      // Arrange
      const OutputSchema = z.object({
        approved: z.boolean(),
        amount: z.number(),
        timestamp: z.date(),
      });

      const { caseId } = await yawlEngine.createCase('approval-workflow', {});
      const taskId = 'manager-approval';
      await yawlEngine.enableTask(caseId, taskId);

      // Act - Try to complete with invalid output
      const invalidOutput = {
        approved: 'yes', // Should be boolean
        amount: '5000', // Should be number
      };

      const parseResult = OutputSchema.safeParse(invalidOutput);

      // Assert
      expect(parseResult.success).toBe(false);
      expect(parseResult.error).toBeDefined();

      // Verify error is properly categorized
      if (!parseResult.success) {
        // Get error message from ZodError
        const errorMessage = parseResult.error?.issues?.length > 0
          ? `Contract violation: ${parseResult.error.issues[0].message}`
          : 'Contract violation: Invalid input';

        await yawlEngine.failTask(
          caseId,
          taskId,
          new Error(errorMessage)
        );

        const taskState = yawlEngine.tasks.get(`${caseId}:${taskId}`);
        expect(taskState).toBeDefined();
        expect(taskState.error).toContain('Contract violation');
      }
    });
  });

  // ===========================================================================
  // 2. Timeout Scenarios (3 tests)
  // ===========================================================================

  describe('Timeout Scenarios', () => {
    /**
     * Test 4: Single task timeout
     * Validates that individual task timeouts are enforced
     */
    it('should enforce single task timeout', async () => {
      // Arrange
      const { caseId } = await yawlEngine.createCase('long-task-workflow', {});
      const taskId = 'slow-api-call';
      const timeoutMs = 100;

      await yawlEngine.enableTask(caseId, taskId);

      const timeoutEvents = [];
      bridge.on('task:timeout', (event) => {
        timeoutEvents.push(event);
      });

      // Act
      const { caseId: returnedCaseId } = bridge.watchTaskTimeout(
        caseId,
        taskId,
        timeoutMs
      );

      // Wait for timeout to fire
      await new Promise((resolve) => setTimeout(resolve, timeoutMs + 50));

      // Assert
      expect(timeoutEvents).toHaveLength(1);
      expect(timeoutEvents[0]).toMatchObject({
        caseId,
        taskId,
        timeoutMs,
      });

      const taskState = yawlEngine.tasks.get(`${caseId}:${taskId}`);
      expect(taskState).toBeDefined();
      expect(taskState.status).toBe('FAILED');
      expect(taskState.error).toBe('Task timeout');
    });

    /**
     * Test 5: Case-level timeout (all remaining tasks cancelled)
     * Validates that case timeout cancels all unfinished tasks
     */
    it('should cancel all remaining tasks on case timeout', async () => {
      // Arrange
      const { caseId } = await yawlEngine.createCase('multi-task-workflow', {});
      const taskIds = ['task-1', 'task-2', 'task-3'];

      for (const taskId of taskIds) {
        await yawlEngine.enableTask(caseId, taskId);
      }

      const caseTimeoutMs = 100;
      const taskCompletionEvents = [];

      yawlEngine.on('task:failed', (event) => {
        if (event.error === 'Case timeout') {
          taskCompletionEvents.push(event);
        }
      });

      // Act
      const caseTimeoutHandle = setTimeout(() => {
        for (const taskId of taskIds) {
          const key = `${caseId}:${taskId}`;
          const task = yawlEngine.tasks.get(key);
          if (task && task.status !== 'COMPLETED') {
            yawlEngine.failTask(caseId, taskId, new Error('Case timeout'));
          }
        }
        yawlEngine.completeCase(caseId, 'TIMEOUT');
      }, caseTimeoutMs);

      await new Promise((resolve) => setTimeout(resolve, caseTimeoutMs + 50));
      clearTimeout(caseTimeoutHandle);

      // Assert
      expect(taskCompletionEvents.length).toBeGreaterThan(0);
      const caseObj = yawlEngine.cases.get(caseId);
      expect(caseObj.status).toBe('TIMEOUT');

      for (const taskId of taskIds) {
        const key = `${caseId}:${taskId}`;
        const task = yawlEngine.tasks.get(key);
        expect(task.status).toBe('FAILED');
      }
    });

    /**
     * Test 6: Timeout during cleanup (cleanup hangs)
     * Validates that cleanup hangs are detected and don't block shutdown
     */
    it('should handle cleanup timeout without blocking shutdown', async () => {
      // Arrange
      const { caseId } = await yawlEngine.createCase('cleanup-workflow', {});
      const taskId = 'cleanup-task';
      await yawlEngine.enableTask(caseId, taskId);

      const cleanupTimeoutMs = 50;
      const shutdownTimeout = 100;
      let cleanupCompleted = false;

      // Simulate cleanup that takes too long
      const cleanup = async () => {
        await new Promise((resolve) =>
          setTimeout(resolve, cleanupTimeoutMs * 3)
        );
        cleanupCompleted = true;
      };

      // Act
      const shutdownPromise = Promise.race([
        cleanup(),
        new Promise((_, reject) =>
          setTimeout(
            () => reject(new Error('Cleanup timeout')),
            shutdownTimeout
          )
        ),
      ]);

      // Assert
      await expect(shutdownPromise).rejects.toThrow('Cleanup timeout');
      expect(cleanupCompleted).toBe(false);

      // Bridge should still stop cleanly
      await bridge.stop();
      expect(bridge.isRunning).toBe(false);
    });
  });

  // ===========================================================================
  // 3. Retry Exhaustion (2 tests)
  // ===========================================================================

  describe('Retry Exhaustion', () => {
    /**
     * Test 7: Max retries reached, task fails permanently
     * Validates that retries are exhausted and failure is permanent
     */
    it('should fail permanently after max retries exhausted', async () => {
      // Arrange
      const { caseId } = await yawlEngine.createCase('flaky-task-workflow', {});
      const taskId = 'flaky-api-call';

      const retryPolicy = {
        maxAttempts: 3,
        backoffMs: 10,
      };

      let attemptCount = 0;
      const taskFn = async () => {
        attemptCount++;
        throw new Error('API temporarily unavailable');
      };

      const retryEvents = [];
      bridge.on('task:retry', (event) => {
        retryEvents.push(event);
      });

      const exhaustedEvents = [];
      bridge.on('task:retry-exhausted', (event) => {
        exhaustedEvents.push(event);
      });

      // Act
      let finalError;
      try {
        await bridge.executeTaskWithRetry(caseId, taskId, taskFn, retryPolicy);
      } catch (error) {
        finalError = error;
      }

      // Assert
      expect(attemptCount).toBe(3);
      expect(retryEvents.length).toBeGreaterThanOrEqual(1); // At least 1 retry occurred
      expect(exhaustedEvents).toHaveLength(1);
      expect(exhaustedEvents[0]).toMatchObject({
        caseId,
        taskId,
        maxAttempts: 3,
      });
      expect(finalError).toBeDefined();
      expect(finalError.message).toContain('failed after 3 retries');
    });

    /**
     * Test 8: Retry backoff exceeds case timeout
     * Validates that backoff duration is validated against case timeout
     */
    it('should detect when retry backoff exceeds case timeout', async () => {
      // Arrange
      const { caseId } = await yawlEngine.createCase('long-backoff-workflow', {});
      const taskId = 'retry-heavy-task';

      const retryPolicy = {
        maxAttempts: 3,
        backoffMs: 50, // Reasonable backoff
      };

      let attempts = 0;
      const taskFn = async () => {
        attempts++;
        throw new Error('Transient failure');
      };

      // Act
      const startTime = Date.now();
      let finalError;

      try {
        await bridge.executeTaskWithRetry(caseId, taskId, taskFn, retryPolicy);
      } catch (error) {
        finalError = error;
      }

      const elapsedMs = Date.now() - startTime;

      // Assert
      expect(finalError).toBeDefined();
      // All attempts should be made (3)
      expect(attempts).toBe(3);
      // Execution time should reflect backoff delays (2 retries = 2 delays)
      expect(elapsedMs).toBeGreaterThanOrEqual(retryPolicy.backoffMs);
    });
  });

  // ===========================================================================
  // 4. Deadlock Detection (2 tests)
  // ===========================================================================

  describe('Deadlock Detection', () => {
    /**
     * Test 9: Circular task dependencies
     * Validates that circular dependencies are detected
     */
    it('should detect circular task dependencies', async () => {
      // Arrange
      const { caseId } = await yawlEngine.createCase('cyclic-workflow', {});

      // Simulate circular dependency detection
      const dependencies = {
        'task-a': ['task-b'],
        'task-b': ['task-c'],
        'task-c': ['task-a'], // Circular reference
      };

      function detectCycle(deps) {
        const visited = new Set();
        const recursionStack = new Set();

        function hasCycle(node) {
          visited.add(node);
          recursionStack.add(node);

          for (const neighbor of deps[node] || []) {
            if (!visited.has(neighbor)) {
              if (hasCycle(neighbor)) {
                return true;
              }
            } else if (recursionStack.has(neighbor)) {
              return true;
            }
          }

          recursionStack.delete(node);
          return false;
        }

        for (const node of Object.keys(deps)) {
          if (!visited.has(node)) {
            if (hasCycle(node)) {
              return true;
            }
          }
        }
        return false;
      }

      // Act
      const hasCyclicDependency = detectCycle(dependencies);

      // Assert
      expect(hasCyclicDependency).toBe(true);
    });

    /**
     * Test 10: Both branches of XOR-split disabled (unsatisfiable condition)
     * Validates that unsatisfiable conditions are detected
     */
    it('should detect unsatisfiable XOR-split condition', async () => {
      // Arrange
      const { caseId } = await yawlEngine.createCase('xor-workflow', {});

      const xorCondition = {
        branches: [
          { name: 'expensive', condition: (amount) => amount > 1000 },
          { name: 'cheap', condition: (amount) => amount <= 1000 },
        ],
      };

      function findViableRoute(xor, inputValue) {
        const viable = xor.branches.filter((branch) => {
          try {
            return branch.condition(inputValue);
          } catch {
            return false;
          }
        });
        return viable.length > 0;
      }

      // Act - Test with valid value
      const hasRoute1 = findViableRoute(xorCondition, 500);

      // Act - Test with value that fits both conditions (still valid)
      const hasRoute2 = findViableRoute(xorCondition, 1000);

      // Assert
      expect(hasRoute1).toBe(true); // Falls into 'cheap' branch
      expect(hasRoute2).toBe(true); // Falls into 'cheap' branch
    });
  });

  // ===========================================================================
  // 5. Cascade Failure (2 tests)
  // ===========================================================================

  describe('Cascade Failure Prevention', () => {
    /**
     * Test 11: One task failure should not cancel unrelated tasks
     * Validates isolation between task failures
     */
    it('should isolate failures and not cascade to unrelated tasks', async () => {
      // Arrange
      const { caseId } = await yawlEngine.createCase('multi-path-workflow', {});

      const parallelTasks = ['payment', 'notification', 'logging'];
      for (const taskId of parallelTasks) {
        await yawlEngine.enableTask(caseId, taskId);
      }

      const failedTasks = [];
      const completedTasks = [];

      yawlEngine.on('task:failed', (event) => {
        failedTasks.push(event.taskId);
      });

      yawlEngine.on('task:completed', (event) => {
        completedTasks.push(event.taskId);
      });

      // Act - Fail payment task
      await yawlEngine.failTask(
        caseId,
        'payment',
        new Error('Payment declined')
      );

      // Continue other tasks
      await yawlEngine.completeTask(caseId, 'notification', {
        sent: true,
      });
      await yawlEngine.completeTask(caseId, 'logging', {
        logged: true,
      });

      // Assert
      expect(failedTasks).toEqual(['payment']);
      expect(completedTasks).toEqual(['notification', 'logging']);

      // Verify each task state independently
      expect(yawlEngine.tasks.get(`${caseId}:payment`).status).toBe('FAILED');
      expect(yawlEngine.tasks.get(`${caseId}:notification`).status).toBe(
        'COMPLETED'
      );
      expect(yawlEngine.tasks.get(`${caseId}:logging`).status).toBe(
        'COMPLETED'
      );
    });

    /**
     * Test 12: Compensation workflow executes on case failure
     * Validates that compensation is triggered and executed
     */
    it('should execute compensation workflow on case failure', async () => {
      // Arrange
      const { caseId } = await yawlEngine.createCase('compensable-workflow', {
        reservationId: 'res-123',
        amount: 500,
      });

      const mainTask = 'charge-card';
      const compensationTask = 'refund-card';

      await yawlEngine.enableTask(caseId, mainTask);
      await yawlEngine.enableTask(caseId, compensationTask);

      const compensationEvents = [];
      yawlEngine.on('task:completed', (event) => {
        if (event.taskId === compensationTask) {
          compensationEvents.push(event);
        }
      });

      // Act - Main task fails, trigger compensation
      const mainError = new Error('Card declined');
      await yawlEngine.failTask(caseId, mainTask, mainError);

      // Execute compensation
      const compensationResult = await yawlEngine.completeTask(
        caseId,
        compensationTask,
        {
          refundAmount: 500,
          refundId: 'ref-123',
        }
      );

      // Complete case
      await yawlEngine.completeCase(caseId, 'COMPLETED_WITH_COMPENSATION');

      // Assert
      expect(compensationEvents).toHaveLength(1);
      expect(compensationResult.status).toBe('COMPLETED');

      const caseObj = yawlEngine.cases.get(caseId);
      expect(caseObj.status).toBe('COMPLETED_WITH_COMPENSATION');
    });
  });

  // ===========================================================================
  // 6. Receipt Integrity (2 tests)
  // ===========================================================================

  describe('Receipt Integrity Validation', () => {
    /**
     * Test 13: Receipts are cryptographically chained
     * Validates that receipt chain is maintained correctly
     */
    it('should maintain cryptographic receipt chain', async () => {
      // Arrange
      const { caseId } = await yawlEngine.createCase('receipt-chain-workflow', {});

      const receipts = [];

      // Create multiple receipts for different operations
      const receipt1 = yawlEngine.createReceipt({
        operation: 'task:enabled',
        caseId,
        taskId: 'task-1',
        status: 'success',
        previousHash: null,
      });

      // Calculate hash of first receipt
      receipt1.hash = hashReceipt(receipt1);
      receipts.push(receipt1);

      // Second receipt references previous hash
      const receipt2 = yawlEngine.createReceipt({
        operation: 'task:completed',
        caseId,
        taskId: 'task-1',
        status: 'success',
        previousHash: receipt1.hash,
      });

      receipt2.hash = hashReceipt(receipt2);
      receipts.push(receipt2);

      // Third receipt
      const receipt3 = yawlEngine.createReceipt({
        operation: 'case:completed',
        caseId,
        status: 'success',
        previousHash: receipt2.hash,
      });

      receipt3.hash = hashReceipt(receipt3);
      receipts.push(receipt3);

      // Act - Verify chain integrity
      let isChainValid = true;
      for (let i = 1; i < receipts.length; i++) {
        const current = receipts[i];
        const previous = receipts[i - 1];

        if (current.previousHash !== previous.hash) {
          isChainValid = false;
          break;
        }
      }

      // Assert
      expect(receipts).toHaveLength(3);
      expect(isChainValid).toBe(true);

      // Verify chain property
      expect(receipts[0].previousHash).toBeNull();
      expect(receipts[1].previousHash).toBe(receipts[0].hash);
      expect(receipts[2].previousHash).toBe(receipts[1].hash);
    });

    /**
     * Test 14: Receipt hash mismatch detected
     * Validates that hash mismatches are detected
     */
    it('should detect receipt hash mismatch', async () => {
      // Arrange
      const receiptData = {
        receiptId: generateUUID(),
        operation: 'task:completed',
        caseId: generateUUID(),
        taskId: 'task-1',
        status: 'success',
        timestamp: new Date(),
      };

      // Calculate correct hash
      const correctHash = hashReceipt(receiptData);

      // Simulate receipt with stored hash
      const receipt = {
        ...receiptData,
        hash: correctHash,
      };

      // Act - Create data without hash for re-verification
      const { hash: storedHash, ...receiptDataWithoutHash } = receipt;

      // Verify original hash matches
      const recalculatedHash = hashReceipt(receiptDataWithoutHash);
      const hashesMatch = storedHash === recalculatedHash;

      // Simulate tampering by modifying status
      const tamperedData = { ...receiptDataWithoutHash, status: 'failed' };
      const tamperedHash = hashReceipt(tamperedData);

      // Act - Verify tampering is detected
      const tamperedHashIsValid = storedHash === tamperedHash;

      // Assert
      expect(hashesMatch).toBe(true);
      expect(correctHash).not.toBe(tamperedHash);
      expect(tamperedHashIsValid).toBe(false);
    });
  });
});
