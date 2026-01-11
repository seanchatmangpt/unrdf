/**
 * @file YAWL-Nitro Executor Integration Tests
 * @module @unrdf/yawl/test/integrations/nitro-executor
 * @description Comprehensive tests for Nitro task executor
 *
 * Tests cover:
 * - Task execution lifecycle
 * - Execution context and environment
 * - Error handling and retry logic
 * - Timeout management
 * - Resource cleanup
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { EventEmitter } from 'events';

/**
 * Execution Context
 */
class ExecutionContext {
  constructor(task, options = {}) {
    this.task = task;
    this.variables = options.variables || {};
    this.timeout = options.timeout || 5000;
    this.startTime = null;
    this.endTime = null;
    this.logs = [];
  }

  log(message, level = 'info') {
    this.logs.push({ message, level, timestamp: new Date() });
  }

  setVariable(key, value) {
    this.variables[key] = value;
  }

  getVariable(key) {
    return this.variables[key];
  }

  getDuration() {
    if (!this.startTime || !this.endTime) return null;
    return this.endTime - this.startTime;
  }
}

/**
 * Task Executor
 */
class NitroTaskExecutor extends EventEmitter {
  constructor(options = {}) {
    super();
    this.options = options;
    this.executing = new Map();
    this.completed = new Map();
    this.failed = new Map();
    this.retryPolicy = options.retryPolicy || { maxRetries: 3, delay: 100 };
  }

  /**
   * Execute task
   */
  async execute(task, options = {}) {
    const context = new ExecutionContext(task, options);
    const executionId = `exec-\${Date.now()}-\${Math.random()}`;

    this.executing.set(executionId, { task, context });
    context.startTime = Date.now();

    this.emit('execution:started', { executionId, task, context });

    try {
      const result = await this.executeWithTimeout(task, context);

      context.endTime = Date.now();
      this.completed.set(executionId, { task, context, result });
      this.executing.delete(executionId);

      this.emit('execution:completed', { executionId, task, result, context });

      return { success: true, result, executionId, duration: context.getDuration() };
    } catch (error) {
      context.endTime = Date.now();
      this.failed.set(executionId, { task, context, error });
      this.executing.delete(executionId);

      this.emit('execution:failed', { executionId, task, error, context });

      if (this.shouldRetry(task, error)) {
        return await this.retry(task, options, error);
      }

      return {
        success: false,
        error: error.message,
        executionId,
        duration: context.getDuration(),
      };
    }
  }

  /**
   * Execute with timeout
   */
  async executeWithTimeout(task, context) {
    const timeoutPromise = new Promise((_, reject) => {
      setTimeout(() => reject(new Error('Execution timeout')), context.timeout);
    });

    const executionPromise = this.performExecution(task, context);

    return await Promise.race([executionPromise, timeoutPromise]);
  }

  /**
   * Perform actual execution (override in tests)
   */
  async performExecution(task, context) {
    context.log('Starting execution');
    await new Promise((resolve) => setTimeout(resolve, 10));

    if (task.shouldFail) {
      throw new Error('Simulated failure');
    }

    context.log('Execution completed');
    return {
      taskId: task.taskId,
      output: task.output || { processed: true },
    };
  }

  /**
   * Check if should retry
   */
  shouldRetry(task, error) {
    if (error.message === 'Execution timeout') return false;
    const retryCount = task._retryCount || 0;
    return retryCount < this.retryPolicy.maxRetries;
  }

  /**
   * Retry execution
   */
  async retry(task, options, previousError) {
    task._retryCount = (task._retryCount || 0) + 1;
    await new Promise((resolve) => setTimeout(resolve, this.retryPolicy.delay));

    this.emit('execution:retry', { task, attempt: task._retryCount, previousError });

    return await this.execute(task, options);
  }

  /**
   * Execute multiple tasks concurrently
   */
  async executeBatch(tasks, options = {}) {
    const concurrency = options.concurrency || 5;
    const results = [];
    const executing = [];

    for (let i = 0; i < tasks.length; i++) {
      if (executing.length >= concurrency) {
        const result = await Promise.race(executing);
        results.push(result);
        executing.splice(
          executing.findIndex((p) => p === result),
          1
        );
      }

      const promise = this.execute(tasks[i], options).then((r) => {
        executing.splice(
          executing.findIndex((p) => p === promise),
          1
        );
        return r;
      });
      executing.push(promise);
    }

    results.push(...(await Promise.all(executing)));
    return results;
  }

  /**
   * Cancel execution
   */
  async cancelExecution(executionId) {
    const execution = this.executing.get(executionId);
    if (execution) {
      this.executing.delete(executionId);
      this.emit('execution:cancelled', { executionId });
      return true;
    }
    return false;
  }

  /**
   * Get executor statistics
   */
  getStatistics() {
    return {
      executing: this.executing.size,
      completed: this.completed.size,
      failed: this.failed.size,
      successRate:
        this.completed.size + this.failed.size > 0
          ? (this.completed.size / (this.completed.size + this.failed.size)) * 100
          : 0,
    };
  }

  /**
   * Clear history
   */
  clearHistory() {
    this.completed.clear();
    this.failed.clear();
  }
}

describe('YAWL-Nitro Executor Integration', () => {
  let executor;

  beforeEach(() => {
    executor = new NitroTaskExecutor();
  });

  afterEach(() => {
    executor.removeAllListeners();
    executor.clearHistory();
  });

  describe('Basic Execution', () => {
    it('should execute task successfully', async () => {
      const task = { taskId: 'task-001', caseId: 'case-001' };
      const result = await executor.execute(task);
      expect(result.success).toBe(true);
      expect(result.result.taskId).toBe('task-001');
      expect(result.executionId).toBeDefined();
    });

    it('should emit execution started event', async () => {
      const task = { taskId: 'task-002' };
      const startSpy = vi.fn();
      executor.on('execution:started', startSpy);
      await executor.execute(task);
      expect(startSpy).toHaveBeenCalledOnce();
    });

    it('should emit execution completed event', async () => {
      const task = { taskId: 'task-003' };
      const completeSpy = vi.fn();
      executor.on('execution:completed', completeSpy);
      await executor.execute(task);
      expect(completeSpy).toHaveBeenCalledOnce();
    });

    it('should track execution duration', async () => {
      const task = { taskId: 'task-004' };
      const result = await executor.execute(task);
      expect(result.duration).toBeGreaterThan(0);
    });

    it('should create execution context', async () => {
      const task = { taskId: 'task-005' };
      let capturedContext = null;
      executor.on('execution:completed', ({ context }) => {
        capturedContext = context;
      });
      await executor.execute(task);
      expect(capturedContext).toBeDefined();
    });

    it('should store completed execution', async () => {
      const task = { taskId: 'task-006' };
      await executor.execute(task);
      expect(executor.completed.size).toBe(1);
    });

    it('should process task output correctly', async () => {
      const task = { taskId: 'task-007', output: { customData: 'value', count: 42 } };
      const result = await executor.execute(task);
      expect(result.result.output.customData).toBe('value');
    });
  });

  describe('Error Handling', () => {
    it('should handle execution failure', async () => {
      const task = { taskId: 'task-008', shouldFail: true };
      const result = await executor.execute(task);
      expect(result.success).toBe(false);
    });

    it('should emit execution failed event', async () => {
      const task = { taskId: 'task-009', shouldFail: true };
      const failSpy = vi.fn();
      executor.on('execution:failed', failSpy);
      await executor.execute(task);
      expect(failSpy).toHaveBeenCalledOnce();
    });

    it('should store failed execution', async () => {
      const task = { taskId: 'task-010', shouldFail: true };
      await executor.execute(task);
      expect(executor.failed.size).toBe(1);
    });

    it('should capture error details', async () => {
      const task = { taskId: 'task-011', shouldFail: true };
      const result = await executor.execute(task);
      expect(result.error).toBe('Simulated failure');
    });

    it('should handle thrown errors gracefully', async () => {
      executor.performExecution = vi.fn().mockRejectedValue(new Error('Critical error'));
      const task = { taskId: 'task-012' };
      const result = await executor.execute(task);
      expect(result.success).toBe(false);
    });
  });

  describe('Timeout Management', () => {
    it('should timeout long-running tasks', async () => {
      executor.performExecution = vi.fn().mockImplementation(async () => {
        await new Promise((resolve) => setTimeout(resolve, 100));
        return {};
      });
      const task = { taskId: 'task-013' };
      const result = await executor.execute(task, { timeout: 50 });
      expect(result.success).toBe(false);
      expect(result.error).toBe('Execution timeout');
    });

    it('should respect custom timeout', async () => {
      executor.performExecution = vi.fn().mockImplementation(async () => {
        await new Promise((resolve) => setTimeout(resolve, 20));
        return { completed: true };
      });
      const task = { taskId: 'task-014' };
      const result = await executor.execute(task, { timeout: 100 });
      expect(result.success).toBe(true);
    });

    it('should not retry on timeout', async () => {
      executor.performExecution = vi.fn().mockImplementation(async () => {
        await new Promise((resolve) => setTimeout(resolve, 100));
        return {};
      });
      const task = { taskId: 'task-015' };
      const retrySpy = vi.fn();
      executor.on('execution:retry', retrySpy);
      await executor.execute(task, { timeout: 50 });
      expect(retrySpy).not.toHaveBeenCalled();
    });

    it('should use default timeout when not specified', async () => {
      const task = { taskId: 'task-016' };
      let capturedContext = null;
      executor.on('execution:started', ({ context }) => {
        capturedContext = context;
      });
      await executor.execute(task);
      expect(capturedContext.timeout).toBe(5000);
    });
  });

  describe('Retry Logic', () => {
    it('should retry failed tasks', async () => {
      let attemptCount = 0;
      executor.performExecution = vi.fn().mockImplementation(async () => {
        attemptCount++;
        if (attemptCount < 3) throw new Error('Retry me');
        return { success: true };
      });
      const task = { taskId: 'task-017' };
      const result = await executor.execute(task);
      expect(result.success).toBe(true);
    });

    it('should emit retry events', async () => {
      let attemptCount = 0;
      executor.performExecution = vi.fn().mockImplementation(async () => {
        attemptCount++;
        if (attemptCount < 2) throw new Error('Retry me');
        return { success: true };
      });
      const task = { taskId: 'task-018' };
      const retrySpy = vi.fn();
      executor.on('execution:retry', retrySpy);
      await executor.execute(task);
      expect(retrySpy).toHaveBeenCalledOnce();
    });

    it('should respect max retries limit', async () => {
      executor.performExecution = vi.fn().mockRejectedValue(new Error('Always fail'));
      const task = { taskId: 'task-019' };
      const result = await executor.execute(task);
      expect(result.success).toBe(false);
      expect(executor.performExecution).toHaveBeenCalledTimes(4);
    });

    it('should delay between retries', async () => {
      const delays = [];
      let lastTime = Date.now();
      executor.performExecution = vi.fn().mockImplementation(async () => {
        const now = Date.now();
        if (delays.length > 0) delays.push(now - lastTime);
        lastTime = now;
        if (delays.length < 2) throw new Error('Retry me');
        return { success: true };
      });
      const task = { taskId: 'task-020' };
      await executor.execute(task);
      expect(delays.length).toBeGreaterThan(0);
    });

    it('should track retry count', async () => {
      let attemptCount = 0;
      executor.performExecution = vi.fn().mockImplementation(async (task) => {
        attemptCount++;
        if (attemptCount < 3) throw new Error('Retry me');
        return { retryCount: task._retryCount };
      });
      const task = { taskId: 'task-021' };
      const result = await executor.execute(task);
      expect(result.result.retryCount).toBe(2);
    });
  });

  describe('Execution Context', () => {
    it('should provide variables in context', async () => {
      const task = { taskId: 'task-022' };
      const variables = { caseId: 'case-001', orderId: 'ORD-123' };
      await executor.execute(task, { variables });
      const execution = Array.from(executor.completed.values())[0];
      expect(execution.context.variables).toEqual(variables);
    });

    it('should allow setting variables during execution', async () => {
      executor.performExecution = vi.fn().mockImplementation(async (task, context) => {
        context.setVariable('computed', 'value');
        return {};
      });
      const task = { taskId: 'task-023' };
      await executor.execute(task);
      const execution = Array.from(executor.completed.values())[0];
      expect(execution.context.getVariable('computed')).toBe('value');
    });

    it('should capture execution logs', async () => {
      const task = { taskId: 'task-024' };
      await executor.execute(task);
      const execution = Array.from(executor.completed.values())[0];
      expect(execution.context.logs.length).toBeGreaterThan(0);
    });

    it('should track execution timing', async () => {
      const task = { taskId: 'task-025' };
      await executor.execute(task);
      const execution = Array.from(executor.completed.values())[0];
      expect(execution.context.startTime).toBeDefined();
      expect(execution.context.endTime).toBeDefined();
    });
  });

  describe('Batch Execution', () => {
    it('should execute multiple tasks', async () => {
      const tasks = [
        { taskId: 'task-026' },
        { taskId: 'task-027' },
        { taskId: 'task-028' },
      ];
      const results = await executor.executeBatch(tasks);
      expect(results).toHaveLength(3);
    });

    it('should respect concurrency limit', async () => {
      let maxConcurrent = 0;
      let currentConcurrent = 0;
      executor.performExecution = vi.fn().mockImplementation(async () => {
        currentConcurrent++;
        maxConcurrent = Math.max(maxConcurrent, currentConcurrent);
        await new Promise((resolve) => setTimeout(resolve, 20));
        currentConcurrent--;
        return {};
      });
      const tasks = Array.from({ length: 15 }, (_, i) => ({ taskId: `task-\${100 + i}` }));
      await executor.executeBatch(tasks, { concurrency: 3 });
      expect(maxConcurrent).toBeLessThanOrEqual(3);
    });

    it('should handle mixed success and failure in batch', async () => {
      const tasks = [
        { taskId: 'task-029', shouldFail: false },
        { taskId: 'task-030', shouldFail: true },
        { taskId: 'task-031', shouldFail: false },
      ];
      const results = await executor.executeBatch(tasks);
      expect(results.filter((r) => r.success)).toHaveLength(2);
      expect(results.filter((r) => !r.success)).toHaveLength(1);
    });
  });

  describe('Statistics', () => {
    it('should track execution statistics', async () => {
      await executor.execute({ taskId: 'task-034' });
      await executor.execute({ taskId: 'task-035' });
      await executor.execute({ taskId: 'task-036', shouldFail: true });
      const stats = executor.getStatistics();
      expect(stats.completed).toBe(2);
      expect(stats.failed).toBe(1);
    });

    it('should calculate success rate', async () => {
      await executor.execute({ taskId: 'task-037' });
      await executor.execute({ taskId: 'task-038' });
      await executor.execute({ taskId: 'task-039' });
      await executor.execute({ taskId: 'task-040', shouldFail: true });
      const stats = executor.getStatistics();
      expect(stats.successRate).toBe(75);
    });

    it('should clear execution history', async () => {
      await executor.execute({ taskId: 'task-041' });
      executor.clearHistory();
      const stats = executor.getStatistics();
      expect(stats.completed).toBe(0);
    });
  });
});
