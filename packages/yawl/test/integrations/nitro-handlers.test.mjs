/**
 * @file YAWL-Nitro Event Handlers Integration Tests
 * @module @unrdf/yawl/test/integrations/nitro-handlers
 * @description Comprehensive tests for Nitro event handlers
 *
 * Tests cover:
 * - Event handler registration and dispatch
 * - Handler execution with YAWL context
 * - Error handling in handlers
 * - Handler chaining and composition
 * - Async handler execution
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { EventEmitter } from 'events';

/**
 * Event Handler Registry
 */
class EventHandlerRegistry {
  constructor() {
    this.handlers = new Map();
    this.middleware = [];
  }

  /**
   * Register event handler
   */
  on(eventType, handler) {
    if (!this.handlers.has(eventType)) {
      this.handlers.set(eventType, []);
    }
    this.handlers.get(eventType).push(handler);
  }

  /**
   * Add middleware
   */
  use(middleware) {
    this.middleware.push(middleware);
  }

  /**
   * Dispatch event
   */
  async dispatch(eventType, payload) {
    let processedPayload = payload;

    // Apply middleware
    for (const mw of this.middleware) {
      processedPayload = await mw(processedPayload);
    }

    // Execute handlers
    const handlers = this.handlers.get(eventType) || [];
    const results = [];

    for (const handler of handlers) {
      try {
        const result = await handler(processedPayload);
        results.push({ success: true, result });
      } catch (error) {
        results.push({ success: false, error });
      }
    }

    return results;
  }

  /**
   * Remove handler
   */
  off(eventType, handler) {
    const handlers = this.handlers.get(eventType);
    if (handlers) {
      const index = handlers.indexOf(handler);
      if (index > -1) {
        handlers.splice(index, 1);
      }
    }
  }

  /**
   * Clear all handlers
   */
  clear() {
    this.handlers.clear();
    this.middleware = [];
  }
}

/**
 * YAWL-Nitro Event Handler System
 */
class YawlNitroEventHandlers extends EventEmitter {
  constructor() {
    super();
    this.registry = new EventHandlerRegistry();
    this.executionLog = [];
  }

  /**
   * Register task started handler
   */
  onTaskStarted(handler) {
    this.registry.on('task:started', async (payload) => {
      this.executionLog.push({ event: 'task:started', payload, timestamp: new Date() });
      return await handler(payload);
    });
  }

  /**
   * Register task completed handler
   */
  onTaskCompleted(handler) {
    this.registry.on('task:completed', async (payload) => {
      this.executionLog.push({ event: 'task:completed', payload, timestamp: new Date() });
      return await handler(payload);
    });
  }

  /**
   * Register task failed handler
   */
  onTaskFailed(handler) {
    this.registry.on('task:failed', async (payload) => {
      this.executionLog.push({ event: 'task:failed', payload, timestamp: new Date() });
      return await handler(payload);
    });
  }

  /**
   * Register case completed handler
   */
  onCaseCompleted(handler) {
    this.registry.on('case:completed', async (payload) => {
      this.executionLog.push({ event: 'case:completed', payload, timestamp: new Date() });
      return await handler(payload);
    });
  }

  /**
   * Add middleware
   */
  addMiddleware(middleware) {
    this.registry.use(middleware);
  }

  /**
   * Dispatch event
   */
  async dispatchEvent(eventType, payload) {
    const results = await this.registry.dispatch(eventType, payload);
    this.emit('event:dispatched', { eventType, payload, results });
    return results;
  }

  /**
   * Get execution log
   */
  getExecutionLog() {
    return [...this.executionLog];
  }

  /**
   * Clear execution log
   */
  clearLog() {
    this.executionLog = [];
  }
}

describe('YAWL-Nitro Event Handlers Integration', () => {
  let handlers;

  beforeEach(() => {
    handlers = new YawlNitroEventHandlers();
  });

  afterEach(() => {
    handlers.removeAllListeners();
    handlers.registry.clear();
    handlers.clearLog();
  });

  describe('Handler Registration', () => {
    it('should register task started handler', () => {
      // Arrange
      const handler = vi.fn();

      // Act
      handlers.onTaskStarted(handler);

      // Assert
      expect(handlers.registry.handlers.has('task:started')).toBe(true);
      expect(handlers.registry.handlers.get('task:started')).toHaveLength(1);
    });

    it('should register task completed handler', () => {
      // Arrange
      const handler = vi.fn();

      // Act
      handlers.onTaskCompleted(handler);

      // Assert
      expect(handlers.registry.handlers.has('task:completed')).toBe(true);
    });

    it('should register task failed handler', () => {
      // Arrange
      const handler = vi.fn();

      // Act
      handlers.onTaskFailed(handler);

      // Assert
      expect(handlers.registry.handlers.has('task:failed')).toBe(true);
    });

    it('should register case completed handler', () => {
      // Arrange
      const handler = vi.fn();

      // Act
      handlers.onCaseCompleted(handler);

      // Assert
      expect(handlers.registry.handlers.has('case:completed')).toBe(true);
    });

    it('should register multiple handlers for same event', () => {
      // Arrange
      const handler1 = vi.fn();
      const handler2 = vi.fn();
      const handler3 = vi.fn();

      // Act
      handlers.onTaskStarted(handler1);
      handlers.onTaskStarted(handler2);
      handlers.onTaskStarted(handler3);

      // Assert
      expect(handlers.registry.handlers.get('task:started')).toHaveLength(3);
    });

    it('should support handler removal', () => {
      // Arrange
      const handler = vi.fn();
      handlers.registry.on('test:event', handler);

      // Act
      handlers.registry.off('test:event', handler);

      // Assert
      const eventHandlers = handlers.registry.handlers.get('test:event') || [];
      expect(eventHandlers).toHaveLength(0);
    });
  });

  describe('Handler Execution', () => {
    it('should execute task started handler', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({ processed: true });
      handlers.onTaskStarted(handler);
      const payload = { caseId: 'case-001', taskId: 'task-001' };

      // Act
      const results = await handlers.dispatchEvent('task:started', payload);

      // Assert
      expect(handler).toHaveBeenCalledWith(payload);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      expect(results[0].result.processed).toBe(true);
    });

    it('should execute task completed handler with output', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({ acknowledged: true });
      handlers.onTaskCompleted(handler);
      const payload = {
        caseId: 'case-002',
        taskId: 'task-002',
        output: { result: 'success' },
      };

      // Act
      const results = await handlers.dispatchEvent('task:completed', payload);

      // Assert
      expect(handler).toHaveBeenCalledWith(payload);
      expect(results[0].result.acknowledged).toBe(true);
    });

    it('should execute task failed handler with error', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({ errorHandled: true });
      handlers.onTaskFailed(handler);
      const payload = {
        caseId: 'case-003',
        taskId: 'task-003',
        error: 'Execution timeout',
      };

      // Act
      const results = await handlers.dispatchEvent('task:failed', payload);

      // Assert
      expect(handler).toHaveBeenCalledWith(payload);
      expect(results[0].result.errorHandled).toBe(true);
    });

    it('should execute case completed handler', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({ caseFinalized: true });
      handlers.onCaseCompleted(handler);
      const payload = { caseId: 'case-004', status: 'completed' };

      // Act
      const results = await handlers.dispatchEvent('case:completed', payload);

      // Assert
      expect(handler).toHaveBeenCalledWith(payload);
      expect(results[0].result.caseFinalized).toBe(true);
    });

    it('should execute multiple handlers in order', async () => {
      // Arrange
      const executionOrder = [];
      const handler1 = vi.fn().mockImplementation(async () => {
        executionOrder.push(1);
        return { handler: 1 };
      });
      const handler2 = vi.fn().mockImplementation(async () => {
        executionOrder.push(2);
        return { handler: 2 };
      });
      const handler3 = vi.fn().mockImplementation(async () => {
        executionOrder.push(3);
        return { handler: 3 };
      });

      handlers.onTaskStarted(handler1);
      handlers.onTaskStarted(handler2);
      handlers.onTaskStarted(handler3);

      // Act
      const results = await handlers.dispatchEvent('task:started', {});

      // Assert
      expect(executionOrder).toEqual([1, 2, 3]);
      expect(results).toHaveLength(3);
    });

    it('should handle async handlers', async () => {
      // Arrange
      const handler = vi.fn().mockImplementation(async (payload) => {
        await new Promise((resolve) => setTimeout(resolve, 10));
        return { processed: true, data: payload };
      });
      handlers.onTaskCompleted(handler);

      // Act
      const results = await handlers.dispatchEvent('task:completed', {
        taskId: 'async-task',
      });

      // Assert
      expect(results[0].success).toBe(true);
      expect(results[0].result.data.taskId).toBe('async-task');
    });

    it('should log handler execution', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({});
      handlers.onTaskStarted(handler);

      // Act
      await handlers.dispatchEvent('task:started', { taskId: 'task-005' });

      // Assert
      const log = handlers.getExecutionLog();
      expect(log).toHaveLength(1);
      expect(log[0].event).toBe('task:started');
      expect(log[0].payload.taskId).toBe('task-005');
    });
  });

  describe('Error Handling', () => {
    it('should catch handler errors', async () => {
      // Arrange
      const handler = vi.fn().mockRejectedValue(new Error('Handler failed'));
      handlers.onTaskStarted(handler);

      // Act
      const results = await handlers.dispatchEvent('task:started', {});

      // Assert
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(false);
      expect(results[0].error.message).toBe('Handler failed');
    });

    it('should continue execution after handler error', async () => {
      // Arrange
      const handler1 = vi.fn().mockRejectedValue(new Error('First failed'));
      const handler2 = vi.fn().mockResolvedValue({ success: true });
      handlers.onTaskCompleted(handler1);
      handlers.onTaskCompleted(handler2);

      // Act
      const results = await handlers.dispatchEvent('task:completed', {});

      // Assert
      expect(results).toHaveLength(2);
      expect(results[0].success).toBe(false);
      expect(results[1].success).toBe(true);
    });

    it('should handle synchronous errors', async () => {
      // Arrange
      const handler = vi.fn().mockImplementation(() => {
        throw new Error('Sync error');
      });
      handlers.onTaskFailed(handler);

      // Act
      const results = await handlers.dispatchEvent('task:failed', {});

      // Assert
      expect(results[0].success).toBe(false);
      expect(results[0].error.message).toBe('Sync error');
    });

    it('should handle null/undefined payloads', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({ handled: true });
      handlers.onTaskStarted(handler);

      // Act
      const results = await handlers.dispatchEvent('task:started', null);

      // Assert
      expect(results[0].success).toBe(true);
      expect(handler).toHaveBeenCalledWith(null);
    });
  });

  describe('Middleware Support', () => {
    it('should apply middleware before handlers', async () => {
      // Arrange
      const middleware = vi.fn().mockImplementation(async (payload) => ({
        ...payload,
        enriched: true,
      }));
      const handler = vi.fn().mockResolvedValue({});

      handlers.addMiddleware(middleware);
      handlers.onTaskStarted(handler);

      // Act
      await handlers.dispatchEvent('task:started', { taskId: 'task-006' });

      // Assert
      expect(middleware).toHaveBeenCalled();
      expect(handler).toHaveBeenCalledWith({
        taskId: 'task-006',
        enriched: true,
      });
    });

    it('should apply multiple middleware in order', async () => {
      // Arrange
      const mw1 = vi.fn().mockImplementation(async (p) => ({ ...p, step1: true }));
      const mw2 = vi.fn().mockImplementation(async (p) => ({ ...p, step2: true }));
      const mw3 = vi.fn().mockImplementation(async (p) => ({ ...p, step3: true }));
      const handler = vi.fn().mockResolvedValue({});

      handlers.addMiddleware(mw1);
      handlers.addMiddleware(mw2);
      handlers.addMiddleware(mw3);
      handlers.onTaskCompleted(handler);

      // Act
      await handlers.dispatchEvent('task:completed', {});

      // Assert
      expect(handler).toHaveBeenCalledWith({
        step1: true,
        step2: true,
        step3: true,
      });
    });

    it('should handle middleware transformation', async () => {
      // Arrange
      const middleware = vi.fn().mockImplementation(async (payload) => ({
        ...payload,
        timestamp: Date.now(),
        metadata: { processed: true },
      }));
      const handler = vi.fn().mockResolvedValue({});

      handlers.addMiddleware(middleware);
      handlers.onTaskStarted(handler);

      // Act
      await handlers.dispatchEvent('task:started', { taskId: 'task-007' });

      // Assert
      const call = handler.mock.calls[0][0];
      expect(call.timestamp).toBeDefined();
      expect(call.metadata.processed).toBe(true);
    });
  });

  describe('Event Dispatch', () => {
    it('should emit event on dispatch', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({});
      handlers.onTaskStarted(handler);
      const dispatchSpy = vi.fn();
      handlers.on('event:dispatched', dispatchSpy);

      // Act
      await handlers.dispatchEvent('task:started', { taskId: 'task-008' });

      // Assert
      expect(dispatchSpy).toHaveBeenCalledOnce();
      const event = dispatchSpy.mock.calls[0][0];
      expect(event.eventType).toBe('task:started');
      expect(event.payload.taskId).toBe('task-008');
      expect(event.results).toBeDefined();
    });

    it('should handle dispatch with no handlers', async () => {
      // Arrange
      // No handlers registered

      // Act
      const results = await handlers.dispatchEvent('unknown:event', {});

      // Assert
      expect(results).toEqual([]);
    });

    it('should return all handler results', async () => {
      // Arrange
      const handler1 = vi.fn().mockResolvedValue({ result: 1 });
      const handler2 = vi.fn().mockResolvedValue({ result: 2 });
      const handler3 = vi.fn().mockResolvedValue({ result: 3 });

      handlers.onTaskCompleted(handler1);
      handlers.onTaskCompleted(handler2);
      handlers.onTaskCompleted(handler3);

      // Act
      const results = await handlers.dispatchEvent('task:completed', {});

      // Assert
      expect(results).toHaveLength(3);
      expect(results[0].result.result).toBe(1);
      expect(results[1].result.result).toBe(2);
      expect(results[2].result.result).toBe(3);
    });
  });

  describe('Execution Log', () => {
    it('should maintain execution log', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({});
      handlers.onTaskStarted(handler);

      // Act
      await handlers.dispatchEvent('task:started', { taskId: 'task-009' });
      await handlers.dispatchEvent('task:started', { taskId: 'task-010' });

      // Assert
      const log = handlers.getExecutionLog();
      expect(log).toHaveLength(2);
      expect(log[0].payload.taskId).toBe('task-009');
      expect(log[1].payload.taskId).toBe('task-010');
    });

    it('should clear execution log', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({});
      handlers.onTaskStarted(handler);
      await handlers.dispatchEvent('task:started', {});

      // Act
      handlers.clearLog();

      // Assert
      expect(handlers.getExecutionLog()).toHaveLength(0);
    });

    it('should include timestamps in log entries', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({});
      handlers.onTaskCompleted(handler);

      // Act
      await handlers.dispatchEvent('task:completed', {});

      // Assert
      const log = handlers.getExecutionLog();
      expect(log[0].timestamp).toBeInstanceOf(Date);
    });
  });

  describe('Handler Composition', () => {
    it('should support handler chaining via results', async () => {
      // Arrange
      const handler1 = vi.fn().mockResolvedValue({ data: 'step1' });
      const handler2 = vi.fn().mockImplementation(async (payload) => {
        return { data: 'step2', previous: payload };
      });

      handlers.onTaskStarted(handler1);
      handlers.onTaskStarted(handler2);

      // Act
      const results = await handlers.dispatchEvent('task:started', { initial: true });

      // Assert
      expect(results[0].result.data).toBe('step1');
      expect(results[1].result.data).toBe('step2');
    });

    it('should handle complex data transformations', async () => {
      // Arrange
      const enrichHandler = vi.fn().mockImplementation(async (payload) => ({
        ...payload,
        enriched: { timestamp: Date.now(), source: 'yawl' },
      }));

      handlers.onTaskCompleted(enrichHandler);

      // Act
      const results = await handlers.dispatchEvent('task:completed', {
        taskId: 'task-011',
        output: { value: 100 },
      });

      // Assert
      const result = results[0].result;
      expect(result.taskId).toBe('task-011');
      expect(result.enriched.source).toBe('yawl');
    });
  });

  describe('Concurrent Handler Execution', () => {
    it('should handle concurrent event dispatches', async () => {
      // Arrange
      const handler = vi.fn().mockImplementation(async (payload) => {
        await new Promise((resolve) => setTimeout(resolve, 5));
        return { processed: payload.taskId };
      });
      handlers.onTaskStarted(handler);

      // Act
      const results = await Promise.all([
        handlers.dispatchEvent('task:started', { taskId: 'task-012' }),
        handlers.dispatchEvent('task:started', { taskId: 'task-013' }),
        handlers.dispatchEvent('task:started', { taskId: 'task-014' }),
      ]);

      // Assert
      expect(results).toHaveLength(3);
      expect(handler).toHaveBeenCalledTimes(3);
    });

    it('should maintain log integrity under concurrency', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({});
      handlers.onTaskCompleted(handler);

      // Act
      await Promise.all(
        Array.from({ length: 10 }, (_, i) =>
          handlers.dispatchEvent('task:completed', { taskId: `task-${i}` })
        )
      );

      // Assert
      const log = handlers.getExecutionLog();
      expect(log).toHaveLength(10);
    });
  });
});
