/**
 * @file YAWL WP13 - Multiple Instances with Design-Time Knowledge - Tests
 * @module @unrdf/yawl/test/multiple-instance/wp13
 *
 * @description
 * Comprehensive test suite for WP13 pattern implementation.
 * Tests synchronization barrier, multiple instance spawning, and error handling.
 */

import { describe, test, expect, beforeEach, vi } from 'vitest';
import {
  createSyncBarrier,
  SyncBarrier,
} from '../../src/multiple-instance/sync-barrier.mjs';
import {
  spawnInstancesDesignTime,
  createMultipleInstanceTask,
  estimateCompletionTime,
} from '../../src/multiple-instance/wp13-design-time.mjs';

// =============================================================================
// Synchronization Barrier Tests
// =============================================================================

describe('SyncBarrier - AND-join Synchronization', () => {
  test('should create barrier with correct count', () => {
    // Arrange & Act
    const barrier = createSyncBarrier(5);

    // Assert
    expect(barrier).toBeInstanceOf(SyncBarrier);
    expect(barrier.count).toBe(5);
    expect(barrier.timeout).toBe(30000); // default
  });

  test('should create barrier with custom options', () => {
    // Arrange & Act
    const barrier = createSyncBarrier(3, {
      timeout: 10000,
      cancelOnFailure: false,
      id: 'test-barrier',
    });

    // Assert
    expect(barrier.id).toBe('test-barrier');
    expect(barrier.count).toBe(3);
    expect(barrier.timeout).toBe(10000);
    expect(barrier.cancelOnFailure).toBe(false);
  });

  test('should wait for all instances to arrive', async () => {
    // Arrange
    const barrier = createSyncBarrier(3);
    const waitPromise = barrier.wait();

    // Act - Arrive in sequence
    await barrier.arrive({
      instanceId: 'i1',
      result: { value: 1 },
    });

    await barrier.arrive({
      instanceId: 'i2',
      result: { value: 2 },
    });

    await barrier.arrive({
      instanceId: 'i3',
      result: { value: 3 },
    });

    const result = await waitPromise;

    // Assert
    expect(result.success).toBe(true);
    expect(result.completedCount).toBe(3);
    expect(result.instances).toHaveLength(3);
    expect(result.instances.map(i => i.instanceId)).toEqual(['i1', 'i2', 'i3']);
    expect(result.timedOut).toBe(false);
    expect(result.cancelled).toBe(false);
  });

  test('should aggregate receipts from all instances', async () => {
    // Arrange
    const barrier = createSyncBarrier(2);
    const waitPromise = barrier.wait();

    // Act
    await barrier.arrive({
      instanceId: 'i1',
      result: { data: 'a' },
      receipt: { id: 'receipt-1', taskInstanceId: 'i1', action: 'complete', timestamp: '2024-01-01T00:00:00Z' },
    });

    await barrier.arrive({
      instanceId: 'i2',
      result: { data: 'b' },
      receipt: { id: 'receipt-2', taskInstanceId: 'i2', action: 'complete', timestamp: '2024-01-01T00:00:01Z' },
    });

    const result = await waitPromise;

    // Assert
    expect(result.receipts).toHaveLength(2);
    expect(result.receipts.map(r => r.id)).toEqual(['receipt-1', 'receipt-2']);
  });

  test('should release barrier after last instance completes', async () => {
    // Arrange
    const barrier = createSyncBarrier(4);
    const waitPromise = barrier.wait(); // Start waiting

    // Act - Complete instances one by one
    const state1 = barrier.getState();
    expect(state1.completed).toBe(false);

    await barrier.arrive({ instanceId: 'i1' });
    await barrier.arrive({ instanceId: 'i2' });
    await barrier.arrive({ instanceId: 'i3' });

    const state2 = barrier.getState();
    expect(state2.completed).toBe(false);
    expect(state2.arrivals).toBe(3);

    // Last arrival triggers completion
    await barrier.arrive({ instanceId: 'i4' });

    const state3 = barrier.getState();
    expect(state3.completed).toBe(true);
    expect(state3.arrivals).toBe(4);

    // Assert
    const result = await waitPromise;
    expect(result.success).toBe(true);
  });

  test('should timeout and cancel all instances', async () => {
    // Arrange
    const barrier = createSyncBarrier(3, { timeout: 100 });
    const cancelHandler = vi.fn();
    barrier.onCancel(cancelHandler);

    // Act - Only 2 instances arrive, wait for timeout
    await barrier.arrive({ instanceId: 'i1' });
    await barrier.arrive({ instanceId: 'i2' });

    const result = await barrier.wait();

    // Assert
    expect(result.success).toBe(false);
    expect(result.timedOut).toBe(true);
    expect(result.cancelled).toBe(true);
    expect(result.completedCount).toBe(2);
    expect(cancelHandler).toHaveBeenCalledWith('Barrier timeout exceeded');
  });

  test('should cancel all on first failure when cancelOnFailure=true', async () => {
    // Arrange
    const barrier = createSyncBarrier(3, { cancelOnFailure: true });
    const cancelHandler = vi.fn();
    barrier.onCancel(cancelHandler);
    const waitPromise = barrier.wait(); // Start waiting

    // Act - One instance fails
    await barrier.arrive({
      instanceId: 'i1',
      failed: true,
      error: 'Task execution failed',
    });

    const result = await waitPromise;

    // Assert
    expect(result.success).toBe(false);
    expect(result.cancelled).toBe(true);
    expect(result.failures).toContain('Task execution failed');
    expect(cancelHandler).toHaveBeenCalledWith('Cancelling all instances due to failure');
  });

  test('should continue on failure when cancelOnFailure=false', async () => {
    // Arrange
    const barrier = createSyncBarrier(3, { cancelOnFailure: false });
    const waitPromise = barrier.wait(); // Start waiting

    // Act - One instance fails, others succeed
    await barrier.arrive({
      instanceId: 'i1',
      failed: true,
      error: 'Instance 1 failed',
    });

    await barrier.arrive({
      instanceId: 'i2',
      result: { value: 2 },
    });

    await barrier.arrive({
      instanceId: 'i3',
      result: { value: 3 },
    });

    const result = await waitPromise;

    // Assert
    expect(result.success).toBe(false); // Has failures
    expect(result.cancelled).toBe(false);
    expect(result.completedCount).toBe(3);
    expect(result.failures).toHaveLength(1);
    expect(result.failures[0]).toBe('Instance 1 failed');
  });

  test('should reject arrival after completion', async () => {
    // Arrange
    const barrier = createSyncBarrier(2);
    const waitPromise = barrier.wait(); // Start waiting

    // Act
    await barrier.arrive({ instanceId: 'i1' });
    await barrier.arrive({ instanceId: 'i2' });
    await waitPromise;

    // Assert - Try to arrive after completion
    await expect(
      barrier.arrive({ instanceId: 'i3' })
    ).rejects.toThrow('Barrier');
  });

  test('should track barrier state correctly', async () => {
    // Arrange
    const barrier = createSyncBarrier(3);
    const waitPromise = barrier.wait(); // Start waiting

    // Act & Assert - Initial state
    let state = barrier.getState();
    expect(state.arrivals).toBe(0);
    expect(state.completed).toBe(false);

    // After 1 arrival
    await barrier.arrive({ instanceId: 'i1' });
    state = barrier.getState();
    expect(state.arrivals).toBe(1);

    // After 2 arrivals
    await barrier.arrive({ instanceId: 'i2' });
    state = barrier.getState();
    expect(state.arrivals).toBe(2);

    // After 3 arrivals (completion)
    await barrier.arrive({ instanceId: 'i3' });
    state = barrier.getState();
    expect(state.arrivals).toBe(3);
    expect(state.completed).toBe(true);

    await waitPromise; // Clean up
  });
});

// =============================================================================
// WP13 - Multiple Instances with Design-Time Knowledge Tests
// =============================================================================

describe('WP13 - Multiple Instances with Design-Time Knowledge', () => {
  test('should spawn N instances with known count', async () => {
    // Arrange
    const task = createMultipleInstanceTask({
      id: 'test-task',
      name: 'Test Task',
      execute: async (data) => {
        return { processed: data.value * 2 };
      },
    });

    // Act
    const result = await spawnInstancesDesignTime(task, 3, { value: 10 });

    // Assert
    expect(result.success).toBe(true);
    expect(result.count).toBe(3);
    expect(result.instances).toHaveLength(3);
    expect(result.instances.every(i => i.success)).toBe(true);
    expect(result.receipts).toHaveLength(3);
  });

  test('should synchronize on completion of all instances', async () => {
    // Arrange
    let completionOrder = [];
    const task = createMultipleInstanceTask({
      id: 'async-task',
      execute: async (data) => {
        await new Promise(resolve => setTimeout(resolve, data.delay));
        completionOrder.push(data.index);
        return { completed: data.index };
      },
    });

    // Act
    const result = await spawnInstancesDesignTime(
      task,
      3,
      {},
      null,
      {
        transformInput: (index) => ({ index, delay: (3 - index) * 10 }),
      }
    );

    // Assert
    expect(result.success).toBe(true);
    expect(result.instances).toHaveLength(3);
    // All instances should complete (regardless of order)
    expect(completionOrder).toHaveLength(3);
  });

  test('should aggregate results from all instances', async () => {
    // Arrange
    const task = createMultipleInstanceTask({
      id: 'sum-task',
      execute: async (data) => {
        return { sum: data.a + data.b };
      },
    });

    // Act
    const result = await spawnInstancesDesignTime(
      task,
      4,
      { a: 5, b: 3 }
    );

    // Assert
    expect(result.success).toBe(true);
    expect(result.instances.every(i => i.outputData.sum === 8)).toBe(true);
    expect(result.instances.map(i => i.index)).toEqual([0, 1, 2, 3]);
  });

  test('should transform input data per instance', async () => {
    // Arrange
    const task = createMultipleInstanceTask({
      id: 'transform-task',
      execute: async (data) => {
        return { squared: data.value * data.value };
      },
    });

    // Act
    const result = await spawnInstancesDesignTime(
      task,
      5,
      {},
      null,
      {
        transformInput: (index) => ({ value: index + 1 }),
      }
    );

    // Assert
    expect(result.success).toBe(true);
    expect(result.instances.map(i => i.outputData.squared)).toEqual([1, 4, 9, 16, 25]);
  });

  test('should cancel all instances on timeout', async () => {
    // Arrange
    const task = createMultipleInstanceTask({
      id: 'slow-task',
      execute: async (data) => {
        await new Promise(resolve => setTimeout(resolve, 200));
        return { completed: true };
      },
    });

    // Act
    const result = await spawnInstancesDesignTime(
      task,
      3,
      {},
      null,
      { timeout: 50 }
    );

    // Assert
    expect(result.success).toBe(false);
    expect(result.timedOut).toBe(true);
  });

  test('should cancel all on first failure when configured', async () => {
    // Arrange
    let executionCount = 0;
    const task = createMultipleInstanceTask({
      id: 'failing-task',
      execute: async (data) => {
        executionCount++;
        if (data._instanceIndex === 1) {
          throw new Error('Instance 1 failed');
        }
        await new Promise(resolve => setTimeout(resolve, 100));
        return { completed: true };
      },
    });

    // Act
    const result = await spawnInstancesDesignTime(
      task,
      3,
      {},
      null,
      { cancelOnFailure: true, timeout: 5000 }
    );

    // Assert
    expect(result.success).toBe(false);
    expect(result.failures.length).toBeGreaterThan(0);
  });

  test('should continue on failure when cancelOnFailure=false', async () => {
    // Arrange
    const task = createMultipleInstanceTask({
      id: 'partial-fail-task',
      execute: async (data) => {
        if (data._instanceIndex === 1) {
          throw new Error('Instance 1 failed');
        }
        return { value: data._instanceIndex };
      },
    });

    // Act
    const result = await spawnInstancesDesignTime(
      task,
      3,
      {},
      null,
      { cancelOnFailure: false }
    );

    // Assert
    expect(result.success).toBe(false); // Has failures
    expect(result.instances).toHaveLength(3);
    expect(result.instances[0].success).toBe(true);
    expect(result.instances[1].success).toBe(false);
    expect(result.instances[2].success).toBe(true);
  });

  test('should generate receipts for each instance', async () => {
    // Arrange
    const task = createMultipleInstanceTask({
      id: 'receipt-task',
      execute: async (data) => {
        return { processed: true };
      },
    });

    // Act
    const result = await spawnInstancesDesignTime(task, 5, { data: 'test' });

    // Assert
    expect(result.receipts).toHaveLength(5);
    result.receipts.forEach((receipt, index) => {
      expect(receipt.id).toBeDefined();
      expect(receipt.taskInstanceId).toContain('receipt-task-instance-');
      expect(receipt.action).toBe('complete');
    });
  });

  test('should use custom sync barrier if provided', async () => {
    // Arrange
    const customBarrier = createSyncBarrier(2, { id: 'custom-barrier' });
    const task = createMultipleInstanceTask({
      id: 'barrier-task',
      execute: async (data) => {
        return { value: data._instanceIndex };
      },
    });

    // Act
    const result = await spawnInstancesDesignTime(
      task,
      2,
      {},
      customBarrier
    );

    // Assert
    expect(result.success).toBe(true);
    expect(result.barrier.id).toBe('custom-barrier');
    expect(result.barrier.count).toBe(2);
    expect(result.barrier.arrivals).toBe(2);
    expect(result.barrier.completed).toBe(true);
  });

  test('should track execution time', async () => {
    // Arrange
    const task = createMultipleInstanceTask({
      id: 'timed-task',
      execute: async (data) => {
        await new Promise(resolve => setTimeout(resolve, 50));
        return { done: true };
      },
    });

    // Act
    const result = await spawnInstancesDesignTime(task, 3, {});

    // Assert
    expect(result.executionTime).toBeGreaterThanOrEqual(50);
    expect(result.executionTime).toBeLessThan(500);
  });

  test('should validate task definition', async () => {
    // Arrange - Invalid task (no execute function)
    const invalidTask = {
      id: 'invalid-task',
    };

    // Act & Assert
    await expect(
      spawnInstancesDesignTime(invalidTask, 3, {})
    ).rejects.toThrow('must have execute function');
  });

  test('should validate count parameter', async () => {
    // Arrange
    const task = createMultipleInstanceTask({
      id: 'test-task',
      execute: async (data) => ({ value: 1 }),
    });

    // Act & Assert - Zero count
    await expect(
      spawnInstancesDesignTime(task, 0, {})
    ).rejects.toThrow('Count must be positive integer');

    // Negative count
    await expect(
      spawnInstancesDesignTime(task, -5, {})
    ).rejects.toThrow('Count must be positive integer');

    // Non-integer count
    await expect(
      spawnInstancesDesignTime(task, 3.5, {})
    ).rejects.toThrow('Count must be positive integer');
  });

  test('should handle large number of instances', async () => {
    // Arrange
    const task = createMultipleInstanceTask({
      id: 'bulk-task',
      execute: async (data) => {
        return { index: data._instanceIndex };
      },
    });

    // Act
    const result = await spawnInstancesDesignTime(task, 50, {});

    // Assert
    expect(result.success).toBe(true);
    expect(result.instances).toHaveLength(50);
    expect(result.instances.every(i => i.success)).toBe(true);
  });
});

// =============================================================================
// Utility Function Tests
// =============================================================================

describe('Multiple Instance Utilities', () => {
  test('should create multiple instance task with config', () => {
    // Arrange & Act
    const task = createMultipleInstanceTask({
      id: 'util-task',
      name: 'Utility Task',
      execute: async (data) => ({ result: data }),
    });

    // Assert
    expect(task.id).toBe('util-task');
    expect(task.name).toBe('Utility Task');
    expect(task.execute).toBeInstanceOf(Function);
  });

  test('should estimate completion time correctly', () => {
    // Arrange & Act
    const time1 = estimateCompletionTime(10, 500);
    const time2 = estimateCompletionTime(100, 1000);

    // Assert
    expect(time1).toBe(500); // Parallel execution
    expect(time2).toBe(1000);
  });

  test('should validate task configuration', () => {
    // Arrange & Act & Assert - Invalid ID
    expect(() => {
      createMultipleInstanceTask({
        id: '',
        execute: async () => {},
      });
    }).toThrow();

    // Missing execute function is allowed (will fail at spawn time)
    const taskWithoutExecute = createMultipleInstanceTask({
      id: 'no-exec',
    });
    expect(taskWithoutExecute.id).toBe('no-exec');
  });
});

// =============================================================================
// Integration Tests
// =============================================================================

describe('WP13 Integration - Real-World Scenarios', () => {
  test('should process batch of emails (3 recipients)', async () => {
    // Arrange
    const sentEmails = [];
    const emailTask = createMultipleInstanceTask({
      id: 'send-email',
      execute: async (data) => {
        sentEmails.push(data.recipient);
        return { sent: true, recipient: data.recipient };
      },
    });

    const recipients = ['alice@example.com', 'bob@example.com', 'carol@example.com'];

    // Act
    const result = await spawnInstancesDesignTime(
      emailTask,
      3,
      {},
      null,
      {
        transformInput: (index) => ({ recipient: recipients[index] }),
      }
    );

    // Assert
    expect(result.success).toBe(true);
    expect(sentEmails).toEqual(recipients);
    expect(result.instances.every(i => i.outputData.sent)).toBe(true);
  });

  test('should process parallel API calls (5 endpoints)', async () => {
    // Arrange
    const apiResults = [];
    const apiTask = createMultipleInstanceTask({
      id: 'fetch-data',
      execute: async (data) => {
        const response = { endpoint: data.url, status: 200 };
        apiResults.push(response);
        return response;
      },
    });

    const endpoints = ['/api/users', '/api/posts', '/api/comments', '/api/likes', '/api/shares'];

    // Act
    const result = await spawnInstancesDesignTime(
      apiTask,
      5,
      {},
      null,
      {
        transformInput: (index) => ({ url: endpoints[index] }),
      }
    );

    // Assert
    expect(result.success).toBe(true);
    expect(apiResults).toHaveLength(5);
    expect(result.instances.every(i => i.outputData.status === 200)).toBe(true);
  });

  test('should handle data validation across instances', async () => {
    // Arrange
    const validationTask = createMultipleInstanceTask({
      id: 'validate-data',
      execute: async (data) => {
        if (data.value < 0) {
          throw new Error('Negative value not allowed');
        }
        return { valid: true, value: data.value };
      },
    });

    // Act - All valid
    const result1 = await spawnInstancesDesignTime(
      validationTask,
      3,
      {},
      null,
      {
        transformInput: (index) => ({ value: index + 1 }),
        cancelOnFailure: false,
      }
    );

    // Act - One invalid
    const result2 = await spawnInstancesDesignTime(
      validationTask,
      3,
      {},
      null,
      {
        transformInput: (index) => ({ value: index - 1 }), // index 0 => value -1
        cancelOnFailure: false,
      }
    );

    // Assert
    expect(result1.success).toBe(true);
    expect(result2.success).toBe(false);
    expect(result2.instances[0].success).toBe(false);
    expect(result2.instances[1].success).toBe(true);
    expect(result2.instances[2].success).toBe(true);
  });
});
