/**
 * @fileoverview Parallel Orchestration Tests
 *
 * Tests for parallel agent execution system.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import {
  WorkerPool,
  WorkerStatus,
  createWorkerPool
} from './worker-pool.mjs';
import {
  TaskQueue,
  TaskPriority,
  TaskStatus,
  createTaskQueue
} from './task-queue.mjs';
import {
  AgentRouter,
  AgentRole,
  RoutingStrategy,
  createAgentRouter
} from './agent-router.mjs';
import {
  CircuitBreaker,
  CircuitState,
  createCircuitBreaker,
  withCircuitBreaker
} from './circuit-breaker.mjs';
import {
  ParallelOrchestrator,
  createParallelOrchestrator
} from './parallel-orchestrator.mjs';

// ============================================================================
// WorkerPool Tests
// ============================================================================

describe('WorkerPool', () => {
  let pool;

  beforeEach(async () => {
    pool = createWorkerPool({
      minWorkers: 2,
      maxWorkers: 5,
      idleTimeout: 1000
    });
    await pool.start();
  });

  afterEach(async () => {
    await pool.shutdown({ graceful: false });
  });

  describe('start', () => {
    it('should create minimum workers on start', () => {
      expect(pool.workers.size).toBe(2);
      expect(pool.running).toBe(true);
    });
  });

  describe('acquireWorker', () => {
    it('should acquire idle worker', async () => {
      const worker = await pool.acquireWorker();
      expect(worker).toBeDefined();
      expect(worker.status).toBe(WorkerStatus.BUSY);
    });

    it('should scale up if no idle workers', async () => {
      const worker1 = await pool.acquireWorker();
      const worker2 = await pool.acquireWorker();
      const worker3 = await pool.acquireWorker(); // Should create new worker

      expect(pool.workers.size).toBe(3);
      expect(worker3.status).toBe(WorkerStatus.BUSY);
    });

    it('should respect max workers limit', async () => {
      // Acquire all workers
      const workers = [];
      for (let i = 0; i < 5; i++) {
        workers.push(await pool.acquireWorker());
      }

      // Next acquisition should timeout
      await expect(
        pool.acquireWorker([], 100)
      ).rejects.toThrow('Failed to acquire worker');
    });

    it('should match capabilities', async () => {
      // Create pool with custom worker factory
      const customPool = createWorkerPool({
        minWorkers: 1,
        maxWorkers: 3,
        workerFactory: async (worker) => ({
          ...worker,
          capabilities: ['special']
        })
      });
      await customPool.start();

      const worker = await customPool.acquireWorker(['special']);
      expect(worker.capabilities).toContain('special');

      await customPool.shutdown({ graceful: false });
    });
  });

  describe('releaseWorker', () => {
    it('should release worker back to pool', async () => {
      const worker = await pool.acquireWorker();
      pool.releaseWorker(worker.id, { success: true });

      expect(pool.workers.get(worker.id).status).toBe(WorkerStatus.IDLE);
      expect(pool.workers.get(worker.id).tasksCompleted).toBe(1);
    });

    it('should track task failures', async () => {
      const worker = await pool.acquireWorker();
      pool.releaseWorker(worker.id, { success: false });

      expect(pool.workers.get(worker.id).tasksFailed).toBe(1);
    });
  });

  describe('getStats', () => {
    it('should return pool statistics', async () => {
      const worker = await pool.acquireWorker();
      pool.releaseWorker(worker.id, { success: true });

      const stats = pool.getStats();
      expect(stats.currentWorkers).toBeGreaterThan(0);
      expect(stats.totalTasksCompleted).toBe(1);
    });
  });
});

// ============================================================================
// TaskQueue Tests
// ============================================================================

describe('TaskQueue', () => {
  let queue;

  beforeEach(() => {
    queue = createTaskQueue({
      maxSize: 100,
      defaultPriority: TaskPriority.NORMAL
    });
  });

  describe('enqueue', () => {
    it('should enqueue task', () => {
      const taskId = queue.enqueue({
        type: 'test-task',
        payload: { data: 'test' }
      });

      expect(taskId).toBeDefined();
      expect(queue.size()).toBe(1);
    });

    it('should respect priority', () => {
      queue.enqueue({
        type: 'low',
        priority: TaskPriority.LOW,
        payload: {}
      });

      queue.enqueue({
        type: 'high',
        priority: TaskPriority.HIGH,
        payload: {}
      });

      queue.enqueue({
        type: 'critical',
        priority: TaskPriority.CRITICAL,
        payload: {}
      });

      // Dequeue should return critical first
      const task = queue.dequeue();
      expect(task.type).toBe('critical');
    });

    it('should throw when queue is full', () => {
      const smallQueue = createTaskQueue({ maxSize: 2 });

      smallQueue.enqueue({ type: 'task1', payload: {} });
      smallQueue.enqueue({ type: 'task2', payload: {} });

      expect(() =>
        smallQueue.enqueue({ type: 'task3', payload: {} })
      ).toThrow('Queue full');
    });
  });

  describe('dequeue', () => {
    it('should dequeue highest priority task', () => {
      queue.enqueue({ type: 'normal', priority: TaskPriority.NORMAL, payload: {} });
      queue.enqueue({ type: 'high', priority: TaskPriority.HIGH, payload: {} });

      const task = queue.dequeue();
      expect(task.type).toBe('high');
      expect(task.status).toBe(TaskStatus.RUNNING);
    });

    it('should return null when empty', () => {
      const task = queue.dequeue();
      expect(task).toBeNull();
    });

    it('should match capabilities', () => {
      queue.enqueue({
        type: 'task1',
        capabilities: ['cap-a'],
        payload: {}
      });

      queue.enqueue({
        type: 'task2',
        capabilities: ['cap-b'],
        payload: {}
      });

      const task = queue.dequeue(['cap-b']);
      expect(task.type).toBe('task2');
    });
  });

  describe('complete', () => {
    it('should complete task', () => {
      const taskId = queue.enqueue({ type: 'test', payload: {} });
      const task = queue.dequeue();

      queue.complete(taskId, { result: 'success' });

      const retrieved = queue.getTask(taskId);
      expect(retrieved.status).toBe(TaskStatus.COMPLETED);
      expect(retrieved.result.result).toBe('success');
    });
  });

  describe('fail', () => {
    it('should retry failed task', () => {
      const taskId = queue.enqueue({
        type: 'test',
        maxRetries: 3,
        payload: {}
      });

      queue.dequeue();
      const retried = queue.fail(taskId, new Error('Test error'), true);

      expect(retried).toBe(true);
      expect(queue.size()).toBe(1); // Back in queue
    });

    it('should mark as failed after max retries', () => {
      const taskId = queue.enqueue({
        type: 'test',
        maxRetries: 0,
        payload: {}
      });

      queue.dequeue();
      const retried = queue.fail(taskId, new Error('Test error'), true);

      expect(retried).toBe(false);
      const task = queue.getTask(taskId);
      expect(task.status).toBe(TaskStatus.FAILED);
    });
  });

  describe('getStats', () => {
    it('should return queue statistics', () => {
      queue.enqueue({ type: 'test1', payload: {} });
      queue.enqueue({ type: 'test2', priority: TaskPriority.HIGH, payload: {} });

      const stats = queue.getStats();
      expect(stats.currentQueued).toBe(2);
      expect(stats.queuedByPriority.HIGH).toBe(1);
    });
  });
});

// ============================================================================
// AgentRouter Tests
// ============================================================================

describe('AgentRouter', () => {
  let router;

  beforeEach(() => {
    router = createAgentRouter({
      strategy: RoutingStrategy.CAPABILITY_MATCH
    });
  });

  describe('registerAgent', () => {
    it('should register agent', () => {
      router.registerAgent({
        agentId: 'agent-1',
        role: AgentRole.OBSERVER,
        capabilities: ['observe', 'compress']
      });

      expect(router.agents.size).toBe(1);
      expect(router.getAgent('agent-1')).toBeDefined();
    });
  });

  describe('route', () => {
    beforeEach(() => {
      router.registerAgent({
        agentId: 'observer-1',
        role: AgentRole.OBSERVER,
        capabilities: ['observe', 'compress']
      });

      router.registerAgent({
        agentId: 'validator-1',
        role: AgentRole.VALIDATOR,
        capabilities: ['validate']
      });
    });

    it('should route by capability', () => {
      const agents = router.route({
        requiredCapabilities: ['validate'],
        count: 1
      });

      expect(agents).toHaveLength(1);
      expect(agents[0].agentId).toBe('validator-1');
    });

    it('should route by role preference', () => {
      const agents = router.route({
        requiredCapabilities: [],
        preferredRole: AgentRole.OBSERVER,
        count: 1
      });

      expect(agents).toHaveLength(1);
      expect(agents[0].role).toBe(AgentRole.OBSERVER);
    });

    it('should throw when no agents match', () => {
      expect(() =>
        router.route({
          requiredCapabilities: ['nonexistent'],
          count: 1
        })
      ).toThrow('No agents found');
    });

    it('should respect max concurrent', () => {
      router.registerAgent({
        agentId: 'limited-1',
        role: AgentRole.EXECUTOR,
        capabilities: ['execute'],
        maxConcurrent: 1
      });

      // First routing succeeds
      const agents1 = router.route({
        requiredCapabilities: ['execute'],
        count: 1
      });
      expect(agents1).toHaveLength(1);

      // Second routing should fail (agent at max)
      expect(() =>
        router.route({
          requiredCapabilities: ['execute'],
          count: 1
        })
      ).toThrow('Insufficient agents');
    });
  });

  describe('releaseAgent', () => {
    it('should decrement agent load', () => {
      router.registerAgent({
        agentId: 'agent-1',
        role: AgentRole.EXECUTOR,
        capabilities: ['execute'],
        maxConcurrent: 2
      });

      router.route({ requiredCapabilities: ['execute'], count: 1 });
      expect(router.agentLoad.get('agent-1')).toBe(1);

      router.releaseAgent('agent-1');
      expect(router.agentLoad.get('agent-1')).toBe(0);
    });
  });
});

// ============================================================================
// CircuitBreaker Tests
// ============================================================================

describe('CircuitBreaker', () => {
  let breaker;

  beforeEach(() => {
    breaker = createCircuitBreaker({
      failureThreshold: 3,
      successThreshold: 2,
      resetTimeout: 100
    });
  });

  describe('execute', () => {
    it('should execute operation when closed', async () => {
      const result = await breaker.execute(async () => 'success');
      expect(result).toBe('success');
    });

    it('should open after failure threshold', async () => {
      const failingOp = async () => {
        throw new Error('Failed');
      };

      // Trigger failures
      for (let i = 0; i < 3; i++) {
        await expect(breaker.execute(failingOp)).rejects.toThrow('Failed');
      }

      expect(breaker.getState()).toBe(CircuitState.OPEN);
    });

    it('should reject when open', async () => {
      breaker.open();

      await expect(
        breaker.execute(async () => 'test')
      ).rejects.toThrow('Circuit breaker is OPEN');
    });

    it('should transition to half-open after reset timeout', async () => {
      breaker.open();

      // Wait for reset timeout
      await new Promise(resolve => setTimeout(resolve, 150));

      expect(breaker.getState()).toBe(CircuitState.HALF_OPEN);
    });

    it('should close after success threshold in half-open', async () => {
      breaker.open();
      await new Promise(resolve => setTimeout(resolve, 150));

      expect(breaker.getState()).toBe(CircuitState.HALF_OPEN);

      // Execute successful operations
      await breaker.execute(async () => 'success');
      await breaker.execute(async () => 'success');

      expect(breaker.getState()).toBe(CircuitState.CLOSED);
    });
  });

  describe('getStats', () => {
    it('should return breaker statistics', async () => {
      await breaker.execute(async () => 'success');

      try {
        await breaker.execute(async () => {
          throw new Error('Failed');
        });
      } catch {}

      const stats = breaker.getStats();
      expect(stats.totalRequests).toBe(2);
      expect(stats.totalSuccess).toBe(1);
      expect(stats.totalFailures).toBe(1);
    });
  });
});

describe('withCircuitBreaker', () => {
  it('should wrap function with breaker', async () => {
    const protectedFn = withCircuitBreaker(
      async (x) => x * 2,
      { failureThreshold: 3 }
    );

    const result = await protectedFn(5);
    expect(result).toBe(10);
  });
});

// ============================================================================
// ParallelOrchestrator Tests
// ============================================================================

describe('ParallelOrchestrator', () => {
  let orchestrator;

  beforeEach(async () => {
    orchestrator = createParallelOrchestrator({
      workerPool: {
        minWorkers: 2,
        maxWorkers: 5
      },
      taskQueue: {
        maxSize: 100
      },
      streaming: {
        enableDeltaCompression: true,
        snapshotInterval: 5
      }
    });

    await orchestrator.start();
  });

  afterEach(async () => {
    await orchestrator.shutdown({ graceful: false });
  });

  describe('registerAgent', () => {
    it('should register agent', () => {
      orchestrator.registerAgent({
        agentId: 'observer-1',
        role: AgentRole.OBSERVER,
        capabilities: ['observe', 'compress']
      });

      const agent = orchestrator.agentRouter.getAgent('observer-1');
      expect(agent).toBeDefined();
      expect(orchestrator.circuitBreakers.has('observer-1')).toBe(true);
    });
  });

  describe('submit', () => {
    it('should submit task and return observable', () => {
      const observable = orchestrator.submit({
        type: 'test-task',
        capabilities: ['test'],
        payload: { data: 'test' }
      });

      expect(observable).toBeDefined();
      expect(observable.taskId).toBeDefined();
      expect(orchestrator.taskQueue.size()).toBe(1);
    });

    it('should emit updates through observable', async () => {
      orchestrator.registerAgent({
        agentId: 'test-agent',
        role: AgentRole.EXECUTOR,
        capabilities: ['test']
      });

      const observable = orchestrator.submit({
        type: 'test-task',
        capabilities: ['test'],
        payload: {}
      });

      const updates = [];
      observable.on('delta', (update) => {
        updates.push(update);
      });

      return new Promise((resolve) => {
        observable.on('complete', (result) => {
          expect(updates.length).toBeGreaterThan(0);
          expect(result.success).toBe(true);
          resolve();
        });
      });
    });
  });

  describe('cancel', () => {
    it('should cancel queued task', () => {
      const observable = orchestrator.submit({
        type: 'test-task',
        payload: {}
      });

      const cancelled = orchestrator.cancel(observable.taskId);
      expect(cancelled).toBe(true);
      expect(orchestrator.taskQueue.size()).toBe(0);
    });
  });

  describe('checkpoint and resume', () => {
    it('should create and resume from checkpoint', () => {
      const taskId = 'test-task-1';
      const state = { progress: 50, data: 'checkpoint' };

      orchestrator.checkpoint(taskId, state);

      const resumed = orchestrator.resumeFromCheckpoint(taskId);
      expect(resumed).toEqual(state);
    });
  });

  describe('getStats', () => {
    it('should return orchestrator statistics', () => {
      const stats = orchestrator.getStats();

      expect(stats.workerPool).toBeDefined();
      expect(stats.taskQueue).toBeDefined();
      expect(stats.agentRouter).toBeDefined();
      expect(stats.uptime).toBeGreaterThan(0);
    });
  });
});

// ============================================================================
// Integration Tests
// ============================================================================

describe('Integration: Parallel Orchestration', () => {
  it('should orchestrate multiple agents in parallel', async () => {
    const orchestrator = createParallelOrchestrator({
      workerPool: { minWorkers: 2, maxWorkers: 5 },
      taskQueue: { maxSize: 100 }
    });

    await orchestrator.start();

    // Register multiple agents
    for (let i = 0; i < 3; i++) {
      orchestrator.registerAgent({
        agentId: `agent-${i}`,
        role: AgentRole.EXECUTOR,
        capabilities: ['execute'],
        maxConcurrent: 2
      });
    }

    // Submit multiple tasks
    const tasks = [];
    for (let i = 0; i < 5; i++) {
      const observable = orchestrator.submit({
        type: `task-${i}`,
        capabilities: ['execute'],
        payload: { id: i }
      });
      tasks.push(observable);
    }

    // Wait for all tasks to complete
    const results = await Promise.all(
      tasks.map(observable =>
        new Promise((resolve, reject) => {
          observable.on('complete', resolve);
          observable.on('error', reject);
        })
      )
    );

    expect(results).toHaveLength(5);
    expect(results.every(r => r.success)).toBe(true);

    await orchestrator.shutdown({ graceful: false });
  });
});
