/**
 * @fileoverview Tests for Worker Swarm
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { WorkerSwarm, Agent, AgentStatus } from '../worker-swarm.mjs';
import { CoordinationHub } from '../coordination.mjs';

describe('Agent', () => {
  it('should process work successfully', async () => {
    const processor = vi.fn(async (work) => ({ processed: work }));
    const agent = new Agent('agent-1', processor);

    const result = await agent.process({
      id: 'work-1',
      payload: { data: 'test' }
    });

    expect(result.success).toBe(true);
    expect(result.result.processed).toEqual({ data: 'test' });
    expect(agent.completedCount).toBe(1);
    expect(agent.status).toBe(AgentStatus.IDLE);
  });

  it('should handle work failure', async () => {
    const processor = vi.fn(async () => {
      throw new Error('Processing failed');
    });
    const agent = new Agent('agent-1', processor);

    const result = await agent.process({
      id: 'work-1',
      payload: {}
    });

    expect(result.success).toBe(false);
    expect(result.error).toBe('Processing failed');
    expect(agent.failedCount).toBe(1);
  });

  it('should track availability', () => {
    const agent = new Agent('agent-1', async () => ({}));

    expect(agent.isAvailable()).toBe(true);

    agent.status = AgentStatus.WORKING;
    expect(agent.isAvailable()).toBe(false);
  });
});

describe('WorkerSwarm', () => {
  let swarm;

  beforeEach(() => {
    swarm = new WorkerSwarm('test-swarm', {
      domain: 'test',
      capacity: 5
    });
  });

  it('should create a worker swarm', () => {
    expect(swarm.id).toBe('test-swarm');
    expect(swarm.domain).toBe('test');
    expect(swarm.capacity).toBe(5);
  });

  it('should add and remove agents', () => {
    const processor = async (work) => work;

    swarm.addAgent('agent-1', processor);
    expect(swarm.agents.size).toBe(1);

    swarm.addAgent('agent-2', processor);
    expect(swarm.agents.size).toBe(2);

    swarm.removeAgent('agent-1');
    expect(swarm.agents.size).toBe(1);
  });

  it('should enforce capacity limit', () => {
    const processor = async (work) => work;

    for (let i = 0; i < 5; i++) {
      swarm.addAgent(`agent-${i}`, processor);
    }

    expect(() => {
      swarm.addAgent('agent-overflow', processor);
    }).toThrow('Swarm at capacity');
  });

  it('should process work items', async () => {
    const processor = vi.fn(async (work) => ({
      processed: true,
      ...work
    }));

    swarm.addAgent('agent-1', processor);
    await swarm.start();

    const result = await swarm.submitWork({
      type: 'test',
      payload: { data: 'test-data' }
    });

    expect(result.processed).toBe(true);
    expect(result.data).toBe('test-data');
    expect(processor).toHaveBeenCalledTimes(1);

    await swarm.stop();
  });

  it('should queue work when agents are busy', async () => {
    let resolveWork;
    const processor = vi.fn(async () => {
      return new Promise(resolve => {
        resolveWork = resolve;
      });
    });

    swarm.addAgent('agent-1', processor);
    await swarm.start();

    // Submit first work (will block)
    const promise1 = swarm.submitWork({
      type: 'test',
      payload: { id: 1 }
    });

    // Submit second work (should queue)
    const promise2 = swarm.submitWork({
      type: 'test',
      payload: { id: 2 }
    });

    // Check queue
    expect(swarm.workQueue.length).toBeGreaterThan(0);

    // Complete first work
    resolveWork({ id: 1 });
    await promise1;

    // Second work should now process
    resolveWork({ id: 2 });
    await promise2;

    await swarm.stop();
  });

  it('should retry failed work', async () => {
    let attemptCount = 0;
    const processor = vi.fn(async () => {
      attemptCount++;
      if (attemptCount < 3) {
        throw new Error('Retry me');
      }
      return { success: true, attempts: attemptCount };
    });

    swarm.addAgent('agent-1', processor);
    await swarm.start();

    const result = await swarm.submitWork({
      type: 'test',
      payload: {},
      maxRetries: 3
    });

    expect(result.success).toBe(true);
    expect(result.attempts).toBe(3);
    expect(processor).toHaveBeenCalledTimes(3);

    await swarm.stop();
  });

  it('should timeout long-running work', async () => {
    const processor = vi.fn(async () => {
      await new Promise(resolve => setTimeout(resolve, 10000)); // 10 seconds
      return { completed: true };
    });

    swarm.addAgent('agent-1', processor);
    await swarm.start();

    await expect(swarm.submitWork({
      type: 'test',
      payload: {},
      timeout: 100, // 100ms timeout
      maxRetries: 0
    })).rejects.toThrow('Work timeout');

    await swarm.stop();
  });

  it('should connect to coordination hub', () => {
    const hub = new CoordinationHub();
    swarm.connectToHub(hub);

    expect(swarm.hub).toBe(hub);

    const stats = hub.getStats();
    expect(stats.totalSwarms).toBe(1);
  });

  it('should generate receipts for work', async () => {
    const processor = vi.fn(async (work) => ({ result: 'success' }));

    swarm.addAgent('agent-1', processor);
    await swarm.start();

    await swarm.submitWork({
      type: 'test',
      payload: { data: 'test' }
    });

    expect(swarm.receiptChain.length).toBe(1);

    const verification = await swarm.verifyReceipts();
    expect(verification.valid).toBe(true);

    await swarm.stop();
  });

  it('should provide statistics', async () => {
    const processor = vi.fn(async (work) => work);

    swarm.addAgent('agent-1', processor);
    swarm.addAgent('agent-2', processor);
    await swarm.start();

    await swarm.submitWork({
      type: 'test',
      payload: {}
    });

    const stats = swarm.getStats();

    expect(stats.id).toBe('test-swarm');
    expect(stats.domain).toBe('test');
    expect(stats.agents.total).toBe(2);
    expect(stats.work.completed).toBe(1);

    await swarm.stop();
  });

  it('should export to JSON-LD', () => {
    swarm.addAgent('agent-1', async () => ({}));

    const jsonld = swarm.toJSONLD();

    expect(jsonld['@type']).toBe('swarm:WorkerSwarm');
    expect(jsonld['swarm:id']).toBe('test-swarm');
    expect(jsonld['swarm:domain']).toBe('test');
    expect(jsonld['swarm:agents']).toContain('agent-1');
  });
});
