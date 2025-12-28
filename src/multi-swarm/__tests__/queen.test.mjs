/**
 * @fileoverview Tests for Queen Swarm
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { QueenSwarm } from '../queen.mjs';
import { WorkerSwarm } from '../worker-swarm.mjs';

describe('QueenSwarm', () => {
  let queen;

  beforeEach(() => {
    queen = new QueenSwarm({ id: 'test-queen' });
  });

  it('should create a queen swarm', () => {
    expect(queen.id).toBe('test-queen');
    expect(queen.workerSwarms.size).toBe(0);
  });

  it('should add and remove worker swarms', () => {
    const worker1 = new WorkerSwarm('worker-1', { domain: 'test' });
    const worker2 = new WorkerSwarm('worker-2', { domain: 'test' });

    queen.addWorkerSwarm(worker1);
    queen.addWorkerSwarm(worker2);

    expect(queen.workerSwarms.size).toBe(2);

    queen.removeWorkerSwarm('worker-1');
    expect(queen.workerSwarms.size).toBe(1);
  });

  it('should distribute jobs to worker swarms', async () => {
    const processor = vi.fn(async (work) => ({
      processed: true,
      ...work
    }));

    const worker1 = new WorkerSwarm('worker-1', { domain: 'test', capacity: 2 });
    worker1.addAgent('agent-1', processor);

    queen.addWorkerSwarm(worker1);
    await queen.start();

    const result = await queen.submitJob({
      type: 'test',
      domain: 'test',
      payload: { data: 'test-data' }
    });

    expect(result.processed).toBe(true);
    expect(processor).toHaveBeenCalledTimes(1);

    await queen.stop();
  });

  it('should partition work across swarms', async () => {
    const processor1 = vi.fn(async (work) => work);
    const processor2 = vi.fn(async (work) => work);

    const worker1 = new WorkerSwarm('worker-1', { domain: 'domain-1', capacity: 2 });
    worker1.addAgent('agent-1', processor1);

    const worker2 = new WorkerSwarm('worker-2', { domain: 'domain-2', capacity: 2 });
    worker2.addAgent('agent-2', processor2);

    queen.addWorkerSwarm(worker1);
    queen.addWorkerSwarm(worker2);
    await queen.start();

    const testData = Array.from({ length: 10 }, (_, i) => ({
      id: i,
      value: i
    }));

    const result = await queen.submitJob({
      type: 'process',
      payload: testData,
      partitionStrategy: 'domain',
      aggregationStrategy: 'concat'
    });

    // Both workers should have processed data
    expect(processor1).toHaveBeenCalled();
    expect(processor2).toHaveBeenCalled();

    // Result should be concatenated
    expect(Array.isArray(result)).toBe(true);

    await queen.stop();
  });

  it('should aggregate results using concat strategy', async () => {
    const processor = vi.fn(async (work) => work);

    const worker1 = new WorkerSwarm('worker-1', { domain: 'test', capacity: 2 });
    worker1.addAgent('agent-1', processor);

    queen.addWorkerSwarm(worker1);
    await queen.start();

    const result = await queen.submitJob({
      type: 'process',
      payload: [[1, 2], [3, 4], [5, 6]],
      partitionStrategy: 'round-robin',
      aggregationStrategy: 'concat'
    });

    expect(result).toEqual([1, 2, 3, 4, 5, 6]);

    await queen.stop();
  });

  it('should aggregate results using merge strategy', async () => {
    const processor = vi.fn(async (work) => work);

    const worker1 = new WorkerSwarm('worker-1', { domain: 'test', capacity: 2 });
    worker1.addAgent('agent-1', processor);

    queen.addWorkerSwarm(worker1);
    await queen.start();

    const result = await queen.submitJob({
      type: 'process',
      payload: [{ a: 1 }, { b: 2 }, { c: 3 }],
      partitionStrategy: 'round-robin',
      aggregationStrategy: 'merge'
    });

    expect(result).toEqual({ a: 1, b: 2, c: 3 });

    await queen.stop();
  });

  it('should generate queen-level receipts', async () => {
    const processor = vi.fn(async (work) => work);

    const worker1 = new WorkerSwarm('worker-1', { domain: 'test', capacity: 2 });
    worker1.addAgent('agent-1', processor);

    queen.addWorkerSwarm(worker1);
    await queen.start();

    await queen.submitJob({
      type: 'test',
      payload: { data: 'test' }
    });

    expect(queen.queenReceiptChain.length).toBe(1);

    await queen.stop();
  });

  it('should verify nested receipt chains', async () => {
    const processor = vi.fn(async (work) => work);

    const worker1 = new WorkerSwarm('worker-1', { domain: 'test', capacity: 2 });
    worker1.addAgent('agent-1', processor);

    queen.addWorkerSwarm(worker1);
    await queen.start();

    await queen.submitJob({
      type: 'test',
      payload: { data: 'test' }
    });

    const verification = await queen.verifyAllReceipts();

    expect(verification.valid).toBe(true);
    expect(verification.queen.valid).toBe(true);
    expect(verification.workers.length).toBe(1);
    expect(verification.workers[0].verification.valid).toBe(true);

    await queen.stop();
  });

  it('should handle job failures gracefully', async () => {
    const processor = vi.fn(async () => {
      throw new Error('Processing failed');
    });

    const worker1 = new WorkerSwarm('worker-1', { domain: 'test', capacity: 2 });
    worker1.addAgent('agent-1', processor);

    queen.addWorkerSwarm(worker1);
    await queen.start();

    await expect(queen.submitJob({
      type: 'test',
      payload: { data: 'test' },
      timeout: 1000
    })).rejects.toThrow();

    // Should still generate failure receipt
    expect(queen.queenReceiptChain.length).toBe(1);

    await queen.stop();
  });

  it('should provide comprehensive statistics', async () => {
    const processor = vi.fn(async (work) => work);

    const worker1 = new WorkerSwarm('worker-1', { domain: 'test', capacity: 2 });
    worker1.addAgent('agent-1', processor);

    const worker2 = new WorkerSwarm('worker-2', { domain: 'test', capacity: 2 });
    worker2.addAgent('agent-2', processor);

    queen.addWorkerSwarm(worker1);
    queen.addWorkerSwarm(worker2);
    await queen.start();

    await queen.submitJob({
      type: 'test',
      payload: { data: 'test' }
    });

    const stats = queen.getStats();

    expect(stats.queen.id).toBe('test-queen');
    expect(stats.queen.swarms).toBe(2);
    expect(stats.queen.completedJobs).toBe(1);
    expect(stats.swarms.length).toBe(2);
    expect(stats.jobs.completed).toBe(1);

    await queen.stop();
  });

  it('should send heartbeats to worker swarms', async () => {
    const processor = vi.fn(async (work) => work);

    const worker1 = new WorkerSwarm('worker-1', { domain: 'test', capacity: 2 });
    worker1.addAgent('agent-1', processor);

    queen.addWorkerSwarm(worker1);

    let heartbeatCount = 0;
    queen.on('queen:heartbeat', () => {
      heartbeatCount++;
    });

    await queen.start();

    // Wait for at least one heartbeat
    await new Promise(resolve => setTimeout(resolve, 6000));

    expect(heartbeatCount).toBeGreaterThan(0);

    await queen.stop();
  });

  it('should export to JSON-LD', async () => {
    const processor = vi.fn(async (work) => work);

    const worker1 = new WorkerSwarm('worker-1', { domain: 'test', capacity: 2 });
    worker1.addAgent('agent-1', processor);

    queen.addWorkerSwarm(worker1);

    const jsonld = queen.toJSONLD();

    expect(jsonld['@type']).toBe('swarm:QueenSwarm');
    expect(jsonld['swarm:id']).toBe('test-queen');
    expect(jsonld['swarm:workerSwarms'].length).toBe(1);
  });

  it('should handle multiple concurrent jobs', async () => {
    const processor = vi.fn(async (work) => {
      await new Promise(resolve => setTimeout(resolve, 50));
      return work;
    });

    const worker1 = new WorkerSwarm('worker-1', { domain: 'test', capacity: 5 });
    for (let i = 0; i < 5; i++) {
      worker1.addAgent(`agent-${i}`, processor);
    }

    queen.addWorkerSwarm(worker1);
    await queen.start();

    const jobPromises = Array.from({ length: 10 }, (_, i) =>
      queen.submitJob({
        type: 'test',
        payload: { id: i }
      })
    );

    const results = await Promise.all(jobPromises);

    expect(results.length).toBe(10);
    expect(queen.completedJobs.size).toBe(10);

    await queen.stop();
  });
});
