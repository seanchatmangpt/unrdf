/**
 * @file Unit Tests
 * @description Unit tests for orchestrator components
 */

import { describe, it, expect } from 'vitest';
import { DistributedOrchestrator } from '../src/orchestrator.mjs';
import { WorkerNode } from '../src/worker-node.mjs';

describe('DistributedOrchestrator', () => {
  it('should create orchestrator with default config', () => {
    const orchestrator = new DistributedOrchestrator();

    expect(orchestrator).toBeDefined();
    expect(orchestrator.config.port).toBe(8080);
    expect(orchestrator.config.maxWorkers).toBe(10);
  });

  it('should register worker node', async () => {
    const orchestrator = new DistributedOrchestrator();

    await orchestrator.registerWorker('worker-1', { capacity: 5 });

    expect(orchestrator.workers.has('worker-1')).toBe(true);
    const worker = orchestrator.workers.get('worker-1');
    expect(worker.capacity).toBe(5);
    expect(worker.status).toBe('healthy');
  });

  it('should get orchestrator stats', () => {
    const orchestrator = new DistributedOrchestrator();
    const stats = orchestrator.getStats();

    expect(stats).toHaveProperty('workflows');
    expect(stats).toHaveProperty('workers');
    expect(stats).toHaveProperty('tasks');
    expect(stats.workflows.total).toBe(0);
  });
});

describe('WorkerNode', () => {
  it('should create worker with default config', () => {
    const worker = new WorkerNode();

    expect(worker).toBeDefined();
    expect(worker.config.capacity).toBe(5);
    expect(worker.config.heartbeatInterval).toBe(5000);
  });

  it('should get worker stats', () => {
    const worker = new WorkerNode({ nodeId: 'test-worker' });
    const stats = worker.getStats();

    expect(stats.nodeId).toBe('test-worker');
    expect(stats.capacity).toBe(5);
    expect(stats.activeTasks).toBe(0);
    expect(stats.available).toBe(5);
  });
});
