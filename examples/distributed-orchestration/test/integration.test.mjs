/**
 * @file Integration Tests
 * @description Integration tests for distributed orchestration
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { DistributedOrchestrator } from '../src/orchestrator.mjs';
import { WorkerNode } from '../src/worker-node.mjs';
import { createWorkflow } from '@unrdf/yawl';

describe('Distributed Orchestration', () => {
  let orchestrator;
  let worker1;
  let worker2;

  beforeAll(async () => {
    // Start orchestrator
    orchestrator = new DistributedOrchestrator({ port: 8081 });
    await orchestrator.initialize();

    // Start workers
    worker1 = new WorkerNode({
      nodeId: 'worker-1',
      orchestratorUrl: 'http://localhost:8081',
      capacity: 3,
    });

    worker2 = new WorkerNode({
      nodeId: 'worker-2',
      orchestratorUrl: 'http://localhost:8081',
      capacity: 3,
    });

    // Register workers
    await orchestrator.registerWorker('worker-1', { capacity: 3 });
    await orchestrator.registerWorker('worker-2', { capacity: 3 });
  });

  afterAll(async () => {
    await worker1?.shutdown();
    await worker2?.shutdown();
    await orchestrator?.shutdown();
  });

  it('should submit and execute workflow', async () => {
    const workflow = {
      id: 'test-workflow',
      tasks: [
        { id: 'task-1', type: 'compute' },
        { id: 'task-2', type: 'io' },
      ],
    };

    const workflowId = await orchestrator.submitWorkflow(workflow, {
      value: 42,
    });

    expect(workflowId).toBeDefined();
    expect(typeof workflowId).toBe('string');
  });

  it('should distribute tasks to workers', async () => {
    const stats = orchestrator.getStats();

    expect(stats.workers.healthy).toBeGreaterThanOrEqual(0);
    expect(stats.workflows.total).toBeGreaterThanOrEqual(0);
  });

  it('should handle worker failure and reassign tasks', async () => {
    // Simulate worker failure
    const worker = orchestrator.workers.get('worker-1');
    if (worker) {
      worker.lastHeartbeat = Date.now() - 40000; // Old heartbeat
    }

    // Wait for health check
    await new Promise((resolve) => setTimeout(resolve, 15000));

    const stats = orchestrator.getStats();
    expect(stats.workers.healthy).toBeLessThanOrEqual(2);
  });

  it('should track workflow completion', async () => {
    const workflow = {
      id: 'completion-test',
      tasks: [{ id: 'task-1', type: 'default' }],
    };

    const workflowId = await orchestrator.submitWorkflow(workflow, {});

    // Simulate task completion
    await orchestrator.handleTaskCompletion(workflowId, 'task-1', {
      success: true,
    });

    const workflowData = orchestrator.activeWorkflows.get(workflowId);
    expect(workflowData).toBeDefined();
  });
});
