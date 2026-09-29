/**
 * @file End-to-end tests over a real WebSocket transport
 * @description A real WorkerNode connects to a real orchestrator and executes
 * a dependent workflow; results flow back through the transport.
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { DistributedOrchestrator } from '../src/orchestrator.mjs';
import { WorkerNode } from '../src/worker-node.mjs';

async function waitFor(predicate, timeoutMs = 4000) {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    if (await predicate()) return;
    await new Promise((resolve) => setTimeout(resolve, 25));
  }
  throw new Error('waitFor timed out');
}

describe('orchestrator <-> worker over WebSocket', () => {
  let orchestrator;
  let worker;

  beforeAll(async () => {
    orchestrator = new DistributedOrchestrator({ port: 0 });
    await orchestrator.initialize();

    worker = new WorkerNode({
      nodeId: 'e2e-worker',
      orchestratorUrl: `http://localhost:${orchestrator.config.port}`,
      capacity: 2,
      heartbeatInterval: 50,
    });
    await worker.start();
    await waitFor(() => orchestrator.workers.has('e2e-worker'));
  });

  afterAll(async () => {
    await worker?.shutdown();
    await orchestrator?.shutdown();
  });

  it('runs a dependent workflow to completion', async () => {
    const workflowId = await orchestrator.submitWorkflow(
      {
        id: 'chain',
        tasks: [
          { id: 'a', type: 'compute' },
          { id: 'b', type: 'transform', dependsOn: ['a'] },
        ],
      },
      { value: 21 }
    );

    await waitFor(() => orchestrator.activeWorkflows.get(workflowId).status === 'completed');

    const status = await orchestrator.engine.getStatus(workflowId);
    expect(status).toEqual({ state: 'completed', completed: 2, total: 2 });
    expect(orchestrator.getStats().tasks.active).toBe(0);
    expect(orchestrator.getStats().tasks.queued).toBe(0);
  });

  it('records worker heartbeats', async () => {
    const before = orchestrator.workers.get('e2e-worker').lastHeartbeat;
    await waitFor(() => orchestrator.workers.get('e2e-worker').lastHeartbeat > before);
  });

  it('reports worker uptime after start', () => {
    expect(worker.getStats().uptime).toBeGreaterThan(0);
  });
});
