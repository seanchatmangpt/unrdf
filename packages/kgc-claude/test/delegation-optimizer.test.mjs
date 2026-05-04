/**
 * Delegation Optimizer Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  DelegationOptimizer,
  createDelegationOptimizer,
  createOptimizerWithAgents,
} from '../src/delegation-optimizer.mjs';

describe('DelegationOptimizer', () => {
  let optimizer;

  beforeEach(() => {
    optimizer = createDelegationOptimizer({
      strategy: 'greedy',
      capability_weight: 0.5,
      availability_weight: 0.3,
      cost_weight: 0.2,
    });
  });

  it('should register agents', () => {
    optimizer.registerAgent({
      agent_id: 'agent-1',
      capabilities: ['read', 'write'],
      priority: 5,
      max_concurrent_tasks: 5,
      cost_factor: 1.0,
    });

    const stats = optimizer.getStatistics();
    expect(stats.agent_count).toBe(1);
  });

  it('should enqueue and prioritize tasks', () => {
    optimizer.enqueueTask({
      id: 'task-1',
      type: 'read',
      priority: 3,
      estimated_cost: 1,
    });

    optimizer.enqueueTask({
      id: 'task-2',
      type: 'write',
      priority: 8,
      estimated_cost: 2,
    });

    const stats = optimizer.getStatistics();
    expect(stats.queued_tasks).toBe(2);

    // High priority task should be first
    expect(optimizer.taskQueue[0].id).toBe('task-2');
  });

  it('should assign task to best matching agent', async () => {
    optimizer.registerAgent({
      agent_id: 'reader',
      capabilities: ['read'],
      max_concurrent_tasks: 5,
    });

    optimizer.registerAgent({
      agent_id: 'writer',
      capabilities: ['write'],
      max_concurrent_tasks: 5,
    });

    const task = {
      id: 'task-1',
      type: 'read',
      required_capabilities: ['read'],
      estimated_cost: 1,
    };

    const assignment = await optimizer.assignTask(task);

    expect(assignment).toBeDefined();
    expect(assignment.agent_id).toBe('reader');
    expect(assignment.task_id).toBe('task-1');
    expect(assignment.score).toBeGreaterThan(0);
  });

  it('should return null when no eligible agents', async () => {
    optimizer.registerAgent({
      agent_id: 'reader',
      capabilities: ['read'],
      max_concurrent_tasks: 5,
    });

    const task = {
      id: 'task-1',
      type: 'write',
      required_capabilities: ['write', 'transform'],
      estimated_cost: 1,
    };

    const assignment = await optimizer.assignTask(task);

    expect(assignment).toBeNull();
  });

  it('should process entire queue', async () => {
    optimizer.registerAgent({
      agent_id: 'multi-agent',
      capabilities: ['read', 'write', 'transform'],
      max_concurrent_tasks: 10,
    });

    for (let i = 0; i < 5; i++) {
      optimizer.enqueueTask({
        id: `task-${i}`,
        type: 'read',
        required_capabilities: ['read'],
        priority: i,
        estimated_cost: 1,
      });
    }

    const { assignments, unassigned } = await optimizer.processQueue();

    expect(assignments).toHaveLength(5);
    expect(unassigned).toHaveLength(0);
  });

  it('should balance load across agents', async () => {
    // Create agents
    const agents = [
      {
        agent_id: 'agent-1',
        capabilities: ['read'],
        max_concurrent_tasks: 5,
      },
      {
        agent_id: 'agent-2',
        capabilities: ['read'],
        max_concurrent_tasks: 5,
      },
      {
        agent_id: 'agent-3',
        capabilities: ['read'],
        max_concurrent_tasks: 5,
      },
    ];

    agents.forEach(a => optimizer.registerAgent(a));

    // Assign multiple tasks
    for (let i = 0; i < 9; i++) {
      await optimizer.assignTask({
        id: `task-${i}`,
        type: 'read',
        required_capabilities: ['read'],
        priority: 5,
        estimated_cost: 1,
      });
    }

    const stats = optimizer.getStatistics();
    expect(stats.total_load).toBe(9);

    // Load should be distributed
    const agent1 = optimizer.agents.get('agent-1');
    const agent2 = optimizer.agents.get('agent-2');
    const agent3 = optimizer.agents.get('agent-3');

    expect(agent1.current_load).toBe(3);
    expect(agent2.current_load).toBe(3);
    expect(agent3.current_load).toBe(3);
  });

  it('should release agents after task completion', async () => {
    optimizer.registerAgent({
      agent_id: 'agent-1',
      capabilities: ['read'],
      max_concurrent_tasks: 5,
    });

    await optimizer.assignTask({
      id: 'task-1',
      type: 'read',
      required_capabilities: ['read'],
      estimated_cost: 1,
    });

    const agent = optimizer.agents.get('agent-1');
    expect(agent.current_load).toBe(1);

    optimizer.releaseAgent('agent-1');
    expect(agent.current_load).toBe(0);
  });

  it('should update agent success rate', () => {
    optimizer.registerAgent({
      agent_id: 'agent-1',
      capabilities: ['read'],
      max_concurrent_tasks: 5,
      success_rate: 1.0,
    });

    const agent = optimizer.agents.get('agent-1');
    expect(agent.success_rate).toBe(1.0);

    // Simulate failure
    optimizer.updateSuccessRate('agent-1', false);
    expect(agent.success_rate).toBeLessThan(1.0);

    // Simulate success
    optimizer.updateSuccessRate('agent-1', true);
    expect(agent.success_rate).toBeGreaterThan(0);
  });

  it('should rebalance load when enabled', () => {
    optimizer = createDelegationOptimizer({
      enable_load_balancing: true,
      rebalance_threshold: 0.3,
    });

    // Create agents with imbalanced load
    optimizer.registerAgent({
      agent_id: 'overloaded',
      capabilities: ['read'],
      max_concurrent_tasks: 5,
      current_load: 5,
    });

    optimizer.registerAgent({
      agent_id: 'underloaded',
      capabilities: ['read'],
      max_concurrent_tasks: 5,
      current_load: 0,
    });

    const rebalanced = optimizer.rebalanceLoad();

    expect(rebalanced).toBeGreaterThan(0);

    const over = optimizer.agents.get('overloaded');
    const under = optimizer.agents.get('underloaded');

    expect(over.current_load).toBeLessThan(5);
    expect(under.current_load).toBeGreaterThan(0);
  });

  it('should create optimizer with agents', () => {
    const agents = [
      { agent_id: 'agent-1', capabilities: ['read'], max_concurrent_tasks: 5 },
      { agent_id: 'agent-2', capabilities: ['write'], max_concurrent_tasks: 5 },
    ];

    const optimizerWithAgents = createOptimizerWithAgents(agents);

    const stats = optimizerWithAgents.getStatistics();
    expect(stats.agent_count).toBe(2);
  });

  it('should provide utilization metrics', async () => {
    optimizer.registerAgent({
      agent_id: 'agent-1',
      capabilities: ['read'],
      max_concurrent_tasks: 10,
    });

    // Assign 5 tasks
    for (let i = 0; i < 5; i++) {
      await optimizer.assignTask({
        id: `task-${i}`,
        type: 'read',
        required_capabilities: ['read'],
        estimated_cost: 1,
      });
    }

    const stats = optimizer.getStatistics();
    expect(stats.utilization).toBe(0.5); // 5/10
    expect(stats.total_assignments).toBe(5);
  });
});
