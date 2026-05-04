/**
 * Agent Swarm Patterns Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  AgentSwarmPatterns,
  createSwarmPatterns,
  executeFanOutFanIn,
} from '../src/agent-swarm-patterns.mjs';
import { AgentHarness } from '../src/agent-harness.mjs';

describe('AgentSwarmPatterns', () => {
  let patterns;
  let agents;

  beforeEach(() => {
    patterns = createSwarmPatterns();

    // Create test agents
    agents = Array.from({ length: 3 }, (_, i) => ({
      config: {
        id: `agent-${i + 1}`,
        scope: { files: [`agent-${i + 1}/**`] },
        priority: i + 1,
        capabilities: ['read', 'write'],
        budget: {
          time_ms: 10000,
          steps: 50,
          bytes: 1024 * 1024,
        },
      },
      run: async (executor, tasks) => ({
        observations: tasks.length * 2,
        deltas: tasks.length,
        final_hash: `hash-${i + 1}`,
      }),
    }));
  });

  it('should execute fan-out/fan-in pattern', async () => {
    const tasks = [
      { type: 'read', target: 'file1' },
      { type: 'read', target: 'file2' },
      { type: 'read', target: 'file3' },
    ];

    const executor = async (probe) => ({
      success: true,
      data: `result-${probe.id}`,
    });

    const result = await patterns.fanOutFanIn(agents, executor, tasks);

    expect(result.pattern).toBe('fan_out_fan_in');
    expect(result.success).toBe(true);
    expect(result.agents_executed).toBe(3);
    expect(result.agents_succeeded).toBe(3);
    expect(result.total_observations).toBeGreaterThan(0);
  });

  it('should execute pipeline pattern', async () => {
    const initialInput = {
      target: 'start',
      params: { value: 10 },
    };

    const executor = async (probe) => ({
      success: true,
      result: `transformed-${probe.params.value || 0}`,
    });

    const result = await patterns.pipeline(agents, executor, initialInput);

    expect(result.pattern).toBe('pipeline');
    expect(result.success).toBe(true);
    expect(result.agents_executed).toBe(3);
  });

  it('should execute consensus pattern with majority', async () => {
    const tasks = [{ type: 'vote', target: 'decision' }];

    const executor = async () => ({
      success: true,
      vote: 'option-A',
    });

    const result = await patterns.consensus(
      agents,
      executor,
      tasks,
      'majority'
    );

    expect(result.pattern).toBe('consensus');
    // Consensus achieved is based on agents having same hash values
    expect(result.consensus_achieved).toBeDefined();
    expect(typeof result.consensus_achieved).toBe('boolean');
  });

  it('should execute map-reduce pattern', async () => {
    const tasks = Array.from({ length: 6 }, (_, i) => ({
      type: 'map',
      target: `item-${i}`,
      value: i + 1,
    }));

    const executor = async (probe) => ({
      success: true,
      value: probe.value || 0,
    });

    const reduceFunction = async (results) => {
      const sum = results.reduce((acc, r) => acc + (r.value || 0), 0);
      return { sum };
    };

    const result = await patterns.mapReduce(
      agents,
      executor,
      tasks,
      reduceFunction
    );

    expect(result.pattern).toBe('map_reduce');
    expect(result.success).toBe(true);
    expect(result.consensus_value).toHaveProperty('sum');
  });

  it('should handle agent failures in fan-out', async () => {
    const tasks = [{ type: 'read', target: 'test' }];

    // Create agents where one fails
    const mixedAgents = [
      {
        config: { id: 'agent-1', scope: { files: [] } },
        run: async () => ({ observations: 1, deltas: 0 }),
      },
      {
        config: { id: 'agent-2', scope: { files: [] } },
        run: async () => {
          throw new Error('Agent failed');
        },
      },
      {
        config: { id: 'agent-3', scope: { files: [] } },
        run: async () => ({ observations: 1, deltas: 0 }),
      },
    ];

    const executor = async () => ({ success: true });

    const result = await patterns.fanOutFanIn(
      mixedAgents,
      executor,
      tasks,
      { allow_partial_results: true }
    );

    expect(result.agents_succeeded).toBe(2);
    expect(result.agents_failed).toBe(1);
    expect(result.success).toBe(true); // Partial results allowed
  });

  it('should track execution history', async () => {
    const tasks = [{ type: 'test', target: 'history' }];
    const executor = async () => ({ success: true });

    await patterns.fanOutFanIn(agents, executor, tasks);
    await patterns.pipeline(agents, executor, { target: 'test' });

    const history = patterns.getHistory();
    expect(history).toHaveLength(2);
    expect(history[0].pattern).toBe('fan_out_fan_in');
    expect(history[1].pattern).toBe('pipeline');
  });

  it('should handle consensus with different strategies', async () => {
    const tasks = [{ type: 'vote', target: 'strategy-test' }];
    const executor = async () => ({ success: true });

    const strategies = ['majority', 'supermajority', 'unanimous', 'plurality'];

    for (const strategy of strategies) {
      const result = await patterns.consensus(agents, executor, tasks, strategy);
      expect(result.pattern).toBe('consensus');
      expect(result).toHaveProperty('consensus_achieved');
    }
  });
});
