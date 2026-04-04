/**
 * @file MCP Self-Play Loop Tests
 * @module test/mcp-self-play
 * @description Test autonomous MCP tool chains with feedback loops
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  SelfPlayEpisode,
  SelfPlayAgent,
  SelfPlayPolicies,
  runSelfPlayLoop,
} from '../src/mcp-self-play.mjs';

describe('SelfPlayEpisode', () => {
  let episode;

  beforeEach(() => {
    episode = new SelfPlayEpisode({ graphFile: 'test.ttl' });
  });

  it('should initialize with context and empty steps', () => {
    expect(episode.context).toEqual({ graphFile: 'test.ttl' });
    expect(episode.steps).toEqual([]);
    expect(episode.feedback).toEqual([]);
    expect(episode.terminated).toBe(false);
  });

  it('should record tool execution steps', () => {
    episode.recordStep('unrdf_query', { query: 'SELECT *' }, { count: 5 });
    expect(episode.steps).toHaveLength(1);
    expect(episode.steps[0].toolName).toBe('unrdf_query');
    expect(episode.steps[0].output.count).toBe(5);
  });

  it('should record feedback signals', () => {
    episode.recordFeedback(0.5, 'query succeeded');
    episode.recordFeedback(-0.2, 'validation failed');
    expect(episode.feedback).toHaveLength(2);
    expect(episode.feedback[0].signal).toBe(0.5);
    expect(episode.feedback[1].signal).toBe(-0.2);
  });

  it('should calculate cumulative feedback', () => {
    episode.recordFeedback(0.3, 'step 1');
    episode.recordFeedback(0.4, 'step 2');
    episode.recordFeedback(-0.1, 'step 3');
    const metrics = episode.getMetrics();
    expect(metrics.cumulativeFeedback).toBe(0.6);
    expect(metrics.avgFeedback).toBeCloseTo(0.2, 5);
  });

  it('should terminate with reason', () => {
    episode.terminate('max steps reached');
    expect(episode.terminated).toBe(true);
    expect(episode.terminationReason).toBe('max steps reached');
  });

  it('should serialize to JSON', () => {
    episode.recordStep('tool1', { input: 'x' }, { output: 'y' });
    episode.recordFeedback(0.5, 'success');
    const json = episode.toJSON();
    expect(json.episodeId).toBe(episode.episodeId);
    expect(json.steps).toHaveLength(1);
    expect(json.feedback).toHaveLength(1);
    expect(json.metrics).toBeDefined();
  });
});

describe('SelfPlayAgent', () => {
  let agent;
  let toolRegistry;

  beforeEach(() => {
    toolRegistry = {
      tool_a: {
        handler: async (input) => ({ result: 'a', input }),
        schema: {},
      },
      tool_b: {
        handler: async (input) => ({ result: 'b', input }),
        schema: {},
      },
    };

    // Simple decision function: alternate between tools
    const decisionFn = async (episode, _previousResult) => {
      const stepCount = episode.steps.length;
      if (stepCount >= 2) return null;
      const toolName = stepCount === 0 ? 'tool_a' : 'tool_b';
      return { toolName, input: { step: stepCount } };
    };

    agent = new SelfPlayAgent(toolRegistry, decisionFn);
    agent.maxStepsPerEpisode = 5;
  });

  it('should initialize with tool registry and decision function', () => {
    expect(agent.toolRegistry).toBe(toolRegistry);
    expect(agent.episodes).toEqual([]);
  });

  it('should execute single episode', async () => {
    const episode = await agent.runEpisode({ graphFile: 'test.ttl' });
    expect(episode.steps.length).toBe(2);
    expect(episode.steps[0].toolName).toBe('tool_a');
    expect(episode.steps[1].toolName).toBe('tool_b');
    expect(episode.terminated).toBe(true);
  });

  it('should record success on tool execution', async () => {
    const episode = await agent.runEpisode({ graphFile: 'test.ttl' });
    expect(episode.steps.every(s => s.metadata.success)).toBe(true);
    expect(episode.feedback.some(f => f.signal > 0)).toBe(true);
  });

  it('should handle tool execution errors', async () => {
    toolRegistry.tool_a.handler = async () => {
      throw new Error('Tool failed');
    };

    const episode = await agent.runEpisode({ graphFile: 'test.ttl' });
    expect(episode.steps[0].metadata.success).toBe(false);
    expect(episode.steps[0].metadata.error).toBe('Tool failed');
    expect(episode.terminated).toBe(true);
    expect(episode.feedback.some(f => f.signal < 0)).toBe(true);
  });

  it('should track multiple episodes', async () => {
    await agent.runEpisodes({ graphFile: 'test.ttl' }, 3);
    expect(agent.episodes).toHaveLength(3);
  });

  it('should calculate agent statistics', async () => {
    await agent.runEpisodes({ graphFile: 'test.ttl' }, 3);
    const stats = agent.getStats();
    expect(stats.totalEpisodes).toBe(3);
    expect(stats.successRate).toBe(1);
    expect(stats.avgSteps).toBe(2);
  });

  it('should identify best episode by feedback', async () => {
    await agent.runEpisodes({ graphFile: 'test.ttl' }, 3);
    const best = agent.getBestEpisode();
    expect(best).toBeDefined();
    expect(best.episodeId).toBeDefined();
  });
});

describe('SelfPlayPolicies', () => {
  let episode;
  let toolRegistry;

  beforeEach(() => {
    episode = new SelfPlayEpisode({ graphFile: 'test.ttl', hooksConfig: 'hooks.json' });
    toolRegistry = {
      unrdf_graph_query: { handler: async () => ({ count: 10 }) },
      unrdf_hooks_execute: { handler: async () => ({ success: true }) },
      unrdf_graph_stats: { handler: async () => ({ quads: 100 }) },
    };
  });

  it('should have explorePatternDecision policy', async () => {
    const decision1 = await SelfPlayPolicies.explorePatternDecision(episode, null);
    expect(decision1.toolName).toBe('unrdf_graph_query');

    episode.recordStep('unrdf_graph_query', {}, { queryResult: 'data' });
    const decision2 = await SelfPlayPolicies.explorePatternDecision(episode, { queryResult: 'data' });
    expect(decision2.toolName).toBe('unrdf_hooks_execute');

    episode.recordStep('unrdf_hooks_execute', {}, { hooksResult: 'executed' });
    const decision3 = await SelfPlayPolicies.explorePatternDecision(episode, { hooksResult: 'executed' });
    expect(decision3.toolName).toBe('unrdf_graph_query');

    episode.recordStep('unrdf_graph_query', {}, { finalQuery: 'result' });
    const decision4 = await SelfPlayPolicies.explorePatternDecision(episode, { finalQuery: 'result' });
    expect(decision4).toBeNull();
  });

  it('should have randomDecision policy', async () => {
    const toolNames = Object.keys(toolRegistry);
    const decision = await SelfPlayPolicies.randomDecision(episode, null, toolNames);
    expect(toolNames).toContain(decision.toolName);
  });

  it('should have greedyFeedbackDecision policy', async () => {
    const decision = await SelfPlayPolicies.greedyFeedbackDecision(
      episode,
      null,
      toolRegistry
    );
    expect(Object.keys(toolRegistry)).toContain(decision.toolName);
  });
});

describe('Self-play orchestration', () => {
  it('should run self-play loop with explore policy', async () => {
    const toolRegistry = {
      tool_1: {
        handler: async () => ({ result: 'step1' }),
      },
      tool_2: {
        handler: async () => ({ result: 'step2' }),
      },
    };

    const decisionFn = async (episode) => {
      if (episode.steps.length >= 2) return null;
      return {
        toolName: episode.steps.length === 0 ? 'tool_1' : 'tool_2',
        input: {},
      };
    };

    const result = await runSelfPlayLoop(toolRegistry, {
      initialContext: { graphFile: 'test.ttl' },
      decisionPolicy: decisionFn,
      episodeCount: 2,
      maxStepsPerEpisode: 5,
    });

    expect(result.episodes).toHaveLength(2);
    expect(result.stats.totalEpisodes).toBe(2);
    expect(result.stats.successRate).toBe(1);
    expect(result.bestEpisode).toBeDefined();
  });
});
