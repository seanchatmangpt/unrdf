/**
 * @file Knowledge Hooks Self-Play Autonomics Tests
 * @module test/self-play-autonomics
 * @description Test autonomous hooks-driven feedback loops
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  buildHooksToolRegistry,
  createHooksAwarePolicy,
  computeHooksFeedback,
  runHooksAutonomics,
} from '../src/hooks/self-play-autonomics.mjs';

describe('buildHooksToolRegistry', () => {
  it('should return tool registry with structured handlers', () => {
    const store = {};
    const hooks = [];

    const registry = buildHooksToolRegistry(store, hooks);

    expect(registry).toHaveProperty('hooks_evaluate_conditions');
    expect(registry).toHaveProperty('hooks_execute_effects');
    expect(registry).toHaveProperty('hooks_query');
  });

  it('hooks_evaluate_conditions handler should return structured result', async () => {
    const store = {};
    const hooks = [];
    const registry = buildHooksToolRegistry(store, hooks);

    const result = await registry.hooks_evaluate_conditions.handler({
      store,
      hooks,
    });

    expect(result).toHaveProperty('conditionResults');
    expect(result).toHaveProperty('satisfied');
    expect(result).toHaveProperty('successRate');
    expect(Array.isArray(result.conditionResults)).toBe(true);
    expect(Array.isArray(result.satisfied)).toBe(true);
    expect(typeof result.successRate).toBe('number');
  });

  it('hooks_execute_effects handler should return structured result with receipt', async () => {
    const store = {};
    const hooks = [];
    const registry = buildHooksToolRegistry(store, hooks);

    const result = await registry.hooks_execute_effects.handler({
      store,
      hooks,
      delta: { adds: 0, deletes: 0 },
    });

    expect(result).toHaveProperty('executionResults');
    expect(result).toHaveProperty('receipt');
    expect(result.receipt).toHaveProperty('receiptHash');
    expect(result.receipt).toHaveProperty('input_hash');
    expect(result.receipt).toHaveProperty('output_hash');
    expect(result.receipt).toHaveProperty('previousReceiptHash');
    expect(result.receipt).toHaveProperty('hooksExecuted');
    expect(result.receipt).toHaveProperty('successful');
    expect(result.receipt).toHaveProperty('failed');
  });

  it('hooks_query handler should return result object', async () => {
    const store = {};
    const registry = buildHooksToolRegistry(store);

    const result = await registry.hooks_query.handler({
      store,
      query: 'SELECT ?s WHERE { ?s ?p ?o }',
      kind: 'sparql-ask',
    });

    expect(result).toHaveProperty('result');
  });
});

describe('createHooksAwarePolicy', () => {
  let policy;
  let episode;

  beforeEach(() => {
    policy = createHooksAwarePolicy(async () => false);
    episode = {
      steps: [],
      context: {
        store: {},
        hooks: [],
      },
      recordFeedback: () => {},
    };
  });

  it('should evaluate conditions on step 0', async () => {
    const decision = await policy(episode, null);
    expect(decision).toEqual({
      toolName: 'hooks_evaluate_conditions',
      input: {
        store: episode.context.store,
        hooks: episode.context.hooks,
      },
    });
  });

  it('should execute effects on step 1 if conditions satisfied', async () => {
    episode.steps.push({ toolName: 'hooks_evaluate_conditions' });
    const previousResult = { satisfied: [{ name: 'cond1' }] };

    const decision = await policy(episode, previousResult);

    expect(decision).toEqual({
      toolName: 'hooks_execute_effects',
      input: {
        store: episode.context.store,
        hooks: episode.context.hooks,
        delta: { adds: 0, deletes: 0 },
      },
    });
  });

  it('should terminate on step 1 if no conditions satisfied', async () => {
    episode.steps.push({ toolName: 'hooks_evaluate_conditions' });
    const previousResult = { satisfied: [] };

    const decision = await policy(episode, previousResult);

    expect(decision).toBeNull();
  });

  it('should terminate on step 2+ if goal condition met', async () => {
    const goalMet = async () => true;
    const policyWithGoal = createHooksAwarePolicy(goalMet);

    episode.steps.push({ toolName: 'hooks_evaluate_conditions' });
    episode.steps.push({ toolName: 'hooks_execute_effects' });

    const decision = await policyWithGoal(episode, {});

    expect(decision).toBeNull();
  });

  it('should re-evaluate conditions on step 2+ if goal not met', async () => {
    const goalNotMet = async () => false;
    const policyWithGoal = createHooksAwarePolicy(goalNotMet);

    episode.steps.push({ toolName: 'hooks_evaluate_conditions' });
    episode.steps.push({ toolName: 'hooks_execute_effects' });

    const decision = await policyWithGoal(episode, {});

    expect(decision).toEqual({
      toolName: 'hooks_evaluate_conditions',
      input: {
        store: episode.context.store,
        hooks: episode.context.hooks,
      },
    });
  });
});

describe('computeHooksFeedback', () => {
  it('should return negative signal on null execution result', () => {
    const feedback = computeHooksFeedback(null, {});
    expect(feedback).toBe(-0.5);
  });

  it('should return neutral signal on null receipt', () => {
    const feedback = computeHooksFeedback({}, {});
    expect(feedback).toBe(0);
  });

  it('should return negative signal on hook failures', () => {
    const result = {
      receipt: {
        hooksExecuted: 2,
        successful: 1,
        failed: 1,
      },
    };
    const feedback = computeHooksFeedback(result, {});
    expect(feedback).toBe(-0.3);
  });

  it('should return neutral signal when no hooks ran', () => {
    const result = {
      receipt: {
        hooksExecuted: 0,
        successful: 0,
        failed: 0,
      },
    };
    const feedback = computeHooksFeedback(result, {});
    expect(feedback).toBe(0);
  });

  it('should return positive signal based on success rate', () => {
    const result = {
      receipt: {
        hooksExecuted: 2,
        successful: 2,
        failed: 0,
      },
    };
    const feedback = computeHooksFeedback(result, {});
    expect(feedback).toBeGreaterThan(0.1);
    expect(feedback).toBeLessThanOrEqual(0.2);
  });

  it('should scale feedback with partial success', () => {
    const result = {
      receipt: {
        hooksExecuted: 4,
        successful: 2,
        failed: 0,
      },
    };
    const feedback = computeHooksFeedback(result, {});
    expect(feedback).toBeGreaterThan(0.1);
    expect(feedback).toBeLessThan(0.2);
  });
});

describe('runHooksAutonomics', () => {
  it('should initialize and run episodes', async () => {
    const store = {};
    const hooks = [];

    const result = await runHooksAutonomics(store, hooks, {
      goalCondition: async () => false,
      episodeCount: 1,
      maxStepsPerEpisode: 5,
    });

    expect(result).toHaveProperty('episodes');
    expect(result).toHaveProperty('finalStore');
    expect(result).toHaveProperty('receiptChain');
    expect(result).toHaveProperty('stats');
    expect(Array.isArray(result.episodes)).toBe(true);
    expect(result.episodes.length).toBe(1);
  });

  it('should track episode structure', async () => {
    const store = {};
    const hooks = [];

    const result = await runHooksAutonomics(store, hooks, {
      episodeCount: 1,
      maxStepsPerEpisode: 5,
    });

    const episode = result.episodes[0];
    expect(episode).toHaveProperty('episodeId');
    expect(episode).toHaveProperty('stepCount');
    expect(episode).toHaveProperty('stepResults');
    expect(episode).toHaveProperty('feedback');
    expect(episode).toHaveProperty('terminated');
    expect(episode).toHaveProperty('terminationReason');
    expect(episode).toHaveProperty('metrics');
  });

  it('should calculate episode metrics', async () => {
    const store = {};
    const hooks = [];

    const result = await runHooksAutonomics(store, hooks, {
      episodeCount: 1,
      maxStepsPerEpisode: 5,
    });

    const metrics = result.episodes[0].metrics;
    expect(metrics).toHaveProperty('stepCount');
    expect(metrics).toHaveProperty('feedbackCount');
    expect(metrics).toHaveProperty('totalFeedback');
    expect(metrics).toHaveProperty('avgFeedback');
  });

  it('should accumulate receipt chain', async () => {
    const store = {};
    const hooks = [];

    const result = await runHooksAutonomics(store, hooks, {
      episodeCount: 2,
      maxStepsPerEpisode: 5,
    });

    expect(Array.isArray(result.receiptChain)).toBe(true);
    expect(result.stats).toHaveProperty('receiptChainLength');
  });

  it('should link receipts with previousReceiptHash', async () => {
    const store = {};
    const hooks = [];

    const result = await runHooksAutonomics(store, hooks, {
      episodeCount: 1,
      maxStepsPerEpisode: 5,
    });

    const chain = result.receiptChain;
    if (chain.length > 1) {
      // If there are multiple receipts, check chaining
      for (let i = 1; i < chain.length; i++) {
        expect(chain[i].previousReceiptHash).toBeDefined();
      }
    }
  });

  it('should calculate aggregate stats', async () => {
    const store = {};
    const hooks = [];

    const result = await runHooksAutonomics(store, hooks, {
      episodeCount: 3,
      maxStepsPerEpisode: 5,
    });

    const stats = result.stats;
    expect(stats.totalEpisodes).toBe(3);
    expect(stats).toHaveProperty('successCount');
    expect(stats).toHaveProperty('successRate');
    expect(stats.successRate).toBeGreaterThanOrEqual(0);
    expect(stats.successRate).toBeLessThanOrEqual(1);
    expect(stats).toHaveProperty('totalFeedback');
    expect(stats).toHaveProperty('avgFeedback');
  });

  it('should invoke onEpisodeEnd callback', async () => {
    const store = {};
    const hooks = [];
    const episodeEnds = [];

    const _result = await runHooksAutonomics(store, hooks, {
      episodeCount: 2,
      maxStepsPerEpisode: 5,
      onEpisodeEnd: (ep) => {
        episodeEnds.push(ep.episodeId);
      },
    });

    expect(episodeEnds.length).toBe(2);
  });

  it('should return finalStore as shared reference', async () => {
    const store = { data: 'test' };
    const hooks = [];

    const result = await runHooksAutonomics(store, hooks, {
      episodeCount: 1,
      maxStepsPerEpisode: 5,
    });

    expect(result.finalStore).toBe(store);
  });

  it('should handle termination reasons', async () => {
    const store = {};
    const hooks = [];

    const result = await runHooksAutonomics(store, hooks, {
      episodeCount: 1,
      maxStepsPerEpisode: 1,
    });

    const episode = result.episodes[0];
    expect(episode.terminated).toBe(true);
    expect(episode.terminationReason).toBeDefined();
  });

  it('should record feedback signals from conditions', async () => {
    const store = {};
    const hooks = [];

    const result = await runHooksAutonomics(store, hooks, {
      episodeCount: 1,
      maxStepsPerEpisode: 5,
    });

    const episode = result.episodes[0];
    expect(episode.feedback.length).toBeGreaterThanOrEqual(0);
    if (episode.feedback.length > 0) {
      episode.feedback.forEach(fb => {
        expect(typeof fb.signal).toBe('number');
        expect(typeof fb.reason).toBe('string');
      });
    }
  });
});
