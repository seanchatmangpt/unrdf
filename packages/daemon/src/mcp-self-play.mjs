/**
 * @file MCP Self-Play Loop Engine
 * @module @unrdf/daemon/mcp-self-play
 *
 * Autonomous feedback loops where MCP tools call each other.
 * Each iteration: tool → result → next tool selection → execute → feedback loop
 */

import { randomUUID } from 'crypto';

/**
 * Self-play episode (autonomous tool chain execution)
 */
export class SelfPlayEpisode {
  constructor(initialContext) {
    this.episodeId = randomUUID();
    this.context = initialContext;
    this.steps = [];
    this.feedback = [];
    this.startTime = Date.now();
    this.terminated = false;
    this.terminationReason = null;
  }

  /**
   * Record a tool execution step
   */
  recordStep(toolName, input, output, metadata = {}) {
    this.steps.push({
      stepId: randomUUID(),
      toolName,
      input,
      output,
      metadata,
      timestamp: Date.now(),
      duration: metadata.duration || 0,
    });
  }

  /**
   * Record feedback signal (reward/penalty)
   */
  recordFeedback(signal, reason = '') {
    this.feedback.push({
      feedbackId: randomUUID(),
      signal, // number: -1 to 1 for penalty/reward
      reason,
      timestamp: Date.now(),
    });
  }

  /**
   * Terminate episode with reason
   */
  terminate(reason) {
    this.terminated = true;
    this.terminationReason = reason;
  }

  /**
   * Get episode metrics
   */
  getMetrics() {
    const totalDuration = Date.now() - this.startTime;
    const totalSteps = this.steps.length;
    const totalFeedback = this.feedback.length;
    const cumulativeFeedback = this.feedback.reduce((sum, f) => sum + f.signal, 0);
    const avgFeedback = totalFeedback > 0 ? cumulativeFeedback / totalFeedback : 0;

    return {
      episodeId: this.episodeId,
      totalDuration,
      totalSteps,
      totalFeedback,
      cumulativeFeedback,
      avgFeedback,
      terminated: this.terminated,
      terminationReason: this.terminationReason,
    };
  }

  /**
   * Serialize episode to JSON
   */
  toJSON() {
    return {
      episodeId: this.episodeId,
      context: this.context,
      steps: this.steps,
      feedback: this.feedback,
      metrics: this.getMetrics(),
    };
  }
}

/**
 * Self-play agent (makes decisions and executes tools)
 */
export class SelfPlayAgent {
  constructor(toolRegistry, decisionFn) {
    this.toolRegistry = toolRegistry; // { toolName: { handler, schema } }
    this.decisionFn = decisionFn; // (episode, previousResult) → { toolName, input } or null
    this.episodes = [];
    this.maxStepsPerEpisode = 10;
    this.maxEpisodes = 5;
  }

  /**
   * Run a single episode
   */
  async runEpisode(initialContext) {
    const episode = new SelfPlayEpisode(initialContext);

    let previousResult = null;

    for (let stepCount = 0; stepCount < this.maxStepsPerEpisode; stepCount++) {
      if (episode.terminated) break;

      // Decide next tool
      const decision = await this.decisionFn(episode, previousResult);
      if (!decision) {
        episode.terminate('no more decisions');
        break;
      }

      const { toolName, input } = decision;

      // Validate tool exists
      if (!this.toolRegistry[toolName]) {
        episode.recordStep(toolName, input, null, { success: false, error: 'Tool not found' });
        episode.recordFeedback(-1, `Tool not found: ${toolName}`);
        episode.terminate(`unknown tool: ${toolName}`);
        break;
      }

      // Execute tool
      const stepStartTime = Date.now();
      try {
        const result = await this.toolRegistry[toolName].handler(input);
        const duration = Date.now() - stepStartTime;

        episode.recordStep(toolName, input, result, { duration, success: true });
        previousResult = result;

        // Auto-reward on success (positive feedback)
        episode.recordFeedback(0.1, `${toolName} succeeded`);
      } catch (err) {
        const duration = Date.now() - stepStartTime;
        episode.recordStep(toolName, input, null, { duration, success: false, error: err.message });

        // Auto-penalty on failure (negative feedback)
        episode.recordFeedback(-0.5, `${toolName} failed: ${err.message}`);
        episode.terminate(`tool execution failed: ${toolName}`);
      }
    }

    // Check if we hit step limit
    if (!episode.terminated && episode.steps.length >= this.maxStepsPerEpisode) {
      episode.terminate('max steps reached');
    }

    this.episodes.push(episode);
    return episode;
  }

  /**
   * Run multiple episodes
   */
  async runEpisodes(initialContext, count = this.maxEpisodes) {
    const results = [];
    for (let i = 0; i < count; i++) {
      const episode = await this.runEpisode(initialContext);
      results.push(episode);
    }
    return results;
  }

  /**
   * Get all episodes
   */
  getEpisodes() {
    return this.episodes;
  }

  /**
   * Get best episode by cumulative feedback
   */
  getBestEpisode() {
    if (this.episodes.length === 0) return null;
    return this.episodes.reduce((best, ep) => {
      const bestFeedback = best.getMetrics().cumulativeFeedback;
      const epFeedback = ep.getMetrics().cumulativeFeedback;
      return epFeedback > bestFeedback ? ep : best;
    });
  }

  /**
   * Get episode statistics
   */
  getStats() {
    if (this.episodes.length === 0) return null;

    const metrics = this.episodes.map(ep => ep.getMetrics());
    const avgFeedback = metrics.reduce((sum, m) => sum + m.avgFeedback, 0) / metrics.length;
    const avgSteps = metrics.reduce((sum, m) => sum + m.totalSteps, 0) / metrics.length;
    const successCount = metrics.filter(m => m.cumulativeFeedback > 0).length;

    return {
      totalEpisodes: this.episodes.length,
      successCount,
      successRate: successCount / this.episodes.length,
      avgFeedback,
      avgSteps,
      bestEpisodeId: this.getBestEpisode().episodeId,
    };
  }
}

/**
 * Factory function for random tool selection policy (fixed signature)
 * @param {Array<string>} toolNames - Available tool names
 * @returns {Function} decisionFn(episode, previousResult) => { toolName, input } | null
 */
export function createRandomPolicy(toolNames) {
  return async (episode, _previousResult) => {
    if (episode.steps.length >= 5) return null;
    const toolName = toolNames[Math.floor(Math.random() * toolNames.length)];
    return { toolName, input: { file: episode.context.graphFile } };
  };
}

/**
 * Factory function for greedy feedback policy (fixed signature)
 * Selects tools with highest success rate, with inter-episode history
 * @param {Object} toolRegistry - { toolName: { handler, schema } }
 * @returns {Function} decisionFn(episode, previousResult) => { toolName, input } | null
 */
export function createGreedyPolicy(toolRegistry) {
  const history = {}; // toolName → { success, total }

  return async (episode, _previousResult) => {
    if (episode.steps.length >= 10) return null;

    // Update history from latest step
    const last = episode.steps.at(-1);
    if (last) {
      history[last.toolName] ??= { success: 0, total: 0 };
      history[last.toolName].total++;
      if (last.metadata.success) history[last.toolName].success++;
    }

    // Pick tool with best success rate (try untested tools first)
    const toolNames = Object.keys(toolRegistry);
    const untested = toolNames.find(n => !history[n]);
    if (untested) {
      return { toolName: untested, input: { file: episode.context.graphFile } };
    }

    // Find best rate
    const best = toolNames.reduce((a, b) => {
      const ra = history[a].success / history[a].total;
      const rb = history[b].success / history[b].total;
      return ra >= rb ? a : b;
    });

    return { toolName: best, input: { file: episode.context.graphFile } };
  };
}

/**
 * Pre-built decision functions (policies)
 */
export const SelfPlayPolicies = {
  /**
   * Query → Hooks → Query (explore pattern)
   */
  explorePatternDecision: async (episode, _previousResult) => {
    const stepCount = episode.steps.length;

    if (stepCount === 0) {
      // Start with query
      return {
        toolName: 'unrdf_graph_query',
        input: {
          file: episode.context.graphFile,
          query: `
            SELECT ?s ?p ?o
            WHERE { ?s ?p ?o }
            LIMIT 100
          `,
          format: 'json',
        },
      };
    }

    if (stepCount === 1) {
      // Execute hooks based on query results
      return {
        toolName: 'unrdf_hooks_execute',
        input: {
          store: episode.context.graphFile,
          config: episode.context.hooksConfig || 'hooks.json',
          showReceipts: true,
        },
      };
    }

    if (stepCount === 2) {
      // Query again to see changes
      return {
        toolName: 'unrdf_graph_query',
        input: {
          file: episode.context.graphFile,
          query: `
            SELECT (COUNT(?s) as ?count)
            WHERE { ?s ?p ?o }
          `,
          format: 'json',
        },
      };
    }

    // End episode
    return null;
  },
};

/**
 * Self-play orchestrator
 */
export async function runSelfPlayLoop(toolRegistry, options = {}) {
  const {
    initialContext = {},
    decisionPolicy = SelfPlayPolicies.explorePatternDecision,
    episodeCount = 3,
    maxStepsPerEpisode = 10,
  } = options;

  const agent = new SelfPlayAgent(toolRegistry, decisionPolicy);
  agent.maxStepsPerEpisode = maxStepsPerEpisode;
  agent.maxEpisodes = episodeCount;

  const episodes = await agent.runEpisodes(initialContext, episodeCount);
  const stats = agent.getStats();

  return {
    episodes,
    stats,
    bestEpisode: agent.getBestEpisode(),
    agent,
  };
}
