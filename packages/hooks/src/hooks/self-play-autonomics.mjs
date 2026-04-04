/**
 * @file Knowledge Hooks Self-Play Autonomics
 * @module @unrdf/hooks/self-play-autonomics
 * @description Autonomous feedback loops where hook conditions trigger effects which mutate the RDF store
 *
 * Enables closed-loop knowledge engineering:
 * 1. Evaluate conditions (SPARQL ASK/SELECT, SHACL, delta, threshold, ...)
 * 2. Execute satisfied effects (SPARQL CONSTRUCT mutations)
 * 3. Store changes produce receipt (BLAKE3 hash, linked chain)
 * 4. Re-evaluate conditions until goal or no-op
 */

import { randomUUID } from 'crypto';
import { evaluateCondition } from './condition-evaluator.mjs';
import { KnowledgeHookEngine } from './knowledge-hook-engine.mjs';
import { ask, select, construct } from './query.mjs';

/**
 * Build a self-play toolRegistry from knowledge hooks
 * Returns structured ChainResult objects instead of CLI strings
 *
 * @param {Object} store - RDF store (N3 or Oxigraph)
 * @param {Array} hooks - Array of KnowledgeHook definitions
 * @returns {Object} toolRegistry { toolName: { handler, schema } }
 */
export function buildHooksToolRegistry(store, hooks = []) {
  const engine = new KnowledgeHookEngine(store);

  return {
    hooks_evaluate_conditions: {
      handler: async ({ store: evalStore, hooks: evalHooks }) => {
        // Evaluate all conditions and collect results
        const conditionResults = [];
        const satisfied = [];

        for (const hook of evalHooks) {
          try {
            const result = await evaluateCondition(hook.condition, evalStore);
            conditionResults.push({
              hookName: hook.name,
              condition: hook.condition,
              satisfied: result,
            });

            if (result) {
              satisfied.push(hook);
            }
          } catch (err) {
            conditionResults.push({
              hookName: hook.name,
              condition: hook.condition,
              satisfied: false,
              error: err.message,
            });
          }
        }

        const successRate = evalHooks.length > 0 ? satisfied.length / evalHooks.length : 0;

        return {
          conditionResults,
          satisfied,
          successRate,
        };
      },
    },

    hooks_execute_effects: {
      handler: async ({ store: execStore, hooks: execHooks, delta: _delta }) => {
        // Execute hooks with receipt chaining
        const context = {
          nodeId: 'self-play-autonomics',
          t_ns: BigInt(Date.now() * 1000000),
        };

        try {
          const result = await engine.execute(context, execHooks);

          return {
            executionResults: execHooks.map((h) => ({
              hookName: h.name,
              status: 'executed',
            })),
            receipt: {
              receiptHash: result.receipt.receiptHash,
              input_hash: result.receipt.input_hash,
              output_hash: result.receipt.output_hash,
              previousReceiptHash: result.receipt.previousReceiptHash || null,
              hooksExecuted: result.successful + result.failed,
              successful: result.successful,
              failed: result.failed,
              delta: result.receipt.delta,
              timestamp: result.receipt.timestamp,
            },
          };
        } catch (err) {
          return {
            executionResults: execHooks.map((h) => ({
              hookName: h.name,
              status: 'failed',
              error: err.message,
            })),
            receipt: null,
            error: err.message,
          };
        }
      },
    },

    hooks_query: {
      handler: async ({ store: queryStore, query: sparqlQuery, kind = 'sparql-ask' }) => {
        // Execute SPARQL query based on kind
        try {
          let result;

          switch (kind.toLowerCase()) {
            case 'sparql-ask':
            case 'ask':
              result = await ask(queryStore, sparqlQuery);
              break;

            case 'sparql-select':
            case 'select':
              result = await select(queryStore, sparqlQuery);
              break;

            case 'sparql-construct':
            case 'construct':
              result = await construct(queryStore, sparqlQuery);
              break;

            default:
              // Default to ASK
              result = await ask(queryStore, sparqlQuery);
          }

          return {
            result,
            kind,
            success: true,
          };
        } catch (err) {
          return {
            result: null,
            kind,
            success: false,
            error: err.message,
          };
        }
      },
    },
  };
}

/**
 * Create a decision policy that reads hook results and branches intelligently
 *
 * @param {Function} goalCondition - async (store, previousResult) => boolean
 * @returns {Function} decisionFn(episode, previousResult) => { toolName, input } | null
 */
export function createHooksAwarePolicy(goalCondition = async () => false) {
  return async (episode, previousResult) => {
    const stepCount = episode.steps.length;
    const { store, hooks } = episode.context;

    // Step 0: always evaluate conditions first
    if (stepCount === 0) {
      return {
        toolName: 'hooks_evaluate_conditions',
        input: { store, hooks },
      };
    }

    // Step 1: if conditions satisfied, execute effects; else terminate
    if (stepCount === 1) {
      const { satisfied } = previousResult || {};
      if (!satisfied || satisfied.length === 0) {
        return null; // No conditions to satisfy
      }

      return {
        toolName: 'hooks_execute_effects',
        input: { store, hooks, delta: { adds: 0, deletes: 0 } },
      };
    }

    // Step 2+: check goal condition
    if (stepCount >= 2) {
      if (await goalCondition(store, previousResult)) {
        episode.recordFeedback(1.0, 'goal condition satisfied');
        return null; // Success
      }

      // Re-evaluate to see if more conditions fire
      return {
        toolName: 'hooks_evaluate_conditions',
        input: { store, hooks },
      };
    }

    return null;
  };
}

/**
 * Compute feedback signal based on hook execution outcome
 *
 * @param {Object} executionResult - from hooks_execute_effects
 * @param {Object} previousResult - from hooks_evaluate_conditions
 * @returns {number} feedback signal (-1 to 1)
 */
export function computeHooksFeedback(executionResult, _previousResult) {
  if (!executionResult) return -0.5; // Execution failed

  const { receipt } = executionResult;
  if (!receipt) return 0; // No-op

  const { hooksExecuted, successful, failed } = receipt;

  if (failed > 0) return -0.3; // Some failures
  if (successful === 0) return 0; // No hooks ran (no-op)
  if (hooksExecuted > 0) return 0.1 + 0.1 * Math.min(successful / hooksExecuted, 1); // Pro-rata success

  return 0;
}

/**
 * Receipt chain node
 */
class ReceiptChainNode {
  constructor(receipt, previousNode = null) {
    this.receiptHash = receipt.receiptHash;
    this.input_hash = receipt.input_hash;
    this.output_hash = receipt.output_hash;
    this.previousReceiptHash = previousNode?.receiptHash || receipt.previousReceiptHash || null;
    this.timestamp = receipt.timestamp || new Date().toISOString();
    this.delta = receipt.delta;
    this.hooksExecuted = receipt.hooksExecuted;
  }

  toJSON() {
    return {
      receiptHash: this.receiptHash,
      input_hash: this.input_hash,
      output_hash: this.output_hash,
      previousReceiptHash: this.previousReceiptHash,
      timestamp: this.timestamp,
      delta: this.delta,
      hooksExecuted: this.hooksExecuted,
    };
  }
}

/**
 * Run full autonomous hooks loop across multiple episodes
 *
 * @param {Object} store - RDF store (shared across episodes)
 * @param {Array} hookDefinitions - KnowledgeHook array
 * @param {Object} options - { goalCondition, episodeCount, maxStepsPerEpisode, onEpisodeEnd }
 * @returns {Promise<Object>} { episodes, finalStore, receiptChain, stats }
 */
export async function runHooksAutonomics(store, hookDefinitions = [], options = {}) {
  const {
    goalCondition = async () => false,
    episodeCount = 3,
    maxStepsPerEpisode = 10,
    onEpisodeEnd = () => {},
  } = options;

  // Build tool registry
  const toolRegistry = buildHooksToolRegistry(store, hookDefinitions);

  // Create decision policy
  const decisionFn = createHooksAwarePolicy(goalCondition);

  // Track receipt chain
  const receiptChain = [];
  let previousReceiptNode = null;

  const episodes = [];

  for (let e = 0; e < episodeCount; e++) {
    const episode = {
      episodeId: randomUUID(),
      stepCount: 0,
      stepResults: [],
      feedback: [],
      terminated: false,
      terminationReason: null,
      timestamp: new Date().toISOString(),
    };

    const context = {
      store,
      hooks: hookDefinitions,
    };

    let previousResult = null;

    for (let step = 0; step < maxStepsPerEpisode; step++) {
      if (episode.terminated) break;

      // Decide next tool
      const decision = await decisionFn(
        { steps: episode.stepResults, context, recordFeedback: (s, r) => episode.feedback.push({ signal: s, reason: r }) },
        previousResult
      );

      if (!decision) {
        episode.terminated = true;
        episode.terminationReason = 'no more decisions';
        episode.feedback.push({ signal: 0, reason: 'terminated: no more decisions' });
        break;
      }

      const { toolName, input } = decision;

      // Execute tool
      const stepStartTime = Date.now();
      try {
        const tool = toolRegistry[toolName];
        if (!tool) {
          episode.terminated = true;
          episode.terminationReason = `unknown tool: ${toolName}`;
          episode.feedback.push({ signal: -1, reason: `unknown tool: ${toolName}` });
          break;
        }

        const result = await tool.handler(input);
        const duration = Date.now() - stepStartTime;

        episode.stepResults.push({
          stepId: randomUUID(),
          toolName,
          input,
          output: result,
          duration,
          timestamp: new Date().toISOString(),
          success: true,
        });

        previousResult = result;

        // Record receipt if present
        if (result?.receipt) {
          const receiptNode = new ReceiptChainNode(result.receipt, previousReceiptNode);
          receiptChain.push(receiptNode.toJSON());
          previousReceiptNode = receiptNode;
        }

        // Compute feedback
        const feedback = computeHooksFeedback(result, previousResult);
        episode.feedback.push({
          signal: feedback,
          reason: `${toolName} succeeded`,
        });
      } catch (err) {
        const duration = Date.now() - stepStartTime;

        episode.stepResults.push({
          stepId: randomUUID(),
          toolName,
          input,
          output: null,
          duration,
          timestamp: new Date().toISOString(),
          success: false,
          error: err.message,
        });

        episode.terminated = true;
        episode.terminationReason = `tool failed: ${toolName}`;
        episode.feedback.push({ signal: -0.5, reason: `${toolName} failed: ${err.message}` });
      }

      episode.stepCount++;
    }

    if (!episode.terminated && episode.stepCount >= maxStepsPerEpisode) {
      episode.terminated = true;
      episode.terminationReason = 'max steps reached';
      episode.feedback.push({ signal: 0, reason: 'max steps reached' });
    }

    // Calculate metrics
    const totalFeedback = episode.feedback.reduce((sum, f) => sum + f.signal, 0);
    const avgFeedback = episode.feedback.length > 0 ? totalFeedback / episode.feedback.length : 0;

    episode.metrics = {
      stepCount: episode.stepCount,
      feedbackCount: episode.feedback.length,
      totalFeedback,
      avgFeedback,
    };

    episodes.push(episode);
    await onEpisodeEnd(episode);
  }

  // Calculate stats
  const totalFeedback = episodes.reduce((sum, ep) => sum + ep.metrics.totalFeedback, 0);
  const successCount = episodes.filter(ep => ep.metrics.totalFeedback > 0).length;

  const stats = {
    totalEpisodes: episodes.length,
    successCount,
    successRate: episodes.length > 0 ? successCount / episodes.length : 0,
    totalFeedback,
    avgFeedback: episodes.length > 0 ? totalFeedback / episodes.length : 0,
    receiptChainLength: receiptChain.length,
  };

  return {
    episodes,
    finalStore: store,
    receiptChain,
    stats,
  };
}
