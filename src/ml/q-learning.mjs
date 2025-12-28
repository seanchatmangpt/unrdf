/**
 * @file q-learning.mjs
 * @description Q-learning for KGC-SWARM agent action selection
 * Native JavaScript implementation using typed arrays for Q-table
 *
 * Features:
 * - Epsilon-greedy exploration vs exploitation
 * - Action selection for agent probes/strategies
 * - Reward based on convergence speed + compression ratio
 * - No external ML libraries - pure JavaScript with Float64Array
 */

import { z } from 'zod';

/**
 * State representation schema
 * @type {z.ZodObject}
 */
export const StateSchema = z.object({
  driftLevel: z.enum(['high', 'medium', 'low', 'converged']),
  epochCount: z.enum(['early', 'middle', 'late']),
  budgetUsed: z.enum(['low', 'medium', 'high']),
});

/**
 * Action schema
 * @type {z.ZodObject}
 */
export const ActionSchema = z.object({
  id: z.string(),
  name: z.string(),
  description: z.string().optional(),
});

/**
 * Experience schema for replay
 * @type {z.ZodObject}
 */
export const ExperienceSchema = z.object({
  state: StateSchema,
  action: z.string(),
  reward: z.number(),
  nextState: StateSchema,
  terminal: z.boolean(),
});

/**
 * QLearningAgent: Reinforcement learning for agent control
 *
 * @class
 * @description
 * Implements Q-learning algorithm for selecting which probe/strategy an agent
 * should choose. Uses epsilon-greedy policy with learning rate decay and
 * experience replay for stability.
 *
 * @example
 * const agent = new QLearningAgent({
 *   actions: [
 *     { id: 'aggressive', name: 'Aggressive Compression' },
 *     { id: 'balanced', name: 'Balanced' },
 *     { id: 'conservative', name: 'Conservative' }
 *   ],
 *   epsilon: 0.1,
 *   learningRate: 0.1,
 *   discountFactor: 0.9
 * });
 *
 * const state = { driftLevel: 'high', epochCount: 'early', budgetUsed: 'low' };
 * const action = agent.selectAction(state);
 * agent.learn(state, action, reward, nextState, false);
 */
export class QLearningAgent {
  /**
   * @param {Object} config - Q-learning configuration
   * @param {Array<{id: string, name: string}>} config.actions - Available actions
   * @param {number} [config.epsilon=0.1] - Exploration rate (0-1)
   * @param {number} [config.learningRate=0.1] - Learning rate α (0-1)
   * @param {number} [config.discountFactor=0.9] - Discount factor γ (0-1)
   * @param {number} [config.epsilonDecay=0.995] - Epsilon decay per episode
   * @param {number} [config.minEpsilon=0.01] - Minimum epsilon value
   */
  constructor(config) {
    // Validate actions
    if (!config.actions || config.actions.length === 0) {
      throw new Error('At least one action must be defined');
    }

    this.actions = config.actions.map(a => ActionSchema.parse(a));
    this.actionIds = this.actions.map(a => a.id);

    this.epsilon = config.epsilon !== undefined ? config.epsilon : 0.1;
    this.learningRate = config.learningRate !== undefined ? config.learningRate : 0.1;
    this.discountFactor = config.discountFactor !== undefined ? config.discountFactor : 0.9;
    this.epsilonDecay = config.epsilonDecay !== undefined ? config.epsilonDecay : 0.995;
    this.minEpsilon = config.minEpsilon !== undefined ? config.minEpsilon : 0.01;

    // State space dimensions
    this.driftLevels = ['high', 'medium', 'low', 'converged'];
    this.epochCounts = ['early', 'middle', 'late'];
    this.budgetUsed = ['low', 'medium', 'high'];

    const numStates = this.driftLevels.length * this.epochCounts.length * this.budgetUsed.length;
    const numActions = this.actions.length;

    // Q-table: states × actions matrix (using typed array for performance)
    this.qTable = new Float64Array(numStates * numActions);
    this.qTable.fill(0); // Initialize to zero (optimistic initialization)

    // Visit counts for exploration bonus
    this.visitCounts = new Uint32Array(numStates * numActions);
    this.visitCounts.fill(0);

    // Statistics
    this.episodeCount = 0;
    this.totalReward = 0;
    this.experienceReplay = [];
    this.maxReplaySize = 1000;

    // Action selection history
    this.actionHistory = new Map();
    for (const actionId of this.actionIds) {
      this.actionHistory.set(actionId, { count: 0, totalReward: 0 });
    }
  }

  /**
   * Convert state object to state index
   *
   * @param {StateSchema} state - State object
   * @returns {number} State index
   * @private
   */
  _stateToIndex(state) {
    const validated = StateSchema.parse(state);

    const driftIdx = this.driftLevels.indexOf(validated.driftLevel);
    const epochIdx = this.epochCounts.indexOf(validated.epochCount);
    const budgetIdx = this.budgetUsed.indexOf(validated.budgetUsed);

    if (driftIdx === -1 || epochIdx === -1 || budgetIdx === -1) {
      throw new Error(`Invalid state: ${JSON.stringify(validated)}`);
    }

    // Flatten 3D state space to 1D index
    return (
      driftIdx * (this.epochCounts.length * this.budgetUsed.length) +
      epochIdx * this.budgetUsed.length +
      budgetIdx
    );
  }

  /**
   * Get Q-value for (state, action) pair
   *
   * @param {StateSchema} state - State
   * @param {string} actionId - Action ID
   * @returns {number} Q-value
   * @private
   */
  _getQValue(state, actionId) {
    const stateIdx = this._stateToIndex(state);
    const actionIdx = this.actionIds.indexOf(actionId);

    if (actionIdx === -1) {
      throw new Error(`Unknown action: ${actionId}`);
    }

    const index = stateIdx * this.actions.length + actionIdx;
    return this.qTable[index];
  }

  /**
   * Set Q-value for (state, action) pair
   *
   * @param {StateSchema} state - State
   * @param {string} actionId - Action ID
   * @param {number} value - New Q-value
   * @returns {void}
   * @private
   */
  _setQValue(state, actionId, value) {
    const stateIdx = this._stateToIndex(state);
    const actionIdx = this.actionIds.indexOf(actionId);

    if (actionIdx === -1) {
      throw new Error(`Unknown action: ${actionId}`);
    }

    const index = stateIdx * this.actions.length + actionIdx;
    this.qTable[index] = value;

    // Update visit count
    this.visitCounts[index]++;
  }

  /**
   * Select action using epsilon-greedy policy
   *
   * @param {StateSchema} state - Current state
   * @param {Object} options - Options
   * @param {boolean} [options.explore=true] - Enable exploration
   * @returns {string} Selected action ID
   */
  selectAction(state, options = {}) {
    const explore = options.explore !== false;

    // Epsilon-greedy: explore with probability epsilon
    if (explore && Math.random() < this.epsilon) {
      // Exploration: random action with UCB bonus for less-visited actions
      return this._selectExploratoryAction(state);
    }

    // Exploitation: best action based on Q-values
    return this._selectBestAction(state);
  }

  /**
   * Select exploratory action using UCB (Upper Confidence Bound)
   *
   * @param {StateSchema} state - Current state
   * @returns {string} Selected action ID
   * @private
   */
  _selectExploratoryAction(state) {
    const stateIdx = this._stateToIndex(state);
    const totalVisits = Array.from(this.visitCounts.slice(
      stateIdx * this.actions.length,
      (stateIdx + 1) * this.actions.length
    )).reduce((a, b) => a + b, 0);

    if (totalVisits === 0) {
      // Never visited this state - random action
      return this.actionIds[Math.floor(Math.random() * this.actionIds.length)];
    }

    // UCB: Q(s,a) + c * sqrt(ln(N) / n(s,a))
    const c = 2.0; // Exploration constant
    let bestAction = this.actionIds[0];
    let bestScore = -Infinity;

    for (const actionId of this.actionIds) {
      const qValue = this._getQValue(state, actionId);
      const actionIdx = this.actionIds.indexOf(actionId);
      const index = stateIdx * this.actions.length + actionIdx;
      const visits = this.visitCounts[index];

      const ucbBonus = visits > 0
        ? c * Math.sqrt(Math.log(totalVisits + 1) / visits)
        : Infinity; // Unvisited actions get priority

      const score = qValue + ucbBonus;

      if (score > bestScore) {
        bestScore = score;
        bestAction = actionId;
      }
    }

    return bestAction;
  }

  /**
   * Select best action (highest Q-value)
   *
   * @param {StateSchema} state - Current state
   * @returns {string} Best action ID
   * @private
   */
  _selectBestAction(state) {
    let bestAction = this.actionIds[0];
    let bestQValue = this._getQValue(state, bestAction);

    for (let i = 1; i < this.actionIds.length; i++) {
      const actionId = this.actionIds[i];
      const qValue = this._getQValue(state, actionId);

      if (qValue > bestQValue) {
        bestQValue = qValue;
        bestAction = actionId;
      }
    }

    return bestAction;
  }

  /**
   * Get maximum Q-value for a state
   *
   * @param {StateSchema} state - State
   * @returns {number} Max Q-value
   * @private
   */
  _getMaxQValue(state) {
    let maxQ = -Infinity;

    for (const actionId of this.actionIds) {
      const qValue = this._getQValue(state, actionId);
      if (qValue > maxQ) {
        maxQ = qValue;
      }
    }

    return maxQ;
  }

  /**
   * Learn from experience using Q-learning update rule
   *
   * Q(s,a) ← Q(s,a) + α[r + γ max_a' Q(s',a') - Q(s,a)]
   *
   * @param {StateSchema} state - Current state s
   * @param {string} actionId - Action taken a
   * @param {number} reward - Reward received r
   * @param {StateSchema} nextState - Next state s'
   * @param {boolean} terminal - Whether episode terminated
   * @returns {void}
   */
  learn(state, actionId, reward, nextState, terminal) {
    // Validate inputs
    StateSchema.parse(state);
    StateSchema.parse(nextState);

    // Store experience for replay
    const experience = ExperienceSchema.parse({
      state,
      action: actionId,
      reward,
      nextState,
      terminal,
    });

    this.experienceReplay.push(experience);
    if (this.experienceReplay.length > this.maxReplaySize) {
      this.experienceReplay.shift(); // Remove oldest
    }

    // Q-learning update
    const currentQ = this._getQValue(state, actionId);
    const maxNextQ = terminal ? 0 : this._getMaxQValue(nextState);

    const tdTarget = reward + this.discountFactor * maxNextQ;
    const tdError = tdTarget - currentQ;
    const newQ = currentQ + this.learningRate * tdError;

    this._setQValue(state, actionId, newQ);

    // Update statistics
    this.totalReward += reward;
    const actionStats = this.actionHistory.get(actionId);
    actionStats.count++;
    actionStats.totalReward += reward;

    // Experience replay: learn from random past experiences
    if (this.experienceReplay.length >= 10) {
      this._replayExperience();
    }
  }

  /**
   * Perform experience replay for stable learning
   *
   * @returns {void}
   * @private
   */
  _replayExperience() {
    const batchSize = Math.min(4, this.experienceReplay.length);

    for (let i = 0; i < batchSize; i++) {
      const idx = Math.floor(Math.random() * this.experienceReplay.length);
      const exp = this.experienceReplay[idx];

      const currentQ = this._getQValue(exp.state, exp.action);
      const maxNextQ = exp.terminal ? 0 : this._getMaxQValue(exp.nextState);

      const tdTarget = exp.reward + this.discountFactor * maxNextQ;
      const tdError = tdTarget - currentQ;
      const newQ = currentQ + this.learningRate * 0.5 * tdError; // Reduced learning rate for replay

      this._setQValue(exp.state, exp.action, newQ);
    }
  }

  /**
   * End episode and decay epsilon
   *
   * @returns {void}
   */
  endEpisode() {
    this.episodeCount++;
    this.epsilon = Math.max(this.minEpsilon, this.epsilon * this.epsilonDecay);
  }

  /**
   * Get policy (best action for each state)
   *
   * @returns {Map<string, string>} Map of state string to best action
   */
  getPolicy() {
    const policy = new Map();

    for (const drift of this.driftLevels) {
      for (const epoch of this.epochCounts) {
        for (const budget of this.budgetUsed) {
          const state = { driftLevel: drift, epochCount: epoch, budgetUsed: budget };
          const stateKey = `${drift}-${epoch}-${budget}`;
          const bestAction = this._selectBestAction(state);

          policy.set(stateKey, bestAction);
        }
      }
    }

    return policy;
  }

  /**
   * Get summary statistics
   *
   * @returns {Object} Summary
   */
  getSummary() {
    const actionStats = [];
    for (const [actionId, stats] of this.actionHistory.entries()) {
      actionStats.push({
        actionId,
        selectionCount: stats.count,
        avgReward: stats.count > 0 ? stats.totalReward / stats.count : 0,
      });
    }

    return {
      episodeCount: this.episodeCount,
      totalReward: this.totalReward,
      avgReward: this.episodeCount > 0 ? this.totalReward / this.episodeCount : 0,
      epsilon: this.epsilon,
      experienceReplaySize: this.experienceReplay.length,
      actionStats: actionStats.sort((a, b) => b.avgReward - a.avgReward),
    };
  }

  /**
   * Export Q-table for persistence
   *
   * @returns {Object} Serializable Q-table
   */
  exportQTable() {
    return {
      qTable: Array.from(this.qTable),
      visitCounts: Array.from(this.visitCounts),
      episodeCount: this.episodeCount,
      totalReward: this.totalReward,
      epsilon: this.epsilon,
      actionHistory: Object.fromEntries(this.actionHistory),
    };
  }

  /**
   * Import Q-table from persistence
   *
   * @param {Object} data - Exported Q-table data
   * @returns {void}
   */
  importQTable(data) {
    if (data.qTable && data.qTable.length === this.qTable.length) {
      this.qTable.set(data.qTable);
    }
    if (data.visitCounts && data.visitCounts.length === this.visitCounts.length) {
      this.visitCounts.set(data.visitCounts);
    }
    if (data.episodeCount !== undefined) {
      this.episodeCount = data.episodeCount;
    }
    if (data.totalReward !== undefined) {
      this.totalReward = data.totalReward;
    }
    if (data.epsilon !== undefined) {
      this.epsilon = data.epsilon;
    }
    if (data.actionHistory) {
      this.actionHistory = new Map(Object.entries(data.actionHistory));
    }
  }

  /**
   * Reset agent (clear Q-table and statistics)
   *
   * @returns {void}
   */
  reset() {
    this.qTable.fill(0);
    this.visitCounts.fill(0);
    this.episodeCount = 0;
    this.totalReward = 0;
    this.experienceReplay = [];
    this.epsilon = 0.1; // Reset to initial
    this.actionHistory.clear();
    for (const actionId of this.actionIds) {
      this.actionHistory.set(actionId, { count: 0, totalReward: 0 });
    }
  }
}

/**
 * Calculate reward for convergence performance
 *
 * @param {Object} metrics - Performance metrics
 * @param {number} metrics.convergenceTime - Time to convergence (epochs)
 * @param {number} metrics.compressionRatio - Compression ratio (0-1)
 * @param {number} [metrics.budgetUsed=0] - Budget used (0-1)
 * @returns {number} Reward value
 */
export function calculateReward(metrics) {
  const { convergenceTime, compressionRatio, budgetUsed = 0 } = metrics;

  // Reward components:
  // 1. Fast convergence: inversely proportional to time
  // 2. High compression: proportional to ratio
  // 3. Budget efficiency: penalty for high budget usage

  const convergenceReward = 10 / Math.max(1, convergenceTime); // 10 / time
  const compressionReward = compressionRatio * 5; // 0-5 points
  const budgetPenalty = -budgetUsed * 2; // -0 to -2 points

  return convergenceReward + compressionReward + budgetPenalty;
}

/**
 * Create a new Q-learning agent
 *
 * @param {Object} config - Configuration
 * @returns {QLearningAgent} New agent instance
 */
export function createQLearningAgent(config) {
  return new QLearningAgent(config);
}

export default { QLearningAgent, createQLearningAgent, calculateReward };
