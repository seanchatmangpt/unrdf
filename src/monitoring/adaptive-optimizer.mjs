/**
 * @fileoverview Adaptive Optimizer - Auto-tune system parameters
 * @module monitoring/adaptive-optimizer
 *
 * @description
 * Adaptive optimization based on real-time metrics:
 * - Auto-tune convergence epsilon (ε) based on drift patterns
 * - Dynamic budget allocation (B) based on workload
 * - Agent scaling (add/remove agents dynamically)
 * - Load balancing and resource optimization
 */

/**
 * Optimization strategy
 * @typedef {object} OptimizationStrategy
 * @property {string} name - Strategy name
 * @property {Function} evaluate - Evaluate if strategy should trigger
 * @property {Function} apply - Apply the optimization
 * @property {number} cooldown - Cooldown period in ms
 * @property {number} lastApplied - Last application timestamp
 */

/**
 * Adaptive optimizer for system parameters
 */
export class AdaptiveOptimizer {
  /**
   * Create a new adaptive optimizer
   * @param {object} metricsCollector - MetricsCollector instance
   * @param {object} options - Configuration options
   * @param {number} options.evaluationInterval - Evaluation interval in ms (default: 10000)
   * @param {number} options.minCooldown - Minimum cooldown between optimizations (default: 30000)
   */
  constructor(metricsCollector, options = {}) {
    this.metrics = metricsCollector;
    this.evaluationInterval = options.evaluationInterval || 10000;
    this.minCooldown = options.minCooldown || 30000;

    /** @type {Map<string, OptimizationStrategy>} */
    this.strategies = new Map();

    /** @type {number|null} */
    this.intervalId = null;

    /** @type {object[]} */
    this.optimizationHistory = [];

    /** @type {object} */
    this.parameters = {
      epsilon: 0.01, // Convergence threshold
      budget: 1000, // Operation budget
      agentCount: 10, // Active agents
      batchSize: 100, // Batch size
      compressionThreshold: 0.8, // Compression trigger
    };

    // Register default strategies
    this.registerDefaultStrategies();
  }

  /**
   * Register default optimization strategies
   */
  registerDefaultStrategies() {
    // Strategy 1: Tune epsilon based on drift
    this.registerStrategy({
      name: 'tune-epsilon',
      evaluate: () => {
        const snapshot = this.metrics.getCurrentSnapshot();
        if (!snapshot) return false;

        // Check latency variance
        const latencyStdDev = this.calculateStdDev(
          this.metrics.getAllSnapshots().slice(-30).map(s => s.latency.p95)
        );

        // High variance → increase epsilon (faster convergence)
        // Low variance → decrease epsilon (better accuracy)
        return latencyStdDev > 50; // More than 50ms variance
      },
      apply: () => {
        const snapshot = this.metrics.getCurrentSnapshot();
        const latencyStdDev = this.calculateStdDev(
          this.metrics.getAllSnapshots().slice(-30).map(s => s.latency.p95)
        );

        if (latencyStdDev > 50) {
          // High variance - increase epsilon
          this.parameters.epsilon = Math.min(0.1, this.parameters.epsilon * 1.5);
          return {
            success: true,
            message: `Increased epsilon to ${this.parameters.epsilon.toFixed(4)} due to high latency variance`,
            oldValue: this.parameters.epsilon / 1.5,
            newValue: this.parameters.epsilon,
          };
        } else if (latencyStdDev < 10) {
          // Low variance - decrease epsilon for accuracy
          this.parameters.epsilon = Math.max(0.001, this.parameters.epsilon * 0.8);
          return {
            success: true,
            message: `Decreased epsilon to ${this.parameters.epsilon.toFixed(4)} for better accuracy`,
            oldValue: this.parameters.epsilon / 0.8,
            newValue: this.parameters.epsilon,
          };
        }

        return { success: false, message: 'No epsilon adjustment needed' };
      },
      cooldown: 60000,
      lastApplied: 0,
    });

    // Strategy 2: Dynamic budget allocation
    this.registerStrategy({
      name: 'allocate-budget',
      evaluate: () => {
        const throughput = this.metrics.getAverageThroughput(30000);
        const currentSnapshot = this.metrics.getCurrentSnapshot();

        if (!currentSnapshot) return false;

        // Adjust budget based on throughput and queue depth
        const queueDepth = this.getTotalQueueDepth();
        return queueDepth > 100 || throughput < 10;
      },
      apply: () => {
        const queueDepth = this.getTotalQueueDepth();
        const throughput = this.metrics.getAverageThroughput(30000);

        let adjustment = 0;
        let reason = '';

        if (queueDepth > 100) {
          // High queue - increase budget
          adjustment = 1.3;
          reason = `high queue depth (${queueDepth})`;
        } else if (throughput < 10) {
          // Low throughput - increase budget
          adjustment = 1.2;
          reason = `low throughput (${throughput.toFixed(2)} ops/sec)`;
        } else if (throughput > 100 && queueDepth < 10) {
          // System is efficient - can reduce budget
          adjustment = 0.9;
          reason = 'system running efficiently';
        }

        if (adjustment !== 0) {
          const oldBudget = this.parameters.budget;
          this.parameters.budget = Math.floor(this.parameters.budget * adjustment);
          this.parameters.budget = Math.max(100, Math.min(10000, this.parameters.budget));

          return {
            success: true,
            message: `Adjusted budget from ${oldBudget} to ${this.parameters.budget} due to ${reason}`,
            oldValue: oldBudget,
            newValue: this.parameters.budget,
          };
        }

        return { success: false, message: 'No budget adjustment needed' };
      },
      cooldown: 30000,
      lastApplied: 0,
    });

    // Strategy 3: Agent scaling
    this.registerStrategy({
      name: 'scale-agents',
      evaluate: () => {
        const snapshot = this.metrics.getCurrentSnapshot();
        if (!snapshot) return false;

        const cpuPercent = snapshot.cpu.percentageUser + snapshot.cpu.percentageSystem;
        const memPercent = (snapshot.memory.heapUsed / snapshot.memory.heapTotal) * 100;
        const queueDepth = this.getTotalQueueDepth();

        // Scale up if high load
        // Scale down if low load
        return (cpuPercent > 80 && queueDepth > 50) || (cpuPercent < 20 && queueDepth < 5);
      },
      apply: () => {
        const snapshot = this.metrics.getCurrentSnapshot();
        const cpuPercent = snapshot.cpu.percentageUser + snapshot.cpu.percentageSystem;
        const queueDepth = this.getTotalQueueDepth();

        const oldCount = this.parameters.agentCount;

        if (cpuPercent > 80 && queueDepth > 50) {
          // Scale up
          this.parameters.agentCount = Math.min(20, this.parameters.agentCount + 2);
          return {
            success: true,
            message: `Scaled up agents from ${oldCount} to ${this.parameters.agentCount} due to high load`,
            oldValue: oldCount,
            newValue: this.parameters.agentCount,
          };
        } else if (cpuPercent < 20 && queueDepth < 5 && this.parameters.agentCount > 5) {
          // Scale down
          this.parameters.agentCount = Math.max(5, this.parameters.agentCount - 1);
          return {
            success: true,
            message: `Scaled down agents from ${oldCount} to ${this.parameters.agentCount} due to low load`,
            oldValue: oldCount,
            newValue: this.parameters.agentCount,
          };
        }

        return { success: false, message: 'No agent scaling needed' };
      },
      cooldown: 60000,
      lastApplied: 0,
    });

    // Strategy 4: Batch size optimization
    this.registerStrategy({
      name: 'optimize-batch-size',
      evaluate: () => {
        const latency = this.metrics.getCurrentSnapshot()?.latency;
        if (!latency) return false;

        // Adjust batch size based on latency
        return latency.p95 > 100 || latency.p95 < 10;
      },
      apply: () => {
        const latency = this.metrics.getCurrentSnapshot()?.latency;
        const oldSize = this.parameters.batchSize;

        if (latency.p95 > 100) {
          // High latency - reduce batch size
          this.parameters.batchSize = Math.max(10, Math.floor(this.parameters.batchSize * 0.8));
          return {
            success: true,
            message: `Reduced batch size from ${oldSize} to ${this.parameters.batchSize} due to high latency`,
            oldValue: oldSize,
            newValue: this.parameters.batchSize,
          };
        } else if (latency.p95 < 10 && this.parameters.batchSize < 500) {
          // Low latency - increase batch size
          this.parameters.batchSize = Math.min(500, Math.floor(this.parameters.batchSize * 1.2));
          return {
            success: true,
            message: `Increased batch size from ${oldSize} to ${this.parameters.batchSize} for better throughput`,
            oldValue: oldSize,
            newValue: this.parameters.batchSize,
          };
        }

        return { success: false, message: 'No batch size adjustment needed' };
      },
      cooldown: 45000,
      lastApplied: 0,
    });
  }

  /**
   * Register a custom optimization strategy
   * @param {OptimizationStrategy} strategy - Strategy to register
   */
  registerStrategy(strategy) {
    this.strategies.set(strategy.name, {
      ...strategy,
      lastApplied: 0,
    });
  }

  /**
   * Start the optimizer
   */
  start() {
    if (this.intervalId !== null) return;

    this.intervalId = setInterval(() => {
      this.evaluate();
    }, this.evaluationInterval);

    console.log('[AdaptiveOptimizer] Started');
  }

  /**
   * Stop the optimizer
   */
  stop() {
    if (this.intervalId !== null) {
      clearInterval(this.intervalId);
      this.intervalId = null;
      console.log('[AdaptiveOptimizer] Stopped');
    }
  }

  /**
   * Evaluate all strategies
   */
  evaluate() {
    const now = Date.now();

    for (const [name, strategy] of this.strategies) {
      // Check cooldown
      if (now - strategy.lastApplied < strategy.cooldown) {
        continue;
      }

      // Evaluate strategy
      if (strategy.evaluate()) {
        const result = strategy.apply();

        if (result.success) {
          strategy.lastApplied = now;

          // Record in history
          this.optimizationHistory.push({
            timestamp: now,
            strategy: name,
            result,
          });

          // Keep history bounded
          if (this.optimizationHistory.length > 100) {
            this.optimizationHistory.shift();
          }

          console.log(`[AdaptiveOptimizer] ${result.message}`);
        }
      }
    }
  }

  /**
   * Get current parameters
   * @returns {object}
   */
  getParameters() {
    return { ...this.parameters };
  }

  /**
   * Set parameter value
   * @param {string} name - Parameter name
   * @param {*} value - New value
   */
  setParameter(name, value) {
    if (this.parameters.hasOwnProperty(name)) {
      this.parameters[name] = value;
    }
  }

  /**
   * Get optimization history
   * @returns {object[]}
   */
  getHistory() {
    return [...this.optimizationHistory];
  }

  /**
   * Calculate standard deviation
   * @param {number[]} values - Array of values
   * @returns {number}
   */
  calculateStdDev(values) {
    if (values.length === 0) return 0;

    const mean = values.reduce((sum, v) => sum + v, 0) / values.length;
    const squaredDiffs = values.map(v => Math.pow(v - mean, 2));
    const variance = squaredDiffs.reduce((sum, v) => sum + v, 0) / values.length;

    return Math.sqrt(variance);
  }

  /**
   * Get total queue depth across all agents
   * @returns {number}
   */
  getTotalQueueDepth() {
    const agentMetrics = this.metrics.getAllAgentMetrics();
    let total = 0;

    for (const metrics of agentMetrics.values()) {
      total += metrics.tasksQueued || 0;
    }

    return total;
  }

  /**
   * Export optimizer state
   * @returns {object}
   */
  toJSON() {
    return {
      parameters: this.parameters,
      history: this.optimizationHistory,
      strategies: Array.from(this.strategies.keys()),
    };
  }
}
