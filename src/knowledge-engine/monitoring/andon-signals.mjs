/**
 * @file Andon signal system for production monitoring
 * @module knowledge-engine/monitoring/andon-signals
 *
 * @description
 * Provides RED/YELLOW/GREEN status for all critical systems.
 * Implements the Andon cord pattern from Toyota Production System
 * for immediate visibility into system health.
 *
 * Andon States:
 * - GREEN:  Score >= 80, system healthy, all signals nominal
 * - YELLOW: Score 60-79, warning state, investigation needed
 * - RED:    Score < 60, critical state, immediate action required
 */

import { z } from 'zod';
import { EventEmitter } from 'events';

/**
 * Andon signal states
 * @readonly
 * @enum {string}
 */
export const AndonState = {
  GREEN: 'green',
  YELLOW: 'yellow',
  RED: 'red',
};

/**
 * Signal type categories
 * @readonly
 * @enum {string}
 */
export const SignalCategory = {
  VALIDATION: 'validation',
  CI_CD: 'ci_cd',
  PERFORMANCE: 'performance',
  SECURITY: 'security',
  HEALTH: 'health',
};

/**
 * Schema for signal configuration
 */
const SignalConfigSchema = z.object({
  name: z.string().min(1),
  category: z.nativeEnum(SignalCategory),
  description: z.string().optional(),
  weight: z.number().min(0).max(1).default(1),
  thresholds: z
    .object({
      green: z.number().min(0).max(100).default(80),
      yellow: z.number().min(0).max(100).default(60),
    })
    .default({ green: 80, yellow: 60 }),
});

/**
 * Schema for signal state
 */
const SignalStateSchema = z.object({
  name: z.string(),
  category: z.nativeEnum(SignalCategory),
  state: z.nativeEnum(AndonState),
  score: z.number().min(0).max(100),
  message: z.string().optional(),
  timestamp: z.number(),
  metadata: z.record(z.unknown()).optional(),
});

/**
 * Compute Andon signal state from a score
 * @param {number} score - Score value (0-100)
 * @param {Object} [thresholds] - Custom thresholds
 * @param {number} [thresholds.green=80] - Green threshold
 * @param {number} [thresholds.yellow=60] - Yellow threshold
 * @returns {AndonState} Computed signal state
 */
export function computeSignalState(score, thresholds = { green: 80, yellow: 60 }) {
  const normalizedScore = Math.max(0, Math.min(100, score));

  if (normalizedScore >= thresholds.green) {
    return AndonState.GREEN;
  }
  if (normalizedScore >= thresholds.yellow) {
    return AndonState.YELLOW;
  }
  return AndonState.RED;
}

/**
 * Get state priority (higher = more critical)
 * @param {AndonState} state - Signal state
 * @returns {number} Priority value
 */
export function getStatePriority(state) {
  switch (state) {
    case AndonState.RED:
      return 3;
    case AndonState.YELLOW:
      return 2;
    case AndonState.GREEN:
      return 1;
    default:
      return 0;
  }
}

/**
 * Compare two states and return the more critical one
 * @param {AndonState} state1 - First state
 * @param {AndonState} state2 - Second state
 * @returns {AndonState} More critical state
 */
export function getWorstState(state1, state2) {
  return getStatePriority(state1) > getStatePriority(state2) ? state1 : state2;
}

/**
 * Andon Signal Manager
 * Manages all system signals and provides deployment gates
 */
export class AndonSignalManager extends EventEmitter {
  /**
   * Create an Andon Signal Manager
   * @param {Object} [config] - Configuration options
   * @param {Object} [config.thresholds] - Default thresholds
   * @param {number} [config.thresholds.green=80] - Green threshold
   * @param {number} [config.thresholds.yellow=60] - Yellow threshold
   * @param {boolean} [config.strictDeployment=true] - Require all GREEN for deployment
   * @param {string[]} [config.requiredSignals=[]] - Signals required for deployment
   */
  constructor(config = {}) {
    super();

    this.config = {
      thresholds: config.thresholds || { green: 80, yellow: 60 },
      strictDeployment: config.strictDeployment !== false,
      requiredSignals: config.requiredSignals || [],
    };

    /** @type {Map<string, Object>} */
    this.signals = new Map();

    /** @type {Map<string, Object>} */
    this.signalConfigs = new Map();

    /** @type {Array<Function>} */
    this.listeners = [];

    /** @type {Map<string, Object>} */
    this.history = new Map();
  }

  /**
   * Register a signal configuration
   * @param {Object} config - Signal configuration
   * @returns {AndonSignalManager} This instance for chaining
   */
  registerSignal(config) {
    const validated = SignalConfigSchema.parse(config);
    this.signalConfigs.set(validated.name, validated);
    return this;
  }

  /**
   * Update a signal's state
   * @param {string} name - Signal name
   * @param {number} score - Score value (0-100)
   * @param {Object} [options] - Additional options
   * @param {string} [options.message] - Status message
   * @param {Object} [options.metadata] - Additional metadata
   * @returns {Object} Updated signal state
   */
  updateSignal(name, score, options = {}) {
    const config = this.signalConfigs.get(name);
    if (!config) {
      throw new Error(`Signal '${name}' not registered. Call registerSignal first.`);
    }

    const thresholds = config.thresholds || this.config.thresholds;
    const state = computeSignalState(score, thresholds);

    const signalState = SignalStateSchema.parse({
      name,
      category: config.category,
      state,
      score,
      message: options.message,
      timestamp: Date.now(),
      metadata: options.metadata,
    });

    const previousState = this.signals.get(name);
    this.signals.set(name, signalState);

    // Track history
    if (!this.history.has(name)) {
      this.history.set(name, []);
    }
    const historyEntry = this.history.get(name);
    historyEntry.push(signalState);
    // Keep last 100 entries
    if (historyEntry.length > 100) {
      historyEntry.shift();
    }

    // Emit change event if state changed
    if (!previousState || previousState.state !== state) {
      this.emit('signalChange', {
        signal: signalState,
        previousState: previousState?.state,
        newState: state,
      });

      // Notify registered listeners
      for (const listener of this.listeners) {
        try {
          listener({
            signal: signalState,
            previousState: previousState?.state,
            newState: state,
          });
        } catch (err) {
          console.error(`Listener error: ${err.message}`);
        }
      }
    }

    return signalState;
  }

  /**
   * Register validation signals from OTEL validator
   * @param {Object} otelValidator - OTEL validation runner or results
   * @returns {AndonSignalManager} This instance for chaining
   */
  registerValidationSignals(otelValidator) {
    // Default validation signals (7 core features)
    const validationSignals = [
      {
        name: 'knowledge-engine-core',
        category: SignalCategory.VALIDATION,
        weight: 0.3,
        description: 'Core knowledge engine operations',
      },
      {
        name: 'knowledge-hooks-api',
        category: SignalCategory.VALIDATION,
        weight: 0.2,
        description: 'Knowledge hooks API',
      },
      {
        name: 'policy-packs',
        category: SignalCategory.VALIDATION,
        weight: 0.15,
        description: 'Policy pack system',
      },
      {
        name: 'lockchain-integrity',
        category: SignalCategory.VALIDATION,
        weight: 0.15,
        description: 'Cryptographic audit trail',
      },
      {
        name: 'transaction-manager',
        category: SignalCategory.VALIDATION,
        weight: 0.1,
        description: 'ACID transaction guarantees',
      },
      {
        name: 'browser-compatibility',
        category: SignalCategory.VALIDATION,
        weight: 0.1,
        description: 'Browser compatibility layer',
      },
      {
        name: 'isolated-vm-security',
        category: SignalCategory.VALIDATION,
        weight: 0.05,
        description: 'Sandbox security',
      },
    ];

    for (const signal of validationSignals) {
      this.registerSignal(signal);
    }

    // If otelValidator has results, update signals
    if (otelValidator?.features) {
      for (const feature of otelValidator.features) {
        if (this.signalConfigs.has(feature.name)) {
          this.updateSignal(feature.name, feature.score, {
            message: feature.passed
              ? 'Validation passed'
              : `${feature.violations?.length || 0} violations`,
            metadata: {
              passed: feature.passed,
              violations: feature.violations,
              metrics: feature.metrics,
            },
          });
        }
      }
    }

    return this;
  }

  /**
   * Register CI/CD signals from GitHub Actions or similar
   * @param {Object} config - CI/CD configuration
   * @param {string} [config.workflowUrl] - GitHub Actions workflow URL
   * @param {Object[]} [config.jobs] - Job results
   * @returns {AndonSignalManager} This instance for chaining
   */
  registerCISignals(config = {}) {
    // Default CI/CD signals (9 pipeline stages)
    const ciSignals = [
      {
        name: 'ci-lint',
        category: SignalCategory.CI_CD,
        weight: 0.1,
        description: 'Code linting',
      },
      {
        name: 'ci-typecheck',
        category: SignalCategory.CI_CD,
        weight: 0.1,
        description: 'Type checking',
      },
      {
        name: 'ci-unit-tests',
        category: SignalCategory.CI_CD,
        weight: 0.2,
        description: 'Unit test suite',
      },
      {
        name: 'ci-integration-tests',
        category: SignalCategory.CI_CD,
        weight: 0.15,
        description: 'Integration tests',
      },
      {
        name: 'ci-e2e-tests',
        category: SignalCategory.CI_CD,
        weight: 0.15,
        description: 'End-to-end tests',
      },
      {
        name: 'ci-security-scan',
        category: SignalCategory.CI_CD,
        weight: 0.1,
        description: 'Security scanning',
      },
      {
        name: 'ci-build',
        category: SignalCategory.CI_CD,
        weight: 0.1,
        description: 'Build process',
      },
      {
        name: 'ci-coverage',
        category: SignalCategory.CI_CD,
        weight: 0.05,
        description: 'Code coverage',
      },
      {
        name: 'ci-performance',
        category: SignalCategory.CI_CD,
        weight: 0.05,
        description: 'Performance benchmarks',
      },
    ];

    for (const signal of ciSignals) {
      this.registerSignal(signal);
    }

    // Parse job results if provided
    if (config.jobs && Array.isArray(config.jobs)) {
      for (const job of config.jobs) {
        const signalName = this.mapJobToSignal(job.name || job.id);
        if (signalName && this.signalConfigs.has(signalName)) {
          const score = this.computeJobScore(job);
          this.updateSignal(signalName, score, {
            message: job.conclusion || job.status,
            metadata: {
              jobId: job.id,
              status: job.status,
              conclusion: job.conclusion,
              duration: job.duration,
              url: job.url,
            },
          });
        }
      }
    }

    return this;
  }

  /**
   * Map GitHub Actions job name to signal name
   * @param {string} jobName - Job name
   * @returns {string|null} Signal name or null
   * @private
   */
  mapJobToSignal(jobName) {
    // Normalize: lowercase and replace hyphens/underscores with nothing for matching
    const jobNameLower = (jobName || '').toLowerCase();
    const jobNameNormalized = jobNameLower.replace(/[-_\s]/g, '');

    // Order matters: more specific patterns first
    const mappings = [
      { patterns: ['typecheck'], signal: 'ci-typecheck' },
      { patterns: ['unittest'], signal: 'ci-unit-tests' },
      { patterns: ['integration'], signal: 'ci-integration-tests' },
      { patterns: ['e2e', 'endtoend'], signal: 'ci-e2e-tests' },
      { patterns: ['security'], signal: 'ci-security-scan' },
      {
        patterns: ['performance', 'benchmark', 'perf'],
        signal: 'ci-performance',
      },
      { patterns: ['coverage'], signal: 'ci-coverage' },
      { patterns: ['build'], signal: 'ci-build' },
      { patterns: ['lint'], signal: 'ci-lint' },
      { patterns: ['test'], signal: 'ci-unit-tests' },
      { patterns: ['type'], signal: 'ci-typecheck' },
    ];

    for (const { patterns, signal } of mappings) {
      for (const pattern of patterns) {
        if (jobNameNormalized.includes(pattern) || jobNameLower.includes(pattern)) {
          return signal;
        }
      }
    }

    return null;
  }

  /**
   * Compute score from job result
   * @param {Object} job - Job result
   * @returns {number} Score (0-100)
   * @private
   */
  computeJobScore(job) {
    if (job.score !== undefined) {
      return job.score;
    }

    const conclusion = (job.conclusion || '').toLowerCase();
    const status = (job.status || '').toLowerCase();

    if (conclusion === 'success' || status === 'completed') {
      return 100;
    }
    if (conclusion === 'failure' || status === 'failed') {
      return 0;
    }
    if (conclusion === 'cancelled' || status === 'cancelled') {
      return 30;
    }
    if (conclusion === 'skipped') {
      return 50;
    }
    if (status === 'in_progress' || status === 'queued') {
      return 70;
    }

    return 50; // Unknown state
  }

  /**
   * Register performance signals
   * @param {Object} [performanceMetrics] - Performance metrics from ObservabilityManager
   * @returns {AndonSignalManager} This instance for chaining
   */
  registerPerformanceSignals(performanceMetrics = null) {
    const perfSignals = [
      {
        name: 'perf-latency',
        category: SignalCategory.PERFORMANCE,
        weight: 0.3,
        description: 'Response latency P95',
      },
      {
        name: 'perf-throughput',
        category: SignalCategory.PERFORMANCE,
        weight: 0.25,
        description: 'Transaction throughput',
      },
      {
        name: 'perf-error-rate',
        category: SignalCategory.PERFORMANCE,
        weight: 0.25,
        description: 'Error rate',
      },
      {
        name: 'perf-memory',
        category: SignalCategory.PERFORMANCE,
        weight: 0.2,
        description: 'Memory usage',
      },
    ];

    for (const signal of perfSignals) {
      this.registerSignal(signal);
    }

    if (performanceMetrics) {
      this.updatePerformanceSignals(performanceMetrics);
    }

    return this;
  }

  /**
   * Update performance signals from metrics
   * @param {Object} metrics - Performance metrics
   */
  updatePerformanceSignals(metrics) {
    // Latency score: 100ms = 100, 500ms = 80, 1000ms = 60, 2000ms+ = 0
    if (metrics.transactionLatency) {
      const p95 = metrics.transactionLatency.p95 || 0;
      const latencyScore = Math.max(0, Math.min(100, 100 - p95 / 20));
      this.updateSignal('perf-latency', latencyScore, {
        message: `P95: ${p95}ms`,
        metadata: metrics.transactionLatency,
      });
    }

    // Error rate score: 0% = 100, 1% = 80, 5% = 60, 10%+ = 0
    if (metrics.errorRate !== undefined) {
      const errorPct = metrics.errorRate * 100;
      const errorScore = Math.max(0, Math.min(100, 100 - errorPct * 10));
      this.updateSignal('perf-error-rate', errorScore, {
        message: `Error rate: ${errorPct.toFixed(2)}%`,
        metadata: { errorRate: metrics.errorRate },
      });
    }

    // Throughput score: relative to expected baseline
    if (metrics.hookExecutionRate !== undefined) {
      const rate = metrics.hookExecutionRate;
      // Assume 10 ops/min is baseline
      const throughputScore = Math.min(100, (rate / 10) * 100);
      this.updateSignal('perf-throughput', throughputScore, {
        message: `${rate} ops/min`,
        metadata: { rate },
      });
    }

    // Memory score: based on heap usage
    if (metrics.memoryUsage) {
      const heapUsed = metrics.memoryUsage.heapUsed || 0;
      const heapTotal = metrics.memoryUsage.heapTotal || 1;
      const memPct = (heapUsed / heapTotal) * 100;
      const memScore = Math.max(0, 100 - memPct);
      this.updateSignal('perf-memory', memScore, {
        message: `Heap: ${(heapUsed / 1024 / 1024).toFixed(1)}MB / ${(heapTotal / 1024 / 1024).toFixed(1)}MB`,
        metadata: metrics.memoryUsage,
      });
    }
  }

  /**
   * Check if deployment is ready
   * @returns {Object} Deployment readiness result
   */
  isDeploymentReady() {
    const allSignals = this.getAllSignals();

    if (allSignals.length === 0) {
      return {
        ready: false,
        reason: 'No signals registered',
        signals: [],
        summary: { green: 0, yellow: 0, red: 0 },
      };
    }

    const summary = { green: 0, yellow: 0, red: 0 };
    const failures = [];
    const warnings = [];

    for (const signal of allSignals) {
      summary[signal.state]++;

      if (signal.state === AndonState.RED) {
        failures.push(signal);
      } else if (signal.state === AndonState.YELLOW) {
        warnings.push(signal);
      }
    }

    // Check required signals
    const missingRequired = this.config.requiredSignals.filter(name => !this.signals.has(name));

    if (missingRequired.length > 0) {
      return {
        ready: false,
        reason: `Missing required signals: ${missingRequired.join(', ')}`,
        signals: allSignals,
        summary,
        missingRequired,
      };
    }

    // Strict mode: all must be GREEN
    if (this.config.strictDeployment) {
      const ready = failures.length === 0 && warnings.length === 0;
      return {
        ready,
        reason: ready
          ? 'All signals GREEN'
          : failures.length > 0
            ? `${failures.length} RED signal(s): ${failures.map(s => s.name).join(', ')}`
            : `${warnings.length} YELLOW signal(s): ${warnings.map(s => s.name).join(', ')}`,
        signals: allSignals,
        summary,
        failures,
        warnings,
      };
    }

    // Lenient mode: no RED signals
    const ready = failures.length === 0;
    return {
      ready,
      reason: ready
        ? warnings.length > 0
          ? `${warnings.length} warning(s), but deployable`
          : 'All signals healthy'
        : `${failures.length} RED signal(s): ${failures.map(s => s.name).join(', ')}`,
      signals: allSignals,
      summary,
      failures,
      warnings,
    };
  }

  /**
   * Get all registered signals with their current states
   * @returns {Array<Object>} All signal states
   */
  getAllSignals() {
    return Array.from(this.signals.values());
  }

  /**
   * Get signals by category
   * @param {SignalCategory} category - Signal category
   * @returns {Array<Object>} Signals in category
   */
  getSignalsByCategory(category) {
    return this.getAllSignals().filter(s => s.category === category);
  }

  /**
   * Get the overall system state (worst signal)
   * @returns {AndonState} Overall system state
   */
  getOverallState() {
    const signals = this.getAllSignals();
    if (signals.length === 0) {
      return AndonState.GREEN;
    }

    return signals.reduce((worst, signal) => getWorstState(worst, signal.state), AndonState.GREEN);
  }

  /**
   * Get weighted overall score
   * @returns {number} Weighted score (0-100)
   */
  getWeightedScore() {
    let totalWeight = 0;
    let weightedSum = 0;

    for (const [name, signal] of this.signals) {
      const config = this.signalConfigs.get(name);
      const weight = config?.weight || 1;
      totalWeight += weight;
      weightedSum += signal.score * weight;
    }

    return totalWeight > 0 ? Math.round(weightedSum / totalWeight) : 0;
  }

  /**
   * Subscribe to signal changes
   * @param {Function} callback - Callback function
   * @returns {Function} Unsubscribe function
   */
  onSignalChange(callback) {
    if (typeof callback !== 'function') {
      throw new TypeError('Callback must be a function');
    }

    this.listeners.push(callback);

    return () => {
      const index = this.listeners.indexOf(callback);
      if (index > -1) {
        this.listeners.splice(index, 1);
      }
    };
  }

  /**
   * Get signal history
   * @param {string} name - Signal name
   * @param {number} [limit=10] - Maximum entries to return
   * @returns {Array<Object>} Signal history
   */
  getSignalHistory(name, limit = 10) {
    const history = this.history.get(name) || [];
    return history.slice(-limit);
  }

  /**
   * Clear all signals
   */
  clear() {
    this.signals.clear();
    this.signalConfigs.clear();
    this.history.clear();
    this.listeners = [];
  }

  /**
   * Generate a dashboard-friendly summary
   * @returns {Object} Dashboard summary
   */
  getDashboardSummary() {
    const _signals = this.getAllSignals();
    const deployment = this.isDeploymentReady();

    return {
      overallState: this.getOverallState(),
      overallScore: this.getWeightedScore(),
      deployment: {
        ready: deployment.ready,
        reason: deployment.reason,
      },
      summary: deployment.summary,
      byCategory: {
        validation: this.getSignalsByCategory(SignalCategory.VALIDATION),
        cicd: this.getSignalsByCategory(SignalCategory.CI_CD),
        performance: this.getSignalsByCategory(SignalCategory.PERFORMANCE),
        security: this.getSignalsByCategory(SignalCategory.SECURITY),
        health: this.getSignalsByCategory(SignalCategory.HEALTH),
      },
      timestamp: Date.now(),
    };
  }
}

/**
 * Create an Andon Signal Manager instance
 * @param {Object} [config] - Configuration
 * @returns {AndonSignalManager} Manager instance
 */
export function createAndonSignalManager(config = {}) {
  return new AndonSignalManager(config);
}

/**
 * Default Andon Signal Manager instance
 */
export const defaultAndonSignalManager = createAndonSignalManager();
