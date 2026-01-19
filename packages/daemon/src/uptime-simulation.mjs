/**
 * @file Uptime Simulation Module for UNRDF Daemon
 * @module @unrdf/daemon/uptime-simulation
 * @description Chaos engineering and uptime simulation with network partitions,
 * resource constraints, and service degradation patterns.
 */

import { z } from 'zod';
import { EventEmitter } from 'events';

// ============================================================================
// Zod Schemas
// ============================================================================

export const FailureTypeSchema = z.enum([
  'crash', 'hang', 'slow_response', 'error_response', 'partial_failure', 'cascade_failure',
]);

export const ResourceConstraintTypeSchema = z.enum([
  'cpu_pressure', 'memory_pressure', 'disk_io_throttle',
  'network_latency', 'network_partition', 'bandwidth_limit',
]);

export const DegradationLevelSchema = z.enum(['none', 'minor', 'moderate', 'severe', 'critical']);

export const ChaosEventSchema = z.object({
  id: z.string().uuid(),
  type: FailureTypeSchema,
  targetService: z.string().min(1),
  probability: z.number().min(0).max(1).default(0.1),
  durationMs: z.number().int().min(0).default(5000),
  metadata: z.record(z.string(), z.any()).default({}),
});

export const ResourceConstraintSchema = z.object({
  type: ResourceConstraintTypeSchema,
  intensity: z.number().min(0).max(1).default(0.5),
  durationMs: z.number().int().min(0).default(10000),
  targetPercentage: z.number().min(0).max(100).optional(),
});

export const NetworkPartitionSchema = z.object({
  partitionId: z.string().uuid(),
  isolatedNodes: z.array(z.string()).min(1),
  durationMs: z.number().int().min(0).default(30000),
  allowPartialConnectivity: z.boolean().default(false),
  packetLossRate: z.number().min(0).max(1).default(1),
});

export const DegradationPatternSchema = z.object({
  patternId: z.string().uuid(),
  serviceName: z.string().min(1),
  level: DegradationLevelSchema,
  latencyMultiplier: z.number().min(1).default(1),
  errorRate: z.number().min(0).max(1).default(0),
  throughputReduction: z.number().min(0).max(1).default(0),
});

export const UptimeMetricsSchema = z.object({
  totalUptimeMs: z.number().int().min(0),
  totalDowntimeMs: z.number().int().min(0),
  uptimePercentage: z.number().min(0).max(100),
  mtbfMs: z.number().min(0),
  mttrMs: z.number().min(0),
  failureCount: z.number().int().min(0),
  recoveryCount: z.number().int().min(0),
  lastFailureAt: z.date().nullable(),
  lastRecoveryAt: z.date().nullable(),
});

export const SimulatorConfigSchema = z.object({
  serviceName: z.string().min(1).default('unrdf-daemon'),
  enableChaos: z.boolean().default(false),
  chaosIntensity: z.number().min(0).max(1).default(0.1),
  metricsIntervalMs: z.number().int().min(100).default(1000),
  maxHistorySize: z.number().int().min(10).max(100000).default(10000),
  autoRecoveryEnabled: z.boolean().default(true),
  autoRecoveryDelayMs: z.number().int().min(0).default(5000),
});

/** @private Lightweight OTEL span tracker */
class SpanTracker {
  constructor(serviceName) {
    this.serviceName = serviceName;
    this.spans = [];
    this.activeSpans = new Map();
  }

  startSpan(name, attributes = {}) {
    const spanId = `uptime-${Date.now()}-${Math.random().toString(36).slice(2, 11)}`;
    this.activeSpans.set(spanId, {
      id: spanId, name, service: this.serviceName, startTime: Date.now(),
      attributes, events: [], status: 'unset',
    });
    return spanId;
  }

  addEvent(spanId, eventName, attributes = {}) {
    const span = this.activeSpans.get(spanId);
    if (span) span.events.push({ name: eventName, timestamp: Date.now(), attributes });
  }

  endSpan(spanId, status = 'ok') {
    const span = this.activeSpans.get(spanId);
    if (span) {
      span.endTime = Date.now();
      span.duration = span.endTime - span.startTime;
      span.status = status;
      this.spans.push(span);
      this.activeSpans.delete(spanId);
      if (this.spans.length > 10000) this.spans.shift();
    }
  }

  getSpans() { return [...this.spans]; }
}

/**
 * Comprehensive uptime simulation with chaos engineering capabilities
 * @fires UptimeSimulator#stateChange - When system state changes
 * @fires UptimeSimulator#failure - When a failure is injected
 * @fires UptimeSimulator#recovery - When system recovers
 * @fires UptimeSimulator#metricsUpdate - When metrics are updated
 */
export class UptimeSimulator extends EventEmitter {
  /**
   * Create uptime simulator instance
   * @param {Object} config - Simulator configuration
   * @param {string} [config.serviceName='unrdf-daemon'] - Service name for tracing
   * @param {boolean} [config.enableChaos=false] - Enable chaos engineering
   * @param {number} [config.chaosIntensity=0.1] - Chaos intensity (0-1)
   * @param {number} [config.metricsIntervalMs=1000] - Metrics collection interval
   * @param {number} [config.maxHistorySize=10000] - Maximum event history size
   * @param {boolean} [config.autoRecoveryEnabled=true] - Enable auto-recovery
   * @param {number} [config.autoRecoveryDelayMs=5000] - Auto-recovery delay
   */
  constructor(config = {}) {
    super();
    this.config = SimulatorConfigSchema.parse(config);
    this.spanTracker = new SpanTracker(this.config.serviceName);
    this.state = {
      isHealthy: true, currentDegradation: 'none',
      activeFailures: new Map(), activeConstraints: new Map(),
      activePartitions: new Map(), activeDegradations: new Map(),
    };
    this.metrics = {
      startTime: Date.now(), totalUptimeMs: 0, totalDowntimeMs: 0,
      failureCount: 0, recoveryCount: 0,
      lastFailureAt: null, lastRecoveryAt: null,
      failureHistory: [], recoveryHistory: [],
    };
    this.timers = new Map();
    this.metricsInterval = null;
    this.running = false;
  }

  /** Start the uptime simulator @returns {UptimeSimulator} */
  start() {
    if (this.running) return this;
    const spanId = this.spanTracker.startSpan('simulator.start');
    this.running = true;
    this.metrics.startTime = Date.now();
    this.metricsInterval = setInterval(() => this._updateMetrics(), this.config.metricsIntervalMs);
    this.spanTracker.endSpan(spanId, 'ok');
    this.emit('stateChange', { type: 'started', timestamp: new Date() });
    return this;
  }

  /** Stop the uptime simulator @returns {UptimeSimulator} */
  stop() {
    if (!this.running) return this;
    const spanId = this.spanTracker.startSpan('simulator.stop');
    if (this.metricsInterval) { clearInterval(this.metricsInterval); this.metricsInterval = null; }
    for (const timerId of this.timers.values()) clearTimeout(timerId);
    this.timers.clear();
    ['activeFailures', 'activeConstraints', 'activePartitions', 'activeDegradations']
      .forEach(k => this.state[k].clear());
    this.running = false;
    this.spanTracker.endSpan(spanId, 'ok');
    this.emit('stateChange', { type: 'stopped', timestamp: new Date() });
    return this;
  }

  /**
   * Inject a random failure based on chaos configuration
   * @param {Object} [options] - Failure options
   * @returns {Object|null} Injected failure or null
   */
  injectRandomFailure(options = {}) {
    const spanId = this.spanTracker.startSpan('chaos.injectRandomFailure', options);
    const probability = options.probability ?? this.config.chaosIntensity;
    if (Math.random() > probability) { this.spanTracker.endSpan(spanId, 'ok'); return null; }
    const failureTypes = FailureTypeSchema.options;
    const failure = ChaosEventSchema.parse({
      id: crypto.randomUUID(),
      type: failureTypes[Math.floor(Math.random() * failureTypes.length)],
      targetService: options.targetService ?? this.config.serviceName,
      probability, durationMs: Math.floor(Math.random() * 10000) + 1000,
      metadata: { injectedAt: Date.now(), source: 'random' },
    });
    this._applyFailure(failure);
    this.spanTracker.endSpan(spanId, 'ok');
    return failure;
  }

  /**
   * Inject a specific failure
   * @param {Object} failureConfig - Failure configuration
   * @returns {Object} Applied failure
   */
  injectFailure(failureConfig) {
    const spanId = this.spanTracker.startSpan('chaos.injectFailure', failureConfig);
    const failure = ChaosEventSchema.parse({ id: crypto.randomUUID(), ...failureConfig });
    this._applyFailure(failure);
    this.spanTracker.endSpan(spanId, 'ok');
    return failure;
  }

  /** @private */
  _applyFailure(failure) {
    this.state.activeFailures.set(failure.id, failure);
    this.state.isHealthy = false;
    this.metrics.failureCount++;
    this.metrics.lastFailureAt = new Date();
    this.metrics.failureHistory.push({
      id: failure.id, type: failure.type, timestamp: new Date(), durationMs: failure.durationMs,
    });
    if (this.metrics.failureHistory.length > this.config.maxHistorySize) this.metrics.failureHistory.shift();
    this.emit('failure', { failure, timestamp: new Date() });
    this.emit('stateChange', { type: 'failure', failure, timestamp: new Date() });
    if (failure.durationMs > 0) {
      const timerId = setTimeout(() => this.recoverFromFailure(failure.id), failure.durationMs);
      this.timers.set(`failure-${failure.id}`, timerId);
    }
  }

  /**
   * Recover from a specific failure
   * @param {string} failureId - Failure ID
   * @returns {boolean} Success
   */
  recoverFromFailure(failureId) {
    const spanId = this.spanTracker.startSpan('chaos.recoverFromFailure', { failureId });
    if (!this.state.activeFailures.has(failureId)) { this.spanTracker.endSpan(spanId, 'ok'); return false; }
    this.state.activeFailures.delete(failureId);
    const timerKey = `failure-${failureId}`;
    if (this.timers.has(timerKey)) { clearTimeout(this.timers.get(timerKey)); this.timers.delete(timerKey); }
    if (this.state.activeFailures.size === 0 && this.state.activeConstraints.size === 0) this.state.isHealthy = true;
    this.metrics.recoveryCount++;
    this.metrics.lastRecoveryAt = new Date();
    this.metrics.recoveryHistory.push({ failureId, timestamp: new Date() });
    if (this.metrics.recoveryHistory.length > this.config.maxHistorySize) this.metrics.recoveryHistory.shift();
    this.emit('recovery', { failureId, timestamp: new Date() });
    this.emit('stateChange', { type: 'recovery', failureId, timestamp: new Date() });
    this.spanTracker.endSpan(spanId, 'ok');
    return true;
  }

  /**
   * Simulate network partition between nodes
   * @param {Object} partitionConfig - Partition configuration
   * @returns {Object} Applied partition
   */
  simulateNetworkPartition(partitionConfig) {
    const spanId = this.spanTracker.startSpan('chaos.networkPartition', partitionConfig);
    const partition = NetworkPartitionSchema.parse({ partitionId: crypto.randomUUID(), ...partitionConfig });
    this.state.activePartitions.set(partition.partitionId, partition);
    this.state.isHealthy = false;
    this.emit('stateChange', { type: 'networkPartition', partition, timestamp: new Date() });
    if (partition.durationMs > 0) {
      const timerId = setTimeout(() => this.healNetworkPartition(partition.partitionId), partition.durationMs);
      this.timers.set(`partition-${partition.partitionId}`, timerId);
    }
    this.spanTracker.endSpan(spanId, 'ok');
    return partition;
  }

  /**
   * Heal a network partition
   * @param {string} partitionId - Partition ID
   * @returns {boolean} Success
   */
  healNetworkPartition(partitionId) {
    const spanId = this.spanTracker.startSpan('chaos.healPartition', { partitionId });
    if (!this.state.activePartitions.has(partitionId)) { this.spanTracker.endSpan(spanId, 'ok'); return false; }
    this.state.activePartitions.delete(partitionId);
    const timerKey = `partition-${partitionId}`;
    if (this.timers.has(timerKey)) { clearTimeout(this.timers.get(timerKey)); this.timers.delete(timerKey); }
    this._checkAndRestoreHealth();
    this.emit('stateChange', { type: 'partitionHealed', partitionId, timestamp: new Date() });
    this.spanTracker.endSpan(spanId, 'ok');
    return true;
  }

  /**
   * Apply CPU/memory pressure simulation
   * @param {Object} constraintConfig - Resource constraint configuration
   * @returns {Object} Applied constraint with ID
   */
  applyResourceConstraint(constraintConfig) {
    const spanId = this.spanTracker.startSpan('chaos.resourceConstraint', constraintConfig);
    const constraint = ResourceConstraintSchema.parse(constraintConfig);
    const constraintId = crypto.randomUUID();
    const constraintWithId = { id: constraintId, ...constraint, appliedAt: Date.now() };
    this.state.activeConstraints.set(constraintId, constraintWithId);
    if (constraint.intensity > 0.7) this.state.isHealthy = false;
    this.emit('stateChange', { type: 'resourceConstraint', constraint: constraintWithId, timestamp: new Date() });
    if (constraint.durationMs > 0) {
      const timerId = setTimeout(() => this.releaseResourceConstraint(constraintId), constraint.durationMs);
      this.timers.set(`constraint-${constraintId}`, timerId);
    }
    this.spanTracker.endSpan(spanId, 'ok');
    return constraintWithId;
  }

  /**
   * Release a resource constraint
   * @param {string} constraintId - Constraint ID
   * @returns {boolean} Success
   */
  releaseResourceConstraint(constraintId) {
    const spanId = this.spanTracker.startSpan('chaos.releaseConstraint', { constraintId });
    if (!this.state.activeConstraints.has(constraintId)) { this.spanTracker.endSpan(spanId, 'ok'); return false; }
    this.state.activeConstraints.delete(constraintId);
    const timerKey = `constraint-${constraintId}`;
    if (this.timers.has(timerKey)) { clearTimeout(this.timers.get(timerKey)); this.timers.delete(timerKey); }
    this._checkAndRestoreHealth();
    this.emit('stateChange', { type: 'constraintReleased', constraintId, timestamp: new Date() });
    this.spanTracker.endSpan(spanId, 'ok');
    return true;
  }

  /**
   * Apply service degradation pattern
   * @param {Object} degradationConfig - Degradation configuration
   * @returns {Object} Applied degradation pattern
   */
  applyDegradationPattern(degradationConfig) {
    const spanId = this.spanTracker.startSpan('chaos.degradationPattern', degradationConfig);
    const pattern = DegradationPatternSchema.parse({ patternId: crypto.randomUUID(), ...degradationConfig });
    this.state.activeDegradations.set(pattern.patternId, pattern);
    this.state.currentDegradation = pattern.level;
    if (pattern.level === 'severe' || pattern.level === 'critical') this.state.isHealthy = false;
    this.emit('stateChange', { type: 'degradation', pattern, timestamp: new Date() });
    this.spanTracker.endSpan(spanId, 'ok');
    return pattern;
  }

  /**
   * Remove service degradation pattern
   * @param {string} patternId - Pattern ID
   * @returns {boolean} Success
   */
  removeDegradationPattern(patternId) {
    const spanId = this.spanTracker.startSpan('chaos.removeDegradation', { patternId });
    if (!this.state.activeDegradations.has(patternId)) { this.spanTracker.endSpan(spanId, 'ok'); return false; }
    this.state.activeDegradations.delete(patternId);
    this._recalculateDegradationLevel();
    this._checkAndRestoreHealth();
    this.emit('stateChange', { type: 'degradationRemoved', patternId, timestamp: new Date() });
    this.spanTracker.endSpan(spanId, 'ok');
    return true;
  }

  /**
   * Simulate latency injection for operations
   * @param {number} baseLatencyMs - Base latency in milliseconds
   * @returns {Promise<number>} Actual latency applied
   */
  async simulateLatency(baseLatencyMs) {
    const spanId = this.spanTracker.startSpan('chaos.latencyInjection', { baseLatencyMs });
    let multiplier = 1;
    for (const d of this.state.activeDegradations.values()) multiplier = Math.max(multiplier, d.latencyMultiplier);
    const actualLatency = Math.floor(baseLatencyMs * multiplier);
    await new Promise(resolve => setTimeout(resolve, actualLatency));
    this.spanTracker.endSpan(spanId, 'ok');
    return actualLatency;
  }

  /**
   * Check if operation should fail based on current degradation
   * @returns {boolean} True if operation should fail
   */
  shouldOperationFail() {
    let maxErrorRate = 0;
    for (const d of this.state.activeDegradations.values()) maxErrorRate = Math.max(maxErrorRate, d.errorRate);
    return Math.random() < maxErrorRate;
  }

  /**
   * Get current uptime metrics
   * @returns {Object} Validated uptime metrics
   */
  getMetrics() {
    const now = Date.now();
    const totalTime = now - this.metrics.startTime;
    const uptimePercentage = totalTime > 0 ? ((totalTime - this.metrics.totalDowntimeMs) / totalTime) * 100 : 100;
    const mtbfMs = this.metrics.failureCount > 0 ? (totalTime - this.metrics.totalDowntimeMs) / this.metrics.failureCount : totalTime;
    const mttrMs = this.metrics.recoveryCount > 0 ? this.metrics.totalDowntimeMs / this.metrics.recoveryCount : 0;
    return UptimeMetricsSchema.parse({
      totalUptimeMs: totalTime - this.metrics.totalDowntimeMs,
      totalDowntimeMs: this.metrics.totalDowntimeMs,
      uptimePercentage: Math.max(0, Math.min(100, uptimePercentage)),
      mtbfMs, mttrMs,
      failureCount: this.metrics.failureCount,
      recoveryCount: this.metrics.recoveryCount,
      lastFailureAt: this.metrics.lastFailureAt,
      lastRecoveryAt: this.metrics.lastRecoveryAt,
    });
  }

  /**
   * Get current system state
   * @returns {Object} Current state snapshot
   */
  getState() {
    return {
      isHealthy: this.state.isHealthy,
      currentDegradation: this.state.currentDegradation,
      activeFailures: Array.from(this.state.activeFailures.values()),
      activeConstraints: Array.from(this.state.activeConstraints.values()),
      activePartitions: Array.from(this.state.activePartitions.values()),
      activeDegradations: Array.from(this.state.activeDegradations.values()),
      running: this.running,
    };
  }

  /** Get OTEL spans for observability @returns {Array} Completed spans */
  getSpans() { return this.spanTracker.getSpans(); }

  /** Clear all active chaos conditions and restore health @returns {UptimeSimulator} */
  clearAll() {
    const spanId = this.spanTracker.startSpan('simulator.clearAll');
    for (const timerId of this.timers.values()) clearTimeout(timerId);
    this.timers.clear();
    ['activeFailures', 'activeConstraints', 'activePartitions', 'activeDegradations']
      .forEach(k => this.state[k].clear());
    this.state.isHealthy = true;
    this.state.currentDegradation = 'none';
    this.emit('stateChange', { type: 'cleared', timestamp: new Date() });
    this.spanTracker.endSpan(spanId, 'ok');
    return this;
  }

  /** @private */
  _updateMetrics() {
    if (!this.state.isHealthy) this.metrics.totalDowntimeMs += this.config.metricsIntervalMs;
    const now = Date.now();
    const totalTime = now - this.metrics.startTime;
    const uptimePercentage = totalTime > 0 ? ((totalTime - this.metrics.totalDowntimeMs) / totalTime) * 100 : 100;
    this.emit('metricsUpdate', { uptimePercentage, totalDowntimeMs: this.metrics.totalDowntimeMs, timestamp: new Date() });
  }

  /** @private */
  _recalculateDegradationLevel() {
    const levels = ['none', 'minor', 'moderate', 'severe', 'critical'];
    let maxLevel = 0;
    for (const d of this.state.activeDegradations.values()) maxLevel = Math.max(maxLevel, levels.indexOf(d.level));
    this.state.currentDegradation = levels[maxLevel];
  }

  /** @private */
  _checkAndRestoreHealth() {
    const hasActiveIssues = this.state.activeFailures.size > 0 ||
      this.state.activePartitions.size > 0 ||
      Array.from(this.state.activeConstraints.values()).some(c => c.intensity > 0.7) ||
      ['severe', 'critical'].includes(this.state.currentDegradation);
    this.state.isHealthy = !hasActiveIssues;
  }
}

/**
 * Create a pre-configured uptime simulator for testing
 * @param {Object} [overrides] - Configuration overrides
 * @returns {UptimeSimulator} Configured simulator instance
 */
export function createTestSimulator(overrides = {}) {
  return new UptimeSimulator({
    serviceName: 'test-daemon', enableChaos: true, chaosIntensity: 0.3,
    metricsIntervalMs: 100, autoRecoveryEnabled: true, autoRecoveryDelayMs: 1000, ...overrides,
  });
}

/**
 * Create a production-grade uptime simulator
 * @param {string} serviceName - Service name
 * @param {Object} [overrides] - Configuration overrides
 * @returns {UptimeSimulator} Configured simulator instance
 */
export function createProductionSimulator(serviceName, overrides = {}) {
  return new UptimeSimulator({
    serviceName, enableChaos: false, chaosIntensity: 0, metricsIntervalMs: 5000,
    maxHistorySize: 50000, autoRecoveryEnabled: true, autoRecoveryDelayMs: 10000, ...overrides,
  });
}
