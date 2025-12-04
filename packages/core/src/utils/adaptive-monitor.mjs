/**
 * @file Adaptive monitoring with health-based sampling
 * @module knowledge-engine/utils/adaptive-monitor
 *
 * @description
 * TRIZ #21 - Skipping pattern: Implements adaptive monitoring that adjusts its
 * sampling frequency based on system health. When the system is healthy, monitoring
 * intervals increase (skip unnecessary checks). When issues are detected, intervals
 * decrease rapidly for faster response.
 *
 * Key features:
 * - Exponential backoff when system is healthy (reduce overhead)
 * - Rapid interval decrease when issues detected
 * - Health history tracking for trend analysis
 * - Configurable thresholds and bounds
 * - Event-based notifications
 * - Multiple monitor orchestration
 *
 * @example
 * ```javascript
 * import { AdaptiveMonitor } from 'unrdf/knowledge-engine/utils/adaptive-monitor';
 *
 * const monitor = new AdaptiveMonitor({
 *   baseInterval: 60000,  // Start at 1 minute
 *   minInterval: 1000,    // Min 1 second when unhealthy
 *   maxInterval: 300000   // Max 5 minutes when stable
 * });
 *
 * monitor.on('health', ({ healthy, details }) => {
 *   if (!healthy) console.warn('System unhealthy:', details);
 * });
 *
 * monitor.start(async () => {
 *   const memUsage = process.memoryUsage();
 *   return memUsage.heapUsed / memUsage.heapTotal < 0.8;
 * });
 * ```
 */

import { EventEmitter } from 'node:events';

/**
 * Default configuration for AdaptiveMonitor
 * @constant {Object}
 */
export const DEFAULT_CONFIG = {
  /** Base monitoring interval in ms */
  baseInterval: 60000,
  /** Minimum interval (fastest polling) */
  minInterval: 1000,
  /** Maximum interval (slowest polling) */
  maxInterval: 300000,
  /** Factor to increase interval when healthy */
  healthyBackoffFactor: 1.5,
  /** Factor to decrease interval when unhealthy */
  unhealthyRushFactor: 0.5,
  /** Number of consecutive healthy checks to consider stable */
  stableThreshold: 3,
  /** Number of consecutive unhealthy checks to consider critical */
  criticalThreshold: 3,
  /** Maximum health history entries */
  maxHistorySize: 100,
  /** Enable verbose logging */
  verbose: false,
};

/**
 * Health status enumeration
 * @constant {Object.<string, string>}
 */
export const HealthStatus = {
  /** System is healthy */
  HEALTHY: 'healthy',
  /** System is unhealthy */
  UNHEALTHY: 'unhealthy',
  /** System health is unknown */
  UNKNOWN: 'unknown',
  /** System is in stable state (multiple healthy checks) */
  STABLE: 'stable',
  /** System is in critical state (multiple unhealthy checks) */
  CRITICAL: 'critical',
  /** System is recovering */
  RECOVERING: 'recovering',
};

/**
 * Adaptive monitor events
 * @constant {Object.<string, string>}
 */
export const MonitorEvents = {
  /** Emitted on each health check */
  HEALTH: 'health',
  /** Emitted when status changes */
  STATUS_CHANGE: 'statusChange',
  /** Emitted when interval adjusts */
  INTERVAL_CHANGE: 'intervalChange',
  /** Emitted on monitor start */
  START: 'start',
  /** Emitted on monitor stop */
  STOP: 'stop',
  /** Emitted on check error */
  ERROR: 'error',
  /** Emitted when entering critical state */
  CRITICAL: 'critical',
  /** Emitted when entering stable state */
  STABLE: 'stable',
  /** Emitted when recovery begins */
  RECOVERY: 'recovery',
};

/**
 * Health check result type
 * @typedef {Object} HealthCheckResult
 * @property {boolean} healthy - Whether the check passed
 * @property {number} timestamp - Unix timestamp of check
 * @property {number} duration - Check duration in ms
 * @property {Object} [details] - Additional check details
 * @property {Error} [error] - Error if check failed
 */

/**
 * Adaptive Monitor class with health-based sampling
 * @extends EventEmitter
 */
export class AdaptiveMonitor extends EventEmitter {
  /**
   * Create an adaptive monitor
   * @param {Object} [config={}] - Configuration options
   * @param {number} [config.baseInterval=60000] - Base monitoring interval
   * @param {number} [config.minInterval=1000] - Minimum interval
   * @param {number} [config.maxInterval=300000] - Maximum interval
   * @param {number} [config.healthyBackoffFactor=1.5] - Backoff multiplier
   * @param {number} [config.unhealthyRushFactor=0.5] - Rush multiplier
   * @param {number} [config.stableThreshold=3] - Checks for stable status
   * @param {number} [config.criticalThreshold=3] - Checks for critical status
   * @param {number} [config.maxHistorySize=100] - Max history entries
   * @param {boolean} [config.verbose=false] - Enable verbose logging
   */
  constructor(config = {}) {
    super();

    this.config = { ...DEFAULT_CONFIG, ...config };

    // Current state
    this.currentInterval = this.config.baseInterval;
    this.running = false;
    this.paused = false;
    this.status = HealthStatus.UNKNOWN;

    // Health tracking
    this.healthHistory = [];
    this.consecutiveHealthy = 0;
    this.consecutiveUnhealthy = 0;
    this.totalChecks = 0;
    this.totalHealthy = 0;
    this.totalUnhealthy = 0;

    // Timing
    this.startTime = null;
    this.lastCheckTime = null;
    this.nextCheckTime = null;

    // Internal
    this._timeoutId = null;
    this._checkFn = null;

    // Bind methods for stable references
    this._executeCheck = this._executeCheck.bind(this);
    this._scheduleNext = this._scheduleNext.bind(this);
  }

  /**
   * Start monitoring with the given health check function
   * @param {Function} checkFn - Async function returning boolean or {healthy, details}
   * @returns {AdaptiveMonitor} this for chaining
   */
  start(checkFn) {
    if (this.running) {
      this._log('Monitor already running');
      return this;
    }

    if (typeof checkFn !== 'function') {
      throw new TypeError('checkFn must be a function');
    }

    this._checkFn = checkFn;
    this.running = true;
    this.paused = false;
    this.startTime = Date.now();
    this.currentInterval = this.config.baseInterval;

    this._log(`Starting monitor with ${this.currentInterval}ms interval`);
    this.emit(MonitorEvents.START, {
      interval: this.currentInterval,
      timestamp: this.startTime,
    });

    // Execute first check immediately
    this._executeCheck();

    return this;
  }

  /**
   * Stop monitoring
   * @returns {AdaptiveMonitor} this for chaining
   */
  stop() {
    if (!this.running) {
      return this;
    }

    this.running = false;

    if (this._timeoutId) {
      clearTimeout(this._timeoutId);
      this._timeoutId = null;
    }

    const duration = Date.now() - this.startTime;
    this._log(`Stopping monitor after ${duration}ms`);

    this.emit(MonitorEvents.STOP, {
      duration,
      totalChecks: this.totalChecks,
      timestamp: Date.now(),
    });

    return this;
  }

  /**
   * Pause monitoring (keeps state, stops checks)
   * @returns {AdaptiveMonitor} this for chaining
   */
  pause() {
    if (!this.running || this.paused) {
      return this;
    }

    this.paused = true;

    if (this._timeoutId) {
      clearTimeout(this._timeoutId);
      this._timeoutId = null;
    }

    this._log('Monitor paused');
    return this;
  }

  /**
   * Resume monitoring after pause
   * @returns {AdaptiveMonitor} this for chaining
   */
  resume() {
    if (!this.running || !this.paused) {
      return this;
    }

    this.paused = false;
    this._log('Monitor resumed');
    this._executeCheck();

    return this;
  }

  /**
   * Force an immediate health check
   * @returns {Promise<HealthCheckResult>} Check result
   */
  async checkNow() {
    return this._executeCheck(true);
  }

  /**
   * Reset the monitor to initial state
   * @returns {AdaptiveMonitor} this for chaining
   */
  reset() {
    this.stop();

    this.currentInterval = this.config.baseInterval;
    this.status = HealthStatus.UNKNOWN;
    this.healthHistory = [];
    this.consecutiveHealthy = 0;
    this.consecutiveUnhealthy = 0;
    this.totalChecks = 0;
    this.totalHealthy = 0;
    this.totalUnhealthy = 0;
    this.startTime = null;
    this.lastCheckTime = null;
    this.nextCheckTime = null;

    this._log('Monitor reset');
    return this;
  }

  /**
   * Get current monitoring statistics
   * @returns {Object} Monitor statistics
   */
  getStats() {
    const now = Date.now();
    const uptime = this.startTime ? now - this.startTime : 0;

    return {
      running: this.running,
      paused: this.paused,
      status: this.status,
      currentInterval: this.currentInterval,
      uptime,
      totalChecks: this.totalChecks,
      totalHealthy: this.totalHealthy,
      totalUnhealthy: this.totalUnhealthy,
      healthRate:
        this.totalChecks > 0
          ? ((this.totalHealthy / this.totalChecks) * 100).toFixed(2) + '%'
          : 'N/A',
      consecutiveHealthy: this.consecutiveHealthy,
      consecutiveUnhealthy: this.consecutiveUnhealthy,
      lastCheckTime: this.lastCheckTime,
      nextCheckTime: this.nextCheckTime,
      historySize: this.healthHistory.length,
    };
  }

  /**
   * Get health history
   * @param {number} [limit=10] - Maximum entries to return
   * @returns {HealthCheckResult[]} Recent health checks
   */
  getHistory(limit = 10) {
    return this.healthHistory.slice(-limit);
  }

  /**
   * Get health trend (positive = improving, negative = degrading)
   * @param {number} [window=10] - Number of recent checks to analyze
   * @returns {number} Trend value between -1 and 1
   */
  getHealthTrend(window = 10) {
    const recent = this.healthHistory.slice(-window);

    if (recent.length < 2) {
      return 0;
    }

    const firstHalf = recent.slice(0, Math.floor(recent.length / 2));
    const secondHalf = recent.slice(Math.floor(recent.length / 2));

    const firstHealthy = firstHalf.filter(h => h.healthy).length / firstHalf.length;
    const secondHealthy = secondHalf.filter(h => h.healthy).length / secondHalf.length;

    return secondHealthy - firstHealthy;
  }

  /**
   * Execute a health check
   * @private
   * @param {boolean} [immediate=false] - If true, don't schedule next
   * @returns {Promise<HealthCheckResult>} Check result
   */
  async _executeCheck(immediate = false) {
    if (!this.running || (this.paused && !immediate)) {
      return null;
    }

    const startTime = Date.now();
    let result;

    try {
      const checkResult = await this._checkFn();

      // Normalize result
      let healthy, details;
      if (typeof checkResult === 'boolean') {
        healthy = checkResult;
        details = {};
      } else if (checkResult && typeof checkResult === 'object') {
        healthy = checkResult.healthy;
        details = checkResult.details || checkResult;
      } else {
        healthy = !!checkResult;
        details = {};
      }

      result = {
        healthy,
        timestamp: startTime,
        duration: Date.now() - startTime,
        details,
      };

      this._recordResult(result);
    } catch (error) {
      result = {
        healthy: false,
        timestamp: startTime,
        duration: Date.now() - startTime,
        error,
      };

      this._recordResult(result);
      this.emit(MonitorEvents.ERROR, { error, timestamp: startTime });
      this._log(`Check error: ${error.message}`);
    }

    // Schedule next check unless immediate
    if (!immediate && this.running && !this.paused) {
      this._scheduleNext();
    }

    return result;
  }

  /**
   * Record a health check result
   * @private
   * @param {HealthCheckResult} result - Check result
   */
  _recordResult(result) {
    this.lastCheckTime = result.timestamp;
    this.totalChecks++;

    // Add to history (with size limit)
    this.healthHistory.push(result);
    if (this.healthHistory.length > this.config.maxHistorySize) {
      this.healthHistory.shift();
    }

    // Update counters
    const previousStatus = this.status;

    if (result.healthy) {
      this.totalHealthy++;
      this.consecutiveHealthy++;
      this.consecutiveUnhealthy = 0;

      // Check for stable state
      if (this.consecutiveHealthy >= this.config.stableThreshold) {
        if (this.status !== HealthStatus.STABLE) {
          this.status = HealthStatus.STABLE;
          this.emit(MonitorEvents.STABLE, { checks: this.consecutiveHealthy });
        }
      } else if (this.status === HealthStatus.CRITICAL || this.status === HealthStatus.UNHEALTHY) {
        this.status = HealthStatus.RECOVERING;
        this.emit(MonitorEvents.RECOVERY, { timestamp: Date.now() });
      } else {
        this.status = HealthStatus.HEALTHY;
      }
    } else {
      this.totalUnhealthy++;
      this.consecutiveUnhealthy++;
      this.consecutiveHealthy = 0;

      // Check for critical state
      if (this.consecutiveUnhealthy >= this.config.criticalThreshold) {
        if (this.status !== HealthStatus.CRITICAL) {
          this.status = HealthStatus.CRITICAL;
          this.emit(MonitorEvents.CRITICAL, {
            checks: this.consecutiveUnhealthy,
            details: result.details || result.error,
          });
        }
      } else {
        this.status = HealthStatus.UNHEALTHY;
      }
    }

    // Emit status change if changed
    if (previousStatus !== this.status) {
      this.emit(MonitorEvents.STATUS_CHANGE, {
        previous: previousStatus,
        current: this.status,
        timestamp: Date.now(),
      });
    }

    // Emit health event
    this.emit(MonitorEvents.HEALTH, result);

    // Adjust interval
    this._adjustInterval(result.healthy);
  }

  /**
   * Adjust the monitoring interval based on health
   * @private
   * @param {boolean} isHealthy - Whether the last check was healthy
   */
  _adjustInterval(isHealthy) {
    const previousInterval = this.currentInterval;

    if (isHealthy) {
      // Healthy: exponential backoff (increase interval)
      this.currentInterval = Math.min(
        this.currentInterval * this.config.healthyBackoffFactor,
        this.config.maxInterval
      );
    } else {
      // Unhealthy: rush (decrease interval)
      this.currentInterval = Math.max(
        this.currentInterval * this.config.unhealthyRushFactor,
        this.config.minInterval
      );
    }

    // Round to nearest 100ms for cleaner values
    this.currentInterval = Math.round(this.currentInterval / 100) * 100;

    // Emit if interval changed significantly (>10%)
    if (Math.abs(this.currentInterval - previousInterval) / previousInterval > 0.1) {
      this._log(`Interval adjusted: ${previousInterval}ms -> ${this.currentInterval}ms`);
      this.emit(MonitorEvents.INTERVAL_CHANGE, {
        previous: previousInterval,
        current: this.currentInterval,
        healthy: isHealthy,
      });
    }
  }

  /**
   * Schedule the next health check
   * @private
   */
  _scheduleNext() {
    if (this._timeoutId) {
      clearTimeout(this._timeoutId);
    }

    this.nextCheckTime = Date.now() + this.currentInterval;
    this._timeoutId = setTimeout(this._executeCheck, this.currentInterval);

    // Don't block process exit
    if (this._timeoutId.unref) {
      this._timeoutId.unref();
    }
  }

  /**
   * Log a message if verbose mode is enabled
   * @private
   * @param {string} message - Message to log
   */
  _log(message) {
    if (this.config.verbose) {
      console.log(`[AdaptiveMonitor] ${message}`);
    }
  }
}

/**
 * Create an adaptive monitor with default configuration
 * @param {Object} [config] - Configuration overrides
 * @returns {AdaptiveMonitor} New monitor instance
 */
export function createAdaptiveMonitor(config = {}) {
  return new AdaptiveMonitor(config);
}

/**
 * Create a memory usage monitor
 * @param {number} [threshold=0.8] - Memory usage threshold (0-1)
 * @param {Object} [config] - Additional config
 * @returns {AdaptiveMonitor} Configured monitor
 */
export function createMemoryMonitor(threshold = 0.8, config = {}) {
  const monitor = new AdaptiveMonitor({
    baseInterval: 30000,
    minInterval: 5000,
    maxInterval: 120000,
    ...config,
  });

  const checkFn = async () => {
    const usage = process.memoryUsage();
    const heapUsage = usage.heapUsed / usage.heapTotal;

    return {
      healthy: heapUsage < threshold,
      details: {
        heapUsed: usage.heapUsed,
        heapTotal: usage.heapTotal,
        heapUsage: `${(heapUsage * 100).toFixed(1)}%`,
        external: usage.external,
        rss: usage.rss,
      },
    };
  };

  return { monitor, start: () => monitor.start(checkFn) };
}

/**
 * Create an event loop monitor
 * @param {number} [lagThreshold=100] - Event loop lag threshold in ms
 * @param {Object} [config] - Additional config
 * @returns {AdaptiveMonitor} Configured monitor
 */
export function createEventLoopMonitor(lagThreshold = 100, config = {}) {
  const monitor = new AdaptiveMonitor({
    baseInterval: 10000,
    minInterval: 1000,
    maxInterval: 60000,
    ...config,
  });

  const checkFn = async () => {
    const start = Date.now();
    await new Promise(resolve => setImmediate(resolve));
    const lag = Date.now() - start;

    return {
      healthy: lag < lagThreshold,
      details: {
        lag,
        lagThreshold,
      },
    };
  };

  return { monitor, start: () => monitor.start(checkFn) };
}

/**
 * Monitor orchestrator for managing multiple monitors
 */
export class MonitorOrchestrator extends EventEmitter {
  /**
   * Create a monitor orchestrator
   */
  constructor() {
    super();
    this.monitors = new Map();
    this.aggregatedStatus = HealthStatus.UNKNOWN;
  }

  /**
   * Add a monitor
   * @param {string} name - Monitor name
   * @param {AdaptiveMonitor} monitor - Monitor instance
   * @returns {MonitorOrchestrator} this for chaining
   */
  add(name, monitor) {
    this.monitors.set(name, monitor);

    // Forward events
    monitor.on(MonitorEvents.HEALTH, result => {
      this.emit(`${name}:health`, result);
      this._updateAggregatedStatus();
    });

    monitor.on(MonitorEvents.CRITICAL, data => {
      this.emit(`${name}:critical`, data);
      this.emit('critical', { monitor: name, ...data });
    });

    return this;
  }

  /**
   * Remove a monitor
   * @param {string} name - Monitor name
   * @returns {boolean} True if removed
   */
  remove(name) {
    const monitor = this.monitors.get(name);
    if (monitor) {
      monitor.stop();
      this.monitors.delete(name);
      return true;
    }
    return false;
  }

  /**
   * Start all monitors
   */
  startAll() {
    for (const monitor of this.monitors.values()) {
      if (!monitor.running) {
        // Monitor should have been started with checkFn via .start()
        // This just ensures running state
      }
    }
  }

  /**
   * Stop all monitors
   */
  stopAll() {
    for (const monitor of this.monitors.values()) {
      monitor.stop();
    }
  }

  /**
   * Get aggregated statistics
   * @returns {Object} Aggregated stats
   */
  getAggregatedStats() {
    const stats = {};
    for (const [name, monitor] of this.monitors.entries()) {
      stats[name] = monitor.getStats();
    }
    return {
      monitors: stats,
      aggregatedStatus: this.aggregatedStatus,
      monitorCount: this.monitors.size,
    };
  }

  /**
   * Update aggregated status based on all monitors
   * @private
   */
  _updateAggregatedStatus() {
    const statuses = Array.from(this.monitors.values()).map(m => m.status);

    if (statuses.includes(HealthStatus.CRITICAL)) {
      this.aggregatedStatus = HealthStatus.CRITICAL;
    } else if (statuses.includes(HealthStatus.UNHEALTHY)) {
      this.aggregatedStatus = HealthStatus.UNHEALTHY;
    } else if (statuses.every(s => s === HealthStatus.STABLE)) {
      this.aggregatedStatus = HealthStatus.STABLE;
    } else if (statuses.includes(HealthStatus.RECOVERING)) {
      this.aggregatedStatus = HealthStatus.RECOVERING;
    } else if (statuses.every(s => s === HealthStatus.HEALTHY || s === HealthStatus.STABLE)) {
      this.aggregatedStatus = HealthStatus.HEALTHY;
    } else {
      this.aggregatedStatus = HealthStatus.UNKNOWN;
    }
  }
}

/**
 * Create a monitor orchestrator
 * @returns {MonitorOrchestrator} New orchestrator
 */
export function createMonitorOrchestrator() {
  return new MonitorOrchestrator();
}

export default AdaptiveMonitor;
