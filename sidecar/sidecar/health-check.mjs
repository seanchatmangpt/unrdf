/**
 * @file Health check system for sidecar monitoring
 * @module sidecar/health-check
 *
 * @description
 * Health monitoring system with liveness, readiness, and startup probes.
 */

import { z } from 'zod';
import { EventEmitter } from 'events';

/**
 * Health status enum
 */
export const HealthStatus = {
  HEALTHY: 'HEALTHY',
  UNHEALTHY: 'UNHEALTHY',
  DEGRADED: 'DEGRADED',
  UNKNOWN: 'UNKNOWN'
};

/**
 * Health check configuration schema
 */
const HealthCheckConfigSchema = z.object({
  interval: z.number().int().positive().default(10000),
  timeout: z.number().int().positive().default(5000),
  unhealthyThreshold: z.number().int().positive().default(3),
  healthyThreshold: z.number().int().positive().default(2),
  startupGracePeriod: z.number().int().nonnegative().default(30000)
});

/**
 * Health check result
 */
class HealthCheckResult {
  constructor(status, details = {}, error = null) {
    this.status = status;
    this.details = details;
    this.error = error;
    this.timestamp = Date.now();
  }

  isHealthy() {
    return this.status === HealthStatus.HEALTHY;
  }

  isDegraded() {
    return this.status === HealthStatus.DEGRADED;
  }

  isUnhealthy() {
    return this.status === HealthStatus.UNHEALTHY;
  }
}

/**
 * Health monitor for sidecar connection
 */
export class HealthMonitor extends EventEmitter {
  /**
   * Create a new health monitor
   * @param {Function} checkFn - Health check function
   * @param {Object} [config] - Health check configuration
   */
  constructor(checkFn, config = {}) {
    super();
    this.checkFn = checkFn;
    this.config = HealthCheckConfigSchema.parse(config);
    this.status = HealthStatus.UNKNOWN;
    this.consecutiveFailures = 0;
    this.consecutiveSuccesses = 0;
    this.lastCheckTime = null;
    this.lastCheckResult = null;
    this.checkTimer = null;
    this.metrics = {
      totalChecks: 0,
      successfulChecks: 0,
      failedChecks: 0,
      degradedChecks: 0,
      averageLatency: 0,
      statusChanges: []
    };
    this.startupTime = Date.now();
  }

  /**
   * Start health monitoring
   */
  start() {
    if (this.checkTimer) {
      return;
    }

    // Perform initial check
    this._performCheck();

    // Schedule periodic checks
    this.checkTimer = setInterval(() => {
      this._performCheck();
    }, this.config.interval);

    this.emit('started');
  }

  /**
   * Stop health monitoring
   */
  stop() {
    if (this.checkTimer) {
      clearInterval(this.checkTimer);
      this.checkTimer = null;
    }

    this.emit('stopped');
  }

  /**
   * Perform health check
   * @private
   */
  async _performCheck() {
    const startTime = Date.now();
    this.lastCheckTime = startTime;
    this.metrics.totalChecks++;

    // Skip checks during startup grace period
    if (this._isInStartupGracePeriod()) {
      this.emit('checkSkipped', { reason: 'startup_grace_period' });
      return;
    }

    try {
      // Execute health check with timeout
      const result = await Promise.race([
        this.checkFn(),
        this._timeout()
      ]);

      const latency = Date.now() - startTime;
      this._updateLatencyMetrics(latency);

      // Process result
      if (result.status === HealthStatus.HEALTHY) {
        this._onSuccess(result, latency);
      } else if (result.status === HealthStatus.DEGRADED) {
        this._onDegraded(result, latency);
      } else {
        this._onFailure(result, latency);
      }
    } catch (error) {
      const latency = Date.now() - startTime;
      this._updateLatencyMetrics(latency);

      const result = new HealthCheckResult(
        HealthStatus.UNHEALTHY,
        { error: error.message },
        error
      );

      this._onFailure(result, latency);
    }
  }

  /**
   * Handle successful health check
   * @param {HealthCheckResult} result - Check result
   * @param {number} latency - Check latency
   * @private
   */
  _onSuccess(result, latency) {
    this.consecutiveSuccesses++;
    this.consecutiveFailures = 0;
    this.lastCheckResult = result;
    this.metrics.successfulChecks++;

    // Transition to healthy if threshold met
    if (this.consecutiveSuccesses >= this.config.healthyThreshold) {
      this._updateStatus(HealthStatus.HEALTHY);
    }

    this.emit('checkCompleted', {
      status: HealthStatus.HEALTHY,
      latency,
      details: result.details
    });
  }

  /**
   * Handle degraded health check
   * @param {HealthCheckResult} result - Check result
   * @param {number} latency - Check latency
   * @private
   */
  _onDegraded(result, latency) {
    this.consecutiveSuccesses = 0;
    this.consecutiveFailures = 0;
    this.lastCheckResult = result;
    this.metrics.degradedChecks++;

    this._updateStatus(HealthStatus.DEGRADED);

    this.emit('checkCompleted', {
      status: HealthStatus.DEGRADED,
      latency,
      details: result.details
    });
  }

  /**
   * Handle failed health check
   * @param {HealthCheckResult} result - Check result
   * @param {number} latency - Check latency
   * @private
   */
  _onFailure(result, latency) {
    this.consecutiveFailures++;
    this.consecutiveSuccesses = 0;
    this.lastCheckResult = result;
    this.metrics.failedChecks++;

    // Transition to unhealthy if threshold met
    if (this.consecutiveFailures >= this.config.unhealthyThreshold) {
      this._updateStatus(HealthStatus.UNHEALTHY);
    }

    this.emit('checkCompleted', {
      status: HealthStatus.UNHEALTHY,
      latency,
      details: result.details,
      error: result.error
    });
  }

  /**
   * Update health status
   * @param {string} newStatus - New health status
   * @private
   */
  _updateStatus(newStatus) {
    if (this.status !== newStatus) {
      const oldStatus = this.status;
      this.status = newStatus;

      this.metrics.statusChanges.push({
        from: oldStatus,
        to: newStatus,
        timestamp: Date.now()
      });

      this.emit('statusChanged', {
        from: oldStatus,
        to: newStatus
      });
    }
  }

  /**
   * Update latency metrics
   * @param {number} latency - Check latency
   * @private
   */
  _updateLatencyMetrics(latency) {
    const totalLatency = this.metrics.averageLatency * (this.metrics.totalChecks - 1);
    this.metrics.averageLatency = (totalLatency + latency) / this.metrics.totalChecks;
  }

  /**
   * Check if in startup grace period
   * @returns {boolean} True if in grace period
   * @private
   */
  _isInStartupGracePeriod() {
    return Date.now() - this.startupTime < this.config.startupGracePeriod;
  }

  /**
   * Timeout promise
   * @returns {Promise<never>}
   * @private
   */
  _timeout() {
    return new Promise((_, reject) => {
      setTimeout(() => {
        reject(new Error('Health check timeout'));
      }, this.config.timeout);
    });
  }

  /**
   * Get current health status
   * @returns {string} Health status
   */
  getStatus() {
    return this.status;
  }

  /**
   * Check if healthy
   * @returns {boolean} True if healthy
   */
  isHealthy() {
    return this.status === HealthStatus.HEALTHY;
  }

  /**
   * Check if unhealthy
   * @returns {boolean} True if unhealthy
   */
  isUnhealthy() {
    return this.status === HealthStatus.UNHEALTHY;
  }

  /**
   * Get last check result
   * @returns {HealthCheckResult|null} Last check result
   */
  getLastCheckResult() {
    return this.lastCheckResult;
  }

  /**
   * Get health metrics
   * @returns {Object} Health metrics
   */
  getMetrics() {
    const successRate = this.metrics.totalChecks > 0
      ? (this.metrics.successfulChecks / this.metrics.totalChecks) * 100
      : 0;

    return {
      status: this.status,
      totalChecks: this.metrics.totalChecks,
      successfulChecks: this.metrics.successfulChecks,
      failedChecks: this.metrics.failedChecks,
      degradedChecks: this.metrics.degradedChecks,
      successRate,
      averageLatency: this.metrics.averageLatency,
      consecutiveSuccesses: this.consecutiveSuccesses,
      consecutiveFailures: this.consecutiveFailures,
      lastCheckTime: this.lastCheckTime,
      uptime: Date.now() - this.startupTime,
      statusChanges: this.metrics.statusChanges.length
    };
  }

  /**
   * Cleanup health monitor
   */
  cleanup() {
    this.stop();
    this.removeAllListeners();
  }
}

/**
 * Create a health monitor
 * @param {Function} checkFn - Health check function
 * @param {Object} [config] - Health check configuration
 * @returns {HealthMonitor} Health monitor instance
 */
export function createHealthMonitor(checkFn, config) {
  return new HealthMonitor(checkFn, config);
}

/**
 * Create health check result
 * @param {string} status - Health status
 * @param {Object} [details] - Status details
 * @param {Error} [error] - Error if unhealthy
 * @returns {HealthCheckResult} Health check result
 */
export function createHealthCheckResult(status, details, error) {
  return new HealthCheckResult(status, details, error);
}

export default HealthMonitor;
