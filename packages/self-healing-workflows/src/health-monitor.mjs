/**
 * @file Health monitoring and alerting
 * @module @unrdf/self-healing-workflows/health
 * @description Monitors system health and triggers alerts
 */

import {
  HealthCheckConfigSchema
} from './schemas.mjs';

/**
 * Health monitor for system health checks
 */
export class HealthMonitor {
  /**
   * Creates a new health monitor
   * @param {Object} [config] - Health check configuration
   * @param {number} [config.interval=30000] - Check interval in ms
   * @param {number} [config.timeout=5000] - Check timeout in ms
   * @param {number} [config.unhealthyThreshold=3] - Failures before unhealthy
   * @param {number} [config.healthyThreshold=2] - Successes before healthy
   */
  constructor(config = {}) {
    this.config = HealthCheckConfigSchema.parse(config);
    this.checks = new Map();
    this.status = 'healthy';
    this.intervalId = null;
    this.listeners = new Set();
    this.consecutiveFailures = 0;
    this.consecutiveSuccesses = 0;
  }

  /**
   * Registers a health check
   * @param {string} name - Check name
   * @param {Function} checkFn - Async check function
   * @param {Object} [options] - Check options
   * @returns {void}
   */
  registerCheck(name, checkFn, options = {}) {
    this.checks.set(name, {
      name,
      fn: checkFn,
      status: 'healthy',
      lastCheck: null,
      lastSuccess: null,
      lastFailure: null,
      failures: 0,
      ...options
    });
  }

  /**
   * Removes a health check
   * @param {string} name - Check name
   * @returns {boolean} True if check was removed
   */
  unregisterCheck(name) {
    return this.checks.delete(name);
  }

  /**
   * Executes all health checks
   * @returns {Promise<Object>} Health check result
   */
  async check() {
    const startTime = Date.now();
    const checkResults = [];

    for (const [name, check] of this.checks) {
      const checkStart = Date.now();
      let status = 'healthy';
      let message;

      try {
        // Execute check with timeout
        await Promise.race([
          check.fn(),
          new Promise((_, reject) => {
            setTimeout(() => {
              reject(new Error(`Health check timeout: ${name}`));
            }, this.config.timeout);
          })
        ]);

        check.lastSuccess = Date.now();
        check.failures = 0;
      } catch (error) {
        status = 'unhealthy';
        message = error.message;
        check.lastFailure = Date.now();
        check.failures++;
      }

      check.status = status;
      check.lastCheck = Date.now();

      checkResults.push({
        name,
        status,
        message,
        duration: Date.now() - checkStart
      });
    }

    // Calculate overall status
    const overallStatus = this.calculateOverallStatus(checkResults);
    this.updateStatus(overallStatus);

    const result = {
      status: overallStatus,
      timestamp: Date.now(),
      checks: checkResults,
      metadata: {
        duration: Date.now() - startTime,
        totalChecks: checkResults.length
      }
    };

    // Notify listeners
    this.notifyListeners(result);

    return result;
  }

  /**
   * Calculates overall health status from check results
   * @param {Array<Object>} checkResults - Individual check results
   * @returns {string} Overall status
   */
  calculateOverallStatus(checkResults) {
    const unhealthyCount = checkResults.filter(c => c.status === 'unhealthy').length;
    const totalCount = checkResults.length;

    if (totalCount === 0) {
      return 'healthy';
    }

    if (unhealthyCount === 0) {
      return 'healthy';
    }

    if (unhealthyCount === totalCount) {
      return 'unhealthy';
    }

    return 'degraded';
  }

  /**
   * Updates health status with thresholds
   * @param {string} newStatus - New status
   * @returns {void}
   */
  updateStatus(newStatus) {
    if (newStatus === 'unhealthy') {
      this.consecutiveFailures++;
      this.consecutiveSuccesses = 0;

      if (this.consecutiveFailures >= this.config.unhealthyThreshold) {
        this.status = 'unhealthy';
      }
    } else if (newStatus === 'healthy') {
      this.consecutiveSuccesses++;
      this.consecutiveFailures = 0;

      if (this.consecutiveSuccesses >= this.config.healthyThreshold) {
        this.status = 'healthy';
      }
    } else {
      this.status = 'degraded';
    }
  }

  /**
   * Starts periodic health checks
   * @returns {void}
   */
  start() {
    if (this.intervalId) {
      return; // Already running
    }

    // Initial check
    this.check();

    // Schedule periodic checks
    this.intervalId = setInterval(() => {
      this.check();
    }, this.config.interval);
  }

  /**
   * Stops periodic health checks
   * @returns {void}
   */
  stop() {
    if (this.intervalId) {
      clearInterval(this.intervalId);
      this.intervalId = null;
    }
  }

  /**
   * Gets current health status
   * @returns {string} Current status
   */
  getStatus() {
    return this.status;
  }

  /**
   * Checks if system is healthy
   * @returns {boolean} True if healthy
   */
  isHealthy() {
    return this.status === 'healthy';
  }

  /**
   * Gets all registered checks
   * @returns {Array<Object>} Health checks
   */
  getChecks() {
    return Array.from(this.checks.values());
  }

  /**
   * Adds a status change listener
   * @param {Function} listener - Listener function
   * @returns {Function} Unsubscribe function
   */
  onStatusChange(listener) {
    this.listeners.add(listener);
    return () => this.listeners.delete(listener);
  }

  /**
   * Notifies all listeners of health check result
   * @param {Object} result - Health check result
   * @returns {void}
   */
  notifyListeners(result) {
    for (const listener of this.listeners) {
      try {
        listener(result);
      } catch (error) {
        console.error('Health monitor listener error:', error);
      }
    }
  }

  /**
   * Gets health statistics
   * @returns {Object} Health statistics
   */
  getStats() {
    const stats = {
      overall: this.status,
      consecutiveFailures: this.consecutiveFailures,
      consecutiveSuccesses: this.consecutiveSuccesses,
      checks: {}
    };

    for (const [name, check] of this.checks) {
      stats.checks[name] = {
        status: check.status,
        failures: check.failures,
        lastCheck: check.lastCheck,
        lastSuccess: check.lastSuccess,
        lastFailure: check.lastFailure
      };
    }

    return stats;
  }

  /**
   * Resets health monitor state
   * @returns {void}
   */
  reset() {
    this.status = 'healthy';
    this.consecutiveFailures = 0;
    this.consecutiveSuccesses = 0;

    for (const [, check] of this.checks) {
      check.status = 'healthy';
      check.failures = 0;
    }
  }
}

/**
 * Creates a new health monitor instance
 * @param {Object} [config] - Health check configuration
 * @returns {HealthMonitor} Health monitor instance
 */
export function createHealthMonitor(config) {
  return new HealthMonitor(config);
}
