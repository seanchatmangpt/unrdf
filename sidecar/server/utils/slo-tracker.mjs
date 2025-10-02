/**
 * @file SLO (Service Level Objective) Tracker
 * @description Track SLOs, error budgets, and alerting for UNRDF services
 */

import { EventEmitter } from 'events';

/**
 * SLO Target Types
 */
export const SLOType = {
  LATENCY: 'latency',
  AVAILABILITY: 'availability',
  ERROR_RATE: 'error_rate',
  THROUGHPUT: 'throughput',
};

/**
 * SLO Status
 */
export const SLOStatus = {
  HEALTHY: 'healthy',
  WARNING: 'warning',
  CRITICAL: 'critical',
  EXHAUSTED: 'exhausted',
};

/**
 * SLO Configuration Schema
 * @typedef {Object} SLOConfig
 * @property {string} name - SLO name
 * @property {string} type - SLO type (latency, availability, error_rate, throughput)
 * @property {number} target - Target percentage (0-1, e.g., 0.99 = 99%)
 * @property {number} threshold - Threshold value (ms for latency, requests/s for throughput)
 * @property {number} errorBudget - Error budget percentage (0-1, e.g., 0.01 = 1%)
 * @property {number} window - Time window in milliseconds for SLO calculation
 */

/**
 * SLO Tracker
 * Tracks service level objectives, calculates error budgets, and triggers alerts
 */
export class SLOTracker extends EventEmitter {
  /**
   * Create SLO tracker
   * @param {Object} options - Tracker options
   * @param {Array<SLOConfig>} options.slos - SLO configurations
   * @param {number} [options.checkInterval=60000] - Check interval in ms
   * @param {boolean} [options.enableAlerts=true] - Enable alerting
   */
  constructor(options = {}) {
    super();
    this.slos = new Map();
    this.measurements = new Map();
    this.checkInterval = options.checkInterval || 60000; // 1 minute
    this.enableAlerts = options.enableAlerts !== false;
    this.intervalId = null;

    // Initialize SLOs
    if (options.slos && Array.isArray(options.slos)) {
      options.slos.forEach(slo => this.addSLO(slo));
    }
  }

  /**
   * Add SLO to tracker
   * @param {SLOConfig} slo - SLO configuration
   */
  addSLO(slo) {
    const sloId = slo.name;

    this.slos.set(sloId, {
      ...slo,
      status: SLOStatus.HEALTHY,
      currentBudget: slo.errorBudget,
      lastCheck: Date.now(),
    });

    this.measurements.set(sloId, []);
  }

  /**
   * Record measurement for SLO
   * @param {string} sloName - SLO name
   * @param {Object} measurement - Measurement data
   * @param {number} measurement.value - Measured value
   * @param {boolean} [measurement.success=true] - Whether operation succeeded
   * @param {number} [measurement.timestamp] - Measurement timestamp
   */
  recordMeasurement(sloName, measurement) {
    if (!this.slos.has(sloName)) {
      return;
    }

    const measurements = this.measurements.get(sloName);
    measurements.push({
      value: measurement.value,
      success: measurement.success !== false,
      timestamp: measurement.timestamp || Date.now(),
    });

    // Keep only measurements within the window
    const slo = this.slos.get(sloName);
    const windowStart = Date.now() - slo.window;

    const filteredMeasurements = measurements.filter(m => m.timestamp >= windowStart);
    this.measurements.set(sloName, filteredMeasurements);

    // Check SLO compliance
    this._checkSLO(sloName);
  }

  /**
   * Check SLO compliance
   * @param {string} sloName - SLO name
   * @private
   */
  _checkSLO(sloName) {
    const slo = this.slos.get(sloName);
    const measurements = this.measurements.get(sloName);

    if (measurements.length === 0) {
      return;
    }

    let compliance;
    let budgetUsed;

    switch (slo.type) {
      case SLOType.LATENCY:
        compliance = this._checkLatencySLO(slo, measurements);
        break;

      case SLOType.AVAILABILITY:
        compliance = this._checkAvailabilitySLO(slo, measurements);
        break;

      case SLOType.ERROR_RATE:
        compliance = this._checkErrorRateSLO(slo, measurements);
        break;

      case SLOType.THROUGHPUT:
        compliance = this._checkThroughputSLO(slo, measurements);
        break;

      default:
        return;
    }

    budgetUsed = 1 - compliance;
    const budgetRemaining = slo.errorBudget - budgetUsed;

    // Update SLO state
    slo.currentBudget = budgetRemaining;
    slo.lastCheck = Date.now();
    slo.compliance = compliance;

    // Determine status
    const previousStatus = slo.status;

    if (budgetRemaining <= 0) {
      slo.status = SLOStatus.EXHAUSTED;
    } else if (budgetRemaining <= slo.errorBudget * 0.1) {
      slo.status = SLOStatus.CRITICAL;
    } else if (budgetRemaining <= slo.errorBudget * 0.25) {
      slo.status = SLOStatus.WARNING;
    } else {
      slo.status = SLOStatus.HEALTHY;
    }

    // Emit events on status change
    if (previousStatus !== slo.status) {
      this.emit('statusChanged', {
        sloName,
        from: previousStatus,
        to: slo.status,
        budgetRemaining,
        compliance,
      });

      if (this.enableAlerts) {
        this._triggerAlert(sloName, slo);
      }
    }
  }

  /**
   * Check latency SLO
   * @param {SLOConfig} slo - SLO configuration
   * @param {Array} measurements - Measurements
   * @returns {number} Compliance percentage (0-1)
   * @private
   */
  _checkLatencySLO(slo, measurements) {
    // Calculate percentile (default to 95th)
    const percentile = slo.percentile || 0.95;
    const sorted = measurements.map(m => m.value).sort((a, b) => a - b);
    const index = Math.floor(sorted.length * percentile);
    const p95 = sorted[index];

    // Check if within threshold
    return p95 <= slo.threshold ? 1 : (slo.threshold / p95);
  }

  /**
   * Check availability SLO
   * @param {SLOConfig} slo - SLO configuration
   * @param {Array} measurements - Measurements
   * @returns {number} Compliance percentage (0-1)
   * @private
   */
  _checkAvailabilitySLO(slo, measurements) {
    const total = measurements.length;
    const successful = measurements.filter(m => m.success).length;
    return successful / total;
  }

  /**
   * Check error rate SLO
   * @param {SLOConfig} slo - SLO configuration
   * @param {Array} measurements - Measurements
   * @returns {number} Compliance percentage (0-1)
   * @private
   */
  _checkErrorRateSLO(slo, measurements) {
    const total = measurements.length;
    const errors = measurements.filter(m => !m.success).length;
    const errorRate = errors / total;
    return 1 - errorRate;
  }

  /**
   * Check throughput SLO
   * @param {SLOConfig} slo - SLO configuration
   * @param {Array} measurements - Measurements
   * @returns {number} Compliance percentage (0-1)
   * @private
   */
  _checkThroughputSLO(slo, measurements) {
    const windowSeconds = slo.window / 1000;
    const requestsPerSecond = measurements.length / windowSeconds;
    return requestsPerSecond >= slo.threshold ? 1 : (requestsPerSecond / slo.threshold);
  }

  /**
   * Trigger alert for SLO violation
   * @param {string} sloName - SLO name
   * @param {Object} slo - SLO data
   * @private
   */
  _triggerAlert(sloName, slo) {
    const alert = {
      sloName,
      status: slo.status,
      budgetRemaining: slo.currentBudget,
      compliance: slo.compliance,
      timestamp: Date.now(),
    };

    this.emit('alert', alert);

    // Log alert
    console.warn(`[SLO Alert] ${sloName}: ${slo.status}`, {
      budgetRemaining: `${(slo.currentBudget * 100).toFixed(2)}%`,
      compliance: `${(slo.compliance * 100).toFixed(2)}%`,
    });
  }

  /**
   * Get SLO status
   * @param {string} sloName - SLO name
   * @returns {Object|null} SLO status
   */
  getSLOStatus(sloName) {
    const slo = this.slos.get(sloName);
    if (!slo) {
      return null;
    }

    return {
      name: slo.name,
      type: slo.type,
      status: slo.status,
      target: slo.target,
      compliance: slo.compliance,
      errorBudget: slo.errorBudget,
      budgetRemaining: slo.currentBudget,
      lastCheck: slo.lastCheck,
    };
  }

  /**
   * Get all SLO statuses
   * @returns {Array<Object>} Array of SLO statuses
   */
  getAllSLOStatuses() {
    return Array.from(this.slos.keys()).map(name => this.getSLOStatus(name));
  }

  /**
   * Start SLO monitoring
   */
  start() {
    if (this.intervalId) {
      return;
    }

    this.intervalId = setInterval(() => {
      this.slos.forEach((_, sloName) => {
        this._checkSLO(sloName);
      });
    }, this.checkInterval);

    this.emit('started');
  }

  /**
   * Stop SLO monitoring
   */
  stop() {
    if (this.intervalId) {
      clearInterval(this.intervalId);
      this.intervalId = null;
    }

    this.emit('stopped');
  }

  /**
   * Reset SLO measurements
   * @param {string} [sloName] - Optional SLO name to reset (resets all if not provided)
   */
  reset(sloName) {
    if (sloName) {
      this.measurements.set(sloName, []);
      const slo = this.slos.get(sloName);
      if (slo) {
        slo.currentBudget = slo.errorBudget;
        slo.status = SLOStatus.HEALTHY;
      }
    } else {
      this.measurements.forEach((_, name) => {
        this.measurements.set(name, []);
      });
      this.slos.forEach(slo => {
        slo.currentBudget = slo.errorBudget;
        slo.status = SLOStatus.HEALTHY;
      });
    }
  }
}

/**
 * Create SLO tracker with default UNRDF SLOs
 * @param {Object} [options] - Tracker options
 * @returns {SLOTracker} SLO tracker instance
 */
export function createDefaultSLOTracker(options = {}) {
  const defaultSLOs = [
    {
      name: 'api_latency',
      type: SLOType.LATENCY,
      target: 0.95,
      threshold: 100,
      errorBudget: 0.01,
      window: 300000, // 5 minutes
      percentile: 0.95,
    },
    {
      name: 'availability',
      type: SLOType.AVAILABILITY,
      target: 0.999,
      errorBudget: 0.001,
      window: 300000,
    },
    {
      name: 'error_rate',
      type: SLOType.ERROR_RATE,
      target: 0.99,
      threshold: 0.01,
      errorBudget: 0.01,
      window: 300000,
    },
  ];

  return new SLOTracker({
    ...options,
    slos: [...(options.slos || []), ...defaultSLOs],
  });
}

export default SLOTracker;
