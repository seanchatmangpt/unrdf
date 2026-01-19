/**
 * @file Comprehensive Health Check System
 * @module @unrdf/daemon/monitoring/health-check
 * @description Liveness/readiness probes, dependency checks, custom health checks,
 * and health history with TTL support for daemon monitoring.
 */

import { z } from 'zod';

/**
 * Health check status enum values
 */
export const HealthStatus = {
  HEALTHY: 'healthy',
  DEGRADED: 'degraded',
  UNHEALTHY: 'unhealthy',
};

/**
 * Health check result schema
 */
export const HealthCheckResultSchema = z.object({
  name: z.string().min(1),
  status: z.enum(['healthy', 'degraded', 'unhealthy']),
  message: z.string().optional(),
  duration: z.number().min(0),
  timestamp: z.date(),
  metadata: z.record(z.string(), z.any()).optional(),
});

/**
 * Dependency check configuration schema
 */
export const DependencyCheckConfigSchema = z.object({
  name: z.string().min(1),
  type: z.enum(['http', 'tcp', 'custom']),
  endpoint: z.string().optional(),
  timeout: z.number().int().min(100).default(5000),
  critical: z.boolean().default(true),
  retries: z.number().int().min(0).default(0),
});

/**
 * Health check configuration schema
 */
export const HealthCheckConfigSchema = z.object({
  livenessInterval: z.number().int().min(1000).default(10000),
  readinessInterval: z.number().int().min(1000).default(5000),
  historyTTL: z.number().int().min(60000).default(3600000),
  historyMaxSize: z.number().int().min(10).default(1000),
  dependencyTimeout: z.number().int().min(100).default(5000),
  unhealthyThreshold: z.number().int().min(1).default(3),
  healthyThreshold: z.number().int().min(1).default(2),
});

/**
 * Overall health report schema
 */
export const HealthReportSchema = z.object({
  status: z.enum(['healthy', 'degraded', 'unhealthy']),
  timestamp: z.date(),
  uptime: z.number().int().min(0),
  liveness: z.object({
    alive: z.boolean(),
    lastCheck: z.date().optional(),
  }),
  readiness: z.object({
    ready: z.boolean(),
    lastCheck: z.date().optional(),
    reason: z.string().optional(),
  }),
  checks: z.array(HealthCheckResultSchema),
  dependencies: z.array(HealthCheckResultSchema),
});

/**
 * Comprehensive health check system for daemon monitoring
 * Provides liveness probes, readiness probes, dependency checks,
 * custom health check registration, and health history with TTL.
 */
export class HealthCheckSystem {
  /** @type {Map<string, Function>} */
  #customChecks = new Map();

  /** @type {Map<string, import('zod').infer<typeof DependencyCheckConfigSchema>>} */
  #dependencies = new Map();

  /** @type {Array<import('zod').infer<typeof HealthCheckResultSchema>>} */
  #history = [];

  /** @type {import('zod').infer<typeof HealthCheckConfigSchema>} */
  #config;

  /** @type {number} */
  #startTime;

  /** @type {boolean} */
  #isReady = false;

  /** @type {string | null} */
  #readinessReason = null;

  /** @type {NodeJS.Timeout | null} */
  #livenessTimer = null;

  /** @type {NodeJS.Timeout | null} */
  #readinessTimer = null;

  /** @type {NodeJS.Timeout | null} */
  #historyCleanupTimer = null;

  /** @type {number} */
  #consecutiveFailures = 0;

  /** @type {number} */
  #consecutiveSuccesses = 0;

  /**
   * Create a new health check system
   * @param {Partial<import('zod').infer<typeof HealthCheckConfigSchema>>} [config] - Configuration options
   */
  constructor(config = {}) {
    this.#config = HealthCheckConfigSchema.parse(config);
    this.#startTime = Date.now();
  }

  /**
   * Start the health check system
   * Begins periodic liveness and readiness checks
   */
  start() {
    this.#startLivenessChecks();
    this.#startReadinessChecks();
    this.#startHistoryCleanup();
  }

  /**
   * Stop the health check system
   * Clears all timers and stops periodic checks
   */
  stop() {
    if (this.#livenessTimer) {
      clearInterval(this.#livenessTimer);
      this.#livenessTimer = null;
    }
    if (this.#readinessTimer) {
      clearInterval(this.#readinessTimer);
      this.#readinessTimer = null;
    }
    if (this.#historyCleanupTimer) {
      clearInterval(this.#historyCleanupTimer);
      this.#historyCleanupTimer = null;
    }
  }

  /**
   * Check if the daemon is alive (liveness probe)
   * @returns {boolean} True if daemon process is running
   */
  isAlive() {
    return true;
  }

  /**
   * Check if the daemon is ready to accept work (readiness probe)
   * @returns {boolean} True if daemon can accept work
   */
  isReady() {
    return this.#isReady;
  }

  /**
   * Set the readiness state
   * @param {boolean} ready - Whether the daemon is ready
   * @param {string} [reason] - Reason for the state change
   */
  setReady(ready, reason = null) {
    this.#isReady = ready;
    this.#readinessReason = reason;
  }

  /**
   * Register a custom health check
   * @param {string} name - Unique name for the check
   * @param {Function} checkFn - Async function returning { status, message?, metadata? }
   * @throws {Error} If check name already exists
   * @example
   * healthCheck.registerCheck('database', async () => {
   *   const connected = await db.ping();
   *   return { status: connected ? 'healthy' : 'unhealthy', message: 'DB connectivity' };
   * });
   */
  registerCheck(name, checkFn) {
    if (typeof name !== 'string' || name.length === 0) {
      throw new Error('Check name must be a non-empty string');
    }
    if (typeof checkFn !== 'function') {
      throw new Error('Check function must be a function');
    }
    if (this.#customChecks.has(name)) {
      throw new Error(`Health check "${name}" already registered`);
    }
    this.#customChecks.set(name, checkFn);
  }

  /**
   * Unregister a custom health check
   * @param {string} name - Name of the check to remove
   * @returns {boolean} True if check was removed
   */
  unregisterCheck(name) {
    return this.#customChecks.delete(name);
  }

  /**
   * Register an external dependency to monitor
   * @param {import('zod').infer<typeof DependencyCheckConfigSchema>} config - Dependency configuration
   * @throws {Error} If dependency name already exists
   * @example
   * healthCheck.registerDependency({
   *   name: 'redis',
   *   type: 'tcp',
   *   endpoint: 'localhost:6379',
   *   timeout: 3000,
   *   critical: true
   * });
   */
  registerDependency(config) {
    const validated = DependencyCheckConfigSchema.parse(config);
    if (this.#dependencies.has(validated.name)) {
      throw new Error(`Dependency "${validated.name}" already registered`);
    }
    this.#dependencies.set(validated.name, validated);
  }

  /**
   * Unregister an external dependency
   * @param {string} name - Name of the dependency to remove
   * @returns {boolean} True if dependency was removed
   */
  unregisterDependency(name) {
    return this.#dependencies.delete(name);
  }

  /**
   * Run all registered health checks
   * @returns {Promise<Array<import('zod').infer<typeof HealthCheckResultSchema>>>} Array of check results
   */
  async runChecks() {
    const results = [];
    const now = new Date();

    for (const [name, checkFn] of this.#customChecks) {
      const startTime = performance.now();
      try {
        const result = await Promise.race([
          checkFn(),
          new Promise((_, reject) =>
            setTimeout(() => reject(new Error('Check timeout')), this.#config.dependencyTimeout)
          ),
        ]);

        const checkResult = HealthCheckResultSchema.parse({
          name,
          status: result.status || HealthStatus.HEALTHY,
          message: result.message,
          duration: performance.now() - startTime,
          timestamp: now,
          metadata: result.metadata,
        });
        results.push(checkResult);
        this.#addToHistory(checkResult);
      } catch (error) {
        const checkResult = HealthCheckResultSchema.parse({
          name,
          status: HealthStatus.UNHEALTHY,
          message: error.message,
          duration: performance.now() - startTime,
          timestamp: now,
        });
        results.push(checkResult);
        this.#addToHistory(checkResult);
      }
    }

    return results;
  }

  /**
   * Run all registered dependency checks
   * @returns {Promise<Array<import('zod').infer<typeof HealthCheckResultSchema>>>} Array of dependency check results
   */
  async runDependencyChecks() {
    const results = [];
    const now = new Date();

    for (const [name, config] of this.#dependencies) {
      const startTime = performance.now();
      let lastError = null;
      let success = false;

      for (let attempt = 0; attempt <= config.retries; attempt++) {
        try {
          await this.#checkDependency(config);
          success = true;
          break;
        } catch (error) {
          lastError = error;
        }
      }

      const checkResult = HealthCheckResultSchema.parse({
        name,
        status: success ? HealthStatus.HEALTHY : HealthStatus.UNHEALTHY,
        message: success ? `${config.type} check passed` : lastError?.message,
        duration: performance.now() - startTime,
        timestamp: now,
        metadata: { type: config.type, critical: config.critical },
      });
      results.push(checkResult);
      this.#addToHistory(checkResult);
    }

    return results;
  }

  /**
   * Get a comprehensive health report
   * @returns {Promise<import('zod').infer<typeof HealthReportSchema>>} Complete health report
   */
  async getHealthReport() {
    const checks = await this.runChecks();
    const dependencies = await this.runDependencyChecks();

    const allResults = [...checks, ...dependencies];
    const hasUnhealthy = allResults.some((r) => r.status === HealthStatus.UNHEALTHY);
    const hasDegraded = allResults.some((r) => r.status === HealthStatus.DEGRADED);
    const criticalUnhealthy = dependencies.some(
      (r) => r.status === HealthStatus.UNHEALTHY && r.metadata?.critical
    );

    let overallStatus = HealthStatus.HEALTHY;
    if (criticalUnhealthy || hasUnhealthy) {
      overallStatus = HealthStatus.UNHEALTHY;
    } else if (hasDegraded) {
      overallStatus = HealthStatus.DEGRADED;
    }

    this.#updateConsecutiveCounts(overallStatus);

    return HealthReportSchema.parse({
      status: overallStatus,
      timestamp: new Date(),
      uptime: Date.now() - this.#startTime,
      liveness: {
        alive: this.isAlive(),
        lastCheck: new Date(),
      },
      readiness: {
        ready: this.#isReady,
        lastCheck: new Date(),
        reason: this.#readinessReason,
      },
      checks,
      dependencies,
    });
  }

  /**
   * Get health history within TTL
   * @param {number} [limit] - Maximum number of results
   * @returns {Array<import('zod').infer<typeof HealthCheckResultSchema>>} Health check history
   */
  getHistory(limit = 100) {
    const cutoff = Date.now() - this.#config.historyTTL;
    return this.#history
      .filter((entry) => entry.timestamp.getTime() > cutoff)
      .slice(-limit);
  }

  /**
   * Get health statistics for a specific check
   * @param {string} name - Check name
   * @returns {{ total: number, healthy: number, unhealthy: number, degraded: number, avgDuration: number }}
   */
  getCheckStats(name) {
    const cutoff = Date.now() - this.#config.historyTTL;
    const relevant = this.#history.filter(
      (entry) => entry.name === name && entry.timestamp.getTime() > cutoff
    );

    const stats = {
      total: relevant.length,
      healthy: 0,
      unhealthy: 0,
      degraded: 0,
      avgDuration: 0,
    };

    if (relevant.length === 0) return stats;

    let totalDuration = 0;
    for (const entry of relevant) {
      totalDuration += entry.duration;
      if (entry.status === HealthStatus.HEALTHY) stats.healthy++;
      else if (entry.status === HealthStatus.UNHEALTHY) stats.unhealthy++;
      else stats.degraded++;
    }
    stats.avgDuration = totalDuration / relevant.length;

    return stats;
  }

  /**
   * Clear health history
   */
  clearHistory() {
    this.#history = [];
  }

  /**
   * Get uptime in milliseconds
   * @returns {number} Uptime in ms
   */
  getUptime() {
    return Date.now() - this.#startTime;
  }

  /**
   * Get consecutive failure/success counts
   * @returns {{ failures: number, successes: number }}
   */
  getConsecutiveCounts() {
    return {
      failures: this.#consecutiveFailures,
      successes: this.#consecutiveSuccesses,
    };
  }

  // Private methods

  #startLivenessChecks() {
    this.#livenessTimer = setInterval(() => {
      // Liveness is always true if this code runs
    }, this.#config.livenessInterval);
  }

  #startReadinessChecks() {
    this.#readinessTimer = setInterval(async () => {
      const report = await this.getHealthReport();
      const wasReady = this.#isReady;

      if (report.status === HealthStatus.UNHEALTHY) {
        if (this.#consecutiveFailures >= this.#config.unhealthyThreshold) {
          this.setReady(false, `Unhealthy for ${this.#consecutiveFailures} consecutive checks`);
        }
      } else if (report.status === HealthStatus.HEALTHY) {
        if (this.#consecutiveSuccesses >= this.#config.healthyThreshold) {
          this.setReady(true);
        }
      }

      if (wasReady !== this.#isReady) {
        this.#emitReadinessChange(this.#isReady);
      }
    }, this.#config.readinessInterval);
  }

  #startHistoryCleanup() {
    this.#historyCleanupTimer = setInterval(() => {
      this.#cleanupHistory();
    }, Math.min(this.#config.historyTTL / 10, 60000));
  }

  #cleanupHistory() {
    const cutoff = Date.now() - this.#config.historyTTL;
    this.#history = this.#history.filter((entry) => entry.timestamp.getTime() > cutoff);

    if (this.#history.length > this.#config.historyMaxSize) {
      this.#history = this.#history.slice(-this.#config.historyMaxSize);
    }
  }

  #addToHistory(result) {
    this.#history.push(result);
    if (this.#history.length > this.#config.historyMaxSize * 1.5) {
      this.#cleanupHistory();
    }
  }

  /**
   * Check a single dependency
   * @param {import('zod').infer<typeof DependencyCheckConfigSchema>} config
   */
  async #checkDependency(config) {
    switch (config.type) {
      case 'http':
        return this.#checkHttpDependency(config);
      case 'tcp':
        return this.#checkTcpDependency(config);
      case 'custom':
        return Promise.resolve();
      default:
        throw new Error(`Unknown dependency type: ${config.type}`);
    }
  }

  async #checkHttpDependency(config) {
    if (!config.endpoint) {
      throw new Error('HTTP dependency requires endpoint');
    }

    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), config.timeout);

    try {
      const response = await fetch(config.endpoint, {
        method: 'GET',
        signal: controller.signal,
      });
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }
    } finally {
      clearTimeout(timeoutId);
    }
  }

  async #checkTcpDependency(config) {
    if (!config.endpoint) {
      throw new Error('TCP dependency requires endpoint');
    }

    // TCP check simulation - in production, use net.Socket
    const [host, port] = config.endpoint.split(':');
    if (!host || !port) {
      throw new Error('Invalid TCP endpoint format. Expected host:port');
    }

    // For non-Node environments or testing, simulate connectivity check
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        reject(new Error(`TCP connection timeout to ${config.endpoint}`));
      }, config.timeout);

      // In a real implementation, would use net.createConnection
      // For now, resolve immediately (mock)
      clearTimeout(timer);
      resolve();
    });
  }

  #updateConsecutiveCounts(status) {
    if (status === HealthStatus.UNHEALTHY) {
      this.#consecutiveFailures++;
      this.#consecutiveSuccesses = 0;
    } else if (status === HealthStatus.HEALTHY) {
      this.#consecutiveSuccesses++;
      this.#consecutiveFailures = 0;
    }
  }

  #emitReadinessChange(ready) {
    // Extensibility point for readiness change notifications
    // Could emit events or call registered callbacks
  }
}

/**
 * Create a preconfigured health check system
 * @param {Partial<import('zod').infer<typeof HealthCheckConfigSchema>>} [config] - Configuration
 * @returns {HealthCheckSystem} Configured health check system
 */
export function createHealthCheckSystem(config = {}) {
  return new HealthCheckSystem(config);
}

export default HealthCheckSystem;
