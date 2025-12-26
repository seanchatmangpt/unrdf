/**
 * @file Health Check Endpoints
 * @module @unrdf/core/health
 *
 * @description
 * Production-grade health check system with liveness, readiness, and metrics endpoints.
 * Follows 12-factor app health check patterns with structured responses.
 *
 * @example
 * ```javascript
 * import { createHealthChecks } from '@unrdf/core/health';
 *
 * const health = createHealthChecks({
 *   serviceName: 'unrdf-api',
 *   version: '5.0.1',
 *   dependencies: {
 *     database: async () => await db.ping(),
 *     cache: async () => await redis.ping()
 *   }
 * });
 *
 * const liveness = await health.liveness();
 * const readiness = await health.readiness();
 * const metrics = await health.metrics();
 * ```
 */

import { z } from 'zod';

/**
 * Health check status enumeration
 * @enum {string}
 */
export const HealthStatus = {
  HEALTHY: 'healthy',
  DEGRADED: 'degraded',
  UNHEALTHY: 'unhealthy',
  UNKNOWN: 'unknown'
};

/**
 * Dependency check result schema
 */
const _DependencyCheckSchema = z.object({
  name: z.string(),
  status: z.enum(['connected', 'disconnected', 'degraded', 'unknown']),
  latency: z.number().optional(),
  error: z.string().optional(),
  metadata: z.record(z.unknown()).optional()
});

/**
 * Health check configuration schema
 */
const HealthCheckConfigSchema = z.object({
  serviceName: z.string(),
  version: z.string(),
  environment: z.string().default('production'),
  dependencies: z.record(z.string(), z.any()).default({}),
  customChecks: z.record(z.string(), z.any()).default({})
});

/**
 * Create health check system
 *
 * @param {Object} config - Health check configuration
 * @param {string} config.serviceName - Service name
 * @param {string} config.version - Service version
 * @param {string} [config.environment='production'] - Environment
 * @param {Object<string, Function>} [config.dependencies={}] - Dependency check functions
 * @param {Object<string, Function>} [config.customChecks={}] - Custom health check functions
 * @returns {Object} Health check handlers
 */
export function createHealthChecks(config) {
  const validated = HealthCheckConfigSchema.parse(config);
  const startTime = Date.now();
  let requestCount = 0;
  let errorCount = 0;

  /**
   * Basic liveness check - indicates process is running
   *
   * @returns {Promise<Object>} Liveness status
   */
  async function liveness() {
    requestCount++;

    return {
      status: HealthStatus.HEALTHY,
      timestamp: new Date().toISOString(),
      service: validated.serviceName,
      version: validated.version,
      uptime: Date.now() - startTime,
      pid: process.pid
    };
  }

  /**
   * Readiness check - indicates service can handle requests
   * Checks all dependencies and custom checks
   *
   * @returns {Promise<Object>} Readiness status with dependency details
   */
  async function readiness() {
    requestCount++;
    const checkStart = Date.now();
    const dependencyResults = {};
    let overallStatus = HealthStatus.HEALTHY;

    // Check all dependencies
    for (const [name, checkFn] of Object.entries(validated.dependencies)) {
      const depStart = Date.now();
      try {
        const result = await Promise.race([
          checkFn(),
          new Promise((_, reject) =>
            setTimeout(() => reject(new Error('Health check timeout')), 5000)
          )
        ]);

        dependencyResults[name] = {
          status: result ? 'connected' : 'disconnected',
          latency: Date.now() - depStart,
          ...(typeof result === 'object' && result !== null ? result : {})
        };

        if (!result) {
          overallStatus = HealthStatus.UNHEALTHY;
        }
      } catch (error) {
        errorCount++;
        dependencyResults[name] = {
          status: 'disconnected',
          latency: Date.now() - depStart,
          error: error.message
        };
        overallStatus = HealthStatus.UNHEALTHY;
      }
    }

    // Run custom health checks
    for (const [_name, checkFn] of Object.entries(validated.customChecks)) {
      try {
        const result = await Promise.race([
          checkFn(),
          new Promise((_, reject) =>
            setTimeout(() => reject(new Error('Custom check timeout')), 5000)
          )
        ]);

        if (!result || (typeof result === 'object' && result.status === 'unhealthy')) {
          overallStatus = HealthStatus.DEGRADED;
        }
      } catch (error) {
        errorCount++;
        overallStatus = HealthStatus.DEGRADED;
      }
    }

    return {
      status: overallStatus,
      timestamp: new Date().toISOString(),
      service: validated.serviceName,
      version: validated.version,
      environment: validated.environment,
      uptime: Date.now() - startTime,
      dependencies: dependencyResults,
      checkDuration: Date.now() - checkStart
    };
  }

  /**
   * Metrics endpoint - Prometheus-compatible metrics
   *
   * @returns {Promise<Object>} Service metrics
   */
  async function metrics() {
    const memUsage = process.memoryUsage();
    const cpuUsage = process.cpuUsage();

    return {
      timestamp: new Date().toISOString(),
      service: validated.serviceName,
      version: validated.version,
      uptime: Date.now() - startTime,

      // Request metrics
      requests: {
        total: requestCount,
        errors: errorCount,
        errorRate: requestCount > 0 ? errorCount / requestCount : 0
      },

      // Process metrics
      process: {
        pid: process.pid,
        uptime: process.uptime(),
        platform: process.platform,
        nodeVersion: process.version
      },

      // Memory metrics (in MB)
      memory: {
        rss: Math.round(memUsage.rss / 1024 / 1024 * 100) / 100,
        heapTotal: Math.round(memUsage.heapTotal / 1024 / 1024 * 100) / 100,
        heapUsed: Math.round(memUsage.heapUsed / 1024 / 1024 * 100) / 100,
        external: Math.round(memUsage.external / 1024 / 1024 * 100) / 100,
        arrayBuffers: Math.round((memUsage.arrayBuffers || 0) / 1024 / 1024 * 100) / 100
      },

      // CPU metrics (in microseconds)
      cpu: {
        user: cpuUsage.user,
        system: cpuUsage.system
      }
    };
  }

  /**
   * Prometheus-formatted metrics
   *
   * @returns {Promise<string>} Metrics in Prometheus format
   */
  async function prometheus() {
    const metricsData = await metrics();
    const lines = [];

    // Service info
    lines.push(`# HELP unrdf_service_info Service information`);
    lines.push(`# TYPE unrdf_service_info gauge`);
    lines.push(`unrdf_service_info{service="${metricsData.service}",version="${metricsData.version}",environment="${validated.environment}"} 1`);

    // Uptime
    lines.push(`# HELP unrdf_uptime_seconds Service uptime in seconds`);
    lines.push(`# TYPE unrdf_uptime_seconds counter`);
    lines.push(`unrdf_uptime_seconds ${Math.floor(metricsData.uptime / 1000)}`);

    // Request metrics
    lines.push(`# HELP unrdf_requests_total Total number of requests`);
    lines.push(`# TYPE unrdf_requests_total counter`);
    lines.push(`unrdf_requests_total ${metricsData.requests.total}`);

    lines.push(`# HELP unrdf_errors_total Total number of errors`);
    lines.push(`# TYPE unrdf_errors_total counter`);
    lines.push(`unrdf_errors_total ${metricsData.requests.errors}`);

    lines.push(`# HELP unrdf_error_rate Error rate (0-1)`);
    lines.push(`# TYPE unrdf_error_rate gauge`);
    lines.push(`unrdf_error_rate ${metricsData.requests.errorRate}`);

    // Memory metrics
    lines.push(`# HELP unrdf_memory_rss_bytes Resident set size in bytes`);
    lines.push(`# TYPE unrdf_memory_rss_bytes gauge`);
    lines.push(`unrdf_memory_rss_bytes ${metricsData.memory.rss * 1024 * 1024}`);

    lines.push(`# HELP unrdf_memory_heap_used_bytes Heap used in bytes`);
    lines.push(`# TYPE unrdf_memory_heap_used_bytes gauge`);
    lines.push(`unrdf_memory_heap_used_bytes ${metricsData.memory.heapUsed * 1024 * 1024}`);

    // CPU metrics
    lines.push(`# HELP unrdf_cpu_user_microseconds CPU user time in microseconds`);
    lines.push(`# TYPE unrdf_cpu_user_microseconds counter`);
    lines.push(`unrdf_cpu_user_microseconds ${metricsData.cpu.user}`);

    lines.push(`# HELP unrdf_cpu_system_microseconds CPU system time in microseconds`);
    lines.push(`# TYPE unrdf_cpu_system_microseconds counter`);
    lines.push(`unrdf_cpu_system_microseconds ${metricsData.cpu.system}`);

    return lines.join('\n') + '\n';
  }

  return {
    liveness,
    readiness,
    metrics,
    prometheus
  };
}

/**
 * Express/Fastify middleware factory for health checks
 *
 * @param {Object} config - Health check configuration
 * @returns {Object} Middleware handlers
 */
export function createHealthMiddleware(config) {
  const health = createHealthChecks(config);

  return {
    /**
     * GET /health - Liveness endpoint
     */
    liveness: async (req, res) => {
      const result = await health.liveness();
      res.status(200).json(result);
    },

    /**
     * GET /health/ready - Readiness endpoint
     */
    readiness: async (req, res) => {
      const result = await health.readiness();
      const statusCode = result.status === HealthStatus.HEALTHY ? 200 : 503;
      res.status(statusCode).json(result);
    },

    /**
     * GET /health/metrics - Metrics endpoint (JSON)
     */
    metrics: async (req, res) => {
      const result = await health.metrics();
      res.status(200).json(result);
    },

    /**
     * GET /metrics - Prometheus metrics endpoint
     */
    prometheus: async (req, res) => {
      const result = await health.prometheus();
      res.setHeader('Content-Type', 'text/plain; version=0.0.4');
      res.status(200).send(result);
    }
  };
}

/**
 * Default health check configuration for UNRDF
 *
 * @param {Object} options - Additional configuration
 * @returns {Object} Health check handlers
 */
export function createUnrdfHealthChecks(options = {}) {
  return createHealthChecks({
    serviceName: options.serviceName || 'unrdf',
    version: options.version || '5.0.1',
    environment: process.env.NODE_ENV || 'production',
    ...options
  });
}
