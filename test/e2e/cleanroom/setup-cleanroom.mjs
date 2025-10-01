/**
 * @file Cleanroom Environment Setup and Initialization
 * @module test/e2e/cleanroom/setup-cleanroom
 *
 * @description
 * High-level cleanroom environment manager for integration testing.
 * Provides simplified API for starting/stopping stack and running tests.
 *
 * @example
 * ```javascript
 * import { CleanroomEnvironment } from './setup-cleanroom.mjs';
 *
 * const env = new CleanroomEnvironment();
 * await env.initialize();
 *
 * // Execute CLI command with tracing
 * const result = await env.executeCli('hook', 'evaluate', {...});
 *
 * // Query traces
 * const traces = await env.queryTraces({ service: 'kgc-sidecar' });
 *
 * await env.shutdown();
 * ```
 */

import { CleanroomStack } from './testcontainer-stack.mjs';
import { HealthChecker } from './health-checks.mjs';
import fetch from 'node-fetch';

/**
 * Cleanroom environment manager
 */
export class CleanroomEnvironment {
  constructor(options = {}) {
    this.stack = new CleanroomStack();
    this.healthChecker = null;
    this.initialized = false;
    this.options = {
      waitForHealthy: true,
      healthCheckTimeout: 30000,
      ...options
    };
  }

  /**
   * Initialize cleanroom environment
   * @returns {Promise<void>}
   */
  async initialize() {
    console.log('[CleanroomEnv] Initializing environment...');

    // Start stack
    await this.stack.start();

    // Initialize health checker
    this.healthChecker = new HealthChecker(this.stack);

    // Wait for all services to be healthy
    if (this.options.waitForHealthy) {
      const healthy = await this.healthChecker.waitForAllHealthy(
        this.options.healthCheckTimeout
      );

      if (!healthy) {
        throw new Error('Services did not become healthy within timeout');
      }
    }

    this.initialized = true;
    console.log('[CleanroomEnv] Environment initialized successfully');
  }

  /**
   * Shutdown cleanroom environment
   * @returns {Promise<void>}
   */
  async shutdown() {
    console.log('[CleanroomEnv] Shutting down environment...');

    if (this.stack) {
      await this.stack.cleanup();
    }

    this.initialized = false;
    console.log('[CleanroomEnv] Environment shutdown complete');
  }

  /**
   * Get service endpoints
   * @returns {Object}
   */
  getEndpoints() {
    return {
      postgres: this.stack.getPostgresUrl(),
      jaegerUi: this.stack.getJaegerUrl(),
      otelCollector: this.stack.getOtelEndpoint(),
      sidecarGrpc: this.stack.getSidecarEndpoint()
    };
  }

  /**
   * Query traces from Jaeger
   * @param {Object} query - Jaeger query parameters
   * @returns {Promise<Array>}
   */
  async queryTraces(query = {}) {
    const jaegerUrl = this.stack.getJaegerUrl();

    const params = new URLSearchParams({
      service: query.service || 'kgc-sidecar',
      operation: query.operation || '',
      limit: query.limit || 20,
      lookback: query.lookback || '1h',
      maxDuration: query.maxDuration || '',
      minDuration: query.minDuration || '',
      tags: query.tags ? JSON.stringify(query.tags) : ''
    });

    const response = await fetch(`${jaegerUrl}/api/traces?${params}`);

    if (!response.ok) {
      throw new Error(`Failed to query traces: ${response.statusText}`);
    }

    const result = await response.json();
    return result.data || [];
  }

  /**
   * Get specific trace by ID
   * @param {string} traceId - Trace ID
   * @returns {Promise<Object>}
   */
  async getTrace(traceId) {
    const jaegerUrl = this.stack.getJaegerUrl();
    const response = await fetch(`${jaegerUrl}/api/traces/${traceId}`);

    if (!response.ok) {
      throw new Error(`Failed to get trace: ${response.statusText}`);
    }

    const result = await response.json();
    return result.data?.[0] || null;
  }

  /**
   * Get Jaeger services
   * @returns {Promise<Array<string>>}
   */
  async getServices() {
    const jaegerUrl = this.stack.getJaegerUrl();
    const response = await fetch(`${jaegerUrl}/api/services`);

    if (!response.ok) {
      throw new Error(`Failed to get services: ${response.statusText}`);
    }

    const result = await response.json();
    return result.data || [];
  }

  /**
   * Get operations for a service
   * @param {string} service - Service name
   * @returns {Promise<Array<string>>}
   */
  async getOperations(service) {
    const jaegerUrl = this.stack.getJaegerUrl();
    const response = await fetch(`${jaegerUrl}/api/services/${service}/operations`);

    if (!response.ok) {
      throw new Error(`Failed to get operations: ${response.statusText}`);
    }

    const result = await response.json();
    return result.data || [];
  }

  /**
   * Execute health check for all services
   * @returns {Promise<Object>}
   */
  async checkHealth() {
    if (!this.healthChecker) {
      throw new Error('Environment not initialized');
    }

    return await this.healthChecker.checkAll();
  }

  /**
   * Get PostgreSQL client
   * @returns {Object}
   */
  getPostgresClient() {
    // This would return a pg.Client instance
    // Implementation depends on whether you want to use pg or another driver
    return {
      connectionString: this.stack.getPostgresUrl()
    };
  }

  /**
   * Get gRPC client for sidecar
   * @returns {Object}
   */
  getSidecarClient() {
    // This would return a gRPC client instance
    // Implementation depends on your protobuf definitions
    return {
      endpoint: this.stack.getSidecarEndpoint()
    };
  }

  /**
   * Wait for specific condition
   * @param {Function} condition - Async function that returns boolean
   * @param {number} timeout - Timeout in milliseconds
   * @param {number} interval - Check interval in milliseconds
   * @returns {Promise<boolean>}
   */
  async waitFor(condition, timeout = 30000, interval = 1000) {
    const startTime = Date.now();

    while (Date.now() - startTime < timeout) {
      try {
        if (await condition()) {
          return true;
        }
      } catch (error) {
        // Condition check failed, continue waiting
      }

      await new Promise(resolve => setTimeout(resolve, interval));
    }

    return false;
  }

  /**
   * Execute test scenario with automatic tracing
   * @param {string} name - Scenario name
   * @param {Function} fn - Async test function
   * @returns {Promise<Object>}
   */
  async executeScenario(name, fn) {
    const startTime = Date.now();
    const testRunId = `test-${Date.now()}`;

    console.log(`[CleanroomEnv] Executing scenario: ${name}`);

    try {
      const result = await fn({
        stack: this.stack,
        endpoints: this.getEndpoints(),
        testRunId
      });

      const duration = Date.now() - startTime;
      console.log(`[CleanroomEnv] Scenario completed in ${duration}ms`);

      return {
        success: true,
        duration,
        result,
        testRunId
      };
    } catch (error) {
      const duration = Date.now() - startTime;
      console.error(`[CleanroomEnv] Scenario failed after ${duration}ms:`, error);

      return {
        success: false,
        duration,
        error: error.message,
        testRunId
      };
    }
  }

  /**
   * Get environment statistics
   * @returns {Object}
   */
  getStats() {
    return {
      initialized: this.initialized,
      startupDuration: this.stack.getStartupDuration(),
      endpoints: this.getEndpoints()
    };
  }
}

/**
 * Create and initialize cleanroom environment
 * @param {Object} options - Environment options
 * @returns {Promise<CleanroomEnvironment>}
 */
export async function createCleanroomEnvironment(options = {}) {
  const env = new CleanroomEnvironment(options);
  await env.initialize();
  return env;
}

/**
 * Run test with cleanroom environment (auto cleanup)
 * @param {Function} testFn - Test function
 * @param {Object} options - Environment options
 * @returns {Promise<any>}
 */
export async function withCleanroom(testFn, options = {}) {
  const env = new CleanroomEnvironment(options);

  try {
    await env.initialize();
    return await testFn(env);
  } finally {
    await env.shutdown();
  }
}

export default CleanroomEnvironment;
