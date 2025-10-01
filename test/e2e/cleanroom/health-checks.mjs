/**
 * @file Container Health Check Utilities
 * @module test/e2e/cleanroom/health-checks
 *
 * @description
 * Health check utilities for validating cleanroom container readiness.
 * Ensures all services are healthy before running tests.
 */

import fetch from 'node-fetch';
import { CLEANROOM_CONFIG } from './testcontainer-stack.mjs';

/**
 * Health checker for cleanroom services
 */
export class HealthChecker {
  constructor(stack) {
    this.stack = stack;
  }

  /**
   * Check PostgreSQL health
   * @returns {Promise<boolean>}
   */
  async checkPostgres() {
    try {
      const postgres = this.stack.getPostgres();
      if (!postgres) return false;

      // PostgreSQL health is checked via container healthcheck
      const inspectResult = await postgres.inspect();
      const health = inspectResult.State?.Health?.Status;

      return health === 'healthy';
    } catch (error) {
      console.error('[HealthCheck] PostgreSQL check failed:', error.message);
      return false;
    }
  }

  /**
   * Check OTEL Collector health
   * @returns {Promise<boolean>}
   */
  async checkOtelCollector() {
    try {
      const otel = this.stack.getOtelCollector();
      if (!otel) return false;

      const host = otel.getHost();
      const port = otel.getMappedPort(CLEANROOM_CONFIG.PORTS.OTEL_HEALTH);
      const response = await fetch(`http://${host}:${port}/`, {
        timeout: 3000
      });

      return response.ok;
    } catch (error) {
      console.error('[HealthCheck] OTEL Collector check failed:', error.message);
      return false;
    }
  }

  /**
   * Check Jaeger health
   * @returns {Promise<boolean>}
   */
  async checkJaeger() {
    try {
      const jaeger = this.stack.getJaeger();
      if (!jaeger) return false;

      const host = jaeger.getHost();
      const port = jaeger.getMappedPort(CLEANROOM_CONFIG.PORTS.JAEGER_HEALTH);
      const response = await fetch(`http://${host}:${port}/`, {
        timeout: 3000
      });

      return response.ok;
    } catch (error) {
      console.error('[HealthCheck] Jaeger check failed:', error.message);
      return false;
    }
  }

  /**
   * Check KGC Sidecar health
   * @returns {Promise<boolean>}
   */
  async checkSidecar() {
    try {
      const sidecar = this.stack.getSidecar();
      if (!sidecar) return false;

      // Check via container healthcheck
      const inspectResult = await sidecar.inspect();
      const health = inspectResult.State?.Health?.Status;

      return health === 'healthy' || health === 'starting';
    } catch (error) {
      console.error('[HealthCheck] Sidecar check failed:', error.message);
      return false;
    }
  }

  /**
   * Check all services
   * @returns {Promise<Object>}
   */
  async checkAll() {
    const results = await Promise.allSettled([
      this.checkPostgres(),
      this.checkOtelCollector(),
      this.checkJaeger(),
      this.checkSidecar()
    ]);

    return {
      postgres: results[0].status === 'fulfilled' && results[0].value,
      otelCollector: results[1].status === 'fulfilled' && results[1].value,
      jaeger: results[2].status === 'fulfilled' && results[2].value,
      sidecar: results[3].status === 'fulfilled' && results[3].value,
      allHealthy: results.every(r => r.status === 'fulfilled' && r.value)
    };
  }

  /**
   * Wait for all services to be healthy
   * @param {number} timeout - Timeout in milliseconds
   * @returns {Promise<boolean>}
   */
  async waitForAllHealthy(timeout = 30000) {
    const startTime = Date.now();
    const checkInterval = 2000;

    console.log('[HealthCheck] Waiting for all services to be healthy...');

    while (Date.now() - startTime < timeout) {
      const health = await this.checkAll();

      console.log('[HealthCheck] Status:', {
        postgres: health.postgres ? '✓' : '✗',
        otelCollector: health.otelCollector ? '✓' : '✗',
        jaeger: health.jaeger ? '✓' : '✗',
        sidecar: health.sidecar ? '✓' : '✗'
      });

      if (health.allHealthy) {
        console.log('[HealthCheck] All services are healthy');
        return true;
      }

      await new Promise(resolve => setTimeout(resolve, checkInterval));
    }

    console.error('[HealthCheck] Services did not become healthy within timeout');
    return false;
  }
}

export default HealthChecker;
