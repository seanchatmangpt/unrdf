/**
 * @file PM4Py Integration Tests
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  getPM4Py,
  checkPM4PyAvailable,
  recordUNRDFMetrics,
  initializeUNRDFMetrics,
  recordMetric,
  getSystemMetrics,
  getPrometheusMetrics
} from '../../src/pm4py.mjs';

import {
  monitorOperation,
  UNRDFOperation,
  checkMonitoringHealth,
  initializeMonitoring,
  createMonitoringContext
} from '../../src/monitoring.mjs';

describe('PM4Py Integration', () => {
  describe('Availability Checks', () => {
    it('should check pm4py availability', async () => {
      const available = await checkPM4PyAvailable();
      expect(typeof available).toBe('boolean');
    });

    it('should get PM4Py instance', () => {
      const pm4py = getPM4Py();
      expect(pm4py).toBeDefined();
      expect(pm4py).toHaveProperty('pythonPath');
      expect(pm4py).toHaveProperty('pm4pyPath');
    });
  });

  describe.skipIf(!process.env.PM4PY_PATH, 'Metric Recording', () => {
    beforeEach(async () => {
      // Initialize UNRDF metrics before each test
      await initializeUNRDFMetrics();
    });

    it('should record UNRDF metrics', async () => {
      const success = await recordUNRDFMetrics('test_operation', {
        graph: 'test-graph'
      });

      expect(success).toBe(true);
    });

    it('should record custom metric', async () => {
      const success = await recordMetric(
        'test_metric',
        42,
        'gauge',
        { test: 'integration' }
      );

      expect(success).toBe(true);
    });

    it('should get system metrics', async () => {
      const metrics = await getSystemMetrics();

      expect(metrics).toBeDefined();
      expect(metrics).toHaveProperty('cpu_percent');
      expect(metrics).toHaveProperty('memory_usage_mb');
      expect(metrics).toHaveProperty('uptime_seconds');
    });

    it('should get Prometheus metrics', async () => {
      const metrics = await getPrometheusMetrics();

      expect(typeof metrics).toBe('string');
      expect(metrics.length).toBeGreaterThan(0);
      expect(metrics).toContain('# HELP');
      expect(metrics).toContain('# TYPE');
    });
  });

  describe('Monitoring Context', () => {
    it('should create monitoring context', () => {
      const context = createMonitoringContext('test-service', 'test-operation');

      expect(context).toBeDefined();
      expect(context.service).toBe('test-service');
      expect(context.operation).toBe('test-operation');
      expect(context).toHaveProperty('startTime');
      expect(context).toHaveProperty('metadata');
    });
  });

  describe.skipIf(!process.env.PM4PY_PATH, 'Operation Monitoring', () => {
    it('should monitor successful operation', async () => {
      let executed = false;

      const result = await monitorOperation(
        UNRDFOperation.SPARQL_QUERY,
        async () => {
          executed = true;
          return { success: true, data: 'test' };
        },
        { graph: 'test-graph' }
      );

      expect(executed).toBe(true);
      expect(result).toBeDefined();
      expect(result.success).toBe(true);
    });

    it('should monitor failed operation', async () => {
      let executed = false;

      await expect(
        monitorOperation(
          UNRDFOperation.GRAPH_LOAD,
          async () => {
            executed = true;
            throw new Error('Test error');
          },
          { graph: 'test-graph' }
        )
      ).rejects.toThrow('Test error');

      expect(executed).toBe(true);
    });

    it('should record operation duration', async () => {
      const result = await monitorOperation(
        UNRDFOperation.HOOK_EXECUTE,
        async () => {
          // Simulate work
          await new Promise(resolve => setTimeout(resolve, 100));
          return { completed: true };
        },
        { hook: 'test-hook' }
      );

      expect(result).toBeDefined();
      expect(result.completed).toBe(true);
      // Duration should be recorded automatically
    });
  });

  describe('Monitoring Health', () => {
    it('should check monitoring health', async () => {
      const health = await checkMonitoringHealth();

      expect(health).toBeDefined();
      expect(health).toHaveProperty('pm4py');
      expect(health).toHaveProperty('prometheus');
      expect(health).toHaveProperty('jaeger');
      expect(health).toHaveProperty('otelCollector');
    });
  });
});
