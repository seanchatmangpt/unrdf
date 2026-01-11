/**
 * @file Nitro Monitor Tests
 * @module @unrdf/yawl/test/integrations/nitro-monitor
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { randomUUID } from 'crypto';
import { EventEmitter } from 'events';
import { NitroMonitor } from '../../src/integrations/nitro-monitor.mjs';

describe('NitroMonitor', () => {
  let monitor;
  let mockEngine;

  beforeEach(() => {
    mockEngine = new EventEmitter();
    mockEngine.cases = new Map();
    mockEngine.resources = new Map();

    monitor = new NitroMonitor({
      engine: mockEngine,
      collectionIntervalMs: 100,
      retentionMs: 1000,
    });
  });

  afterEach(() => {
    if (monitor.isRunning) {
      monitor.stop();
    }
  });

  describe('Initialization', () => {
    it('should create monitor with valid config', () => {
      expect(monitor).toBeDefined();
      expect(monitor.engine).toBe(mockEngine);
      expect(monitor.isRunning).toBe(false);
    });

    it('should throw error without engine', () => {
      expect(() => new NitroMonitor({})).toThrow('WorkflowEngine instance required');
    });

    it('should auto-start if configured', () => {
      const autoMonitor = new NitroMonitor({
        engine: mockEngine,
        autoStart: true,
      });

      expect(autoMonitor.isRunning).toBe(true);
      autoMonitor.stop();
    });
  });

  describe('Lifecycle', () => {
    it('should start monitoring', () => {
      monitor.start();
      expect(monitor.isRunning).toBe(true);
      expect(monitor.collectionInterval).toBeDefined();
    });

    it('should stop monitoring', () => {
      monitor.start();
      monitor.stop();
      expect(monitor.isRunning).toBe(false);
      expect(monitor.collectionInterval).toBeNull();
    });

    it('should throw error if starting already running monitor', () => {
      monitor.start();
      expect(() => monitor.start()).toThrow('already running');
    });

    it('should not throw if stopping non-running monitor', () => {
      expect(() => monitor.stop()).not.toThrow();
    });
  });

  describe('Task Metrics', () => {
    it('should record work item completion', () => {
      const taskId = 'task-1';
      const workItemId = randomUUID();

      mockEngine.emit('workitem:started', { taskId, workItemId });

      setTimeout(() => {
        mockEngine.emit('workitem:completed', { taskId, workItemId });
      }, 50);

      return new Promise(resolve => {
        setTimeout(() => {
          const metrics = monitor.getTaskMetrics(taskId);
          expect(metrics).toBeDefined();
          expect(metrics.totalExecutions).toBe(1);
          expect(metrics.successfulExecutions).toBe(1);
          expect(metrics.failedExecutions).toBe(0);
          expect(metrics.avgDurationMs).toBeGreaterThan(0);
          resolve();
        }, 100);
      });
    });

    it('should record work item failure', () => {
      const taskId = 'task-1';
      const workItemId = randomUUID();

      mockEngine.emit('workitem:started', { taskId, workItemId });

      setTimeout(() => {
        mockEngine.emit('workitem:failed', { taskId, workItemId });
      }, 50);

      return new Promise(resolve => {
        setTimeout(() => {
          const metrics = monitor.getTaskMetrics(taskId);
          expect(metrics).toBeDefined();
          expect(metrics.totalExecutions).toBe(1);
          expect(metrics.successfulExecutions).toBe(0);
          expect(metrics.failedExecutions).toBe(1);
          resolve();
        }, 100);
      });
    });

    it('should track multiple executions', () => {
      const taskId = 'task-1';

      for (let i = 0; i < 3; i++) {
        const workItemId = randomUUID();
        mockEngine.emit('workitem:started', { taskId, workItemId });
        mockEngine.emit('workitem:completed', { taskId, workItemId });
      }

      const metrics = monitor.getTaskMetrics(taskId);
      expect(metrics.totalExecutions).toBe(3);
      expect(metrics.successfulExecutions).toBe(3);
    });

    it('should calculate min/max/avg duration', () => {
      const taskId = 'task-1';

      const durations = [100, 200, 300];
      durations.forEach((duration, _i) => {
        const workItemId = randomUUID();
        mockEngine.emit('workitem:started', { taskId, workItemId });

        setTimeout(() => {
          mockEngine.emit('workitem:completed', { taskId, workItemId });
        }, duration);
      });

      return new Promise(resolve => {
        setTimeout(() => {
          const metrics = monitor.getTaskMetrics(taskId);
          expect(metrics.minDurationMs).toBeGreaterThan(0);
          expect(metrics.maxDurationMs).toBeGreaterThan(metrics.minDurationMs);
          expect(metrics.avgDurationMs).toBeGreaterThan(0);
          resolve();
        }, 500);
      });
    });

    it('should get all task metrics', () => {
      const workItemId1 = randomUUID();
      const workItemId2 = randomUUID();

      mockEngine.emit('workitem:started', { taskId: 'task-1', workItemId: workItemId1 });
      mockEngine.emit('workitem:completed', { taskId: 'task-1', workItemId: workItemId1 });

      mockEngine.emit('workitem:started', { taskId: 'task-2', workItemId: workItemId2 });
      mockEngine.emit('workitem:completed', { taskId: 'task-2', workItemId: workItemId2 });

      const allMetrics = monitor.getTaskMetrics();
      expect(Array.isArray(allMetrics)).toBe(true);
      expect(allMetrics.length).toBeGreaterThanOrEqual(2);
    });
  });

  describe('Resource Metrics', () => {
    it('should track resource metrics', () => {
      mockEngine.resources.set('resource-1', {
        id: 'resource-1',
        capacity: 5,
        currentLoad: 2,
      });

      monitor.start();

      return new Promise(resolve => {
        setTimeout(() => {
          const metrics = monitor.getResourceMetrics('resource-1');
          expect(metrics).toBeDefined();
          expect(metrics.currentLoad).toBe(0.4);
          expect(metrics.activeWorkItems).toBe(2);
          resolve();
        }, 150);
      });
    });

    it('should get all resource metrics', () => {
      mockEngine.resources.set('resource-1', {
        id: 'resource-1',
        capacity: 5,
        currentLoad: 2,
      });
      mockEngine.resources.set('resource-2', {
        id: 'resource-2',
        capacity: 10,
        currentLoad: 5,
      });

      monitor.start();

      return new Promise(resolve => {
        setTimeout(() => {
          const allMetrics = monitor.getResourceMetrics();
          expect(Array.isArray(allMetrics)).toBe(true);
          expect(allMetrics.length).toBe(2);
          resolve();
        }, 150);
      });
    });
  });

  describe('Health Status', () => {
    it('should return healthy status with no failures', () => {
      mockEngine.cases.set('case-1', {
        id: 'case-1',
        status: 'active',
        workItems: [],
      });

      const health = monitor.getHealthStatus();
      expect(health).toBeDefined();
      expect(health.status).toBe('healthy');
      expect(health.activeCases).toBe(1);
      expect(health.failedWorkItems).toBe(0);
    });

    it('should return degraded status with some failures', () => {
      mockEngine.cases.set('case-1', {
        id: 'case-1',
        status: 'active',
        workItems: [
          { id: 'wi-1', status: 'started' },
          { id: 'wi-2', status: 'started' },
          { id: 'wi-3', status: 'started' },
          { id: 'wi-4', status: 'started' },
          { id: 'wi-5', status: 'started' },
          { id: 'wi-6', status: 'failed' },
        ],
      });

      const health = monitor.getHealthStatus();
      expect(health.status).toBe('degraded');
      expect(health.failedWorkItems).toBe(1);
    });

    it('should return unhealthy status with many failures', () => {
      mockEngine.cases.set('case-1', {
        id: 'case-1',
        status: 'active',
        workItems: [
          { id: 'wi-1', status: 'failed' },
          { id: 'wi-2', status: 'failed' },
          { id: 'wi-3', status: 'failed' },
          { id: 'wi-4', status: 'started' },
        ],
      });

      const health = monitor.getHealthStatus();
      expect(health.status).toBe('unhealthy');
      expect(health.failedWorkItems).toBe(3);
    });

    it('should track work item counts', () => {
      mockEngine.cases.set('case-1', {
        id: 'case-1',
        status: 'active',
        workItems: [
          { id: 'wi-1', status: 'enabled' },
          { id: 'wi-2', status: 'enabled' },
          { id: 'wi-3', status: 'started' },
          { id: 'wi-4', status: 'failed' },
        ],
      });

      const health = monitor.getHealthStatus();
      expect(health.enabledWorkItems).toBe(2);
      expect(health.activeWorkItems).toBe(1);
      expect(health.failedWorkItems).toBe(1);
    });

    it('should include memory usage', () => {
      const health = monitor.getHealthStatus();
      expect(health.memoryUsageBytes).toBeDefined();
      expect(health.memoryUsageBytes).toBeGreaterThan(0);
    });
  });

  describe('Alerts', () => {
    it('should add alert', () => {
      const alert = monitor.addAlert({
        type: 'high_failure_rate',
        threshold: 0.1,
        operator: 'gt',
      });

      expect(alert).toBeDefined();
      expect(alert.id).toBeDefined();
      expect(alert.type).toBe('high_failure_rate');
      expect(alert.status).toBe('active');
    });

    it('should remove alert', () => {
      const alert = monitor.addAlert({
        type: 'high_failure_rate',
        threshold: 0.1,
        operator: 'gt',
      });

      const removed = monitor.removeAlert(alert.id);
      expect(removed).toBe(true);

      const alerts = monitor.getAlerts();
      expect(alerts).toHaveLength(0);
    });

    it('should trigger high failure rate alert', async () => {
      const onAlert = vi.fn();
      const alertMonitor = new NitroMonitor({
        engine: mockEngine,
        collectionIntervalMs: 100,
        onAlert,
      });

      alertMonitor.addAlert({
        type: 'high_failure_rate',
        threshold: 0.2,
        operator: 'gt',
      });

      mockEngine.cases.set('case-1', {
        id: 'case-1',
        status: 'active',
        workItems: [
          { id: 'wi-1', status: 'started' },
          { id: 'wi-2', status: 'failed' },
          { id: 'wi-3', status: 'failed' },
        ],
      });

      alertMonitor.start();

      await new Promise(resolve => setTimeout(resolve, 150));

      expect(onAlert).toHaveBeenCalled();
      const triggeredAlerts = alertMonitor.getTriggeredAlerts();
      expect(triggeredAlerts.length).toBeGreaterThan(0);

      alertMonitor.stop();
    });

    it('should trigger system degraded alert', async () => {
      const onAlert = vi.fn();
      const alertMonitor = new NitroMonitor({
        engine: mockEngine,
        collectionIntervalMs: 100,
        onAlert,
      });

      alertMonitor.addAlert({
        type: 'system_degraded',
        threshold: 0,
        operator: 'gt',
      });

      mockEngine.cases.set('case-1', {
        id: 'case-1',
        status: 'active',
        workItems: [
          { id: 'wi-1', status: 'started' },
          { id: 'wi-2', status: 'started' },
          { id: 'wi-3', status: 'started' },
          { id: 'wi-4', status: 'started' },
          { id: 'wi-5', status: 'started' },
          { id: 'wi-6', status: 'failed' },
        ],
      });

      alertMonitor.start();

      await new Promise(resolve => setTimeout(resolve, 150));

      expect(onAlert).toHaveBeenCalled();

      alertMonitor.stop();
    });

    it('should get all alerts', () => {
      monitor.addAlert({
        type: 'high_failure_rate',
        threshold: 0.1,
        operator: 'gt',
      });
      monitor.addAlert({
        type: 'system_degraded',
        threshold: 0,
        operator: 'gt',
      });

      const alerts = monitor.getAlerts();
      expect(alerts).toHaveLength(2);
    });
  });

  describe('OTEL Metrics Export', () => {
    it('should export OTEL-compatible metrics', () => {
      const taskId = 'task-1';
      const workItemId = randomUUID();

      mockEngine.emit('workitem:started', { taskId, workItemId });
      mockEngine.emit('workitem:completed', { taskId, workItemId });

      const otelMetrics = monitor.exportOTELMetrics();

      expect(Array.isArray(otelMetrics)).toBe(true);
      expect(otelMetrics.length).toBeGreaterThan(0);

      const execMetric = otelMetrics.find(m => m.name === 'yawl.task.executions.total');
      expect(execMetric).toBeDefined();
      expect(execMetric.type).toBe('counter');
      expect(execMetric.labels.task_id).toBe(taskId);
    });

    it('should export case metrics', () => {
      mockEngine.cases.set('case-1', {
        id: 'case-1',
        status: 'active',
        workItems: [],
      });

      const otelMetrics = monitor.exportOTELMetrics();

      const casesMetric = otelMetrics.find(m => m.name === 'yawl.cases.active');
      expect(casesMetric).toBeDefined();
      expect(casesMetric.type).toBe('gauge');
      expect(casesMetric.value).toBe(1);
    });

    it('should export memory metrics', () => {
      const otelMetrics = monitor.exportOTELMetrics();

      const memoryMetric = otelMetrics.find(m => m.name === 'yawl.memory.heap_used');
      expect(memoryMetric).toBeDefined();
      expect(memoryMetric.type).toBe('gauge');
      expect(memoryMetric.value).toBeGreaterThan(0);
    });

    it('should include proper OTEL metadata', () => {
      const taskId = 'task-1';
      const workItemId = randomUUID();

      mockEngine.emit('workitem:started', { taskId, workItemId });
      mockEngine.emit('workitem:completed', { taskId, workItemId });

      const otelMetrics = monitor.exportOTELMetrics();

      for (const metric of otelMetrics) {
        expect(metric.name).toBeDefined();
        expect(metric.type).toBeDefined();
        expect(metric.value).toBeDefined();
        expect(metric.timestamp).toBeInstanceOf(Date);
      }
    });
  });

  describe('Metrics History', () => {
    it('should record metrics history', async () => {
      monitor.start();

      await new Promise(resolve => setTimeout(resolve, 250));

      const history = monitor.getMetricsHistory();
      expect(Array.isArray(history)).toBe(true);
      expect(history.length).toBeGreaterThan(0);
    });

    it('should limit metrics history', () => {
      monitor.start();

      return new Promise(resolve => {
        setTimeout(() => {
          const history = monitor.getMetricsHistory();
          expect(history.length).toBeLessThanOrEqual(1000);
          resolve();
        }, 150);
      });
    });

    it('should prune old metrics', async () => {
      const shortRetentionMonitor = new NitroMonitor({
        engine: mockEngine,
        collectionIntervalMs: 50,
        retentionMs: 100,
      });

      shortRetentionMonitor.start();

      await new Promise(resolve => setTimeout(resolve, 200));

      const history = shortRetentionMonitor.getMetricsHistory();
      const now = Date.now();

      for (const metric of history) {
        expect(now - metric.timestamp.getTime()).toBeLessThanOrEqual(100);
      }

      shortRetentionMonitor.stop();
    });
  });

  describe('Error Handling', () => {
    it('should call onError callback on collection error', async () => {
      const onError = vi.fn();

      const badEngine = new EventEmitter();
      badEngine.cases = new Map();

      Object.defineProperty(badEngine, 'resources', {
        get() {
          throw new Error('Resource access error');
        }
      });

      const errorMonitor = new NitroMonitor({
        engine: badEngine,
        collectionIntervalMs: 100,
        onError,
      });

      errorMonitor.start();

      await new Promise(resolve => setTimeout(resolve, 150));

      expect(onError).toHaveBeenCalled();

      errorMonitor.stop();
    });
  });
});
