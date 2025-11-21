/**
 * @file Tests for adaptive-monitor utility
 * @description TRIZ #21 - Skipping pattern implementation tests
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import {
  AdaptiveMonitor,
  createAdaptiveMonitor,
  createMemoryMonitor,
  createEventLoopMonitor,
  MonitorOrchestrator,
  createMonitorOrchestrator,
  DEFAULT_CONFIG,
  HealthStatus,
  MonitorEvents
} from '../../../src/knowledge-engine/utils/adaptive-monitor.mjs';

describe('AdaptiveMonitor', () => {
  let monitor;

  beforeEach(() => {
    monitor = new AdaptiveMonitor();
    vi.useFakeTimers();
  });

  afterEach(() => {
    if (monitor && monitor.running) {
      monitor.stop();
    }
    vi.useRealTimers();
  });

  describe('constructor', () => {
    it('should create monitor with default config', () => {
      const m = new AdaptiveMonitor();
      expect(m.config.baseInterval).toBe(DEFAULT_CONFIG.baseInterval);
      expect(m.config.minInterval).toBe(DEFAULT_CONFIG.minInterval);
      expect(m.config.maxInterval).toBe(DEFAULT_CONFIG.maxInterval);
    });

    it('should accept custom config', () => {
      const m = new AdaptiveMonitor({
        baseInterval: 30000,
        minInterval: 500,
        maxInterval: 600000
      });
      expect(m.config.baseInterval).toBe(30000);
      expect(m.config.minInterval).toBe(500);
      expect(m.config.maxInterval).toBe(600000);
    });

    it('should initialize state correctly', () => {
      const m = new AdaptiveMonitor();
      expect(m.running).toBe(false);
      expect(m.paused).toBe(false);
      expect(m.status).toBe(HealthStatus.UNKNOWN);
      expect(m.healthHistory).toHaveLength(0);
      expect(m.totalChecks).toBe(0);
    });
  });

  describe('DEFAULT_CONFIG', () => {
    it('should have expected defaults', () => {
      expect(DEFAULT_CONFIG.baseInterval).toBe(60000);
      expect(DEFAULT_CONFIG.minInterval).toBe(1000);
      expect(DEFAULT_CONFIG.maxInterval).toBe(300000);
      expect(DEFAULT_CONFIG.healthyBackoffFactor).toBe(1.5);
      expect(DEFAULT_CONFIG.unhealthyRushFactor).toBe(0.5);
      expect(DEFAULT_CONFIG.stableThreshold).toBe(3);
      expect(DEFAULT_CONFIG.criticalThreshold).toBe(3);
    });
  });

  describe('HealthStatus', () => {
    it('should export all health statuses', () => {
      expect(HealthStatus.HEALTHY).toBe('healthy');
      expect(HealthStatus.UNHEALTHY).toBe('unhealthy');
      expect(HealthStatus.UNKNOWN).toBe('unknown');
      expect(HealthStatus.STABLE).toBe('stable');
      expect(HealthStatus.CRITICAL).toBe('critical');
      expect(HealthStatus.RECOVERING).toBe('recovering');
    });
  });

  describe('MonitorEvents', () => {
    it('should export all event types', () => {
      expect(MonitorEvents.HEALTH).toBe('health');
      expect(MonitorEvents.STATUS_CHANGE).toBe('statusChange');
      expect(MonitorEvents.INTERVAL_CHANGE).toBe('intervalChange');
      expect(MonitorEvents.START).toBe('start');
      expect(MonitorEvents.STOP).toBe('stop');
      expect(MonitorEvents.ERROR).toBe('error');
      expect(MonitorEvents.CRITICAL).toBe('critical');
      expect(MonitorEvents.STABLE).toBe('stable');
      expect(MonitorEvents.RECOVERY).toBe('recovery');
    });
  });

  describe('start()', () => {
    it('should start monitoring', () => {
      const checkFn = vi.fn().mockResolvedValue(true);
      monitor.start(checkFn);
      expect(monitor.running).toBe(true);
      expect(monitor.startTime).toBeDefined();
    });

    it('should throw if checkFn is not a function', () => {
      expect(() => monitor.start('not a function')).toThrow(TypeError);
      expect(() => monitor.start(null)).toThrow(TypeError);
      expect(() => monitor.start(123)).toThrow(TypeError);
    });

    it('should emit start event', () => {
      const startHandler = vi.fn();
      monitor.on(MonitorEvents.START, startHandler);

      monitor.start(() => true);
      expect(startHandler).toHaveBeenCalledWith(
        expect.objectContaining({
          interval: expect.any(Number),
          timestamp: expect.any(Number)
        })
      );
    });

    it('should return this for chaining', () => {
      const result = monitor.start(() => true);
      expect(result).toBe(monitor);
    });

    it('should not restart if already running', () => {
      const checkFn = vi.fn().mockResolvedValue(true);
      monitor.start(checkFn);
      const startTime = monitor.startTime;

      monitor.start(checkFn);
      expect(monitor.startTime).toBe(startTime);
    });
  });

  describe('stop()', () => {
    it('should stop monitoring', () => {
      monitor.start(() => true);
      expect(monitor.running).toBe(true);

      monitor.stop();
      expect(monitor.running).toBe(false);
    });

    it('should emit stop event', () => {
      const stopHandler = vi.fn();
      monitor.on(MonitorEvents.STOP, stopHandler);

      monitor.start(() => true);
      monitor.stop();

      expect(stopHandler).toHaveBeenCalledWith(
        expect.objectContaining({
          duration: expect.any(Number),
          totalChecks: expect.any(Number),
          timestamp: expect.any(Number)
        })
      );
    });

    it('should return this for chaining', () => {
      monitor.start(() => true);
      const result = monitor.stop();
      expect(result).toBe(monitor);
    });

    it('should handle stop when not running', () => {
      const result = monitor.stop();
      expect(result).toBe(monitor);
    });
  });

  describe('pause() and resume()', () => {
    it('should pause monitoring', () => {
      monitor.start(() => true);
      monitor.pause();
      expect(monitor.paused).toBe(true);
    });

    it('should resume monitoring', () => {
      monitor.start(() => true);
      monitor.pause();
      expect(monitor.paused).toBe(true);

      monitor.resume();
      expect(monitor.paused).toBe(false);
    });

    it('should return this for chaining', () => {
      monitor.start(() => true);
      expect(monitor.pause()).toBe(monitor);
      expect(monitor.resume()).toBe(monitor);
    });

    it('should handle pause when not running', () => {
      const result = monitor.pause();
      expect(result).toBe(monitor);
      expect(monitor.paused).toBe(false);
    });

    it('should handle resume when not paused', () => {
      monitor.start(() => true);
      const result = monitor.resume();
      expect(result).toBe(monitor);
    });
  });

  describe('reset()', () => {
    it('should reset monitor to initial state', () => {
      monitor.start(() => true);
      monitor.totalChecks = 10;
      monitor.consecutiveHealthy = 5;

      monitor.reset();

      expect(monitor.running).toBe(false);
      expect(monitor.totalChecks).toBe(0);
      expect(monitor.consecutiveHealthy).toBe(0);
      expect(monitor.status).toBe(HealthStatus.UNKNOWN);
      expect(monitor.healthHistory).toHaveLength(0);
    });

    it('should return this for chaining', () => {
      const result = monitor.reset();
      expect(result).toBe(monitor);
    });
  });

  describe('checkNow()', () => {
    it('should execute immediate check', async () => {
      vi.useRealTimers();
      const checkFn = vi.fn().mockResolvedValue(true);
      monitor.start(checkFn);

      // Wait for initial check
      await vi.waitFor(() => expect(checkFn).toHaveBeenCalled());

      const result = await monitor.checkNow();
      expect(result).toBeDefined();
      expect(result.healthy).toBe(true);
      monitor.stop();
    });

    it('should return null if not running', async () => {
      const result = await monitor.checkNow();
      expect(result).toBeNull();
    });
  });

  describe('getStats()', () => {
    it('should return statistics object', () => {
      const stats = monitor.getStats();

      expect(stats).toHaveProperty('running');
      expect(stats).toHaveProperty('paused');
      expect(stats).toHaveProperty('status');
      expect(stats).toHaveProperty('currentInterval');
      expect(stats).toHaveProperty('uptime');
      expect(stats).toHaveProperty('totalChecks');
      expect(stats).toHaveProperty('totalHealthy');
      expect(stats).toHaveProperty('totalUnhealthy');
      expect(stats).toHaveProperty('healthRate');
      expect(stats).toHaveProperty('historySize');
    });

    it('should show correct stats after checks', async () => {
      vi.useRealTimers();
      const checkFn = vi.fn().mockResolvedValue(true);
      monitor.start(checkFn);

      // Wait for check to complete and record
      await vi.waitFor(() => expect(monitor.totalChecks).toBeGreaterThan(0), { timeout: 2000 });

      const stats = monitor.getStats();
      expect(stats.running).toBe(true);
      expect(stats.totalChecks).toBeGreaterThan(0);
      monitor.stop();
    });
  });

  describe('getHistory()', () => {
    it('should return empty array initially', () => {
      expect(monitor.getHistory()).toHaveLength(0);
    });

    it('should respect limit parameter', async () => {
      vi.useRealTimers();
      const checkFn = vi.fn().mockResolvedValue(true);
      monitor = new AdaptiveMonitor({ baseInterval: 50 });
      monitor.start(checkFn);

      await vi.waitFor(() => expect(checkFn.mock.calls.length).toBeGreaterThanOrEqual(3));
      monitor.stop();

      const history = monitor.getHistory(2);
      expect(history.length).toBeLessThanOrEqual(2);
    });
  });

  describe('getHealthTrend()', () => {
    it('should return 0 for insufficient history', () => {
      expect(monitor.getHealthTrend()).toBe(0);
    });

    it('should calculate trend correctly', () => {
      // Manually add history entries
      for (let i = 0; i < 5; i++) {
        monitor.healthHistory.push({ healthy: false, timestamp: Date.now() });
      }
      for (let i = 0; i < 5; i++) {
        monitor.healthHistory.push({ healthy: true, timestamp: Date.now() });
      }

      const trend = monitor.getHealthTrend(10);
      expect(trend).toBeGreaterThan(0); // Improving trend
    });
  });

  describe('interval adjustment', () => {
    it('should increase interval when healthy (backoff)', async () => {
      vi.useRealTimers();
      monitor = new AdaptiveMonitor({
        baseInterval: 100,
        healthyBackoffFactor: 2
      });

      const initialInterval = monitor.currentInterval;
      const checkFn = vi.fn().mockResolvedValue(true);
      monitor.start(checkFn);

      await vi.waitFor(() => expect(checkFn).toHaveBeenCalled());

      // Interval should increase
      expect(monitor.currentInterval).toBeGreaterThanOrEqual(initialInterval);
      monitor.stop();
    });

    it('should decrease interval when unhealthy (rush)', async () => {
      vi.useRealTimers();
      monitor = new AdaptiveMonitor({
        baseInterval: 1000,
        minInterval: 100,
        unhealthyRushFactor: 0.5
      });

      const initialInterval = monitor.currentInterval;
      const checkFn = vi.fn().mockResolvedValue(false);
      monitor.start(checkFn);

      await vi.waitFor(() => expect(checkFn).toHaveBeenCalled());

      // Interval should decrease
      expect(monitor.currentInterval).toBeLessThanOrEqual(initialInterval);
      monitor.stop();
    });

    it('should respect minInterval bound', async () => {
      vi.useRealTimers();
      monitor = new AdaptiveMonitor({
        baseInterval: 100,
        minInterval: 50,
        unhealthyRushFactor: 0.1
      });

      const checkFn = vi.fn().mockResolvedValue(false);
      monitor.start(checkFn);

      await vi.waitFor(() => expect(checkFn.mock.calls.length).toBeGreaterThanOrEqual(3));

      expect(monitor.currentInterval).toBeGreaterThanOrEqual(50);
      monitor.stop();
    });

    it('should respect maxInterval bound', async () => {
      vi.useRealTimers();
      monitor = new AdaptiveMonitor({
        baseInterval: 100,
        maxInterval: 500,
        healthyBackoffFactor: 10
      });

      const checkFn = vi.fn().mockResolvedValue(true);
      monitor.start(checkFn);

      await vi.waitFor(() => expect(checkFn.mock.calls.length).toBeGreaterThanOrEqual(2), { timeout: 3000 });

      expect(monitor.currentInterval).toBeLessThanOrEqual(500);
      monitor.stop();
    });
  });

  describe('status transitions', () => {
    it('should transition to STABLE after threshold', async () => {
      vi.useRealTimers();
      monitor = new AdaptiveMonitor({
        baseInterval: 50,
        stableThreshold: 3
      });

      const stableHandler = vi.fn();
      monitor.on(MonitorEvents.STABLE, stableHandler);

      monitor.start(() => true);

      await vi.waitFor(() => expect(stableHandler).toHaveBeenCalled(), { timeout: 2000 });
      expect(monitor.status).toBe(HealthStatus.STABLE);
      monitor.stop();
    });

    it('should transition to CRITICAL after threshold', async () => {
      vi.useRealTimers();
      monitor = new AdaptiveMonitor({
        baseInterval: 50,
        minInterval: 10,
        criticalThreshold: 3
      });

      const criticalHandler = vi.fn();
      monitor.on(MonitorEvents.CRITICAL, criticalHandler);

      monitor.start(() => false);

      await vi.waitFor(() => expect(criticalHandler).toHaveBeenCalled(), { timeout: 5000 });
      expect(monitor.status).toBe(HealthStatus.CRITICAL);
      monitor.stop();
    });

    it('should emit status change events', async () => {
      vi.useRealTimers();
      monitor = new AdaptiveMonitor({ baseInterval: 50 });

      const statusHandler = vi.fn();
      monitor.on(MonitorEvents.STATUS_CHANGE, statusHandler);

      monitor.start(() => true);

      await vi.waitFor(() => expect(statusHandler).toHaveBeenCalled(), { timeout: 1000 });
      monitor.stop();
    });
  });

  describe('error handling', () => {
    it('should handle check function errors', async () => {
      vi.useRealTimers();
      const errorHandler = vi.fn();
      monitor.on(MonitorEvents.ERROR, errorHandler);

      monitor.start(() => {
        throw new Error('Check failed');
      });

      await vi.waitFor(() => expect(errorHandler).toHaveBeenCalled(), { timeout: 1000 });

      expect(errorHandler).toHaveBeenCalledWith(
        expect.objectContaining({
          error: expect.any(Error)
        })
      );
      monitor.stop();
    });

    it('should treat errors as unhealthy', async () => {
      vi.useRealTimers();
      // Must add error listener to prevent unhandled error
      monitor.on(MonitorEvents.ERROR, () => {});

      monitor.start(() => {
        throw new Error('Oops');
      });

      await vi.waitFor(() => expect(monitor.totalUnhealthy).toBeGreaterThan(0), { timeout: 1000 });
      monitor.stop();
    });
  });

  describe('check result normalization', () => {
    it('should handle boolean result', async () => {
      vi.useRealTimers();
      const healthHandler = vi.fn();
      monitor.on(MonitorEvents.HEALTH, healthHandler);

      monitor.start(() => true);

      await vi.waitFor(() => expect(healthHandler).toHaveBeenCalled());

      expect(healthHandler).toHaveBeenCalledWith(
        expect.objectContaining({
          healthy: true
        })
      );
      monitor.stop();
    });

    it('should handle object result with healthy property', async () => {
      vi.useRealTimers();
      const healthHandler = vi.fn();
      monitor.on(MonitorEvents.HEALTH, healthHandler);

      monitor.start(() => ({
        healthy: true,
        details: { metric: 42 }
      }));

      await vi.waitFor(() => expect(healthHandler).toHaveBeenCalled());

      expect(healthHandler).toHaveBeenCalledWith(
        expect.objectContaining({
          healthy: true,
          details: expect.objectContaining({ metric: 42 })
        })
      );
      monitor.stop();
    });
  });
});

describe('createAdaptiveMonitor', () => {
  it('should create a new monitor instance', () => {
    const monitor = createAdaptiveMonitor();
    expect(monitor).toBeInstanceOf(AdaptiveMonitor);
  });

  it('should pass config to constructor', () => {
    const monitor = createAdaptiveMonitor({ baseInterval: 5000 });
    expect(monitor.config.baseInterval).toBe(5000);
  });
});

describe('createMemoryMonitor', () => {
  it('should create memory monitor configuration', () => {
    const { monitor, start } = createMemoryMonitor();
    expect(monitor).toBeInstanceOf(AdaptiveMonitor);
    expect(typeof start).toBe('function');
  });

  it('should accept threshold parameter', () => {
    const { monitor } = createMemoryMonitor(0.9);
    expect(monitor).toBeDefined();
  });

  it('should accept additional config', () => {
    const { monitor } = createMemoryMonitor(0.8, { verbose: true });
    expect(monitor.config.verbose).toBe(true);
  });
});

describe('createEventLoopMonitor', () => {
  it('should create event loop monitor configuration', () => {
    const { monitor, start } = createEventLoopMonitor();
    expect(monitor).toBeInstanceOf(AdaptiveMonitor);
    expect(typeof start).toBe('function');
  });

  it('should accept lag threshold parameter', () => {
    const { monitor } = createEventLoopMonitor(50);
    expect(monitor).toBeDefined();
  });
});

describe('MonitorOrchestrator', () => {
  let orchestrator;

  beforeEach(() => {
    orchestrator = new MonitorOrchestrator();
  });

  afterEach(() => {
    orchestrator.stopAll();
  });

  describe('constructor', () => {
    it('should initialize empty monitors map', () => {
      expect(orchestrator.monitors.size).toBe(0);
    });

    it('should set initial aggregated status', () => {
      expect(orchestrator.aggregatedStatus).toBe(HealthStatus.UNKNOWN);
    });
  });

  describe('add()', () => {
    it('should add monitor to collection', () => {
      const monitor = new AdaptiveMonitor();
      orchestrator.add('test', monitor);
      expect(orchestrator.monitors.has('test')).toBe(true);
    });

    it('should return this for chaining', () => {
      const monitor = new AdaptiveMonitor();
      const result = orchestrator.add('test', monitor);
      expect(result).toBe(orchestrator);
    });

    it('should forward events from monitors', () => {
      const monitor = new AdaptiveMonitor();
      const handler = vi.fn();

      orchestrator.add('test', monitor);
      orchestrator.on('test:health', handler);

      // Manually emit health event
      monitor.emit(MonitorEvents.HEALTH, { healthy: true });

      expect(handler).toHaveBeenCalled();
    });
  });

  describe('remove()', () => {
    it('should remove monitor from collection', () => {
      const monitor = new AdaptiveMonitor();
      orchestrator.add('test', monitor);
      expect(orchestrator.remove('test')).toBe(true);
      expect(orchestrator.monitors.has('test')).toBe(false);
    });

    it('should stop removed monitor', () => {
      const monitor = new AdaptiveMonitor();
      monitor.start(() => true);
      orchestrator.add('test', monitor);

      orchestrator.remove('test');
      expect(monitor.running).toBe(false);
    });

    it('should return false for non-existent monitor', () => {
      expect(orchestrator.remove('nonexistent')).toBe(false);
    });
  });

  describe('stopAll()', () => {
    it('should stop all monitors', () => {
      const m1 = new AdaptiveMonitor();
      const m2 = new AdaptiveMonitor();

      m1.start(() => true);
      m2.start(() => true);

      orchestrator.add('m1', m1);
      orchestrator.add('m2', m2);

      orchestrator.stopAll();

      expect(m1.running).toBe(false);
      expect(m2.running).toBe(false);
    });
  });

  describe('getAggregatedStats()', () => {
    it('should return aggregated statistics', () => {
      const monitor = new AdaptiveMonitor();
      orchestrator.add('test', monitor);

      const stats = orchestrator.getAggregatedStats();

      expect(stats).toHaveProperty('monitors');
      expect(stats).toHaveProperty('aggregatedStatus');
      expect(stats).toHaveProperty('monitorCount');
      expect(stats.monitorCount).toBe(1);
      expect(stats.monitors.test).toBeDefined();
    });
  });

  describe('aggregated status', () => {
    it('should be CRITICAL if any monitor is critical', () => {
      const m1 = new AdaptiveMonitor();
      const m2 = new AdaptiveMonitor();

      m1.status = HealthStatus.HEALTHY;
      m2.status = HealthStatus.CRITICAL;

      orchestrator.add('m1', m1);
      orchestrator.add('m2', m2);

      orchestrator._updateAggregatedStatus();

      expect(orchestrator.aggregatedStatus).toBe(HealthStatus.CRITICAL);
    });

    it('should be STABLE if all monitors are stable', () => {
      const m1 = new AdaptiveMonitor();
      const m2 = new AdaptiveMonitor();

      m1.status = HealthStatus.STABLE;
      m2.status = HealthStatus.STABLE;

      orchestrator.add('m1', m1);
      orchestrator.add('m2', m2);

      orchestrator._updateAggregatedStatus();

      expect(orchestrator.aggregatedStatus).toBe(HealthStatus.STABLE);
    });
  });
});

describe('createMonitorOrchestrator', () => {
  it('should create a new orchestrator instance', () => {
    const orchestrator = createMonitorOrchestrator();
    expect(orchestrator).toBeInstanceOf(MonitorOrchestrator);
  });
});
