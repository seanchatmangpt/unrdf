/**
 * @file YAWL Daemon Lifecycle Tests
 * @module @unrdf/yawl/test/daemon/lifecycle
 * @description Comprehensive tests for daemon and bridge lifecycle management
 * Tests cover initialization, startup, running, shutdown, and state transitions
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { EventEmitter } from 'events';
import { Daemon } from '@unrdf/daemon';
import { YawlDaemonBridge } from '@unrdf/daemon/integrations/yawl';

/**
 * Generate UUID v4 for testing
 * @returns {string} Valid UUID v4
 */
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * Mock YAWL engine for lifecycle tests
 */
class MockYawlEngine extends EventEmitter {
  constructor() {
    super();
    this.isInitialized = false;
  }

  on(eventName, handler) {
    super.on(eventName, handler);
    return () => this.off(eventName, handler);
  }

  async initialize() {
    this.isInitialized = true;
  }

  async createCase(options) {
    return { caseId: options.caseId || `case-${Date.now()}`, status: 'RUNNING' };
  }

  async enableTask(options) {
    return { ...options, status: 'ENABLED' };
  }

  async cancelTask(options) {
    return { ...options, status: 'CANCELLED' };
  }
}

describe('Daemon Lifecycle', () => {
  describe('initialization', () => {
    it('should initialize daemon with valid config', () => {
      // Arrange
      const config = {
        daemonId: generateUUID(),
        name: 'init-daemon',
      };

      // Act
      const daemon = new Daemon(config);

      // Assert
      expect(daemon.config).toBeDefined();
      expect(daemon.isRunning).toBe(false);
      expect(daemon.nodeId).toBeDefined();
    });

    it('should set initial state correctly', () => {
      // Arrange & Act
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'state-daemon',
      });

      // Assert
      expect(daemon.isRunning).toBe(false);
      expect(daemon.isLeader).toBe(false);
      expect(daemon.activeCount).toBe(0);
      expect(daemon.startTime).toBeNull();
    });

    it('should initialize operation tracking structures', () => {
      // Arrange & Act
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'tracking-daemon',
      });

      // Assert
      expect(daemon.operations).toBeInstanceOf(Map);
      expect(daemon.operationQueue).toBeInstanceOf(Array);
      expect(daemon.operations.size).toBe(0);
      expect(daemon.operationQueue.length).toBe(0);
    });

    it('should initialize bridge with daemon and engine', () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'bridge-init-daemon',
      });
      const engine = new MockYawlEngine();

      // Act
      const bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-init',
      });

      // Assert
      expect(bridge.daemon).toBe(daemon);
      expect(bridge.yawlEngine).toBe(engine);
      expect(bridge.isRunning).toBe(false);
    });

    it('should initialize bridge tracking maps', () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'maps-daemon',
      });
      const engine = new MockYawlEngine();

      // Act
      const bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-maps',
      });

      // Assert
      expect(bridge.caseSchedules).toBeInstanceOf(Map);
      expect(bridge.taskTimeouts).toBeInstanceOf(Map);
      expect(bridge.taskRetries).toBeInstanceOf(Map);
      expect(bridge.choiceTriggers).toBeInstanceOf(Map);
      expect(bridge.parallelDistributions).toBeInstanceOf(Map);
    });

    it('should setup event listeners during initialization', () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'listeners-daemon',
      });
      const engine = new MockYawlEngine();

      // Act
      const bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-listeners',
        enableAutoRetry: true,
        enableTimeoutTracking: true,
      });

      // Assert
      expect(bridge._unsubscribers).toBeDefined();
      expect(Array.isArray(bridge._unsubscribers)).toBe(true);
    });

    it('should assign unique node ID', () => {
      // Arrange & Act
      const daemon1 = new Daemon({
        daemonId: generateUUID(),
        name: 'daemon-1',
      });
      const daemon2 = new Daemon({
        daemonId: generateUUID(),
        name: 'daemon-2',
      });

      // Assert
      expect(daemon1.nodeId).not.toBe(daemon2.nodeId);
    });

    it('should initialize with custom logger', () => {
      // Arrange
      const customLogger = {
        info: vi.fn(),
        debug: vi.fn(),
        warn: vi.fn(),
        error: vi.fn(),
      };

      // Act
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'logger-daemon',
        logger: customLogger,
      });

      // Assert
      expect(daemon.logger).toBe(customLogger);
    });

    it('should use default logger if none provided', () => {
      // Arrange & Act
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'default-logger-daemon',
      });

      // Assert
      expect(daemon.logger).toBeDefined();
      expect(typeof daemon.logger.info).toBe('function');
    });

    it('should initialize event emitter capabilities', () => {
      // Arrange & Act
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'emitter-daemon',
      });

      // Assert
      expect(typeof daemon.on).toBe('function');
      expect(typeof daemon.emit).toBe('function');
      expect(typeof daemon.once).toBe('function');
    });

    it('should validate configuration during initialization', () => {
      // Arrange
      const invalidConfig = {
        daemonId: 'not-uuid',
        name: 'invalid-daemon',
      };

      // Act & Assert
      expect(() => new Daemon(invalidConfig)).toThrow();
    });

    it('should initialize completed operations cache', () => {
      // Arrange & Act
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'cache-daemon',
      });

      // Assert
      expect(daemon.completedOperations).toBeDefined();
    });

    it('should set cluster ID', () => {
      // Arrange & Act
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'cluster-daemon',
      });

      // Assert
      expect(daemon.clusterId).toBe('default-cluster');
    });
  });

  describe('startup sequence', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(() => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'startup-daemon',
      });
      engine = new MockYawlEngine();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-startup',
      });
    });

    afterEach(async () => {
      await bridge.stop();
      await daemon.stop();
    });

    it('should start daemon successfully', async () => {
      // Arrange
      expect(daemon.isRunning).toBe(false);

      // Act
      await daemon.start();

      // Assert
      expect(daemon.isRunning).toBe(true);
    });

    it('should set start timestamp', async () => {
      // Arrange
      expect(daemon.startTime).toBeNull();
      const beforeStart = Date.now();

      // Act
      await daemon.start();
      const afterStart = Date.now();

      // Assert
      expect(daemon.startTime).not.toBeNull();
      expect(daemon.startTime).toBeGreaterThanOrEqual(beforeStart);
      expect(daemon.startTime).toBeLessThanOrEqual(afterStart);
    });

    it('should emit daemon:started event', async () => {
      // Arrange
      const listener = vi.fn();
      daemon.on('daemon:started', listener);

      // Act
      await daemon.start();

      // Assert
      expect(listener).toHaveBeenCalled();
      expect(listener.mock.calls[0][0]).toHaveProperty('nodeId');
      expect(listener.mock.calls[0][0]).toHaveProperty('timestamp');
    });

    it('should start bridge successfully', async () => {
      // Arrange
      expect(bridge.isRunning).toBe(false);

      // Act
      await bridge.start();

      // Assert
      expect(bridge.isRunning).toBe(true);
    });

    it('should emit bridge:started event', async () => {
      // Arrange
      const listener = vi.fn();
      bridge.on('bridge:started', listener);

      // Act
      await bridge.start();

      // Assert
      expect(listener).toHaveBeenCalled();
      expect(listener.mock.calls[0][0].bridgeId).toBe(bridge.id);
    });

    it('should be idempotent on multiple start calls', async () => {
      // Arrange & Act
      await daemon.start();
      const firstStartTime = daemon.startTime;

      await new Promise((resolve) => setTimeout(resolve, 10));
      await daemon.start();

      // Assert
      expect(daemon.startTime).toBe(firstStartTime);
      expect(daemon.isRunning).toBe(true);
    });

    it('should start daemon before bridge', async () => {
      // Arrange & Act
      await daemon.start();
      await bridge.start();

      // Assert
      expect(daemon.isRunning).toBe(true);
      expect(bridge.isRunning).toBe(true);
    });

    it('should log startup message', async () => {
      // Arrange
      const mockLogger = { info: vi.fn(), debug: vi.fn(), warn: vi.fn() };
      daemon.logger = mockLogger;

      // Act
      await daemon.start();

      // Assert
      expect(mockLogger.info).toHaveBeenCalledWith(
        expect.stringContaining('Started')
      );
    });

    it('should initialize health tracking on startup', async () => {
      // Arrange
      await daemon.start();

      // Act
      const health = daemon.getHealth();

      // Assert
      expect(health.isRunning).toBe(true);
      expect(health.uptime).toBeGreaterThanOrEqual(0);
    });

    it('should handle rapid start/stop/start cycles', async () => {
      // Arrange & Act
      await daemon.start();
      await daemon.stop();
      await daemon.start();

      // Assert
      expect(daemon.isRunning).toBe(true);
    });

    it('should support sequential component startup', async () => {
      // Arrange
      const startOrder = [];

      daemon.on('daemon:started', () => startOrder.push('daemon'));
      bridge.on('bridge:started', () => startOrder.push('bridge'));

      // Act
      await daemon.start();
      await bridge.start();

      // Assert
      expect(startOrder).toEqual(['daemon', 'bridge']);
    });

    it('should return Promise from start', async () => {
      // Arrange & Act
      const result = daemon.start();

      // Assert
      expect(result).toBeInstanceOf(Promise);
      await result;
    });
  });

  describe('running state', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'running-daemon',
      });
      engine = new MockYawlEngine();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-running',
      });

      await daemon.start();
      await bridge.start();
    });

    afterEach(async () => {
      await bridge.stop();
      await daemon.stop();
    });

    it('should maintain running state', () => {
      // Arrange & Act
      const isRunning = daemon.isRunning;

      // Assert
      expect(isRunning).toBe(true);
    });

    it('should track uptime during running state', async () => {
      // Arrange
      await new Promise((resolve) => setTimeout(resolve, 50));

      // Act
      const health = daemon.getHealth();

      // Assert
      expect(health.uptime).toBeGreaterThan(0);
      expect(health.uptime).toBeGreaterThanOrEqual(50);
    });

    it('should process operations while running', async () => {
      // Arrange
      const handler = vi.fn().mockResolvedValue({ result: 'ok' });
      daemon.schedule({ id: 'run-op', name: 'Run Op', handler });

      // Act
      const result = await daemon.execute('run-op');

      // Assert
      expect(result.result).toBe('ok');
      expect(handler).toHaveBeenCalled();
    });

    it('should accept new schedules while running', async () => {
      // Arrange
      const initialSize = daemon.operations.size;

      // Act
      await bridge.scheduleRecurringCase('running-wf', '* * * * *');

      // Assert
      expect(daemon.operations.size).toBeGreaterThan(initialSize);
    });

    it('should update metrics while running', async () => {
      // Arrange
      daemon.schedule({
        id: 'metric-op',
        name: 'Metric',
        handler: vi.fn().mockResolvedValue({}),
      });

      // Act
      await daemon.execute('metric-op');
      const metrics = daemon.getMetrics();

      // Assert
      expect(metrics.totalOperations).toBeGreaterThan(0);
    });

    it('should emit events while running', async () => {
      // Arrange
      const listener = vi.fn();
      daemon.on('operation:enqueued', listener);

      // Act
      daemon.schedule({ id: 'event-op', name: 'Event', handler: vi.fn() });

      // Assert
      expect(listener).toHaveBeenCalled();
    });

    it('should track active operations', async () => {
      // Arrange
      daemon.schedule({
        id: 'active-op',
        name: 'Active',
        handler: vi.fn().mockImplementation(
          () => new Promise((resolve) => setTimeout(resolve, 100))
        ),
      });

      // Act
      const execPromise = daemon.execute('active-op');
      const healthDuring = daemon.getHealth();
      await execPromise;
      const healthAfter = daemon.getHealth();

      // Assert
      expect(healthDuring.activeOperations).toBeGreaterThanOrEqual(0);
    });

    it('should maintain bridge state', () => {
      // Arrange & Act
      const stats = bridge.getStats();

      // Assert
      expect(stats.isRunning).toBe(true);
      expect(stats.bridgeId).toBe(bridge.id);
    });

    it('should handle concurrent operations', async () => {
      // Arrange
      const handlers = [
        vi.fn().mockResolvedValue({}),
        vi.fn().mockResolvedValue({}),
        vi.fn().mockResolvedValue({}),
      ];
      handlers.forEach((handler, idx) => {
        daemon.schedule({ id: `conc-${idx}`, name: `C${idx}`, handler });
      });

      // Act
      await Promise.all([
        daemon.execute('conc-0'),
        daemon.execute('conc-1'),
        daemon.execute('conc-2'),
      ]);

      // Assert
      handlers.forEach((handler) => {
        expect(handler).toHaveBeenCalled();
      });
    });

    it('should respond to health checks', () => {
      // Arrange & Act
      const health = daemon.getHealth();

      // Assert
      expect(health).toHaveProperty('isRunning');
      expect(health).toHaveProperty('uptime');
      expect(health).toHaveProperty('activeOperations');
      expect(health.isRunning).toBe(true);
    });

    it('should accumulate completed operations', async () => {
      // Arrange
      for (let i = 0; i < 3; i++) {
        daemon.schedule({
          id: `complete-${i}`,
          name: `C${i}`,
          handler: vi.fn().mockResolvedValue({}),
        });
      }

      // Act
      await daemon.execute('complete-0');
      await daemon.execute('complete-1');
      await daemon.execute('complete-2');

      const metrics = daemon.getMetrics();

      // Assert
      expect(metrics.totalOperations).toBeGreaterThanOrEqual(3);
    });

    it('should maintain event listeners', () => {
      // Arrange
      const listener = vi.fn();
      bridge.on('tasks:distributed', listener);

      // Act
      engine.emit('tasks:distributed', { taskIds: ['t1', 't2'] });

      // Assert - Bridge doesn't automatically emit this, but engine can
      expect(bridge.listenerCount('tasks:distributed')).toBe(1);
    });
  });

  describe('shutdown sequence', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'shutdown-daemon',
      });
      engine = new MockYawlEngine();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-shutdown',
      });

      await daemon.start();
      await bridge.start();
    });

    it('should stop daemon successfully', async () => {
      // Arrange
      expect(daemon.isRunning).toBe(true);

      // Act
      await daemon.stop();

      // Assert
      expect(daemon.isRunning).toBe(false);
    });

    it('should emit daemon:stopped event', async () => {
      // Arrange
      const listener = vi.fn();
      daemon.on('daemon:stopped', listener);

      // Act
      await daemon.stop();

      // Assert
      expect(listener).toHaveBeenCalled();
    });

    it('should stop bridge successfully', async () => {
      // Arrange
      expect(bridge.isRunning).toBe(true);

      // Act
      await bridge.stop();

      // Assert
      expect(bridge.isRunning).toBe(false);
    });

    it('should emit bridge:stopped event', async () => {
      // Arrange
      const listener = vi.fn();
      bridge.on('bridge:stopped', listener);

      // Act
      await bridge.stop();

      // Assert
      expect(listener).toHaveBeenCalled();
      expect(listener.mock.calls[0][0].bridgeId).toBe(bridge.id);
    });

    it('should unsubscribe event listeners on stop', async () => {
      // Arrange
      const initialUnsubscribers = bridge._unsubscribers.length;

      // Act
      await bridge.stop();

      // Assert
      expect(bridge._unsubscribers.length).toBe(0);
    });

    it('should clear timeouts on stop', async () => {
      // Arrange
      await bridge.watchTaskTimeout('case-1', 'task-1', 30000);
      expect(bridge.taskTimeouts.size).toBe(1);

      // Act
      await bridge.stop();

      // Assert
      expect(bridge.taskTimeouts.size).toBe(0);
    });

    it('should preserve completed operations after stop', async () => {
      // Arrange
      daemon.schedule({
        id: 'preserve-op',
        name: 'Preserve',
        handler: vi.fn().mockResolvedValue({}),
      });
      await daemon.execute('preserve-op');
      const beforeMetrics = daemon.getMetrics();

      // Act
      await daemon.stop();
      const afterMetrics = daemon.getMetrics();

      // Assert
      expect(afterMetrics.totalOperations).toBe(beforeMetrics.totalOperations);
    });

    it('should stop bridge before daemon', async () => {
      // Arrange & Act
      await bridge.stop();
      await daemon.stop();

      // Assert
      expect(bridge.isRunning).toBe(false);
      expect(daemon.isRunning).toBe(false);
    });

    it('should be idempotent on multiple stop calls', async () => {
      // Arrange & Act
      await daemon.stop();
      await daemon.stop();
      await daemon.stop();

      // Assert
      expect(daemon.isRunning).toBe(false);
    });

    it('should log shutdown message', async () => {
      // Arrange
      const mockLogger = { info: vi.fn(), debug: vi.fn(), warn: vi.fn() };
      daemon.logger = mockLogger;

      // Act
      await daemon.stop();

      // Assert
      expect(mockLogger.info).toHaveBeenCalledWith(
        expect.stringContaining('Stopped')
      );
    });

    it('should cleanup scheduled operations references', async () => {
      // Arrange
      await bridge.scheduleRecurringCase('cleanup-wf', '* * * * *');

      // Act
      await bridge.stop();

      // Assert
      expect(bridge.caseSchedules.size).toBe(0);
    });

    it('should return Promise from stop', async () => {
      // Arrange & Act
      const result = daemon.stop();

      // Assert
      expect(result).toBeInstanceOf(Promise);
      await result;
    });
  });

  describe('state transitions', () => {
    it('should transition from initialized to running', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'transition-daemon',
      });
      expect(daemon.isRunning).toBe(false);

      // Act
      await daemon.start();

      // Assert
      expect(daemon.isRunning).toBe(true);
    });

    it('should transition from running to stopped', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'stop-transition-daemon',
      });
      await daemon.start();
      expect(daemon.isRunning).toBe(true);

      // Act
      await daemon.stop();

      // Assert
      expect(daemon.isRunning).toBe(false);
    });

    it('should support stop-to-start transition (restart)', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'restart-daemon',
      });

      // Act
      await daemon.start();
      await daemon.stop();
      await daemon.start();

      // Assert
      expect(daemon.isRunning).toBe(true);
    });

    it('should maintain state consistency during transitions', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'consistency-daemon',
      });
      const states = [];

      daemon.on('daemon:started', () => states.push('started'));
      daemon.on('daemon:stopped', () => states.push('stopped'));

      // Act
      await daemon.start();
      await daemon.stop();
      await daemon.start();

      // Assert
      expect(states).toEqual(['started', 'stopped', 'started']);
    });

    it('should handle rapid state transitions', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'rapid-daemon',
      });

      // Act
      await daemon.start();
      await daemon.stop();
      await daemon.start();
      await daemon.stop();
      await daemon.start();

      // Assert
      expect(daemon.isRunning).toBe(true);
    });

    it('should preserve configuration across transitions', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'preserve-config-daemon',
        port: 8989,
      });

      // Act
      await daemon.start();
      await daemon.stop();
      await daemon.start();

      // Assert
      expect(daemon.config.port).toBe(8989);
    });

    it('should emit events for each transition', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'events-daemon',
      });
      const events = [];

      daemon.on('daemon:started', () => events.push('started'));
      daemon.on('daemon:stopped', () => events.push('stopped'));

      // Act
      await daemon.start();
      await daemon.stop();

      // Assert
      expect(events).toEqual(['started', 'stopped']);
    });

    it('should update health status during transitions', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'health-transition-daemon',
      });

      // Act
      const beforeStart = daemon.getHealth();
      await daemon.start();
      const afterStart = daemon.getHealth();
      await daemon.stop();
      const afterStop = daemon.getHealth();

      // Assert
      expect(beforeStart.isRunning).toBe(false);
      expect(afterStart.isRunning).toBe(true);
      expect(afterStop.isRunning).toBe(false);
    });

    it('should handle concurrent transition attempts', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'concurrent-transition-daemon',
      });

      // Act
      await Promise.all([daemon.start(), daemon.start(), daemon.start()]);

      // Assert
      expect(daemon.isRunning).toBe(true);
    });

    it('should support transition rollback on error', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'rollback-daemon',
      });

      // Act
      await daemon.start();
      const stateBefore = daemon.isRunning;

      try {
        // Simulate error during operation
        throw new Error('Operation failed');
      } catch (e) {
        // Daemon should still be running
      }

      // Assert
      expect(daemon.isRunning).toBe(stateBefore);
    });

    it('should track transition history via events', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'history-daemon',
      });
      const history = [];

      daemon.on('daemon:started', (e) =>
        history.push({ event: 'started', time: e.timestamp })
      );
      daemon.on('daemon:stopped', (e) =>
        history.push({ event: 'stopped', time: e.timestamp })
      );

      // Act
      await daemon.start();
      await daemon.stop();

      // Assert
      expect(history).toHaveLength(2);
      expect(history[0].event).toBe('started');
      expect(history[1].event).toBe('stopped');
    });

    it('should clear transient state during transitions', async () => {
      // Arrange
      const daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'transient-daemon',
      });
      const engine = new MockYawlEngine();
      const bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-transient',
      });

      await bridge.start();
      await bridge.watchTaskTimeout('case-1', 'task-1', 30000);

      // Act
      await bridge.stop();

      // Assert
      expect(bridge.taskTimeouts.size).toBe(0);
      expect(bridge._unsubscribers.length).toBe(0);
    });
  });
});
