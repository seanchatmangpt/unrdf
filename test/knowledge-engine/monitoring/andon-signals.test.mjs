/**
 * @file Andon Signal System Tests
 * @module test/knowledge-engine/monitoring/andon-signals
 *
 * @description
 * Comprehensive test suite for the Andon signal system.
 * Tests signal state computation, validation integration,
 * CI/CD parsing, deployment gates, and notifications.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import {
  AndonState,
  SignalCategory,
  AndonSignalManager,
  computeSignalState,
  getStatePriority,
  getWorstState,
  createAndonSignalManager,
  defaultAndonSignalManager
} from '../../../src/knowledge-engine/monitoring/andon-signals.mjs';

describe('Andon Signals', () => {
  describe('computeSignalState', () => {
    it('should return GREEN for scores >= 80', () => {
      expect(computeSignalState(100)).toBe(AndonState.GREEN);
      expect(computeSignalState(90)).toBe(AndonState.GREEN);
      expect(computeSignalState(80)).toBe(AndonState.GREEN);
    });

    it('should return YELLOW for scores 60-79', () => {
      expect(computeSignalState(79)).toBe(AndonState.YELLOW);
      expect(computeSignalState(70)).toBe(AndonState.YELLOW);
      expect(computeSignalState(60)).toBe(AndonState.YELLOW);
    });

    it('should return RED for scores < 60', () => {
      expect(computeSignalState(59)).toBe(AndonState.RED);
      expect(computeSignalState(30)).toBe(AndonState.RED);
      expect(computeSignalState(0)).toBe(AndonState.RED);
    });

    it('should handle custom thresholds', () => {
      const customThresholds = { green: 90, yellow: 70 };
      expect(computeSignalState(95, customThresholds)).toBe(AndonState.GREEN);
      expect(computeSignalState(85, customThresholds)).toBe(AndonState.YELLOW);
      expect(computeSignalState(65, customThresholds)).toBe(AndonState.RED);
    });

    it('should clamp scores to 0-100 range', () => {
      expect(computeSignalState(150)).toBe(AndonState.GREEN);
      expect(computeSignalState(-10)).toBe(AndonState.RED);
    });

    it('should handle boundary values precisely', () => {
      expect(computeSignalState(80.0)).toBe(AndonState.GREEN);
      expect(computeSignalState(79.9)).toBe(AndonState.YELLOW);
      expect(computeSignalState(60.0)).toBe(AndonState.YELLOW);
      expect(computeSignalState(59.9)).toBe(AndonState.RED);
    });
  });

  describe('getStatePriority', () => {
    it('should assign correct priorities', () => {
      expect(getStatePriority(AndonState.RED)).toBe(3);
      expect(getStatePriority(AndonState.YELLOW)).toBe(2);
      expect(getStatePriority(AndonState.GREEN)).toBe(1);
    });

    it('should return 0 for unknown states', () => {
      expect(getStatePriority('unknown')).toBe(0);
      expect(getStatePriority(null)).toBe(0);
    });
  });

  describe('getWorstState', () => {
    it('should return RED when comparing with RED', () => {
      expect(getWorstState(AndonState.RED, AndonState.GREEN)).toBe(AndonState.RED);
      expect(getWorstState(AndonState.GREEN, AndonState.RED)).toBe(AndonState.RED);
      expect(getWorstState(AndonState.RED, AndonState.YELLOW)).toBe(AndonState.RED);
    });

    it('should return YELLOW when comparing YELLOW and GREEN', () => {
      expect(getWorstState(AndonState.YELLOW, AndonState.GREEN)).toBe(AndonState.YELLOW);
      expect(getWorstState(AndonState.GREEN, AndonState.YELLOW)).toBe(AndonState.YELLOW);
    });

    it('should return same state when comparing identical states', () => {
      expect(getWorstState(AndonState.GREEN, AndonState.GREEN)).toBe(AndonState.GREEN);
      expect(getWorstState(AndonState.RED, AndonState.RED)).toBe(AndonState.RED);
    });
  });

  describe('AndonSignalManager', () => {
    /** @type {AndonSignalManager} */
    let manager;

    beforeEach(() => {
      manager = new AndonSignalManager();
    });

    afterEach(() => {
      manager.clear();
    });

    describe('constructor', () => {
      it('should create with default config', () => {
        expect(manager.config.thresholds.green).toBe(80);
        expect(manager.config.thresholds.yellow).toBe(60);
        expect(manager.config.strictDeployment).toBe(true);
      });

      it('should accept custom config', () => {
        const custom = new AndonSignalManager({
          thresholds: { green: 90, yellow: 70 },
          strictDeployment: false
        });
        expect(custom.config.thresholds.green).toBe(90);
        expect(custom.config.strictDeployment).toBe(false);
      });
    });

    describe('registerSignal', () => {
      it('should register a valid signal', () => {
        manager.registerSignal({
          name: 'test-signal',
          category: SignalCategory.VALIDATION
        });
        expect(manager.signalConfigs.has('test-signal')).toBe(true);
      });

      it('should support chaining', () => {
        const result = manager
          .registerSignal({ name: 'sig1', category: SignalCategory.VALIDATION })
          .registerSignal({ name: 'sig2', category: SignalCategory.CI_CD });

        expect(result).toBe(manager);
        expect(manager.signalConfigs.size).toBe(2);
      });

      it('should throw on invalid config', () => {
        expect(() => manager.registerSignal({})).toThrow();
        expect(() => manager.registerSignal({ name: '' })).toThrow();
      });
    });

    describe('updateSignal', () => {
      beforeEach(() => {
        manager.registerSignal({
          name: 'test-signal',
          category: SignalCategory.VALIDATION
        });
      });

      it('should update signal state', () => {
        const result = manager.updateSignal('test-signal', 85);
        expect(result.state).toBe(AndonState.GREEN);
        expect(result.score).toBe(85);
      });

      it('should throw for unregistered signal', () => {
        expect(() => manager.updateSignal('unknown', 50)).toThrow(/not registered/);
      });

      it('should store message and metadata', () => {
        const result = manager.updateSignal('test-signal', 75, {
          message: 'Test message',
          metadata: { key: 'value' }
        });
        expect(result.message).toBe('Test message');
        expect(result.metadata.key).toBe('value');
      });

      it('should track signal history', () => {
        manager.updateSignal('test-signal', 50);
        manager.updateSignal('test-signal', 70);
        manager.updateSignal('test-signal', 90);

        const history = manager.getSignalHistory('test-signal');
        expect(history.length).toBe(3);
        expect(history[0].score).toBe(50);
        expect(history[2].score).toBe(90);
      });
    });

    describe('registerValidationSignals', () => {
      it('should register 7 validation signals', () => {
        manager.registerValidationSignals();

        const validationSignals = manager.getSignalsByCategory(SignalCategory.VALIDATION);
        expect(validationSignals.length).toBe(0); // Not updated yet, just registered
        expect(manager.signalConfigs.size).toBe(7);
      });

      it('should update signals from OTEL results', () => {
        const mockResults = {
          features: [
            { name: 'knowledge-engine-core', score: 85, passed: true },
            { name: 'knowledge-hooks-api', score: 45, passed: false, violations: ['v1', 'v2'] }
          ]
        };

        manager.registerValidationSignals(mockResults);

        const coreSignal = manager.signals.get('knowledge-engine-core');
        expect(coreSignal.score).toBe(85);
        expect(coreSignal.state).toBe(AndonState.GREEN);

        const hooksSignal = manager.signals.get('knowledge-hooks-api');
        expect(hooksSignal.score).toBe(45);
        expect(hooksSignal.state).toBe(AndonState.RED);
      });
    });

    describe('registerCISignals', () => {
      it('should register 9 CI/CD signals', () => {
        manager.registerCISignals();

        expect(manager.signalConfigs.has('ci-lint')).toBe(true);
        expect(manager.signalConfigs.has('ci-build')).toBe(true);
        expect(manager.signalConfigs.has('ci-unit-tests')).toBe(true);
      });

      it('should parse GitHub Actions job results', () => {
        manager.registerCISignals({
          jobs: [
            { name: 'lint', conclusion: 'success' },
            { name: 'unit-tests', conclusion: 'failure' },
            { name: 'build', status: 'in_progress' }
          ]
        });

        expect(manager.signals.get('ci-lint').score).toBe(100);
        expect(manager.signals.get('ci-unit-tests').score).toBe(0);
        expect(manager.signals.get('ci-build').score).toBe(70);
      });

      it('should handle various job conclusions', () => {
        manager.registerCISignals({
          jobs: [
            { name: 'lint', conclusion: 'success' },
            { name: 'typecheck', conclusion: 'failure' },
            { name: 'test', conclusion: 'cancelled' },
            { name: 'integration', conclusion: 'skipped' }
          ]
        });

        expect(manager.signals.get('ci-lint').score).toBe(100);
        expect(manager.signals.get('ci-typecheck').score).toBe(0);
        expect(manager.signals.get('ci-unit-tests').score).toBe(30);
        expect(manager.signals.get('ci-integration-tests').score).toBe(50);
      });

      it('should map various job names correctly', () => {
        manager.registerCISignals({
          jobs: [
            { name: 'Run Lint Checks', conclusion: 'success' },
            { name: 'Type-Check Source', conclusion: 'success' },
            { name: 'Security Scan', conclusion: 'success' },
            { name: 'E2E Tests', conclusion: 'success' },
            { name: 'Performance Benchmark', conclusion: 'success' }
          ]
        });

        expect(manager.signals.get('ci-lint')).toBeDefined();
        expect(manager.signals.get('ci-typecheck')).toBeDefined();
        expect(manager.signals.get('ci-security-scan')).toBeDefined();
        expect(manager.signals.get('ci-e2e-tests')).toBeDefined();
        expect(manager.signals.get('ci-performance')).toBeDefined();
      });
    });

    describe('registerPerformanceSignals', () => {
      it('should register 4 performance signals', () => {
        manager.registerPerformanceSignals();

        expect(manager.signalConfigs.has('perf-latency')).toBe(true);
        expect(manager.signalConfigs.has('perf-throughput')).toBe(true);
        expect(manager.signalConfigs.has('perf-error-rate')).toBe(true);
        expect(manager.signalConfigs.has('perf-memory')).toBe(true);
      });

      it('should update from performance metrics', () => {
        manager.registerPerformanceSignals({
          transactionLatency: { p50: 50, p95: 200, p99: 500 },
          errorRate: 0.01,
          hookExecutionRate: 15,
          memoryUsage: { heapUsed: 50 * 1024 * 1024, heapTotal: 100 * 1024 * 1024 }
        });

        const latency = manager.signals.get('perf-latency');
        expect(latency.score).toBe(90); // 100 - (200/20) = 90

        const errorRate = manager.signals.get('perf-error-rate');
        expect(errorRate.score).toBe(90); // 100 - (1 * 10) = 90

        const throughput = manager.signals.get('perf-throughput');
        expect(throughput.score).toBe(100); // 15/10 * 100 capped at 100

        const memory = manager.signals.get('perf-memory');
        expect(memory.score).toBe(50); // 100 - 50% = 50
      });
    });

    describe('isDeploymentReady', () => {
      beforeEach(() => {
        manager.registerSignal({ name: 'sig1', category: SignalCategory.VALIDATION });
        manager.registerSignal({ name: 'sig2', category: SignalCategory.CI_CD });
        manager.registerSignal({ name: 'sig3', category: SignalCategory.PERFORMANCE });
      });

      it('should return not ready when no signals registered', () => {
        const emptyManager = new AndonSignalManager();
        const result = emptyManager.isDeploymentReady();
        expect(result.ready).toBe(false);
        expect(result.reason).toContain('No signals');
      });

      it('should return ready when all GREEN in strict mode', () => {
        manager.updateSignal('sig1', 90);
        manager.updateSignal('sig2', 85);
        manager.updateSignal('sig3', 80);

        const result = manager.isDeploymentReady();
        expect(result.ready).toBe(true);
        expect(result.summary.green).toBe(3);
      });

      it('should return not ready with YELLOW in strict mode', () => {
        manager.updateSignal('sig1', 90);
        manager.updateSignal('sig2', 70); // YELLOW
        manager.updateSignal('sig3', 80);

        const result = manager.isDeploymentReady();
        expect(result.ready).toBe(false);
        expect(result.warnings.length).toBe(1);
      });

      it('should return not ready with RED signals', () => {
        manager.updateSignal('sig1', 90);
        manager.updateSignal('sig2', 30); // RED
        manager.updateSignal('sig3', 80);

        const result = manager.isDeploymentReady();
        expect(result.ready).toBe(false);
        expect(result.failures.length).toBe(1);
        expect(result.failures[0].name).toBe('sig2');
      });

      it('should allow YELLOW in lenient mode', () => {
        const lenientManager = new AndonSignalManager({ strictDeployment: false });
        lenientManager.registerSignal({ name: 'sig1', category: SignalCategory.VALIDATION });
        lenientManager.updateSignal('sig1', 70); // YELLOW

        const result = lenientManager.isDeploymentReady();
        expect(result.ready).toBe(true);
        expect(result.warnings.length).toBe(1);
      });

      it('should check required signals', () => {
        const strictManager = new AndonSignalManager({
          requiredSignals: ['required-signal']
        });
        strictManager.registerSignal({ name: 'other', category: SignalCategory.VALIDATION });
        strictManager.updateSignal('other', 100);

        const result = strictManager.isDeploymentReady();
        expect(result.ready).toBe(false);
        expect(result.missingRequired).toContain('required-signal');
      });
    });

    describe('getAllSignals', () => {
      it('should return empty array when no signals', () => {
        expect(manager.getAllSignals()).toEqual([]);
      });

      it('should return all updated signals', () => {
        manager.registerSignal({ name: 'sig1', category: SignalCategory.VALIDATION });
        manager.registerSignal({ name: 'sig2', category: SignalCategory.CI_CD });
        manager.updateSignal('sig1', 80);
        manager.updateSignal('sig2', 60);

        const signals = manager.getAllSignals();
        expect(signals.length).toBe(2);
      });
    });

    describe('getSignalsByCategory', () => {
      beforeEach(() => {
        manager.registerSignal({ name: 'val1', category: SignalCategory.VALIDATION });
        manager.registerSignal({ name: 'val2', category: SignalCategory.VALIDATION });
        manager.registerSignal({ name: 'ci1', category: SignalCategory.CI_CD });
        manager.updateSignal('val1', 80);
        manager.updateSignal('val2', 90);
        manager.updateSignal('ci1', 70);
      });

      it('should filter by category', () => {
        const validation = manager.getSignalsByCategory(SignalCategory.VALIDATION);
        expect(validation.length).toBe(2);

        const cicd = manager.getSignalsByCategory(SignalCategory.CI_CD);
        expect(cicd.length).toBe(1);
      });

      it('should return empty for unused category', () => {
        const security = manager.getSignalsByCategory(SignalCategory.SECURITY);
        expect(security.length).toBe(0);
      });
    });

    describe('getOverallState', () => {
      beforeEach(() => {
        manager.registerSignal({ name: 'sig1', category: SignalCategory.VALIDATION });
        manager.registerSignal({ name: 'sig2', category: SignalCategory.CI_CD });
      });

      it('should return GREEN when no signals', () => {
        const emptyManager = new AndonSignalManager();
        expect(emptyManager.getOverallState()).toBe(AndonState.GREEN);
      });

      it('should return worst state among signals', () => {
        manager.updateSignal('sig1', 90); // GREEN
        manager.updateSignal('sig2', 30); // RED

        expect(manager.getOverallState()).toBe(AndonState.RED);
      });

      it('should return YELLOW if worst is YELLOW', () => {
        manager.updateSignal('sig1', 90); // GREEN
        manager.updateSignal('sig2', 70); // YELLOW

        expect(manager.getOverallState()).toBe(AndonState.YELLOW);
      });
    });

    describe('getWeightedScore', () => {
      it('should return 0 when no signals', () => {
        expect(manager.getWeightedScore()).toBe(0);
      });

      it('should calculate weighted average', () => {
        manager.registerSignal({ name: 'sig1', category: SignalCategory.VALIDATION, weight: 0.7 });
        manager.registerSignal({ name: 'sig2', category: SignalCategory.CI_CD, weight: 0.3 });
        manager.updateSignal('sig1', 100);
        manager.updateSignal('sig2', 0);

        // (100 * 0.7 + 0 * 0.3) / 1.0 = 70
        expect(manager.getWeightedScore()).toBe(70);
      });
    });

    describe('onSignalChange', () => {
      it('should notify listeners on state change', () => {
        const callback = vi.fn();
        manager.registerSignal({ name: 'sig', category: SignalCategory.VALIDATION });
        manager.onSignalChange(callback);

        manager.updateSignal('sig', 50);

        expect(callback).toHaveBeenCalledTimes(1);
        expect(callback).toHaveBeenCalledWith(expect.objectContaining({
          newState: AndonState.RED
        }));
      });

      it('should not notify when state unchanged', () => {
        const callback = vi.fn();
        manager.registerSignal({ name: 'sig', category: SignalCategory.VALIDATION });
        manager.onSignalChange(callback);

        manager.updateSignal('sig', 50); // RED
        manager.updateSignal('sig', 55); // Still RED

        expect(callback).toHaveBeenCalledTimes(1);
      });

      it('should return unsubscribe function', () => {
        const callback = vi.fn();
        manager.registerSignal({ name: 'sig', category: SignalCategory.VALIDATION });
        const unsubscribe = manager.onSignalChange(callback);

        manager.updateSignal('sig', 50);
        expect(callback).toHaveBeenCalledTimes(1);

        unsubscribe();
        manager.updateSignal('sig', 90);
        expect(callback).toHaveBeenCalledTimes(1); // Still 1, not 2
      });

      it('should throw for non-function callback', () => {
        expect(() => manager.onSignalChange('not a function')).toThrow(TypeError);
      });

      it('should handle listener errors gracefully', () => {
        const errorCallback = vi.fn(() => { throw new Error('Listener error'); });
        const goodCallback = vi.fn();

        manager.registerSignal({ name: 'sig', category: SignalCategory.VALIDATION });
        manager.onSignalChange(errorCallback);
        manager.onSignalChange(goodCallback);

        // Should not throw, and should still call other listeners
        expect(() => manager.updateSignal('sig', 50)).not.toThrow();
        expect(goodCallback).toHaveBeenCalled();
      });
    });

    describe('getSignalHistory', () => {
      it('should return empty array for unknown signal', () => {
        expect(manager.getSignalHistory('unknown')).toEqual([]);
      });

      it('should limit history entries', () => {
        manager.registerSignal({ name: 'sig', category: SignalCategory.VALIDATION });
        for (let i = 0; i < 20; i++) {
          manager.updateSignal('sig', i * 5);
        }

        const history = manager.getSignalHistory('sig', 5);
        expect(history.length).toBe(5);
      });
    });

    describe('getDashboardSummary', () => {
      beforeEach(() => {
        manager.registerSignal({ name: 'val', category: SignalCategory.VALIDATION, weight: 0.5 });
        manager.registerSignal({ name: 'ci', category: SignalCategory.CI_CD, weight: 0.5 });
        manager.updateSignal('val', 90);
        manager.updateSignal('ci', 70);
      });

      it('should return comprehensive summary', () => {
        const summary = manager.getDashboardSummary();

        expect(summary.overallState).toBe(AndonState.YELLOW);
        expect(summary.overallScore).toBe(80); // (90*0.5 + 70*0.5) / 1 = 80
        expect(summary.deployment.ready).toBe(false);
        expect(summary.byCategory.validation.length).toBe(1);
        expect(summary.byCategory.cicd.length).toBe(1);
        expect(summary.timestamp).toBeDefined();
      });
    });

    describe('clear', () => {
      it('should clear all data', () => {
        manager.registerSignal({ name: 'sig', category: SignalCategory.VALIDATION });
        manager.updateSignal('sig', 80);
        manager.onSignalChange(() => {});

        manager.clear();

        expect(manager.signals.size).toBe(0);
        expect(manager.signalConfigs.size).toBe(0);
        expect(manager.listeners.length).toBe(0);
      });
    });
  });

  describe('Factory Functions', () => {
    it('should create manager with createAndonSignalManager', () => {
      const manager = createAndonSignalManager({ strictDeployment: false });
      expect(manager).toBeInstanceOf(AndonSignalManager);
      expect(manager.config.strictDeployment).toBe(false);
    });

    it('should export default manager instance', () => {
      expect(defaultAndonSignalManager).toBeInstanceOf(AndonSignalManager);
    });
  });

  describe('EventEmitter Integration', () => {
    it('should emit signalChange events', () => {
      const manager = new AndonSignalManager();
      const eventHandler = vi.fn();

      manager.on('signalChange', eventHandler);
      manager.registerSignal({ name: 'sig', category: SignalCategory.VALIDATION });
      manager.updateSignal('sig', 50);

      expect(eventHandler).toHaveBeenCalledWith(expect.objectContaining({
        signal: expect.objectContaining({ name: 'sig' }),
        newState: AndonState.RED
      }));
    });
  });

  describe('Edge Cases', () => {
    let manager;

    beforeEach(() => {
      manager = new AndonSignalManager();
    });

    it('should handle rapid signal updates', () => {
      manager.registerSignal({ name: 'sig', category: SignalCategory.VALIDATION });

      for (let i = 0; i < 100; i++) {
        manager.updateSignal('sig', Math.random() * 100);
      }

      const history = manager.getSignalHistory('sig', 100);
      expect(history.length).toBe(100);
    });

    it('should handle concurrent category registrations', () => {
      manager.registerValidationSignals();
      manager.registerCISignals();
      manager.registerPerformanceSignals();

      expect(manager.signalConfigs.size).toBe(20); // 7 + 9 + 4
    });

    it('should preserve signal metadata across updates', () => {
      manager.registerSignal({ name: 'sig', category: SignalCategory.VALIDATION });

      manager.updateSignal('sig', 80, { metadata: { key: 'value1' } });
      manager.updateSignal('sig', 85, { metadata: { key: 'value2' } });

      const signal = manager.signals.get('sig');
      expect(signal.metadata.key).toBe('value2');
    });
  });
});
