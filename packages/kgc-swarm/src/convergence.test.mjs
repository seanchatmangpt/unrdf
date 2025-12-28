/**
 * @file convergence.test.mjs
 * @description Test suite for convergence detection and metrics collection
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { ConvergenceDetector } from './convergence.mjs';
import { MetricsCollector } from './metrics.mjs';

describe('ConvergenceDetector', () => {
  let detector;

  beforeEach(() => {
    detector = new ConvergenceDetector({
      driftThreshold: 0.01,
      windowSize: 3,
      diminishingThreshold: 0.1,
      budget: {
        maxTime: 10000,
        maxSteps: 100,
        maxBytes: 1000000,
        maxNetworkOps: 50,
      },
    });
  });

  describe('drift calculation', () => {
    it('should calculate drift for added artifacts', () => {
      const previous = {
        timestamp: Date.now() - 1000,
        artifacts: new Set(['a1', 'a2']),
        weights: new Map([['a1', 1], ['a2', 1]]),
        totalSize: 100,
      };

      const current = {
        timestamp: Date.now(),
        artifacts: new Set(['a1', 'a2', 'a3', 'a4']),
        weights: new Map([['a1', 1], ['a2', 1], ['a3', 1], ['a4', 1]]),
        totalSize: 200,
      };

      const drift = detector.calculateDrift(current, previous);

      expect(drift.drift).toBe(2);
      expect(drift.added).toBe(2);
      expect(drift.removed).toBe(0);
      expect(drift.modified).toBe(0);
      expect(drift.normalized).toBe(0.5); // 2/4 = 0.5
    });

    it('should calculate drift for removed artifacts', () => {
      const previous = {
        timestamp: Date.now() - 1000,
        artifacts: new Set(['a1', 'a2', 'a3', 'a4']),
        weights: new Map(),
        totalSize: 200,
      };

      const current = {
        timestamp: Date.now(),
        artifacts: new Set(['a1', 'a2']),
        weights: new Map(),
        totalSize: 100,
      };

      const drift = detector.calculateDrift(current, previous);

      expect(drift.drift).toBe(2);
      expect(drift.added).toBe(0);
      expect(drift.removed).toBe(2);
      expect(drift.normalized).toBe(1.0); // 2/2 = 1.0
    });

    it('should calculate drift for modified artifacts', () => {
      const previous = {
        timestamp: Date.now() - 1000,
        artifacts: new Set(['a1', 'a2', 'a3']),
        weights: new Map([['a1', 1], ['a2', 2], ['a3', 3]]),
        totalSize: 100,
      };

      const current = {
        timestamp: Date.now(),
        artifacts: new Set(['a1', 'a2', 'a3']),
        weights: new Map([['a1', 1], ['a2', 5], ['a3', 3]]),
        totalSize: 100,
      };

      const drift = detector.calculateDrift(current, previous);

      expect(drift.drift).toBe(1);
      expect(drift.added).toBe(0);
      expect(drift.removed).toBe(0);
      expect(drift.modified).toBe(1);
      expect(drift.normalized).toBeCloseTo(0.333, 2);
    });

    it('should handle zero drift', () => {
      const state = {
        timestamp: Date.now(),
        artifacts: new Set(['a1', 'a2']),
        weights: new Map([['a1', 1], ['a2', 1]]),
        totalSize: 100,
      };

      const drift = detector.calculateDrift(state, state);

      expect(drift.drift).toBe(0);
      expect(drift.normalized).toBe(0);
    });
  });

  describe('epoch recording', () => {
    it('should record first epoch without drift', () => {
      const state = {
        timestamp: Date.now(),
        artifacts: new Set(['a1', 'a2']),
        weights: new Map(),
        totalSize: 100,
      };

      const drift = detector.recordEpoch(state);

      expect(drift).toBeNull();
      expect(detector.history.length).toBe(1);
      expect(detector.stepCount).toBe(1);
    });

    it('should record subsequent epochs with drift', () => {
      const state1 = {
        timestamp: Date.now(),
        artifacts: new Set(['a1', 'a2']),
        weights: new Map(),
        totalSize: 100,
      };

      const state2 = {
        timestamp: Date.now(),
        artifacts: new Set(['a1', 'a2', 'a3']),
        weights: new Map(),
        totalSize: 150,
      };

      detector.recordEpoch(state1);
      const drift = detector.recordEpoch(state2);

      expect(drift).not.toBeNull();
      expect(drift.added).toBe(1);
      expect(detector.history.length).toBe(2);
      expect(detector.driftHistory.length).toBe(1);
    });
  });

  describe('convergence detection', () => {
    it('should detect saturation when drift below threshold', () => {
      const states = [
        { artifacts: new Set(['a1', 'a2', 'a3', 'a4', 'a5']), weights: new Map(), totalSize: 100 },
        { artifacts: new Set(['a1', 'a2', 'a3', 'a4', 'a5', 'a6']), weights: new Map(), totalSize: 120 },
      ];

      states.forEach(s => detector.recordEpoch({ ...s, timestamp: Date.now() }));

      expect(detector.isSaturated()).toBe(false);

      // Add state with minimal drift (normalized < 0.01)
      detector.recordEpoch({
        timestamp: Date.now(),
        artifacts: new Set(['a1', 'a2', 'a3', 'a4', 'a5', 'a6']),
        weights: new Map(),
        totalSize: 120,
      });

      expect(detector.isSaturated()).toBe(true);
    });

    it('should detect diminishing drift', () => {
      const states = [
        { artifacts: new Set(['a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 'a10']), totalSize: 100 },
        { artifacts: new Set(['a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 'a10', 'a11', 'a12', 'a13', 'a14', 'a15']), totalSize: 150 }, // +5 (normalized: 0.333)
        { artifacts: new Set(['a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 'a10', 'a11', 'a12', 'a13', 'a14', 'a15', 'a16', 'a17']), totalSize: 170 }, // +2 (normalized: 0.118)
        { artifacts: new Set(['a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 'a10', 'a11', 'a12', 'a13', 'a14', 'a15', 'a16', 'a17', 'a18']), totalSize: 180 }, // +1 (normalized: 0.056)
      ];

      states.forEach(s => detector.recordEpoch({ ...s, timestamp: Date.now(), weights: new Map() }));

      expect(detector.isDriftDiminishing()).toBe(true);
    });

    it('should not detect diminishing drift with insufficient data', () => {
      const state = {
        timestamp: Date.now(),
        artifacts: new Set(['a1', 'a2']),
        weights: new Map(),
        totalSize: 100,
      };

      detector.recordEpoch(state);

      expect(detector.isDriftDiminishing()).toBe(false);
    });

    it('should check convergence and return status', () => {
      const states = [
        { artifacts: new Set(Array.from({ length: 100 }, (_, i) => `a${i}`)), totalSize: 1000 },
        { artifacts: new Set(Array.from({ length: 120 }, (_, i) => `a${i}`)), totalSize: 1200 },
        { artifacts: new Set(Array.from({ length: 130 }, (_, i) => `a${i}`)), totalSize: 1300 },
        { artifacts: new Set(Array.from({ length: 135 }, (_, i) => `a${i}`)), totalSize: 1350 },
      ];

      states.forEach(s => detector.recordEpoch({ ...s, timestamp: Date.now(), weights: new Map() }));

      const result = detector.checkConvergence();

      expect(result.converged).toBe(true);
      expect(result.reason).toContain('Diminishing');
    });
  });

  describe('budget enforcement', () => {
    it('should enforce time budget', async () => {
      const shortDetector = new ConvergenceDetector({
        driftThreshold: 0.01,
        budget: { maxTime: 100 },
      });

      await new Promise(resolve => setTimeout(resolve, 150));

      const budgetStatus = shortDetector.checkBudget();
      expect(budgetStatus.exceeded).toBe(true);
      expect(budgetStatus.reason).toContain('Time budget');
    });

    it('should enforce step budget', () => {
      const state = {
        timestamp: Date.now(),
        artifacts: new Set(['a1']),
        weights: new Map(),
        totalSize: 10,
      };

      for (let i = 0; i < 105; i++) {
        detector.recordEpoch({ ...state, artifacts: new Set([`a${i}`]) });
      }

      const budgetStatus = detector.checkBudget();
      expect(budgetStatus.exceeded).toBe(true);
      expect(budgetStatus.reason).toContain('Step budget');
    });

    it('should enforce bytes budget', () => {
      const state = {
        timestamp: Date.now(),
        artifacts: new Set(['a1']),
        weights: new Map(),
        totalSize: 500000,
      };

      detector.recordEpoch(state);
      detector.recordEpoch({ ...state, totalSize: 600000 });

      const budgetStatus = detector.checkBudget();
      expect(budgetStatus.exceeded).toBe(true);
      expect(budgetStatus.reason).toContain('Bytes budget');
    });

    it('should enforce network ops budget', () => {
      for (let i = 0; i < 55; i++) {
        detector.recordNetworkOp();
      }

      const budgetStatus = detector.checkBudget();
      expect(budgetStatus.exceeded).toBe(true);
      expect(budgetStatus.reason).toContain('Network ops budget');
    });
  });

  describe('metrics', () => {
    it('should return current metrics', () => {
      const states = [
        { artifacts: new Set(['a1', 'a2']), totalSize: 100 },
        { artifacts: new Set(['a1', 'a2', 'a3']), totalSize: 150 },
      ];

      states.forEach(s => detector.recordEpoch({ ...s, timestamp: Date.now(), weights: new Map() }));

      const metrics = detector.getMetrics();

      expect(metrics.epochCount).toBe(2);
      expect(metrics.stepCount).toBe(2);
      expect(metrics.currentDrift).not.toBeNull();
      expect(metrics.totalBytes).toBe(250);
    });

    it('should reset detector state', () => {
      const state = {
        timestamp: Date.now(),
        artifacts: new Set(['a1', 'a2']),
        weights: new Map(),
        totalSize: 100,
      };

      detector.recordEpoch(state);
      detector.recordEpoch(state);
      detector.reset();

      expect(detector.history.length).toBe(0);
      expect(detector.driftHistory.length).toBe(0);
      expect(detector.stepCount).toBe(0);
      expect(detector.converged).toBe(false);
    });
  });
});

describe('MetricsCollector', () => {
  let collector;

  beforeEach(() => {
    collector = new MetricsCollector();
  });

  describe('drift tracking', () => {
    it('should record drift measurements', () => {
      collector.recordDrift({ drift: 5, normalized: 0.5, added: 3, removed: 2, modified: 0 });
      collector.recordDrift({ drift: 3, normalized: 0.3, added: 2, removed: 1, modified: 0 });

      expect(collector.driftHistory.length).toBe(2);
      expect(collector.driftHistory[0].drift).toBe(5);
      expect(collector.driftHistory[1].drift).toBe(3);
    });

    it('should calculate drift statistics', () => {
      const drifts = [
        { drift: 10, normalized: 1.0, added: 10, removed: 0, modified: 0 },
        { drift: 5, normalized: 0.5, added: 5, removed: 0, modified: 0 },
        { drift: 2, normalized: 0.2, added: 2, removed: 0, modified: 0 },
      ];

      drifts.forEach(d => collector.recordDrift(d));

      const stats = collector.getDriftStats();

      expect(stats.normalized.mean).toBeCloseTo(0.567, 2);
      expect(stats.normalized.min).toBe(0.2);
      expect(stats.normalized.max).toBe(1.0);
      expect(stats.trend).toBe('decreasing');
    });
  });

  describe('performance tracking', () => {
    it('should record performance samples', () => {
      collector.recordPerformance({ throughput: 100, latency: 50 });
      collector.recordPerformance({ throughput: 120, latency: 45 });

      expect(collector.performanceSamples.length).toBe(2);
    });

    it('should calculate performance statistics', () => {
      const samples = [
        { throughput: 100, latency: 50, memoryUsed: 1000000 },
        { throughput: 120, latency: 45, memoryUsed: 1100000 },
        { throughput: 110, latency: 48, memoryUsed: 1050000 },
      ];

      samples.forEach(s => collector.recordPerformance(s));

      const stats = collector.getPerformanceStats();

      expect(stats.throughput.mean).toBeCloseTo(110, 0);
      expect(stats.latency.mean).toBeCloseTo(47.667, 2);
      expect(stats.memory.mean).toBeCloseTo(1050000, 0);
    });

    it('should calculate average latency', () => {
      collector.recordPerformance({ latency: 50 });
      collector.recordPerformance({ latency: 60 });
      collector.recordPerformance({ latency: 40 });

      const stats = collector.getPerformanceStats();

      expect(stats.avgLatency).toBe(50);
      expect(collector.totalOperations).toBe(3);
    });
  });

  describe('budget tracking', () => {
    it('should record budget consumption', () => {
      collector.recordBudget({ step: 10, time: 1000, bytes: 5000, networkOps: 5 });
      collector.recordBudget({ step: 20, time: 2000, bytes: 10000, networkOps: 10 });

      expect(collector.budgetConsumption.length).toBe(2);
    });

    it('should calculate budget statistics', () => {
      const budgets = [
        { step: 10, time: 1000, bytes: 5000, networkOps: 5 },
        { step: 20, time: 2000, bytes: 10000, networkOps: 10 },
        { step: 30, time: 3000, bytes: 15000, networkOps: 15 },
      ];

      budgets.forEach(b => collector.recordBudget(b));

      const stats = collector.getBudgetStats();

      expect(stats.current.step).toBe(30);
      expect(stats.time.mean).toBe(2000);
      expect(stats.bytes.mean).toBe(10000);
    });
  });

  describe('expansion rate tracking', () => {
    it('should record expansion rates', () => {
      collector.recordExpansionRate(100, 10);
      collector.recordExpansionRate(110, 10);

      expect(collector.expansionRates.length).toBe(2);
    });

    it('should calculate expansion statistics', async () => {
      collector.recordExpansionRate(100, 10);
      await new Promise(resolve => setTimeout(resolve, 10));
      collector.recordExpansionRate(110, 10);
      await new Promise(resolve => setTimeout(resolve, 10));
      collector.recordExpansionRate(120, 10);

      const stats = collector.getExpansionStats();

      expect(stats.current.totalArtifacts).toBe(120);
      expect(stats.current.deltaArtifacts).toBe(10);
      expect(stats.stats.mean).toBeGreaterThanOrEqual(0);
    });
  });

  describe('summary and utilities', () => {
    it('should generate comprehensive summary', () => {
      collector.recordDrift({ drift: 5, normalized: 0.5, added: 5, removed: 0, modified: 0 });
      collector.recordPerformance({ throughput: 100, latency: 50 });
      collector.recordBudget({ step: 10, time: 1000, bytes: 5000, networkOps: 5 });
      collector.incrementEpoch();

      const summary = collector.getSummary();

      expect(summary.epochCount).toBe(1);
      expect(summary.drift).not.toBeNull();
      expect(summary.performance).not.toBeNull();
      expect(summary.budget).not.toBeNull();
    });

    it('should export to JSON', () => {
      collector.recordDrift({ drift: 5, normalized: 0.5, added: 5, removed: 0, modified: 0 });

      const json = collector.toJSON();
      const parsed = JSON.parse(json);

      expect(parsed.drift).not.toBeNull();
    });

    it('should reset collector', () => {
      collector.recordDrift({ drift: 5, normalized: 0.5, added: 5, removed: 0, modified: 0 });
      collector.recordPerformance({ throughput: 100 });
      collector.reset();

      expect(collector.driftHistory.length).toBe(0);
      expect(collector.performanceSamples.length).toBe(0);
      expect(collector.epochCount).toBe(0);
    });

    it('should get snapshot for time range', () => {
      const now = Date.now();

      collector.recordDrift({ drift: 5, normalized: 0.5, timestamp: now - 2000, added: 5, removed: 0, modified: 0 });
      collector.recordDrift({ drift: 3, normalized: 0.3, timestamp: now - 1000, added: 3, removed: 0, modified: 0 });
      collector.recordDrift({ drift: 1, normalized: 0.1, timestamp: now, added: 1, removed: 0, modified: 0 });

      const snapshot = collector.getSnapshot(now - 1500, now - 500);

      expect(snapshot.driftHistory.length).toBe(1);
      expect(snapshot.driftHistory[0].drift).toBe(3);
    });
  });

  describe('benchmarks', () => {
    it('should handle high-volume drift recording efficiently', () => {
      const start = performance.now();
      const iterations = 10000;

      for (let i = 0; i < iterations; i++) {
        collector.recordDrift({
          drift: Math.random() * 10,
          normalized: Math.random(),
          added: Math.floor(Math.random() * 5),
          removed: Math.floor(Math.random() * 5),
          modified: Math.floor(Math.random() * 5),
        });
      }

      const elapsed = performance.now() - start;
      const opsPerSec = (iterations / elapsed) * 1000;

      expect(collector.driftHistory.length).toBe(iterations);
      expect(opsPerSec).toBeGreaterThan(10000); // Should handle >10k ops/sec
    });

    it('should calculate statistics efficiently', () => {
      // Record 1000 samples
      for (let i = 0; i < 1000; i++) {
        collector.recordPerformance({
          throughput: 100 + Math.random() * 50,
          latency: 40 + Math.random() * 20,
          memoryUsed: 1000000 + Math.random() * 500000,
        });
      }

      const start = performance.now();
      const stats = collector.getPerformanceStats();
      const elapsed = performance.now() - start;

      expect(stats.throughput).not.toBeNull();
      expect(elapsed).toBeLessThan(100); // Should complete in <100ms
    });
  });
});
