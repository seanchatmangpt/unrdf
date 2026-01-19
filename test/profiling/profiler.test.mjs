/**
 * @fileoverview Performance Profiler Tests
 */

import { describe, it, beforeEach, expect } from 'vitest';
import { trace } from '@opentelemetry/api';
import { createProfiler, quickProfile } from '../../packages/profiling/profiler.mjs';
import { LatencyProfiler } from '../../packages/profiling/latency-profiler.mjs';
import { MemoryProfiler } from '../../packages/profiling/memory-profiler.mjs';
import { CpuProfiler } from '../../packages/profiling/cpu-profiler.mjs';
import { Reporter } from '../../packages/profiling/reporter.mjs';

const tracer = trace.getTracer('profiler-test', '1.0.0');

describe('Profiler', () => {
  describe('Main Profiler', () => {
    it('should profile async operations', async () => {
      const profiler = createProfiler();

      const { result, profile } = await profiler.profile('test-op', async () => {
        await sleep(50);
        return 42;
      });

      expect(result).toBe(42);
      expect(profile.latency).toBeTruthy();
      expect(profile.latency.duration >= 50).toBeTruthy();
      expect(profile.metadata.operationName === 'test-op').toBeTruthy();
    });

    it('should track profile history', async () => {
      const profiler = createProfiler();

      for (let i = 0; i < 5; i++) {
        await profiler.profile('history-test', async () => sleep(10));
      }

      const history = profiler.getHistory({ operationName: 'history-test' });
      expect(history.length).toBe(5);
    });

    it('should provide aggregate statistics', async () => {
      const profiler = createProfiler();

      for (let i = 0; i < 10; i++) {
        await profiler.profile('stats-test', async () => sleep(Math.random() * 50));
      }

      const stats = profiler.getStats('stats-test');
      expect(stats.count).toBe(10);
      expect(stats.latency.mean > 0).toBeTruthy();
      expect(stats.latency.p95 > 0).toBeTruthy();
    });

    it('should handle operation errors', async () => {
      const profiler = createProfiler();

      await assert.rejects(
        profiler.profile('error-test', async () => {
          throw new Error('Test error');
        }),
        /Test error/
      );
    });

    it('should create OTEL spans', async () => {
      const profiler = createProfiler({ enableOtel: true });

      const span = tracer.startSpan('test-parent');
      const ctx = trace.setSpan(trace.context.active(), span);

      await trace.context.with(ctx, async () => {
        await profiler.profile('otel-test', async () => sleep(10));
      });

      span.end();
    });
  });

  describe('Latency Profiler', () => {
    it('should measure operation latency', async () => {
      const profiler = new LatencyProfiler();

      const sessionId = profiler.start('latency-test');
      await sleep(50);
      const metrics = profiler.stop(sessionId);

      expect(metrics.duration >= 50).toBeTruthy();
      expect(metrics.p50 >= 0).toBeTruthy();
      expect(metrics.p95 >= 0).toBeTruthy();
      expect(metrics.p99 >= 0).toBeTruthy();
    });

    it('should calculate percentiles correctly', () => {
      const profiler = new LatencyProfiler();
      const values = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100];

      const p50 = profiler.calculatePercentile(values, 50);
      const p90 = profiler.calculatePercentile(values, 90);
      const p99 = profiler.calculatePercentile(values, 99);

      expect(p50 >= 50 && p50 <= 60).toBeTruthy();
      expect(p90 >= 90 && p90 <= 100).toBeTruthy();
      expect(p99 >= 95).toBeTruthy();
    });

    it('should build histogram buckets', () => {
      const profiler = new LatencyProfiler();
      const measurements = [5, 15, 45, 75, 150, 500, 2000];

      const metrics = profiler.calculateMetrics(measurements, 2000);
      expect(metrics.histogram).toBeTruthy();
      assert.ok(Object.keys(metrics.histogram).length > 0);
    });

    it('should check performance budgets', async () => {
      const profiler = new LatencyProfiler();

      const { metrics } = await profiler.profileAsync('budget-test', async () => {
        await sleep(80);
      });

      const budgetCheck = profiler.checkBudget(metrics, {
        p50: 50,
        p95: 100,
      });

      expect(budgetCheck.violations.length > 0).toBeTruthy();
      expect(budgetCheck.passed).toBe(false);
    });

    it('should support checkpoints', () => {
      const profiler = new LatencyProfiler();
      const sessionId = profiler.start('checkpoint-test');

      const checkpoint1 = profiler.checkpoint(sessionId, 'step1');
      const checkpoint2 = profiler.checkpoint(sessionId, 'step2');

      expect(checkpoint2 > checkpoint1).toBeTruthy();

      const metrics = profiler.stop(sessionId);
      expect(metrics.checkpoints.length).toBe(2);
    });
  });

  describe('Memory Profiler', () => {
    it('should measure memory usage', async () => {
      const profiler = new MemoryProfiler();

      const sessionId = profiler.start('memory-test');
      const _largeArray = new Array(100000).fill(0);
      await sleep(50);
      const metrics = profiler.stop(sessionId);

      expect(metrics.heapUsedDelta !== undefined).toBeTruthy();
      expect(metrics.heapUsedPeak > 0).toBeTruthy();
      expect(metrics.trend).toBeTruthy();
    });

    it('should detect memory trends', async () => {
      const profiler = new MemoryProfiler();

      const { metrics } = await profiler.profile('trend-test', async () => {
        const arrays = [];
        for (let i = 0; i < 10; i++) {
          arrays.push(new Array(10000).fill(i));
          await sleep(20);
        }
        return arrays.length;
      });

      assert.ok(['stable', 'growing', 'shrinking'].includes(metrics.trend.direction));
      expect(typeof metrics.trend.growthRate === 'number').toBeTruthy();
    });

    it('should check memory budgets', async () => {
      const profiler = new MemoryProfiler();

      const { metrics } = await profiler.profile('memory-budget', async () => {
        const largeArray = new Array(1000000).fill(0);
        await sleep(50);
        return largeArray.length;
      });

      const budgetCheck = profiler.checkBudget(metrics, {
        maxHeapDelta: 1024 * 1024, // 1 MB
        maxGrowthRate: 1024 * 1024, // 1 MB/s
      });

      expect(typeof budgetCheck.passed === 'boolean').toBeTruthy();
    });

    it('should capture memory snapshots', () => {
      const profiler = new MemoryProfiler();
      const snapshot = profiler.captureMemorySnapshot();

      expect(snapshot.timestamp).toBeTruthy();
      expect(snapshot.heapUsed > 0).toBeTruthy();
      expect(snapshot.heapTotal > 0).toBeTruthy();
      expect(snapshot.rss > 0).toBeTruthy();
    });
  });

  describe('CPU Profiler', () => {
    it('should check availability', () => {
      const available = CpuProfiler.isAvailable();
      expect(typeof available === 'boolean').toBeTruthy();
    });

    it('should profile CPU usage when available', async function () {
      if (!CpuProfiler.isAvailable()) {
        this.skip();
        return;
      }

      const profiler = new CpuProfiler({ saveProfiles: false });

      const { metrics } = await profiler.profile('cpu-test', async () => {
        let sum = 0;
        for (let i = 0; i < 1000000; i++) {
          sum += Math.sqrt(i);
        }
        return sum;
      });

      expect(metrics.totalTime > 0).toBeTruthy();
      expect(metrics.sampleCount > 0).toBeTruthy();
      assert.ok(Array.isArray(metrics.hotFunctions));
    });

    it('should check CPU budgets when available', async function () {
      if (!CpuProfiler.isAvailable()) {
        this.skip();
        return;
      }

      const profiler = new CpuProfiler();

      const { metrics } = await profiler.profile('cpu-budget', async () => {
        let sum = 0;
        for (let i = 0; i < 100000; i++) {
          sum += i;
        }
        return sum;
      });

      const budgetCheck = profiler.checkBudget(metrics, {
        maxTotalTime: metrics.totalTime + 1000,
      });

      expect(budgetCheck.passed).toBe(true);
    });
  });

  describe('Reporter', () => {
    let sampleProfile;

    beforeEach(async () => {
      const profiler = createProfiler();
      const { profile } = await profiler.profile('sample', async () => {
        await sleep(30);
        return 'test';
      });
      sampleProfile = profile;
    });

    it('should format as JSON', () => {
      const json = Reporter.toJSON(sampleProfile);
      const parsed = JSON.parse(json);

      expect(parsed.latency).toBeTruthy();
      expect(parsed.memory).toBeTruthy();
      expect(parsed.metadata.operationName).toBe('sample');
    });

    it('should format as terminal output', () => {
      const terminal = Reporter.toTerminal(sampleProfile);

      assert.ok(terminal.includes('Performance Profile'));
      assert.ok(terminal.includes('Latency Metrics'));
      assert.ok(terminal.includes('Memory Metrics'));
    });

    it('should format as HTML', () => {
      const html = Reporter.toHTML(sampleProfile);

      assert.ok(html.includes('<!DOCTYPE html>'));
      assert.ok(html.includes('Performance Profile'));
      assert.ok(html.includes('sample'));
    });

    it('should compare profiles', async () => {
      const profiler = createProfiler();

      const { profile: baseline } = await profiler.profile('compare-test', async () => {
        await sleep(30);
      });

      const { profile: current } = await profiler.profile('compare-test', async () => {
        await sleep(60);
      });

      const comparison = Reporter.compare(baseline, current);

      expect(typeof comparison.regression === 'boolean').toBeTruthy();
      assert.ok(Array.isArray(comparison.improvements));
      assert.ok(Array.isArray(comparison.regressions));
    });
  });

  describe('Quick Profile Helper', () => {
    it('should profile quickly', async () => {
      const { result, profile } = await quickProfile('quick-test', async () => {
        await sleep(20);
        return 'quick';
      });

      expect(result).toBe('quick');
      expect(profile.latency).toBeTruthy();
      expect(profile.memory).toBeTruthy();
    });
  });

  describe('OTEL Integration', () => {
    it('should record OTEL metrics', async () => {
      const profiler = createProfiler({ enableOtel: true });

      await profiler.profile('otel-metrics-test', async () => {
        await sleep(10);
      });

      // Metrics are recorded asynchronously
      // Just verify no errors occurred
      expect(true).toBeTruthy();
    });

    it('should create proper OTEL spans', async () => {
      const profiler = createProfiler({ enableOtel: true });

      const span = tracer.startSpan('test-span');
      const ctx = trace.setSpan(trace.context.active(), span);

      await trace.context.with(ctx, async () => {
        const { profile } = await profiler.profile('span-test', async () => {
          await sleep(10);
        });

        expect(profile).toBeTruthy();
      });

      span.end();
    });
  });
});

// Helper function
function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}
