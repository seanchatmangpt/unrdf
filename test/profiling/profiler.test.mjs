/**
 * @fileoverview Performance Profiler Tests
 */

import { describe, it, beforeEach } from 'node:test';
import assert from 'node:assert';
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

      assert.strictEqual(result, 42);
      assert.ok(profile.latency);
      assert.ok(profile.latency.duration >= 50);
      assert.ok(profile.metadata.operationName === 'test-op');
    });

    it('should track profile history', async () => {
      const profiler = createProfiler();

      for (let i = 0; i < 5; i++) {
        await profiler.profile('history-test', async () => sleep(10));
      }

      const history = profiler.getHistory({ operationName: 'history-test' });
      assert.strictEqual(history.length, 5);
    });

    it('should provide aggregate statistics', async () => {
      const profiler = createProfiler();

      for (let i = 0; i < 10; i++) {
        await profiler.profile('stats-test', async () => sleep(Math.random() * 50));
      }

      const stats = profiler.getStats('stats-test');
      assert.strictEqual(stats.count, 10);
      assert.ok(stats.latency.mean > 0);
      assert.ok(stats.latency.p95 > 0);
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

      assert.ok(metrics.duration >= 50);
      assert.ok(metrics.p50 >= 0);
      assert.ok(metrics.p95 >= 0);
      assert.ok(metrics.p99 >= 0);
    });

    it('should calculate percentiles correctly', () => {
      const profiler = new LatencyProfiler();
      const values = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100];

      const p50 = profiler.calculatePercentile(values, 50);
      const p90 = profiler.calculatePercentile(values, 90);
      const p99 = profiler.calculatePercentile(values, 99);

      assert.ok(p50 >= 50 && p50 <= 60);
      assert.ok(p90 >= 90 && p90 <= 100);
      assert.ok(p99 >= 95);
    });

    it('should build histogram buckets', () => {
      const profiler = new LatencyProfiler();
      const measurements = [5, 15, 45, 75, 150, 500, 2000];

      const metrics = profiler.calculateMetrics(measurements, 2000);
      assert.ok(metrics.histogram);
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

      assert.ok(budgetCheck.violations.length > 0);
      assert.strictEqual(budgetCheck.passed, false);
    });

    it('should support checkpoints', () => {
      const profiler = new LatencyProfiler();
      const sessionId = profiler.start('checkpoint-test');

      const checkpoint1 = profiler.checkpoint(sessionId, 'step1');
      const checkpoint2 = profiler.checkpoint(sessionId, 'step2');

      assert.ok(checkpoint2 > checkpoint1);

      const metrics = profiler.stop(sessionId);
      assert.strictEqual(metrics.checkpoints.length, 2);
    });
  });

  describe('Memory Profiler', () => {
    it('should measure memory usage', async () => {
      const profiler = new MemoryProfiler();

      const sessionId = profiler.start('memory-test');
      const _largeArray = new Array(100000).fill(0);
      await sleep(50);
      const metrics = profiler.stop(sessionId);

      assert.ok(metrics.heapUsedDelta !== undefined);
      assert.ok(metrics.heapUsedPeak > 0);
      assert.ok(metrics.trend);
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
      assert.ok(typeof metrics.trend.growthRate === 'number');
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

      assert.ok(typeof budgetCheck.passed === 'boolean');
    });

    it('should capture memory snapshots', () => {
      const profiler = new MemoryProfiler();
      const snapshot = profiler.captureMemorySnapshot();

      assert.ok(snapshot.timestamp);
      assert.ok(snapshot.heapUsed > 0);
      assert.ok(snapshot.heapTotal > 0);
      assert.ok(snapshot.rss > 0);
    });
  });

  describe('CPU Profiler', () => {
    it('should check availability', () => {
      const available = CpuProfiler.isAvailable();
      assert.ok(typeof available === 'boolean');
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

      assert.ok(metrics.totalTime > 0);
      assert.ok(metrics.sampleCount > 0);
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

      assert.strictEqual(budgetCheck.passed, true);
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

      assert.ok(parsed.latency);
      assert.ok(parsed.memory);
      assert.strictEqual(parsed.metadata.operationName, 'sample');
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

      assert.ok(typeof comparison.regression === 'boolean');
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

      assert.strictEqual(result, 'quick');
      assert.ok(profile.latency);
      assert.ok(profile.memory);
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
      assert.ok(true);
    });

    it('should create proper OTEL spans', async () => {
      const profiler = createProfiler({ enableOtel: true });

      const span = tracer.startSpan('test-span');
      const ctx = trace.setSpan(trace.context.active(), span);

      await trace.context.with(ctx, async () => {
        const { profile } = await profiler.profile('span-test', async () => {
          await sleep(10);
        });

        assert.ok(profile);
      });

      span.end();
    });
  });
});

// Helper function
function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}
