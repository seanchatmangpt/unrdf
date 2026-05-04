/**
 * @file Performance Probe Tests
 * @description Tests for performance benchmarking probe
 */

import { describe, it, expect } from 'vitest';
import { probePerformance } from '../src/probes/performance.mjs';
import { ObservationSchema } from '../src/schemas.mjs';

describe('Performance Probe', () => {
  it('should probe performance with default config', async () => {
    const observations = await probePerformance({
      roots: ['/tmp'],
      samples: 10, // Reduced for fast testing
      budgetMs: 30000,
      warmupIterations: 2, // Reduced for fast testing
    });

    expect(observations).toBeDefined();
    expect(Array.isArray(observations)).toBe(true);
    expect(observations.length).toBeGreaterThan(0);

    // Verify all observations are valid
    for (const obs of observations) {
      expect(() => ObservationSchema.parse(obs)).not.toThrow();
    }
  });

  it('should include JSON parse benchmarks', async () => {
    const observations = await probePerformance({
      roots: ['/tmp'],
      samples: 10,
      budgetMs: 30000,
      warmupIterations: 2,
    });

    const jsonParseObservations = observations.filter(obs => obs.metric.startsWith('json.parse'));
    expect(jsonParseObservations.length).toBeGreaterThan(0);

    // Should have small, medium, large variants
    const metrics = jsonParseObservations.map(obs => obs.metric);
    expect(metrics).toContain('json.parse.small');
    expect(metrics).toContain('json.parse.medium');
    expect(metrics).toContain('json.parse.large');
  });

  it('should include JSON stringify benchmarks', async () => {
    const observations = await probePerformance({
      roots: ['/tmp'],
      samples: 10,
      budgetMs: 30000,
      warmupIterations: 2,
    });

    const jsonStringifyObservations = observations.filter(obs =>
      obs.metric.startsWith('json.stringify')
    );
    expect(jsonStringifyObservations.length).toBeGreaterThan(0);

    const metrics = jsonStringifyObservations.map(obs => obs.metric);
    expect(metrics).toContain('json.stringify.small');
    expect(metrics).toContain('json.stringify.medium');
    expect(metrics).toContain('json.stringify.large');
  });

  it('should include BLAKE3 hashing benchmarks', async () => {
    const observations = await probePerformance({
      roots: ['/tmp'],
      samples: 10,
      budgetMs: 30000,
      warmupIterations: 2,
    });

    const hashObservations = observations.filter(obs => obs.metric.startsWith('hash.blake3'));
    expect(hashObservations.length).toBeGreaterThan(0);

    const metrics = hashObservations.map(obs => obs.metric);
    expect(metrics).toContain('hash.blake3.small');
    expect(metrics).toContain('hash.blake3.medium');
    expect(metrics).toContain('hash.blake3.large');
  });

  it('should include streaming benchmarks', async () => {
    const observations = await probePerformance({
      roots: ['/tmp'],
      samples: 10,
      budgetMs: 30000,
      warmupIterations: 2,
    });

    const streamObservations = observations.filter(obs => obs.metric.startsWith('stream'));
    expect(streamObservations.length).toBeGreaterThan(0);
  });

  it('should include file I/O benchmarks', async () => {
    const observations = await probePerformance({
      roots: ['/tmp'],
      samples: 10,
      budgetMs: 30000,
      warmupIterations: 2,
    });

    const fileReadObservations = observations.filter(obs => obs.metric.startsWith('file.read'));
    const fileWriteObservations = observations.filter(obs => obs.metric.startsWith('file.write'));

    expect(fileReadObservations.length).toBeGreaterThan(0);
    expect(fileWriteObservations.length).toBeGreaterThan(0);
  });

  it('should include buffer operation benchmarks', async () => {
    const observations = await probePerformance({
      roots: ['/tmp'],
      samples: 10,
      budgetMs: 30000,
      warmupIterations: 2,
    });

    const bufferObservations = observations.filter(obs => obs.metric.startsWith('buffer'));
    expect(bufferObservations.length).toBeGreaterThan(0);
  });

  it('should include string operation benchmarks', async () => {
    const observations = await probePerformance({
      roots: ['/tmp'],
      samples: 10,
      budgetMs: 30000,
      warmupIterations: 2,
    });

    const stringObservations = observations.filter(obs => obs.metric.startsWith('string'));
    expect(stringObservations.length).toBeGreaterThan(0);

    const metrics = stringObservations.map(obs => obs.metric);
    expect(metrics).toContain('string.concat');
    expect(metrics).toContain('string.slice');
    expect(metrics).toContain('string.regex');
  });

  it('should provide statistical metadata', async () => {
    const observations = await probePerformance({
      roots: ['/tmp'],
      samples: 10,
      budgetMs: 30000,
      warmupIterations: 2,
    });

    const obs = observations[0];
    expect(obs.metadata).toBeDefined();
    expect(obs.metadata.mean_ms).toBeDefined();
    expect(obs.metadata.median_ms).toBeDefined();
    expect(obs.metadata.p95_ms).toBeDefined();
    expect(obs.metadata.p99_ms).toBeDefined();
    expect(obs.metadata.stddev_ms).toBeDefined();
  });

  it('should respect budget constraint', async () => {
    const budgetMs = 5000; // 5 seconds

    const start = performance.now();

    await expect(async () => {
      await probePerformance({
        roots: ['/tmp'],
        samples: 10000, // Very high to trigger budget
        budgetMs,
        warmupIterations: 1,
      });
    }).rejects.toThrow(/budget exceeded/i);

    const elapsed = performance.now() - start;
    // Should fail before taking too long (allow some margin)
    expect(elapsed).toBeLessThan(budgetMs * 1.5);
  });

  it('should validate configuration', async () => {
    await expect(async () => {
      await probePerformance({
        samples: -1, // Invalid
      });
    }).rejects.toThrow();

    await expect(async () => {
      await probePerformance({
        budgetMs: 1000000, // Exceeds max
      });
    }).rejects.toThrow();
  });

  it('should have stable ordering of results', async () => {
    const observations1 = await probePerformance({
      roots: ['/tmp'],
      samples: 10,
      budgetMs: 30000,
      warmupIterations: 2,
    });

    const observations2 = await probePerformance({
      roots: ['/tmp'],
      samples: 10,
      budgetMs: 30000,
      warmupIterations: 2,
    });

    // Metrics should appear in the same order
    const metrics1 = observations1.map(obs => obs.metric);
    const metrics2 = observations2.map(obs => obs.metric);

    expect(metrics1).toEqual(metrics2);
  });
});
