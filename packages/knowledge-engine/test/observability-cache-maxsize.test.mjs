/**
 * @file Regression: getPerformanceMetrics() must not throw when config.cacheMaxSize is unset.
 * It used to call an undefined getRuntimeConfig(), so any manager without a numeric
 * cacheMaxSize threw a ReferenceError from the metrics path.
 * @module test/observability-cache-maxsize
 */

import { describe, it, expect, afterEach } from 'vitest';
import { ObservabilityManager } from '../src/observability.mjs';

describe('ObservabilityManager cacheStats.maxSize', () => {
  let manager;

  afterEach(async () => {
    await manager?.shutdown();
  });

  it('reports the current cache size when cacheMaxSize is not configured', () => {
    manager = new ObservabilityManager({ serviceName: 'test-service', enableTracing: false });

    const metrics = manager.getPerformanceMetrics();

    expect(metrics.cacheStats.maxSize).toBe(metrics.cacheStats.size);
  });

  it('reports the configured cacheMaxSize when one is set', () => {
    manager = new ObservabilityManager({
      serviceName: 'test-service',
      enableTracing: false,
      cacheMaxSize: 128,
    });

    expect(manager.getPerformanceMetrics().cacheStats.maxSize).toBe(128);
  });
});
