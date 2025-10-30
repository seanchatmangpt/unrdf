import { describe, it, expect } from 'vitest';
import { createObservabilityManager } from '../src/knowledge-engine/observability.mjs';

describe('Observability - smoothing and minSamples', () => {
  it('applies EWMA fallback when samples are below minSamples', async () => {
    const obs = createObservabilityManager({ minSamples: 10, ewmaAlpha: 0.5, enableMetrics: false, enableTracing: false });

    // Prime last metrics by calling once with some fake latency entries
    obs.metrics.transactionLatency = [
      { timestamp: Date.now() - 1000, duration: 100, success: true },
      { timestamp: Date.now() - 900, duration: 120, success: true },
      { timestamp: Date.now() - 800, duration: 140, success: true },
      { timestamp: Date.now() - 700, duration: 160, success: true },
      { timestamp: Date.now() - 600, duration: 180, success: true },
      { timestamp: Date.now() - 500, duration: 200, success: true },
      { timestamp: Date.now() - 400, duration: 220, success: true },
      { timestamp: Date.now() - 300, duration: 240, success: true },
      { timestamp: Date.now() - 200, duration: 260, success: true },
      { timestamp: Date.now() - 100, duration: 280, success: true },
    ];
    const first = obs.getPerformanceMetrics();

    // Now very few samples; smoothing should use previous
    obs.metrics.transactionLatency = [
      { timestamp: Date.now() - 50, duration: 1000, success: true }
    ];

    const second = obs.getPerformanceMetrics();
    expect(second.transactionLatency.p50).toBeGreaterThan(first.transactionLatency.p50);
    expect(second.transactionLatency.p50).toBeLessThan(1000); // smoothed, not raw spike
  });
});


