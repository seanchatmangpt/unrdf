/**
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  getMetrics,
  getMetricsJSON,
  resetMetrics,
  recordQuery,
  recordError,
  updatePeerMetrics,
  trackConcurrentQuery,
  queryCounter,
  queryDuration,
  peerGauge,
  concurrentQueries,
  errorCounter,
} from '../src/federation/metrics.mjs';

/* ========================================================================= */
/* Setup and Teardown                                                       */
/* ========================================================================= */

beforeEach(() => {
  resetMetrics();
});

afterEach(() => {
  resetMetrics();
});

/* ========================================================================= */
/* Query Counter Tests                                                      */
/* ========================================================================= */

describe('Query Counter', () => {
  it('should increment on successful query', async () => {
    recordQuery('peer1', 100, 'broadcast');

    const metrics = await getMetricsJSON();
    const counter = metrics.find(m => m.name === 'unrdf_federation_queries_total');

    expect(counter).toBeDefined();
    expect(counter.values).toHaveLength(1);
    expect(counter.values[0].labels.peer_id).toBe('peer1');
    expect(counter.values[0].labels.status).toBe('success');
    expect(counter.values[0].value).toBe(1);
  });

  it('should increment multiple times', async () => {
    recordQuery('peer1', 100, 'broadcast');
    recordQuery('peer1', 150, 'broadcast');
    recordQuery('peer1', 200, 'broadcast');

    const metrics = await getMetricsJSON();
    const counter = metrics.find(m => m.name === 'unrdf_federation_queries_total');

    expect(counter.values[0].value).toBe(3);
  });

  it('should track different peers separately', async () => {
    recordQuery('peer1', 100, 'broadcast');
    recordQuery('peer2', 150, 'broadcast');

    const metrics = await getMetricsJSON();
    const counter = metrics.find(m => m.name === 'unrdf_federation_queries_total');

    expect(counter.values).toHaveLength(2);
    expect(counter.values.some(v => v.labels.peer_id === 'peer1')).toBe(true);
    expect(counter.values.some(v => v.labels.peer_id === 'peer2')).toBe(true);
  });

  it('should track different strategies', async () => {
    recordQuery('peer1', 100, 'broadcast');
    recordQuery('peer1', 150, 'selective');

    const metrics = await getMetricsJSON();
    const duration = metrics.find(m => m.name === 'unrdf_federation_query_duration_seconds');

    expect(duration.values.some(v => v.labels.strategy === 'broadcast')).toBe(true);
    expect(duration.values.some(v => v.labels.strategy === 'selective')).toBe(true);
  });
});

/* ========================================================================= */
/* Query Duration Tests                                                     */
/* ========================================================================= */

describe('Query Duration', () => {
  it('should record query duration in seconds', async () => {
    recordQuery('peer1', 1000, 'broadcast'); // 1000ms = 1s

    const metrics = await getMetricsJSON();
    const histogram = metrics.find(m => m.name === 'unrdf_federation_query_duration_seconds');

    expect(histogram).toBeDefined();
    expect(histogram.type).toBe('histogram');
  });

  it('should record multiple durations', async () => {
    recordQuery('peer1', 50, 'broadcast');
    recordQuery('peer1', 500, 'broadcast');
    recordQuery('peer1', 5000, 'broadcast');

    const metrics = await getMetricsJSON();
    const histogram = metrics.find(m => m.name === 'unrdf_federation_query_duration_seconds');

    expect(histogram).toBeDefined();
    expect(histogram.type).toBe('histogram');
    // Histogram values include buckets with _bucket suffix
    expect(histogram.values.length).toBeGreaterThan(0);
  });

  it('should use correct histogram buckets', async () => {
    recordQuery('peer1', 1, 'broadcast'); // 0.001s
    recordQuery('peer1', 100, 'broadcast'); // 0.1s
    recordQuery('peer1', 10000, 'broadcast'); // 10s

    const metricsText = await getMetrics();

    // Verify buckets exist in output
    expect(metricsText).toContain('le="0.001"');
    expect(metricsText).toContain('le="0.1"');
    expect(metricsText).toContain('le="10"');
  });
});

/* ========================================================================= */
/* Error Counter Tests                                                      */
/* ========================================================================= */

describe('Error Counter', () => {
  it('should increment on error', async () => {
    recordError('peer1', 'timeout');

    const metrics = await getMetricsJSON();
    const errors = metrics.find(m => m.name === 'unrdf_federation_errors_total');

    expect(errors).toBeDefined();
    expect(errors.values).toHaveLength(1);
    expect(errors.values[0].labels.peer_id).toBe('peer1');
    expect(errors.values[0].labels.error_type).toBe('timeout');
    expect(errors.values[0].value).toBe(1);
  });

  it('should track different error types', async () => {
    recordError('peer1', 'timeout');
    recordError('peer1', 'network_error');
    recordError('peer1', 'query_failed');

    const metrics = await getMetricsJSON();
    const errors = metrics.find(m => m.name === 'unrdf_federation_errors_total');

    expect(errors.values).toHaveLength(3);
    expect(errors.values.some(v => v.labels.error_type === 'timeout')).toBe(true);
    expect(errors.values.some(v => v.labels.error_type === 'network_error')).toBe(true);
    expect(errors.values.some(v => v.labels.error_type === 'query_failed')).toBe(true);
  });

  it('should increment query counter on error', async () => {
    recordError('peer1', 'timeout');

    const metrics = await getMetricsJSON();
    const counter = metrics.find(m => m.name === 'unrdf_federation_queries_total');

    const errorEntry = counter.values.find(
      v => v.labels.peer_id === 'peer1' && v.labels.status === 'error'
    );

    expect(errorEntry).toBeDefined();
    expect(errorEntry.value).toBe(1);
  });
});

/* ========================================================================= */
/* Peer Gauge Tests                                                         */
/* ========================================================================= */

describe('Peer Gauge', () => {
  it('should update peer counts', async () => {
    updatePeerMetrics({
      healthyPeers: 2,
      degradedPeers: 1,
      unreachablePeers: 1,
    });

    const metrics = await getMetricsJSON();
    const gauge = metrics.find(m => m.name === 'unrdf_federation_peers');

    expect(gauge).toBeDefined();
    expect(gauge.values).toHaveLength(3);

    const healthy = gauge.values.find(v => v.labels.status === 'healthy');
    const degraded = gauge.values.find(v => v.labels.status === 'degraded');
    const unreachable = gauge.values.find(v => v.labels.status === 'unreachable');

    expect(healthy.value).toBe(2);
    expect(degraded.value).toBe(1);
    expect(unreachable.value).toBe(1);
  });

  it('should handle zero peers', async () => {
    updatePeerMetrics({
      healthyPeers: 0,
      degradedPeers: 0,
      unreachablePeers: 0,
    });

    const metrics = await getMetricsJSON();
    const gauge = metrics.find(m => m.name === 'unrdf_federation_peers');

    expect(gauge.values.every(v => v.value === 0)).toBe(true);
  });

  it('should update dynamically', async () => {
    updatePeerMetrics({ healthyPeers: 1, degradedPeers: 0, unreachablePeers: 0 });

    let metrics = await getMetricsJSON();
    let gauge = metrics.find(m => m.name === 'unrdf_federation_peers');
    let healthy = gauge.values.find(v => v.labels.status === 'healthy');
    expect(healthy.value).toBe(1);

    updatePeerMetrics({ healthyPeers: 3, degradedPeers: 1, unreachablePeers: 0 });

    metrics = await getMetricsJSON();
    gauge = metrics.find(m => m.name === 'unrdf_federation_peers');
    healthy = gauge.values.find(v => v.labels.status === 'healthy');
    const degraded = gauge.values.find(v => v.labels.status === 'degraded');

    expect(healthy.value).toBe(3);
    expect(degraded.value).toBe(1);
  });
});

/* ========================================================================= */
/* Concurrent Queries Gauge Tests                                           */
/* ========================================================================= */

describe('Concurrent Queries Gauge', () => {
  it('should track concurrent queries', async () => {
    const end1 = trackConcurrentQuery();

    let metrics = await getMetricsJSON();
    let gauge = metrics.find(m => m.name === 'unrdf_federation_concurrent_queries');
    expect(gauge.values[0].value).toBe(1);

    const end2 = trackConcurrentQuery();

    metrics = await getMetricsJSON();
    gauge = metrics.find(m => m.name === 'unrdf_federation_concurrent_queries');
    expect(gauge.values[0].value).toBe(2);

    end1();

    metrics = await getMetricsJSON();
    gauge = metrics.find(m => m.name === 'unrdf_federation_concurrent_queries');
    expect(gauge.values[0].value).toBe(1);

    end2();

    metrics = await getMetricsJSON();
    gauge = metrics.find(m => m.name === 'unrdf_federation_concurrent_queries');
    expect(gauge.values[0].value).toBe(0);
  });

  it('should handle multiple concurrent queries', async () => {
    const ends = [];

    for (let i = 0; i < 10; i++) {
      ends.push(trackConcurrentQuery());
    }

    const metrics = await getMetricsJSON();
    const gauge = metrics.find(m => m.name === 'unrdf_federation_concurrent_queries');
    expect(gauge.values[0].value).toBe(10);

    // Clean up
    ends.forEach(end => end());

    const metricsAfter = await getMetricsJSON();
    const gaugeAfter = metricsAfter.find(m => m.name === 'unrdf_federation_concurrent_queries');
    expect(gaugeAfter.values[0].value).toBe(0);
  });
});

/* ========================================================================= */
/* Metrics Export Tests                                                     */
/* ========================================================================= */

describe('Metrics Export', () => {
  it('should export metrics in Prometheus format', async () => {
    recordQuery('peer1', 100, 'broadcast');
    recordError('peer2', 'timeout');

    const metricsText = await getMetrics();

    expect(typeof metricsText).toBe('string');
    expect(metricsText).toContain('unrdf_federation_queries_total');
    expect(metricsText).toContain('unrdf_federation_errors_total');
    expect(metricsText).toContain('peer_id="peer1"');
    expect(metricsText).toContain('peer_id="peer2"');
  });

  it('should export metrics as JSON', async () => {
    recordQuery('peer1', 100, 'broadcast');

    const metricsJSON = await getMetricsJSON();

    expect(Array.isArray(metricsJSON)).toBe(true);
    expect(metricsJSON.length).toBeGreaterThan(0);
    expect(metricsJSON.every(m => m.name && m.type)).toBe(true);
  });

  it('should include help text in Prometheus format', async () => {
    const metricsText = await getMetrics();

    expect(metricsText).toContain('# HELP unrdf_federation_queries_total');
    expect(metricsText).toContain('# TYPE unrdf_federation_queries_total counter');
  });
});

/* ========================================================================= */
/* Reset Metrics Tests                                                      */
/* ========================================================================= */

describe('Reset Metrics', () => {
  it('should reset all metrics to zero', async () => {
    recordQuery('peer1', 100, 'broadcast');
    recordError('peer1', 'timeout');
    updatePeerMetrics({ healthyPeers: 5, degradedPeers: 2, unreachablePeers: 1 });

    resetMetrics();

    const metrics = await getMetricsJSON();

    // After reset, only default metrics should exist with zero values
    const queries = metrics.find(m => m.name === 'unrdf_federation_queries_total');
    expect(!queries || queries.values.length === 0).toBe(true);
  });

  it('should allow metrics to be recorded after reset', async () => {
    recordQuery('peer1', 100, 'broadcast');
    resetMetrics();
    recordQuery('peer2', 200, 'selective');

    const metrics = await getMetricsJSON();
    const counter = metrics.find(m => m.name === 'unrdf_federation_queries_total');

    expect(counter.values).toHaveLength(1);
    expect(counter.values[0].labels.peer_id).toBe('peer2');
  });
});
