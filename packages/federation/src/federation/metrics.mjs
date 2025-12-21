/**
 * @file Prometheus Metrics - Observability metrics for federation
 * @module federation/metrics
 */

import { register, Counter, Histogram, Gauge } from 'prom-client';

/* ========================================================================= */
/* Metrics Definitions                                                      */
/* ========================================================================= */

/**
 * Total number of queries executed across all peers.
 */
export const queryCounter = new Counter({
  name: 'unrdf_federation_queries_total',
  help: 'Total number of queries executed',
  labelNames: ['peer_id', 'status'],
});

/**
 * Query execution duration in seconds.
 */
export const queryDuration = new Histogram({
  name: 'unrdf_federation_query_duration_seconds',
  help: 'Query execution duration in seconds',
  labelNames: ['peer_id', 'strategy'],
  buckets: [0.001, 0.01, 0.05, 0.1, 0.5, 1, 5, 10],
});

/**
 * Number of active peers by status.
 */
export const peerGauge = new Gauge({
  name: 'unrdf_federation_peers',
  help: 'Number of peers by status',
  labelNames: ['status'],
});

/**
 * Number of concurrent queries in flight.
 */
export const concurrentQueries = new Gauge({
  name: 'unrdf_federation_concurrent_queries',
  help: 'Number of concurrent queries in flight',
});

/**
 * Query error counter.
 */
export const errorCounter = new Counter({
  name: 'unrdf_federation_errors_total',
  help: 'Total number of query errors',
  labelNames: ['peer_id', 'error_type'],
});

/* ========================================================================= */
/* Metrics Collection                                                       */
/* ========================================================================= */

/**
 * Get metrics in Prometheus format.
 *
 * @returns {Promise<string>} Prometheus-formatted metrics
 */
export async function getMetrics() {
  return register.metrics();
}

/**
 * Get metrics as JSON.
 *
 * @returns {Promise<Array>} Metrics as JSON array
 */
export async function getMetricsJSON() {
  return register.getMetricsAsJSON();
}

/**
 * Clear all metrics (useful for testing).
 *
 * @returns {void}
 */
export function clearMetrics() {
  register.clear();
}

/**
 * Reset all metrics to zero (useful for testing).
 *
 * @returns {void}
 */
export function resetMetrics() {
  queryCounter.reset();
  queryDuration.reset();
  peerGauge.reset();
  concurrentQueries.reset();
  errorCounter.reset();
}

/* ========================================================================= */
/* Metrics Helpers                                                          */
/* ========================================================================= */

/**
 * Record a successful query.
 *
 * @param {string} peerId - Peer identifier
 * @param {number} duration - Query duration in milliseconds
 * @param {string} [strategy='broadcast'] - Query strategy
 * @returns {void}
 */
export function recordQuery(peerId, duration, strategy = 'broadcast') {
  queryCounter.inc({ peer_id: peerId, status: 'success' });
  queryDuration.observe({ peer_id: peerId, strategy }, duration / 1000);
}

/**
 * Record a failed query.
 *
 * @param {string} peerId - Peer identifier
 * @param {string} errorType - Error type/category
 * @returns {void}
 */
export function recordError(peerId, errorType = 'unknown') {
  queryCounter.inc({ peer_id: peerId, status: 'error' });
  errorCounter.inc({ peer_id: peerId, error_type: errorType });
}

/**
 * Update peer count metrics.
 *
 * @param {Object} stats - Peer statistics
 * @param {number} stats.healthyPeers - Number of healthy peers
 * @param {number} stats.degradedPeers - Number of degraded peers
 * @param {number} stats.unreachablePeers - Number of unreachable peers
 * @returns {void}
 */
export function updatePeerMetrics(stats) {
  peerGauge.set({ status: 'healthy' }, stats.healthyPeers || 0);
  peerGauge.set({ status: 'degraded' }, stats.degradedPeers || 0);
  peerGauge.set({ status: 'unreachable' }, stats.unreachablePeers || 0);
}

/**
 * Track concurrent query execution.
 *
 * @returns {function} Cleanup function to decrement counter
 */
export function trackConcurrentQuery() {
  concurrentQueries.inc();
  return () => concurrentQueries.dec();
}
