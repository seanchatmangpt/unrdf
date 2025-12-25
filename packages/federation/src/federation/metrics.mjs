/**
 * @file Federation Metrics - OTEL instrumentation for federation coordinator
 * @module federation/metrics
 *
 * @description
 * Provides OpenTelemetry metrics tracking for federated query operations,
 * peer health, and system performance.
 */

import { metrics } from '@opentelemetry/api';

const meter = metrics.getMeter('@unrdf/federation');

/**
 * Federation metrics counters and histograms
 * @type {Object}
 */
const metricsStore = {
  queryCounter: null,
  errorCounter: null,
  queryDuration: null,
  peerMetricsGauge: null,
  concurrentQueriesGauge: null,
  initialized: false,
};

/**
 * Current concurrent query count
 * @type {number}
 */
let concurrentQueries = 0;

/**
 * Initialize federation metrics
 * Creates all OTEL metric instruments
 *
 * @returns {void}
 */
function initializeMetrics() {
  if (metricsStore.initialized) return;

  metricsStore.queryCounter = meter.createCounter('federation.queries.total', {
    description: 'Total number of federated queries executed',
  });

  metricsStore.errorCounter = meter.createCounter('federation.errors.total', {
    description: 'Total number of federation errors',
  });

  metricsStore.queryDuration = meter.createHistogram('federation.query.duration', {
    description: 'Federated query execution duration in milliseconds',
    unit: 'ms',
  });

  metricsStore.peerMetricsGauge = meter.createUpDownCounter('federation.peer.metrics', {
    description: 'Peer-level metrics (queries, errors, health)',
  });

  metricsStore.concurrentQueriesGauge = meter.createUpDownCounter('federation.queries.concurrent', {
    description: 'Number of concurrent queries being executed',
  });

  metricsStore.initialized = true;
}

/**
 * Record a successful query execution
 *
 * @param {string} peerId - Peer identifier
 * @param {number} duration - Query duration in milliseconds
 * @param {string} strategy - Query routing strategy
 * @returns {void}
 */
export function recordQuery(peerId, duration, strategy) {
  initializeMetrics();

  metricsStore.queryCounter.add(1, {
    peer_id: peerId,
    strategy,
    status: 'success',
  });

  metricsStore.queryDuration.record(duration, {
    peer_id: peerId,
    strategy,
  });
}

/**
 * Record a query or execution error
 *
 * @param {string} peerId - Peer identifier
 * @param {string} errorType - Type of error (query_failed, peer_not_found, execution_error, etc.)
 * @returns {void}
 */
export function recordError(peerId, errorType) {
  initializeMetrics();

  metricsStore.errorCounter.add(1, {
    peer_id: peerId,
    error_type: errorType,
  });
}

/**
 * Update peer-level metrics
 *
 * @param {Object} stats - Federation statistics
 * @param {number} stats.totalPeers - Total number of peers
 * @param {number} stats.healthyPeers - Number of healthy peers
 * @param {number} stats.degradedPeers - Number of degraded peers
 * @param {number} stats.unreachablePeers - Number of unreachable peers
 * @param {number} stats.totalQueries - Total queries executed
 * @param {number} stats.totalErrors - Total errors encountered
 * @param {number} stats.errorRate - Current error rate (0-1)
 * @returns {void}
 */
export function updatePeerMetrics(stats) {
  initializeMetrics();

  metricsStore.peerMetricsGauge.add(stats.healthyPeers || 0, {
    metric: 'healthy_peers',
  });

  metricsStore.peerMetricsGauge.add(stats.degradedPeers || 0, {
    metric: 'degraded_peers',
  });

  metricsStore.peerMetricsGauge.add(stats.unreachablePeers || 0, {
    metric: 'unreachable_peers',
  });

  metricsStore.peerMetricsGauge.add(stats.totalQueries || 0, {
    metric: 'total_queries',
  });

  metricsStore.peerMetricsGauge.add(stats.totalErrors || 0, {
    metric: 'total_errors',
  });
}

/**
 * Track concurrent query execution
 * Returns a function to call when query completes
 *
 * @returns {Function} End function to decrement concurrent counter
 */
export function trackConcurrentQuery() {
  initializeMetrics();

  concurrentQueries++;
  metricsStore.concurrentQueriesGauge.add(1);

  // Return cleanup function
  return () => {
    concurrentQueries--;
    metricsStore.concurrentQueriesGauge.add(-1);
  };
}

/**
 * Get current metrics state
 *
 * @returns {Object} Current metrics
 */
export function getMetricsState() {
  return {
    concurrentQueries,
    initialized: metricsStore.initialized,
  };
}

/**
 * Reset all metrics (primarily for testing)
 *
 * @returns {void}
 */
export function resetMetrics() {
  concurrentQueries = 0;
  metricsStore.initialized = false;
}
