/**
 * @file Federation Metrics - OTEL instrumentation for federation coordinator
 * @module federation/metrics
 *
 * @description
 * Provides OpenTelemetry metrics tracking for federated query operations,
 * peer health, and system performance. Also maintains an in-process store
 * for Prometheus text and JSON export (used by tests and diagnostics).
 */

import { metrics } from '@opentelemetry/api';

const meter = metrics.getMeter('@unrdf/federation');

/* ========================================================================= */
/* In-process metrics store (for getMetrics / getMetricsJSON)                */
/* ========================================================================= */

/**
 * Internal store keyed by metric name, then by serialized label set.
 * Each entry: { value, labels, timestamp }
 */
let store = new Map();

/**
 * Merge a value into the store for a given metric + label combination.
 * @param {string} name
 * @param {Object} labels
 * @param {number} value
 * @param {'set'|'add'|'observe'} mode
 */
function storeSet(name, labels, value, mode = 'set') {
  const key = `${name}|${JSON.stringify(labels)}`;
  const existing = store.get(key);
  if (mode === 'add') {
    store.set(key, { name, labels, value: (existing?.value ?? 0) + value });
  } else if (mode === 'observe') {
    // Histogram buckets
    store.set(key, { name, labels, value });
  } else {
    store.set(key, { name, labels, value });
  }
}

/* ========================================================================= */
/* OTEL instruments                                                          */
/* ========================================================================= */

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

/* ========================================================================= */
/* Histogram bucket helpers                                                  */
/* ========================================================================= */

const HISTOGRAM_BUCKETS = [0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10];

function recordHistogramBucket(name, labels, valueSeconds) {
  for (const b of HISTOGRAM_BUCKETS) {
    const bucketLabels = { ...labels, le: String(b) };
    storeSet(name, bucketLabels, valueSeconds <= b ? 1 : 0, 'set');
  }
  // +Inf bucket
  storeSet(name, { ...labels, le: '+Inf' }, 1, 'set');
  // _sum and _count
  storeSet(name + '_sum', labels, valueSeconds, 'add');
  storeSet(name + '_count', labels, 1, 'add');
}

/* ========================================================================= */
/* Public API — same signatures as before, now also populates in-process store */
/* ========================================================================= */

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

  const labels = { peer_id: peerId, strategy, status: 'success' };
  metricsStore.queryCounter.add(1, labels);
  storeSet('unrdf_federation_queries_total', labels, 1, 'add');

  const durationSeconds = duration / 1000;
  const histLabels = { peer_id: peerId, strategy };
  metricsStore.queryDuration.record(duration, histLabels);
  recordHistogramBucket('unrdf_federation_query_duration_seconds', histLabels, durationSeconds);
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

  const labels = { peer_id: peerId, error_type: errorType };
  metricsStore.errorCounter.add(1, labels);
  storeSet('unrdf_federation_errors_total', labels, 1, 'add');

  // Also increment query counter with error status
  const queryLabels = { peer_id: peerId, status: 'error' };
  storeSet('unrdf_federation_queries_total', queryLabels, 1, 'add');
}

/**
 * Update peer-level metrics
 *
 * @param {Object} stats - Federation statistics
 * @param {number} stats.healthyPeers - Number of healthy peers
 * @param {number} stats.degradedPeers - Number of degraded peers
 * @param {number} stats.unreachablePeers - Number of unreachable peers
 * @returns {void}
 */
export function updatePeerMetrics(stats) {
  initializeMetrics();

  metricsStore.peerMetricsGauge.add(stats.healthyPeers || 0, { metric: 'healthy_peers' });
  metricsStore.peerMetricsGauge.add(stats.degradedPeers || 0, { metric: 'degraded_peers' });
  metricsStore.peerMetricsGauge.add(stats.unreachablePeers || 0, { metric: 'unreachable_peers' });
  metricsStore.peerMetricsGauge.add(stats.totalQueries || 0, { metric: 'total_queries' });
  metricsStore.peerMetricsGauge.add(stats.totalErrors || 0, { metric: 'total_errors' });

  // In-process store uses gauge semantics (set, not add)
  storeSet('unrdf_federation_peers', { status: 'healthy' }, stats.healthyPeers || 0, 'set');
  storeSet('unrdf_federation_peers', { status: 'degraded' }, stats.degradedPeers || 0, 'set');
  storeSet('unrdf_federation_peers', { status: 'unreachable' }, stats.unreachablePeers || 0, 'set');
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
  storeSet('unrdf_federation_concurrent_queries', {}, concurrentQueries, 'set');

  // Return cleanup function
  return () => {
    concurrentQueries--;
    metricsStore.concurrentQueriesGauge.add(-1);
    storeSet('unrdf_federation_concurrent_queries', {}, concurrentQueries, 'set');
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
  store = new Map();
}

/* ========================================================================= */
/* Metrics export — Prometheus text format                                    */
/* ========================================================================= */

const METRIC_HELP = {
  unrdf_federation_queries_total: 'Total number of federated queries executed',
  unrdf_federation_errors_total: 'Total number of federation errors',
  unrdf_federation_query_duration_seconds: 'Federated query execution duration in seconds',
  unrdf_federation_peers: 'Number of federation peers by health status',
  unrdf_federation_concurrent_queries: 'Number of concurrent queries being executed',
};

const METRIC_TYPE = {
  unrdf_federation_queries_total: 'counter',
  unrdf_federation_errors_total: 'counter',
  unrdf_federation_query_duration_seconds: 'histogram',
  unrdf_federation_peers: 'gauge',
  unrdf_federation_concurrent_queries: 'gauge',
};

/**
 * Export all metrics in Prometheus exposition format.
 *
 * @returns {Promise<string>}
 */
export async function getMetrics() {
  const lines = [];
  const seen = new Set();

  // Always emit HELP/TYPE for all known metrics (even if no data yet)
  for (const name of Object.keys(METRIC_TYPE)) {
    const help = METRIC_HELP[name];
    const type = METRIC_TYPE[name];
    if (help) lines.push(`# HELP ${name} ${help}`);
    if (type) lines.push(`# TYPE ${name} ${type}`);
    seen.add(name);
  }

  for (const [, entry] of store) {
    const { name, labels, value } = entry;

    const labelStr = Object.entries(labels)
      .map(([k, v]) => `${k}="${v}"`)
      .join(',');
    lines.push(`${name}{${labelStr}} ${value}`);
  }

  return lines.join('\n') + '\n';
}

/* ========================================================================= */
/* Metrics export — JSON format                                               */
/* ========================================================================= */

/**
 * Export all metrics as a JSON-compatible array.
 * Each element: { name, type, values: [{ labels, value }] }
 *
 * @returns {Promise<Array>}
 */
export async function getMetricsJSON() {
  const grouped = new Map();

  for (const [, entry] of store) {
    const { name, labels, value } = entry;
    if (!grouped.has(name)) {
      grouped.set(name, {
        name,
        type: METRIC_TYPE[name] || 'untyped',
        values: [],
      });
    }
    grouped.get(name).values.push({ labels, value });
  }

  return [...grouped.values()];
}
