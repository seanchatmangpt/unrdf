/**
 * @file Monitoring Utilities - High-level monitoring for UNRDF using pm4py infrastructure
 * @module @unrdf/otel/monitoring
 *
 * This module provides monitoring utilities that integrate UNRDF's semantic
 * conventions with pm4py's comprehensive observability stack.
 */

import { getPM4Py, recordUNRDFMetrics, checkPM4PyAvailable } from './pm4py.mjs';

/**
 * UNRDF operation types for monitoring
 */
export const UNRDFOperation = {
  SPARQL_QUERY: 'sparql_query',
  GRAPH_LOAD: 'graph_load',
  GRAPH_SAVE: 'graph_save',
  HOOK_EXECUTE: 'hook_execute',
  FEDERATION_QUERY: 'federation_query',
  STREAMING_ADMIT: 'streaming_admit',
  VALIDATION_SHACL: 'validation_shacl',
  EMBEDDING_GENERATE: 'embedding_generate',
  HNSW_SEARCH: 'hnsw_search'
};

/**
 * Monitor UNRDF operation with automatic metric recording
 */
export async function monitorOperation(operation, fn, metadata = {}) {
  const startTime = Date.now();

  try {
    // Record operation start
    await recordUNRDFMetrics(operation, {
      ...metadata,
      status: 'started'
    });

    // Execute operation
    const result = await fn();

    // Record success
    const duration = (Date.now() - startTime) / 1000;
    await recordUNRDFMetrics(operation, {
      ...metadata,
      status: 'success',
      duration_seconds: duration
    });

    return result;
  } catch (error) {
    // Record failure
    const duration = (Date.now() - startTime) / 1000;
    await recordUNRDFMetrics(operation, {
      ...metadata,
      status: 'error',
      error: error.message,
      duration_seconds: duration
    });

    throw error;
  }
}

/**
 * Get monitoring dashboard URL
 */
export function getDashboardURL() {
  return process.env.PROMETHEUS_URL || 'http://localhost:3001';
}

/**
 * Get Jaeger tracing URL
 */
export function getTracingURL() {
  return process.env.JAEGER_URL || 'http://localhost:16686';
}

/**
 * Get OTLP endpoint for traces
 */
export function getOTLPEndpoint() {
  return process.env.OTEL_EXPORTER_OTLP_ENDPOINT || 'http://localhost:14317';
}

/**
 * Check monitoring stack health
 */
export async function checkMonitoringHealth() {
  const health = {
    pm4py: false,
    prometheus: false,
    jaeger: false,
    otelCollector: false
  };

  // Check pm4py availability
  health.pm4py = await checkPM4PyAvailable();

  // Check Prometheus (simple HTTP check)
  try {
    const response = await fetch('http://localhost:3001/api/v1/status/config');
    health.prometheus = response.ok;
  } catch {
    health.prometheus = false;
  }

  // Check Jaeger
  try {
    const response = await fetch('http://localhost:16686/api/services');
    health.jaeger = response.ok;
  } catch {
    health.jaeger = false;
  }

  // Check OTEL collector
  try {
    const response = await fetch('http://localhost:13133');
    health.otelCollector = response.ok;
  } catch {
    health.otelCollector = false;
  }

  return health;
}

/**
 * Create monitoring context for UNRDF operations
 */
export function createMonitoringContext(service, operation) {
  return {
    service,
    operation,
    startTime: Date.now(),
    metadata: {}
  };
}

/**
 * Record custom metric in pm4py
 */
export async function recordMetric(name, value, type = 'gauge', labels = {}) {
  const pm4py = getPM4Py();
  const defaultLabels = {
    service: 'unrdf',
    ...labels
  };

  return await pm4py.recordMetric(name, value, type, defaultLabels);
}

/**
 * Get current system metrics from pm4py
 */
export async function getSystemMetrics() {
  const pm4py = getPM4Py();
  return await pm4py.getSystemMetrics();
}

/**
 * Get Prometheus metrics from pm4py
 */
export async function getPrometheusMetrics() {
  const pm4py = getPM4Py();
  return await pm4py.getPrometheusMetrics();
}

/**
 * Initialize UNRDF monitoring stack
 */
export async function initializeMonitoring() {
  const pm4py = getPM4Py();

  // Initialize pm4py metrics
  await pm4py.execute(`
from pm4py.monitoring import MetricCollector

collector = MetricCollector()

# Register core UNRDF metrics
collector.register(collector.Counter('unrdf_graph_loads_total', 'Total graph loads'))
collector.register(collector.Counter('unrdf_sparql_queries_total', 'Total SPARQL queries'))
collector.register(collector.Counter('unrdf_hook_executions_total', 'Total hook executions'))
collector.register(collector.Counter('unrdf_federation_queries_total', 'Total federation queries'))
collector.register(collector.Counter('unrdf_streaming_events_total', 'Total streaming events'))

collector.register(collector.Gauge('unrdf_active_graphs', 'Active RDF graphs'))
collector.register(collector.Gauge('unrdf_cached_embeddings', 'Cached embeddings'))
collector.register(collector.Gauge('unrdf_hnsw_index_size', 'HNSW index size'))

collector.register(collector.Histogram('unrdf_sparql_query_duration_seconds', 'SPARQL query duration'))
collector.register(collector.Histogram('unrdf_hook_execution_duration_seconds', 'Hook execution duration'))
collector.register(collector.Histogram('unrdf_federation_latency_seconds', 'Federation latency'))

print('OK')
`);

  return true;
}

/**
 * Decorator for monitoring UNRDF class methods
 */
export function monitor(operation, metadata = {}) {
  return function(target, propertyKey, descriptor) {
    const originalMethod = descriptor.value;

    descriptor.value = async function(...args) {
      return monitorOperation(operation, () => originalMethod.apply(this, args), {
        class: target.constructor.name,
        method: propertyKey,
        ...metadata
      });
    };

    return descriptor;
  };
}
