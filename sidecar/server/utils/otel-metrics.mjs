/**
 * OpenTelemetry Metrics for Rate Limiting & DDoS
 *
 * Comprehensive metrics collection for:
 * - Rate limit enforcement
 * - DDoS detection and mitigation
 * - Query cost analysis
 * - Backpressure management
 *
 * @module sidecar/utils/otel-metrics
 */

import { metrics as otelMetrics } from '@opentelemetry/api';

const meter = otelMetrics.getMeter('sidecar-rate-limit', '1.0.0');

/**
 * Rate Limiting Metrics
 */
export const metrics = {
  // Rate limit counters
  rateLimitCounter: meter.createCounter('rate_limit.requests', {
    description: 'Total rate limit checks',
    unit: '1',
  }),

  // DDoS detection
  ddosThreatGauge: meter.createGauge('ddos.threat_score', {
    description: 'Current DDoS threat score (0-1)',
    unit: '1',
  }),

  ddosBlacklistCounter: meter.createCounter('ddos.blacklist.additions', {
    description: 'IPs added to blacklist',
    unit: '1',
  }),

  ddosBlockedCounter: meter.createCounter('ddos.requests.blocked', {
    description: 'Requests blocked due to blacklist',
    unit: '1',
  }),

  // Query cost
  queryPerformanceHistogram: meter.createHistogram('query.cost', {
    description: 'SPARQL query cost estimation',
    unit: '1',
  }),

  queryRejectedCounter: meter.createCounter('query.rejected', {
    description: 'Queries rejected due to high cost',
    unit: '1',
  }),

  // Backpressure
  backpressureQueueDepthGauge: meter.createGauge('backpressure.queue_depth', {
    description: 'Current queue depth',
    unit: '1',
  }),

  backpressureSystemLoadGauge: meter.createGauge('backpressure.system_load', {
    description: 'Current system load (0-1)',
    unit: '1',
  }),

  backpressureRejectedCounter: meter.createCounter('backpressure.requests.rejected', {
    description: 'Requests rejected due to backpressure',
    unit: '1',
  }),
};

export default metrics;
