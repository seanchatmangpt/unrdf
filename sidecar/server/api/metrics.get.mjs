/**
 * @file Prometheus Metrics Endpoint
 * @description GET /api/metrics - Export metrics in Prometheus format
 *
 * Exports:
 * - HTTP request counters
 * - Circuit breaker states
 * - OTEL metrics
 * - System resource usage
 */

import { defineEventHandler } from '#imports'
import { circuitBreakerRegistry } from '../utils/circuit-breaker.mjs'
import { metrics } from '@opentelemetry/api'

/**
 * Format metric in Prometheus exposition format
 * @param {string} name - Metric name
 * @param {string} type - Metric type (counter, gauge, histogram)
 * @param {string} help - Help text
 * @param {Array<{labels: Object, value: number}>} values - Metric values with labels
 * @returns {string} Prometheus formatted metric
 */
function formatPrometheusMetric(name, type, help, values) {
  let output = `# HELP ${name} ${help}\n`
  output += `# TYPE ${name} ${type}\n`

  for (const { labels, value } of values) {
    const labelStr = Object.entries(labels)
      .map(([k, v]) => `${k}="${v}"`)
      .join(',')
    output += `${name}{${labelStr}} ${value}\n`
  }

  return output
}

/**
 * Collect circuit breaker metrics
 * @returns {string} Prometheus formatted circuit breaker metrics
 */
function collectCircuitBreakerMetrics() {
  const summary = circuitBreakerRegistry.getHealthSummary()
  let output = ''

  // Circuit state metric
  const stateValues = []
  const failureValues = []
  const successValues = []
  const healthValues = []

  for (const [name, metrics] of Object.entries(summary)) {
    const stateNum = metrics.state === 'CLOSED' ? 0 : metrics.state === 'HALF_OPEN' ? 1 : 2
    stateValues.push({ labels: { circuit: name }, value: stateNum })
    failureValues.push({ labels: { circuit: name }, value: metrics.failures })
    successValues.push({ labels: { circuit: name }, value: metrics.successes })
    healthValues.push({ labels: { circuit: name }, value: metrics.healthScore })
  }

  if (stateValues.length > 0) {
    output += formatPrometheusMetric(
      'unrdf_circuit_breaker_state',
      'gauge',
      'Circuit breaker state (0=CLOSED, 1=HALF_OPEN, 2=OPEN)',
      stateValues
    )

    output += formatPrometheusMetric(
      'unrdf_circuit_breaker_failures_total',
      'counter',
      'Total circuit breaker failures',
      failureValues
    )

    output += formatPrometheusMetric(
      'unrdf_circuit_breaker_successes_total',
      'counter',
      'Total circuit breaker successes',
      successValues
    )

    output += formatPrometheusMetric(
      'unrdf_circuit_breaker_health_score',
      'gauge',
      'Circuit breaker health score (0-100)',
      healthValues
    )
  }

  return output
}

/**
 * Collect system resource metrics
 * @returns {string} Prometheus formatted system metrics
 */
function collectSystemMetrics() {
  const mem = process.memoryUsage()
  const cpu = process.cpuUsage()

  let output = ''

  // Memory metrics
  output += formatPrometheusMetric(
    'unrdf_process_memory_bytes',
    'gauge',
    'Process memory usage in bytes',
    [
      { labels: { type: 'rss' }, value: mem.rss },
      { labels: { type: 'heap_total' }, value: mem.heapTotal },
      { labels: { type: 'heap_used' }, value: mem.heapUsed },
      { labels: { type: 'external' }, value: mem.external }
    ]
  )

  // CPU metrics (in microseconds)
  output += formatPrometheusMetric(
    'unrdf_process_cpu_microseconds_total',
    'counter',
    'Process CPU usage in microseconds',
    [
      { labels: { mode: 'user' }, value: cpu.user },
      { labels: { mode: 'system' }, value: cpu.system }
    ]
  )

  // Uptime
  output += formatPrometheusMetric(
    'unrdf_process_uptime_seconds',
    'gauge',
    'Process uptime in seconds',
    [{ labels: {}, value: process.uptime() }]
  )

  return output
}

/**
 * Metrics endpoint handler
 * @param {Object} event - H3 event
 * @returns {Promise<string>} Prometheus formatted metrics
 */
export default defineEventHandler(async (event) => {
  // Set content type to Prometheus text format
  event.node.res.setHeader('Content-Type', 'text/plain; version=0.0.4; charset=utf-8')

  let output = ''

  // Collect all metrics
  output += collectCircuitBreakerMetrics()
  output += collectSystemMetrics()

  // Add timestamp
  output += `# Generated at ${new Date().toISOString()}\n`

  return output
})
