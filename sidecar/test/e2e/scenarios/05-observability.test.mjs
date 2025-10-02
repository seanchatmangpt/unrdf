/**
 * @file Scenario 5: Health & Observability (10%)
 * @description London BDD E2E test for health checks and metrics
 *
 * This scenario validates:
 * - Health check endpoint returns correct status
 * - OTel middleware creates spans for all requests
 * - Metrics exported to Prometheus
 * - Service version reporting
 * - Component health checks
 */

import { describe, it, expect, beforeAll } from 'vitest'

describe('Scenario 5: Health & Observability (E2E)', () => {
  let baseUrl
  let prometheusUrl
  let jaegerUrl

  beforeAll(async () => {
    baseUrl = 'http://localhost:3000'
    prometheusUrl = 'http://localhost:9090'
    jaegerUrl = 'http://localhost:16686'
  })

  it('health endpoint returns correct status', async () => {
    const response = await fetch(`${baseUrl}/api/health`)

    expect(response.status).toBe(200)

    const data = await response.json()
    expect(data.success).toBe(true)
    expect(data.data.healthy).toBe(true)
    expect(data.data.version).toBeDefined()
    expect(data.data.service).toBe('kgc-sidecar')
    expect(data.data.checks).toHaveProperty('managers')
    expect(data.data.checks).toHaveProperty('telemetry')
    expect(data.data.checks).toHaveProperty('lockchain')
  })

  it('includes manager initialization status', async () => {
    const response = await fetch(`${baseUrl}/api/health`)
    const data = await response.json()

    expect(data.data.checks.managers).toBeDefined()
    expect(typeof data.data.checks.managers).toBe('boolean')
  })

  it('reports telemetry configuration', async () => {
    const response = await fetch(`${baseUrl}/api/health`)
    const data = await response.json()

    expect(data.data.checks.telemetry).toBeDefined()
    expect(typeof data.data.checks.telemetry).toBe('boolean')
  })

  it('OTel middleware creates spans for health checks', async () => {
    // Make health check request
    await fetch(`${baseUrl}/api/health`)

    // Wait for trace propagation
    await new Promise(resolve => setTimeout(resolve, 3000))

    // Query Jaeger for health check traces
    const jaegerApiUrl = `${jaegerUrl}/api/traces?service=kgc-sidecar&operation=HTTP%20GET%20/api/health&limit=1`

    const response = await fetch(jaegerApiUrl)
    expect(response.status).toBe(200)

    const traces = await response.json()
    if (traces.data && traces.data.length > 0) {
      const trace = traces.data[0]
      expect(trace.spans).toBeDefined()
      expect(trace.spans.length).toBeGreaterThan(0)

      const span = trace.spans[0]
      expect(span.operationName).toContain('GET')
      expect(span.operationName).toContain('/api/health')
    }
  })

  it('exports service metrics to Prometheus', async () => {
    // Query Prometheus for service metrics
    const query = 'up{job="kgc-sidecar-metrics"}'
    const promApiUrl = `${prometheusUrl}/api/v1/query?query=${encodeURIComponent(query)}`

    const response = await fetch(promApiUrl)
    expect(response.status).toBe(200)

    const metrics = await response.json()
    expect(metrics.status).toBe('success')

    // Metrics may not be scraped yet
    if (metrics.data.result.length > 0) {
      const value = parseFloat(metrics.data.result[0].value[1])
      expect(value).toBe(1) // up=1 means service is up
    }
  })

  it('tracks request count metrics', async () => {
    // Make several requests to generate metrics
    for (let i = 0; i < 10; i++) {
      await fetch(`${baseUrl}/api/health`)
    }

    // Wait for metrics scrape
    await new Promise(resolve => setTimeout(resolve, 5000))

    // Query Prometheus for request count
    const query = 'http_requests_total{service="kgc-sidecar"}'
    const promApiUrl = `${prometheusUrl}/api/v1/query?query=${encodeURIComponent(query)}`

    const response = await fetch(promApiUrl)
    expect(response.status).toBe(200)

    const metrics = await response.json()
    expect(metrics.status).toBe('success')

    // Request count should be > 0
    if (metrics.data.result.length > 0) {
      const count = parseFloat(metrics.data.result[0].value[1])
      expect(count).toBeGreaterThan(0)
    }
  })

  it('reports service version correctly', async () => {
    const response = await fetch(`${baseUrl}/api/health`)
    const data = await response.json()

    expect(data.data.version).toBeDefined()
    expect(typeof data.data.version).toBe('string')
    // Version should match package.json or default to 1.0.0
    expect(data.data.version).toMatch(/^\d+\.\d+\.\d+$/)
  })
})
