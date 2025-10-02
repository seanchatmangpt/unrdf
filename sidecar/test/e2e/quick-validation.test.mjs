/**
 * @file Quick E2E Validation Test
 * @description Validates existing Docker infrastructure is working
 */

import { describe, it, expect } from 'vitest'

describe('Quick E2E Infrastructure Validation', () => {
  const baseUrl = 'http://localhost:3000'
  const jaegerUrl = 'http://localhost:16686'
  const prometheusUrl = 'http://localhost:9090'

  it('should connect to KGC Sidecar health endpoint', async () => {
    const response = await fetch(`${baseUrl}/api/health`, {
      signal: AbortSignal.timeout(5000)
    })

    expect(response.status).toBe(200)
    const data = await response.json()
    console.log('[Validation] KGC Sidecar health:', data)
  })

  it('should connect to Jaeger UI', async () => {
    const response = await fetch(jaegerUrl, {
      signal: AbortSignal.timeout(5000)
    })

    expect(response.status).toBe(200)
    console.log('[Validation] Jaeger UI is accessible')
  })

  it('should connect to Prometheus', async () => {
    const response = await fetch(`${prometheusUrl}/-/healthy`, {
      signal: AbortSignal.timeout(5000)
    })

    expect(response.status).toBe(200)
    console.log('[Validation] Prometheus is healthy')
  })

  it('should apply a simple transaction', async () => {
    const txRequest = {
      delta: [{
        subject: { termType: 'NamedNode', value: `http://example.org/test-${Date.now()}` },
        predicate: { termType: 'NamedNode', value: 'http://example.org/validation' },
        object: { termType: 'Literal', value: 'quick-test', datatype: { value: 'http://www.w3.org/2001/XMLSchema#string' } }
      }],
      author: 'validation-test'
    }

    const response = await fetch(`${baseUrl}/api/transaction/apply`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(txRequest),
      signal: AbortSignal.timeout(10000)
    })

    expect(response.status).toBe(201)

    const data = await response.json()
    console.log('[Validation] Transaction result:', data)
    expect(data.success).toBe(true)
    expect(data.data.transactionId).toBeDefined()
  })

  it('should query Prometheus for metrics', async () => {
    const query = 'up'
    const url = `${prometheusUrl}/api/v1/query?query=${encodeURIComponent(query)}`

    const response = await fetch(url, {
      signal: AbortSignal.timeout(5000)
    })

    expect(response.status).toBe(200)

    const metrics = await response.json()
    console.log('[Validation] Prometheus query result:', metrics)
    expect(metrics.status).toBe('success')
  })
})
