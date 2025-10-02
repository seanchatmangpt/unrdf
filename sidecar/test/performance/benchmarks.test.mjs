/**
 * @file Performance Benchmarks
 * @description E2E performance validation with real infrastructure
 *
 * Performance SLOs:
 * - Transaction throughput: >1000 tx/sec
 * - Latency p99: <500ms (HTTP round-trip)
 * - Latency p99: <2ms (transaction core)
 * - Memory usage: <512MB under load
 * - Concurrent connections: >100 simultaneous
 */

import { describe, it, expect, beforeAll } from 'vitest'

describe('Performance Benchmarks (E2E)', () => {
  let baseUrl

  beforeAll(() => {
    // Infrastructure already started by global setup
    baseUrl = 'http://localhost:3000'
  })

  it('should achieve >1000 transactions/second throughput', async () => {
    const duration = 10000 // 10 seconds
    const targetTPS = 1000
    const startTime = Date.now()
    let completedTxs = 0
    let errors = 0

    console.log('[Benchmark] Starting throughput test...')

    // Fire transactions as fast as possible for 10 seconds
    const workers = Array.from({ length: 10 }, async (_, workerId) => {
      while (Date.now() - startTime < duration) {
        try {
          const response = await fetch(`${baseUrl}/api/transaction/apply`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
              delta: [{
                subject: { termType: 'NamedNode', value: `http://example.org/perf-${workerId}-${Date.now()}` },
                predicate: { termType: 'NamedNode', value: 'http://example.org/test' },
                object: { termType: 'Literal', value: 'benchmark', datatype: { value: 'http://www.w3.org/2001/XMLSchema#string' } }
              }],
              author: `worker-${workerId}`
            })
          })

          if (response.ok) {
            completedTxs++
          } else {
            errors++
          }
        } catch (error) {
          errors++
        }
      }
    })

    await Promise.all(workers)

    const actualDuration = Date.now() - startTime
    const tps = (completedTxs / actualDuration) * 1000

    console.log(`[Benchmark] Throughput: ${tps.toFixed(2)} tx/sec`)
    console.log(`[Benchmark] Total transactions: ${completedTxs}`)
    console.log(`[Benchmark] Errors: ${errors}`)
    console.log(`[Benchmark] Error rate: ${((errors / (completedTxs + errors)) * 100).toFixed(2)}%`)

    expect(tps).toBeGreaterThan(targetTPS)
    expect(errors / (completedTxs + errors)).toBeLessThan(0.01) // <1% error rate
  }, 30000)

  it('should achieve p99 latency <500ms for HTTP round-trip', async () => {
    const samples = 1000
    const latencies = []

    console.log(`[Benchmark] Collecting ${samples} latency samples...`)

    for (let i = 0; i < samples; i++) {
      const start = performance.now()

      const response = await fetch(`${baseUrl}/api/transaction/apply`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          delta: [{
            subject: { termType: 'NamedNode', value: `http://example.org/latency-${i}` },
            predicate: { termType: 'NamedNode', value: 'http://example.org/test' },
            object: { termType: 'Literal', value: `latency-${i}`, datatype: { value: 'http://www.w3.org/2001/XMLSchema#string' } }
          }],
          author: 'latency-test'
        })
      })

      const end = performance.now()
      const latency = end - start

      if (response.ok) {
        latencies.push(latency)
      }
    }

    // Calculate percentiles
    latencies.sort((a, b) => a - b)
    const p50 = latencies[Math.floor(latencies.length * 0.50)]
    const p95 = latencies[Math.floor(latencies.length * 0.95)]
    const p99 = latencies[Math.floor(latencies.length * 0.99)]
    const avg = latencies.reduce((a, b) => a + b, 0) / latencies.length

    console.log(`[Benchmark] Latency p50: ${p50.toFixed(2)}ms`)
    console.log(`[Benchmark] Latency p95: ${p95.toFixed(2)}ms`)
    console.log(`[Benchmark] Latency p99: ${p99.toFixed(2)}ms`)
    console.log(`[Benchmark] Latency avg: ${avg.toFixed(2)}ms`)

    expect(p99).toBeLessThan(500) // SLO: p99 < 500ms for HTTP
  }, 60000)

  it('should handle >100 concurrent connections', async () => {
    const concurrentRequests = 200
    console.log(`[Benchmark] Testing ${concurrentRequests} concurrent connections...`)

    const start = Date.now()

    const requests = Array.from({ length: concurrentRequests }, (_, i) =>
      fetch(`${baseUrl}/api/transaction/apply`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          delta: [{
            subject: { termType: 'NamedNode', value: `http://example.org/concurrent-${i}` },
            predicate: { termType: 'NamedNode', value: 'http://example.org/test' },
            object: { termType: 'Literal', value: `concurrent-${i}`, datatype: { value: 'http://www.w3.org/2001/XMLSchema#string' } }
          }],
          author: 'concurrent-test'
        })
      })
    )

    const results = await Promise.allSettled(requests)
    const end = Date.now()

    const successful = results.filter(r => r.status === 'fulfilled' && r.value.ok).length
    const failed = results.filter(r => r.status === 'rejected' || !r.value?.ok).length
    const totalTime = end - start

    console.log(`[Benchmark] Concurrent requests: ${concurrentRequests}`)
    console.log(`[Benchmark] Successful: ${successful}`)
    console.log(`[Benchmark] Failed: ${failed}`)
    console.log(`[Benchmark] Total time: ${totalTime}ms`)
    console.log(`[Benchmark] Avg time per request: ${(totalTime / concurrentRequests).toFixed(2)}ms`)

    expect(successful).toBe(concurrentRequests) // All should succeed
    expect(totalTime).toBeLessThan(10000) // Should complete in <10s
  }, 30000)

  it('should maintain performance under sustained load', async () => {
    const duration = 30000 // 30 seconds sustained load
    const requestsPerSecond = 50
    const startTime = Date.now()

    console.log('[Benchmark] Starting 30-second sustained load test...')

    let totalRequests = 0
    let successfulRequests = 0
    let latencies = []

    while (Date.now() - startTime < duration) {
      const batchStart = Date.now()

      const batch = Array.from({ length: requestsPerSecond }, async () => {
        const reqStart = performance.now()

        try {
          const response = await fetch(`${baseUrl}/api/transaction/apply`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
              delta: [{
                subject: { termType: 'NamedNode', value: `http://example.org/sustained-${Date.now()}` },
                predicate: { termType: 'NamedNode', value: 'http://example.org/test' },
                object: { termType: 'Literal', value: 'sustained', datatype: { value: 'http://www.w3.org/2001/XMLSchema#string' } }
              }],
              author: 'sustained-test'
            })
          })

          const reqEnd = performance.now()

          if (response.ok) {
            successfulRequests++
            latencies.push(reqEnd - reqStart)
          }

          return response.ok
        } catch (error) {
          return false
        }
      })

      await Promise.all(batch)
      totalRequests += requestsPerSecond

      // Wait for next second
      const elapsed = Date.now() - batchStart
      if (elapsed < 1000) {
        await new Promise(resolve => setTimeout(resolve, 1000 - elapsed))
      }
    }

    const successRate = successfulRequests / totalRequests
    latencies.sort((a, b) => a - b)
    const p99 = latencies[Math.floor(latencies.length * 0.99)]

    console.log(`[Benchmark] Sustained load results:`)
    console.log(`  Total requests: ${totalRequests}`)
    console.log(`  Successful: ${successfulRequests}`)
    console.log(`  Success rate: ${(successRate * 100).toFixed(2)}%`)
    console.log(`  p99 latency: ${p99.toFixed(2)}ms`)

    expect(successRate).toBeGreaterThan(0.95) // >95% success rate
    expect(p99).toBeLessThan(500) // p99 < 500ms even under sustained load
  }, 60000)

  it('should query metrics from Prometheus under load', async () => {
    const prometheusUrl = 'http://localhost:9090'

    // Fire some transactions first
    await Promise.all(
      Array.from({ length: 100 }, (_, i) =>
        fetch(`${baseUrl}/api/transaction/apply`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            delta: [{
              subject: { termType: 'NamedNode', value: `http://example.org/metrics-${i}` },
              predicate: { termType: 'NamedNode', value: 'http://example.org/test' },
              object: { termType: 'Literal', value: 'metrics', datatype: { value: 'http://www.w3.org/2001/XMLSchema#string' } }
            }],
            author: 'metrics-test'
          })
        })
      )
    )

    // Wait for metrics export
    await new Promise(resolve => setTimeout(resolve, 5000))

    // Query transaction count
    const txQuery = 'kgc_transactions_total'
    const txUrl = `${prometheusUrl}/api/v1/query?query=${encodeURIComponent(txQuery)}`

    const txResponse = await fetch(txUrl)
    expect(txResponse.status).toBe(200)

    const txData = await txResponse.json()
    expect(txData.status).toBe('success')

    if (txData.data.result.length > 0) {
      const txCount = parseFloat(txData.data.result[0].value[1])
      console.log(`[Benchmark] Prometheus kgc_transactions_total: ${txCount}`)
      expect(txCount).toBeGreaterThan(0)
    }
  })
})
