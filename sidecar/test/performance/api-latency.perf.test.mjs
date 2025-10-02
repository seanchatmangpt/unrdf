/**
 * @file API Latency Performance Tests
 * @description Benchmark API endpoint response times with authentication and middleware
 *
 * SLOs:
 * - API latency: p95 <200ms, p99 <500ms
 * - Authentication overhead: <20ms
 * - Health check: <10ms
 * - Effect registration: p95 <150ms
 * - Effect execution: p95 <200ms
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest'
import { performance } from 'node:perf_hooks'
import { $fetch, fetch } from 'ofetch'
import { setup, url } from '@nuxt/test-utils'

/**
 * Calculate percentile from sorted array
 */
function percentile(sortedArray, p) {
  const index = Math.ceil((p / 100) * sortedArray.length) - 1
  return sortedArray[index]
}

/**
 * Measure endpoint latency
 */
async function measureEndpoint(name, iterations, requestFn) {
  const latencies = []
  const warmup = Math.min(5, Math.floor(iterations * 0.1))

  // Warmup
  for (let i = 0; i < warmup; i++) {
    try {
      await requestFn()
    } catch (error) {
      // Ignore warmup errors
    }
  }

  // Measure
  for (let i = 0; i < iterations; i++) {
    const start = performance.now()
    try {
      await requestFn()
      latencies.push(performance.now() - start)
    } catch (error) {
      // Record error latency too
      latencies.push(performance.now() - start)
    }
  }

  const sorted = latencies.sort((a, b) => a - b)
  const mean = latencies.reduce((sum, l) => sum + l, 0) / latencies.length

  return {
    name,
    iterations,
    mean: mean.toFixed(2),
    median: sorted[Math.floor(sorted.length / 2)].toFixed(2),
    p95: percentile(sorted, 95).toFixed(2),
    p99: percentile(sorted, 99).toFixed(2),
    min: sorted[0].toFixed(2),
    max: sorted[sorted.length - 1].toFixed(2)
  }
}

describe('API Latency Performance', () => {
  let authToken

  beforeAll(async () => {
    // Setup test server
    await setup({
      server: true,
      browser: false
    })

    // Register test user and get auth token
    try {
      const registerResponse = await $fetch('/api/auth/register', {
        method: 'POST',
        body: {
          email: 'perf-test@example.com',
          password: 'Test123!@#',
          name: 'Performance Test User'
        }
      })

      authToken = registerResponse.token
    } catch (error) {
      // User might already exist, try login
      const loginResponse = await $fetch('/api/auth/login', {
        method: 'POST',
        body: {
          email: 'perf-test@example.com',
          password: 'Test123!@#'
        }
      })

      authToken = loginResponse.token
    }
  })

  describe('Health Check Endpoint (<10ms SLO)', () => {
    it('should respond in <10ms p95', async () => {
      const result = await measureEndpoint(
        'Health check',
        100,
        async () => {
          await $fetch('/api/health')
        }
      )

      console.log('\n💚 Health Check Performance:')
      console.log(`  Mean: ${result.mean}ms`)
      console.log(`  P95: ${result.p95}ms (SLO: <10ms)`)
      console.log(`  P99: ${result.p99}ms`)

      expect(parseFloat(result.p95)).toBeLessThan(10)
    })
  })

  describe('Authentication Latency (<20ms overhead)', () => {
    it('should validate auth token in <20ms', async () => {
      const authenticatedLatencies = []
      const unauthenticatedLatencies = []
      const iterations = 50

      // Measure authenticated requests
      for (let i = 0; i < iterations; i++) {
        const start = performance.now()
        try {
          await $fetch('/api/auth/me', {
            headers: {
              Authorization: `Bearer ${authToken}`
            }
          })
        } catch (error) {
          // Ignore errors
        }
        authenticatedLatencies.push(performance.now() - start)
      }

      // Measure unauthenticated health check (baseline)
      for (let i = 0; i < iterations; i++) {
        const start = performance.now()
        await $fetch('/api/health')
        unauthenticatedLatencies.push(performance.now() - start)
      }

      const avgAuthenticated = authenticatedLatencies.reduce((sum, l) => sum + l, 0) / iterations
      const avgUnauthenticated = unauthenticatedLatencies.reduce((sum, l) => sum + l, 0) / iterations
      const authOverhead = avgAuthenticated - avgUnauthenticated

      console.log('\n🔐 Authentication Performance:')
      console.log(`  Avg authenticated latency: ${avgAuthenticated.toFixed(2)}ms`)
      console.log(`  Avg unauthenticated latency: ${avgUnauthenticated.toFixed(2)}ms`)
      console.log(`  Auth overhead: ${authOverhead.toFixed(2)}ms (SLO: <20ms)`)

      expect(authOverhead).toBeLessThan(20)
    })
  })

  describe('Effect Registration Endpoint (p95 <150ms)', () => {
    it('should register effects in <150ms p95', async () => {
      const effectCode = `
        const effect = (input) => {
          return { result: input.value * 2 }
        }
      `

      const result = await measureEndpoint(
        'Effect registration',
        30,
        async () => {
          const effectId = `perf-test-${Date.now()}-${Math.random()}`
          await $fetch('/api/effects/register', {
            method: 'POST',
            headers: {
              Authorization: `Bearer ${authToken}`
            },
            body: {
              id: effectId,
              code: effectCode
            }
          })
        }
      )

      console.log('\n📝 Effect Registration Performance:')
      console.log(`  Mean: ${result.mean}ms`)
      console.log(`  P95: ${result.p95}ms (SLO: <150ms)`)
      console.log(`  P99: ${result.p99}ms`)

      expect(parseFloat(result.p95)).toBeLessThan(150)
    })
  })

  describe('Effect Execution Endpoint (p95 <200ms)', () => {
    let testEffectId

    beforeAll(async () => {
      // Register test effect
      testEffectId = `perf-execution-test-${Date.now()}`
      await $fetch('/api/effects/register', {
        method: 'POST',
        headers: {
          Authorization: `Bearer ${authToken}`
        },
        body: {
          id: testEffectId,
          code: `const effect = (input) => ({ result: input.value * 2 })`
        }
      })
    })

    it('should execute effects in <200ms p95', async () => {
      const result = await measureEndpoint(
        'Effect execution',
        50,
        async () => {
          await $fetch('/api/effects/execute', {
            method: 'POST',
            body: {
              effectId: testEffectId,
              input: { value: 42 }
            }
          })
        }
      )

      console.log('\n⚡ Effect Execution Performance:')
      console.log(`  Mean: ${result.mean}ms`)
      console.log(`  P95: ${result.p95}ms (SLO: <200ms)`)
      console.log(`  P99: ${result.p99}ms`)

      expect(parseFloat(result.p95)).toBeLessThan(200)
    })
  })

  describe('Transaction Apply Endpoint (p95 <200ms)', () => {
    it('should apply transactions in <200ms p95', async () => {
      const result = await measureEndpoint(
        'Transaction apply',
        30,
        async () => {
          await $fetch('/api/transaction/apply', {
            method: 'POST',
            headers: {
              Authorization: `Bearer ${authToken}`
            },
            body: {
              delta: {
                additions: [],
                deletions: []
              },
              author: 'perf-test@example.com',
              metadata: { test: true }
            }
          })
        }
      )

      console.log('\n📊 Transaction Apply Performance:')
      console.log(`  Mean: ${result.mean}ms`)
      console.log(`  P95: ${result.p95}ms (SLO: <200ms)`)
      console.log(`  P99: ${result.p99}ms (SLO: <500ms)`)

      expect(parseFloat(result.p95)).toBeLessThan(200)
      expect(parseFloat(result.p99)).toBeLessThan(500)
    })
  })

  describe('Concurrent API Requests', () => {
    it('should handle concurrent requests efficiently', async () => {
      const concurrency = 20
      const start = performance.now()

      const promises = Array.from({ length: concurrency }, () =>
        $fetch('/api/health')
      )

      await Promise.all(promises)
      const duration = performance.now() - start
      const avgLatency = duration / concurrency
      const throughput = (concurrency / duration) * 1000

      console.log('\n🚀 Concurrent Request Performance:')
      console.log(`  Concurrent requests: ${concurrency}`)
      console.log(`  Total duration: ${duration.toFixed(2)}ms`)
      console.log(`  Avg latency: ${avgLatency.toFixed(2)}ms`)
      console.log(`  Throughput: ${throughput.toFixed(2)} req/sec`)

      expect(avgLatency).toBeLessThan(50)
    })
  })

  describe('Error Response Latency', () => {
    it('should return errors quickly (<50ms)', async () => {
      const result = await measureEndpoint(
        'Error response',
        50,
        async () => {
          try {
            await $fetch('/api/effects/execute', {
              method: 'POST',
              body: {
                effectId: 'non-existent-effect',
                input: {}
              }
            })
          } catch (error) {
            // Expected error
          }
        }
      )

      console.log('\n❌ Error Response Performance:')
      console.log(`  Mean: ${result.mean}ms`)
      console.log(`  P95: ${result.p95}ms (SLO: <50ms)`)

      expect(parseFloat(result.p95)).toBeLessThan(50)
    })
  })
})
