/**
 * @file Effect Execution Performance Tests
 * @description Benchmark SecureSandbox effect execution with memory and timeout analysis
 *
 * SLOs:
 * - Simple effect execution: <100ms (p95), <150ms (p99)
 * - Complex effect execution: <500ms (p95), <1000ms (p99)
 * - Memory usage: <50% of limit for simple effects, <80% for complex
 * - Timeout handling: Must enforce timeout within 10% margin
 */

import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { SecureSandbox } from '../../server/utils/secure-sandbox.mjs'
import { performance } from 'node:perf_hooks'

/**
 * Calculate percentile from sorted array
 */
function percentile(sortedArray, p) {
  const index = Math.ceil((p / 100) * sortedArray.length) - 1
  return sortedArray[index]
}

/**
 * Run benchmark iterations and collect metrics
 */
async function runBenchmark(name, iterations, fn) {
  const measurements = []
  const warmupIterations = Math.min(10, Math.floor(iterations * 0.1))

  // Warmup phase
  for (let i = 0; i < warmupIterations; i++) {
    await fn()
  }

  // Measurement phase
  for (let i = 0; i < iterations; i++) {
    const start = performance.now()
    await fn()
    const duration = performance.now() - start
    measurements.push(duration)
  }

  const sorted = measurements.sort((a, b) => a - b)
  const mean = measurements.reduce((sum, m) => sum + m, 0) / measurements.length

  return {
    name,
    iterations,
    mean: mean.toFixed(2),
    median: sorted[Math.floor(sorted.length / 2)].toFixed(2),
    p95: percentile(sorted, 95).toFixed(2),
    p99: percentile(sorted, 99).toFixed(2),
    min: sorted[0].toFixed(2),
    max: sorted[sorted.length - 1].toFixed(2),
    stdDev: Math.sqrt(
      measurements.reduce((sum, m) => sum + Math.pow(m - mean, 2), 0) / measurements.length
    ).toFixed(2)
  }
}

describe('Effect Execution Performance', () => {
  let sandbox

  beforeAll(() => {
    sandbox = new SecureSandbox({
      memoryLimit: 128,
      timeout: 5000,
      enableWasm: true
    })
  })

  afterAll(async () => {
    await sandbox.cleanup()
  })

  describe('Simple Effect Execution (<100ms SLO)', () => {
    const simpleEffectCode = `
      const effect = (input) => {
        return { result: input.value * 2 }
      }
    `

    beforeEach(async () => {
      await sandbox.registerEffect('simple-perf-test', simpleEffectCode)
    })

    it('should meet <100ms p95 latency SLO for simple effects', async () => {
      const result = await runBenchmark(
        'Simple effect execution',
        100,
        async () => {
          await sandbox.executeEffect('simple-perf-test', { value: 42 })
        }
      )

      console.log('\n📊 Simple Effect Performance:')
      console.log(`  Mean: ${result.mean}ms`)
      console.log(`  Median: ${result.median}ms`)
      console.log(`  P95: ${result.p95}ms (SLO: <100ms)`)
      console.log(`  P99: ${result.p99}ms (SLO: <150ms)`)
      console.log(`  Min: ${result.min}ms, Max: ${result.max}ms`)
      console.log(`  StdDev: ${result.stdDev}ms`)

      expect(parseFloat(result.p95)).toBeLessThan(100)
      expect(parseFloat(result.p99)).toBeLessThan(150)
    })

    it('should maintain low memory usage (<50% of limit)', async () => {
      await sandbox.executeEffect('simple-perf-test', { value: 42 })
      const memoryUsage = await sandbox.getMemoryUsage('simple-perf-test')

      console.log('\n💾 Simple Effect Memory:')
      console.log(`  Used: ${(memoryUsage.used / 1024 / 1024).toFixed(2)} MB`)
      console.log(`  Percentage: ${memoryUsage.percentage.toFixed(2)}% (SLO: <50%)`)

      expect(memoryUsage.percentage).toBeLessThan(50)
    })
  })

  describe('Complex Effect Execution (<500ms SLO)', () => {
    const complexEffectCode = `
      const effect = (input) => {
        // Simulate complex computation
        const data = []
        for (let i = 0; i < input.iterations; i++) {
          data.push({
            id: i,
            value: Math.sqrt(i) * Math.PI,
            nested: {
              timestamp: Date.now(),
              random: Math.random()
            }
          })
        }

        // Aggregate
        const sum = data.reduce((acc, item) => acc + item.value, 0)
        const avg = sum / data.length

        return {
          count: data.length,
          sum,
          avg,
          first: data[0],
          last: data[data.length - 1]
        }
      }
    `

    beforeEach(async () => {
      await sandbox.registerEffect('complex-perf-test', complexEffectCode)
    })

    it('should meet <500ms p95 latency SLO for complex effects', async () => {
      const result = await runBenchmark(
        'Complex effect execution',
        50,
        async () => {
          await sandbox.executeEffect('complex-perf-test', { iterations: 1000 })
        }
      )

      console.log('\n📊 Complex Effect Performance:')
      console.log(`  Mean: ${result.mean}ms`)
      console.log(`  Median: ${result.median}ms`)
      console.log(`  P95: ${result.p95}ms (SLO: <500ms)`)
      console.log(`  P99: ${result.p99}ms (SLO: <1000ms)`)
      console.log(`  Min: ${result.min}ms, Max: ${result.max}ms`)
      console.log(`  StdDev: ${result.stdDev}ms`)

      expect(parseFloat(result.p95)).toBeLessThan(500)
      expect(parseFloat(result.p99)).toBeLessThan(1000)
    })

    it('should maintain acceptable memory usage (<80% of limit)', async () => {
      await sandbox.executeEffect('complex-perf-test', { iterations: 1000 })
      const memoryUsage = await sandbox.getMemoryUsage('complex-perf-test')

      console.log('\n💾 Complex Effect Memory:')
      console.log(`  Used: ${(memoryUsage.used / 1024 / 1024).toFixed(2)} MB`)
      console.log(`  Percentage: ${memoryUsage.percentage.toFixed(2)}% (SLO: <80%)`)

      expect(memoryUsage.percentage).toBeLessThan(80)
    })
  })

  describe('Timeout Enforcement (±10% accuracy SLO)', () => {
    const infiniteLoopCode = `
      const effect = (input) => {
        while (true) {
          // Infinite loop to test timeout
        }
      }
    `

    beforeEach(async () => {
      const timeoutSandbox = new SecureSandbox({
        memoryLimit: 128,
        timeout: 1000, // 1 second timeout
        enableWasm: false
      })
      sandbox.timeoutTestSandbox = timeoutSandbox
      await timeoutSandbox.registerEffect('timeout-test', infiniteLoopCode)
    })

    it('should enforce timeout within 10% margin', async () => {
      const expectedTimeout = 1000
      const measurements = []

      for (let i = 0; i < 10; i++) {
        const start = performance.now()
        try {
          await sandbox.timeoutTestSandbox.executeEffect('timeout-test', {})
        } catch (error) {
          const duration = performance.now() - start
          measurements.push(duration)
          expect(error.message).toContain('timed out')
        }
      }

      const avgTimeout = measurements.reduce((sum, m) => sum + m, 0) / measurements.length
      const margin = Math.abs(avgTimeout - expectedTimeout)
      const marginPercent = (margin / expectedTimeout) * 100

      console.log('\n⏱️  Timeout Enforcement:')
      console.log(`  Expected: ${expectedTimeout}ms`)
      console.log(`  Actual (avg): ${avgTimeout.toFixed(2)}ms`)
      console.log(`  Margin: ${marginPercent.toFixed(2)}% (SLO: <10%)`)

      expect(marginPercent).toBeLessThan(10)
    }, 15000)
  })

  describe('Memory Limit Enforcement', () => {
    const memoryIntensiveCode = `
      const effect = (input) => {
        const arrays = []
        // Allocate large arrays to exceed memory limit
        for (let i = 0; i < 1000; i++) {
          arrays.push(new Array(100000).fill(Math.random()))
        }
        return { count: arrays.length }
      }
    `

    it('should enforce memory limits and throw error', async () => {
      const limitedSandbox = new SecureSandbox({
        memoryLimit: 16, // Only 16MB
        timeout: 10000,
        enableWasm: false
      })

      await limitedSandbox.registerEffect('memory-test', memoryIntensiveCode)

      await expect(
        limitedSandbox.executeEffect('memory-test', {})
      ).rejects.toThrow()

      await limitedSandbox.cleanup()
    }, 15000)
  })

  describe('Concurrent Execution Throughput', () => {
    const batchEffectCode = `
      const effect = (input) => {
        return { id: input.id, processed: true, timestamp: Date.now() }
      }
    `

    it('should handle concurrent executions efficiently', async () => {
      await sandbox.registerEffect('batch-test', batchEffectCode)

      const concurrency = 50
      const start = performance.now()

      const promises = Array.from({ length: concurrency }, (_, i) =>
        sandbox.executeEffect('batch-test', { id: i })
      )

      const results = await Promise.all(promises)
      const duration = performance.now() - start
      const throughput = (concurrency / duration) * 1000 // ops/sec

      console.log('\n🚀 Concurrent Execution:')
      console.log(`  Total operations: ${concurrency}`)
      console.log(`  Total duration: ${duration.toFixed(2)}ms`)
      console.log(`  Throughput: ${throughput.toFixed(2)} ops/sec`)
      console.log(`  Avg per operation: ${(duration / concurrency).toFixed(2)}ms`)

      expect(results).toHaveLength(concurrency)
      expect(throughput).toBeGreaterThan(10) // At least 10 ops/sec
    })
  })

  describe('Effect Registration Performance', () => {
    it('should register effects quickly (<50ms p95)', async () => {
      const simpleCode = `const effect = (input) => ({ result: input.value })`

      const result = await runBenchmark(
        'Effect registration',
        50,
        async () => {
          const effectId = `reg-test-${Math.random()}`
          await sandbox.registerEffect(effectId, simpleCode)
        }
      )

      console.log('\n📝 Effect Registration Performance:')
      console.log(`  Mean: ${result.mean}ms`)
      console.log(`  P95: ${result.p95}ms (SLO: <50ms)`)
      console.log(`  P99: ${result.p99}ms`)

      expect(parseFloat(result.p95)).toBeLessThan(50)
    })
  })
})
