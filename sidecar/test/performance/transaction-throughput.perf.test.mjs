/**
 * @file Transaction Throughput Performance Tests
 * @description Benchmark transaction processing rate through hooks and policy validation
 *
 * SLOs:
 * - Transaction throughput: >100 tx/sec
 * - Transaction latency: p95 <100ms, p99 <200ms
 * - Hook execution overhead: <20ms per hook
 * - Policy validation overhead: <30ms
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest'
import { performance } from 'node:perf_hooks'

// Mock managers for isolated testing
class MockTransactionManager {
  constructor() {
    this.transactions = []
  }

  async applyTransaction(delta, options) {
    // Simulate transaction processing (5-10ms)
    await new Promise(resolve => setTimeout(resolve, 5 + Math.random() * 5))
    const txId = `tx_${Date.now()}_${Math.random()}`
    this.transactions.push({ id: txId, delta, ...options })
    return { id: txId }
  }

  getTransactionCount() {
    return this.transactions.length
  }
}

class MockHookManager {
  constructor() {
    this.hooks = {
      pre: [],
      post: []
    }
  }

  registerHook(id, phase, fn) {
    this.hooks[phase].push({ id, fn })
  }

  getHooksByPhase(phase) {
    return this.hooks[phase]
  }

  async executeHook(hookId, delta) {
    const hook = [...this.hooks.pre, ...this.hooks.post].find(h => h.id === hookId)
    if (!hook) {
      throw new Error(`Hook ${hookId} not found`)
    }

    // Simulate hook execution (2-5ms)
    await new Promise(resolve => setTimeout(resolve, 2 + Math.random() * 3))

    return hook.fn ? await hook.fn(delta) : { passed: true }
  }
}

class MockPolicyPack {
  constructor() {
    this.policies = []
  }

  addPolicy(id, validationFn) {
    this.policies.push({ id, validationFn })
  }

  hasPolicies() {
    return this.policies.length > 0
  }

  async validate(delta) {
    // Simulate policy validation (10-20ms)
    await new Promise(resolve => setTimeout(resolve, 10 + Math.random() * 10))

    for (const policy of this.policies) {
      const result = await policy.validationFn(delta)
      if (!result.conforms) {
        return result
      }
    }

    return { conforms: true, violations: [] }
  }
}

class MockObservability {
  constructor() {
    this.metrics = []
  }

  async recordMetric(name, value) {
    this.metrics.push({ name, value, timestamp: Date.now() })
  }
}

/**
 * Calculate percentile from sorted array
 */
function percentile(sortedArray, p) {
  const index = Math.ceil((p / 100) * sortedArray.length) - 1
  return sortedArray[index]
}

/**
 * Simulate transaction apply with hooks and policies
 */
async function applyTransactionWithHooks(managers, delta, author, metadata) {
  const { transactionManager, hookManager, policyPack, observability } = managers
  const startTime = performance.now()
  const hooksExecuted = []

  try {
    // Execute pre-hooks
    const preHooks = hookManager.getHooksByPhase('pre')
    for (const hook of preHooks) {
      const result = await hookManager.executeHook(hook.id, delta)
      if (!result.passed) {
        throw new Error(`Hook ${hook.id} failed`)
      }
      hooksExecuted.push(hook.id)
    }

    // Apply transaction
    const txResult = await transactionManager.applyTransaction(delta, {
      author,
      metadata
    })

    // Validate with policy pack
    if (policyPack.hasPolicies()) {
      const validationResult = await policyPack.validate(delta)
      if (!validationResult.conforms) {
        throw new Error('Policy violation')
      }
    }

    // Execute post-hooks
    const postHooks = hookManager.getHooksByPhase('post')
    for (const hook of postHooks) {
      await hookManager.executeHook(hook.id, delta)
      hooksExecuted.push(hook.id)
    }

    const duration = performance.now() - startTime
    await observability.recordMetric('transaction.duration', duration)
    await observability.recordMetric('transaction.success', 1)

    return {
      transactionId: txResult.id,
      hooksExecuted,
      duration
    }
  } catch (error) {
    await observability.recordMetric('transaction.error', 1)
    throw error
  }
}

describe('Transaction Throughput Performance', () => {
  let managers

  beforeEach(() => {
    managers = {
      transactionManager: new MockTransactionManager(),
      hookManager: new MockHookManager(),
      policyPack: new MockPolicyPack(),
      observability: new MockObservability()
    }
  })

  describe('Baseline Transaction Processing (>100 tx/sec SLO)', () => {
    it('should achieve >100 tx/sec throughput with no hooks', async () => {
      const iterations = 200
      const delta = { additions: [], deletions: [] }
      const start = performance.now()

      for (let i = 0; i < iterations; i++) {
        await applyTransactionWithHooks(
          managers,
          delta,
          `author_${i}`,
          { index: i }
        )
      }

      const duration = performance.now() - start
      const throughput = (iterations / duration) * 1000 // tx/sec

      console.log('\n📈 Baseline Transaction Throughput:')
      console.log(`  Total transactions: ${iterations}`)
      console.log(`  Total duration: ${duration.toFixed(2)}ms`)
      console.log(`  Throughput: ${throughput.toFixed(2)} tx/sec (SLO: >100 tx/sec)`)
      console.log(`  Avg latency: ${(duration / iterations).toFixed(2)}ms`)

      expect(throughput).toBeGreaterThan(100)
    })

    it('should meet latency SLOs (p95 <100ms, p99 <200ms)', async () => {
      const iterations = 100
      const delta = { additions: [], deletions: [] }
      const latencies = []

      for (let i = 0; i < iterations; i++) {
        const start = performance.now()
        await applyTransactionWithHooks(
          managers,
          delta,
          `author_${i}`,
          { index: i }
        )
        latencies.push(performance.now() - start)
      }

      const sorted = latencies.sort((a, b) => a - b)
      const p50 = percentile(sorted, 50)
      const p95 = percentile(sorted, 95)
      const p99 = percentile(sorted, 99)
      const mean = latencies.reduce((sum, l) => sum + l, 0) / latencies.length

      console.log('\n⏱️  Baseline Transaction Latency:')
      console.log(`  Mean: ${mean.toFixed(2)}ms`)
      console.log(`  P50: ${p50.toFixed(2)}ms`)
      console.log(`  P95: ${p95.toFixed(2)}ms (SLO: <100ms)`)
      console.log(`  P99: ${p99.toFixed(2)}ms (SLO: <200ms)`)

      expect(p95).toBeLessThan(100)
      expect(p99).toBeLessThan(200)
    })
  })

  describe('Transaction with Pre-Hooks (<20ms overhead per hook)', () => {
    beforeEach(() => {
      // Register 3 pre-hooks
      managers.hookManager.registerHook('validate-author', 'pre', async (delta) => {
        await new Promise(resolve => setTimeout(resolve, 5))
        return { passed: true }
      })

      managers.hookManager.registerHook('validate-delta', 'pre', async (delta) => {
        await new Promise(resolve => setTimeout(resolve, 5))
        return { passed: true }
      })

      managers.hookManager.registerHook('check-permissions', 'pre', async (delta) => {
        await new Promise(resolve => setTimeout(resolve, 5))
        return { passed: true }
      })
    })

    it('should maintain <20ms overhead per pre-hook', async () => {
      const iterations = 50
      const delta = { additions: [], deletions: [] }
      const withHookLatencies = []
      const baselineLatencies = []

      // Measure with hooks
      for (let i = 0; i < iterations; i++) {
        const start = performance.now()
        await applyTransactionWithHooks(
          managers,
          delta,
          `author_${i}`,
          { index: i }
        )
        withHookLatencies.push(performance.now() - start)
      }

      // Measure baseline (no hooks)
      const noHookManagers = {
        transactionManager: new MockTransactionManager(),
        hookManager: new MockHookManager(),
        policyPack: new MockPolicyPack(),
        observability: new MockObservability()
      }

      for (let i = 0; i < iterations; i++) {
        const start = performance.now()
        await applyTransactionWithHooks(
          noHookManagers,
          delta,
          `author_${i}`,
          { index: i }
        )
        baselineLatencies.push(performance.now() - start)
      }

      const avgWithHooks = withHookLatencies.reduce((sum, l) => sum + l, 0) / iterations
      const avgBaseline = baselineLatencies.reduce((sum, l) => sum + l, 0) / iterations
      const overhead = avgWithHooks - avgBaseline
      const overheadPerHook = overhead / 3 // 3 pre-hooks

      console.log('\n🪝 Pre-Hook Performance:')
      console.log(`  Avg latency with hooks: ${avgWithHooks.toFixed(2)}ms`)
      console.log(`  Avg baseline latency: ${avgBaseline.toFixed(2)}ms`)
      console.log(`  Total overhead: ${overhead.toFixed(2)}ms`)
      console.log(`  Overhead per hook: ${overheadPerHook.toFixed(2)}ms (SLO: <20ms)`)

      expect(overheadPerHook).toBeLessThan(20)
    })
  })

  describe('Transaction with Policy Validation (<30ms overhead)', () => {
    beforeEach(() => {
      // Register validation policy
      managers.policyPack.addPolicy('shacl-validation', async (delta) => {
        // Simulate SHACL validation
        await new Promise(resolve => setTimeout(resolve, 15))
        return { conforms: true, violations: [] }
      })
    })

    it('should maintain <30ms policy validation overhead', async () => {
      const iterations = 50
      const delta = { additions: [], deletions: [] }
      const withPolicyLatencies = []
      const baselineLatencies = []

      // Measure with policy
      for (let i = 0; i < iterations; i++) {
        const start = performance.now()
        await applyTransactionWithHooks(
          managers,
          delta,
          `author_${i}`,
          { index: i }
        )
        withPolicyLatencies.push(performance.now() - start)
      }

      // Measure baseline (no policy)
      const noPolicyManagers = {
        transactionManager: new MockTransactionManager(),
        hookManager: new MockHookManager(),
        policyPack: new MockPolicyPack(), // Empty policy pack
        observability: new MockObservability()
      }

      for (let i = 0; i < iterations; i++) {
        const start = performance.now()
        await applyTransactionWithHooks(
          noPolicyManagers,
          delta,
          `author_${i}`,
          { index: i }
        )
        baselineLatencies.push(performance.now() - start)
      }

      const avgWithPolicy = withPolicyLatencies.reduce((sum, l) => sum + l, 0) / iterations
      const avgBaseline = baselineLatencies.reduce((sum, l) => sum + l, 0) / iterations
      const overhead = avgWithPolicy - avgBaseline

      console.log('\n🛡️  Policy Validation Performance:')
      console.log(`  Avg latency with policy: ${avgWithPolicy.toFixed(2)}ms`)
      console.log(`  Avg baseline latency: ${avgBaseline.toFixed(2)}ms`)
      console.log(`  Policy overhead: ${overhead.toFixed(2)}ms (SLO: <30ms)`)

      expect(overhead).toBeLessThan(30)
    })
  })

  describe('Full Pipeline Throughput (hooks + policy)', () => {
    beforeEach(() => {
      // Register hooks
      managers.hookManager.registerHook('pre-validate', 'pre', async () => ({ passed: true }))
      managers.hookManager.registerHook('post-log', 'post', async () => ({ passed: true }))

      // Register policy
      managers.policyPack.addPolicy('validation', async () => ({ conforms: true, violations: [] }))
    })

    it('should achieve >50 tx/sec with full pipeline', async () => {
      const iterations = 100
      const delta = { additions: [], deletions: [] }
      const start = performance.now()

      for (let i = 0; i < iterations; i++) {
        await applyTransactionWithHooks(
          managers,
          delta,
          `author_${i}`,
          { index: i }
        )
      }

      const duration = performance.now() - start
      const throughput = (iterations / duration) * 1000

      console.log('\n🔄 Full Pipeline Throughput:')
      console.log(`  Total transactions: ${iterations}`)
      console.log(`  Total duration: ${duration.toFixed(2)}ms`)
      console.log(`  Throughput: ${throughput.toFixed(2)} tx/sec (SLO: >50 tx/sec)`)

      expect(throughput).toBeGreaterThan(50)
    })
  })

  describe('Concurrent Transaction Processing', () => {
    it('should handle concurrent transactions efficiently', async () => {
      const concurrency = 50
      const delta = { additions: [], deletions: [] }
      const start = performance.now()

      const promises = Array.from({ length: concurrency }, (_, i) =>
        applyTransactionWithHooks(
          managers,
          delta,
          `author_${i}`,
          { index: i }
        )
      )

      const results = await Promise.all(promises)
      const duration = performance.now() - start
      const throughput = (concurrency / duration) * 1000

      console.log('\n⚡ Concurrent Transaction Processing:')
      console.log(`  Concurrent operations: ${concurrency}`)
      console.log(`  Total duration: ${duration.toFixed(2)}ms`)
      console.log(`  Throughput: ${throughput.toFixed(2)} tx/sec`)
      console.log(`  Avg per transaction: ${(duration / concurrency).toFixed(2)}ms`)

      expect(results).toHaveLength(concurrency)
      expect(throughput).toBeGreaterThan(100)
    })
  })
})
