// @ts-check
/**
 * Scheduled Task Failure Chaos Tests
 *
 * Tests resilience of scheduled tasks under failure conditions:
 * - Task execution failures
 * - Circuit breaker integration
 * - Task timeout scenarios
 * - Concurrent task failures
 * - Recovery mechanisms
 */

import { describe, it, expect, beforeEach, vi } from 'vitest'
import { setTimeout } from 'timers/promises'
import { circuitBreakerRegistry } from '../../server/utils/circuit-breaker.mjs'

describe('Scheduled Task Failure Chaos Tests', () => {
  beforeEach(() => {
    // Reset circuit breakers between tests
    const breakers = circuitBreakerRegistry.getAll()
    for (const breaker of breakers.values()) {
      breaker.reset()
    }
  })

  describe('Task Execution Failures', () => {
    it('should handle task throwing exception', async () => {
      const failingTask = async () => {
        throw new Error('Task execution failed - database unavailable')
      }

      const taskWrapper = async (taskFn) => {
        try {
          return await taskFn()
        } catch (error) {
          return {
            result: 'error',
            error: error.message,
            timestamp: new Date().toISOString()
          }
        }
      }

      const result = await taskWrapper(failingTask)

      expect(result.result).toBe('error')
      expect(result.error).toContain('database unavailable')
      expect(result.timestamp).toBeDefined()
    })

    it('should open circuit breaker after repeated task failures', async () => {
      const breaker = circuitBreakerRegistry.get('task:hooks:evaluate', {
        failureThreshold: 3,
        timeout: 5000
      })

      const failingTask = async () => {
        throw new Error('Hook evaluation failed')
      }

      // Execute task 5 times (should open circuit at 3)
      for (let i = 0; i < 5; i++) {
        try {
          await breaker.execute(failingTask)
        } catch (error) {
          // Expected failures
        }
      }

      const metrics = breaker.getMetrics()
      expect(metrics.state).toBe('OPEN')
      expect(metrics.failures).toBeGreaterThanOrEqual(3)
    })

    it('should track error patterns in failed tasks', async () => {
      const breaker = circuitBreakerRegistry.get('task:policy:refresh', {
        failureThreshold: 5
      })

      const errorTypes = [
        'Network timeout',
        'Database connection failed',
        'Network timeout',
        'Memory exhausted',
        'Network timeout'
      ]

      // Execute tasks with different error types
      for (const errorType of errorTypes) {
        try {
          await breaker.execute(async () => {
            throw new Error(errorType)
          })
        } catch (error) {
          // Expected
        }
      }

      const patterns = breaker.getErrorPatterns()

      // Should detect "Network timeout" as dominant pattern
      expect(patterns.has(/timeout/i.source)).toBe(true)

      const timeoutCount = patterns.get(/timeout/i.source)
      expect(timeoutCount).toBe(3) // Occurred 3 times
    })
  })

  describe('Task Timeout Scenarios', () => {
    it('should timeout task that exceeds time limit', async () => {
      const slowTask = async () => {
        await setTimeout(5000) // 5 second task
        return 'completed'
      }

      const executeWithTimeout = async (task, timeout) => {
        const timeoutPromise = setTimeout(timeout).then(() => {
          throw new Error(`Task timeout after ${timeout}ms`)
        })

        return Promise.race([task(), timeoutPromise])
      }

      await expect(executeWithTimeout(slowTask, 1000))
        .rejects.toThrow(/timeout/)
    })

    it('should allow task to complete within timeout', async () => {
      const quickTask = async () => {
        await setTimeout(100)
        return { result: 'success', duration: 100 }
      }

      const executeWithTimeout = async (task, timeout) => {
        const timeoutPromise = setTimeout(timeout).then(() => {
          throw new Error(`Task timeout after ${timeout}ms`)
        })

        return Promise.race([task(), timeoutPromise])
      }

      const result = await executeWithTimeout(quickTask, 1000)

      expect(result.result).toBe('success')
      expect(result.duration).toBeLessThan(1000)
    })

    it('should handle varying timeout per task type', async () => {
      const tasks = [
        { name: 'health-check', timeout: 1000, duration: 500 },
        { name: 'hook-eval', timeout: 5000, duration: 2000 },
        { name: 'policy-refresh', timeout: 10000, duration: 3000 }
      ]

      const executeTask = async (task) => {
        const timeoutPromise = setTimeout(task.timeout).then(() => {
          throw new Error(`${task.name} timeout`)
        })

        const taskPromise = setTimeout(task.duration).then(() => ({
          name: task.name,
          completed: true
        }))

        return Promise.race([taskPromise, timeoutPromise])
      }

      const results = await Promise.all(tasks.map(executeTask))

      expect(results.length).toBe(3)
      results.forEach(result => {
        expect(result.completed).toBe(true)
      })
    })
  })

  describe('Concurrent Task Failures', () => {
    it('should isolate failures between concurrent tasks', async () => {
      const tasks = [
        { id: 'task1', shouldFail: true, error: 'Task 1 failed' },
        { id: 'task2', shouldFail: false, result: 'success' },
        { id: 'task3', shouldFail: true, error: 'Task 3 failed' },
        { id: 'task4', shouldFail: false, result: 'success' }
      ]

      const executeTask = async (task) => {
        try {
          if (task.shouldFail) {
            throw new Error(task.error)
          }
          return { id: task.id, result: task.result }
        } catch (error) {
          return { id: task.id, error: error.message }
        }
      }

      const results = await Promise.all(tasks.map(executeTask))

      expect(results.length).toBe(4)

      // Task 1 and 3 should have errors
      expect(results[0].error).toBe('Task 1 failed')
      expect(results[2].error).toBe('Task 3 failed')

      // Task 2 and 4 should succeed
      expect(results[1].result).toBe('success')
      expect(results[3].result).toBe('success')
    })

    it('should handle all tasks failing simultaneously', async () => {
      const taskCount = 5
      const tasks = Array(taskCount).fill(null).map((_, i) => ({
        id: `task${i}`,
        shouldFail: true
      }))

      const executeTask = async (task) => {
        try {
          throw new Error(`Task ${task.id} failed - system overload`)
        } catch (error) {
          return { id: task.id, error: error.message }
        }
      }

      const results = await Promise.all(tasks.map(executeTask))

      expect(results.length).toBe(taskCount)
      results.forEach(result => {
        expect(result.error).toContain('failed')
      })
    })

    it('should continue processing tasks when some fail', async () => {
      const taskQueue = [
        { id: 1, shouldFail: false },
        { id: 2, shouldFail: true },
        { id: 3, shouldFail: false },
        { id: 4, shouldFail: true },
        { id: 5, shouldFail: false }
      ]

      const processQueue = async (queue) => {
        const results = []

        for (const task of queue) {
          try {
            if (task.shouldFail) {
              throw new Error(`Task ${task.id} failed`)
            }
            results.push({ id: task.id, status: 'success' })
          } catch (error) {
            results.push({ id: task.id, status: 'failed', error: error.message })
          }
        }

        return results
      }

      const results = await processQueue(taskQueue)

      expect(results.length).toBe(5)
      expect(results.filter(r => r.status === 'success').length).toBe(3)
      expect(results.filter(r => r.status === 'failed').length).toBe(2)
    })
  })

  describe('Task Recovery Mechanisms', () => {
    it('should retry failed task with exponential backoff', async () => {
      let attempts = 0
      const maxRetries = 3

      const unreliableTask = async () => {
        attempts++
        if (attempts < 3) {
          throw new Error('Temporary failure')
        }
        return { result: 'success', attempts }
      }

      const retryTask = async (taskFn, config = { maxRetries: 3, initialDelay: 100 }) => {
        let attempt = 0

        while (attempt <= config.maxRetries) {
          try {
            return await taskFn()
          } catch (error) {
            if (attempt === config.maxRetries) {
              throw error
            }

            const delay = config.initialDelay * Math.pow(2, attempt)
            await setTimeout(delay)
            attempt++
          }
        }
      }

      const result = await retryTask(unreliableTask)

      expect(result.result).toBe('success')
      expect(result.attempts).toBe(3)
    })

    it('should use circuit breaker half-open state for recovery', async () => {
      const breaker = circuitBreakerRegistry.get('task:lockchain:archive', {
        failureThreshold: 3,
        timeout: 500,
        resetTimeout: 500
      })

      // Trip circuit
      for (let i = 0; i < 3; i++) {
        try {
          await breaker.execute(async () => {
            throw new Error('Archive failed')
          })
        } catch (error) {
          // Expected
        }
      }

      expect(breaker.getState()).toBe('OPEN')

      // Wait for reset timeout
      await setTimeout(600)

      expect(breaker.getState()).toBe('HALF_OPEN')

      // Successful execution should close circuit
      const result = await breaker.execute(async () => {
        return { archived: true }
      })

      expect(result.archived).toBe(true)
      expect(breaker.getState()).toBe('CLOSED')
    })

    it('should implement adaptive retry based on error type', async () => {
      const errorRetryConfig = {
        'Network timeout': { maxRetries: 5, initialDelay: 1000 },
        'Database lock': { maxRetries: 10, initialDelay: 100 },
        'Memory exhausted': { maxRetries: 2, initialDelay: 5000 },
        'Unknown error': { maxRetries: 3, initialDelay: 500 }
      }

      const getRetryConfig = (error) => {
        for (const [pattern, config] of Object.entries(errorRetryConfig)) {
          if (error.includes(pattern)) {
            return config
          }
        }
        return errorRetryConfig['Unknown error']
      }

      // Test different error types
      const errors = [
        'Network timeout occurred',
        'Database lock detected',
        'Memory exhausted',
        'Something went wrong'
      ]

      const configs = errors.map(error => getRetryConfig(error))

      expect(configs[0].maxRetries).toBe(5)  // Network timeout
      expect(configs[1].maxRetries).toBe(10) // Database lock
      expect(configs[2].maxRetries).toBe(2)  // Memory exhausted
      expect(configs[3].maxRetries).toBe(3)  // Unknown
    })
  })

  describe('Self-Healing Health Check Task', () => {
    it('should detect degraded health and trigger self-healing', async () => {
      const breaker = circuitBreakerRegistry.get('test:self-heal', {
        failureThreshold: 5
      })

      // Cause some failures to degrade health
      for (let i = 0; i < 3; i++) {
        try {
          await breaker.execute(async () => {
            throw new Error('Degrading health')
          })
        } catch (error) {
          // Expected
        }
      }

      const metrics = breaker.getMetrics()
      const healthScore = metrics.healthScore

      // Health should be degraded but not critical
      expect(healthScore).toBeLessThan(100)
      expect(healthScore).toBeGreaterThan(0)

      // Self-healing action
      if (healthScore < 50) {
        breaker.reset()
      }

      const afterReset = breaker.getMetrics()
      expect(afterReset.healthScore).toBe(100)
    })

    it('should monitor multiple circuit breakers in health check', async () => {
      const breakers = [
        circuitBreakerRegistry.get('hook:eval1'),
        circuitBreakerRegistry.get('hook:eval2'),
        circuitBreakerRegistry.get('policy:refresh')
      ]

      // Cause failures in some breakers
      try {
        await breakers[0].execute(async () => { throw new Error('Fail') })
      } catch (e) {}

      const healthSummary = circuitBreakerRegistry.getHealthSummary()
      const breakerNames = Object.keys(healthSummary)

      expect(breakerNames.length).toBeGreaterThanOrEqual(3)
      expect(healthSummary['hook:eval1']).toBeDefined()
    })

    it('should calculate system-wide health score', async () => {
      const breakers = {
        'healthy1': circuitBreakerRegistry.get('healthy1'),
        'healthy2': circuitBreakerRegistry.get('healthy2'),
        'degraded': circuitBreakerRegistry.get('degraded'),
        'failed': circuitBreakerRegistry.get('failed')
      }

      // Degrade one breaker
      for (let i = 0; i < 2; i++) {
        try {
          await breakers.degraded.execute(async () => { throw new Error('Fail') })
        } catch (e) {}
      }

      // Fail one breaker
      for (let i = 0; i < 5; i++) {
        try {
          await breakers.failed.execute(async () => { throw new Error('Fail') })
        } catch (e) {}
      }

      const calculateSystemHealth = () => {
        const summary = circuitBreakerRegistry.getHealthSummary()
        const scores = Object.values(summary).map(m => m.healthScore)

        if (scores.length === 0) return 100

        return scores.reduce((sum, score) => sum + score, 0) / scores.length
      }

      const systemHealth = calculateSystemHealth()

      expect(systemHealth).toBeLessThan(100) // Some failures
      expect(systemHealth).toBeGreaterThan(0)  // Not complete failure
    })
  })
})
