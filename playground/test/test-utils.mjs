#!/usr/bin/env node

/**
 * @fileoverview Test utilities for UNRDF Playground integration tests
 *
 * Provides helper functions and fixtures for testing the playground functionality
 */

import { randomUUID } from 'node:crypto'

/**
 * Generate a unique test identifier
 * @param {string} prefix - Prefix for the identifier
 * @returns {string} Unique identifier
 */
export function generateTestId(prefix = 'test') {
  const uuid = randomUUID().replace(/-/g, '')
  return `${prefix}-${uuid.substring(0, 8)}`
}

/**
 * Create sample RDF data for testing
 * @param {Object} options - Options for data generation
 * @returns {string} Turtle format RDF data
 */
export function createSampleData(options = {}) {
  const {
    serviceCount = 3,
    prefix = 'http://example.org/',
    includeHealthMetrics = true
  } = options

  const lines = []

  for (let i = 1; i <= serviceCount; i++) {
    lines.push(`${prefix}service${i} a ${prefix}Service ;`)

    if (includeHealthMetrics) {
      const errorRate = Math.random() * 0.2
      const latency = 100 + Math.random() * 2000
      const requests = 1000 + Math.random() * 10000

      lines.push(`  ${prefix}errorRate "${errorRate.toFixed(3)}" ;`)
      lines.push(`  ${prefix}latency "${Math.round(latency)}" ;`)
      lines.push(`  ${prefix}requests "${Math.round(requests)}" ;`)
    }

    lines.push(`  ${prefix}name "Service ${i}" .`)
  }

  return lines.join('\n')
}

/**
 * Create a sample hook for testing
 * @param {Object} options - Hook configuration options
 * @returns {Object} Hook definition
 */
export function createSampleHook(options = {}) {
  const {
    id = generateTestId('hook'),
    name = 'Test Hook',
    select = 'SELECT ?s WHERE { ?s a <http://example.org/Service> }',
    predicates = [{ kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } }],
    combine = 'AND'
  } = options

  return {
    id,
    name,
    description: `Test hook: ${name}`,
    select,
    predicates,
    combine
  }
}

/**
 * Wait for a condition to be true
 * @param {Function} condition - Function that returns a boolean
 * @param {number} timeout - Timeout in milliseconds
 * @param {number} interval - Check interval in milliseconds
 * @returns {Promise<void>}
 */
export async function waitFor(condition, timeout = 5000, interval = 100) {
  const startTime = Date.now()

  while (Date.now() - startTime < timeout) {
    if (await condition()) {
      return
    }
    await new Promise(resolve => setTimeout(resolve, interval))
  }

  throw new Error(`Condition not met within ${timeout}ms`)
}

/**
 * Retry a function until it succeeds
 * @param {Function} fn - Function to retry
 * @param {number} maxAttempts - Maximum number of attempts
 * @param {number} delay - Delay between attempts in milliseconds
 * @returns {Promise<any>} Result of the function
 */
export async function retry(fn, maxAttempts = 3, delay = 1000) {
  let lastError

  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      return await fn()
    } catch (error) {
      lastError = error

      if (attempt < maxAttempts) {
        console.log(`Attempt ${attempt} failed, retrying in ${delay}ms...`)
        await new Promise(resolve => setTimeout(resolve, delay))
      }
    }
  }

  throw lastError
}

/**
 * Create test data fixtures
 * @returns {Object} Test fixtures
 */
export const testFixtures = {
  sampleData: createSampleData(),

  complexData: `@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:user1 a foaf:Person ;
  foaf:name "Alice" ;
  ex:score 85 ;
  ex:status "active" .

ex:user2 a foaf:Person ;
  foaf:name "Bob" ;
  ex:score 92 ;
  ex:status "inactive" .

ex:user3 a foaf:Person ;
  foaf:name "Charlie" ;
  ex:score 78 ;
  ex:status "active" .`,

  minimalHook: {
    id: generateTestId('minimal'),
    name: 'Minimal Hook',
    select: 'SELECT ?s WHERE { ?s a <http://example.org/Service> }',
    predicates: [{ kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } }],
    combine: 'AND'
  },

  thresholdHook: {
    id: generateTestId('threshold'),
    name: 'Threshold Hook',
    select: 'SELECT ?s ?value WHERE { ?s <http://example.org/value> ?value }',
    predicates: [
      {
        kind: 'THRESHOLD',
        spec: {
          var: 'value',
          op: '>',
          value: 50
        }
      }
    ],
    combine: 'AND'
  },

  complexHook: {
    id: generateTestId('complex'),
    name: 'Complex Hook',
    select: `SELECT ?s ?score ?status WHERE {
      ?s a <http://example.org/Person> ;
         <http://example.org/score> ?score ;
         <http://example.org/status> ?status .
    }`,
    predicates: [
      {
        kind: 'THRESHOLD',
        spec: {
          var: 'score',
          op: '>',
          value: 80
        }
      },
      {
        kind: 'THRESHOLD',
        spec: {
          var: 'status',
          op: '=',
          value: 'active'
        }
      }
    ],
    combine: 'AND'
  }
}

/**
 * Performance test helpers
 */
export class PerformanceTester {
  constructor() {
    this.metrics = []
  }

  /**
   * Measure execution time of a function
   * @param {string} name - Test name
   * @param {Function} fn - Function to measure
   * @returns {Promise<number>} Execution time in milliseconds
   */
  async measure(name, fn) {
    const start = performance.now()
    const result = await fn()
    const duration = performance.now() - start

    this.metrics.push({ name, duration, timestamp: new Date().toISOString() })

    return duration
  }

  /**
   * Get performance summary
   * @returns {Object} Performance metrics summary
   */
  getSummary() {
    if (this.metrics.length === 0) return null

    const durations = this.metrics.map(m => m.duration)

    return {
      totalTests: this.metrics.length,
      averageTime: durations.reduce((a, b) => a + b, 0) / durations.length,
      minTime: Math.min(...durations),
      maxTime: Math.max(...durations),
      medianTime: durations.sort((a, b) => a - b)[Math.floor(durations.length / 2)],
      tests: this.metrics
    }
  }
}

/**
 * Load testing helpers
 */
export class LoadTester {
  /**
   * Run concurrent requests
   * @param {Function} requestFn - Function that makes a request
   * @param {number} concurrency - Number of concurrent requests
   * @param {number} total - Total number of requests
   * @returns {Promise<Object>} Load test results
   */
  static async runConcurrent(requestFn, concurrency = 10, total = 100) {
    const results = []
    const errors = []

    const executeBatch = async (batchSize) => {
      const promises = Array.from({ length: batchSize }, () =>
        requestFn().catch(error => ({ error: error.message }))
      )

      const batchResults = await Promise.all(promises)

      for (const result of batchResults) {
        if (result.error) {
          errors.push(result.error)
        } else {
          results.push(result)
        }
      }
    }

    const batches = Math.ceil(total / concurrency)

    for (let i = 0; i < batches; i++) {
      const batchSize = Math.min(concurrency, total - i * concurrency)
      await executeBatch(batchSize)
    }

    return {
      total,
      successful: results.length,
      failed: errors.length,
      successRate: (results.length / total) * 100,
      errors: errors.slice(0, 10) // Return first 10 errors
    }
  }
}
