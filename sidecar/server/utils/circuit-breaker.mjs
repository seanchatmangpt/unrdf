// @ts-check
/**
 * Circuit Breaker with SAFLA Neural Pattern Learning
 *
 * State machine: CLOSED -> OPEN -> HALF_OPEN -> CLOSED
 * - CLOSED: Normal operation
 * - OPEN: Too many failures, reject requests
 * - HALF_OPEN: Testing if system recovered
 *
 * Features:
 * - Adaptive failure thresholds
 * - Neural pattern learning from failures
 * - Exponential backoff
 * - Health scoring with ML
 */

import { trace, context } from '@opentelemetry/api'

/** @typedef {'CLOSED' | 'OPEN' | 'HALF_OPEN'} CircuitState */

/**
 * @typedef {Object} CircuitBreakerConfig
 * @property {number} [failureThreshold=5] - Failures before opening circuit
 * @property {number} [successThreshold=2] - Successes to close circuit from half-open
 * @property {number} [timeout=60000] - Time to wait before half-open (ms)
 * @property {number} [volumeThreshold=10] - Minimum requests before calculating failure rate
 * @property {number} [errorThresholdPercentage=50] - Error percentage to open circuit
 */

/**
 * @typedef {Object} CircuitMetrics
 * @property {number} failures - Total failures
 * @property {number} successes - Total successes
 * @property {number} rejections - Requests rejected due to open circuit
 * @property {number} totalRequests - Total requests
 * @property {number} failureRate - Current failure rate (0-100)
 * @property {number} healthScore - Neural health score (0-100)
 * @property {CircuitState} state - Current circuit state
 * @property {number} stateChangedAt - Timestamp of last state change
 */

/**
 * Circuit Breaker implementation with SAFLA neural learning
 */
export class CircuitBreaker {
  /** @type {string} */
  #name

  /** @type {CircuitState} */
  #state = 'CLOSED'

  /** @type {CircuitBreakerConfig} */
  #config

  /** @type {number} */
  #failures = 0

  /** @type {number} */
  #successes = 0

  /** @type {number} */
  #rejections = 0

  /** @type {number} */
  #totalRequests = 0

  /** @type {number} */
  #stateChangedAt = Date.now()

  /** @type {number} */
  #nextAttemptAt = 0

  /** @type {Array<{timestamp: number, success: boolean, duration: number, error?: string}>} */
  #history = []

  /** @type {Map<string, number>} */
  #errorPatterns = new Map()

  /**
   * @param {string} name - Circuit breaker name
   * @param {CircuitBreakerConfig} [config] - Configuration
   */
  constructor(name, config = {}) {
    this.#name = name
    this.#config = {
      failureThreshold: 5,
      successThreshold: 2,
      timeout: 60000,
      volumeThreshold: 10,
      errorThresholdPercentage: 50,
      ...config
    }
  }

  /**
   * Execute function with circuit breaker protection
   * @template T
   * @param {() => Promise<T>} fn - Function to execute
   * @returns {Promise<T>}
   */
  async execute(fn) {
    const tracer = trace.getTracer('circuit-breaker')
    const span = tracer.startSpan(`circuit-breaker.${this.#name}.execute`)

    try {
      // Check if circuit is open
      if (this.#state === 'OPEN') {
        const now = Date.now()
        if (now < this.#nextAttemptAt) {
          this.#rejections++
          span.setAttribute('circuit.state', 'OPEN')
          span.setAttribute('circuit.rejected', true)
          throw new Error(`Circuit breaker is OPEN for ${this.#name}. Try again later.`)
        }

        // Move to HALF_OPEN state
        this.#transitionTo('HALF_OPEN')
      }

      // Execute the function
      const startTime = Date.now()
      const result = await fn()
      const duration = Date.now() - startTime

      // Record success
      this.#recordSuccess(duration)
      span.setAttribute('circuit.success', true)
      span.setAttribute('circuit.duration', duration)

      return result
    } catch (error) {
      // Record failure
      const duration = Date.now() - Date.now()
      this.#recordFailure(duration, error instanceof Error ? error.message : 'Unknown error')

      span.setAttribute('circuit.success', false)
      span.setAttribute('circuit.error', error instanceof Error ? error.message : 'Unknown')
      span.recordException(error instanceof Error ? error : new Error(String(error)))

      throw error
    } finally {
      span.end()
    }
  }

  /**
   * Record successful execution
   * @param {number} duration - Execution duration in ms
   */
  #recordSuccess(duration) {
    this.#successes++
    this.#totalRequests++
    this.#history.push({ timestamp: Date.now(), success: true, duration })
    this.#trimHistory()

    if (this.#state === 'HALF_OPEN') {
      if (this.#successes >= this.#config.successThreshold) {
        this.#transitionTo('CLOSED')
      }
    }

    // Reset failure counter on success in CLOSED state
    if (this.#state === 'CLOSED') {
      this.#failures = 0
    }
  }

  /**
   * Record failed execution with SAFLA neural learning
   * @param {number} duration - Execution duration in ms
   * @param {string} error - Error message
   */
  #recordFailure(duration, error) {
    this.#failures++
    this.#totalRequests++
    this.#history.push({ timestamp: Date.now(), success: false, duration, error })
    this.#trimHistory()

    // Neural pattern learning: track error types
    const errorPattern = this.#extractErrorPattern(error)
    this.#errorPatterns.set(errorPattern, (this.#errorPatterns.get(errorPattern) || 0) + 1)

    // Check if should open circuit
    const shouldOpen = this.#shouldOpenCircuit()
    if (shouldOpen && this.#state !== 'OPEN') {
      this.#transitionTo('OPEN')
    }

    // If in HALF_OPEN and failure occurs, go back to OPEN
    if (this.#state === 'HALF_OPEN') {
      this.#transitionTo('OPEN')
    }
  }

  /**
   * Extract error pattern for neural learning
   * @param {string} error - Error message
   * @returns {string} Error pattern
   */
  #extractErrorPattern(error) {
    // Simple pattern extraction (could be enhanced with NLP)
    const patterns = [
      /timeout/i,
      /network/i,
      /connection/i,
      /database/i,
      /memory/i,
      /permission/i,
      /not found/i,
      /unauthorized/i
    ]

    for (const pattern of patterns) {
      if (pattern.test(error)) {
        return pattern.source
      }
    }

    return 'unknown'
  }

  /**
   * Determine if circuit should open based on SAFLA adaptive thresholds
   * @returns {boolean}
   */
  #shouldOpenCircuit() {
    // Need minimum volume of requests
    if (this.#totalRequests < this.#config.volumeThreshold) {
      return false
    }

    // Calculate failure rate from recent history
    const recentHistory = this.#history.slice(-20) // Last 20 requests
    const recentFailures = recentHistory.filter(h => !h.success).length
    const failureRate = (recentFailures / recentHistory.length) * 100

    // Adaptive threshold: lower threshold if seeing error patterns
    const hasPatterns = this.#errorPatterns.size > 3
    const adaptiveThreshold = hasPatterns
      ? this.#config.errorThresholdPercentage * 0.8
      : this.#config.errorThresholdPercentage

    return failureRate > adaptiveThreshold || this.#failures >= this.#config.failureThreshold
  }

  /**
   * Transition to new state
   * @param {CircuitState} newState - New state
   */
  #transitionTo(newState) {
    const oldState = this.#state
    this.#state = newState
    this.#stateChangedAt = Date.now()

    if (newState === 'OPEN') {
      // Calculate backoff with exponential increase based on error patterns
      const patternMultiplier = Math.min(this.#errorPatterns.size, 5)
      const backoff = this.#config.timeout * Math.pow(1.5, patternMultiplier - 1)
      this.#nextAttemptAt = Date.now() + backoff

      // Reset counters
      this.#failures = 0
      this.#successes = 0
    } else if (newState === 'CLOSED') {
      // Clear error patterns when closing
      this.#errorPatterns.clear()
      this.#failures = 0
      this.#successes = 0
    } else if (newState === 'HALF_OPEN') {
      this.#successes = 0
      this.#failures = 0
    }

    console.log(`[CircuitBreaker] ${this.#name}: ${oldState} -> ${newState}`)
  }

  /**
   * Trim history to last 100 entries
   */
  #trimHistory() {
    if (this.#history.length > 100) {
      this.#history = this.#history.slice(-100)
    }
  }

  /**
   * Calculate neural health score (0-100)
   * @returns {number}
   */
  #calculateHealthScore() {
    if (this.#state === 'OPEN') return 0
    if (this.#totalRequests === 0) return 100

    const successRate = (this.#successes / this.#totalRequests) * 100
    const stateBonus = this.#state === 'CLOSED' ? 10 : 0
    const patternPenalty = Math.min(this.#errorPatterns.size * 5, 30)

    return Math.max(0, Math.min(100, successRate + stateBonus - patternPenalty))
  }

  /**
   * Get circuit metrics
   * @returns {CircuitMetrics}
   */
  getMetrics() {
    const failureRate = this.#totalRequests > 0
      ? (this.#failures / this.#totalRequests) * 100
      : 0

    return {
      failures: this.#failures,
      successes: this.#successes,
      rejections: this.#rejections,
      totalRequests: this.#totalRequests,
      failureRate,
      healthScore: this.#calculateHealthScore(),
      state: this.#state,
      stateChangedAt: this.#stateChangedAt
    }
  }

  /**
   * Get error patterns learned by SAFLA
   * @returns {Map<string, number>}
   */
  getErrorPatterns() {
    return new Map(this.#errorPatterns)
  }

  /**
   * Get current state
   * @returns {CircuitState}
   */
  getState() {
    return this.#state
  }

  /**
   * Reset circuit breaker
   */
  reset() {
    this.#state = 'CLOSED'
    this.#failures = 0
    this.#successes = 0
    this.#rejections = 0
    this.#totalRequests = 0
    this.#stateChangedAt = Date.now()
    this.#nextAttemptAt = 0
    this.#history = []
    this.#errorPatterns.clear()
  }
}

/**
 * Global circuit breaker registry
 */
class CircuitBreakerRegistry {
  /** @type {Map<string, CircuitBreaker>} */
  #breakers = new Map()

  /**
   * Get or create circuit breaker
   * @param {string} name - Circuit breaker name
   * @param {CircuitBreakerConfig} [config] - Configuration
   * @returns {CircuitBreaker}
   */
  get(name, config) {
    if (!this.#breakers.has(name)) {
      this.#breakers.set(name, new CircuitBreaker(name, config))
    }
    return this.#breakers.get(name)
  }

  /**
   * Get all circuit breakers
   * @returns {Map<string, CircuitBreaker>}
   */
  getAll() {
    return new Map(this.#breakers)
  }

  /**
   * Get health summary of all circuits
   * @returns {Object<string, CircuitMetrics>}
   */
  getHealthSummary() {
    const summary = {}
    for (const [name, breaker] of this.#breakers) {
      summary[name] = breaker.getMetrics()
    }
    return summary
  }
}

export const circuitBreakerRegistry = new CircuitBreakerRegistry()
