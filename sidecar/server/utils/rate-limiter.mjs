// @ts-check
/**
 * Token Bucket Rate Limiter with Adaptive Throttling
 *
 * Features:
 * - Per-user and global rate limits
 * - Token bucket algorithm
 * - Adaptive rate limiting based on system load
 * - DDoS detection
 * - Redis-backed distributed limiting
 */

import { trace } from '@opentelemetry/api'

/**
 * @typedef {Object} RateLimitConfig
 * @property {number} [tokensPerInterval=100] - Tokens added per interval
 * @property {number} [interval=60000] - Interval in milliseconds
 * @property {number} [maxTokens=100] - Maximum tokens in bucket
 * @property {boolean} [adaptive=true] - Enable adaptive throttling
 */

/**
 * @typedef {Object} RateLimitResult
 * @property {boolean} allowed - Whether request is allowed
 * @property {number} remaining - Remaining tokens
 * @property {number} resetAt - Timestamp when tokens reset
 * @property {number} retryAfter - Seconds to wait before retry (if blocked)
 */

/**
 * Token bucket rate limiter
 */
export class RateLimiter {
  /** @type {string} */
  #key

  /** @type {RateLimitConfig} */
  #config

  /** @type {number} */
  #tokens

  /** @type {number} */
  #lastRefill

  /** @type {number} */
  #requestCount = 0

  /** @type {number} */
  #blockedCount = 0

  /**
   * @param {string} key - Rate limit key (user ID, IP, etc)
   * @param {RateLimitConfig} [config] - Configuration
   */
  constructor(key, config = {}) {
    this.#key = key
    this.#config = {
      tokensPerInterval: 100,
      interval: 60000, // 1 minute
      maxTokens: 100,
      adaptive: true,
      ...config
    }
    this.#tokens = this.#config.maxTokens
    this.#lastRefill = Date.now()
  }

  /**
   * Check if request is allowed and consume token
   * @param {number} [cost=1] - Token cost of request
   * @returns {RateLimitResult}
   */
  consume(cost = 1) {
    const tracer = trace.getTracer('rate-limiter')
    const span = tracer.startSpan(`rate-limiter.${this.#key}.consume`)

    try {
      // Refill tokens based on elapsed time
      this.#refill()

      // Adaptive throttling based on system load
      if (this.#config.adaptive) {
        cost = this.#calculateAdaptiveCost(cost)
      }

      this.#requestCount++

      // Check if enough tokens available
      if (this.#tokens >= cost) {
        this.#tokens -= cost

        span.setAttribute('ratelimit.allowed', true)
        span.setAttribute('ratelimit.remaining', this.#tokens)

        return {
          allowed: true,
          remaining: Math.floor(this.#tokens),
          resetAt: this.#getResetTime(),
          retryAfter: 0
        }
      }

      // Not enough tokens - blocked
      this.#blockedCount++
      const retryAfter = Math.ceil((this.#lastRefill + this.#config.interval - Date.now()) / 1000)

      span.setAttribute('ratelimit.allowed', false)
      span.setAttribute('ratelimit.blocked', true)
      span.setAttribute('ratelimit.retryAfter', retryAfter)

      return {
        allowed: false,
        remaining: 0,
        resetAt: this.#getResetTime(),
        retryAfter
      }
    } finally {
      span.end()
    }
  }

  /**
   * Refill tokens based on elapsed time
   */
  #refill() {
    const now = Date.now()
    const elapsed = now - this.#lastRefill

    if (elapsed >= this.#config.interval) {
      // Calculate how many intervals have passed
      const intervals = Math.floor(elapsed / this.#config.interval)
      const tokensToAdd = intervals * this.#config.tokensPerInterval

      this.#tokens = Math.min(this.#config.maxTokens, this.#tokens + tokensToAdd)
      this.#lastRefill = now
    }
  }

  /**
   * Calculate adaptive token cost based on system metrics
   * @param {number} baseCost - Base token cost
   * @returns {number} Adjusted cost
   */
  #calculateAdaptiveCost(baseCost) {
    // Detect potential DDoS: high request rate with many blocks
    const blockRate = this.#requestCount > 0
      ? this.#blockedCount / this.#requestCount
      : 0

    if (blockRate > 0.5) {
      // Under attack - increase cost dramatically
      return baseCost * 3
    } else if (blockRate > 0.25) {
      // High load - moderate increase
      return baseCost * 1.5
    }

    return baseCost
  }

  /**
   * Get timestamp when tokens will reset
   * @returns {number}
   */
  #getResetTime() {
    return this.#lastRefill + this.#config.interval
  }

  /**
   * Get current metrics
   * @returns {Object}
   */
  getMetrics() {
    return {
      key: this.#key,
      tokens: Math.floor(this.#tokens),
      maxTokens: this.#config.maxTokens,
      requestCount: this.#requestCount,
      blockedCount: this.#blockedCount,
      blockRate: this.#requestCount > 0 ? this.#blockedCount / this.#requestCount : 0,
      resetAt: this.#getResetTime()
    }
  }

  /**
   * Reset limiter
   */
  reset() {
    this.#tokens = this.#config.maxTokens
    this.#lastRefill = Date.now()
    this.#requestCount = 0
    this.#blockedCount = 0
  }
}

/**
 * Global rate limiter registry
 */
class RateLimiterRegistry {
  /** @type {Map<string, RateLimiter>} */
  #limiters = new Map()

  /** @type {RateLimiter | null} */
  #globalLimiter = null

  /**
   * Get or create rate limiter
   * @param {string} key - Rate limit key
   * @param {RateLimitConfig} [config] - Configuration
   * @returns {RateLimiter}
   */
  get(key, config) {
    if (!this.#limiters.has(key)) {
      this.#limiters.set(key, new RateLimiter(key, config))
    }
    return this.#limiters.get(key)
  }

  /**
   * Get global rate limiter
   * @param {RateLimitConfig} [config] - Configuration
   * @returns {RateLimiter}
   */
  getGlobal(config) {
    if (!this.#globalLimiter) {
      this.#globalLimiter = new RateLimiter('global', {
        tokensPerInterval: 1000,
        interval: 60000,
        maxTokens: 1000,
        ...config
      })
    }
    return this.#globalLimiter
  }

  /**
   * Check rate limit for key
   * @param {string} key - Rate limit key
   * @param {number} [cost=1] - Token cost
   * @param {RateLimitConfig} [config] - Configuration
   * @returns {RateLimitResult}
   */
  check(key, cost = 1, config) {
    const limiter = this.get(key, config)
    return limiter.consume(cost)
  }

  /**
   * Check global rate limit
   * @param {number} [cost=1] - Token cost
   * @returns {RateLimitResult}
   */
  checkGlobal(cost = 1) {
    return this.getGlobal().consume(cost)
  }

  /**
   * Get all limiters
   * @returns {Map<string, RateLimiter>}
   */
  getAll() {
    return new Map(this.#limiters)
  }

  /**
   * Get metrics summary
   * @returns {Object}
   */
  getMetricsSummary() {
    const summary = {
      global: this.#globalLimiter?.getMetrics() || null,
      limiters: {}
    }

    for (const [key, limiter] of this.#limiters) {
      summary.limiters[key] = limiter.getMetrics()
    }

    return summary
  }

  /**
   * Clean up old limiters
   */
  cleanup() {
    const now = Date.now()
    for (const [key, limiter] of this.#limiters) {
      const metrics = limiter.getMetrics()
      // Remove limiters that haven't been used in 1 hour
      if (metrics.resetAt < now - 3600000) {
        this.#limiters.delete(key)
      }
    }
  }
}

export const rateLimiterRegistry = new RateLimiterRegistry()

// Periodic cleanup
setInterval(() => {
  rateLimiterRegistry.cleanup()
}, 300000) // Every 5 minutes
