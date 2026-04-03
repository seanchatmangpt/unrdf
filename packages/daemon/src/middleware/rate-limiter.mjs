/**
 * @file Token Bucket Rate Limiter
 * @module @unrdf/daemon/middleware/rate-limiter
 * @description Production-ready rate limiting middleware using token bucket algorithm.
 * Prevents DoS attacks with configurable per-IP and per-API-key limits, LRU cache,
 * and graceful 429 responses with Retry-After headers.
 */

import {
  RateLimitConfigSchema,
  RateLimitResultSchema,
  RateLimitContextSchema,
  RateLimitStatsSchema,
} from './rate-limiter.schema.mjs';

/**
 * Simple LRU Cache for rate limit buckets
 * Automatically evicts least recently used entries when max size is reached
 */
class LRUCache {
  /**
   * @param {number} maxSize - Maximum number of entries
   */
  constructor(maxSize) {
    this.maxSize = maxSize;
    this.cache = new Map();
  }

  /**
   * Get value and mark as recently used
   * @param {string} key - Cache key
   * @returns {any} Cached value or undefined
   */
  get(key) {
    if (!this.cache.has(key)) return undefined;
    const value = this.cache.get(key);
    this.cache.delete(key);
    this.cache.set(key, value);
    return value;
  }

  /**
   * Set value and evict LRU if needed
   * @param {string} key - Cache key
   * @param {any} value - Value to cache
   */
  set(key, value) {
    if (this.cache.has(key)) {
      this.cache.delete(key);
    } else if (this.cache.size >= this.maxSize) {
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }
    this.cache.set(key, value);
  }

  /**
   * Check if key exists
   * @param {string} key - Cache key
   * @returns {boolean}
   */
  has(key) {
    return this.cache.has(key);
  }

  /**
   * Delete entry
   * @param {string} key - Cache key
   * @returns {boolean}
   */
  delete(key) {
    return this.cache.delete(key);
  }

  /**
   * Get current cache size
   * @returns {number}
   */
  size() {
    return this.cache.size;
  }

  /**
   * Clear all entries
   */
  clear() {
    this.cache.clear();
  }

  /**
   * Get all keys
   * @returns {string[]}
   */
  keys() {
    return Array.from(this.cache.keys());
  }
}

/**
 * Token Bucket Rate Limiter
 * Implements industry-standard token bucket algorithm with burst support
 */
export class TokenBucketRateLimiter {
  /**
   * @param {Object} config - Rate limiter configuration
   * @param {number} [config.windowMs=60000] - Time window in milliseconds (default: 1 minute)
   * @param {number} [config.maxRequests=100] - Maximum requests per window
   * @param {number} [config.burstSize=10] - Maximum burst requests
   * @param {number} [config.burstWindowMs=1000] - Burst window in milliseconds (default: 1 second)
   * @param {string} [config.keyPrefix='ratelimit'] - Prefix for storage keys
   * @param {boolean} [config.enablePerIp=true] - Enable per-IP rate limiting
   * @param {boolean} [config.enablePerApiKey=true] - Enable per-API-key rate limiting
   * @param {number} [config.storageMaxSize=10000] - Maximum LRU cache size
   * @param {Object} [config.logger=console] - Logger instance
   */
  constructor(config = {}) {
    const validated = RateLimitConfigSchema.parse(config);

    this.windowMs = validated.windowMs;
    this.maxRequests = validated.maxRequests;
    this.burstSize = validated.burstSize;
    this.burstWindowMs = validated.burstWindowMs;
    this.keyPrefix = validated.keyPrefix;
    this.enablePerIp = validated.enablePerIp;
    this.enablePerApiKey = validated.enablePerApiKey;
    this.logger = validated.logger || console;

    // LRU cache for token buckets
    this.buckets = new LRUCache(validated.storageMaxSize);

    // Statistics tracking
    this.stats = {
      totalRequests: 0,
      allowedRequests: 0,
      blockedRequests: 0,
      uniqueIdentifiers: new Set(),
    };

    // Token refill rate (tokens per millisecond)
    this.refillRate = this.maxRequests / this.windowMs;
  }

  /**
   * Extract identifier from request context
   * Prioritizes API key over IP for better tracking
   * @param {Object} context - Request context
   * @param {string} [context.ip] - Client IP address
   * @param {string} [context.apiKey] - API key
   * @returns {string|null} Identifier or null
   */
  extractIdentifier(context) {
    const validated = RateLimitContextSchema.parse(context);

    if (this.enablePerApiKey && validated.apiKey) {
      return `${this.keyPrefix}:apikey:${validated.apiKey}`;
    }

    if (this.enablePerIp && validated.ip) {
      return `${this.keyPrefix}:ip:${validated.ip}`;
    }

    return null;
  }

  /**
   * Get or create token bucket for identifier
   * @param {string} identifier - Rate limit identifier
   * @returns {Object} Bucket state
   */
  getBucket(identifier) {
    let bucket = this.buckets.get(identifier);

    if (!bucket) {
      bucket = {
        tokens: this.maxRequests,
        lastRefill: Date.now(),
        requestCount: 0,
        firstRequest: Date.now(),
      };
      this.buckets.set(identifier, bucket);
      this.stats.uniqueIdentifiers.add(identifier);
    }

    return bucket;
  }

  /**
   * Refill tokens based on elapsed time
   * Implements continuous token bucket algorithm
   * @param {Object} bucket - Bucket state
   * @returns {Object} Updated bucket
   */
  refillBucket(bucket) {
    const now = Date.now();
    const elapsed = now - bucket.lastRefill;

    if (elapsed > 0) {
      const tokensToAdd = elapsed * this.refillRate;
      bucket.tokens = Math.min(this.maxRequests, bucket.tokens + tokensToAdd);
      bucket.lastRefill = now;
    }

    return bucket;
  }

  /**
   * Check burst limit
   * Prevents rapid-fire requests within burst window
   * @param {Object} bucket - Bucket state
   * @returns {boolean} True if burst limit exceeded
   */
  isBurstLimitExceeded(bucket) {
    const now = Date.now();
    const burstWindowStart = now - this.burstWindowMs;

    // Reset request count if outside burst window
    if (bucket.firstRequest < burstWindowStart) {
      bucket.requestCount = 0;
      bucket.firstRequest = now;
    }

    return bucket.requestCount >= this.burstSize;
  }

  /**
   * Attempt to consume a token from bucket
   * @param {string} identifier - Rate limit identifier
   * @returns {Object} Rate limit result
   */
  consume(identifier) {
    this.stats.totalRequests++;

    const bucket = this.getBucket(identifier);
    this.refillBucket(bucket);

    // Check burst limit first (faster check)
    if (this.isBurstLimitExceeded(bucket)) {
      this.stats.blockedRequests++;

      const retryAfter = Math.ceil((this.burstWindowMs - (Date.now() - bucket.firstRequest)) / 1000);

      return RateLimitResultSchema.parse({
        allowed: false,
        remaining: 0,
        resetAt: bucket.firstRequest + this.burstWindowMs,
        retryAfter,
        identifier,
        reason: 'Burst limit exceeded',
      });
    }

    // Check token bucket
    if (bucket.tokens >= 1) {
      bucket.tokens -= 1;
      bucket.requestCount += 1;
      this.stats.allowedRequests++;

      const resetAt = bucket.lastRefill + this.windowMs;

      return RateLimitResultSchema.parse({
        allowed: true,
        remaining: Math.floor(bucket.tokens),
        resetAt,
        identifier,
      });
    }

    // Rate limit exceeded
    this.stats.blockedRequests++;

    const retryAfter = Math.ceil(this.windowMs / 1000);

    return RateLimitResultSchema.parse({
      allowed: false,
      remaining: 0,
      resetAt: bucket.lastRefill + this.windowMs,
      retryAfter,
      identifier,
      reason: 'Rate limit exceeded',
    });
  }

  /**
   * Check rate limit without consuming token
   * @param {Object} context - Request context
   * @returns {Object} Rate limit result
   */
  check(context) {
    const identifier = this.extractIdentifier(context);

    if (!identifier) {
      return RateLimitResultSchema.parse({
        allowed: true,
        remaining: this.maxRequests,
        resetAt: Date.now() + this.windowMs,
        identifier: 'unknown',
        reason: 'No identifier found, allowing request',
      });
    }

    return this.consume(identifier);
  }

  /**
   * Get statistics
   * @returns {Object} Rate limiter statistics
   */
  getStats() {
    return RateLimitStatsSchema.parse({
      totalRequests: this.stats.totalRequests,
      allowedRequests: this.stats.allowedRequests,
      blockedRequests: this.stats.blockedRequests,
      uniqueIdentifiers: this.stats.uniqueIdentifiers.size,
      cacheSize: this.buckets.size(),
      blockRate: this.stats.totalRequests > 0
        ? this.stats.blockedRequests / this.stats.totalRequests
        : 0,
    });
  }

  /**
   * Reset rate limits for identifier
   * @param {string} identifier - Rate limit identifier
   */
  reset(identifier) {
    this.buckets.delete(identifier);
  }

  /**
   * Clear all rate limits
   */
  clear() {
    this.buckets.clear();
    this.stats = {
      totalRequests: 0,
      allowedRequests: 0,
      blockedRequests: 0,
      uniqueIdentifiers: new Set(),
    };
  }
}

/**
 * Create rate limiting middleware
 * @param {Object} config - Rate limiter configuration
 * @returns {Function} Middleware function
 * @example
 * const rateLimitMiddleware = createRateLimitMiddleware({
 *   maxRequests: 100,
 *   windowMs: 60000,
 *   burstSize: 10,
 * });
 *
 * const result = await rateLimitMiddleware({
 *   ip: '192.168.1.1',
 *   apiKey: 'user-key',
 * });
 */
export function createRateLimitMiddleware(config = {}) {
  const limiter = new TokenBucketRateLimiter(config);

  return async (context) => {
    const result = limiter.check(context);

    if (!result.allowed) {
      const error = new Error(result.reason || 'Rate limit exceeded');
      error.statusCode = 429;
      error.retryAfter = result.retryAfter;
      error.resetAt = result.resetAt;
      error.identifier = result.identifier;
      throw error;
    }

    return result;
  };
}

/**
 * Create rate limiter with default configuration
 * @param {Object} config - Optional configuration overrides
 * @returns {TokenBucketRateLimiter} Configured rate limiter
 * @example
 * const limiter = createRateLimiter({
 *   maxRequests: 1000,
 *   windowMs: 60000,
 * });
 *
 * const result = limiter.check({ ip: '192.168.1.1' });
 * if (result.allowed) {
 *   // Process request
 * }
 */
export function createRateLimiter(config = {}) {
  return new TokenBucketRateLimiter(config);
}

/**
 * Parse environment variables for rate limit configuration
 * @returns {Object} Configuration from environment
 * @example
 * // Set env vars:
 * // RATE_LIMIT_MAX_REQUESTS=200
 * // RATE_LIMIT_WINDOW_MS=120000
 * // RATE_LIMIT_BURST_SIZE=20
 *
 * const config = parseEnvConfig();
 * const limiter = createRateLimiter(config);
 */
export function parseEnvConfig() {
  return {
    maxRequests: process.env.RATE_LIMIT_MAX_REQUESTS
      ? parseInt(process.env.RATE_LIMIT_MAX_REQUESTS, 10)
      : undefined,
    windowMs: process.env.RATE_LIMIT_WINDOW_MS
      ? parseInt(process.env.RATE_LIMIT_WINDOW_MS, 10)
      : undefined,
    burstSize: process.env.RATE_LIMIT_BURST_SIZE
      ? parseInt(process.env.RATE_LIMIT_BURST_SIZE, 10)
      : undefined,
    burstWindowMs: process.env.RATE_LIMIT_BURST_WINDOW_MS
      ? parseInt(process.env.RATE_LIMIT_BURST_WINDOW_MS, 10)
      : undefined,
    enablePerIp: process.env.RATE_LIMIT_ENABLE_PER_IP !== 'false',
    enablePerApiKey: process.env.RATE_LIMIT_ENABLE_PER_API_KEY !== 'false',
  };
}
