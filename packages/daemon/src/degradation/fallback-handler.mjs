/**
 * @file Fallback Handler Implementation
 * @module @unrdf/daemon/degradation/fallback-handler
 * @description Comprehensive fallback strategies including primary/fallback execution,
 * timeout-based fallback, cache-based fallback, default values, and retry with exponential backoff.
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import {
  FallbackConfigSchema,
  FallbackResultSchema,
  FallbackMetricsSchema,
  RetryConfigSchema,
  CacheEntrySchema,
} from './degradation.schema.mjs';

const tracer = trace.getTracer('@unrdf/daemon/fallback-handler');

/**
 * Fallback source types
 * @readonly
 * @enum {string}
 */
export const FallbackSource = {
  /** Result from primary function */
  PRIMARY: 'primary',
  /** Result from timeout fallback */
  TIMEOUT: 'timeout',
  /** Result from cache */
  CACHE: 'cache',
  /** Result from default value */
  DEFAULT: 'default',
  /** Result from custom fallback function */
  CUSTOM: 'custom',
};

/**
 * LRU Cache for fallback values
 * @private
 */
class LRUCache {
  /**
   * @param {number} maxSize - Maximum cache entries
   */
  constructor(maxSize) {
    this.maxSize = maxSize;
    this.cache = new Map();
    this.stats = { hits: 0, misses: 0 };
  }

  /**
   * Get value from cache
   * @param {string} key - Cache key
   * @returns {any|undefined}
   */
  get(key) {
    const entry = this.cache.get(key);
    if (!entry) {
      this.stats.misses++;
      return undefined;
    }

    if (Date.now() > entry.timestamp + entry.ttl) {
      this.cache.delete(key);
      this.stats.misses++;
      return undefined;
    }

    this.cache.delete(key);
    entry.hits++;
    this.cache.set(key, entry);
    this.stats.hits++;

    return entry.value;
  }

  /**
   * Set value in cache
   * @param {string} key - Cache key
   * @param {any} value - Value to cache
   * @param {number} ttl - Time to live in ms
   */
  set(key, value, ttl) {
    if (this.cache.has(key)) {
      this.cache.delete(key);
    } else if (this.cache.size >= this.maxSize) {
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }

    this.cache.set(key, CacheEntrySchema.parse({
      value,
      timestamp: Date.now(),
      ttl,
      hits: 0,
    }));
  }

  /**
   * Check if key exists and is valid
   * @param {string} key - Cache key
   * @returns {boolean}
   */
  has(key) {
    const entry = this.cache.get(key);
    if (!entry) return false;
    if (Date.now() > entry.timestamp + entry.ttl) {
      this.cache.delete(key);
      return false;
    }
    return true;
  }

  /**
   * Delete entry
   * @param {string} key - Cache key
   */
  delete(key) {
    this.cache.delete(key);
  }

  /**
   * Clear all entries
   */
  clear() {
    this.cache.clear();
    this.stats = { hits: 0, misses: 0 };
  }

  /**
   * Get cache statistics
   * @returns {Object}
   */
  getStats() {
    const total = this.stats.hits + this.stats.misses;
    return {
      size: this.cache.size,
      maxSize: this.maxSize,
      hits: this.stats.hits,
      misses: this.stats.misses,
      hitRate: total > 0 ? this.stats.hits / total : 0,
    };
  }
}

/**
 * Calculate exponential backoff delay
 * @param {number} attempt - Current attempt number (0-indexed)
 * @param {Object} config - Retry configuration
 * @returns {number} Delay in milliseconds
 */
function calculateBackoffDelay(attempt, config) {
  const delay = Math.min(
    config.initialDelay * Math.pow(config.multiplier, attempt),
    config.maxDelay
  );

  if (config.jitter) {
    const jitterRange = delay * 0.2;
    return delay + (Math.random() * jitterRange * 2 - jitterRange);
  }

  return delay;
}

/**
 * Sleep for specified duration
 * @param {number} ms - Milliseconds to sleep
 * @returns {Promise<void>}
 */
function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

/**
 * Fallback Handler Implementation
 * Provides graceful degradation through multiple fallback strategies
 * @example
 * const handler = new FallbackHandler({
 *   name: 'api-call',
 *   timeout: 5000,
 *   cacheEnabled: true,
 *   defaultValue: { data: [] }
 * });
 *
 * const result = await handler.execute(
 *   async () => await fetch('/api/data'),
 *   'api-data-key'
 * );
 */
export class FallbackHandler {
  /**
   * Create a fallback handler
   * @param {Object} config - Handler configuration
   * @param {string} [config.name='default'] - Handler name
   * @param {number} [config.timeout=5000] - Primary execution timeout
   * @param {boolean} [config.cacheEnabled=false] - Enable cache fallback
   * @param {number} [config.cacheTtl=60000] - Cache TTL in ms
   * @param {number} [config.cacheMaxSize=1000] - Max cache entries
   * @param {any} [config.defaultValue] - Default fallback value
   * @param {Object} [config.retry] - Retry configuration
   * @param {Function[]} [config.fallbacks=[]] - Custom fallback functions
   * @param {Function} [config.onFallback] - Callback when fallback used
   * @param {Function} [config.onSuccess] - Callback on success
   * @param {Function} [config.onFailure] - Callback on failure
   */
  constructor(config = {}) {
    const validated = FallbackConfigSchema.parse(config);

    this.name = validated.name;
    this.timeout = validated.timeout;
    this.cacheEnabled = validated.cacheEnabled;
    this.cacheTtl = validated.cacheTtl;
    this.defaultValue = validated.defaultValue;
    this.fallbacks = validated.fallbacks;

    this.onFallback = validated.onFallback;
    this.onSuccess = validated.onSuccess;
    this.onFailure = validated.onFailure;

    this._retryConfig = validated.retry
      ? RetryConfigSchema.parse(validated.retry)
      : null;

    this._cache = this.cacheEnabled
      ? new LRUCache(validated.cacheMaxSize)
      : null;

    this._metrics = {
      totalCalls: 0,
      primarySuccesses: 0,
      timeoutFallbacks: 0,
      cacheFallbacks: 0,
      defaultFallbacks: 0,
      customFallbacks: 0,
      totalRetries: 0,
      totalFailures: 0,
      totalLatency: 0,
    };

    this._eventListeners = new Map();
  }

  /**
   * Execute with fallback support
   * @param {Function} primaryFn - Primary async function
   * @param {string} [cacheKey] - Optional cache key
   * @param {Object} [context] - Optional execution context
   * @returns {Promise<Object>} Execution result
   * @example
   * const result = await handler.execute(
   *   async () => await api.getData(),
   *   'data-cache-key'
   * );
   * if (result.fromCache) {
   *   console.log('Result served from cache');
   * }
   */
  async execute(primaryFn, cacheKey, context = {}) {
    const span = tracer.startSpan('fallback_handler.execute', {
      attributes: {
        'fallback.name': this.name,
        'fallback.cache_enabled': this.cacheEnabled,
        'fallback.cache_key': cacheKey || 'none',
      },
    });

    const startTime = Date.now();
    this._metrics.totalCalls++;
    let retries = 0;

    try {
      const result = await this._executeWithRetry(primaryFn, cacheKey, context, span);

      if (result.success && result.source === FallbackSource.PRIMARY) {
        this._metrics.primarySuccesses++;

        if (this.cacheEnabled && cacheKey && result.result !== undefined) {
          this._cache.set(cacheKey, result.result, this.cacheTtl);
        }

        if (this.onSuccess) {
          this.onSuccess(result.result);
        }
      }

      result.duration = Date.now() - startTime;
      result.retries = retries;
      this._metrics.totalLatency += result.duration;

      span.setAttribute('fallback.source', result.source);
      span.setAttribute('fallback.success', result.success);
      span.setAttribute('fallback.duration', result.duration);
      span.setStatus({ code: SpanStatusCode.OK });

      this._emit('executed', {
        source: result.source,
        duration: result.duration,
        success: result.success,
      });

      return FallbackResultSchema.parse(result);
    } catch (error) {
      const fallbackResult = await this._handleFallback(error, cacheKey, context, span);
      fallbackResult.duration = Date.now() - startTime;
      fallbackResult.retries = retries;
      this._metrics.totalLatency += fallbackResult.duration;

      span.end();
      return FallbackResultSchema.parse(fallbackResult);
    } finally {
      span.end();
    }
  }

  /**
   * Execute with retry support
   * @private
   */
  async _executeWithRetry(primaryFn, cacheKey, context, span) {
    let lastError;
    const maxAttempts = this._retryConfig ? this._retryConfig.maxRetries + 1 : 1;

    for (let attempt = 0; attempt < maxAttempts; attempt++) {
      try {
        if (attempt > 0) {
          this._metrics.totalRetries++;
          span.addEvent('retry', { attempt });

          if (this._retryConfig.onRetry) {
            this._retryConfig.onRetry(attempt, lastError);
          }

          const delay = calculateBackoffDelay(attempt - 1, this._retryConfig);
          await sleep(delay);
        }

        const result = await this._executeWithTimeout(primaryFn, context);

        return {
          success: true,
          result,
          source: FallbackSource.PRIMARY,
          duration: 0,
          retries: attempt,
          fromCache: false,
        };
      } catch (error) {
        lastError = error;

        if (this._retryConfig && attempt < maxAttempts - 1) {
          const shouldRetry = this._shouldRetry(error);
          if (!shouldRetry) {
            break;
          }
        }
      }
    }

    throw lastError;
  }

  /**
   * Check if error is retryable
   * @private
   */
  _shouldRetry(error) {
    if (!this._retryConfig) return false;

    const errorName = error.name || error.constructor.name;
    return this._retryConfig.retryOn.some(pattern => {
      if (pattern === errorName) return true;
      if (error.message && error.message.includes(pattern)) return true;
      return false;
    });
  }

  /**
   * Execute with timeout
   * @private
   */
  async _executeWithTimeout(fn, context) {
    return new Promise((resolve, reject) => {
      const timeoutId = setTimeout(() => {
        this._metrics.timeoutFallbacks++;
        reject(new Error(`Execution timeout after ${this.timeout}ms`));
      }, this.timeout);

      Promise.resolve(fn(context))
        .then(result => {
          clearTimeout(timeoutId);
          resolve(result);
        })
        .catch(error => {
          clearTimeout(timeoutId);
          reject(error);
        });
    });
  }

  /**
   * Handle fallback chain
   * @private
   */
  async _handleFallback(error, cacheKey, context, span) {
    span.addEvent('fallback_triggered', { error: error.message });

    if (this.cacheEnabled && cacheKey && this._cache.has(cacheKey)) {
      this._metrics.cacheFallbacks++;
      const cachedValue = this._cache.get(cacheKey);

      if (this.onFallback) {
        this.onFallback(FallbackSource.CACHE, error);
      }

      this._emit('fallback', { source: FallbackSource.CACHE, error });

      return {
        success: true,
        result: cachedValue,
        source: FallbackSource.CACHE,
        duration: 0,
        retries: 0,
        fromCache: true,
      };
    }

    for (let i = 0; i < this.fallbacks.length; i++) {
      try {
        const fallbackFn = this.fallbacks[i];
        const result = await fallbackFn(error, context);
        this._metrics.customFallbacks++;

        if (this.onFallback) {
          this.onFallback(FallbackSource.CUSTOM, error);
        }

        this._emit('fallback', { source: FallbackSource.CUSTOM, index: i, error });

        return {
          success: true,
          result,
          source: FallbackSource.CUSTOM,
          duration: 0,
          retries: 0,
          fromCache: false,
        };
      } catch {
        continue;
      }
    }

    if (this.defaultValue !== undefined) {
      this._metrics.defaultFallbacks++;

      if (this.onFallback) {
        this.onFallback(FallbackSource.DEFAULT, error);
      }

      this._emit('fallback', { source: FallbackSource.DEFAULT, error });

      return {
        success: true,
        result: this.defaultValue,
        source: FallbackSource.DEFAULT,
        duration: 0,
        retries: 0,
        fromCache: false,
      };
    }

    this._metrics.totalFailures++;

    if (this.onFailure) {
      this.onFailure(error);
    }

    this._emit('failure', { error });

    return {
      success: false,
      error,
      source: FallbackSource.PRIMARY,
      duration: 0,
      retries: 0,
      fromCache: false,
    };
  }

  /**
   * Get handler metrics
   * @returns {Object} Metrics snapshot
   */
  getMetrics() {
    const total = this._metrics.totalCalls;
    const cacheStats = this._cache ? this._cache.getStats() : { hitRate: 0 };

    return FallbackMetricsSchema.parse({
      totalCalls: this._metrics.totalCalls,
      primarySuccesses: this._metrics.primarySuccesses,
      timeoutFallbacks: this._metrics.timeoutFallbacks,
      cacheFallbacks: this._metrics.cacheFallbacks,
      defaultFallbacks: this._metrics.defaultFallbacks,
      customFallbacks: this._metrics.customFallbacks,
      totalRetries: this._metrics.totalRetries,
      totalFailures: this._metrics.totalFailures,
      cacheHitRate: cacheStats.hitRate,
      averageLatency: total > 0 ? this._metrics.totalLatency / total : 0,
    });
  }

  /**
   * Clear cache
   */
  clearCache() {
    if (this._cache) {
      this._cache.clear();
    }
    this._emit('cacheCleared', {});
  }

  /**
   * Reset metrics
   */
  reset() {
    this._metrics = {
      totalCalls: 0,
      primarySuccesses: 0,
      timeoutFallbacks: 0,
      cacheFallbacks: 0,
      defaultFallbacks: 0,
      customFallbacks: 0,
      totalRetries: 0,
      totalFailures: 0,
      totalLatency: 0,
    };

    if (this._cache) {
      this._cache.clear();
    }

    this._emit('reset', {});
  }

  /**
   * Add event listener
   * @param {string} event - Event name
   * @param {Function} callback - Event callback
   * @returns {Function} Unsubscribe function
   */
  on(event, callback) {
    if (!this._eventListeners.has(event)) {
      this._eventListeners.set(event, new Set());
    }
    this._eventListeners.get(event).add(callback);

    return () => {
      this._eventListeners.get(event)?.delete(callback);
    };
  }

  /**
   * Emit event
   * @private
   */
  _emit(event, data) {
    const listeners = this._eventListeners.get(event);
    if (listeners) {
      for (const callback of listeners) {
        try {
          callback({ ...data, handler: this.name, timestamp: Date.now() });
        } catch {
          // Ignore listener errors
        }
      }
    }
  }
}

/**
 * Create a fallback handler with default configuration
 * @param {Object} config - Configuration overrides
 * @returns {FallbackHandler}
 * @example
 * const handler = createFallbackHandler({
 *   name: 'api',
 *   timeout: 3000,
 *   cacheEnabled: true
 * });
 */
export function createFallbackHandler(config = {}) {
  return new FallbackHandler(config);
}

/**
 * Execute function with simple retry
 * @param {Function} fn - Function to execute
 * @param {Object} [options] - Retry options
 * @returns {Promise<any>}
 * @example
 * const result = await withRetry(
 *   async () => await api.getData(),
 *   { maxRetries: 3, initialDelay: 100 }
 * );
 */
export async function withRetry(fn, options = {}) {
  const config = RetryConfigSchema.parse(options);
  let lastError;

  for (let attempt = 0; attempt <= config.maxRetries; attempt++) {
    try {
      if (attempt > 0) {
        const delay = calculateBackoffDelay(attempt - 1, config);
        await sleep(delay);

        if (config.onRetry) {
          config.onRetry(attempt, lastError);
        }
      }

      return await fn();
    } catch (error) {
      lastError = error;
    }
  }

  throw lastError;
}

/**
 * Execute function with timeout
 * @param {Function} fn - Function to execute
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<any>}
 * @example
 * const result = await withTimeout(
 *   async () => await api.getData(),
 *   5000
 * );
 */
export async function withTimeout(fn, timeout) {
  return new Promise((resolve, reject) => {
    const timeoutId = setTimeout(() => {
      reject(new Error(`Execution timeout after ${timeout}ms`));
    }, timeout);

    Promise.resolve(fn())
      .then(result => {
        clearTimeout(timeoutId);
        resolve(result);
      })
      .catch(error => {
        clearTimeout(timeoutId);
        reject(error);
      });
  });
}

/**
 * Execute function with fallback value
 * @param {Function} fn - Function to execute
 * @param {any} fallbackValue - Value to return on failure
 * @returns {Promise<any>}
 * @example
 * const result = await withFallback(
 *   async () => await api.getData(),
 *   { data: [] }
 * );
 */
export async function withFallback(fn, fallbackValue) {
  try {
    return await fn();
  } catch {
    return fallbackValue;
  }
}

/**
 * Fallback handler registry
 */
export class FallbackHandlerRegistry {
  constructor() {
    this._handlers = new Map();
  }

  /**
   * Get or create a handler
   * @param {string} name - Handler name
   * @param {Object} [config] - Configuration if creating new
   * @returns {FallbackHandler}
   */
  get(name, config = {}) {
    if (!this._handlers.has(name)) {
      this._handlers.set(name, new FallbackHandler({ ...config, name }));
    }
    return this._handlers.get(name);
  }

  /**
   * Remove a handler
   * @param {string} name - Handler name
   */
  remove(name) {
    this._handlers.delete(name);
  }

  /**
   * Get all handlers
   * @returns {Map<string, FallbackHandler>}
   */
  getAll() {
    return new Map(this._handlers);
  }

  /**
   * Get metrics for all handlers
   * @returns {Object}
   */
  getAllMetrics() {
    const metrics = {};
    for (const [name, handler] of this._handlers) {
      metrics[name] = handler.getMetrics();
    }
    return metrics;
  }
}

/** Global fallback handler registry */
export const globalFallbackRegistry = new FallbackHandlerRegistry();
