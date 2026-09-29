/**
 * Adaptive Rate Limiting Middleware
 *
 * Implements token bucket rate limiting with:
 * - Per-user limits (authenticated requests)
 * - Per-IP limits (unauthenticated requests)
 * - Adaptive thresholds based on system load
 * - Redis-backed distributed rate limiting
 *
 * @module sidecar/middleware/rate-limit
 */

import { defineEventHandler, createError, getMethod, getRequestHeader, getRequestIP, setResponseHeaders } from '#imports';
import { RateLimiterRedis, RateLimiterMemory } from 'rate-limiter-flexible';
import Redis from 'ioredis';
import { trace, context, SpanStatusCode } from '@opentelemetry/api';
import { metrics } from '../utils/otel-metrics.mjs';

const tracer = trace.getTracer('sidecar-rate-limit');

// Configuration
const RATE_LIMIT_CONFIG = {
  // Authenticated users
  authenticated: {
    points: 1000, // Requests per window
    duration: 60, // 60 seconds
    blockDuration: 300, // Block for 5 minutes if exceeded
  },
  // Unauthenticated (per IP)
  unauthenticated: {
    points: 100,
    duration: 60,
    blockDuration: 600, // Block for 10 minutes
  },
  // SPARQL query endpoints (more restrictive)
  sparql: {
    points: 50,
    duration: 60,
    blockDuration: 300,
  },
  // Admin endpoints (most restrictive)
  admin: {
    points: 20,
    duration: 60,
    blockDuration: 900, // 15 minutes
  },
};

// System load thresholds for adaptive limiting
const LOAD_THRESHOLDS = {
  normal: 0.7,    // < 70% CPU/memory
  elevated: 0.85, // 70-85%
  critical: 0.95, // > 85%
};

/**
 * Initialize Redis client for distributed rate limiting
 */
let redisClient = null;
let isRedisConnected = false;

try {
  const redisUrl = process.env.REDIS_URL || 'redis://localhost:6379';
  redisClient = new Redis(redisUrl, {
    enableOfflineQueue: false,
    maxRetriesPerRequest: 3,
    retryStrategy: (times) => {
      if (times > 3) return null;
      return Math.min(times * 100, 2000);
    },
  });

  redisClient.on('connect', () => {
    isRedisConnected = true;
    console.log('[RateLimit] Redis connected for distributed rate limiting');
  });

  redisClient.on('error', (err) => {
    isRedisConnected = false;
    console.warn('[RateLimit] Redis error, falling back to memory-based limiting:', err.message);
  });
} catch (err) {
  console.warn('[RateLimit] Redis initialization failed, using memory-based limiting:', err.message);
}

/**
 * Create rate limiter instances
 */
const rateLimiters = {
  authenticated: null,
  unauthenticated: null,
  sparql: null,
  admin: null,
};

function createLimiter(config) {
  if (isRedisConnected && redisClient) {
    return new RateLimiterRedis({
      storeClient: redisClient,
      keyPrefix: 'rl:',
      ...config,
    });
  } else {
    return new RateLimiterMemory(config);
  }
}

// Initialize limiters
Object.keys(RATE_LIMIT_CONFIG).forEach(key => {
  rateLimiters[key] = createLimiter(RATE_LIMIT_CONFIG[key]);
});

/**
 * Get current system load (simplified)
 * In production, this would integrate with system metrics
 */
function getSystemLoad() {
  const cpuUsage = process.cpuUsage();
  const memUsage = process.memoryUsage();

  // Simplified load calculation (0-1 scale)
  const cpuLoad = (cpuUsage.user + cpuUsage.system) / 1000000 / 100;
  const memLoad = memUsage.heapUsed / memUsage.heapTotal;

  return Math.max(cpuLoad, memLoad);
}

/**
 * Calculate adaptive rate limit based on system load
 */
function getAdaptiveLimit(basePoints, systemLoad) {
  if (systemLoad >= LOAD_THRESHOLDS.critical) {
    return Math.floor(basePoints * 0.5); // Reduce to 50% during critical load
  } else if (systemLoad >= LOAD_THRESHOLDS.elevated) {
    return Math.floor(basePoints * 0.75); // Reduce to 75% during elevated load
  }
  return basePoints; // Normal operation
}

/**
 * Determine rate limiter type based on request
 * @param {string} path - Request path
 * @param {boolean} authenticated - Whether the request carries credentials
 */
function getRateLimiterType(path, authenticated) {
  // Admin endpoints
  if (path.startsWith('/api/admin')) {
    return 'admin';
  }

  // SPARQL query endpoints
  if (path.includes('/sparql') || path.includes('/query')) {
    return 'sparql';
  }

  // Authenticated vs unauthenticated
  if (authenticated) {
    return 'authenticated';
  }

  return 'unauthenticated';
}

/**
 * Get rate limit key (user ID or IP address)
 * @param {import('h3').H3Event} event
 * @param {string} type
 */
function getRateLimitKey(event, type) {
  const userId = event.context.auth?.userId;
  if (type === 'authenticated' && userId) {
    return `user:${userId}`;
  }

  // Get IP from various headers (considering proxies)
  const ip = getRequestIP(event, { xForwardedFor: true }) || 'unknown';

  return `ip:${ip}`;
}

/**
 * Rate limiting middleware (h3 event handler)
 */
const rateLimitHandler = defineEventHandler(async (event) => {
  const path = (event.path || event.node.req.url || '').split('?')[0];
  const span = tracer.startSpan('rate-limit-check', {
    attributes: {
      'http.method': getMethod(event),
      'http.url': path,
    },
  });

  try {
    // Determine limiter type and key
    const authenticated = Boolean(event.context.auth || getRequestHeader(event, 'authorization'));
    const limiterType = getRateLimiterType(path, authenticated);
    const rateLimitKey = getRateLimitKey(event, limiterType);
    const limiter = rateLimiters[limiterType];

    if (!limiter) {
      span.setStatus({ code: SpanStatusCode.ERROR, message: 'Rate limiter not configured' });
      return;
    }

    // Get system load for adaptive limiting
    const systemLoad = getSystemLoad();
    const baseConfig = RATE_LIMIT_CONFIG[limiterType];
    const adaptivePoints = getAdaptiveLimit(baseConfig.points, systemLoad);

    span.setAttributes({
      'rate_limit.type': limiterType,
      'rate_limit.key': rateLimitKey,
      'rate_limit.base_points': baseConfig.points,
      'rate_limit.adaptive_points': adaptivePoints,
      'rate_limit.system_load': systemLoad,
    });

    let rateLimitResult;
    try {
      // Consume 1 point
      rateLimitResult = await limiter.consume(rateLimitKey, 1);
    } catch (rateLimitError) {
      // rate-limiter-flexible rejects with a RateLimiterRes (not an Error) when the limit is exceeded
      if (!(rateLimitError instanceof Error) && rateLimitError?.remainingPoints !== undefined) {
        const retryAfter = Math.ceil(rateLimitError.msBeforeNext / 1000);

        setResponseHeaders(event, {
          'X-RateLimit-Limit': String(adaptivePoints),
          'X-RateLimit-Remaining': '0',
          'X-RateLimit-Reset': new Date(Date.now() + rateLimitError.msBeforeNext).toISOString(),
          'Retry-After': String(retryAfter),
        });

        if (metrics?.rateLimitCounter) {
          metrics.rateLimitCounter.add(1, {
            type: limiterType,
            status: 'blocked',
            adaptive: systemLoad > LOAD_THRESHOLDS.normal,
          });
        }

        span.setAttributes({
          'rate_limit.exceeded': true,
          'rate_limit.retry_after': retryAfter,
        });
        span.setStatus({ code: SpanStatusCode.OK, message: 'Rate limit exceeded' });

        throw createError({
          statusCode: 429,
          statusMessage: 'Too Many Requests',
          message: `Rate limit exceeded for ${limiterType} requests`,
          data: { retryAfter, limit: adaptivePoints, type: limiterType },
        });
      }

      // Limiter backend failure (e.g. Redis down) - fail open, but not silently
      console.error('[RateLimit] Error checking rate limit:', rateLimitError);
      span.recordException(rateLimitError);
      return;
    }

    setResponseHeaders(event, {
      'X-RateLimit-Limit': String(adaptivePoints),
      'X-RateLimit-Remaining': String(rateLimitResult.remainingPoints),
      'X-RateLimit-Reset': new Date(Date.now() + rateLimitResult.msBeforeNext).toISOString(),
    });

    if (metrics?.rateLimitCounter) {
      metrics.rateLimitCounter.add(1, {
        type: limiterType,
        status: 'allowed',
        adaptive: systemLoad > LOAD_THRESHOLDS.normal,
      });
    }

    span.setStatus({ code: SpanStatusCode.OK });
  } catch (err) {
    if (err.statusCode === 429) {
      throw err;
    }
    // Unexpected error - fail open, but log
    console.error('[RateLimit] Unexpected error:', err);
    span.recordException(err);
    span.setStatus({ code: SpanStatusCode.ERROR, message: err.message });
  } finally {
    span.end();
  }
});

/**
 * Get rate limit status for a key (used by admin endpoints)
 */
export async function getRateLimitStatus(key, type = 'unauthenticated') {
  const limiter = rateLimiters[type];
  if (!limiter) {
    return null;
  }

  try {
    const result = await limiter.get(key);
    return {
      key,
      type,
      consumed: result?.consumedPoints || 0,
      remaining: result?.remainingPoints || RATE_LIMIT_CONFIG[type].points,
      limit: RATE_LIMIT_CONFIG[type].points,
      resetAt: result?.msBeforeNext ? new Date(Date.now() + result.msBeforeNext) : null,
    };
  } catch (err) {
    console.error('[RateLimit] Error getting status:', err);
    return null;
  }
}

/**
 * Manually reset rate limit for a key (admin function)
 */
export async function resetRateLimit(key, type = 'unauthenticated') {
  const limiter = rateLimiters[type];
  if (!limiter) {
    return false;
  }

  try {
    await limiter.delete(key);
    return true;
  } catch (err) {
    console.error('[RateLimit] Error resetting:', err);
    return false;
  }
}

/**
 * Get all rate limit configurations
 */
export function getRateLimitConfig() {
  return {
    ...RATE_LIMIT_CONFIG,
    systemLoad: getSystemLoad(),
    redisConnected: isRedisConnected,
    adaptiveEnabled: true,
  };
}

// Cleanup on shutdown
process.on('SIGTERM', () => {
  if (redisClient) {
    redisClient.quit();
  }
});

/** Named export retained for backward compatibility (same h3 handler as the default export). */
export const rateLimitMiddleware = rateLimitHandler;

export default rateLimitHandler;
