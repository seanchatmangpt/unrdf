/**
 * @file Rate Limiter Tests
 * @module @unrdf/daemon/test/middleware-rate-limiter
 * @description Comprehensive tests for token bucket rate limiter
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  TokenBucketRateLimiter,
  createRateLimitMiddleware,
  createRateLimiter,
  parseEnvConfig,
} from '../src/middleware/rate-limiter.mjs';

describe('TokenBucketRateLimiter', () => {
  describe('Constructor', () => {
    it('should create limiter with default config', () => {
      const limiter = new TokenBucketRateLimiter();
      expect(limiter.maxRequests).toBe(100);
      expect(limiter.windowMs).toBe(60000);
      expect(limiter.burstSize).toBe(10);
      expect(limiter.enablePerIp).toBe(true);
      expect(limiter.enablePerApiKey).toBe(true);
    });

    it('should create limiter with custom config', () => {
      const limiter = new TokenBucketRateLimiter({
        maxRequests: 200,
        windowMs: 120000,
        burstSize: 20,
        enablePerIp: false,
      });
      expect(limiter.maxRequests).toBe(200);
      expect(limiter.windowMs).toBe(120000);
      expect(limiter.burstSize).toBe(20);
      expect(limiter.enablePerIp).toBe(false);
    });

    it('should validate config with Zod', () => {
      expect(() => new TokenBucketRateLimiter({
        maxRequests: -1,
      })).toThrow();

      expect(() => new TokenBucketRateLimiter({
        windowMs: 0,
      })).toThrow();
    });

    it('should calculate refill rate correctly', () => {
      const limiter = new TokenBucketRateLimiter({
        maxRequests: 100,
        windowMs: 60000,
      });
      expect(limiter.refillRate).toBeCloseTo(100 / 60000);
    });
  });

  describe('extractIdentifier', () => {
    let limiter;

    beforeEach(() => {
      limiter = new TokenBucketRateLimiter();
    });

    it('should prioritize API key over IP', () => {
      const identifier = limiter.extractIdentifier({
        ip: '192.168.1.1',
        apiKey: 'test-key-123',
      });
      expect(identifier).toBe('ratelimit:apikey:test-key-123');
    });

    it('should use IP when API key not provided', () => {
      const identifier = limiter.extractIdentifier({
        ip: '192.168.1.1',
      });
      expect(identifier).toBe('ratelimit:ip:192.168.1.1');
    });

    it('should return null when both disabled', () => {
      limiter = new TokenBucketRateLimiter({
        enablePerIp: false,
        enablePerApiKey: false,
      });
      const identifier = limiter.extractIdentifier({
        ip: '192.168.1.1',
        apiKey: 'test-key',
      });
      expect(identifier).toBeNull();
    });

    it('should respect enablePerApiKey flag', () => {
      limiter = new TokenBucketRateLimiter({
        enablePerApiKey: false,
      });
      const identifier = limiter.extractIdentifier({
        ip: '192.168.1.1',
        apiKey: 'test-key',
      });
      expect(identifier).toBe('ratelimit:ip:192.168.1.1');
    });

    it('should respect enablePerIp flag', () => {
      limiter = new TokenBucketRateLimiter({
        enablePerIp: false,
      });
      const identifier = limiter.extractIdentifier({
        ip: '192.168.1.1',
      });
      expect(identifier).toBeNull();
    });

    it('should use custom key prefix', () => {
      limiter = new TokenBucketRateLimiter({
        keyPrefix: 'custom',
      });
      const identifier = limiter.extractIdentifier({
        ip: '192.168.1.1',
      });
      expect(identifier).toBe('custom:ip:192.168.1.1');
    });
  });

  describe('getBucket', () => {
    let limiter;

    beforeEach(() => {
      limiter = new TokenBucketRateLimiter();
    });

    it('should create new bucket if not exists', () => {
      const bucket = limiter.getBucket('test-id');
      expect(bucket).toBeDefined();
      expect(bucket.tokens).toBe(100);
      expect(bucket.requestCount).toBe(0);
      expect(bucket.lastRefill).toBeDefined();
    });

    it('should return existing bucket', () => {
      const bucket1 = limiter.getBucket('test-id');
      bucket1.tokens = 50;

      const bucket2 = limiter.getBucket('test-id');
      expect(bucket2.tokens).toBe(50);
      expect(bucket1).toBe(bucket2);
    });

    it('should track unique identifiers', () => {
      limiter.getBucket('id1');
      limiter.getBucket('id2');
      limiter.getBucket('id1');

      expect(limiter.stats.uniqueIdentifiers.size).toBe(2);
    });
  });

  describe('refillBucket', () => {
    let limiter;

    beforeEach(() => {
      limiter = new TokenBucketRateLimiter({
        maxRequests: 100,
        windowMs: 10000,
      });
    });

    it('should refill tokens based on elapsed time', () => {
      const bucket = {
        tokens: 50,
        lastRefill: Date.now() - 5000,
        requestCount: 0,
      };

      limiter.refillBucket(bucket);

      // Should add approximately 50 tokens (5000ms / 10000ms * 100)
      expect(bucket.tokens).toBeGreaterThan(50);
      expect(bucket.tokens).toBeLessThanOrEqual(100);
    });

    it('should not exceed max tokens', () => {
      const bucket = {
        tokens: 90,
        lastRefill: Date.now() - 10000,
        requestCount: 0,
      };

      limiter.refillBucket(bucket);
      expect(bucket.tokens).toBe(100);
    });

    it('should update lastRefill timestamp', () => {
      const oldTime = Date.now() - 1000;
      const bucket = {
        tokens: 50,
        lastRefill: oldTime,
        requestCount: 0,
      };

      limiter.refillBucket(bucket);
      expect(bucket.lastRefill).toBeGreaterThan(oldTime);
    });

    it('should handle zero elapsed time', () => {
      const bucket = {
        tokens: 50,
        lastRefill: Date.now(),
        requestCount: 0,
      };

      const beforeTokens = bucket.tokens;
      limiter.refillBucket(bucket);
      expect(bucket.tokens).toBe(beforeTokens);
    });
  });

  describe('isBurstLimitExceeded', () => {
    let limiter;

    beforeEach(() => {
      limiter = new TokenBucketRateLimiter({
        burstSize: 5,
        burstWindowMs: 1000,
      });
    });

    it('should return false when under burst limit', () => {
      const bucket = {
        requestCount: 3,
        firstRequest: Date.now(),
      };
      expect(limiter.isBurstLimitExceeded(bucket)).toBe(false);
    });

    it('should return true when at burst limit', () => {
      const bucket = {
        requestCount: 5,
        firstRequest: Date.now(),
      };
      expect(limiter.isBurstLimitExceeded(bucket)).toBe(true);
    });

    it('should reset count when outside burst window', () => {
      const bucket = {
        requestCount: 10,
        firstRequest: Date.now() - 2000,
      };

      const exceeded = limiter.isBurstLimitExceeded(bucket);
      expect(exceeded).toBe(false);
      expect(bucket.requestCount).toBe(0);
    });

    it('should update firstRequest on reset', () => {
      const oldTime = Date.now() - 2000;
      const bucket = {
        requestCount: 10,
        firstRequest: oldTime,
      };

      limiter.isBurstLimitExceeded(bucket);
      expect(bucket.firstRequest).toBeGreaterThan(oldTime);
    });
  });

  describe('consume', () => {
    let limiter;

    beforeEach(() => {
      limiter = new TokenBucketRateLimiter({
        maxRequests: 10,
        windowMs: 10000,
        burstSize: 3,
        burstWindowMs: 1000,
      });
    });

    it('should allow request when tokens available', () => {
      const result = limiter.consume('test-id');
      expect(result.allowed).toBe(true);
      expect(result.remaining).toBeGreaterThanOrEqual(0);
    });

    it('should decrement tokens on consume', () => {
      limiter.consume('test-id');
      const bucket = limiter.getBucket('test-id');
      expect(bucket.tokens).toBeLessThan(10);
    });

    it('should block when burst limit exceeded', () => {
      limiter.consume('test-id');
      limiter.consume('test-id');
      limiter.consume('test-id');
      const result = limiter.consume('test-id');

      expect(result.allowed).toBe(false);
      expect(result.reason).toBe('Burst limit exceeded');
    });

    it('should block when tokens depleted', () => {
      const bucket = limiter.getBucket('test-id');
      bucket.tokens = 0.5;

      const result = limiter.consume('test-id');
      expect(result.allowed).toBe(false);
      expect(result.reason).toBe('Rate limit exceeded');
    });

    it('should include retryAfter in blocked response', () => {
      const bucket = limiter.getBucket('test-id');
      bucket.tokens = 0;

      const result = limiter.consume('test-id');
      expect(result.retryAfter).toBeGreaterThan(0);
    });

    it('should track statistics', () => {
      limiter.consume('test-id');
      limiter.consume('test-id');

      const stats = limiter.getStats();
      expect(stats.totalRequests).toBe(2);
      expect(stats.allowedRequests).toBeGreaterThan(0);
    });

    it('should handle different identifiers independently', () => {
      const result1 = limiter.consume('id1');
      const result2 = limiter.consume('id2');

      expect(result1.allowed).toBe(true);
      expect(result2.allowed).toBe(true);
    });
  });

  describe('check', () => {
    let limiter;

    beforeEach(() => {
      limiter = new TokenBucketRateLimiter({
        maxRequests: 10,
      });
    });

    it('should check rate limit for IP', () => {
      const result = limiter.check({ ip: '192.168.1.1' });
      expect(result.allowed).toBe(true);
      expect(result.identifier).toContain('ip:192.168.1.1');
    });

    it('should check rate limit for API key', () => {
      const result = limiter.check({ apiKey: 'test-key' });
      expect(result.allowed).toBe(true);
      expect(result.identifier).toContain('apikey:test-key');
    });

    it('should allow when no identifier found', () => {
      const result = limiter.check({});
      expect(result.allowed).toBe(true);
      expect(result.reason).toContain('No identifier');
    });

    it('should block after exceeding limit', () => {
      for (let i = 0; i < 10; i++) {
        limiter.check({ ip: '192.168.1.1' });
      }

      const result = limiter.check({ ip: '192.168.1.1' });
      expect(result.allowed).toBe(false);
    });
  });

  describe('getStats', () => {
    let limiter;

    beforeEach(() => {
      limiter = new TokenBucketRateLimiter();
    });

    it('should return initial stats', () => {
      const stats = limiter.getStats();
      expect(stats.totalRequests).toBe(0);
      expect(stats.allowedRequests).toBe(0);
      expect(stats.blockedRequests).toBe(0);
      expect(stats.uniqueIdentifiers).toBe(0);
      expect(stats.blockRate).toBe(0);
    });

    it('should track total requests', () => {
      limiter.check({ ip: '192.168.1.1' });
      limiter.check({ ip: '192.168.1.2' });

      const stats = limiter.getStats();
      expect(stats.totalRequests).toBe(2);
    });

    it('should calculate block rate correctly', () => {
      for (let i = 0; i < 100; i++) {
        limiter.check({ ip: '192.168.1.1' });
      }

      const stats = limiter.getStats();
      expect(stats.blockRate).toBeGreaterThan(0);
      expect(stats.blockRate).toBeLessThanOrEqual(1);
    });

    it('should track unique identifiers', () => {
      limiter.check({ ip: '192.168.1.1' });
      limiter.check({ ip: '192.168.1.2' });
      limiter.check({ ip: '192.168.1.1' });

      const stats = limiter.getStats();
      expect(stats.uniqueIdentifiers).toBe(2);
    });

    it('should track cache size', () => {
      limiter.check({ ip: '192.168.1.1' });
      limiter.check({ ip: '192.168.1.2' });

      const stats = limiter.getStats();
      expect(stats.cacheSize).toBe(2);
    });
  });

  describe('reset', () => {
    let limiter;

    beforeEach(() => {
      limiter = new TokenBucketRateLimiter();
    });

    it('should reset specific identifier', () => {
      const id = limiter.extractIdentifier({ ip: '192.168.1.1' });
      limiter.check({ ip: '192.168.1.1' });

      limiter.reset(id);

      const bucket = limiter.buckets.get(id);
      expect(bucket).toBeUndefined();
    });

    it('should not affect other identifiers', () => {
      const id1 = limiter.extractIdentifier({ ip: '192.168.1.1' });
      const id2 = limiter.extractIdentifier({ ip: '192.168.1.2' });

      limiter.check({ ip: '192.168.1.1' });
      limiter.check({ ip: '192.168.1.2' });

      limiter.reset(id1);

      expect(limiter.buckets.get(id1)).toBeUndefined();
      expect(limiter.buckets.get(id2)).toBeDefined();
    });
  });

  describe('clear', () => {
    let limiter;

    beforeEach(() => {
      limiter = new TokenBucketRateLimiter();
    });

    it('should clear all buckets', () => {
      limiter.check({ ip: '192.168.1.1' });
      limiter.check({ ip: '192.168.1.2' });

      limiter.clear();

      expect(limiter.buckets.size()).toBe(0);
    });

    it('should reset statistics', () => {
      limiter.check({ ip: '192.168.1.1' });

      limiter.clear();

      const stats = limiter.getStats();
      expect(stats.totalRequests).toBe(0);
      expect(stats.allowedRequests).toBe(0);
      expect(stats.blockedRequests).toBe(0);
    });
  });

  describe('LRU Cache Behavior', () => {
    it('should evict least recently used entries', () => {
      const limiter = new TokenBucketRateLimiter({
        storageMaxSize: 3,
      });

      limiter.check({ ip: '192.168.1.1' });
      limiter.check({ ip: '192.168.1.2' });
      limiter.check({ ip: '192.168.1.3' });
      limiter.check({ ip: '192.168.1.4' });

      const stats = limiter.getStats();
      expect(stats.cacheSize).toBe(3);
    });

    it('should refresh entry on access', () => {
      const limiter = new TokenBucketRateLimiter({
        storageMaxSize: 2,
      });

      limiter.check({ ip: '192.168.1.1' });
      limiter.check({ ip: '192.168.1.2' });
      limiter.check({ ip: '192.168.1.1' }); // Refresh 1
      limiter.check({ ip: '192.168.1.3' }); // Should evict 2, not 1

      const id1 = limiter.extractIdentifier({ ip: '192.168.1.1' });
      const id2 = limiter.extractIdentifier({ ip: '192.168.1.2' });

      expect(limiter.buckets.has(id1)).toBe(true);
      expect(limiter.buckets.has(id2)).toBe(false);
    });
  });
});

describe('createRateLimitMiddleware', () => {
  it('should create middleware function', () => {
    const middleware = createRateLimitMiddleware();
    expect(typeof middleware).toBe('function');
  });

  it('should allow valid requests', async () => {
    const middleware = createRateLimitMiddleware();
    const result = await middleware({ ip: '192.168.1.1' });
    expect(result.allowed).toBe(true);
  });

  it('should throw on rate limit exceeded', async () => {
    const middleware = createRateLimitMiddleware({
      maxRequests: 1,
    });

    await middleware({ ip: '192.168.1.1' });

    await expect(middleware({ ip: '192.168.1.1' }))
      .rejects
      .toThrow('Rate limit exceeded');
  });

  it('should include status code in error', async () => {
    const middleware = createRateLimitMiddleware({
      maxRequests: 1,
    });

    await middleware({ ip: '192.168.1.1' });

    try {
      await middleware({ ip: '192.168.1.1' });
    } catch (error) {
      expect(error.statusCode).toBe(429);
      expect(error.retryAfter).toBeGreaterThan(0);
    }
  });

  it('should pass custom config to limiter', async () => {
    const middleware = createRateLimitMiddleware({
      maxRequests: 200,
      windowMs: 120000,
    });

    const result = await middleware({ ip: '192.168.1.1' });
    expect(result.allowed).toBe(true);
  });
});

describe('createRateLimiter', () => {
  it('should create limiter instance', () => {
    const limiter = createRateLimiter();
    expect(limiter).toBeInstanceOf(TokenBucketRateLimiter);
  });

  it('should apply custom config', () => {
    const limiter = createRateLimiter({
      maxRequests: 500,
    });
    expect(limiter.maxRequests).toBe(500);
  });
});

describe('parseEnvConfig', () => {
  it('should parse environment variables', () => {
    process.env.RATE_LIMIT_MAX_REQUESTS = '200';
    process.env.RATE_LIMIT_WINDOW_MS = '120000';
    process.env.RATE_LIMIT_BURST_SIZE = '20';

    const config = parseEnvConfig();
    expect(config.maxRequests).toBe(200);
    expect(config.windowMs).toBe(120000);
    expect(config.burstSize).toBe(20);

    delete process.env.RATE_LIMIT_MAX_REQUESTS;
    delete process.env.RATE_LIMIT_WINDOW_MS;
    delete process.env.RATE_LIMIT_BURST_SIZE;
  });

  it('should return undefined for missing vars', () => {
    const config = parseEnvConfig();
    expect(config.maxRequests).toBeUndefined();
  });

  it('should parse boolean flags', () => {
    process.env.RATE_LIMIT_ENABLE_PER_IP = 'false';
    const config = parseEnvConfig();
    expect(config.enablePerIp).toBe(false);
    delete process.env.RATE_LIMIT_ENABLE_PER_IP;
  });

  it('should default boolean flags to true', () => {
    const config = parseEnvConfig();
    expect(config.enablePerIp).toBe(true);
    expect(config.enablePerApiKey).toBe(true);
  });
});

describe('Edge Cases and Security', () => {
  describe('Concurrent Requests', () => {
    it('should handle concurrent requests correctly', async () => {
      const limiter = new TokenBucketRateLimiter({
        maxRequests: 10,
      });

      const promises = Array.from({ length: 20 }, (_, i) =>
        Promise.resolve(limiter.check({ ip: '192.168.1.1' }))
      );

      const results = await Promise.all(promises);
      const allowed = results.filter(r => r.allowed).length;
      const blocked = results.filter(r => !r.allowed).length;

      expect(allowed).toBeGreaterThan(0);
      expect(blocked).toBeGreaterThan(0);
      expect(allowed + blocked).toBe(20);
    });
  });

  describe('Time-based Edge Cases', () => {
    it('should handle rapid sequential requests', () => {
      const limiter = new TokenBucketRateLimiter({
        burstSize: 3,
        burstWindowMs: 1000,
      });

      const r1 = limiter.check({ ip: '192.168.1.1' });
      const r2 = limiter.check({ ip: '192.168.1.1' });
      const r3 = limiter.check({ ip: '192.168.1.1' });
      const r4 = limiter.check({ ip: '192.168.1.1' });

      expect(r1.allowed).toBe(true);
      expect(r2.allowed).toBe(true);
      expect(r3.allowed).toBe(true);
      expect(r4.allowed).toBe(false);
    });
  });

  describe('Input Validation', () => {
    it('should handle invalid context gracefully', () => {
      const limiter = new TokenBucketRateLimiter();

      expect(() => limiter.check(null)).toThrow();
      expect(() => limiter.check(undefined)).toThrow();
    });

    it('should handle empty context', () => {
      const limiter = new TokenBucketRateLimiter();
      const result = limiter.check({});
      expect(result.allowed).toBe(true);
    });
  });

  describe('Memory Safety', () => {
    it('should enforce storage limit', () => {
      const limiter = new TokenBucketRateLimiter({
        storageMaxSize: 10,
      });

      for (let i = 0; i < 100; i++) {
        limiter.check({ ip: `192.168.1.${i}` });
      }

      const stats = limiter.getStats();
      expect(stats.cacheSize).toBeLessThanOrEqual(10);
    });
  });
});
