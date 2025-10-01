/**
 * @file Retry strategy tests
 * @module test/sidecar/retry-strategy
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { RetryStrategy, retryWithBackoff } from '../../src/sidecar/retry-strategy.mjs';

describe('RetryStrategy', () => {
  let retryStrategy;

  beforeEach(() => {
    retryStrategy = new RetryStrategy({
      maxRetries: 3,
      initialDelay: 100,
      maxDelay: 1000,
      backoffMultiplier: 2,
      jitter: false // Disable jitter for predictable tests
    });
  });

  describe('initialization', () => {
    it('should accept valid configuration', () => {
      const strategy = new RetryStrategy({
        maxRetries: 5,
        initialDelay: 200,
        maxDelay: 5000
      });

      expect(strategy.config.maxRetries).toBe(5);
      expect(strategy.config.initialDelay).toBe(200);
      expect(strategy.config.maxDelay).toBe(5000);
    });

    it('should use default configuration', () => {
      const strategy = new RetryStrategy();

      expect(strategy.config.maxRetries).toBe(3);
      expect(strategy.config.initialDelay).toBe(100);
      expect(strategy.config.backoffMultiplier).toBe(2);
    });
  });

  describe('successful execution', () => {
    it('should execute function successfully on first attempt', async () => {
      const fn = vi.fn().mockResolvedValue('success');
      const result = await retryStrategy.execute(fn);

      expect(result).toBe('success');
      expect(fn).toHaveBeenCalledTimes(1);
      expect(retryStrategy.metrics.totalAttempts).toBe(1);
      expect(retryStrategy.metrics.totalRetries).toBe(0);
    });

    it('should succeed after retries', async () => {
      const fn = vi.fn()
        .mockRejectedValueOnce(new Error('UNAVAILABLE'))
        .mockRejectedValueOnce(new Error('UNAVAILABLE'))
        .mockResolvedValue('success');

      const result = await retryStrategy.execute(fn);

      expect(result).toBe('success');
      expect(fn).toHaveBeenCalledTimes(3);
      expect(retryStrategy.metrics.totalRetries).toBe(2);
      expect(retryStrategy.metrics.successAfterRetry).toBe(1);
    });
  });

  describe('retry logic', () => {
    it('should retry on retryable errors', async () => {
      const fn = vi.fn().mockRejectedValue(new Error('UNAVAILABLE'));

      await expect(retryStrategy.execute(fn)).rejects.toThrow('UNAVAILABLE');

      expect(fn).toHaveBeenCalledTimes(4); // Initial + 3 retries
      expect(retryStrategy.metrics.totalRetries).toBe(3);
    });

    it('should not retry on non-retryable errors', async () => {
      const error = new Error('NOT_FOUND');
      error.code = 'NOT_FOUND';
      const fn = vi.fn().mockRejectedValue(error);

      await expect(retryStrategy.execute(fn)).rejects.toThrow('NOT_FOUND');

      expect(fn).toHaveBeenCalledTimes(1); // No retries
      expect(retryStrategy.metrics.totalRetries).toBe(0);
      expect(retryStrategy.metrics.permanentFailures).toBe(1);
    });

    it('should retry on timeout errors', async () => {
      const fn = vi.fn()
        .mockRejectedValueOnce(new Error('timeout'))
        .mockResolvedValue('success');

      const result = await retryStrategy.execute(fn);

      expect(result).toBe('success');
      expect(fn).toHaveBeenCalledTimes(2);
    });

    it('should retry on network errors', async () => {
      const fn = vi.fn()
        .mockRejectedValueOnce(new Error('ECONNREFUSED'))
        .mockResolvedValue('success');

      const result = await retryStrategy.execute(fn);

      expect(result).toBe('success');
      expect(fn).toHaveBeenCalledTimes(2);
    });
  });

  describe('exponential backoff', () => {
    it('should implement exponential backoff', async () => {
      const fn = vi.fn().mockRejectedValue(new Error('UNAVAILABLE'));
      const delays = [];

      // Spy on _sleep to capture delays
      const originalSleep = retryStrategy._sleep.bind(retryStrategy);
      retryStrategy._sleep = vi.fn(async (ms) => {
        delays.push(ms);
        return originalSleep(ms);
      });

      await expect(retryStrategy.execute(fn)).rejects.toThrow();

      // Verify exponential backoff: 100, 200, 400
      expect(delays).toHaveLength(3);
      expect(delays[0]).toBe(100);
      expect(delays[1]).toBe(200);
      expect(delays[2]).toBe(400);
    });

    it('should cap delay at maxDelay', async () => {
      const strategy = new RetryStrategy({
        maxRetries: 5,
        initialDelay: 100,
        maxDelay: 300,
        backoffMultiplier: 2,
        jitter: false
      });

      const fn = vi.fn().mockRejectedValue(new Error('UNAVAILABLE'));
      const delays = [];

      strategy._sleep = vi.fn(async (ms) => {
        delays.push(ms);
      });

      await expect(strategy.execute(fn)).rejects.toThrow();

      // Delays should be capped at 300
      expect(Math.max(...delays)).toBeLessThanOrEqual(300);
    });

    it('should add jitter when enabled', async () => {
      const strategy = new RetryStrategy({
        maxRetries: 2,
        initialDelay: 100,
        jitter: true
      });

      const fn = vi.fn().mockRejectedValue(new Error('UNAVAILABLE'));
      const delays = [];

      strategy._sleep = vi.fn(async (ms) => {
        delays.push(ms);
      });

      await expect(strategy.execute(fn)).rejects.toThrow();

      // With jitter, delays should vary slightly from base values
      expect(delays).toHaveLength(2);
      expect(delays[0]).toBeGreaterThanOrEqual(100);
      expect(delays[0]).toBeLessThanOrEqual(125);
    });
  });

  describe('metrics', () => {
    it('should track retry metrics', async () => {
      const fn = vi.fn()
        .mockRejectedValueOnce(new Error('UNAVAILABLE'))
        .mockRejectedValueOnce(new Error('UNAVAILABLE'))
        .mockResolvedValue('success');

      await retryStrategy.execute(fn);

      const metrics = retryStrategy.getMetrics();

      expect(metrics.totalAttempts).toBe(3);
      expect(metrics.totalRetries).toBe(2);
      expect(metrics.successAfterRetry).toBe(1);
      expect(metrics.retryRate).toBeGreaterThan(0);
    });

    it('should track retries by error type', async () => {
      const fn1 = vi.fn().mockRejectedValue(new Error('UNAVAILABLE'));
      const fn2 = vi.fn().mockRejectedValue(new Error('DEADLINE_EXCEEDED'));

      await expect(retryStrategy.execute(fn1)).rejects.toThrow();
      await expect(retryStrategy.execute(fn2)).rejects.toThrow();

      const metrics = retryStrategy.getMetrics();

      expect(metrics.retriesByError.UNKNOWN).toBeGreaterThan(0);
    });

    it('should reset metrics', async () => {
      const fn = vi.fn().mockRejectedValue(new Error('UNAVAILABLE'));
      await expect(retryStrategy.execute(fn)).rejects.toThrow();

      retryStrategy.resetMetrics();

      expect(retryStrategy.metrics.totalAttempts).toBe(0);
      expect(retryStrategy.metrics.totalRetries).toBe(0);
    });
  });

  describe('helper functions', () => {
    it('should work with retryWithBackoff helper', async () => {
      const fn = vi.fn()
        .mockRejectedValueOnce(new Error('UNAVAILABLE'))
        .mockResolvedValue('success');

      const result = await retryWithBackoff(fn, {
        maxRetries: 2,
        initialDelay: 50
      });

      expect(result).toBe('success');
      expect(fn).toHaveBeenCalledTimes(2);
    });
  });
});
