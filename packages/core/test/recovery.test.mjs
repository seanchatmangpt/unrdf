/**
 * @file Error recovery patterns test suite
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  retry,
  CircuitBreaker,
  fallback,
  withTimeout,
  bulkOperation,
  RateLimiter,
  withRecovery,
} from '../src/recovery.mjs';
import { TimeoutError, NetworkError } from '../src/errors.mjs';

describe('Error Recovery Patterns', () => {
  describe('retry', () => {
    it('should succeed on first attempt', async () => {
      const operation = vi.fn().mockResolvedValue('success');

      const result = await retry(operation);

      expect(result).toBe('success');
      expect(operation).toHaveBeenCalledTimes(1);
    });

    it('should retry on failure and eventually succeed', async () => {
      let attempts = 0;
      const operation = vi.fn().mockImplementation(() => {
        attempts++;
        if (attempts < 3) {
          throw new NetworkError('Network error');
        }
        return Promise.resolve('success');
      });

      const result = await retry(operation, { maxAttempts: 5, initialDelay: 10 });

      expect(result).toBe('success');
      expect(operation).toHaveBeenCalledTimes(3);
    });

    it('should throw after max attempts', async () => {
      const operation = vi.fn().mockRejectedValue(new NetworkError('Network error'));

      await expect(
        retry(operation, { maxAttempts: 3, initialDelay: 10 })
      ).rejects.toThrow('Network error');

      expect(operation).toHaveBeenCalledTimes(3);
    });

    it('should use exponential backoff', async () => {
      const delays = [];
      let attempts = 0;

      const operation = vi.fn().mockImplementation(() => {
        attempts++;
        if (attempts < 4) {
          throw new NetworkError('Network error');
        }
        return Promise.resolve('success');
      });

      const startTime = Date.now();

      await retry(operation, {
        maxAttempts: 4,
        initialDelay: 50,
        backoffMultiplier: 2,
        onRetry: async (error, attempt, delay) => {
          delays.push(delay);
        },
      });

      const elapsed = Date.now() - startTime;

      expect(delays).toHaveLength(3);
      expect(delays[0]).toBe(50);
      expect(delays[1]).toBe(100);
      expect(delays[2]).toBe(200);
      expect(elapsed).toBeGreaterThanOrEqual(350); // 50 + 100 + 200
    });

    it('should respect max delay', async () => {
      let attempts = 0;
      const operation = vi.fn().mockImplementation(() => {
        attempts++;
        if (attempts < 5) {
          throw new NetworkError('Network error');
        }
        return Promise.resolve('success');
      });

      const delays = [];

      await retry(operation, {
        maxAttempts: 5,
        initialDelay: 100,
        maxDelay: 150,
        backoffMultiplier: 2,
        onRetry: async (error, attempt, delay) => {
          delays.push(delay);
        },
      });

      expect(delays).toHaveLength(4);
      expect(delays[0]).toBe(100);
      expect(delays[1]).toBe(150); // Capped
      expect(delays[2]).toBe(150); // Capped
      expect(delays[3]).toBe(150); // Capped
    });

    it('should use custom shouldRetry predicate', async () => {
      const operation = vi.fn().mockRejectedValue(new Error('Non-retryable error'));

      await expect(
        retry(operation, {
          maxAttempts: 3,
          shouldRetry: () => false,
        })
      ).rejects.toThrow('Non-retryable error');

      expect(operation).toHaveBeenCalledTimes(1);
    });

    it('should call onRetry callback', async () => {
      let attempts = 0;
      const operation = vi.fn().mockImplementation(() => {
        attempts++;
        if (attempts < 3) {
          throw new NetworkError('Network error');
        }
        return Promise.resolve('success');
      });

      const onRetry = vi.fn();

      await retry(operation, {
        maxAttempts: 3,
        initialDelay: 10,
        onRetry,
      });

      expect(onRetry).toHaveBeenCalledTimes(2);
      expect(onRetry.mock.calls[0][1]).toBe(1); // First retry is attempt 1
      expect(onRetry.mock.calls[1][1]).toBe(2); // Second retry is attempt 2
    });
  });

  describe('CircuitBreaker', () => {
    let breaker;

    beforeEach(() => {
      breaker = new CircuitBreaker({
        failureThreshold: 3,
        successThreshold: 2,
        timeout: 100,
      });
    });

    it('should start in closed state', () => {
      const state = breaker.getState();

      expect(state.state).toBe('closed');
      expect(state.failures).toBe(0);
      expect(state.successes).toBe(0);
    });

    it('should execute successful operations', async () => {
      const operation = vi.fn().mockResolvedValue('success');

      const result = await breaker.execute(operation);

      expect(result).toBe('success');
      expect(breaker.getState().state).toBe('closed');
      expect(breaker.getState().failures).toBe(0);
    });

    it('should open after threshold failures', async () => {
      const operation = vi.fn().mockRejectedValue(new Error('Failure'));

      for (let i = 0; i < 3; i++) {
        await expect(breaker.execute(operation)).rejects.toThrow('Failure');
      }

      const state = breaker.getState();
      expect(state.state).toBe('open');
      expect(state.failures).toBe(3);
    });

    it('should reject immediately when open', async () => {
      const operation = vi.fn().mockRejectedValue(new Error('Failure'));

      // Trigger circuit to open
      for (let i = 0; i < 3; i++) {
        await expect(breaker.execute(operation)).rejects.toThrow();
      }

      // Should reject without calling operation
      operation.mockClear();
      await expect(breaker.execute(operation)).rejects.toThrow();

      expect(operation).not.toHaveBeenCalled();
    });

    it('should transition to half-open after timeout', async () => {
      const operation = vi.fn().mockRejectedValue(new Error('Failure'));

      // Open circuit
      for (let i = 0; i < 3; i++) {
        await expect(breaker.execute(operation)).rejects.toThrow();
      }

      expect(breaker.getState().state).toBe('open');

      // Wait for timeout
      await new Promise((resolve) => setTimeout(resolve, 150));

      // Should be half-open now
      operation.mockResolvedValue('success');
      const result = await breaker.execute(operation);

      expect(result).toBe('success');
      expect(breaker.getState().state).toBe('half-open');
    });

    it('should close from half-open after success threshold', async () => {
      const operation = vi.fn();

      // Open circuit
      operation.mockRejectedValue(new Error('Failure'));
      for (let i = 0; i < 3; i++) {
        await expect(breaker.execute(operation)).rejects.toThrow();
      }

      // Wait for timeout
      await new Promise((resolve) => setTimeout(resolve, 150));

      // Half-open: succeed twice
      operation.mockResolvedValue('success');
      await breaker.execute(operation);
      await breaker.execute(operation);

      expect(breaker.getState().state).toBe('closed');
      expect(breaker.getState().failures).toBe(0);
    });

    it('should reset on demand', async () => {
      const operation = vi.fn().mockRejectedValue(new Error('Failure'));

      // Open circuit
      for (let i = 0; i < 3; i++) {
        await expect(breaker.execute(operation)).rejects.toThrow();
      }

      expect(breaker.getState().state).toBe('open');

      breaker.reset();

      const state = breaker.getState();
      expect(state.state).toBe('closed');
      expect(state.failures).toBe(0);
      expect(state.successes).toBe(0);
    });

    it('should call onStateChange callback', async () => {
      const onStateChange = vi.fn();
      const breaker = new CircuitBreaker({
        failureThreshold: 2,
        onStateChange,
      });

      const operation = vi.fn().mockRejectedValue(new Error('Failure'));

      await expect(breaker.execute(operation)).rejects.toThrow();
      await expect(breaker.execute(operation)).rejects.toThrow();

      expect(onStateChange).toHaveBeenCalledWith('closed', 'open', expect.any(Object));
    });
  });

  describe('fallback', () => {
    it('should return primary result on success', async () => {
      const primary = vi.fn().mockResolvedValue('primary');
      const fallbackFn = vi.fn().mockResolvedValue('fallback');

      const result = await fallback(primary, fallbackFn);

      expect(result).toBe('primary');
      expect(primary).toHaveBeenCalled();
      expect(fallbackFn).not.toHaveBeenCalled();
    });

    it('should call fallback function on primary failure', async () => {
      const primary = vi.fn().mockRejectedValue(new Error('Primary failed'));
      const fallbackFn = vi.fn().mockResolvedValue('fallback');

      const result = await fallback(primary, fallbackFn);

      expect(result).toBe('fallback');
      expect(primary).toHaveBeenCalled();
      expect(fallbackFn).toHaveBeenCalled();
    });

    it('should return static fallback value', async () => {
      const primary = vi.fn().mockRejectedValue(new Error('Primary failed'));

      const result = await fallback(primary, []);

      expect(result).toEqual([]);
      expect(primary).toHaveBeenCalled();
    });

    it('should handle null fallback', async () => {
      const primary = vi.fn().mockRejectedValue(new Error('Primary failed'));

      const result = await fallback(primary, null);

      expect(result).toBeNull();
    });
  });

  describe('withTimeout', () => {
    it('should return result within timeout', async () => {
      const operation = vi.fn().mockImplementation(async () => {
        await new Promise((resolve) => setTimeout(resolve, 10));
        return 'success';
      });

      const result = await withTimeout(operation, 100);

      expect(result).toBe('success');
    });

    it('should throw TimeoutError on timeout', async () => {
      const operation = vi.fn().mockImplementation(async () => {
        await new Promise((resolve) => setTimeout(resolve, 200));
        return 'success';
      });

      await expect(withTimeout(operation, 50, 'Custom timeout message')).rejects.toThrow(
        TimeoutError
      );
    });

    it('should include context in timeout error', async () => {
      const operation = vi.fn().mockImplementation(async () => {
        await new Promise((resolve) => setTimeout(resolve, 200));
        return 'success';
      });

      try {
        await withTimeout(operation, 50, 'Test timeout');
      } catch (error) {
        expect(error).toBeInstanceOf(TimeoutError);
        expect(error.message).toBe('Test timeout');
        expect(error.context.timeoutMs).toBe(50);
      }
    });
  });

  describe('bulkOperation', () => {
    it('should process all items successfully', async () => {
      const items = [1, 2, 3, 4, 5];
      const operation = vi.fn().mockImplementation(async (item) => item * 2);

      const { successes, failures } = await bulkOperation(items, operation);

      expect(successes).toHaveLength(5);
      expect(failures).toHaveLength(0);
      expect(successes[0].result).toBe(2);
      expect(successes[4].result).toBe(10);
    });

    it('should handle failures and continue', async () => {
      const items = [1, 2, 3, 4, 5];
      const operation = vi.fn().mockImplementation(async (item) => {
        if (item === 3) throw new Error('Failed on 3');
        return item * 2;
      });

      const { successes, failures } = await bulkOperation(items, operation, {
        continueOnError: true,
      });

      expect(successes).toHaveLength(4);
      expect(failures).toHaveLength(1);
      expect(failures[0].item).toBe(3);
      expect(failures[0].error.message).toBe('Failed on 3');
    });

    it('should stop on error when continueOnError is false', async () => {
      const items = [1, 2, 3, 4, 5];
      const operation = vi.fn().mockImplementation(async (item) => {
        if (item === 3) throw new Error('Failed on 3');
        return item * 2;
      });

      await expect(
        bulkOperation(items, operation, { continueOnError: false })
      ).rejects.toThrow('Failed on 3');
    });

    it('should respect concurrency limit', async () => {
      const items = Array.from({ length: 20 }, (_, i) => i);
      let concurrentOps = 0;
      let maxConcurrent = 0;

      const operation = vi.fn().mockImplementation(async (item) => {
        concurrentOps++;
        maxConcurrent = Math.max(maxConcurrent, concurrentOps);
        await new Promise((resolve) => setTimeout(resolve, 10));
        concurrentOps--;
        return item;
      });

      await bulkOperation(items, operation, { concurrency: 5 });

      expect(maxConcurrent).toBeLessThanOrEqual(5);
    });

    it('should include item in result', async () => {
      const items = ['a', 'b', 'c'];
      const operation = vi.fn().mockImplementation(async (item) => item.toUpperCase());

      const { successes } = await bulkOperation(items, operation);

      expect(successes[0].item).toBe('a');
      expect(successes[0].result).toBe('A');
    });
  });

  describe('RateLimiter', () => {
    it('should allow operations within limit', async () => {
      const limiter = new RateLimiter({
        maxOperations: 5,
        windowMs: 1000,
      });

      const operation = vi.fn().mockResolvedValue('success');

      for (let i = 0; i < 5; i++) {
        await limiter.execute(operation);
      }

      expect(operation).toHaveBeenCalledTimes(5);
    });

    it('should throttle when limit exceeded', async () => {
      const limiter = new RateLimiter({
        maxOperations: 3,
        windowMs: 200,
      });

      const operation = vi.fn().mockResolvedValue('success');
      const startTime = Date.now();

      // First 3 should be immediate
      for (let i = 0; i < 3; i++) {
        await limiter.execute(operation);
      }

      const immediateElapsed = Date.now() - startTime;
      expect(immediateElapsed).toBeLessThan(100);

      // 4th should wait
      await limiter.execute(operation);
      const totalElapsed = Date.now() - startTime;

      expect(totalElapsed).toBeGreaterThanOrEqual(200);
      expect(operation).toHaveBeenCalledTimes(4);
    });

    it('should get status', async () => {
      const limiter = new RateLimiter({
        maxOperations: 5,
        windowMs: 1000,
      });

      const operation = vi.fn().mockResolvedValue('success');

      await limiter.execute(operation);
      await limiter.execute(operation);

      const status = limiter.getStatus();

      expect(status.max).toBe(5);
      expect(status.current).toBe(2);
      expect(status.available).toBe(3);
      expect(status.windowMs).toBe(1000);
    });

    it('should reset limiter', async () => {
      const limiter = new RateLimiter({
        maxOperations: 2,
        windowMs: 1000,
      });

      const operation = vi.fn().mockResolvedValue('success');

      await limiter.execute(operation);
      await limiter.execute(operation);

      expect(limiter.getStatus().current).toBe(2);

      limiter.reset();

      expect(limiter.getStatus().current).toBe(0);
    });
  });

  describe('withRecovery', () => {
    it('should apply retry strategy', async () => {
      let attempts = 0;
      const operation = vi.fn().mockImplementation(() => {
        attempts++;
        if (attempts < 2) {
          return Promise.reject(new NetworkError('Network error'));
        }
        return Promise.resolve('success');
      });

      const robustOp = withRecovery(operation, {
        retry: { maxAttempts: 3, initialDelay: 10 },
      });

      const result = await robustOp();

      expect(result).toBe('success');
      expect(operation).toHaveBeenCalledTimes(2);
    });

    it('should apply timeout', async () => {
      const operation = vi.fn().mockImplementation(async () => {
        await new Promise((resolve) => setTimeout(resolve, 200));
        return 'success';
      });

      const robustOp = withRecovery(operation, {
        timeout: 50,
      });

      await expect(robustOp()).rejects.toThrow(TimeoutError);
    });

    it('should apply fallback', async () => {
      const operation = vi.fn().mockRejectedValue(new Error('Failed'));

      const robustOp = withRecovery(operation, {
        fallback: 'fallback value',
      });

      const result = await robustOp();

      expect(result).toBe('fallback value');
    });

    it('should apply circuit breaker', async () => {
      const breaker = new CircuitBreaker({ failureThreshold: 2 });
      const operation = vi.fn().mockRejectedValue(new Error('Failed'));

      const robustOp = withRecovery(operation, {
        circuitBreaker: breaker,
      });

      await expect(robustOp()).rejects.toThrow('Failed');
      await expect(robustOp()).rejects.toThrow('Failed');

      expect(breaker.getState().state).toBe('open');
    });

    it('should combine multiple strategies', async () => {
      let attempts = 0;
      const operation = vi.fn().mockImplementation(() => {
        attempts++;
        if (attempts < 2) {
          return Promise.reject(new NetworkError('Network error'));
        }
        return new Promise((resolve) => setTimeout(() => resolve('success'), 20));
      });

      const robustOp = withRecovery(operation, {
        retry: { maxAttempts: 3, initialDelay: 10 },
        timeout: 100,
        fallback: 'fallback',
      });

      const result = await robustOp();

      expect(result).toBe('success');
    });

    it('should pass arguments to operation', async () => {
      const operation = vi.fn().mockImplementation((a, b) => Promise.resolve(a + b));

      const robustOp = withRecovery(operation, {
        retry: { maxAttempts: 2 },
      });

      const result = await robustOp(2, 3);

      expect(result).toBe(5);
      expect(operation).toHaveBeenCalledWith(2, 3);
    });
  });
});
