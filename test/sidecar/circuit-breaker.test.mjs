/**
 * @file Circuit breaker tests
 * @module test/sidecar/circuit-breaker
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { CircuitBreaker, CircuitState } from '../../src/sidecar/circuit-breaker.mjs';

describe('CircuitBreaker', () => {
  let circuitBreaker;

  beforeEach(() => {
    circuitBreaker = new CircuitBreaker({
      threshold: 3,
      resetTimeout: 1000,
      halfOpenRequests: 2
    });
  });

  describe('initialization', () => {
    it('should start in CLOSED state', () => {
      expect(circuitBreaker.getState()).toBe(CircuitState.CLOSED);
    });

    it('should accept valid configuration', () => {
      const cb = new CircuitBreaker({
        threshold: 5,
        resetTimeout: 5000,
        halfOpenRequests: 3
      });

      expect(cb.config.threshold).toBe(5);
      expect(cb.config.resetTimeout).toBe(5000);
      expect(cb.config.halfOpenRequests).toBe(3);
    });
  });

  describe('successful execution', () => {
    it('should execute function successfully', async () => {
      const fn = vi.fn().mockResolvedValue('success');
      const result = await circuitBreaker.execute(fn);

      expect(result).toBe('success');
      expect(fn).toHaveBeenCalledTimes(1);
      expect(circuitBreaker.getState()).toBe(CircuitState.CLOSED);
    });

    it('should reset failure count on success', async () => {
      const fn = vi.fn()
        .mockRejectedValueOnce(new Error('fail'))
        .mockResolvedValue('success');

      await expect(circuitBreaker.execute(fn)).rejects.toThrow('fail');
      expect(circuitBreaker.failures).toBe(1);

      await circuitBreaker.execute(fn);
      expect(circuitBreaker.failures).toBe(0);
    });
  });

  describe('failure handling', () => {
    it('should track failures', async () => {
      const fn = vi.fn().mockRejectedValue(new Error('fail'));

      await expect(circuitBreaker.execute(fn)).rejects.toThrow('fail');
      expect(circuitBreaker.failures).toBe(1);
    });

    it('should open circuit after threshold failures', async () => {
      const fn = vi.fn().mockRejectedValue(new Error('fail'));

      // Execute until threshold
      for (let i = 0; i < 3; i++) {
        await expect(circuitBreaker.execute(fn)).rejects.toThrow('fail');
      }

      expect(circuitBreaker.getState()).toBe(CircuitState.OPEN);
      expect(circuitBreaker.isOpen()).toBe(true);
    });

    it('should reject requests when circuit is open', async () => {
      const fn = vi.fn().mockRejectedValue(new Error('fail'));

      // Open circuit
      for (let i = 0; i < 3; i++) {
        await expect(circuitBreaker.execute(fn)).rejects.toThrow('fail');
      }

      // Next request should fail immediately
      await expect(circuitBreaker.execute(fn)).rejects.toThrow('Circuit breaker is OPEN');
      expect(fn).toHaveBeenCalledTimes(3); // Should not execute again
    });
  });

  describe('state transitions', () => {
    it('should transition from OPEN to HALF_OPEN after reset timeout', async () => {
      const fn = vi.fn().mockRejectedValue(new Error('fail'));

      // Open circuit
      for (let i = 0; i < 3; i++) {
        await expect(circuitBreaker.execute(fn)).rejects.toThrow('fail');
      }

      expect(circuitBreaker.getState()).toBe(CircuitState.OPEN);

      // Wait for reset timeout
      await new Promise(resolve => setTimeout(resolve, 1100));

      // Next request should allow execution
      await expect(circuitBreaker.execute(fn)).rejects.toThrow('fail');
      expect(circuitBreaker.getState()).toBe(CircuitState.OPEN);
      expect(fn).toHaveBeenCalledTimes(4); // One more execution attempted
    });

    it('should transition from HALF_OPEN to CLOSED after successful requests', async () => {
      const fn = vi.fn()
        .mockRejectedValueOnce(new Error('fail'))
        .mockRejectedValueOnce(new Error('fail'))
        .mockRejectedValueOnce(new Error('fail'))
        .mockResolvedValue('success');

      // Open circuit
      for (let i = 0; i < 3; i++) {
        await expect(circuitBreaker.execute(fn)).rejects.toThrow('fail');
      }

      // Manually transition to HALF_OPEN for testing
      circuitBreaker._transitionTo(CircuitState.HALF_OPEN);

      // Execute successful requests
      for (let i = 0; i < 2; i++) {
        await circuitBreaker.execute(fn);
      }

      expect(circuitBreaker.getState()).toBe(CircuitState.CLOSED);
    });

    it('should emit state change events', async () => {
      const stateChangeSpy = vi.fn();
      circuitBreaker.on('stateChange', stateChangeSpy);

      const fn = vi.fn().mockRejectedValue(new Error('fail'));

      // Open circuit
      for (let i = 0; i < 3; i++) {
        await expect(circuitBreaker.execute(fn)).rejects.toThrow('fail');
      }

      expect(stateChangeSpy).toHaveBeenCalledWith({
        from: CircuitState.CLOSED,
        to: CircuitState.OPEN
      });
    });
  });

  describe('metrics', () => {
    it('should track request metrics', async () => {
      const fn = vi.fn()
        .mockResolvedValueOnce('success')
        .mockRejectedValueOnce(new Error('fail'))
        .mockResolvedValueOnce('success');

      await circuitBreaker.execute(fn);
      await expect(circuitBreaker.execute(fn)).rejects.toThrow('fail');
      await circuitBreaker.execute(fn);

      const metrics = circuitBreaker.getMetrics();

      expect(metrics.totalRequests).toBe(3);
      expect(metrics.totalSuccesses).toBe(2);
      expect(metrics.totalFailures).toBe(1);
    });

    it('should calculate error rate', async () => {
      const fn = vi.fn()
        .mockRejectedValueOnce(new Error('fail'))
        .mockResolvedValueOnce('success');

      await expect(circuitBreaker.execute(fn)).rejects.toThrow('fail');
      await circuitBreaker.execute(fn);

      const metrics = circuitBreaker.getMetrics();

      expect(metrics.errorRate).toBe(50); // 1 failure out of 2 requests
    });
  });

  describe('force state changes', () => {
    it('should force circuit open', () => {
      circuitBreaker.forceOpen();
      expect(circuitBreaker.getState()).toBe(CircuitState.OPEN);
    });

    it('should force circuit closed', async () => {
      const fn = vi.fn().mockRejectedValue(new Error('fail'));

      // Open circuit
      for (let i = 0; i < 3; i++) {
        await expect(circuitBreaker.execute(fn)).rejects.toThrow('fail');
      }

      circuitBreaker.forceClosed();
      expect(circuitBreaker.getState()).toBe(CircuitState.CLOSED);
    });

    it('should reset circuit breaker', async () => {
      const fn = vi.fn().mockRejectedValue(new Error('fail'));

      // Open circuit
      for (let i = 0; i < 3; i++) {
        await expect(circuitBreaker.execute(fn)).rejects.toThrow('fail');
      }

      circuitBreaker.reset();

      expect(circuitBreaker.getState()).toBe(CircuitState.CLOSED);
      expect(circuitBreaker.failures).toBe(0);
      expect(circuitBreaker.successes).toBe(0);
    });
  });

  describe('cleanup', () => {
    it('should cleanup resources', () => {
      const listener = vi.fn();
      circuitBreaker.on('stateChange', listener);

      circuitBreaker.cleanup();

      expect(circuitBreaker.listenerCount('stateChange')).toBe(0);
    });
  });
});
