/**
 * @file Circuit Breaker Tests
 * @module test/knowledge-engine/utils/circuit-breaker.test
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  CircuitBreaker,
  CircuitState,
  CircuitOpenError,
  CircuitBreakerRegistry,
  createCircuitBreaker,
  withCircuitBreaker,
  defaultRegistry,
} from '../../../src/knowledge-engine/utils/circuit-breaker.mjs';

describe('CircuitBreaker', () => {
  let breaker;

  beforeEach(() => {
    breaker = new CircuitBreaker({
      failureThreshold: 3,
      resetTimeout: 1000,
      halfOpenMaxCalls: 2,
      successThreshold: 2,
      name: 'test-breaker',
    });
  });

  describe('initialization', () => {
    it('should initialize in CLOSED state', () => {
      expect(breaker.state).toBe(CircuitState.CLOSED);
      expect(breaker.failureCount).toBe(0);
    });

    it('should use default configuration when none provided', () => {
      const defaultBreaker = new CircuitBreaker();
      expect(defaultBreaker.failureThreshold).toBe(5);
      expect(defaultBreaker.resetTimeout).toBe(30000);
      expect(defaultBreaker.halfOpenMaxCalls).toBe(3);
    });

    it('should override defaults with provided config', () => {
      expect(breaker.failureThreshold).toBe(3);
      expect(breaker.resetTimeout).toBe(1000);
      expect(breaker.name).toBe('test-breaker');
    });
  });

  describe('execute', () => {
    it('should execute function successfully when circuit is closed', async () => {
      const result = await breaker.execute(async () => 'success');
      expect(result).toBe('success');
      expect(breaker.state).toBe(CircuitState.CLOSED);
    });

    it('should record success and reset failure count', async () => {
      breaker.failureCount = 2;
      await breaker.execute(async () => 'success');
      expect(breaker.failureCount).toBe(0);
    });

    it('should record failure and increment failure count', async () => {
      try {
        await breaker.execute(async () => {
          throw new Error('test error');
        });
      } catch (e) {
        // Expected
      }
      expect(breaker.failureCount).toBe(1);
    });

    it('should trip circuit after reaching failure threshold', async () => {
      for (let i = 0; i < 3; i++) {
        try {
          await breaker.execute(async () => {
            throw new Error('test error');
          });
        } catch (e) {
          // Expected
        }
      }
      expect(breaker.state).toBe(CircuitState.OPEN);
    });

    it('should throw CircuitOpenError when circuit is open', async () => {
      breaker.trip();

      await expect(breaker.execute(async () => 'should not execute')).rejects.toThrow(
        CircuitOpenError
      );
    });

    it('should track metrics', async () => {
      await breaker.execute(async () => 'success');
      expect(breaker.metrics.totalCalls).toBe(1);
      expect(breaker.metrics.successfulCalls).toBe(1);
    });
  });

  describe('state transitions', () => {
    it('should transition from CLOSED to OPEN on failures', async () => {
      expect(breaker.state).toBe(CircuitState.CLOSED);

      for (let i = 0; i < 3; i++) {
        try {
          await breaker.execute(async () => {
            throw new Error('fail');
          });
        } catch (e) {}
      }

      expect(breaker.state).toBe(CircuitState.OPEN);
    });

    it('should transition from OPEN to HALF_OPEN after timeout', async () => {
      breaker.trip();
      expect(breaker.state).toBe(CircuitState.OPEN);

      // Simulate time passing
      breaker.lastStateChange = Date.now() - 2000;

      // Check state transition
      breaker._checkStateTransition();
      expect(breaker.state).toBe(CircuitState.HALF_OPEN);
    });

    it('should transition from HALF_OPEN to CLOSED on success', async () => {
      breaker.halfOpen();
      expect(breaker.state).toBe(CircuitState.HALF_OPEN);

      // Two successes needed
      await breaker.execute(async () => 'success');
      expect(breaker.state).toBe(CircuitState.HALF_OPEN);

      await breaker.execute(async () => 'success');
      expect(breaker.state).toBe(CircuitState.CLOSED);
    });

    it('should transition from HALF_OPEN to OPEN on failure', async () => {
      breaker.halfOpen();

      try {
        await breaker.execute(async () => {
          throw new Error('fail');
        });
      } catch (e) {}

      expect(breaker.state).toBe(CircuitState.OPEN);
    });
  });

  describe('trip', () => {
    it('should open the circuit', () => {
      breaker.trip();
      expect(breaker.state).toBe(CircuitState.OPEN);
    });

    it('should increment state changes metric', () => {
      breaker.trip();
      expect(breaker.metrics.stateChanges).toBe(1);
    });

    it('should call onStateChange callback', () => {
      const callback = vi.fn();
      breaker.onStateChange = callback;

      breaker.trip();

      expect(callback).toHaveBeenCalledWith(
        expect.objectContaining({
          from: CircuitState.CLOSED,
          to: CircuitState.OPEN,
        })
      );
    });
  });

  describe('reset', () => {
    it('should close the circuit', () => {
      breaker.trip();
      breaker.reset();
      expect(breaker.state).toBe(CircuitState.CLOSED);
    });

    it('should reset all counters', () => {
      breaker.failureCount = 5;
      breaker.successCount = 3;
      breaker.halfOpenCalls = 2;

      breaker.reset();

      expect(breaker.failureCount).toBe(0);
      expect(breaker.successCount).toBe(0);
      expect(breaker.halfOpenCalls).toBe(0);
    });
  });

  describe('halfOpen', () => {
    it('should set state to HALF_OPEN', () => {
      breaker.trip();
      breaker.halfOpen();
      expect(breaker.state).toBe(CircuitState.HALF_OPEN);
    });

    it('should reset success count and half-open calls', () => {
      breaker.trip();
      breaker.halfOpen();
      expect(breaker.successCount).toBe(0);
      expect(breaker.halfOpenCalls).toBe(0);
    });
  });

  describe('getStatus', () => {
    it('should return current status', () => {
      const status = breaker.getStatus();

      expect(status).toEqual(
        expect.objectContaining({
          name: 'test-breaker',
          state: CircuitState.CLOSED,
          failureCount: 0,
          config: expect.objectContaining({
            failureThreshold: 3,
          }),
        })
      );
    });
  });

  describe('isHealthy/isOpen/isHalfOpen', () => {
    it('isHealthy should return true when closed', () => {
      expect(breaker.isHealthy()).toBe(true);
    });

    it('isOpen should return true when open', () => {
      breaker.trip();
      expect(breaker.isOpen()).toBe(true);
    });

    it('isHalfOpen should return true when half-open', () => {
      breaker.halfOpen();
      expect(breaker.isHalfOpen()).toBe(true);
    });
  });

  describe('half-open call limiting', () => {
    it('should limit calls in half-open state', async () => {
      breaker.halfOpen();

      // First two calls should work
      await breaker.execute(async () => 'success');
      await breaker.execute(async () => 'success');

      // Third call should be rejected (limit is 2)
      // But since we succeeded twice, circuit should be closed now
      expect(breaker.state).toBe(CircuitState.CLOSED);
    });

    it('should reject calls when half-open limit reached without success', async () => {
      breaker.halfOpen();

      // Max 2 calls, need 2 successes
      // Simulate reaching limit without transitioning
      breaker.halfOpenCalls = 2;
      breaker.successCount = 0;

      await expect(breaker.execute(async () => 'should fail')).rejects.toThrow(CircuitOpenError);
    });
  });
});

describe('CircuitOpenError', () => {
  it('should have correct name', () => {
    const error = new CircuitOpenError('test', 'circuit-name', Date.now());
    expect(error.name).toBe('CircuitOpenError');
  });

  it('should include circuit name', () => {
    const error = new CircuitOpenError('test', 'my-circuit', Date.now());
    expect(error.circuitName).toBe('my-circuit');
  });

  it('should include last failure time', () => {
    const timestamp = Date.now();
    const error = new CircuitOpenError('test', 'circuit', timestamp);
    expect(error.lastFailureTime).toBe(timestamp);
  });
});

describe('CircuitBreakerRegistry', () => {
  let registry;

  beforeEach(() => {
    registry = new CircuitBreakerRegistry();
  });

  describe('getOrCreate', () => {
    it('should create a new breaker if not exists', () => {
      const breaker = registry.getOrCreate('test', { failureThreshold: 5 });
      expect(breaker).toBeInstanceOf(CircuitBreaker);
      expect(breaker.name).toBe('test');
    });

    it('should return existing breaker if exists', () => {
      const first = registry.getOrCreate('test');
      const second = registry.getOrCreate('test');
      expect(first).toBe(second);
    });
  });

  describe('get', () => {
    it('should return undefined for non-existent breaker', () => {
      expect(registry.get('nonexistent')).toBeUndefined();
    });

    it('should return existing breaker', () => {
      registry.getOrCreate('test');
      expect(registry.get('test')).toBeInstanceOf(CircuitBreaker);
    });
  });

  describe('has', () => {
    it('should return false for non-existent breaker', () => {
      expect(registry.has('nonexistent')).toBe(false);
    });

    it('should return true for existing breaker', () => {
      registry.getOrCreate('test');
      expect(registry.has('test')).toBe(true);
    });
  });

  describe('remove', () => {
    it('should remove existing breaker', () => {
      registry.getOrCreate('test');
      expect(registry.remove('test')).toBe(true);
      expect(registry.has('test')).toBe(false);
    });

    it('should return false for non-existent breaker', () => {
      expect(registry.remove('nonexistent')).toBe(false);
    });
  });

  describe('getAllStatuses', () => {
    it('should return all breaker statuses', () => {
      registry.getOrCreate('breaker1');
      registry.getOrCreate('breaker2');

      const statuses = registry.getAllStatuses();

      expect(statuses).toHaveProperty('breaker1');
      expect(statuses).toHaveProperty('breaker2');
    });
  });

  describe('resetAll', () => {
    it('should reset all breakers', () => {
      const b1 = registry.getOrCreate('b1');
      const b2 = registry.getOrCreate('b2');

      b1.trip();
      b2.trip();

      registry.resetAll();

      expect(b1.state).toBe(CircuitState.CLOSED);
      expect(b2.state).toBe(CircuitState.CLOSED);
    });
  });

  describe('getHealthSummary', () => {
    it('should return health summary', () => {
      registry.getOrCreate('healthy1');
      registry.getOrCreate('healthy2');
      const failing = registry.getOrCreate('failing');
      failing.trip();

      const summary = registry.getHealthSummary();

      expect(summary.total).toBe(3);
      expect(summary.healthy).toBe(2);
      expect(summary.open).toBe(1);
      expect(summary.healthPercent).toBeCloseTo(66.67, 0);
    });

    it('should return 100% health for empty registry', () => {
      const summary = registry.getHealthSummary();
      expect(summary.healthPercent).toBe(100);
    });
  });
});

describe('createCircuitBreaker', () => {
  it('should create a circuit breaker with name', () => {
    const breaker = createCircuitBreaker('my-breaker', {
      failureThreshold: 10,
    });
    expect(breaker.name).toBe('my-breaker');
    expect(breaker.failureThreshold).toBe(10);
  });
});

describe('withCircuitBreaker', () => {
  it('should wrap function with circuit breaker protection', async () => {
    const breaker = new CircuitBreaker({ name: 'wrapper-test' });
    const fn = vi.fn().mockResolvedValue('result');

    const wrapped = withCircuitBreaker(fn, breaker);
    const result = await wrapped('arg1', 'arg2');

    expect(result).toBe('result');
    expect(fn).toHaveBeenCalledWith('arg1', 'arg2');
  });

  it('should use circuit breaker protection', async () => {
    const breaker = new CircuitBreaker({ name: 'wrapper-test' });
    breaker.trip();

    const fn = vi.fn().mockResolvedValue('result');
    const wrapped = withCircuitBreaker(fn, breaker);

    await expect(wrapped()).rejects.toThrow(CircuitOpenError);
    expect(fn).not.toHaveBeenCalled();
  });
});

describe('defaultRegistry', () => {
  it('should be a CircuitBreakerRegistry instance', () => {
    expect(defaultRegistry).toBeInstanceOf(CircuitBreakerRegistry);
  });
});

describe('custom failure detection', () => {
  it('should use custom isFailure function', async () => {
    const breaker = new CircuitBreaker({
      name: 'custom-failure',
      failureThreshold: 1,
      isFailure: error => error.message === 'critical',
    });

    // Non-critical error should not count
    try {
      await breaker.execute(async () => {
        throw new Error('not critical');
      });
    } catch (e) {}

    expect(breaker.state).toBe(CircuitState.CLOSED);

    // Critical error should count
    try {
      await breaker.execute(async () => {
        throw new Error('critical');
      });
    } catch (e) {}

    expect(breaker.state).toBe(CircuitState.OPEN);
  });
});

describe('metrics tracking', () => {
  it('should track all metrics correctly', async () => {
    const breaker = new CircuitBreaker({
      name: 'metrics-test',
      failureThreshold: 10,
    });

    // Success
    await breaker.execute(async () => 'ok');

    // Failure
    try {
      await breaker.execute(async () => {
        throw new Error('fail');
      });
    } catch (e) {}

    expect(breaker.metrics.totalCalls).toBe(2);
    expect(breaker.metrics.successfulCalls).toBe(1);
    expect(breaker.metrics.failedCalls).toBe(1);
  });

  it('should track rejected calls', async () => {
    const breaker = new CircuitBreaker({ name: 'reject-test' });
    breaker.trip();

    try {
      await breaker.execute(async () => 'should not run');
    } catch (e) {}

    expect(breaker.metrics.rejectedCalls).toBe(1);
  });

  it('should allow resetting metrics', () => {
    const breaker = new CircuitBreaker({ name: 'reset-metrics-test' });
    breaker.metrics.totalCalls = 100;

    breaker.resetMetrics();

    expect(breaker.metrics.totalCalls).toBe(0);
  });
});
