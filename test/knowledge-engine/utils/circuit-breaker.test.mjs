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
  });

  describe('execute', () => {
    it('should execute function successfully when circuit is closed', async () => {
      const result = await breaker.execute(async () => 'success');
      expect(result).toBe('success');
      expect(breaker.state).toBe(CircuitState.CLOSED);
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

    it('should transition from HALF_OPEN to CLOSED on success', async () => {
      breaker.halfOpen();
      expect(breaker.state).toBe(CircuitState.HALF_OPEN);

      // Two successes needed
      await breaker.execute(async () => 'success');
      await breaker.execute(async () => 'success');
      expect(breaker.state).toBe(CircuitState.CLOSED);
    });
  });

  describe('trip', () => {
    it('should open the circuit', () => {
      breaker.trip();
      expect(breaker.state).toBe(CircuitState.OPEN);
    });
  });

  describe('reset', () => {
    it('should close the circuit', () => {
      breaker.trip();
      breaker.reset();
      expect(breaker.state).toBe(CircuitState.CLOSED);
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
  });
});

describe('CircuitOpenError', () => {
  it('should have correct name', () => {
    const error = new CircuitOpenError('test', 'circuit-name', Date.now());
    expect(error.name).toBe('CircuitOpenError');
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
  });

  describe('get', () => {
    it('should return existing breaker', () => {
      registry.getOrCreate('test');
      expect(registry.get('test')).toBeInstanceOf(CircuitBreaker);
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
});

describe('defaultRegistry', () => {
  it('should be a CircuitBreakerRegistry instance', () => {
    expect(defaultRegistry).toBeInstanceOf(CircuitBreakerRegistry);
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
});
