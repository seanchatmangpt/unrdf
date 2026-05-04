/**
 * @file Self-healing workflows test suite
 * @description Comprehensive tests for error recovery, retry, circuit breaker, and health monitoring
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  SelfHealingEngine,
  ErrorClassifier,
  RetryStrategy,
  CircuitBreaker,
  RecoveryActionExecutor,
  HealthMonitor,
  immediateRetry,
  exponentialRetry
} from '../src/index.mjs';

describe('ErrorClassifier', () => {
  let classifier;

  beforeEach(() => {
    classifier = new ErrorClassifier();
  });

  it('should classify network errors correctly', () => {
    const error = new Error('ECONNREFUSED: connection refused');
    const classified = classifier.classify(error);

    expect(classified.category).toBe('network');
    expect(classified.severity).toBe('medium');
    expect(classified.retryable).toBe(true);
    expect(classified.matchedPattern).toBe('NetworkError');
  });

  it('should classify timeout errors correctly', () => {
    const error = new Error('Operation timed out after 5000ms');
    const classified = classifier.classify(error);

    expect(classified.category).toBe('timeout');
    expect(classified.severity).toBe('medium');
    expect(classified.retryable).toBe(true);
  });

  it('should classify validation errors as non-retryable', () => {
    const error = new Error('Validation failed: invalid input');
    const classified = classifier.classify(error);

    expect(classified.category).toBe('validation');
    expect(classified.retryable).toBe(false);
  });

  it('should classify resource errors correctly', () => {
    const error = new Error('ENOMEM: out of memory');
    const classified = classifier.classify(error);

    expect(classified.category).toBe('resource');
    expect(classified.severity).toBe('high');
    expect(classified.retryable).toBe(true);
  });

  it('should classify unknown errors', () => {
    const error = new Error('Something went wrong');
    const classified = classifier.classify(error);

    expect(classified.category).toBe('unknown');
    expect(classified.retryable).toBe(false);
  });

  it('should add custom patterns', () => {
    classifier.addPattern({
      name: 'CustomError',
      category: 'network',
      severity: 'low',
      pattern: /custom error/i
    });

    const error = new Error('Custom error occurred');
    const classified = classifier.classify(error);

    expect(classified.matchedPattern).toBe('CustomError');
  });

  it('should classify batch of errors', () => {
    const errors = [
      new Error('ECONNREFUSED'),
      new Error('timeout'),
      new Error('validation')
    ];

    const classified = classifier.classifyBatch(errors);

    expect(classified).toHaveLength(3);
    expect(classified[0].category).toBe('network');
    expect(classified[1].category).toBe('timeout');
    expect(classified[2].category).toBe('validation');
  });

  it('should filter retryable errors', () => {
    const classified = [
      { category: 'network', retryable: true },
      { category: 'validation', retryable: false },
      { category: 'timeout', retryable: true }
    ];

    const retryable = classifier.filterRetryable(classified);

    expect(retryable).toHaveLength(2);
  });
});

describe('RetryStrategy', () => {
  it('should retry operation on failure', async () => {
    const retry = new RetryStrategy({ maxAttempts: 3, initialDelay: 10 });
    let attempts = 0;

    const operation = vi.fn(async () => {
      attempts++;
      if (attempts < 3) {
        throw new Error('Temporary failure');
      }
      return 'success';
    });

    const result = await retry.execute(operation);

    expect(result).toBe('success');
    expect(attempts).toBe(3);
  });

  it('should throw after max attempts exhausted', async () => {
    const retry = new RetryStrategy({ maxAttempts: 2, initialDelay: 10 });
    const operation = vi.fn(async () => {
      throw new Error('Permanent failure');
    });

    await expect(retry.execute(operation)).rejects.toThrow('Operation failed after 2 attempts');
    expect(operation).toHaveBeenCalledTimes(2);
  });

  it('should calculate exponential backoff delays', () => {
    const retry = new RetryStrategy({
      initialDelay: 1000,
      backoffMultiplier: 2,
      jitter: false
    });

    expect(retry.calculateDelay(1)).toBe(1000);
    expect(retry.calculateDelay(2)).toBe(2000);
    expect(retry.calculateDelay(3)).toBe(4000);
    expect(retry.calculateDelay(4)).toBe(8000);
  });

  it('should cap delay at maxDelay', () => {
    const retry = new RetryStrategy({
      initialDelay: 1000,
      maxDelay: 5000,
      backoffMultiplier: 2,
      jitter: false
    });

    expect(retry.calculateDelay(5)).toBe(5000);
    expect(retry.calculateDelay(10)).toBe(5000);
  });

  it('should add jitter to delays', () => {
    const retry = new RetryStrategy({
      initialDelay: 1000,
      jitter: true
    });

    const delay = retry.calculateDelay(1);
    expect(delay).toBeGreaterThanOrEqual(800); // -20%
    expect(delay).toBeLessThanOrEqual(1200); // +20%
  });

  it('should call onRetry callback', async () => {
    const retry = new RetryStrategy({ maxAttempts: 3, initialDelay: 10 });
    const onRetry = vi.fn();
    let attempts = 0;

    await retry.execute(
      async () => {
        attempts++;
        if (attempts < 2) throw new Error('Fail');
        return 'success';
      },
      { onRetry }
    );

    expect(onRetry).toHaveBeenCalledTimes(1);
  });

  it('should execute with metadata', async () => {
    const retry = new RetryStrategy({ maxAttempts: 3, initialDelay: 10 });
    let attempts = 0;

    const result = await retry.executeWithMetadata(async () => {
      attempts++;
      if (attempts < 2) throw new Error('Fail');
      return 'success';
    });

    expect(result.success).toBe(true);
    expect(result.result).toBe('success');
    expect(result.attempts).toBe(2);
    expect(result.retryHistory).toHaveLength(1);
  });

  it('should support immediate retry helper', async () => {
    let attempts = 0;

    const result = await immediateRetry(async () => {
      attempts++;
      if (attempts < 2) throw new Error('Fail');
      return 'success';
    });

    expect(result).toBe('success');
    expect(attempts).toBe(2);
  });

  it('should support exponential retry helper', async () => {
    let attempts = 0;

    const result = await exponentialRetry(async () => {
      attempts++;
      if (attempts < 2) throw new Error('Fail');
      return 'success';
    });

    expect(result).toBe('success');
  });
});

describe('CircuitBreaker', () => {
  it('should remain closed on successful operations', async () => {
    const breaker = new CircuitBreaker({ failureThreshold: 3 });

    await breaker.execute(async () => 'success');

    expect(breaker.getState()).toBe('closed');
  });

  it('should open after threshold failures', async () => {
    const breaker = new CircuitBreaker({ failureThreshold: 3, timeout: 100 });

    for (let i = 0; i < 3; i++) {
      try {
        await breaker.execute(async () => {
          throw new Error('Failure');
        });
      } catch (e) {
        // Expected
      }
    }

    expect(breaker.getState()).toBe('open');
  });

  it('should reject requests when open', async () => {
    const breaker = new CircuitBreaker({ failureThreshold: 2, timeout: 100 });

    // Trigger failures to open circuit
    for (let i = 0; i < 2; i++) {
      try {
        await breaker.execute(async () => {
          throw new Error('Failure');
        });
      } catch (e) {
        // Expected
      }
    }

    // Next request should be rejected
    await expect(
      breaker.execute(async () => 'success')
    ).rejects.toThrow('Circuit breaker is OPEN');
  });

  it('should use fallback when circuit is open', async () => {
    const breaker = new CircuitBreaker({ failureThreshold: 2, timeout: 100 });
    const fallback = vi.fn(() => 'fallback-result');

    // Trigger failures
    for (let i = 0; i < 2; i++) {
      try {
        await breaker.execute(async () => {
          throw new Error('Failure');
        });
      } catch (e) {
        // Expected
      }
    }

    // Use fallback
    const result = await breaker.execute(
      async () => 'primary',
      { fallback }
    );

    expect(result).toBe('fallback-result');
    expect(fallback).toHaveBeenCalled();
  });

  it('should transition to half-open after reset timeout', async () => {
    const breaker = new CircuitBreaker({
      failureThreshold: 2,
      resetTimeout: 50,
      timeout: 100
    });

    // Open circuit
    for (let i = 0; i < 2; i++) {
      try {
        await breaker.execute(async () => {
          throw new Error('Failure');
        });
      } catch (e) {
        // Expected
      }
    }

    expect(breaker.getState()).toBe('open');

    // Wait for reset timeout
    await new Promise(resolve => setTimeout(resolve, 60));

    // Next request should transition to half-open
    try {
      await breaker.execute(async () => 'success');
    } catch (e) {
      // May fail, but state should change
    }

    expect(['half-open', 'closed']).toContain(breaker.getState());
  });

  it('should close after success threshold in half-open', async () => {
    const breaker = new CircuitBreaker({
      failureThreshold: 2,
      successThreshold: 2,
      resetTimeout: 50,
      timeout: 100
    });

    // Open circuit
    for (let i = 0; i < 2; i++) {
      try {
        await breaker.execute(async () => {
          throw new Error('Failure');
        });
      } catch (e) {
        // Expected
      }
    }

    // Wait for reset
    await new Promise(resolve => setTimeout(resolve, 60));

    // Force to half-open and execute successes
    await breaker.execute(async () => 'success');
    await breaker.execute(async () => 'success');

    expect(breaker.getState()).toBe('closed');
  });

  it('should track statistics', async () => {
    const breaker = new CircuitBreaker({ timeout: 100 });

    await breaker.execute(async () => 'success');

    try {
      await breaker.execute(async () => {
        throw new Error('Failure');
      });
    } catch (e) {
      // Expected
    }

    const stats = breaker.getStats();

    expect(stats.totalRequests).toBe(2);
    expect(stats.successfulRequests).toBe(1);
    expect(stats.failedRequests).toBe(1);
    expect(stats.successRate).toBe(0.5);
  });

  it('should enforce timeout on operations', async () => {
    const breaker = new CircuitBreaker({ timeout: 50 });

    await expect(
      breaker.execute(async () => {
        await new Promise(resolve => setTimeout(resolve, 100));
        return 'success';
      })
    ).rejects.toThrow('Operation timed out');
  });
});

describe('RecoveryActionExecutor', () => {
  it('should register and execute recovery actions', async () => {
    const executor = new RecoveryActionExecutor();
    const mockAction = vi.fn(async () => ({ recovered: true }));

    executor.register({
      type: 'retry',
      name: 'test-action',
      execute: mockAction,
      priority: 50
    });

    const result = await executor.execute('retry', 'test-action', { data: 'test' });

    expect(result).toEqual({ recovered: true });
    expect(mockAction).toHaveBeenCalled();
  });

  it('should select best action based on priority', () => {
    const executor = new RecoveryActionExecutor();

    executor.register({
      type: 'retry',
      name: 'low-priority',
      execute: async () => ({}),
      priority: 10
    });

    executor.register({
      type: 'retry',
      name: 'high-priority',
      execute: async () => ({}),
      priority: 90
    });

    const selected = executor.selectAction({ category: 'network', retryable: true });

    expect(selected.action.name).toBe('high-priority');
  });

  it('should filter actions by condition', () => {
    const executor = new RecoveryActionExecutor();

    executor.register({
      type: 'retry',
      name: 'conditional',
      execute: async () => ({}),
      condition: (error) => error.category === 'network',
      priority: 90 // Higher priority than default retry-operation (80)
    });

    const networkError = { category: 'network', retryable: true };
    const timeoutError = { category: 'timeout', retryable: true };

    const networkSelected = executor.selectAction(networkError);
    const timeoutSelected = executor.selectAction(timeoutError);

    expect(networkSelected.action.name).toBe('conditional');
    expect(timeoutSelected.action.name).not.toBe('conditional');
  });

  it('should track action statistics', async () => {
    const executor = new RecoveryActionExecutor();

    await executor.execute('retry', 'retry-operation', {
      operation: async () => 'success',
      maxAttempts: 1
    });

    const stats = executor.getStats();
    const retryStats = stats['retry:retry-operation'];

    expect(retryStats.attempts).toBeGreaterThan(0);
    expect(retryStats.successes).toBeGreaterThan(0);
  });

  it('should execute skip action', async () => {
    const executor = new RecoveryActionExecutor();

    const result = await executor.execute('skip', 'skip-and-continue', {
      error: { message: 'Test error' }
    });

    expect(result.skipped).toBe(true);
  });

  it('should execute compensate action', async () => {
    const executor = new RecoveryActionExecutor();
    const compensationFn = vi.fn(async () => {});

    const result = await executor.execute('compensate', 'compensating-transaction', {
      compensationFn
    });

    expect(result.compensated).toBe(true);
    expect(compensationFn).toHaveBeenCalled();
  });
});

describe('HealthMonitor', () => {
  it('should register and execute health checks', async () => {
    const monitor = new HealthMonitor({ timeout: 100 });
    const checkFn = vi.fn(async () => {});

    monitor.registerCheck('test-check', checkFn);

    const result = await monitor.check();

    expect(result.status).toBe('healthy');
    expect(result.checks).toHaveLength(1);
    expect(checkFn).toHaveBeenCalled();
  });

  it('should detect unhealthy checks', async () => {
    const monitor = new HealthMonitor({ timeout: 100, unhealthyThreshold: 1 });

    monitor.registerCheck('failing-check', async () => {
      throw new Error('Check failed');
    });

    const result = await monitor.check();

    expect(result.checks[0].status).toBe('unhealthy');
  });

  it('should calculate overall status', async () => {
    const monitor = new HealthMonitor({ timeout: 100 });

    monitor.registerCheck('healthy-1', async () => {});
    monitor.registerCheck('healthy-2', async () => {});

    const result = await monitor.check();

    expect(result.status).toBe('healthy');
  });

  it('should report degraded status with partial failures', async () => {
    const monitor = new HealthMonitor({ timeout: 100 });

    monitor.registerCheck('healthy', async () => {});
    monitor.registerCheck('unhealthy', async () => {
      throw new Error('Failed');
    });

    const result = await monitor.check();

    expect(result.status).toBe('degraded');
  });

  it('should enforce check timeout', async () => {
    const monitor = new HealthMonitor({ timeout: 50 });

    monitor.registerCheck('slow-check', async () => {
      await new Promise(resolve => setTimeout(resolve, 100));
    });

    const result = await monitor.check();

    expect(result.checks[0].status).toBe('unhealthy');
    expect(result.checks[0].message).toContain('timeout');
  });

  it('should start and stop periodic checks', async () => {
    const monitor = new HealthMonitor({ interval: 50, timeout: 100 });
    const checkFn = vi.fn(async () => {});

    monitor.registerCheck('periodic', checkFn);

    monitor.start();
    await new Promise(resolve => setTimeout(resolve, 120));
    monitor.stop();

    expect(checkFn).toHaveBeenCalled();
  });

  it('should notify listeners on status change', async () => {
    const monitor = new HealthMonitor({ timeout: 100 });
    const listener = vi.fn();

    monitor.onStatusChange(listener);
    await monitor.check();

    expect(listener).toHaveBeenCalled();
  });
});

describe('SelfHealingEngine', () => {
  it('should execute operation successfully', async () => {
    const engine = new SelfHealingEngine();

    const result = await engine.execute(async () => {
      return 'success';
    });

    expect(result).toBe('success');
  });

  it('should retry on transient failures', async () => {
    const engine = new SelfHealingEngine({
      retry: { maxAttempts: 3, initialDelay: 10 }
    });

    let attempts = 0;

    const result = await engine.execute(async () => {
      attempts++;
      if (attempts < 3) {
        throw new Error('ECONNREFUSED');
      }
      return 'success';
    });

    expect(result).toBe('success');
    expect(attempts).toBe(3);
  });

  it('should use fallback on failure', async () => {
    const engine = new SelfHealingEngine({
      retry: { maxAttempts: 1, initialDelay: 10 }
    });

    const result = await engine.execute(
      async () => {
        throw new Error('Permanent failure');
      },
      {
        fallback: () => 'fallback-value'
      }
    );

    expect(result).toBe('fallback-value');
  });

  it('should track recovery statistics', async () => {
    const engine = new SelfHealingEngine();

    await engine.execute(async () => 'success');

    const stats = engine.getStats();

    expect(stats.totalAttempts).toBe(1);
    expect(stats.successfulRecoveries).toBe(1);
    expect(stats.successRate).toBe(1);
  });

  it('should enforce max concurrent recoveries', async () => {
    const engine = new SelfHealingEngine({ maxConcurrentRecoveries: 2 });

    const promises = [];
    for (let i = 0; i < 3; i++) {
      promises.push(
        engine.execute(async () => {
          await new Promise(resolve => setTimeout(resolve, 50));
          return 'success';
        }).catch(e => e)
      );
    }

    const results = await Promise.all(promises);
    const errors = results.filter(r => r instanceof Error);

    expect(errors.length).toBeGreaterThan(0);
  });

  it('should measure success rate above 85%', async () => {
    const engine = new SelfHealingEngine({
      retry: { maxAttempts: 3, initialDelay: 10 },
      maxConcurrentRecoveries: 150 // Allow all 100 operations to run
    });

    // Create operations that fail on first attempt but succeed on retry
    const operations = Array.from({ length: 100 }, (_, i) => {
      let attempts = 0;
      return async () => {
        attempts++;
        // 15% of operations fail on first attempt
        if (i < 15 && attempts === 1) {
          throw new Error('ECONNREFUSED');
        }
        return 'success';
      };
    });

    const results = await Promise.allSettled(
      operations.map(op => engine.execute(op))
    );

    const successful = results.filter(r => r.status === 'fulfilled').length;
    const successRate = successful / results.length;

    const stats = engine.getStats();

    // Should achieve >85% success rate with retries (100% in this case)
    expect(successRate).toBeGreaterThanOrEqual(0.85);
    expect(stats.successRate).toBeGreaterThanOrEqual(0.85);
  });

  it('should integrate with circuit breaker', async () => {
    const engine = new SelfHealingEngine({
      circuitBreaker: { failureThreshold: 3, timeout: 100 }
    });

    // Cause failures to open circuit
    for (let i = 0; i < 3; i++) {
      try {
        await engine.execute(async () => {
          throw new Error('Service unavailable');
        });
      } catch (e) {
        // Expected
      }
    }

    expect(engine.getCircuitBreakerState()).toBe('open');
  });

  it('should provide comprehensive status', () => {
    const engine = new SelfHealingEngine();

    const status = engine.getStatus();

    expect(status).toHaveProperty('stats');
    expect(status).toHaveProperty('circuitBreaker');
    expect(status).toHaveProperty('health');
    expect(status).toHaveProperty('activeRecoveries');
  });

  it('should support custom error patterns', () => {
    const engine = new SelfHealingEngine();

    engine.addErrorPattern({
      name: 'CustomAPI',
      category: 'dependency',
      severity: 'high',
      pattern: /API_ERROR/
    });

    const patterns = engine.classifier.getPatterns();
    const custom = patterns.find(p => p.name === 'CustomAPI');

    expect(custom).toBeDefined();
  });

  it('should support custom recovery actions', () => {
    const engine = new SelfHealingEngine();
    const customAction = vi.fn(async () => ({ custom: true }));

    engine.addRecoveryAction({
      type: 'fallback',
      name: 'custom-action',
      execute: customAction,
      priority: 70
    });

    const actions = engine.recoveryExecutor.getActions();
    const custom = actions.find(a => a.name === 'custom-action');

    expect(custom).toBeDefined();
  });

  it('should monitor health status', async () => {
    const engine = new SelfHealingEngine();

    engine.startHealthMonitoring();

    const health = await engine.getHealth();

    expect(health.status).toBeDefined();
    expect(health.checks.length).toBeGreaterThan(0);

    engine.stopHealthMonitoring();
  });
});
