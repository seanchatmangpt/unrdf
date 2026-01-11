/**
 * @file Circuit Breaker Tests (Fast)
 * @module test/knowledge-engine/utils/circuit-breaker.test
 * @description Minimal fast tests for CircuitBreaker state validation
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  CircuitBreaker,
  CircuitState,
  CircuitOpenError,
} from '../../../src/knowledge-engine/utils/circuit-breaker.mjs';

describe('CircuitBreaker', () => {
  let breaker;

  beforeEach(() => {
    breaker = new CircuitBreaker({
      failureThreshold: 2,
      resetTimeout: 50,
      name: 'test-breaker',
    });
  });

  it('should initialize in CLOSED state', () => {
    expect(breaker.state).toBe(CircuitState.CLOSED);
    expect(breaker.failureCount).toBe(0);
  });

  it('should trip circuit on threshold failures and block execution', async () => {
    // Trigger 2 failures
    for (let i = 0; i < 2; i++) {
      try {
        await breaker.execute(async () => {
          throw new Error('fail');
        });
      } catch (e) {
        // Expected
      }
    }
    expect(breaker.state).toBe(CircuitState.OPEN);
    await expect(breaker.execute(async () => 'blocked')).rejects.toThrow(
      CircuitOpenError
    );
  });
});
