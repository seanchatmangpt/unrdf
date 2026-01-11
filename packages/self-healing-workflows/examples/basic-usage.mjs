/**
 * @file Basic self-healing workflows example
 * @description Demonstrates basic usage of self-healing engine
 */

import { SelfHealingEngine } from '../src/index.mjs';

// Create engine with default configuration
const engine = new SelfHealingEngine({
  retry: {
    maxAttempts: 3,
    initialDelay: 1000,
    backoffMultiplier: 2
  },
  circuitBreaker: {
    failureThreshold: 5,
    resetTimeout: 30000
  }
});

// Example 1: Basic retry on network errors
console.log('Example 1: Basic retry');
try {
  const result = await engine.execute(async () => {
    // Simulated API call that might fail
    if (Math.random() < 0.3) {
      throw new Error('ECONNREFUSED: Connection refused');
    }
    return { data: 'Success!' };
  });

  console.log('Result:', result);
} catch (error) {
  console.error('Failed after retries:', error.message);
}

// Example 2: Using fallback
console.log('\nExample 2: Fallback strategy');
const resultWithFallback = await engine.execute(
  async () => {
    throw new Error('Service unavailable');
  },
  {
    fallback: () => ({ data: 'Cached data' })
  }
);

console.log('Result with fallback:', resultWithFallback);

// Example 3: Get statistics
console.log('\nExample 3: Recovery statistics');
const stats = engine.getStats();
console.log('Success rate:', (stats.successRate * 100).toFixed(1) + '%');
console.log('Total attempts:', stats.totalAttempts);
console.log('Successful recoveries:', stats.successfulRecoveries);
console.log('Errors by category:', stats.errorsByCategory);

// Example 4: Health monitoring
console.log('\nExample 4: Health monitoring');
engine.startHealthMonitoring();

engine.onHealthChange((healthResult) => {
  console.log('Health status:', healthResult.status);
  console.log('Checks:', healthResult.checks.map(c => `${c.name}: ${c.status}`));
});

const health = await engine.getHealth();
console.log('Current health:', health.status);

engine.stopHealthMonitoring();

// Example 5: Custom error pattern
console.log('\nExample 5: Custom error pattern');
engine.addErrorPattern({
  name: 'RateLimitError',
  category: 'dependency',
  severity: 'medium',
  pattern: /rate limit|429/i
});

try {
  await engine.execute(async () => {
    throw new Error('Rate limit exceeded: 429');
  });
} catch (error) {
  console.log('Caught rate limit error');
}

// Example 6: Circuit breaker status
console.log('\nExample 6: Circuit breaker');
console.log('Circuit breaker state:', engine.getCircuitBreakerState());

// Example 7: Comprehensive status
console.log('\nExample 7: Engine status');
const status = engine.getStatus();
console.log('Active recoveries:', status.activeRecoveries);
console.log('Circuit breaker stats:', status.circuitBreaker);

console.log('\nSelf-healing engine examples completed!');
