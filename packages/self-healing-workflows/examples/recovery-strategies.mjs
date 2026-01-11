/**
 * @file Recovery strategies example
 * @description Demonstrates all recovery strategies
 */

import { SelfHealingEngine, createRetryStrategy } from '../src/index.mjs';

const engine = new SelfHealingEngine();

console.log('Recovery Strategies Demonstration\n');

// Strategy 1: Immediate retry (3 attempts)
console.log('1. Immediate Retry (3 attempts)');
const immediateRetry = createRetryStrategy({
  maxAttempts: 3,
  initialDelay: 0,
  backoffMultiplier: 1
});

let attempt1 = 0;
try {
  await immediateRetry.execute(async () => {
    attempt1++;
    console.log(`  Attempt ${attempt1}`);
    if (attempt1 < 3) throw new Error('Fail');
    return 'Success';
  });
  console.log('  ✓ Succeeded after retries\n');
} catch (e) {
  console.log('  ✗ Failed\n');
}

// Strategy 2: Exponential backoff (2s, 4s, 8s, 16s)
console.log('2. Exponential Backoff (2s, 4s, 8s)');
const expRetry = createRetryStrategy({
  maxAttempts: 4,
  initialDelay: 2000,
  maxDelay: 16000,
  backoffMultiplier: 2,
  jitter: false
});

for (let i = 1; i <= 4; i++) {
  const delay = expRetry.calculateDelay(i);
  console.log(`  Attempt ${i}: delay = ${delay}ms`);
}
console.log();

// Strategy 3: Circuit breaker
console.log('3. Circuit Breaker (fail fast after 5 failures)');
let cbAttempt = 0;
for (let i = 0; i < 7; i++) {
  try {
    await engine.execute(async () => {
      cbAttempt++;
      throw new Error('Service down');
    });
  } catch (error) {
    console.log(`  Attempt ${i + 1}: ${error.message}`);
  }
}
console.log(`  Circuit state: ${engine.getCircuitBreakerState()}\n`);

// Reset for next examples
engine.resetCircuitBreaker();

// Strategy 4: Compensating transaction
console.log('4. Compensating Transaction');
const transactions = [];

try {
  await engine.execute(
    async () => {
      transactions.push('CREATE_ORDER');
      transactions.push('CHARGE_CARD');
      throw new Error('Inventory unavailable');
    },
    {
      compensationFn: async () => {
        console.log('  Rolling back transactions...');
        while (transactions.length > 0) {
          const tx = transactions.pop();
          console.log(`  Compensating: ${tx}`);
        }
      }
    }
  );
} catch (error) {
  console.log(`  ✓ Compensated successfully\n`);
}

// Strategy 5: Skip and continue
console.log('5. Skip and Continue');
const items = ['item1', 'item2', 'item3-broken', 'item4'];

for (const item of items) {
  try {
    if (item.includes('broken')) {
      throw new Error('Validation failed');
    }
    console.log(`  Processed: ${item}`);
  } catch (error) {
    console.log(`  Skipped: ${item} (${error.message})`);
    continue; // Skip and continue
  }
}
console.log();

// Strategy 6: Manual intervention
console.log('6. Manual Intervention Required');
try {
  await engine.execute(
    async () => {
      throw new Error('Database corruption detected');
    },
    {
      notificationFn: async (notification) => {
        console.log('  Alert sent to operations team');
        console.log('  Type:', notification.type);
        console.log('  Error:', notification.error.message);
        console.log('  Waiting for manual fix...');
      }
    }
  );
} catch (error) {
  console.log('  ✓ Manual intervention triggered\n');
}

// Strategy comparison
console.log('7. Strategy Comparison');
console.log('━'.repeat(60));
console.log('Strategy              | Use Case');
console.log('━'.repeat(60));
console.log('Immediate Retry       | Quick transient failures');
console.log('Exponential Backoff   | Service overload, rate limits');
console.log('Circuit Breaker       | Cascading failures prevention');
console.log('Compensate            | Distributed transactions');
console.log('Skip and Continue     | Non-critical batch processing');
console.log('Manual Intervention   | Critical errors needing human input');
console.log('━'.repeat(60));

console.log('\nRecovery strategies demonstration completed!');
