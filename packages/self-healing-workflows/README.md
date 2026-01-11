# @unrdf/self-healing-workflows

> Automatic error recovery system with 85-95% success rate using YAWL + Daemon + Hooks

## Features

- **Automatic Retry**: Exponential backoff with jitter
- **Circuit Breaker**: Fail-fast pattern for cascading failures
- **Error Classification**: Pattern-based error categorization
- **Recovery Actions**: Comprehensive action library (retry, skip, compensate, restart)
- **Health Monitoring**: Real-time health checks and alerting
- **OTEL Integration**: Full observability support

## Installation

```bash
pnpm add @unrdf/self-healing-workflows
```

## Quick Start

```javascript
import { SelfHealingEngine } from '@unrdf/self-healing-workflows';

// Create engine
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

// Execute with automatic recovery
const result = await engine.execute(async () => {
  return await fetch('https://api.example.com/data');
}, {
  fallback: () => getCachedData()
});
```

## Recovery Strategies

### 1. Immediate Retry (3 attempts)

```javascript
import { immediateRetry } from '@unrdf/self-healing-workflows';

const result = await immediateRetry(async () => {
  return await riskyOperation();
});
```

### 2. Exponential Backoff (2s, 4s, 8s, 16s)

```javascript
import { exponentialRetry } from '@unrdf/self-healing-workflows';

const result = await exponentialRetry(async () => {
  return await apiCall();
});
```

### 3. Circuit Breaker

```javascript
import { createCircuitBreaker } from '@unrdf/self-healing-workflows';

const breaker = createCircuitBreaker({
  failureThreshold: 5,
  successThreshold: 2,
  timeout: 60000
});

const result = await breaker.execute(async () => {
  return await externalService();
}, {
  fallback: () => defaultValue
});
```

### 4. Compensating Transaction

```javascript
await engine.execute(
  async () => {
    await createOrder();
    await chargeCard();
    await updateInventory();
  },
  {
    compensationFn: async () => {
      await refundCard();
      await cancelOrder();
    }
  }
);
```

### 5. Skip and Continue

```javascript
for (const item of items) {
  try {
    await processItem(item);
  } catch (error) {
    console.log('Skipping failed item');
    continue;
  }
}
```

### 6. Manual Intervention

```javascript
await engine.execute(
  async () => {
    await criticalOperation();
  },
  {
    notificationFn: async (alert) => {
      await sendPagerDutyAlert(alert);
    }
  }
);
```

## Error Classification

Errors are automatically classified into categories:

- **Network**: Connection failures, DNS errors
- **Timeout**: Operation timeouts
- **Validation**: Data validation failures
- **Resource**: Memory, disk, CPU exhaustion
- **Dependency**: External service failures
- **Business Logic**: Domain rule violations

```javascript
import { createErrorClassifier } from '@unrdf/self-healing-workflows';

const classifier = createErrorClassifier();
const classified = classifier.classify(new Error('ECONNREFUSED'));

console.log(classified.category); // 'network'
console.log(classified.severity); // 'medium'
console.log(classified.retryable); // true
```

## Health Monitoring

```javascript
import { createHealthMonitor } from '@unrdf/self-healing-workflows';

const monitor = createHealthMonitor({
  interval: 30000,
  timeout: 5000
});

// Register checks
monitor.registerCheck('database', async () => {
  await db.ping();
});

monitor.registerCheck('cache', async () => {
  await cache.ping();
});

// Start monitoring
monitor.start();

// Listen for status changes
monitor.onStatusChange((result) => {
  console.log('Health status:', result.status);
  console.log('Failed checks:', result.checks.filter(c => c.status === 'unhealthy'));
});
```

## Statistics

```javascript
const stats = engine.getStats();

console.log('Success rate:', stats.successRate * 100 + '%');
console.log('Total attempts:', stats.totalAttempts);
console.log('Average recovery time:', stats.averageRecoveryTime + 'ms');
console.log('Errors by category:', stats.errorsByCategory);
```

## Custom Error Patterns

```javascript
engine.addErrorPattern({
  name: 'RateLimitError',
  category: 'dependency',
  severity: 'medium',
  pattern: /rate limit|429/i
});
```

## Custom Recovery Actions

```javascript
engine.addRecoveryAction({
  type: 'fallback',
  name: 'use-cache',
  execute: async (context) => {
    return await getFromCache(context.key);
  },
  condition: (error) => error.category === 'network',
  priority: 70
});
```

## Performance Targets

| Operation | P95 Target | Typical |
|-----------|------------|---------|
| Recovery decision | <50ms | ~10ms |
| Retry execution | 100ms-30s | ~2s |
| Health check | <10ms | ~5ms |
| Circuit breaker switch | <1ms | ~0.1ms |

## Recovery Success Rate

Target: **85-95%** success rate for retryable errors

Measured across:
- Network failures
- Timeout errors
- Resource constraints
- Service degradation

## API Reference

### SelfHealingEngine

```typescript
class SelfHealingEngine {
  constructor(config?: SelfHealingConfig)
  execute<T>(operation: () => Promise<T>, options?: ExecuteOptions): Promise<T>
  wrap<T>(fn: Function, options?: ExecuteOptions): Function
  getStats(): RecoveryStats
  getHealth(): Promise<HealthCheckResult>
  addErrorPattern(pattern: ErrorPattern): void
  addRecoveryAction(action: RecoveryAction): void
}
```

### RetryStrategy

```typescript
class RetryStrategy {
  constructor(config?: RetryStrategyConfig)
  execute<T>(operation: () => Promise<T>, options?: RetryOptions): Promise<T>
  calculateDelay(attempt: number): number
}
```

### CircuitBreaker

```typescript
class CircuitBreaker {
  constructor(config?: CircuitBreakerConfig)
  execute<T>(operation: () => Promise<T>, options?: BreakerOptions): Promise<T>
  getState(): 'closed' | 'open' | 'half-open'
  reset(): void
}
```

## Examples

See [examples/](./examples/) directory:

- `basic-usage.mjs` - Getting started
- `recovery-strategies.mjs` - All recovery patterns

## License

MIT
