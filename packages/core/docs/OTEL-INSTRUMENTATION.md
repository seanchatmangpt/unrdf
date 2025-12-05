# OpenTelemetry Instrumentation Guide

## Overview

This guide explains how to add OpenTelemetry (OTEL) instrumentation to transaction manager and circuit breaker when needed. The core modules are intentionally kept **pure** (without observability code) to maintain clarity and testability.

## Why Separate OTEL from Business Logic?

- **Purity**: Business logic remains focused on its core responsibility
- **Testability**: Code is easier to test without observability concerns
- **Flexibility**: Developers choose when and where to add instrumentation
- **Performance**: No observability overhead in production unless explicitly added

## Adding OTEL to TransactionManager

### Basic Example

```javascript
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { TransactionManager } from '@unrdf/core';

const tracer = trace.getTracer('my-app');

class InstrumentedTransactionManager extends TransactionManager {
  async apply(store, delta, options = {}) {
    return tracer.startActiveSpan('transaction.apply', async span => {
      span.setAttributes({
        'transaction.delta.additions': delta.additions.length,
        'transaction.delta.removals': delta.removals.length,
        'transaction.actor': options.actor || 'system',
      });

      try {
        const result = await super.apply(store, delta, options);

        span.setAttributes({
          'transaction.committed': result.receipt.committed,
          'transaction.duration_ms': result.receipt.durationMs,
        });

        span.setStatus({ code: SpanStatusCode.OK });
        return result;
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message,
        });
        throw error;
      }
    });
  }
}

// Usage
const tx = new InstrumentedTransactionManager();
```

### Wrapper Function Approach

```javascript
import { trace } from '@opentelemetry/api';
import { TransactionManager } from '@unrdf/core';

const tracer = trace.getTracer('my-app');

function wrapTransactionWithOTEL(transactionManager) {
  const originalApply = transactionManager.apply.bind(transactionManager);

  transactionManager.apply = async function (store, delta, options) {
    return tracer.startActiveSpan('transaction.apply', async span => {
      span.setAttributes({
        'transaction.additions': delta.additions.length,
        'transaction.removals': delta.removals.length,
      });

      try {
        const result = await originalApply(store, delta, options);
        span.setStatus({ code: SpanStatusCode.OK });
        return result;
      } catch (error) {
        span.recordException(error);
        throw error;
      }
    });
  };

  return transactionManager;
}

// Usage
const tx = new TransactionManager();
const instrumentedTx = wrapTransactionWithOTEL(tx);
```

## Adding OTEL to CircuitBreaker

### Basic Example

```javascript
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { CircuitBreaker } from '@unrdf/core';

const tracer = trace.getTracer('my-app');

class InstrumentedCircuitBreaker extends CircuitBreaker {
  async execute(fn, context = {}) {
    return tracer.startActiveSpan(`circuit-breaker.${this.name}`, async span => {
      span.setAttributes({
        'circuit.state': this.state,
        'circuit.failure_count': this.failureCount,
        ...context,
      });

      try {
        const result = await super.execute(fn, context);
        span.setStatus({ code: SpanStatusCode.OK });
        return result;
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message,
        });
        throw error;
      }
    });
  }
}

// Usage
const breaker = new InstrumentedCircuitBreaker({ name: 'sparql-endpoint' });
```

### Decorator Wrapper

```javascript
import { trace } from '@opentelemetry/api';
import { CircuitBreaker } from '@unrdf/core';

const tracer = trace.getTracer('my-app');

function createOTELCircuitBreaker(config) {
  const breaker = new CircuitBreaker(config);
  const originalExecute = breaker.execute.bind(breaker);

  breaker.execute = async function (fn, context) {
    return tracer.startActiveSpan(`circuit-breaker.${this.name}`, async span => {
      span.setAttributes({
        'circuit.state': this.state,
        'circuit.metrics': JSON.stringify(this.getStatus()),
        ...context,
      });

      try {
        const result = await originalExecute(fn, context);
        return result;
      } catch (error) {
        span.recordException(error);
        throw error;
      }
    });
  };

  return breaker;
}

// Usage
const breaker = createOTELCircuitBreaker({
  name: 'remote-service',
  failureThreshold: 5,
});
```

## Using @unrdf/validation for Testing

For testing with OTEL validation, use the validation package:

```javascript
import {
  OTELValidator,
  createSpanData,
} from '@unrdf/validation';

const validator = new OTELValidator();

// Create test spans
const spans = [
  createSpanData('transaction.apply', 'ok', 150, {
    'transaction.additions': 5,
    'transaction.removals': 2,
    'transaction.committed': true,
  }),
  createSpanData('circuit-breaker.execute', 'ok', 45, {
    'circuit.state': 'closed',
    'circuit.success': true,
  }),
];

// Validate
const result = validator.validate(spans);
console.log('Validation score:', result.score);
```

## Span Attributes Reference

### TransactionManager Spans

| Attribute | Type | Description |
|-----------|------|-------------|
| `transaction.additions` | number | Number of quads to add |
| `transaction.removals` | number | Number of quads to remove |
| `transaction.actor` | string | Actor performing the transaction |
| `transaction.committed` | boolean | Whether transaction succeeded |
| `transaction.duration_ms` | number | Total duration in milliseconds |
| `transaction.hook_results` | number | Number of hook results |
| `transaction.error` | string | Error message if failed |

### CircuitBreaker Spans

| Attribute | Type | Description |
|-----------|------|-------------|
| `circuit.state` | string | Current state (closed/open/half_open) |
| `circuit.failure_count` | number | Number of consecutive failures |
| `circuit.name` | string | Circuit breaker name |
| `circuit.success` | boolean | Whether execution succeeded |
| `circuit.rejected` | boolean | Whether circuit rejected the call |

## Best Practices

1. **Only instrument at boundaries**: Add OTEL at entry/exit points (HTTP handlers, external API calls, etc.)
2. **Use context parameter**: The `context` parameter in `execute()` is specifically reserved for observability data
3. **Keep spans simple**: Avoid adding too many attributes per span
4. **Use Zod for validation**: Validate span data with Zod schemas before recording
5. **Test with validation package**: Use `@unrdf/validation` to ensure spans meet requirements

## Integration Examples

### Express.js Middleware

```javascript
import { CircuitBreaker } from '@unrdf/core';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('my-api');
const breaker = new CircuitBreaker({ name: 'external-api' });

app.get('/data/:id', async (req, res) => {
  return tracer.startActiveSpan('http.request', async span => {
    span.setAttributes({
      'http.method': 'GET',
      'http.path': req.path,
    });

    try {
      const data = await breaker.execute(() =>
        fetchFromExternalAPI(req.params.id)
      );
      res.json(data);
    } catch (error) {
      span.recordException(error);
      res.status(500).json({ error: error.message });
    }
  });
});
```

### Testing OTEL Instrumentation

```javascript
import { describe, it, expect } from 'vitest';
import { InstrumentedTransactionManager } from './transaction-instrumented.mjs';

describe('TransactionManager OTEL', () => {
  it('should emit spans on apply', async () => {
    const spans = [];
    const mockTracer = {
      startActiveSpan: (name, fn) => {
        const span = {
          setAttributes: () => {},
          setStatus: () => {},
          recordException: () => {},
        };
        spans.push({ name });
        return fn(span);
      },
    };

    const tx = new InstrumentedTransactionManager();
    // Override tracer
    tx.tracer = mockTracer;

    const result = await tx.apply(store, delta);

    expect(spans).toContainEqual(
      expect.objectContaining({ name: 'transaction.apply' })
    );
  });
});
```

## See Also

- [KNOWLEDGE-HOOKS-PERFORMANCE.md](./KNOWLEDGE-HOOKS-PERFORMANCE.md) - Hook performance considerations
- [Validation Package](../validation/) - OTEL validation utilities
- [OpenTelemetry Docs](https://opentelemetry.io/) - Official OTEL documentation
