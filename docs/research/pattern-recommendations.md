# Pattern Recommendations for UNRDF Documentation

**Date:** 2026-01-11
**Source:** 7-day commit analysis (daemon package implementation)
**Purpose:** Actionable patterns for future development and documentation

---

## 1. Core Architectural Patterns (Ready for Immediate Use)

### 1.1 Event-Driven Coordination Pattern

**When to Use:** Cross-package communication without dependencies

**Template:**
```javascript
// In your package
export class MyPackageCoordinator extends EventEmitter {
  constructor(daemon, options = {}) {
    super();
    this.daemon = daemon;
    this._attachListeners();
  }

  _attachListeners() {
    // Listen to daemon events
    this.daemon.on('operation:success', (event) => {
      this.handleOperationSuccess(event);
    });
  }

  async handleOperationSuccess(event) {
    // React to daemon events
    // Emit your own events for other packages
    this.emit('mypackage:event', { data: '...' });
  }
}
```

**Registration:**
```javascript
const coordinator = new MyPackageCoordinator(daemon);
daemon.registerCoordinator('@unrdf/mypackage', coordinator);
```

**Benefits:**
- Zero direct dependencies between packages
- Dynamic plugin architecture
- Testable in isolation
- Runtime configuration

---

### 1.2 Delta-Based State Management Pattern

**When to Use:** Any state mutation that needs auditability or rollback

**Template:**
```javascript
import { DaemonDeltaGate, DeltaContractSchema } from '@unrdf/daemon/integrations/v6-deltagate';

// 1. Create gate
const gate = new DaemonDeltaGate({ daemonId: 'my-app' });

// 2. Propose delta
const delta = {
  id: crypto.randomUUID(),
  timestamp_ns: BigInt(Date.now()) * 1_000_000n,
  timestamp_iso: new Date().toISOString(),
  operations: [
    {
      op: 'set',
      path: 'user.status',
      oldValue: 'offline',
      newValue: 'online',
      timestamp_ns: BigInt(Date.now()) * 1_000_000n,
    },
  ],
  source: {
    package: '@unrdf/mypackage',
    actor: 'user-123',
  },
  previousDeltaId: null,
};

// 3. Apply and get receipt
const receipt = await gate.proposeDelta(delta);

// 4. Check result
if (receipt.applied) {
  console.log('State changed:', receipt.stateHash);
} else {
  console.error('Delta rejected:', receipt.reason);
}
```

**Benefits:**
- Full audit trail
- Time-travel debugging
- Cryptographic proof of changes
- Rollback support

---

### 1.3 Adapter Pattern for Package Integration

**When to Use:** Integrating external package with daemon

**Template:**
```javascript
export class MyPackageAdapter extends EventEmitter {
  constructor(daemon, externalPackage, options = {}) {
    super();
    this.daemon = daemon;
    this.externalPackage = externalPackage;
    this._wireEvents();
  }

  _wireEvents() {
    // Daemon → External package
    this.daemon.on('operation:started', (event) => {
      this.externalPackage.handleOperation(event);
    });

    // External package → Daemon
    this.externalPackage.on('task:completed', (event) => {
      this.daemon.emit('mypackage:task_completed', event);
    });
  }

  // Adapter methods
  async executeTask(taskId) {
    const result = await this.externalPackage.run(taskId);
    this.emit('task:executed', { taskId, result });
    return result;
  }
}
```

**Registration:**
```javascript
const adapter = new MyPackageAdapter(daemon, externalPackage);
daemon.registerCoordinator('@unrdf/mypackage', adapter);
```

---

### 1.4 Chain of Proof Pattern

**When to Use:** Cryptographic audit trails for receipts

**Template:**
```javascript
import { blake3 } from 'hash-wasm';

export class ReceiptGenerator {
  constructor() {
    this.lastReceiptHash = null;
    this.receipts = [];
  }

  async generateReceipt(operation) {
    // 1. Hash the payload
    const payloadHash = await blake3(JSON.stringify(operation.payload));

    // 2. Chain to previous receipt
    const chainInput = (this.lastReceiptHash || 'GENESIS') + ':' + payloadHash;
    const receiptHash = await blake3(chainInput);

    // 3. Create receipt
    const receipt = {
      id: crypto.randomUUID(),
      operationId: operation.id,
      payloadHash,
      previousHash: this.lastReceiptHash,
      receiptHash,
      timestamp_ns: BigInt(Date.now()) * 1_000_000n,
    };

    // 4. Update chain state
    this.lastReceiptHash = receiptHash;
    this.receipts.push(receipt);

    return receipt;
  }

  async verifyChain(receipts) {
    for (let i = 1; i < receipts.length; i++) {
      const current = receipts[i];
      const previous = receipts[i - 1];

      // Verify chain link
      if (current.previousHash !== previous.receiptHash) {
        return { valid: false, reason: `Chain broken at index ${i}` };
      }

      // Verify hash integrity
      const expectedHash = await blake3(current.previousHash + ':' + current.payloadHash);
      if (expectedHash !== current.receiptHash) {
        return { valid: false, reason: `Hash integrity failed at index ${i}` };
      }
    }

    return { valid: true };
  }
}
```

**Benefits:**
- Tamper-evident
- Cryptographic proof
- Fast verification
- Auditable

---

## 2. Error Handling Patterns

### 2.1 Safe Event Emission

**When to Use:** Always, when emitting events

**Template:**
```javascript
_safeEmit(event, data) {
  try {
    this.emit(event, data);
  } catch (error) {
    this.logger.warn(`Listener error for event '${event}': ${error.message}`);
    // Continue processing - don't let bad listeners break your code
  }
}

// Usage
this._safeEmit('operation:success', { operationId: 'op-123' });
```

**Benefits:**
- Prevents cascade failures
- Logs errors for debugging
- Robust against bad listeners

---

### 2.2 Zod Validation at Boundaries

**When to Use:** All public APIs, configuration, external inputs

**Template:**
```javascript
import { z } from 'zod';

// 1. Define schema
export const ConfigSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1),
  timeout: z.number().int().positive().default(5000),
  retries: z.number().int().min(0).max(10).default(3),
  metadata: z.record(z.any()).optional(),
});

// 2. Validate in constructor
export class MyClass {
  constructor(config) {
    const validated = ConfigSchema.parse(config);
    Object.assign(this, validated);
  }
}

// 3. Safe validation (returns result object)
export function processInput(data) {
  const result = ConfigSchema.safeParse(data);

  if (!result.success) {
    console.error('Validation failed:', result.error.issues);
    return null;
  }

  return result.data;
}
```

**Benefits:**
- Runtime type safety
- Self-documenting
- Clear error messages
- Zero runtime errors from invalid data

---

### 2.3 Atomic Operations with Rollback

**When to Use:** Multi-step operations that must all succeed or all fail

**Template:**
```javascript
_applyOperations(operations) {
  // 1. Capture state before mutation
  const snapshot = this._captureState();

  try {
    // 2. Apply all operations
    for (const op of operations) {
      this._applyOperation(op);
    }

    return { success: true };
  } catch (error) {
    // 3. Rollback on any failure
    this._restoreState(snapshot);

    return {
      success: false,
      reason: error.message
    };
  }
}

_captureState() {
  const state = {};
  for (const [key, value] of this.store.entries()) {
    state[key] = structuredClone(value);  // Deep copy
  }
  return state;
}

_restoreState(snapshot) {
  this.store.clear();
  for (const [key, value] of Object.entries(snapshot)) {
    this.store.set(key, value);
  }
}
```

**Benefits:**
- Data integrity
- No partial updates
- Simple error recovery
- Testable invariants

---

## 3. Testing Patterns

### 3.1 AAA Pattern (Arrange-Act-Assert)

**When to Use:** All tests

**Template:**
```javascript
import { describe, it, expect, beforeEach } from 'vitest';

describe('MyClass', () => {
  let instance;

  beforeEach(() => {
    instance = new MyClass({ id: 'test' });
  });

  it('should do something when condition is met', async () => {
    // Arrange - Set up test data
    const input = {
      value: 'test-value',
      timestamp: Date.now(),
    };

    // Act - Execute the operation
    const result = await instance.process(input);

    // Assert - Verify the outcome
    expect(result.success).toBe(true);
    expect(result.value).toBe('test-value');
    expect(result.timestamp).toBeGreaterThan(0);
  });
});
```

**Benefits:**
- Readable tests
- Clear intent
- Easy debugging
- Consistent structure

---

### 3.2 Test Utilities Pattern

**When to Use:** Reduce test duplication

**Template:**
```javascript
// test/utils.mjs
export function createTestUser(overrides = {}) {
  return {
    id: crypto.randomUUID(),
    name: 'Test User',
    email: 'test@example.com',
    createdAt: new Date(),
    ...overrides,
  };
}

export function createTestDelta(overrides = {}) {
  return {
    id: crypto.randomUUID(),
    timestamp_ns: BigInt(Date.now()) * 1_000_000n,
    timestamp_iso: new Date().toISOString(),
    operations: [
      { op: 'set', path: 'status', newValue: 'active' },
    ],
    source: { package: '@unrdf/test' },
    ...overrides,
  };
}

// Usage in tests
import { createTestUser, createTestDelta } from './utils.mjs';

it('should process user delta', async () => {
  const user = createTestUser({ name: 'Alice' });
  const delta = createTestDelta({
    operations: [
      { op: 'set', path: 'user.status', newValue: 'active' }
    ]
  });

  const result = await processor.process(user, delta);
  expect(result.success).toBe(true);
});
```

---

### 3.3 Performance Testing Pattern

**When to Use:** Validate latency/throughput requirements

**Template:**
```javascript
import { describe, it, expect } from 'vitest';

describe('Performance', () => {
  it('should process 100 operations under 5 seconds', async () => {
    const operations = Array.from({ length: 100 }, (_, i) => ({
      id: crypto.randomUUID(),
      value: i,
    }));

    const startTime = performance.now();

    // Act
    const results = await Promise.all(
      operations.map(op => processor.execute(op))
    );

    const duration = performance.now() - startTime;

    // Assert performance
    expect(duration).toBeLessThan(5000);  // <5s
    expect(results.every(r => r.success)).toBe(true);
  });

  it('should maintain P95 latency under 5ms', async () => {
    const timings = [];

    for (let i = 0; i < 1000; i++) {
      const start = performance.now();
      await processor.execute({ id: crypto.randomUUID() });
      timings.push(performance.now() - start);
    }

    const sorted = timings.sort((a, b) => a - b);
    const p95 = sorted[Math.floor(sorted.length * 0.95)];

    expect(p95).toBeLessThan(5);  // <5ms P95
  });
});
```

---

## 4. Observability Patterns

### 4.1 OTEL Instrumentation

**When to Use:** Production services

**Template:**
```javascript
import { trace, metrics } from '@opentelemetry/api';

const tracer = trace.getTracer('my-package');
const meter = metrics.getMeter('my-package');

export class MyService {
  constructor() {
    // Create metrics
    this.operationCounter = meter.createCounter('operations.total', {
      description: 'Total operations processed',
    });

    this.latencyHistogram = meter.createHistogram('operations.duration', {
      description: 'Operation duration in milliseconds',
    });
  }

  async executeOperation(operationId) {
    // Create span
    const span = tracer.startSpan('execute_operation', {
      attributes: {
        'operation.id': operationId,
      },
    });

    const startTime = performance.now();

    try {
      // Execute operation
      const result = await this._execute(operationId);

      // Record success
      this.operationCounter.add(1, { status: 'success' });
      span.setStatus({ code: 0 });  // OK

      return result;
    } catch (error) {
      // Record failure
      this.operationCounter.add(1, { status: 'failure' });
      span.recordException(error);
      span.setStatus({ code: 2 });  // ERROR

      throw error;
    } finally {
      // Record latency
      const duration = performance.now() - startTime;
      this.latencyHistogram.record(duration);

      span.end();
    }
  }
}
```

---

### 4.2 Health Status Pattern

**When to Use:** Services that need health monitoring

**Template:**
```javascript
export class MyService {
  constructor() {
    this.successCount = 0;
    this.failureCount = 0;
    this.startTime = Date.now();
  }

  getHealthStatus() {
    const totalAttempts = this.successCount + this.failureCount;
    let status = 'healthy';

    if (totalAttempts === 0) {
      status = 'healthy';
    } else if (this.failureCount / totalAttempts > 0.1) {
      status = 'degraded';  // >10% failure rate
    } else if (this.failureCount / totalAttempts > 0.3) {
      status = 'unhealthy';  // >30% failure rate
    }

    return {
      status,
      uptime: Date.now() - this.startTime,
      successCount: this.successCount,
      failureCount: this.failureCount,
      successRate: totalAttempts > 0
        ? (this.successCount / totalAttempts) * 100
        : 0,
      timestamp: new Date().toISOString(),
    };
  }
}
```

---

## 5. Performance Optimization Patterns

### 5.1 LRU Cache Pattern

**When to Use:** Bounded caches for completed operations

**Template:**
```javascript
export class LRUCache {
  constructor(maxSize = 1000) {
    this.maxSize = maxSize;
    this.cache = new Map();  // Insertion-ordered in JS
  }

  set(key, value) {
    // Move to end (most recent)
    if (this.cache.has(key)) {
      this.cache.delete(key);
    }

    this.cache.set(key, value);

    // Evict oldest if exceeds maxSize
    if (this.cache.size > this.maxSize) {
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }
  }

  get(key) {
    if (!this.cache.has(key)) return undefined;

    // Move to end (mark as recently used)
    const value = this.cache.get(key);
    this.cache.delete(key);
    this.cache.set(key, value);

    return value;
  }

  has(key) {
    return this.cache.has(key);
  }

  size() {
    return this.cache.size;
  }
}
```

**Complexity:** O(1) for set/get/has

---

### 5.2 Batch Processing Pattern

**When to Use:** High-throughput scenarios

**Template:**
```javascript
export class BatchProcessor {
  constructor(batchSize = 100, flushIntervalMs = 10) {
    this.batchSize = batchSize;
    this.flushIntervalMs = flushIntervalMs;
    this.buffer = [];
    this.timer = null;
  }

  add(item) {
    this.buffer.push(item);

    // Flush if batch full
    if (this.buffer.length >= this.batchSize) {
      this.flush();
    } else {
      // Start timer for partial batch
      this._startTimer();
    }
  }

  flush() {
    if (this.buffer.length === 0) return;

    const batch = this.buffer.splice(0, this.buffer.length);
    this._processBatch(batch);

    this._clearTimer();
  }

  _processBatch(items) {
    // Process entire batch at once
    console.log(`Processing batch of ${items.length} items`);
  }

  _startTimer() {
    if (this.timer) return;

    this.timer = setTimeout(() => {
      this.flush();
    }, this.flushIntervalMs);
  }

  _clearTimer() {
    if (this.timer) {
      clearTimeout(this.timer);
      this.timer = null;
    }
  }
}
```

**Benefits:**
- 10x throughput improvement
- Reduced syscalls
- Lower latency variance

---

## 6. Event Naming Conventions

**Convention:** `domain:action` format

**Examples:**
```javascript
// Good
daemon.emit('daemon:started', { nodeId });
daemon.emit('operation:enqueued', { operationId });
daemon.emit('delta:applied', { deltaId, receipt });
daemon.emit('policy:registered', { policyId });

// Bad
daemon.emit('started', { nodeId });  // No domain
daemon.emit('OPERATION_ENQUEUED', { operationId });  // Wrong case
daemon.emit('delta_applied', { deltaId });  // Wrong separator
```

**Categories:**
- **Lifecycle:** `service:started`, `service:stopped`
- **Operations:** `operation:enqueued`, `operation:success`, `operation:failure`
- **State:** `state:changed`, `delta:applied`, `delta:rejected`
- **Policy:** `policy:registered`, `policy:enabled`, `policy:disabled`
- **Consensus:** `leader:elected`, `leader:lost`, `term:updated`

---

## 7. Documentation Patterns

### 7.1 JSDoc Pattern

**When to Use:** All exported functions/classes

**Template:**
```javascript
/**
 * Brief one-line description
 *
 * Detailed description explaining:
 * - What it does
 * - Why you'd use it
 * - How it works (if non-obvious)
 *
 * @param {Type} paramName - Description
 * @param {Object} [options] - Optional config
 * @param {string} options.prop1 - Prop description
 * @param {number} [options.prop2=5] - Optional with default
 * @returns {ReturnType} Description of return value
 * @throws {ErrorType} When error occurs
 *
 * @example
 * const result = functionName('value', { prop1: 'test' });
 * console.log(result); // Expected output
 *
 * @example
 * // Error case
 * try {
 *   functionName(null);  // Throws TypeError
 * } catch (error) {
 *   console.error(error.message);
 * }
 */
export function functionName(paramName, options = {}) {
  // Implementation
}
```

---

### 7.2 README Pattern

**Sections:**
1. **One-line description**
2. **Features** (3-5 bullet points)
3. **Installation**
4. **Quick Start** (30-second example)
5. **API Reference** (link to full docs)
6. **Examples** (2-3 real-world scenarios)
7. **Testing**
8. **Performance**
9. **License**

**Template:**
```markdown
# @unrdf/mypackage

> One-line description of what this package does

## Features

- Feature 1 with benefit
- Feature 2 with benefit
- Feature 3 with benefit

## Installation

\`\`\`bash
pnpm add @unrdf/mypackage
\`\`\`

## Quick Start

\`\`\`javascript
import { MyClass } from '@unrdf/mypackage';

const instance = new MyClass({ id: 'my-app' });
const result = await instance.execute();
console.log(result);
\`\`\`

## API Reference

See [full documentation](./docs/api.md)

## Examples

See [examples directory](./examples/)

## Testing

\`\`\`bash
pnpm test
\`\`\`

## Performance

- Latency: <5ms P95
- Throughput: 10K ops/sec
- Memory: <50MB for 10K operations

## License

MIT
```

---

## 8. Quick Reference Checklist

### Before Merging PR

- [ ] Zero lint errors (`pnpm lint`)
- [ ] Zero TODOs in production code
- [ ] All tests pass (`pnpm test`)
- [ ] Coverage ≥80% (`pnpm test:coverage`)
- [ ] JSDoc on all exports
- [ ] README updated if API changed
- [ ] No `it.skip()` without explanation
- [ ] Zod schemas exported
- [ ] Event names follow `domain:action` convention
- [ ] Logger injected, not `console` direct
- [ ] Safe event emission used
- [ ] OTEL spans added for critical paths

### When Adding New Package

- [ ] Create adapter class
- [ ] Extend `EventEmitter`
- [ ] Define Zod schemas
- [ ] Add E2E test suite
- [ ] Document integration pattern
- [ ] Register with daemon
- [ ] Add to package tier (Essential/Extended/Optional)

### When Implementing State Management

- [ ] Use delta contracts
- [ ] Generate receipts
- [ ] Validate with Zod
- [ ] Support rollback
- [ ] Emit coordination events
- [ ] Add OTEL spans

---

## 9. Pattern Decision Matrix

| Use Case | Pattern | Complexity | Benefits |
|----------|---------|------------|----------|
| Cross-package communication | Event-driven | Low | Zero coupling |
| State mutations | Delta-based | Medium | Full audit trail |
| Validation | Zod schemas | Low | Runtime safety |
| Cryptographic proofs | Chain of Proof | Medium | Tamper-evident |
| High throughput | Batch processing | Low | 10x improvement |
| Bounded memory | LRU cache | Low | O(1) operations |
| Distributed ops | Consensus Manager | High | Raft replication |
| Policy enforcement | Policy Adapter | Medium | Governance |

---

**Document Version:** 1.0
**Last Updated:** 2026-01-11
**Source:** 7-day commit analysis (daemon package)
**Status:** Production-ready patterns
