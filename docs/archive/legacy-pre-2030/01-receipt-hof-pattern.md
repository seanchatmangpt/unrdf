# Pattern 1: Receipt HOF (Higher-Order Function) Pattern

**Version**: v6.0.0
**Maturity Target**: L5 (Stable contracts, deterministic, adversarial-safe, composable)
**Copy-Exact Template**: Yes - Use AS-IS for all P0+P1 packages

---

## Overview

**Problem**: Every state mutation needs cryptographic proof of what changed, when, why, and by whom.

**Solution**: Wrap all state-changing operations in a receipt-generating HOF that returns `{result, receipt}`.

**Invariant**: ALL state changes MUST produce receipts. No exceptions.

---

## Copy-Exact Template

### 1. Basic Receipt HOF (Synchronous)

```javascript
/**
 * Receipt HOF - Wraps operation with receipt generation
 *
 * @template T
 * @param {Function} operation - Operation to wrap (must be pure or deterministic)
 * @param {Object} context - Operation context
 * @param {string} context.operationName - Operation name for receipt
 * @param {string} context.actor - Actor performing operation
 * @param {string|null} [context.previousReceiptHash] - Previous receipt hash for chaining
 * @returns {{result: T, receipt: Receipt}} Operation result + cryptographic receipt
 *
 * @example
 * const {result, receipt} = withReceipt(
 *   (x, y) => x + y,
 *   { operationName: 'add', actor: 'calculator', previousReceiptHash: null },
 *   10, 20
 * );
 * console.assert(result === 30);
 * console.assert(receipt.payloadHash.length === 64); // BLAKE3
 */
export function withReceipt(operation, context, ...args) {
  // 1. Generate timestamp (injected, not Date.now())
  const t_ns = context.t_ns || now(); // Must use injected time provider
  const timestamp_iso = toISO(t_ns);

  // 2. Execute operation (deterministic execution)
  const result = operation(...args);

  // 3. Build payload for hashing
  const payload = {
    operationName: context.operationName,
    args: deterministicSerialize(args),
    result: deterministicSerialize(result),
    actor: context.actor,
    t_ns: t_ns.toString(),
  };

  // 4. Compute payload hash
  const payloadHash = computeBlake3Sync(payload);

  // 5. Compute chain hash
  const receiptHash = computeChainHashSync(
    context.previousReceiptHash || null,
    payloadHash
  );

  // 6. Build receipt
  const receipt = {
    id: generateUUID(),
    receiptType: 'execution',
    t_ns,
    timestamp_iso,
    previousHash: context.previousReceiptHash || null,
    payloadHash,
    receiptHash,
    payload,
  };

  // 7. Validate receipt schema
  const validatedReceipt = ReceiptSchema.parse(receipt);

  return { result, receipt: validatedReceipt };
}
```

### 2. Async Receipt HOF

```javascript
/**
 * Async Receipt HOF - For async operations (DB, network, streaming)
 *
 * @template T
 * @param {Function} asyncOperation - Async operation to wrap
 * @param {Object} context - Operation context
 * @returns {Promise<{result: T, receipt: Receipt}>}
 *
 * @example
 * const {result, receipt} = await withReceiptAsync(
 *   async (id) => await db.query('SELECT * FROM users WHERE id = ?', [id]),
 *   { operationName: 'query_user', actor: 'api', previousReceiptHash: lastReceipt.receiptHash },
 *   42
 * );
 */
export async function withReceiptAsync(asyncOperation, context, ...args) {
  // 1. Generate timestamp
  const t_ns = context.t_ns || now();
  const timestamp_iso = toISO(t_ns);

  // 2. Execute async operation
  const result = await asyncOperation(...args);

  // 3. Build payload
  const payload = {
    operationName: context.operationName,
    args: deterministicSerialize(args),
    result: deterministicSerialize(result),
    actor: context.actor,
    t_ns: t_ns.toString(),
  };

  // 4. Compute hashes (async BLAKE3)
  const payloadHash = await computeBlake3(payload);
  const receiptHash = await computeChainHash(
    context.previousReceiptHash || null,
    payloadHash
  );

  // 5. Build receipt
  const receipt = {
    id: generateUUID(),
    receiptType: 'execution',
    t_ns,
    timestamp_iso,
    previousHash: context.previousReceiptHash || null,
    payloadHash,
    receiptHash,
    payload,
  };

  return { result, receipt: ReceiptSchema.parse(receipt) };
}
```

### 3. Streaming Receipt HOF

**Problem**: Streaming operations produce incremental results. How to receipt?

**Solution**: Generate receipt for EACH chunk, chain them together.

```javascript
/**
 * Streaming Receipt HOF - For streaming/generator operations
 *
 * @template T
 * @param {AsyncGenerator<T>} stream - Async generator stream
 * @param {Object} context - Operation context
 * @returns {AsyncGenerator<{chunk: T, receipt: Receipt}>}
 *
 * @example
 * const stream = readFileByChunks('large-file.txt');
 * let lastReceipt = null;
 * for await (const {chunk, receipt} of withReceiptStream(stream, { operationName: 'read_chunk', actor: 'reader' })) {
 *   console.log(chunk);
 *   lastReceipt = receipt; // Track chain
 * }
 */
export async function* withReceiptStream(stream, context) {
  let previousReceiptHash = context.previousReceiptHash || null;
  let chunkIndex = 0;

  for await (const chunk of stream) {
    const t_ns = now();
    const timestamp_iso = toISO(t_ns);

    const payload = {
      operationName: context.operationName,
      chunkIndex,
      chunk: deterministicSerialize(chunk),
      actor: context.actor,
      t_ns: t_ns.toString(),
    };

    const payloadHash = await computeBlake3(payload);
    const receiptHash = await computeChainHash(previousReceiptHash, payloadHash);

    const receipt = ReceiptSchema.parse({
      id: generateUUID(),
      receiptType: 'execution',
      t_ns,
      timestamp_iso,
      previousHash: previousReceiptHash,
      payloadHash,
      receiptHash,
      payload,
    });

    yield { chunk, receipt };

    previousReceiptHash = receiptHash; // Chain next receipt
    chunkIndex++;
  }
}
```

---

## Determinism Requirements

**CRITICAL**: Receipt HOFs require deterministic execution.

### Banned Operations (Non-Deterministic)

```javascript
// ❌ WRONG - Non-deterministic
function badOperation() {
  return {
    timestamp: Date.now(),           // ❌ External clock
    random: Math.random(),            // ❌ Random number
    uuid: crypto.randomUUID(),        // ❌ Random UUID
    envVar: process.env.SECRET,       // ❌ External state
  };
}
```

### Allowed Operations (Deterministic)

```javascript
// ✅ CORRECT - Deterministic with injection
function goodOperation(x, y, providedTime, providedRandom, providedUUID) {
  return {
    timestamp: providedTime,          // ✅ Injected
    random: providedRandom,           // ✅ Injected
    uuid: providedUUID,               // ✅ Injected
    result: x + y,                    // ✅ Pure function
  };
}

// Usage
const {result, receipt} = withReceipt(
  goodOperation,
  {
    operationName: 'goodOp',
    actor: 'system',
    t_ns: 1234567890000000000n, // Injected
  },
  10, 20,
  1234567890000000000n,           // providedTime
  0.5,                             // providedRandom (from seeded PRNG)
  '550e8400-e29b-41d4-a716-446655440000' // providedUUID
);
```

---

## Async Operations: DB, Network, Streaming

### Database Operations

```javascript
/**
 * Receipt DB Query
 *
 * @param {Object} db - Database connection
 * @param {string} sql - SQL query
 * @param {Array} params - Query parameters
 * @param {Object} context - Receipt context
 * @returns {Promise<{result: Array, receipt: Receipt}>}
 */
export async function receiptedQuery(db, sql, params, context) {
  return withReceiptAsync(
    async () => await db.query(sql, params),
    { ...context, operationName: 'db_query' },
    { sql, params } // Args for payload
  );
}

// Usage
const {result: users, receipt} = await receiptedQuery(
  db,
  'SELECT * FROM users WHERE age > ?',
  [18],
  { actor: 'api', previousReceiptHash: lastReceipt?.receiptHash }
);
```

### Network Operations

```javascript
/**
 * Receipt HTTP Fetch
 *
 * @param {string} url - URL to fetch
 * @param {Object} options - Fetch options
 * @param {Object} context - Receipt context
 * @returns {Promise<{result: Response, receipt: Receipt}>}
 */
export async function receiptedFetch(url, options, context) {
  return withReceiptAsync(
    async () => {
      const response = await fetch(url, options);
      const data = await response.json();
      return data;
    },
    { ...context, operationName: 'http_fetch' },
    { url, method: options?.method || 'GET' }
  );
}

// Usage
const {result: data, receipt} = await receiptedFetch(
  'https://api.example.com/data',
  { method: 'GET' },
  { actor: 'client', previousReceiptHash: null }
);
```

**PROBLEM**: Network responses are non-deterministic (timestamps, server state).

**SOLUTION**: Hash ONLY the deterministic parts (URL, method, status code). Store raw response separately.

```javascript
const payload = {
  url,
  method,
  statusCode: response.status,
  // ❌ DON'T include: response body (may vary)
  // ✅ DO include: content hash
  contentHash: await computeBlake3(responseBody),
};
```

---

## Common Pitfalls

### Pitfall 1: Non-Deterministic Time

```javascript
// ❌ WRONG
function operation() {
  return { timestamp: Date.now() }; // Non-deterministic
}

// ✅ CORRECT
function operation(providedTime) {
  return { timestamp: providedTime }; // Injected
}
```

### Pitfall 2: External State

```javascript
// ❌ WRONG
let counter = 0;
function operation() {
  return { count: ++counter }; // External mutable state
}

// ✅ CORRECT
function operation(currentCount) {
  return { count: currentCount + 1 }; // Pure function
}
```

### Pitfall 3: Forgetting to Chain Receipts

```javascript
// ❌ WRONG - Receipts not chained
const {receipt: r1} = withReceipt(op1, { operationName: 'op1', actor: 'a' });
const {receipt: r2} = withReceipt(op2, { operationName: 'op2', actor: 'a' }); // Not chained!

// ✅ CORRECT - Chained receipts
const {receipt: r1} = withReceipt(op1, { operationName: 'op1', actor: 'a', previousReceiptHash: null });
const {receipt: r2} = withReceipt(op2, { operationName: 'op2', actor: 'a', previousReceiptHash: r1.receiptHash });
```

### Pitfall 4: Adding OTEL to Receipt HOF

```javascript
// ❌ WRONG - OTEL in implementation
export function withReceipt(operation, context, ...args) {
  const span = tracer.startSpan('withReceipt'); // ❌ NO OTEL in HOF
  // ...
}

// ✅ CORRECT - OTEL at boundary
export function apiHandler(req, res) {
  const span = tracer.startSpan('apiHandler'); // ✅ OTEL at API boundary
  const {result, receipt} = withReceipt(businessLogic, context); // Pure HOF
  span.end();
}
```

---

## Zod Schemas (Copy-Exact)

```javascript
import { z } from 'zod';

/**
 * Receipt Schema - BLAKE3 64-char hashes
 */
export const ReceiptSchema = z.object({
  id: z.string().uuid(),
  receiptType: z.enum(['execution', 'allocation', 'compile', 'verification']),
  t_ns: z.bigint(),
  timestamp_iso: z.string().datetime(),
  previousHash: z.string().length(64).nullable(),
  payloadHash: z.string().length(64),
  receiptHash: z.string().length(64),
  payload: z.any(), // Domain-specific payload
  attestation: z.object({
    algorithm: z.string(),
    publicKey: z.string(),
    signature: z.string(),
    signer: z.string().optional(),
  }).optional(),
  vectorClock: z.object({
    nodeId: z.string().min(1),
    counters: z.record(z.string(), z.string()),
  }).optional(),
  gitRef: z.string().optional(),
  kgcEventId: z.string().optional(),
});
```

---

## JSDoc Signature Template

```javascript
/**
 * [Operation Name] - [Brief description]
 *
 * [Detailed explanation of what operation does]
 *
 * DETERMINISM: [Explain deterministic guarantees]
 * RECEIPT: [Explain what receipt proves]
 *
 * @template T - Result type
 * @param {ParamType} param1 - [Description]
 * @param {Object} context - Receipt context
 * @param {string} context.operationName - Operation name
 * @param {string} context.actor - Actor performing operation
 * @param {string|null} [context.previousReceiptHash] - Previous receipt for chaining
 * @param {bigint} [context.t_ns] - Injected timestamp (deterministic)
 * @returns {{result: T, receipt: Receipt}} Operation result + cryptographic receipt
 *
 * @throws {Error} [When operation fails]
 *
 * @example
 * const {result, receipt} = operationName(
 *   param1,
 *   { operationName: 'name', actor: 'actor', previousReceiptHash: null }
 * );
 */
```

---

## Testing Template

```javascript
import { describe, it, expect } from 'vitest';
import { withReceipt } from './receipt-hof.mjs';

describe('Receipt HOF Pattern', () => {
  it('generates receipt for operation', () => {
    const {result, receipt} = withReceipt(
      (x, y) => x + y,
      { operationName: 'add', actor: 'test', t_ns: 1000000000n },
      10, 20
    );

    expect(result).toBe(30);
    expect(receipt.id).toMatch(/^[0-9a-f-]{36}$/); // UUID
    expect(receipt.payloadHash).toHaveLength(64); // BLAKE3
    expect(receipt.receiptHash).toHaveLength(64);
  });

  it('chains receipts correctly', () => {
    const {receipt: r1} = withReceipt(
      (x) => x * 2,
      { operationName: 'double', actor: 'test', t_ns: 1000000000n },
      5
    );

    const {receipt: r2} = withReceipt(
      (x) => x + 1,
      { operationName: 'increment', actor: 'test', t_ns: 2000000000n, previousReceiptHash: r1.receiptHash },
      10
    );

    expect(r2.previousHash).toBe(r1.receiptHash);
  });

  it('is deterministic with same inputs', () => {
    const context = { operationName: 'add', actor: 'test', t_ns: 1000000000n };

    const {receipt: r1} = withReceipt((x, y) => x + y, context, 10, 20);
    const {receipt: r2} = withReceipt((x, y) => x + y, context, 10, 20);

    expect(r1.payloadHash).toBe(r2.payloadHash);
    expect(r1.receiptHash).toBe(r2.receiptHash);
  });
});
```

---

## Enforcement (ESLint Rule Sketch)

```javascript
// .eslintrc.cjs
module.exports = {
  rules: {
    'unrdf/require-receipt': ['error', {
      // All functions matching these patterns MUST return {result, receipt}
      patterns: [
        /^(create|update|delete|modify|mutate)/i,
        /^(apply|execute|process|handle)/i,
      ],
      // Banned non-deterministic operations
      bannedCalls: [
        'Date.now',
        'Math.random',
        'crypto.randomUUID',
        'process.env',
      ],
    }],
  },
};
```

---

## Decision Matrix: When to Use Receipt HOF

| Scenario | Use Receipt HOF? | Rationale |
|----------|------------------|-----------|
| State mutation (DB write, file write) | ✅ YES | MUST prove what changed |
| Pure computation (deterministic) | ✅ YES | Prove correctness |
| Query (read-only) | ⚠️ OPTIONAL | Useful for audit trail, not required |
| Streaming data | ✅ YES | Use `withReceiptStream` |
| External API call | ⚠️ OPTIONAL | Hash request, not response (non-deterministic) |
| Internal pure function | ❌ NO | Leaf functions don't need receipts (only API boundaries) |

---

## Copy-Paste Checklist

Before copying this pattern to a new package:

- [ ] Copy `receipt-hof.mjs` template
- [ ] Copy `receipt-schema.mjs` with Zod schemas
- [ ] Copy `receipt-hof.test.mjs` tests
- [ ] Replace `operationName` with your domain operation
- [ ] Verify determinism: NO `Date.now()`, `Math.random()`, `process.env`
- [ ] Add to ESLint config: `unrdf/require-receipt` rule
- [ ] Document what receipts prove in package README

---

## References

- Existing Implementation: `/home/user/unrdf/packages/yawl/src/receipt-core.mjs`
- Base Receipt Schema: `/home/user/unrdf/packages/v6-core/src/receipts/base-receipt.mjs`
- BLAKE3 Hashing: `hash-wasm` library
- Time Provider: `@unrdf/kgc-4d` (deterministic nanosecond timestamps)
