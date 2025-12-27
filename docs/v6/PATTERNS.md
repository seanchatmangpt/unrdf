# v6 Patterns Library

**Version**: 6.0.0-alpha.1
**Status**: Research Complete
**Last Updated**: 2025-12-27

## Executive Summary

This document catalogues **5 core patterns** extracted from P0+P1 implementation that enable v6's deterministic, receipt-driven architecture. These patterns are reusable across all 47 packages in the UNRDF monorepo.

**Key Insight**: Every pattern composes with others. Receipt HOF wraps functions. Delta Contract describes changes. Zod validates both. Determinism proves correctness. Composition ensures systems integrate.

## Pattern Index

| Pattern | Purpose | Maturity Level | Complexity |
|---------|---------|----------------|------------|
| [Receipt HOF](#receipt-hof-pattern) | Generate verifiable receipts for any operation | L3+ | Low |
| [Delta Contract](#delta-contract-pattern) | Explicit, auditable state transitions | L3+ | Medium |
| [Zod Validation Envelope](#zod-validation-envelope-pattern) | Runtime type safety at boundaries | L2+ | Low |
| [Determinism Proof](#determinism-proof-pattern) | Guarantee reproducible outputs | L3+ | Medium |
| [Composition Layer](#composition-layer-pattern-l5) | Cross-package integration | L5 | High |

---

## Receipt HOF Pattern

### Purpose & Use Cases

**Problem**: Operations execute without proof, making debugging and auditing impossible.
**Solution**: Wrap functions in Higher-Order Function (HOF) that generates cryptographic receipts.

**Use When**:
- State-changing operations (L3+ requirement)
- Workflow execution
- Resource allocation
- Any operation requiring replay/verification

**Don't Use When**:
- Pure read operations (unnecessary overhead)
- Performance-critical inner loops (profile first)
- Operations that already generate receipts

### Template & Pseudocode

```javascript
// Pattern Template
function withReceipt(fn, options = {}) {
  return async function wrappedWithReceipt(...args) {
    const startTime = performance.now();
    const result = await fn(...args);
    const endTime = performance.now();

    const receipt = {
      version: '6.0.0-alpha.1',
      operation: options.operation || fn.name,
      timestamp: Date.now(),
      duration: endTime - startTime,
      args: JSON.stringify(args),
      result: typeof result === 'object' ? JSON.stringify(result) : String(result),
      // Optional: Add hash for integrity
      hash: await computeBlake3({ args, result })
    };

    return { result, receipt };
  };
}
```

### Real Examples (with output)

**Example 1: Workflow Execution**

```javascript
import { withReceipt } from '@unrdf/v6-compat/adapters';

// Original function
async function executeWorkflow(taskId, action) {
  // ... workflow logic ...
  return { status: 'completed', taskId };
}

// Wrapped with receipt
const executeWithReceipt = withReceipt(executeWorkflow, {
  operation: 'workflow.execute'
});

const { result, receipt } = await executeWithReceipt('task-123', 'approve');

console.log(result);
// { status: 'completed', taskId: 'task-123' }

console.log(receipt);
// {
//   version: '6.0.0-alpha.1',
//   operation: 'workflow.execute',
//   timestamp: 1704067200000,
//   duration: 42.5,
//   args: '["task-123","approve"]',
//   result: '{"status":"completed","taskId":"task-123"}'
// }
```

**Example 2: Resource Allocation**

```javascript
import { withReceipt } from '@unrdf/v6-core/receipts';
import { computeBlake3 } from '@unrdf/v6-core/receipts/base-receipt';

const allocateResource = withReceipt(
  async (userId, resourceType, amount) => {
    // Allocation logic
    return { allocated: amount, userId, resourceType };
  },
  { operation: 'resource.allocate' }
);

const { result, receipt } = await allocateResource('user-1', 'cpu', 4);

// Receipt includes hash for verification
receipt.hash = await computeBlake3(receipt);

// Store receipt in chain
await receiptStore.append(receipt);
```

### Composition Rules

**Composes With**:
- ✅ **Delta Contract**: Wrap delta application with receipts
- ✅ **Zod Validation**: Validate inputs before wrapping
- ✅ **Determinism Proof**: Use deterministic timestamps in receipt
- ✅ **Composition Layer**: Receipt chains span package boundaries

**Pattern**:
```javascript
// Full composition
const operation = withReceipt(
  validateSchema(InputSchema)(
    async (data) => {
      const delta = createDelta('add', ...data);
      return await gate.proposeDelta(delta, store);
    }
  ),
  { operation: 'user.createResource' }
);
```

### Testing Approach

```javascript
// Test: Receipt generation
import { describe, it, expect } from 'vitest';

describe('Receipt HOF Pattern', () => {
  it('should generate receipt with all required fields', async () => {
    const fn = async (x) => x * 2;
    const wrapped = withReceipt(fn, { operation: 'double' });

    const { result, receipt } = await wrapped(21);

    expect(result).toBe(42);
    expect(receipt.operation).toBe('double');
    expect(receipt.timestamp).toBeGreaterThan(0);
    expect(receipt.duration).toBeGreaterThan(0);
    expect(receipt.args).toBe('[21]');
  });

  it('should handle async functions', async () => {
    const asyncFn = async (delay) => {
      await new Promise(r => setTimeout(r, delay));
      return 'done';
    };

    const wrapped = withReceipt(asyncFn);
    const { result, receipt } = await wrapped(10);

    expect(result).toBe('done');
    expect(receipt.duration).toBeGreaterThanOrEqual(10);
  });
});
```

**File Location**: `/home/user/unrdf/packages/v6-compat/src/adapters.mjs` (lines 246-267)

---

## Delta Contract Pattern

### Purpose & Use Cases

**Problem**: State changes happen via ad-hoc mutations, making causality tracking impossible.
**Solution**: All state changes must be packaged as explicit Delta proposals.

**Use When**:
- Any RDF store mutation (mandatory in v6)
- Workflow state transitions
- Resource updates
- Policy-controlled operations

**Don't Use When**:
- Read-only queries
- Ephemeral state (not persisted)

### Template & Pseudocode

```javascript
// Pattern Template
const delta = {
  id: crypto.randomUUID(),
  timestamp_iso: new Date().toISOString(),
  t_ns: BigInt(Date.now()) * 1_000_000n,

  operations: [
    {
      op: 'add' | 'delete' | 'update',
      subject: 'http://example.org/subject',
      predicate: 'http://example.org/predicate',
      object: 'value',
      graph: 'http://example.org/graph' // optional
    }
  ],

  source: {
    package: '@unrdf/your-package',
    actor: 'user-id',
    context: { /* additional metadata */ }
  },

  admissibility: {
    policyId: 'policy-123',
    preConditions: ['state-hash-match'],
    constraints: ['quota-check']
  }
};

// Workflow: Propose → Reconcile → Receipt → Apply
const gate = new DeltaGate({ policies });
const receipt = await gate.proposeDelta(delta, store);

if (receipt.applied) {
  console.log('Success:', receipt.stateHash);
} else {
  console.error('Rejected:', receipt.reason);
}
```

### Real Examples (with SPARQL)

**Example 1: Workflow Task Transition**

```javascript
import { createDelta } from '@unrdf/v6-core/delta';

// Transition task from 'enabled' → 'executing'
const delta = createDelta(
  'update',
  'http://workflow.org/task-123',
  'http://workflow.org/state',
  'http://workflow.org/states/executing',
  {
    oldObject: 'http://workflow.org/states/enabled',
    package: '@unrdf/yawl',
    actor: 'workflow-engine',
    context: { caseId: 'case-456' }
  }
);

// Apply through gate
const gate = new DeltaGate({
  policies: {
    'workflow-transitions': async (delta, store) => {
      // Check if transition is legal
      const allowed = await store.query(`
        ASK {
          <${delta.operations[0].subject}> <http://workflow.org/state> <${delta.operations[0].oldObject}> .
        }
      `);
      return allowed;
    }
  }
});

const receipt = await gate.proposeDelta(delta, store);

console.log(receipt);
// {
//   deltaId: 'uuid-123',
//   applied: true,
//   timestamp_ns: 1704067200000000000n,
//   stateHash: 'abc123...def456',
//   operationsApplied: 1
// }
```

**Example 2: Multi-Operation Composite Delta**

```javascript
// Add user and assign role in single atomic delta
const compositeDelta = {
  id: crypto.randomUUID(),
  timestamp_iso: new Date().toISOString(),
  t_ns: BigInt(Date.now()) * 1_000_000n,

  operations: [
    {
      op: 'add',
      subject: 'http://users.org/alice',
      predicate: 'http://schema.org/name',
      object: '"Alice Smith"'
    },
    {
      op: 'add',
      subject: 'http://users.org/alice',
      predicate: 'http://roles.org/hasRole',
      object: 'http://roles.org/admin'
    }
  ],

  source: {
    package: '@unrdf/user-management',
    actor: 'admin-user',
    context: { operation: 'createUser' }
  },

  admissibility: {
    policyId: 'user-creation-policy',
    preConditions: ['admin-permission-check'],
    constraints: ['unique-username']
  }
};

const receipt = await gate.proposeDelta(compositeDelta, store);

// All-or-none: both operations succeed or both fail
expect(receipt.applied).toBe(true);
expect(receipt.operationsApplied).toBe(2);
```

**SPARQL Query Example**: Verify delta was applied

```sparql
PREFIX wf: <http://workflow.org/>
PREFIX states: <http://workflow.org/states/>

SELECT ?state WHERE {
  <http://workflow.org/task-123> wf:state ?state .
  FILTER(?state = states:executing)
}
```

### Composition Rules

**Composes With**:
- ✅ **Receipt HOF**: `withReceipt(gate.proposeDelta)`
- ✅ **Zod Validation**: Validate delta before proposal
- ✅ **Determinism Proof**: Deterministic delta hashing
- ✅ **Composition Layer**: Cross-package deltas

**Receipt Chain**: Before → Delta → After

```javascript
const before = await store.freeze(); // Snapshot
const receipt = await gate.proposeDelta(delta, store);
const after = await store.freeze(); // Snapshot

const chainProof = {
  before: before.hash,
  delta: delta.id,
  after: after.hash,
  proof: receipt.stateHash === after.hash
};
```

### Testing Approach

```javascript
describe('Delta Contract Pattern', () => {
  it('should apply valid delta', async () => {
    const delta = createDelta('add', 'http://ex.org/s', 'http://ex.org/p', 'value');
    const gate = new DeltaGate();
    const store = await createStore();

    const receipt = await gate.proposeDelta(delta, store);

    expect(receipt.applied).toBe(true);
    expect(receipt.operationsApplied).toBe(1);
  });

  it('should reject delta violating preconditions', async () => {
    const delta = {
      ...createDelta('delete', 'http://ex.org/s', 'http://ex.org/p', 'value'),
      admissibility: {
        preConditions: ['triple-must-exist']
      }
    };

    const gate = new DeltaGate({
      policies: {
        'triple-must-exist': async (delta, store) => {
          const exists = await store.has(/* ... */);
          return exists;
        }
      }
    });

    const receipt = await gate.proposeDelta(delta, emptyStore);

    expect(receipt.applied).toBe(false);
    expect(receipt.reason).toContain('precondition failed');
  });
});
```

**File Locations**:
- Schema: `/home/user/unrdf/packages/v6-core/src/delta/schema.mjs`
- Gate: `/home/user/unrdf/packages/v6-core/src/delta/gate.mjs`
- API: `/home/user/unrdf/packages/v6-core/src/delta/index.mjs`

---

## Zod Validation Envelope Pattern

### Purpose & Use Cases

**Problem**: Runtime type errors crash applications at unexpected times.
**Solution**: Validate all inputs at module boundaries using Zod schemas.

**Use When**:
- Public API functions
- Data ingestion points
- Inter-package communication
- User input handling

**Don't Use When**:
- Private internal functions (trusted inputs)
- Performance-critical inner loops (validate once at boundary)

### Template & Pseudocode

```javascript
import { z } from 'zod';

// 1. Define schema
const InputSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1),
  age: z.number().int().positive()
});

const OutputSchema = z.object({
  success: z.boolean(),
  data: z.any()
});

// 2. Create validated function
function processData(input) {
  // Validate input
  const validInput = InputSchema.parse(input);

  // Business logic
  const result = {
    success: true,
    data: { processed: validInput.name }
  };

  // Validate output (optional but recommended)
  return OutputSchema.parse(result);
}

// 3. Composable validator HOF
export function validateSchema(schema) {
  return function validator(data) {
    return schema.parse(data); // Throws ZodError on failure
  };
}
```

### Real Examples

**Example 1: Delta Validation**

```javascript
import { DeltaSchema } from '@unrdf/v6-core/delta';

export async function proposeDelta(deltaData, store) {
  // Validate delta structure
  const delta = DeltaSchema.parse(deltaData);

  // Validate each operation
  delta.operations.forEach(op => {
    DeltaOperationSchema.parse(op);
  });

  // Proceed with validated delta
  return await gate.propose(delta, store);
}

// Usage
try {
  await proposeDelta({ id: 'invalid' }, store);
} catch (error) {
  console.error('Validation failed:', error.errors);
  // [
  //   { path: ['operations'], message: 'Required' },
  //   { path: ['source'], message: 'Required' }
  // ]
}
```

**Example 2: Composable Validation Pipeline**

```javascript
import { validateSchema } from '@unrdf/v6-compat/adapters';
import { withReceipt } from '@unrdf/v6-compat/adapters';

const UserSchema = z.object({
  username: z.string().min(3),
  email: z.string().email()
});

// Compose validators
const createUser = withReceipt(
  validateSchema(UserSchema)( // Input validation
    async (userData) => {
      // Business logic
      return await userStore.create(userData);
    }
  ),
  { operation: 'user.create' }
);

// Valid input
const { result, receipt } = await createUser({
  username: 'alice',
  email: 'alice@example.com'
});

// Invalid input - throws before execution
await createUser({ username: 'ab' }); // Error: username too short
```

**Example 3: Schema Generation from JSDoc**

```javascript
/**
 * @typedef {Object} Task
 * @property {string} id - UUID of task
 * @property {string} name - Task name
 * @property {'enabled'|'executing'|'completed'} state - Task state
 */

// Auto-generate Zod schema (via @unrdf/v6-compat/schema-generator)
import { generateZodFromJSDoc } from '@unrdf/v6-compat/schema-generator';

const TaskSchema = generateZodFromJSDoc(`
  @typedef {Object} Task
  @property {string} id
  @property {string} name
  @property {'enabled'|'executing'|'completed'} state
`);

// Equivalent to:
const TaskSchemaManual = z.object({
  id: z.string(),
  name: z.string(),
  state: z.enum(['enabled', 'executing', 'completed'])
});
```

### Composition Rules

**Composes With**:
- ✅ **Receipt HOF**: Validate before generating receipt
- ✅ **Delta Contract**: Validate delta structure
- ✅ **Determinism Proof**: Schema ensures consistent structure
- ✅ **Composition Layer**: Schema compatibility checking

**Output → Input Matching**:
```javascript
// Module A output schema
const AOutputSchema = z.object({
  result: z.string(),
  metadata: z.any()
});

// Module B input schema
const BInputSchema = z.object({
  result: z.string(),
  metadata: z.any()
});

// Check compatibility (composition layer L5)
const compatible = AOutputSchema.safeParse(
  BInputSchema.parse({ result: 'test', metadata: {} })
).success;
```

### Testing Approach

```javascript
describe('Zod Validation Envelope Pattern', () => {
  it('should accept valid input', () => {
    const schema = z.object({ id: z.string(), value: z.number() });

    const data = { id: 'test', value: 42 };
    const validated = schema.parse(data);

    expect(validated).toEqual(data);
  });

  it('should reject invalid input with readable errors', () => {
    const schema = z.object({ id: z.string().uuid(), value: z.number().positive() });

    try {
      schema.parse({ id: 'not-a-uuid', value: -1 });
      fail('Should have thrown');
    } catch (error) {
      expect(error.errors).toHaveLength(2);
      expect(error.errors[0].path).toEqual(['id']);
      expect(error.errors[1].path).toEqual(['value']);
    }
  });

  it('should compose with other patterns', async () => {
    const InputSchema = z.object({ x: z.number() });
    const OutputSchema = z.object({ result: z.number() });

    const fn = withReceipt(
      validateSchema(InputSchema)(
        (data) => ({ result: data.x * 2 })
      )
    );

    const { result, receipt } = await fn({ x: 21 });

    expect(OutputSchema.parse(result).result).toBe(42);
    expect(receipt.operation).toBeDefined();
  });
});
```

**File Location**: `/home/user/unrdf/packages/v6-compat/src/adapters.mjs` (lines 284-303)

---

## Determinism Proof Pattern

### Purpose & Use Cases

**Problem**: Operations produce different outputs on repeated runs, breaking replay/verification.
**Solution**: Guarantee identical outputs for identical inputs via deterministic hashing and timestamps.

**Use When**:
- L3+ maturity packages (required)
- Receipt generation
- Snapshot creation
- Test verification

**Don't Use When**:
- Operations requiring real-time data
- User-facing timestamps (use separate display timestamp)

### Template & Pseudocode

```javascript
// Pattern Template: Deterministic Execution

// 1. Enable deterministic mode
process.env.DETERMINISTIC = '1';

// 2. Use deterministic serialization
function deterministicSerialize(obj) {
  if (typeof obj !== 'object') return JSON.stringify(obj);

  // Sort keys alphabetically
  const sortedKeys = Object.keys(obj).sort();
  const pairs = sortedKeys.map(key => {
    const value = deterministicSerialize(obj[key]);
    return `${JSON.stringify(key)}:${value}`;
  });

  return `{${pairs.join(',')}}`;
}

// 3. Deterministic timestamp from payload hash
const timestampCache = new Map();

function getDeterministicTimestamp(payload) {
  const hash = computeHash(deterministicSerialize(payload));

  if (!timestampCache.has(hash)) {
    timestampCache.set(hash, BigInt(Date.now()) * 1_000_000n);
  }

  return timestampCache.get(hash);
}

// 4. Deterministic receipt generation
async function generateReceipt(eventType, payload) {
  const t_ns = getDeterministicTimestamp(payload);
  const payloadHash = await computeBlake3(deterministicSerialize(payload));

  return {
    id: crypto.randomUUID(), // Still random for uniqueness
    eventType,
    timestamp: t_ns,
    payloadHash,
    payload
  };
}
```

### Real Examples

**Example 1: Deterministic Receipt**

```javascript
import { deterministicSerialize, computeBlake3 } from '@unrdf/v6-core/receipts/base-receipt';

// Run 100 times - same input produces same hash
const payload = { action: 'approve', taskId: 'task-123' };

const hashes = await Promise.all(
  Array.from({ length: 100 }, async () => {
    const serialized = deterministicSerialize(payload);
    return await computeBlake3(serialized);
  })
);

// All hashes identical
const uniqueHashes = new Set(hashes);
expect(uniqueHashes.size).toBe(1);

console.log(hashes[0]);
// Always: 'abc123...def456' (64-char BLAKE3 hash)
```

**Example 2: Deterministic Timestamp Generation**

```javascript
import { getTimestampForPayload } from '@unrdf/fusion/receipts-kernel';

process.env.DETERMINISTIC = '1';

const payload1 = { task: 'A', value: 100 };
const payload2 = { task: 'A', value: 100 }; // Identical
const payload3 = { task: 'B', value: 200 }; // Different

const t1 = getTimestampForPayload('workflow', payload1);
const t2 = getTimestampForPayload('workflow', payload2);
const t3 = getTimestampForPayload('workflow', payload3);

// Same payload → same timestamp
expect(t1).toBe(t2);

// Different payload → different timestamp (monotonic)
expect(t3).not.toBe(t1);
expect(t3 > t1).toBe(true); // Monotonic increase
```

**Example 3: Deterministic Snapshot Hash**

```javascript
import { freezeUniverse } from '@unrdf/kgc-4d';

// Create identical stores
const store1 = await createStore();
await store1.add(quad('http://ex.org/s', 'http://ex.org/p', 'value'));

const store2 = await createStore();
await store2.add(quad('http://ex.org/s', 'http://ex.org/p', 'value'));

// Freeze both
const snapshot1 = await freezeUniverse(store1);
const snapshot2 = await freezeUniverse(store2);

// Hashes match (deterministic)
expect(snapshot1.hash).toBe(snapshot2.hash);

console.log(snapshot1.hash);
// 'def789...ghi012' (same every time for same data)
```

### Composition Rules

**Composes With**:
- ✅ **Receipt HOF**: Use deterministic timestamps in receipts
- ✅ **Delta Contract**: Hash deltas deterministically
- ✅ **Zod Validation**: Schema ensures structure consistency
- ✅ **Composition Layer**: Verify cross-package determinism

**Determinism Chain**:
```javascript
// Full deterministic workflow
process.env.DETERMINISTIC = '1';

// 1. Deterministic input
const input = deterministicSerialize({ action: 'create', id: 'res-1' });

// 2. Deterministic operation
const delta = createDelta('add', ...fromInput(input));

// 3. Deterministic receipt
const receipt = await generateReceipt('delta.applied', delta);

// 4. Deterministic state hash
const stateHash = await computeBlake3(store.serialize());

// Run 100x → all hashes identical
```

### Testing Approach

**Test Structure**: Run 100x, verify identical outputs

```javascript
describe('Determinism Proof Pattern', () => {
  beforeEach(() => {
    process.env.DETERMINISTIC = '1';
  });

  it('should produce identical hashes for identical inputs', async () => {
    const payload = { x: 1, y: 2 };

    const hashes = await Promise.all(
      Array.from({ length: 100 }, () =>
        computeBlake3(deterministicSerialize(payload))
      )
    );

    const unique = new Set(hashes);
    expect(unique.size).toBe(1);
  });

  it('should produce monotonic timestamps for different inputs', () => {
    const timestamps = [];

    for (let i = 0; i < 100; i++) {
      const payload = { sequence: i };
      timestamps.push(getDeterministicTimestamp(payload));
    }

    // Check monotonic increase
    for (let i = 1; i < timestamps.length; i++) {
      expect(timestamps[i] >= timestamps[i-1]).toBe(true);
    }
  });

  it('should verify receipt chain integrity', async () => {
    const receipts = [];
    let previousHash = null;

    for (let i = 0; i < 10; i++) {
      const receipt = await generateReceipt('test', { i });
      receipt.previousHash = previousHash;
      receipts.push(receipt);
      previousHash = receipt.hash;
    }

    // Re-run 100x - chains should match
    const chains = await Promise.all(
      Array.from({ length: 100 }, async () => {
        const chain = [];
        let prev = null;
        for (let i = 0; i < 10; i++) {
          const r = await generateReceipt('test', { i });
          r.previousHash = prev;
          chain.push(r.hash);
          prev = r.hash;
        }
        return chain.join(':');
      })
    );

    const uniqueChains = new Set(chains);
    expect(uniqueChains.size).toBe(1);
  });
});
```

**Determinism Checker Tool**:
```bash
#!/bin/bash
# determinism-checker.sh

ITERATIONS=100
OPERATION=$1

for i in $(seq 1 $ITERATIONS); do
  node -e "console.log(require('./lib/operation.mjs').$OPERATION())" >> /tmp/run-$i.txt
done

# Compare all outputs
FIRST=$(cat /tmp/run-1.txt)
MISMATCHES=0

for i in $(seq 2 $ITERATIONS); do
  CURRENT=$(cat /tmp/run-$i.txt)
  if [ "$FIRST" != "$CURRENT" ]; then
    echo "MISMATCH at iteration $i"
    MISMATCHES=$((MISMATCHES + 1))
  fi
done

if [ $MISMATCHES -eq 0 ]; then
  echo "✅ DETERMINISTIC: All $ITERATIONS runs produced identical output"
else
  echo "❌ NON-DETERMINISTIC: $MISMATCHES mismatches detected"
fi
```

**File Locations**:
- Serialization: `/home/user/unrdf/packages/v6-core/src/receipts/base-receipt.mjs` (lines 172-199)
- Timestamp: `/home/user/unrdf/packages/fusion/src/receipts-kernel.mjs` (lines 96-100)

---

## Composition Layer Pattern (L5)

### Purpose & Use Cases

**Problem**: Packages work in isolation; cross-package workflows fail due to incompatible interfaces.
**Solution**: Define composition rules based on schema compatibility and receipt chaining.

**Use When**:
- Multi-package workflows
- Cross-package data flow
- Federated systems
- L5 maturity certification

**Don't Use When**:
- Single-package operations
- Internal module composition (use regular imports)

### Template & Pseudocode

```javascript
// Pattern Template: Module Composition

// 1. Define compatibility rules
class CompositionChecker {
  canCompose(moduleA, moduleB) {
    // Check output schema of A matches input schema of B
    const aOutput = moduleA.outputSchema;
    const bInput = moduleB.inputSchema;

    return this.schemasCompatible(aOutput, bInput);
  }

  schemasCompatible(schemaA, schemaB) {
    // Test if A's output can satisfy B's input
    try {
      const testData = this.generateTestData(schemaA);
      schemaB.parse(testData); // Will throw if incompatible
      return true;
    } catch {
      return false;
    }
  }
}

// 2. Compose operations
async function composeOperations(opA, opB) {
  // Execute A
  const { result: resultA, receipt: receiptA } = await opA();

  // Verify A's output matches B's input
  const compatible = opB.inputSchema.safeParse(resultA).success;
  if (!compatible) {
    throw new Error('Composition failed: incompatible schemas');
  }

  // Execute B with A's output
  const { result: resultB, receipt: receiptB } = await opB(resultA);

  // Chain receipts
  receiptB.previousReceipt = receiptA.id;

  return { result: resultB, receipts: [receiptA, receiptB] };
}
```

### Real Examples

**Example 1: Oxigraph → KGC → Workflow Composition**

```javascript
import { createStore } from '@unrdf/oxigraph';
import { freezeUniverse } from '@unrdf/kgc-4d';
import { execute } from '@unrdf/yawl';

// Module A: Oxigraph store operation
const storeOp = withReceipt(
  async () => {
    const store = await createStore();
    await store.add(/* quads */);
    return { store, quadCount: await store.size() };
  },
  { operation: 'oxigraph.create' }
);

// Module B: KGC snapshot
const snapshotOp = withReceipt(
  async ({ store }) => {
    const snapshot = await freezeUniverse(store);
    return { snapshot, hash: snapshot.hash };
  },
  { operation: 'kgc.freeze' }
);

// Module C: Workflow execution
const workflowOp = withReceipt(
  async ({ snapshot }) => {
    const result = await execute({
      task: 'verify-snapshot',
      input: snapshot
    });
    return { verified: result.status === 'ok' };
  },
  { operation: 'workflow.execute' }
);

// Compose A → B → C
const { result: storeResult, receipt: r1 } = await storeOp();
const { result: snapshotResult, receipt: r2 } = await snapshotOp(storeResult);
const { result: workflowResult, receipt: r3 } = await workflowOp(snapshotResult);

// Receipt chain spans packages
console.log([r1.operation, r2.operation, r3.operation]);
// ['oxigraph.create', 'kgc.freeze', 'workflow.execute']

// Verify chain integrity
expect(r2.args).toContain(r1.result);
expect(r3.args).toContain(r2.result);
```

**Example 2: Module Compatibility Matrix**

```javascript
// Define all module interfaces
const modules = {
  '@unrdf/oxigraph': {
    outputSchema: z.object({
      store: z.any(),
      quadCount: z.number()
    })
  },
  '@unrdf/kgc-4d': {
    inputSchema: z.object({
      store: z.any()
    }),
    outputSchema: z.object({
      snapshot: z.any(),
      hash: z.string()
    })
  },
  '@unrdf/yawl': {
    inputSchema: z.object({
      snapshot: z.any()
    }),
    outputSchema: z.object({
      verified: z.boolean()
    })
  }
};

// Generate compatibility matrix
const checker = new CompositionChecker();

const matrix = Object.keys(modules).map(modA =>
  Object.keys(modules).map(modB => ({
    from: modA,
    to: modB,
    compatible: checker.canCompose(modules[modA], modules[modB])
  }))
);

console.table(matrix);
// ┌──────────────────────┬──────────────────────┬────────────┐
// │ from                 │ to                   │ compatible │
// ├──────────────────────┼──────────────────────┼────────────┤
// │ @unrdf/oxigraph      │ @unrdf/kgc-4d        │ true       │
// │ @unrdf/kgc-4d        │ @unrdf/yawl          │ true       │
// │ @unrdf/yawl          │ @unrdf/oxigraph      │ false      │
// └──────────────────────┴──────────────────────┴────────────┘
```

**Example 3: Cross-Package Delta Composition**

```javascript
// Package A: Resource creation
const createResourceDelta = {
  id: 'delta-1',
  operations: [
    { op: 'add', subject: 'http://ex.org/res-1', predicate: 'rdf:type', object: 'Resource' }
  ],
  source: { package: '@unrdf/resources' }
};

// Package B: Workflow task linking (depends on A)
const linkWorkflowDelta = {
  id: 'delta-2',
  operations: [
    { op: 'add', subject: 'http://ex.org/task-1', predicate: 'uses', object: 'http://ex.org/res-1' }
  ],
  source: { package: '@unrdf/yawl' },
  admissibility: {
    preConditions: ['resource-exists'] // Depends on delta-1
  }
};

// Apply deltas in order
const receipt1 = await gate.proposeDelta(createResourceDelta, store);
const receipt2 = await gate.proposeDelta(linkWorkflowDelta, store);

// Chain receipts
receipt2.previousDelta = receipt1.deltaId;

// Verify composition
expect(receipt1.applied).toBe(true);
expect(receipt2.applied).toBe(true);
expect(receipt2.previousDelta).toBe('delta-1');
```

### Composition Rules

**Legal Compositions**:
| Module A | Module B | Compatible | Reason |
|----------|----------|------------|--------|
| @unrdf/oxigraph | @unrdf/kgc-4d | ✅ Yes | Store output → Freeze input |
| @unrdf/kgc-4d | @unrdf/yawl | ✅ Yes | Snapshot → Workflow input |
| @unrdf/yawl | @unrdf/federation | ✅ Yes | Workflow result → Query input |
| @unrdf/federation | @unrdf/oxigraph | ❌ No | Query result ≠ Store creation |

**Composition Recipes**:
```javascript
// Recipe 1: Data Pipeline
oxigraph.create()
  → kgc.freeze()
  → workflow.execute()
  → federation.query()

// Recipe 2: Verification Pipeline
workflow.execute()
  → kgc.verify()
  → blockchain.anchor()

// Recipe 3: Update Pipeline
federation.query()
  → delta.create()
  → gate.apply()
  → kgc.freeze()
```

### Testing Approach

```javascript
describe('Composition Layer Pattern', () => {
  it('should compose modules with compatible schemas', async () => {
    const modA = {
      execute: async () => ({ value: 42 }),
      outputSchema: z.object({ value: z.number() })
    };

    const modB = {
      execute: async ({ value }) => ({ doubled: value * 2 }),
      inputSchema: z.object({ value: z.number() })
    };

    const checker = new CompositionChecker();
    expect(checker.canCompose(modA, modB)).toBe(true);
  });

  it('should reject incompatible modules', () => {
    const modA = {
      outputSchema: z.object({ name: z.string() })
    };

    const modB = {
      inputSchema: z.object({ value: z.number() })
    };

    const checker = new CompositionChecker();
    expect(checker.canCompose(modA, modB)).toBe(false);
  });

  it('should create receipt chains across packages', async () => {
    const receipts = [];

    // Execute 3 operations
    receipts.push((await operation1()).receipt);
    receipts.push((await operation2()).receipt);
    receipts.push((await operation3()).receipt);

    // Verify chain
    expect(receipts[1].previousReceipt).toBe(receipts[0].id);
    expect(receipts[2].previousReceipt).toBe(receipts[1].id);
  });
});
```

**File Locations**:
- Pattern inferred from: `/home/user/unrdf/docs/v6/PROGRAM_CHARTER.md` (lines 380-391)
- Composition examples: Various cross-package workflows

---

## Pattern Compatibility Matrix

This matrix shows which patterns compose with each other:

|  | Receipt HOF | Delta Contract | Zod Validation | Determinism Proof | Composition Layer |
|--|-------------|----------------|----------------|-------------------|-------------------|
| **Receipt HOF** | N/A | ✅ Wrap delta ops | ✅ Validate before wrap | ✅ Deterministic receipts | ✅ Receipt chains |
| **Delta Contract** | ✅ Receipt delta | N/A | ✅ Validate delta | ✅ Hash delta | ✅ Cross-package deltas |
| **Zod Validation** | ✅ Input validation | ✅ Delta validation | N/A | ✅ Schema consistency | ✅ Schema compat check |
| **Determinism Proof** | ✅ Hash receipts | ✅ Hash deltas | ✅ Validate structure | N/A | ✅ Verify determinism |
| **Composition Layer** | ✅ Chain receipts | ✅ Compose deltas | ✅ Check schemas | ✅ Cross-package determinism | N/A |

**Legend**: ✅ Composes well | ⚠️ Composes with caveats | ❌ Don't compose | N/A = Self-composition

---

## Next Steps

1. **For Pattern Users**: See [/docs/v6/tutorials/](/home/user/unrdf/docs/v6/tutorials/) for step-by-step guides
2. **For Package Maintainers**: See [/docs/v6/MIGRATION_RUNBOOKS.md](/home/user/unrdf/docs/v6/MIGRATION_RUNBOOKS.md) for migration guides
3. **For Architecture Review**: See [/docs/v6/PROGRAM_CHARTER.md](/home/user/unrdf/docs/v6/PROGRAM_CHARTER.md) for v6 design rationale

---

## References

- **v6 Migration Plan**: [/docs/v6/MIGRATION_PLAN.md](/home/user/unrdf/docs/v6/MIGRATION_PLAN.md)
- **Maturity Ladder**: [/docs/v6/MATURITY_LADDER.md](/home/user/unrdf/docs/v6/MATURITY_LADDER.md)
- **BB80/20 Methodology**: [/docs/bb80-20-methodology.md](/home/user/unrdf/docs/bb80-20-methodology.md)
- **Receipt Standard**: [/docs/RECEIPT-STANDARD.md](/home/user/unrdf/docs/RECEIPT-STANDARD.md)

---

**Pattern Library Version**: 1.0.0
**Last Research Update**: 2025-12-27
**Researcher**: Claude Code (Researcher Agent)
