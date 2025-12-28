# v6 Migration Runbooks (P1 Packages)

**Version**: 6.0.0-alpha.1
**Status**: Implementation Ready
**Last Updated**: 2025-12-27

## Overview

This document provides step-by-step migration runbooks for the **10 P1 (Priority 1) packages** in the UNRDF v6 migration. Each runbook follows the same structure:

1. **Current State Assessment**
2. **Pattern Application Plan**
3. **Step-by-Step Commands**
4. **Testing Checklist**
5. **Common Issues & Fixes**

**Maturity Target**: All P1 packages → L3+ (Deterministic outputs + receipts)

---

## Package Index

| Package | Current Level | Target Level | Effort (hours) | Status |
|---------|---------------|--------------|----------------|--------|
| [@unrdf/oxigraph](#1-unrdfoxigraph) | L1 | L5 | 20h | Planned |
| [@unrdf/core](#2-unrdfcore) | L2 | L5 | 24h | Planned |
| [@unrdf/kgc-4d](#3-unrdfkgc-4d) | L3 | L5 | 16h | Planned |
| [@unrdf/hooks](#4-unrdfhooks) | L2 | L4 | 18h | Planned |
| [@unrdf/streaming](#5-unrdfstreaming) | L1 | L3 | 20h | Planned |
| [@unrdf/federation](#6-unrdffederation) | L2 | L4 | 22h | Planned |
| [@unrdf/cli](#7-unrdfcli) | L2 | L3 | 16h | Planned |
| [@unrdf/yawl](#8-unrdfyawl) | L3 | L5 | 24h | Planned |
| [@unrdf/knowledge-engine](#9-unrdfknowledge-engine) | L1 | L3 | 18h | Planned |
| [@unrdf/graph-analytics](#10-unrdfgraph-analytics) | L1 | L3 | 18h | Planned |

**Total**: 196 hours (parallelizable across 5 developers = ~5 weeks)

---

## 1. @unrdf/oxigraph

**Priority**: P1 (Core Foundation)
**Current Level**: L1
**Target Level**: L5
**Effort**: 20 hours

### Current State Assessment

**What Exists**:
- Pure Oxigraph WASM adapter
- `createStore()` async factory
- Basic SPARQL execution
- No receipts
- No Zod validation

**What's Missing**:
- Receipt generation for store operations
- Zod schemas for store API
- Deterministic snapshot hashing
- Cross-package composition support

### Pattern Application Plan

1. **Receipt HOF**: Wrap all store operations (`add`, `delete`, `query`)
2. **Zod Validation**: Define schemas for quads, query results
3. **Determinism Proof**: Deterministic serialization of store contents
4. **Composition Layer**: Output schemas match KGC-4D inputs

### Step-by-Step Commands

#### Step 1: Add Dependencies

```bash
cd /home/user/unrdf/packages/oxigraph

# Add Zod and receipt dependencies
pnpm add zod hash-wasm
pnpm add -D @unrdf/v6-core
```

#### Step 2: Create Schemas

Create `/home/user/unrdf/packages/oxigraph/src/schemas.mjs`:

```javascript
import { z } from 'zod';

export const QuadSchema = z.object({
  subject: z.object({
    termType: z.literal('NamedNode'),
    value: z.string()
  }),
  predicate: z.object({
    termType: z.literal('NamedNode'),
    value: z.string()
  }),
  object: z.union([
    z.object({
      termType: z.literal('NamedNode'),
      value: z.string()
    }),
    z.object({
      termType: z.literal('Literal'),
      value: z.string(),
      datatype: z.object({ value: z.string() }).optional()
    })
  ]),
  graph: z.object({
    termType: z.literal('DefaultGraph')
  }).optional()
});

export const StoreSnapshotSchema = z.object({
  quads: z.array(QuadSchema),
  quadCount: z.number().int().nonnegative(),
  hash: z.string().length(64) // BLAKE3
});
```

#### Step 3: Wrap Store Operations

Update `/home/user/unrdf/packages/oxigraph/src/index.mjs`:

```javascript
import { withReceipt } from '@unrdf/v6-core/receipts';
import { QuadSchema } from './schemas.mjs';

export async function createStore(options = {}) {
  const store = /* ... Oxigraph initialization ... */;

  // Wrap add operation
  store.addWithReceipt = withReceipt(
    async (quad) => {
      QuadSchema.parse(quad); // Validate
      await store.add(quad);
      return { added: true, quadCount: await store.size() };
    },
    { operation: 'oxigraph.add' }
  );

  // Wrap delete operation
  store.deleteWithReceipt = withReceipt(
    async (quad) => {
      QuadSchema.parse(quad); // Validate
      await store.delete(quad);
      return { deleted: true, quadCount: await store.size() };
    },
    { operation: 'oxigraph.delete' }
  );

  return store;
}
```

#### Step 4: Add Deterministic Snapshot

Create `/home/user/unrdf/packages/oxigraph/src/snapshot.mjs`:

```javascript
import { computeBlake3, deterministicSerialize } from '@unrdf/v6-core/receipts/base-receipt';
import { StoreSnapshotSchema } from './schemas.mjs';

export async function snapshotStore(store) {
  // Extract all quads (deterministic order)
  const quads = [];
  for await (const quad of store.match()) {
    quads.push({
      subject: { termType: quad.subject.termType, value: quad.subject.value },
      predicate: { termType: quad.predicate.termType, value: quad.predicate.value },
      object: { termType: quad.object.termType, value: quad.object.value }
    });
  }

  // Sort deterministically (by subject, predicate, object)
  quads.sort((a, b) => {
    const aStr = `${a.subject.value}${a.predicate.value}${a.object.value}`;
    const bStr = `${b.subject.value}${b.predicate.value}${b.object.value}`;
    return aStr.localeCompare(bStr);
  });

  const snapshot = {
    quads,
    quadCount: quads.length,
    hash: await computeBlake3(deterministicSerialize(quads))
  };

  return StoreSnapshotSchema.parse(snapshot);
}
```

#### Step 5: Update Tests

Create `/home/user/unrdf/packages/oxigraph/test/receipts.test.mjs`:

```javascript
import { describe, it, expect } from 'vitest';
import { createStore } from '../src/index.mjs';
import { snapshotStore } from '../src/snapshot.mjs';
import { dataFactory } from '@rdfjs/data-model';

describe('Oxigraph Receipt Integration', () => {
  it('should generate receipt for add operation', async () => {
    const store = await createStore();
    const quad = dataFactory.quad(
      dataFactory.namedNode('http://ex.org/s'),
      dataFactory.namedNode('http://ex.org/p'),
      dataFactory.literal('value')
    );

    const { result, receipt } = await store.addWithReceipt(quad);

    expect(result.added).toBe(true);
    expect(receipt.operation).toBe('oxigraph.add');
    expect(receipt.timestamp).toBeGreaterThan(0);
  });

  it('should produce deterministic snapshots', async () => {
    const store1 = await createStore();
    const store2 = await createStore();

    const quad = dataFactory.quad(
      dataFactory.namedNode('http://ex.org/s'),
      dataFactory.namedNode('http://ex.org/p'),
      dataFactory.literal('value')
    );

    await store1.add(quad);
    await store2.add(quad);

    const snapshot1 = await snapshotStore(store1);
    const snapshot2 = await snapshotStore(store2);

    expect(snapshot1.hash).toBe(snapshot2.hash);
  });
});
```

#### Step 6: Run Tests

```bash
cd /home/user/unrdf/packages/oxigraph
timeout 10s pnpm test

# Expected output:
# ✓ should generate receipt for add operation
# ✓ should produce deterministic snapshots
# Tests: 2 passed (2 total)
```

### Testing Checklist

- [ ] All store operations generate receipts
- [ ] Zod schemas validate all inputs
- [ ] Snapshots are deterministic (100x test)
- [ ] Receipt chains work across operations
- [ ] Integration test with @unrdf/kgc-4d passes

### Common Issues & Fixes

**Issue**: Snapshot hash changes on every run
**Fix**: Ensure quads are sorted deterministically before hashing

**Issue**: Receipt timestamp not deterministic
**Fix**: Use `DETERMINISTIC=1` env var during tests

**Issue**: Zod validation fails on valid quads
**Fix**: Check quad structure matches `QuadSchema` exactly

---

## 2. @unrdf/core

**Priority**: P1 (Core Foundation)
**Current Level**: L2
**Target Level**: L5
**Effort**: 24 hours

### Current State Assessment

**What Exists**:
- RDF operations (add, delete, query)
- SPARQL utilities
- Basic Zod schemas (partial)
- No receipts

**What's Missing**:
- Receipt HOF for all operations
- Complete Zod coverage
- Deterministic SPARQL results
- Delta Contract integration

### Pattern Application Plan

1. **Receipt HOF**: Wrap SPARQL queries and RDF operations
2. **Delta Contract**: All mutations via Delta
3. **Zod Validation**: Complete schema coverage
4. **Determinism Proof**: Deterministic query result ordering

### Step-by-Step Commands

#### Step 1: Create Delta Adapter

Create `/home/user/unrdf/packages/core/src/delta-adapter.mjs`:

```javascript
import { createDelta } from '@unrdf/v6-core/delta';

export function createRDFDelta(op, subject, predicate, object, options = {}) {
  return createDelta(op, subject, predicate, object, {
    ...options,
    package: '@unrdf/core'
  });
}

export async function applyDelta(delta, store) {
  // Convert delta to RDF operations
  for (const operation of delta.operations) {
    if (operation.op === 'add') {
      await store.add(/* quad from operation */);
    } else if (operation.op === 'delete') {
      await store.delete(/* quad from operation */);
    } else if (operation.op === 'update') {
      await store.delete(/* old quad */);
      await store.add(/* new quad */);
    }
  }
}
```

#### Step 2: Add Receipt to SPARQL

Update `/home/user/unrdf/packages/core/src/sparql.mjs`:

```javascript
import { withReceipt } from '@unrdf/v6-core/receipts';

export const executeSparql = withReceipt(
  async (store, query, options = {}) => {
    // Execute SPARQL query
    const results = await store.query(query);

    // Deterministic ordering (if SELECT query)
    if (results.type === 'bindings') {
      results.bindings.sort((a, b) => {
        // Sort by all variables alphabetically
        const aStr = JSON.stringify(a);
        const bStr = JSON.stringify(b);
        return aStr.localeCompare(bStr);
      });
    }

    return results;
  },
  { operation: 'sparql.execute' }
);
```

#### Step 3: Update Tests

Create `/home/user/unrdf/packages/core/test/delta.test.mjs`:

```javascript
import { describe, it, expect } from 'vitest';
import { createRDFDelta, applyDelta } from '../src/delta-adapter.mjs';
import { createStore } from '@unrdf/oxigraph';

describe('Core Delta Integration', () => {
  it('should create and apply delta', async () => {
    const delta = createRDFDelta(
      'add',
      'http://ex.org/s',
      'http://ex.org/p',
      'value'
    );

    const store = await createStore();
    await applyDelta(delta, store);

    const size = await store.size();
    expect(size).toBe(1);
  });

  it('should generate receipt for delta application', async () => {
    const delta = createRDFDelta('add', 'http://ex.org/s', 'http://ex.org/p', 'value');

    const applyWithReceipt = withReceipt(applyDelta, { operation: 'core.applyDelta' });

    const store = await createStore();
    const { result, receipt } = await applyWithReceipt(delta, store);

    expect(receipt.operation).toBe('core.applyDelta');
  });
});
```

#### Step 4: Run Tests

```bash
cd /home/user/unrdf/packages/core
timeout 10s pnpm test

# Expected: All tests pass with receipts generated
```

### Testing Checklist

- [ ] All RDF operations use Delta Contract
- [ ] SPARQL queries generate receipts
- [ ] Query results deterministically ordered
- [ ] Integration with @unrdf/oxigraph works
- [ ] All schemas validated

---

## 3. @unrdf/kgc-4d

**Priority**: P1 (Core Foundation)
**Current Level**: L3
**Target Level**: L5
**Effort**: 16 hours

### Current State Assessment

**What Exists**:
- Receipt generation (BLAKE3)
- `freezeUniverse()` snapshot function
- Deterministic hashing (already implemented!)
- Git integration

**What's Missing**:
- Cross-package composition support
- Zod schemas for receipts
- Delta Contract integration

### Pattern Application Plan

1. **Zod Validation**: Add receipt schemas
2. **Composition Layer**: Output schemas for workflow integration
3. **Delta Contract**: Track deltas in freeze operations

### Step-by-Step Commands

#### Step 1: Add Receipt Schemas

Create `/home/user/unrdf/packages/kgc-4d/src/receipt-schema.mjs`:

```javascript
import { z } from 'zod';
import { BaseReceiptSchema } from '@unrdf/v6-core/receipts/base-receipt';

export const KGCFreezeReceiptSchema = BaseReceiptSchema.extend({
  receiptType: z.literal('kgc-freeze'),
  payload: z.object({
    storeHash: z.string().length(64),
    quadCount: z.number().int().nonnegative(),
    gitRef: z.string().optional(),
    timestamp: z.string()
  })
});
```

#### Step 2: Update Freeze Function

Update `/home/user/unrdf/packages/kgc-4d/src/freeze.mjs`:

```javascript
import { KGCFreezeReceiptSchema } from './receipt-schema.mjs';
import { computeBlake3 } from '@unrdf/v6-core/receipts/base-receipt';

export async function freezeUniverse(store) {
  const quadCount = await store.size();
  const storeHash = await computeBlake3(/* store contents */);

  const receipt = {
    id: crypto.randomUUID(),
    receiptType: 'kgc-freeze',
    t_ns: BigInt(Date.now()) * 1_000_000n,
    timestamp_iso: new Date().toISOString(),
    previousHash: null,
    payloadHash: storeHash,
    receiptHash: await computeBlake3({ storeHash, quadCount }),
    payload: {
      storeHash,
      quadCount,
      timestamp: new Date().toISOString()
    }
  };

  return KGCFreezeReceiptSchema.parse(receipt);
}
```

#### Step 3: Add Composition Support

Create `/home/user/unrdf/packages/kgc-4d/src/composition.mjs`:

```javascript
import { z } from 'zod';

export const KGCOutputSchema = z.object({
  snapshot: z.object({
    hash: z.string().length(64),
    quadCount: z.number(),
    timestamp: z.string()
  }),
  receipt: z.any()
});

export function getOutputSchema() {
  return KGCOutputSchema;
}
```

#### Step 4: Run Tests

```bash
cd /home/user/unrdf/packages/kgc-4d
timeout 10s pnpm test

# Verify:
# - Receipt schema validation passes
# - Output schema matches expected format
# - Composition with @unrdf/yawl works
```

### Testing Checklist

- [ ] Freeze operations generate valid receipts
- [ ] Receipt schema validation passes
- [ ] Output schema defined for composition
- [ ] Integration with @unrdf/yawl passes

---

## 4. @unrdf/hooks

**Priority**: P1 (Essential Infrastructure)
**Current Level**: L2
**Target Level**: L4
**Effort**: 18 hours

### Pattern Application Plan

1. **Receipt HOF**: Hook execution generates receipts
2. **Zod Validation**: Hook input/output schemas
3. **Delta Contract**: Hooks validate deltas before application

### Step-by-Step Commands

#### Step 1: Add Hook Schemas

Create `/home/user/unrdf/packages/hooks/src/schemas.mjs`:

```javascript
import { z } from 'zod';

export const HookContextSchema = z.object({
  delta: z.any().optional(),
  store: z.any(),
  metadata: z.record(z.any()).optional()
});

export const HookResultSchema = z.object({
  allowed: z.boolean(),
  reason: z.string().optional(),
  modifications: z.any().optional()
});
```

#### Step 2: Wrap Hook Execution

Update `/home/user/unrdf/packages/hooks/src/execute.mjs`:

```javascript
import { withReceipt } from '@unrdf/v6-core/receipts';
import { HookContextSchema, HookResultSchema } from './schemas.mjs';

export const executeHook = withReceipt(
  async (hook, context) => {
    // Validate context
    const validContext = HookContextSchema.parse(context);

    // Execute hook
    const result = await hook.handler(validContext);

    // Validate result
    return HookResultSchema.parse(result);
  },
  { operation: 'hooks.execute' }
);
```

#### Step 3: Delta Validation Hook

Create `/home/user/unrdf/packages/hooks/src/delta-validator.mjs`:

```javascript
import { DeltaSchema } from '@unrdf/v6-core/delta';

export function createDeltaValidationHook(policyFn) {
  return {
    name: 'delta-validator',
    handler: async (context) => {
      // Validate delta structure
      const delta = DeltaSchema.parse(context.delta);

      // Run policy check
      const allowed = await policyFn(delta, context.store);

      return {
        allowed,
        reason: allowed ? undefined : 'Policy check failed'
      };
    }
  };
}
```

---

## 5-10: Quick Migration Guides

### 5. @unrdf/streaming

**Key Changes**:
- Convert EventEmitter → AsyncIterator
- Add receipts to stream completion
- Deterministic chunk ordering

**Commands**:
```bash
# Add stream adapter
pnpm add @unrdf/v6-compat

# Wrap streams
import { streamToAsync } from '@unrdf/v6-compat/adapters';
for await (const quad of streamToAsync(legacyStream)) {
  // Process
}
```

### 6. @unrdf/federation

**Key Changes**:
- SPARQL string → template literal
- Add timeout enforcement
- Receipt for query execution

**Commands**:
```bash
# Update queries
const results = await federation.query(
  sparql`SELECT * WHERE { ?s ?p ?o }`
    .timeout(5000)
    .receipt(true)
);
```

### 7. @unrdf/cli

**Key Changes**:
- Add `kgc delta` commands
- Add `kgc receipt` commands
- JSON output mode

**Commands**:
```bash
# Register commands
kgc delta create --file delta.json
kgc receipt verify --chain --from genesis
```

### 8. @unrdf/yawl

**Key Changes**:
- Workflow transitions → Deltas
- Task state changes generate receipts
- Deterministic workflow execution

**Commands**:
```bash
# Wrap workflow execution
const { result, receipt } = await workflow.execute(task);
await kgc.freeze(receipt);
```

### 9. @unrdf/knowledge-engine

**Key Changes**:
- Reasoning operations → Receipts
- Inference rules validation

**Commands**:
```bash
# Add receipts to inference
const { inferences, receipt } = await engine.infer(rules, store);
```

### 10. @unrdf/graph-analytics

**Key Changes**:
- Algorithm execution → Receipts
- Deterministic graph traversal

**Commands**:
```bash
# Wrap analytics
const { result, receipt } = await analytics.pageRank(graph);
```

---

## Migration Validation

After completing each package migration:

### 1. Run Tests

```bash
cd /home/user/unrdf/packages/<package-name>
timeout 10s pnpm test
```

**Expected**: 100% test pass rate

### 2. Check OTEL Validation

```bash
cd /home/user/unrdf
node validation/run-all.mjs comprehensive
grep "Score:" validation-output.log
```

**Expected**: Score ≥ 80/100

### 3. Verify No N3 Imports

```bash
timeout 5s grep -r "from 'n3'" packages/<package-name>/src --include="*.mjs"
```

**Expected**: 0 results

### 4. Check Receipt Generation

```bash
# Run sample operation
node -e "
  import('./packages/<package-name>/src/index.mjs').then(async (mod) => {
    const { result, receipt } = await mod.someOperation();
    console.log('Receipt:', receipt);
  });
"
```

**Expected**: Receipt with all required fields

---

## Completion Criteria

All P1 packages must meet:

- [ ] L3+ maturity level
- [ ] 100% test pass rate
- [ ] OTEL score ≥ 80/100
- [ ] Zero N3 imports (outside justified modules)
- [ ] All operations generate receipts
- [ ] Zod schemas validated
- [ ] Deterministic outputs proven (100x test)

---

## References

- **Pattern Library**: [/docs/v6/PATTERNS.md](/home/user/unrdf/docs/v6/PATTERNS.md)
- **Tutorials**: [/docs/v6/PATTERN_TUTORIALS.md](/home/user/unrdf/docs/v6/PATTERN_TUTORIALS.md)
- **Migration Plan**: [/docs/v6/MIGRATION_PLAN.md](/home/user/unrdf/docs/v6/MIGRATION_PLAN.md)
- **Maturity Ladder**: [/docs/v6/MATURITY_LADDER.md](/home/user/unrdf/docs/v6/MATURITY_LADDER.md)

---

**Runbooks Version**: 1.0.0
**Last Updated**: 2025-12-27
**Researcher**: Claude Code (Researcher Agent)
