# UNRDF v6 Core Concepts

**Target Audience**: Developers wanting deep understanding of v6 architecture
**Prerequisites**: Familiarity with RDF and knowledge graphs
**Time**: 20-30 minutes

This guide explains the fundamental concepts and architectural decisions behind UNRDF v6.

---

## Table of Contents

1. [Receipt-Driven Operations](#1-receipt-driven-operations)
2. [Delta-Based Versioning](#2-delta-based-versioning)
3. [Pure ESM Architecture](#3-pure-esm-architecture)
4. [Zod-First Validation](#4-zod-first-validation)
5. [Maturity Ladder](#5-maturity-ladder)
6. [Oxigraph Backend](#6-oxigraph-backend)
7. [Deterministic Execution](#7-deterministic-execution)

---

## 1. Receipt-Driven Operations

### What Are Receipts?

**Receipts** are cryptographic proofs that an operation was executed with specific inputs at a specific time.

### Structure

```typescript
interface Receipt {
  id: string;              // Unique receipt ID (UUIDv4)
  operation: string;       // Operation name
  timestamp: string;       // ISO 8601 timestamp
  merkleRoot: string;      // Merkle tree root hash
  proof: string[];         // Merkle proof path
  metadata: object;        // Operation-specific data
}
```

### Why Receipts?

**Problem**: How do you prove an operation happened exactly as claimed?

**v5 Approach**:
```javascript
await workflow.run(task);
// No proof. Did it run? What inputs? When?
```

**v6 Approach**:
```javascript
const receipt = await workflow.execute(task);
// Cryptographic proof:
// - Operation: 'workflow-execute'
// - Inputs: hash of task
// - Timestamp: nanosecond precision
// - Merkle proof: tamper-evident chain
```

### Benefits

1. **Deterministic Replay**: Reproduce exact execution from receipts
2. **Audit Trails**: Compliance and security requirements
3. **Adversarial Validation**: Prove correctness to external parties
4. **Time Travel**: Reconstruct state at any point

### Merkle Trees

Receipts use Merkle trees for tamper-evidence:

```
         Root
        /    \
      H1      H2
     /  \    /  \
   op1  op2 op3 op4

Root = H(H1 + H2)
H1 = H(op1 + op2)
```

**Property**: Changing any operation invalidates the root hash.

**Example**:
```javascript
import { MerkleTree } from '@unrdf/v6-core/receipts';

const operations = ['add-triple', 'delete-triple', 'query'];
const tree = new MerkleTree(operations);

console.log('Root:', tree.root); // sha256:abc123...

// Tamper detection
operations[1] = 'malicious-op';
const newTree = new MerkleTree(operations);
console.log('New root:', newTree.root); // sha256:xyz789... (different!)
```

### Receipt Chains

Link receipts to form tamper-evident chains:

```javascript
const receipt1 = createReceipt('op1', { data: 'a' });
const receipt2 = createReceipt('op2', {
  data: 'b',
  previousReceipt: receipt1.id // Links to receipt1
});
const receipt3 = createReceipt('op3', {
  data: 'c',
  previousReceipt: receipt2.id // Links to receipt2
});

// Verify chain integrity
const chainValid = verifyChain([receipt1, receipt2, receipt3]);
```

**Use Cases**:
- Blockchain-like audit trails (without blockchain overhead)
- Regulatory compliance (GDPR, HIPAA, SOX)
- Reproducible research (scientific workflows)
- Adversarial environments (zero-trust systems)

---

## 2. Delta-Based Versioning

### What Are Deltas?

**Deltas** are explicit descriptions of changes between two graph versions.

### Structure

```typescript
interface DeltaProposal {
  id: string;              // Delta ID
  from: string;            // Source version
  to: string;              // Target version
  operations: Operation[]; // List of changes
  timestamp: string;       // When created
}

interface Operation {
  type: 'add' | 'remove';
  quad: {
    subject: string;
    predicate: string;
    object: string;
    graph?: string;
  };
}
```

### Why Deltas?

**Problem**: How do you track and version knowledge graph changes?

**Git-like Approach**:
```bash
# Snapshot entire graph at each version
v1.0: 1,000,000 triples
v1.1: 1,000,100 triples (added 100)
# Storage: 2M triples
```

**Delta Approach**:
```bash
# Store base + deltas
v1.0: 1,000,000 triples (base)
v1.0→v1.1: 100 add operations (delta)
# Storage: 1M + 100 = ~1M triples
```

**Savings**: 99.99% for incremental changes

### Benefits

1. **Explicit Changes**: Know exactly what changed
2. **Conflict Detection**: Identify concurrent edits
3. **Rollback**: Undo changes by reversing delta
4. **Storage Efficiency**: Store deltas, not full snapshots
5. **Collaboration**: Merge deltas from multiple sources

### Example

```javascript
import { createDeltaProposal, applyDelta } from '@unrdf/v6-core/delta';

// v1.0 → v1.1: Update Alice's email
const delta = createDeltaProposal('v1.0', 'v1.1', [
  {
    type: 'remove',
    quad: {
      subject: 'http://example.org/Alice',
      predicate: 'http://xmlns.com/foaf/0.1/mbox',
      object: 'alice@old.org'
    }
  },
  {
    type: 'add',
    quad: {
      subject: 'http://example.org/Alice',
      predicate: 'http://xmlns.com/foaf/0.1/mbox',
      object: 'alice@new.org'
    }
  }
]);

// Apply with receipt
const receipt = await applyDelta(store, delta);
```

### Conflict Resolution

When two deltas edit the same triple:

```javascript
import { detectConflicts } from '@unrdf/v6-core/delta';

const deltaA = createDeltaProposal('v1.0', 'v1.1-alice', [
  { type: 'add', quad: { subject: 'ex:Alice', predicate: 'ex:age', object: '30' } }
]);

const deltaB = createDeltaProposal('v1.0', 'v1.1-bob', [
  { type: 'add', quad: { subject: 'ex:Alice', predicate: 'ex:age', object: '31' } }
]);

const conflicts = detectConflicts([deltaA, deltaB]);
// conflicts = [{ property: 'ex:age', values: ['30', '31'] }]

// Resolve: Last write wins, merge, manual review, etc.
```

**Use Cases**:
- Collaborative knowledge editing (Wikipedia-like)
- Version control systems
- Distributed databases (CRDTs)
- Incremental backups

---

## 3. Pure ESM Architecture

### What Is ESM?

**ESM** (EcmaScript Modules) is the standard module system for JavaScript.

### Why Pure ESM?

**v5 Dual Mode** (ESM + CommonJS):
```json
{
  "main": "dist/index.js",      // CJS
  "module": "dist/index.mjs",   // ESM
  "exports": {
    "require": "./dist/index.js",
    "import": "./dist/index.mjs"
  }
}
```

**Problems**:
- ❌ Doubled bundle size (2 builds)
- ❌ Dual runtime semantics (bugs)
- ❌ Complex build tooling

**v6 Pure ESM**:
```json
{
  "type": "module",
  "main": "src/index.mjs",
  "exports": {
    ".": "./src/index.mjs"
  }
}
```

**Benefits**:
- ✅ 40% smaller bundles (one build)
- ✅ Consistent semantics
- ✅ Native tree-shaking
- ✅ Simpler tooling

### Migration

**v5 (CJS)**:
```javascript
const unrdf = require('@unrdf/core');
const { Store } = require('n3');
```

**v6 (ESM)**:
```javascript
import * as unrdf from '@unrdf/core';
import { createStore } from '@unrdf/oxigraph';
```

**Requirements**:
- Node.js 18+ (native ESM support)
- `"type": "module"` in package.json
- `.mjs` extensions or `"type": "module"`

---

## 4. Zod-First Validation

### What Is Zod?

**Zod** is a TypeScript-first schema validation library with runtime checks.

### Why Zod?

**Problem**: How do you guarantee runtime type safety in JavaScript?

**TypeScript Approach**:
```typescript
interface User {
  id: string;
  name: string;
}

function processUser(user: User) {
  // TypeScript validates at compile time
  // Runtime: user could be anything!
}

processUser({ id: 123, name: null }); // Compiles, crashes at runtime
```

**Zod Approach**:
```javascript
import { z } from 'zod';

const UserSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1)
});

function processUser(user) {
  const validated = UserSchema.parse(user); // Runtime validation
  // validated is guaranteed to match schema
}

processUser({ id: 123, name: null });
// Throws ZodError: id must be string
```

### Benefits

1. **Runtime Safety**: Catch invalid data at runtime
2. **Self-Documenting**: Schemas are executable documentation
3. **TypeScript Integration**: Infer types from schemas
4. **Deterministic**: Same input always validates the same way

### v6 Enforcement

All public APIs require Zod schemas:

```javascript
import { z } from 'zod';
import { createStore } from '@unrdf/oxigraph';

const AddTripleSchema = z.object({
  subject: z.string().url(),
  predicate: z.string().url(),
  object: z.string().min(1)
});

async function addTriple(data) {
  // Validate input (throws on invalid)
  const validated = AddTripleSchema.parse(data);

  const store = await createStore();
  // ... use validated data ...
}
```

### Schema Composition

```javascript
const PersonSchema = z.object({
  name: z.string(),
  age: z.number().int().positive()
});

const EmployeeSchema = PersonSchema.extend({
  employeeId: z.string().uuid(),
  department: z.enum(['Engineering', 'Sales', 'HR'])
});

// EmployeeSchema includes name, age, employeeId, department
```

**Use Cases**:
- API input validation
- Configuration validation
- Data migration validation
- External data ingestion

---

## 5. Maturity Ladder

### What Is the Maturity Ladder?

A **graduated quality framework** for package evolution (L1 → L5).

### Levels

| Level | Name | Criteria | Example |
|-------|------|----------|---------|
| **L1** | Baseline | Compiles, runs | All 47 packages |
| **L2** | Stable | Zod schemas, stable API | 12 packages |
| **L3** | Deterministic | No Date.now(), Math.random() | 5 packages |
| **L4** | Adversarial | OTEL validation ≥80/100 | 3 packages |
| **L5** | Composition | Works in all combinations | 0 packages (target) |

### Why?

**Problem**: How do you incrementally improve 47 packages without "big bang" releases?

**Traditional Approach**:
```bash
# All packages must be perfect for v6.0.0
# Result: Never ship
```

**Maturity Ladder Approach**:
```bash
# v6.0.0-alpha: Core packages at L5
# v6.0.0-beta: All packages at L3
# v6.0.0: All packages at L5
```

### L3: Deterministic Execution

**Non-Deterministic (L1/L2)**:
```javascript
const receipt = createReceipt('operation', {
  timestamp: Date.now(), // Non-deterministic!
  random: Math.random()   // Non-deterministic!
});

// Same operation produces different receipts
```

**Deterministic (L3)**:
```javascript
import { sha256 } from '@noble/hashes/sha256';

const receipt = createReceipt('operation', {
  timestamp: providedTimestamp, // From input, not system
  hash: sha256(input)           // Same input → same hash
});

// Same operation always produces same receipt
```

**Benefits**:
- Reproducible builds
- Testing (same input → same output)
- Debugging (replay exact conditions)

### L4: Adversarial Safety

**OTEL Validation ≥80/100**:

```bash
node validation/run-all.mjs comprehensive
```

Validates:
1. All operations produce receipts
2. Receipts are verifiable
3. No uncaught errors
4. Spans properly nested
5. Metrics collected

**Adversarial**: External party can verify correctness without trusting you.

---

## 6. Oxigraph Backend

### What Is Oxigraph?

**Oxigraph** is a Rust-based RDF triple store with WASM compilation.

### Why Oxigraph?

**N3.js (v5)**:
- JavaScript implementation
- ~2.5ms per SPARQL query
- 100MB memory for 1M triples

**Oxigraph (v6)**:
- Rust implementation (compiled to WASM)
- ~0.3ms per SPARQL query (8x faster)
- 40MB memory for 1M triples (60% less)

### Architecture

```
┌─────────────────┐
│   JavaScript    │
│   Application   │
└────────┬────────┘
         │
         ↓
┌─────────────────┐
│  WASM Bindings  │
│   (thin layer)  │
└────────┬────────┘
         │
         ↓
┌─────────────────┐
│  Oxigraph Core  │
│     (Rust)      │
└─────────────────┘
```

### Memory Backend

```javascript
import { createStore } from '@unrdf/oxigraph';

const store = await createStore({ backend: 'memory' });
// All data in RAM
// Fast, but volatile
```

### Persistent Backend

```javascript
const store = await createStore({
  backend: 'sqlite',
  path: '/path/to/data.db'
});
// Data persisted to SQLite
// Survives restarts
```

### Browser Support

```javascript
// WASM automatically loads in browser
const store = await createStore({ backend: 'memory' });
// Works in Chrome, Firefox, Safari, Edge
```

**Benefits**:
- 10x faster SPARQL execution
- 60% lower memory usage
- Browser support (WASM)
- Persistent storage (SQLite)

---

## 7. Deterministic Execution

### What Is Deterministic Execution?

**Deterministic**: Same inputs always produce same outputs.

### Why?

**Problem**: How do you debug non-reproducible bugs?

**Non-Deterministic**:
```javascript
const receipt = createReceipt('operation', {
  timestamp: Date.now(),    // Changes every call
  random: Math.random(),    // Different every time
  order: Object.keys(obj)   // Insertion order varies
});

// Same operation → different receipts
// Cannot replay from receipts
```

**Deterministic**:
```javascript
const receipt = createReceipt('operation', {
  timestamp: inputTimestamp, // From caller, not system
  hash: sha256(input),       // Same input → same hash
  order: sortedKeys(obj)     // Consistent ordering
});

// Same operation → same receipt
// Perfect replay from receipts
```

### Sources of Non-Determinism

1. **System Time**: `Date.now()`, `new Date()`
2. **Random**: `Math.random()`, `crypto.randomUUID()`
3. **Object Ordering**: `Object.keys()`, `for...in`
4. **Floating Point**: `0.1 + 0.2 === 0.30000000000000004`
5. **Async Timing**: Race conditions, network delays

### v6 Solutions

**Time**:
```javascript
// ❌ Non-deterministic
const now = Date.now();

// ✅ Deterministic
const now = providedTimestamp || Date.now(); // Caller controls
```

**Randomness**:
```javascript
// ❌ Non-deterministic
const id = crypto.randomUUID();

// ✅ Deterministic
const id = sha256(input).slice(0, 16); // Hash of input
```

**Ordering**:
```javascript
// ❌ Non-deterministic
const keys = Object.keys(obj);

// ✅ Deterministic
const keys = Object.keys(obj).sort();
```

### Benefits

1. **Reproducible Builds**: CI/CD produces identical artifacts
2. **Testing**: Replay exact scenarios
3. **Debugging**: Reproduce production bugs locally
4. **Audit**: Verify receipts offline

---

## Summary

UNRDF v6 introduces:

1. **Receipts**: Cryptographic proofs for all operations
2. **Deltas**: Explicit, auditable version transitions
3. **Pure ESM**: Modern JavaScript, smaller bundles
4. **Zod Validation**: Runtime type safety
5. **Maturity Ladder**: Graduated quality framework
6. **Oxigraph**: 10x faster SPARQL, 60% less memory
7. **Determinism**: Reproducible execution

These concepts work together to provide:

- ✅ **Auditability**: Prove what happened
- ✅ **Reproducibility**: Replay exact conditions
- ✅ **Performance**: Faster, smaller, more efficient
- ✅ **Safety**: Runtime guarantees
- ✅ **Collaboration**: Conflict detection and resolution

---

## Next Steps

- **[Quick Start Guide](/home/user/unrdf/docs/v6/QUICK-START.md)** - Get started with v6
- **[Advanced Patterns](/home/user/unrdf/docs/v6/ADVANCED-PATTERNS.md)** - Deep dive examples
- **[API Reference](/home/user/unrdf/docs/v6/API-REFERENCE.md)** - Complete API docs
- **[Migration Guide](/home/user/unrdf/docs/v6/MIGRATION_PLAN.md)** - Upgrade from v5

---

**Questions?** Open an issue on [GitHub](https://github.com/unrdf/unrdf/issues) 🚀
