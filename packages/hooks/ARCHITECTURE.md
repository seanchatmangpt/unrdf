# @unrdf/hooks Architecture

Deep dive into the design, data flows, and determinism guarantees of the Knowledge Hook Engine.

## Table of Contents

1. [Overview](#overview)
2. [6 Core Priorities](#6-core-priorities)
3. [9 Condition Kinds](#9-condition-kinds)
4. [Component Architecture](#component-architecture)
5. [Data Flows](#data-flows)
6. [Receipt Chaining](#receipt-chaining)
7. [Condition Evaluation Pipeline](#condition-evaluation-pipeline)
8. [Effect Execution Pipeline](#effect-execution-pipeline)
9. [Determinism Guarantees](#determinism-guarantees)
10. [Performance Characteristics](#performance-characteristics)

---

## Overview

The Knowledge Hook Engine implements a complete governance layer for RDF knowledge graphs. It's built on 6 core priorities and 9 condition kinds.

**Design Philosophy**:
- **RDF-Native**: SPARQL CONSTRUCT for effects (no JavaScript in production code)
- **Deterministic**: BLAKE3 hashing + receipt chaining enables immutable audit trails
- **Safe**: SHACL soft-fail modes (block/annotate/repair) for risk management
- **Expressive**: N3 rules + Datalog for complex inference without imperative code

---

## 6 Core Priorities

### 1. withReceipt Integration

Every hook execution produces a BLAKE3-hashed receipt with full chain linkage.

**Components**:
- `ReceiptGenerator` - BLAKE3 hashing of payloads, input/output state
- `ReceiptChain` - Maintains linked list of receipts with previousReceiptHash
- `DeltaTracker` - Records adds/deletes for audit trail

**Flow**:
```
Hook Execution
    ↓
Capture Input State (BLAKE3)
    ↓
Execute Effects
    ↓
Capture Output State (BLAKE3)
    ↓
Generate Receipt (link previous)
    ↓
Store in Receipt Chain
```

**Guarantee**: Immutable chain proves sequence of operations.

---

### 2. SPARQL CONSTRUCT Effects

RDF-native transformation via SPARQL (no JavaScript).

**Components**:
- `SparqlConstructExecutor` - Executes CONSTRUCT queries
- `QueryValidator` - Validates CONSTRUCT syntax
- `ResultProcessor` - Materializes results as quads

**Design Decision**: Pure RDF eliminates execution risk and simplifies determinism.

**Example**:
```sparql
CONSTRUCT {
  ?s ex:processed true ;
     ex:processedAt ?now ;
     ex:delta ?changeCount .
}
WHERE {
  ?s a ex:Trade .
  BIND (NOW() as ?now)
  BIND (1 as ?changeCount)
}
```

**Advantage**: Deterministic, composable, testable without side effects.

---

### 3. SHACL Enforcement Modes

Soft-fail governance with 3 modes:

**Components**:
- `ShaclValidator` - Shape validation engine
- `ViolationAnnotator` - Materializes violations as RDF triples
- `ViolationRepairer` - Auto-fixes via SPARQL CONSTRUCT

**Three Modes**:

| Mode | Behavior | Use Case |
|---|---|---|
| `block` | Fail if violations | Strict compliance, immutable audit |
| `annotate` | Execute + log violations | Risk management, warning systems |
| `repair` | Auto-fix + re-validate | Data quality, self-healing |

**Flow** (block mode):
```
Evaluate Shape
    ↓
Violations?
    ├─ No → Execute Effects
    ├─ Yes → Fail with Report
```

**Flow** (annotate mode):
```
Evaluate Shape
    ├─ No violations → Execute Effects
    └─ Violations → Store as RDF + Execute Effects
```

**Flow** (repair mode):
```
Evaluate Shape
    ├─ No violations → Execute Effects
    └─ Violations → Apply CONSTRUCT + Re-validate → Execute Effects
```

---

### 4. Input/Output Hash Receipts

Cryptographic proof via canonical hashing.

**Components**:
- `StateHasher` - BLAKE3(canonical RDF state)
- `DeltaCalculator` - Diff before/after states
- `ReceiptSigner` - BLAKE3(entire receipt)

**Receipt Structure**:
```javascript
{
  receiptHash: BLAKE3(entire receipt),
  payloadHash: BLAKE3(hook definitions),
  input_hash: BLAKE3(store before),
  output_hash: BLAKE3(store after),
  previousReceiptHash: prior receipt's receiptHash,
  delta: { adds: [...], deletes: [...] }
}
```

**Invariant**: If `input_hash(op N+1) == output_hash(op N)`, chain is valid.

---

### 5. N3 Forward-Chaining Rules

Declarative inference via EYE reasoner.

**Components**:
- `N3RuleParser` - Parses Notation3 rules
- `EYEBridge` - Calls EYE reasoning engine
- `RuleResultMaterializer` - Stores inferred triples in store

**Rule Format**:
```
{ ANTECEDENT } => { CONSEQUENT } .
```

**Example**:
```
{ ?x a :HighRisk ; :exposure ?exp } => { ?x :requiresReview true } .
```

**Flow**:
```
Parse Rules
    ↓
Add Store Triples to EYE
    ↓
Run EYE Reasoning
    ↓
Capture Inferred Triples
    ↓
Store in RDF Store
```

**Property**: Forward-chaining (rules fire until fixpoint reached).

---

### 6. Datalog Logic Programming

Constraint evaluation via bottom-up fixpoint.

**Components**:
- `DatalogParser` - Parses facts and rules
- `DatalogEngine` - Bottom-up evaluation with memoization
- `GoalProver` - Proves goal or returns failure

**Syntax**:
```
Facts:  predicate(arg1, arg2)
Rules:  head(X, Y) :- body1(X), body2(Y)
Goal:   predicate(value, Variable)
```

**Example**:
```
Facts:
  admin(alice)
  member(alice, admins)
  
Rules:
  allowed(X) :- admin(X)
  allowed(X) :- member(X, admins)
  
Goal:
  allowed(alice)
```

**Flow**:
```
Add Facts to Database
    ↓
Apply Rules Repeatedly Until Fixpoint
    ↓
Query Goal Against Database
    ↓
Return True/False
```

**Property**: Decidable via bottom-up evaluation with tabling.

---

## 9 Condition Kinds

### Matrix View

| Kind | Complexity | Input | Output | Use Case |
|---|---|---|---|---|
| sparql-ask | Low | Query | Boolean | Quick checks |
| sparql-select | Low | Query | Boolean | Binding checks |
| shacl | Medium | Shape + Data | Boolean + Violations | Validation |
| delta | Low | Pattern | Boolean | Change detection |
| threshold | Low | Query + Value | Boolean | Quantitative limits |
| count | Low | Pattern + Count | Boolean | Cardinality |
| window | Medium | Query + Time | Boolean | Time-based |
| n3 | High | Rules + Data | Boolean + Inferred | Inference |
| datalog | High | Facts + Rules + Goal | Boolean | Logic |

### Evaluation Order

1. **Fast-Path Conditions** (sparql-ask, delta, count)
   - <5ms typically
   - No complex computation
   - Run first in pipeline

2. **Medium-Path Conditions** (sparql-select, threshold, window, shacl)
   - 5-20ms typically
   - Some computation
   - Run after fast-path

3. **Slow-Path Conditions** (n3, datalog)
   - 10-100ms typically
   - Complex inference/logic
   - Run last, optional caching

---

## Component Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ KnowledgeHookEngine (Main Orchestrator)                     │
│                                                              │
│ - Manages hook registry                                      │
│ - Coordinates condition evaluation                           │
│ - Sequences effect execution                                 │
│ - Manages receipt chain                                      │
└─────────────────────────────────────────────────────────────┘
              ↓                    ↓                    ↓
    ┌─────────────────┐  ┌────────────────┐  ┌──────────────┐
    │ Condition       │  │ Effect         │  │ Receipt      │
    │ Evaluator       │  │ Executor       │  │ Manager      │
    │                 │  │                │  │              │
    │ 9 condition     │  │ SPARQL/Function│  │ BLAKE3       │
    │ kind evaluators │  │ execution      │  │ chaining     │
    └─────────────────┘  └────────────────┘  └──────────────┘
              ↓                    ↓                    ↓
    ┌──────────────────────────────────────────────────────┐
    │ @unrdf/oxigraph - RDF Store + SPARQL Engine          │
    └──────────────────────────────────────────────────────┘
              ↓
    ┌──────────────────────────────────────────────────────┐
    │ v6-core - Receipt Generation + Determinism           │
    └──────────────────────────────────────────────────────┘
```

### Core Classes

#### KnowledgeHookEngine

```javascript
class KnowledgeHookEngine {
  constructor(store) {}
  
  registerHook(hook): string
  evaluateCondition(condition): Promise<boolean>
  execute(context, hooks): Promise<ExecutionResult>
  getReceiptChain(): Receipt[]
}
```

**Responsibilities**:
1. Hook registration (Zod validation)
2. Condition evaluation dispatch
3. Effect execution sequencing
4. Receipt generation and chaining

---

#### ConditionEvaluator

```javascript
class ConditionEvaluator {
  async evaluate(condition, store): Promise<boolean>
  
  // 9 handlers:
  async evaluateSparqlAsk(...)
  async evaluateSparqlSelect(...)
  async evaluateShacl(...)
  async evaluateDelta(...)
  async evaluateThreshold(...)
  async evaluateCount(...)
  async evaluateWindow(...)
  async evaluateN3(...)
  async evaluateDatalog(...)
}
```

**Pattern**: Strategy pattern for pluggable condition kinds.

---

#### EffectExecutor

```javascript
class EffectExecutor {
  async execute(effect, store): Promise<ExecutionResult>
  
  // 2 handlers:
  async executeSparqlConstruct(...)
  async executeFunction(...)
}
```

---

#### ReceiptManager

```javascript
class ReceiptManager {
  createReceipt(
    context,
    payloadHash,
    inputHash,
    outputHash,
    delta
  ): Receipt
  
  chainReceipt(receipt, previousHash): Receipt
  
  getChain(): Receipt[]
}
```

---

## Data Flows

### Complete Execution Flow

```
User Input (Hook + Context)
    ↓
┌─────────────────────────────┐
│ Validation Phase            │
│ - Zod schema validation     │
│ - Condition reference check │
│ - Effect syntax validation  │
└─────────────────────────────┘
    ↓
┌─────────────────────────────┐
│ Receipt Phase (Start)       │
│ - Capture store state       │
│ - Calculate input_hash      │
└─────────────────────────────┘
    ↓
┌─────────────────────────────┐
│ Condition Evaluation        │
│ for each condition:         │
│  - Dispatch to handler      │
│  - Cache result if enabled  │
│  - Return boolean           │
└─────────────────────────────┘
    ↓
    ├─ All true? → Continue
    └─ Any false? → Short-circuit
    
    ↓ (All conditions passed)
    
┌─────────────────────────────┐
│ Effect Execution            │
│ for each effect:            │
│  - Execute transformation   │
│  - Materialize results      │
│  - Update store             │
└─────────────────────────────┘
    ↓
┌─────────────────────────────┐
│ Receipt Phase (End)         │
│ - Capture new store state   │
│ - Calculate output_hash     │
│ - Calculate delta           │
│ - Generate receiptHash      │
│ - Link to previousReceipt    │
│ - Store in chain            │
└─────────────────────────────┘
    ↓
Return ExecutionResult {
  successful,
  failed,
  receipt,
  violations,
  errors
}
```

### Condition Evaluation Flow (Detail)

```
Input: Condition object
    ↓
┌──────────────────────┐
│ Determine kind       │
│ (9 possible values)  │
└──────────────────────┘
    ↓
    ├─ sparql-ask ──→ ┌──────────────────────┐
    │                 │ Extract query        │
    │                 │ Execute on store     │
    │                 │ Return boolean       │
    │                 └──────────────────────┘
    │
    ├─ sparql-select → ┌──────────────────────┐
    │                 │ Extract query        │
    │                 │ Execute on store     │
    │                 │ Return true if rows  │
    │                 └──────────────────────┘
    │
    ├─ shacl ────────→ ┌──────────────────────┐
    │                 │ Load shape (ref)     │
    │                 │ Validate store       │
    │                 │ Return conforms bool │
    │                 │ Track violations     │
    │                 └──────────────────────┘
    │
    ├─ delta ────────→ ┌──────────────────────┐
    │                 │ Match add/del pats   │
    │                 │ Return boolean       │
    │                 └──────────────────────┘
    │
    ├─ threshold ───→ ┌──────────────────────┐
    │                 │ Execute query        │
    │                 │ Bind numeric result  │
    │                 │ Compare to threshold │
    │                 │ Return boolean       │
    │                 └──────────────────────┘
    │
    ├─ count ───────→ ┌──────────────────────┐
    │                 │ Count pattern match  │
    │                 │ Compare to expected  │
    │                 │ Return boolean       │
    │                 └──────────────────────┘
    │
    ├─ window ──────→ ┌──────────────────────┐
    │                 │ Query for timestamps │
    │                 │ Filter by window     │
    │                 │ Check against limit  │
    │                 │ Return boolean       │
    │                 └──────────────────────┘
    │
    ├─ n3 ──────────→ ┌──────────────────────┐
    │                 │ Parse N3 rules       │
    │                 │ Add store to EYE     │
    │                 │ Run reasoning        │
    │                 │ Execute ask query    │
    │                 │ Return boolean       │
    │                 └──────────────────────┘
    │
    └─ datalog ─────→ ┌──────────────────────┐
                      │ Parse facts/rules    │
                      │ Build KB             │
                      │ Prove goal           │
                      │ Return boolean       │
                      └──────────────────────┘
    ↓
Output: boolean (condition passed/failed)
```

### Effect Execution Flow (Detail)

```
Input: Effect object
    ↓
┌──────────────────────┐
│ Determine kind       │
│ (2 possible values)  │
└──────────────────────┘
    ↓
    ├─ sparql-construct ────→ ┌──────────────────────┐
    │                         │ Extract query        │
    │                         │ Execute CONSTRUCT    │
    │                         │ Materialize results  │
    │                         │ Insert quads to store│
    │                         │ Return success       │
    │                         └──────────────────────┘
    │
    └─ function ────────────→ ┌──────────────────────┐
                              │ Extract function     │
                              │ Execute with timeout │
                              │ Retry on failure     │
                              │ Sandbox if enabled   │
                              │ Return result        │
                              └──────────────────────┘
    ↓
Output: ExecutionResult { success, modified, errors }
```

---

## Receipt Chaining

### Chain Structure

```
Operation 1
├─ receiptHash: a3f7d9e2c8f4b6a1e9c7d5f3a1b9e8c6
├─ previousReceiptHash: null (first)
├─ input_hash: <state before op 1>
├─ output_hash: <state after op 1>
└─ delta: <changes by op 1>

Operation 2
├─ receiptHash: b9e2c4d1f7a3e8c9b5d3f1a7e9c6b4a2
├─ previousReceiptHash: a3f7d9e2c8f4b6a1e9c7d5f3a1b9e8c6 ← Links to op 1
├─ input_hash: <state after op 1> ← Matches op 1's output_hash
├─ output_hash: <state after op 2>
└─ delta: <changes by op 2>

Operation 3
├─ receiptHash: c4d1e7f3a9b5c1d7e3f9a5b1c7d3e9f5
├─ previousReceiptHash: b9e2c4d1f7a3e8c9b5d3f1a7e9c6b4a2 ← Links to op 2
├─ input_hash: <state after op 2> ← Matches op 2's output_hash
├─ output_hash: <state after op 3>
└─ delta: <changes by op 3>
```

### Verification Algorithm

```javascript
function verifyChain(chain) {
  for (let i = 1; i < chain.length; i++) {
    const prev = chain[i - 1];
    const curr = chain[i];
    
    // Verify linkage
    if (curr.previousReceiptHash !== prev.receiptHash) {
      return false; // Chain broken
    }
    
    // Verify state continuity
    if (curr.input_hash !== prev.output_hash) {
      return false; // State mismatch
    }
  }
  return true; // Chain valid
}
```

### Invariants

1. **Immutability**: receiptHash never changes (BLAKE3)
2. **Linkage**: Each receipt points to prior receipt
3. **Continuity**: input_hash(N) = output_hash(N-1)
4. **Atomicity**: Delta represents atomic changes

---

## Condition Evaluation Pipeline

### Pipeline Stages

```
Stage 1: Parse Condition
├─ Validate kind field
├─ Extract parameters
└─ Validate schema

Stage 2: Prepare Context
├─ Load external references (shapes, rules)
├─ Initialize state (facts, KB)
└─ Bind variables

Stage 3: Evaluate
├─ Execute kind-specific handler
├─ Handle errors with retries
└─ Track timing

Stage 4: Post-Process
├─ Cache result (optional)
├─ Track violations (SHACL)
├─ Materialize inferences (N3, Datalog)
└─ Return boolean

```

### Caching Strategy

```
Condition Hash = BLAKE3(condition object)
└─ Lookup in ConditionCache
   ├─ Hit → Return cached result
   └─ Miss → Evaluate + Store result
```

**TTL Options**:
- `0` - No caching
- `N > 0` - Cache for N seconds

---

## Effect Execution Pipeline

### Pipeline Stages

```
Stage 1: Parse Effect
├─ Validate kind field
├─ Extract query/function
└─ Validate schema

Stage 2: Prepare Execution
├─ Compile SPARQL (if CONSTRUCT)
├─ Sandbox environment (if Function)
└─ Set timeout/retries

Stage 3: Execute
├─ Run transformation
├─ Handle errors with retries
├─ Track timing + delta
└─ Update store state

Stage 4: Post-Process
├─ Materialize results
├─ Calculate new output_hash
├─ Update delta tracker
└─ Return success/failure
```

### Execution Safety

**SPARQL CONSTRUCT**:
- ✅ No side effects
- ✅ Deterministic
- ✅ Rollback-safe (append-only)

**Function** (Legacy):
- ⚠ Timeout protection (default 30s, max 5m)
- ⚠ Retry logic (default 1, max 5)
- ⚠ Sandbox isolation (optional)

---

## Determinism Guarantees

### Layer 1: Input Determinism

All inputs Zod-validated:
```
- Hook definition
- Condition specification  
- Effect specification
- Execution context
```

**Guarantee**: Invalid inputs rejected before execution.

---

### Layer 2: Condition Determinism

Each condition kind is deterministic:
- SPARQL ASK/SELECT: Same query on same data = same result
- SHACL: Same shape on same data = same violations
- Delta: Same patterns on same adds/deletes = same match
- Threshold: Same query + operator + value = same result
- Count: Same pattern on same data = same count
- Window: Same parameters on same timestamps = same result
- N3: Same rules on same facts = same inferred triples
- Datalog: Same facts/rules/goal = same provability

**Guarantee**: Condition evaluation is deterministic per kind.

---

### Layer 3: Effect Determinism

SPARQL CONSTRUCT is deterministic:
- Same CONSTRUCT query on same data = same output
- NOW() bound once at effect start (not per row)
- Variables bound from store, not from random source

Function effects are non-deterministic by design:
- Include in result metadata for transparency
- Allow optional sandboxing for isolation

**Guarantee**: SPARQL CONSTRUCT effects are deterministic.

---

### Layer 4: Receipt Determinism

BLAKE3 hashing is deterministic:
```
receiptHash = BLAKE3({
  timestamp,
  nodeId,
  payloadHash,
  input_hash,
  output_hash,
  delta,
  hooksExecuted,
  successful,
  failed
})
```

Same inputs → Same hash.

**Guarantee**: Receipt generation is deterministic.

---

### Layer 5: Chain Determinism

Receipt chain validates on every operation:
1. Calculate current state hash
2. Load previous receipt hash from context
3. Link current receipt to previous
4. Verify continuity invariant

**Guarantee**: Chain proves sequence of deterministic operations.

---

## Performance Characteristics

### Latency by Condition Kind

| Kind | P50 | P95 | P99 | Notes |
|---|---|---|---|---|
| sparql-ask | 1ms | 3ms | 5ms | In-memory store |
| sparql-select | 2ms | 5ms | 8ms | Adds binding overhead |
| delta | <1ms | <1ms | <1ms | Pattern matching |
| count | 1ms | 3ms | 5ms | Count aggregation |
| threshold | 2ms | 5ms | 8ms | Numeric comparison |
| window | 5ms | 15ms | 20ms | Temporal filtering |
| shacl | 5ms | 15ms | 25ms | Shape evaluation |
| n3 | 10ms | 50ms | 100ms | Rule inference |
| datalog | 1ms | 10ms | 30ms | Logic evaluation |

**Factors**:
- Store size (Oxigraph scales linearly)
- Query complexity (SPARQL parsing)
- Rule complexity (N3 inference time)
- Fact count (Datalog fixpoint iterations)

### Memory Characteristics

**Per-Hook Memory**:
- Hook definition: ~1KB
- Receipt: ~2KB
- Cache (if enabled): Variable

**Total Memory**:
- 1000 hooks: ~3MB
- Receipt chain (1000 receipts): ~2MB
- Condition cache (10K results): ~5MB

**Optimization**:
- ConditionCache with LRU eviction
- QuadPool for zero-allocation transforms
- Streaming results for large queries

---

## Scaling Properties

### Horizontal Scaling

Each node maintains independent receipt chains:
```
Node 1                    Node 2
├─ Receipt 1              ├─ Receipt A
├─ Receipt 2              ├─ Receipt B
└─ Receipt 3              └─ Receipt C
```

Cross-node verification via receipt hashes.

### Vertical Scaling

Store performance is primary bottleneck:
- Oxigraph: ~100K triples in memory
- Disk-backed: ~1M triples with indexing

Condition evaluation scales linearly with:
- Store size (SPARQL queries)
- Rule complexity (N3 inference)
- Fact count (Datalog fixpoint)

---

## Extensibility

### Adding New Condition Kind

```javascript
// 1. Define schema
const MyConditionSchema = z.object({
  kind: z.literal('my-condition'),
  param1: z.string(),
  param2: z.number()
});

// 2. Implement evaluator
class MyConditionEvaluator {
  async evaluate(condition, store) {
    // Custom logic
    return boolean;
  }
}

// 3. Register in ConditionEvaluator
ConditionEvaluator.register('my-condition', MyConditionEvaluator);
```

### Adding New Effect Kind

```javascript
// 1. Define schema
const MyEffectSchema = z.object({
  kind: z.literal('my-effect'),
  config: z.any()
});

// 2. Implement executor
class MyEffectExecutor {
  async execute(effect, store) {
    // Custom logic
    return { success: true, modified: N };
  }
}

// 3. Register in EffectExecutor
EffectExecutor.register('my-effect', MyEffectExecutor);
```

---

## Design Decisions

### Why SPARQL CONSTRUCT for Effects?

**Decision**: Use SPARQL CONSTRUCT exclusively for RDF effects.

**Rationale**:
1. **Determinism**: Same query on same data = same output
2. **Auditability**: Query is explicit, testable
3. **Safety**: No JavaScript execution risk
4. **Composability**: Can chain multiple CONSTRUCT operations
5. **RDF-Native**: Respects RDF semantics, no impedance mismatch

**Alternative Considered**: JavaScript functions
- ❌ Non-deterministic (side effects possible)
- ❌ Harder to audit
- ❌ Execution risk
- ❌ Sandboxing overhead

### Why 3 SHACL Enforcement Modes?

**Decision**: Support block/annotate/repair modes.

**Rationale**:
1. **Block**: Strict governance when immutability critical
2. **Annotate**: Risk management when soft-fail needed
3. **Repair**: Self-healing when correction possible

**Real-world Use Cases**:
- Financial compliance: block (regulatory requirement)
- Risk scoring: annotate (track violations without blocking)
- Data quality: repair (auto-fix known patterns)

### Why Receipt Chaining?

**Decision**: Every operation generates BLAKE3-hashed receipt linked to previous.

**Rationale**:
1. **Immutability Proof**: Hash never changes
2. **Sequence Proof**: previousReceiptHash chains operations
3. **State Proof**: input_hash + output_hash validate state continuity
4. **Audit Trail**: Delta proves what changed
5. **Cryptographic**: BLAKE3 is cryptographically secure

**Alternative Considered**: Append-only log
- ❌ Requires log durability
- ❌ Harder to verify offline
- ❌ Not self-contained

---

## Testing Strategy

### Unit Tests by Component

```
test/
├─ condition-evaluator.test.mjs     (9 kinds)
├─ effect-executor.test.mjs         (2 kinds)
├─ receipt-manager.test.mjs         (chaining)
├─ shacl-validator.test.mjs         (3 modes)
├─ n3-evaluator.test.mjs
├─ datalog-engine.test.mjs
└─ integration.test.mjs
```

### Integration Tests

```javascript
// Full execution pipeline
const result = await engine.execute(ctx, hooks);

// Assertions:
assert(result.successful >= 0);          // Counter valid
assert(result.receipt.receiptHash);      // Receipt generated
assert(verifyChain(engine.getReceiptChain())); // Chain valid
assert(validateDelta(result.receipt.delta)); // Delta valid
```

---

## Monitoring & Observability

### Telemetry Points

```javascript
// Condition evaluation
otel.span('condition.evaluate', {
  kind: condition.kind,
  duration_ms: duration,
  result: boolean,
  cached: wasCached
});

// Effect execution
otel.span('effect.execute', {
  kind: effect.kind,
  duration_ms: duration,
  modified: countModified,
  success: boolean
});

// Receipt generation
otel.span('receipt.generate', {
  hash: receiptHash,
  deltaSize: delta.adds.length + delta.deletes.length,
  chainLength: getReceiptChain().length
});
```

---

See [DEPLOYMENT.md](./DEPLOYMENT.md) for production monitoring setup.
