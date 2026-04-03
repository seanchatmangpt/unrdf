# @unrdf/hooks - Knowledge Hook Engine

![Version](https://img.shields.io/badge/version-6.0.0-blue) ![Production Ready](https://img.shields.io/badge/production-ready-green)

> Production-grade policy definition and execution framework for RDF knowledge graphs
> 
> Implements 9 condition kinds, deterministic receipt chaining, SPARQL transformations, SHACL enforcement, N3 forward-chaining inference, and Datalog logic programming

## Overview

The hooks package provides a complete governance layer for RDF operations with 6 core priorities:

1. **withReceipt Integration** - BLAKE3 cryptographic audit trails with receipt chaining
2. **SPARQL CONSTRUCT Effects** - RDF-native transformations (SPARQL-native, no JavaScript)
3. **SHACL Enforcement Modes** - Three modes (block/annotate/repair) for soft-fail governance
4. **Input/Output Hash Receipts** - State change proof via canonical hashing with delta tracking
5. **N3 Forward-Chaining Rules** - Inference engine via EYE reasoner with explicit rule definitions
6. **Datalog Logic Programming** - Constraint evaluation via bottom-up fixpoint with goal satisfaction

## Quick Start

### Installation

```bash
pnpm add @unrdf/hooks @unrdf/v6-core @unrdf/oxigraph
```

### Basic Hook Definition

```javascript
import { KnowledgeHookEngine, createKnowledgeHook } from '@unrdf/hooks';
import { createStore } from '@unrdf/oxigraph';
import { createContext } from '@unrdf/v6-core/receipt-pattern';

const store = createStore();
const engine = new KnowledgeHookEngine(store);
const context = createContext({
  nodeId: 'my-app',
  t_ns: BigInt(Date.now() * 1000000)
});

// Define a hook with SHACL condition (soft-fail annotation)
const hook = createKnowledgeHook({
  name: 'validate-compliance',
  condition: {
    kind: 'shacl',
    ref: { uri: 'file:///shapes/compliance.ttl' },
    enforcementMode: 'annotate'  // Log violations but don't block
  },
  effects: [{
    kind: 'sparql-construct',
    query: `
      CONSTRUCT {
        ?s ex:status ex:Valid ;
           ex:validatedAt ?now .
      }
      WHERE {
        ?s ?p ?o .
        BIND (NOW() as ?now)
      }
    `
  }]
});

// Execute with receipt chaining
const result = await engine.execute(context, [hook]);

// Access deterministic receipt chain
console.log('Receipt Hash:', result.receipt.receiptHash);        // BLAKE3(entire receipt)
console.log('Input Hash:', result.receipt.input_hash);          // BLAKE3(store before)
console.log('Output Hash:', result.receipt.output_hash);        // BLAKE3(store after)
console.log('Previous Hash:', result.receipt.previousReceiptHash); // Links to prior op
```

## 9 Condition Kinds

### 1. SPARQL ASK (Boolean Query)

Returns true if SPARQL ASK query matches any bindings.

```javascript
{
  kind: 'sparql-ask',
  query: 'ASK { ?s ?p ?o }'
}
```

**Use Case**: Quick boolean checks without result binding.

### 2. SPARQL SELECT (Result Set)

Returns true if SELECT query has results.

```javascript
{
  kind: 'sparql-select',
  query: 'SELECT ?s WHERE { ?s a ex:Person }'
}
```

**Use Case**: Condition depends on having matching results.

### 3. SHACL Validation (RDF Shape)

Validates store against SHACL shape. Three enforcement modes for soft-fail governance.

```javascript
{
  kind: 'shacl',
  ref: { uri: 'file:///shapes/person.ttl' },
  enforcementMode: 'block',  // or 'annotate', 'repair'
  repairConstruct: 'CONSTRUCT { ?s ex:status ex:Repaired } WHERE { ... }'
}
```

**Enforcement Modes**:
- `block`: Fail if violations exist (strict governance)
- `annotate`: Execute but add violations as RDF triples (soft-fail + audit trail)
- `repair`: Auto-fix via SPARQL CONSTRUCT then re-validate (self-healing)

### 4. Delta (Change Detection)

Detects RDF changes (add/delete patterns) for reactive governance.

```javascript
{
  kind: 'delta',
  adds: [{ subject: '?s', predicate: 'rdf:type', object: 'ex:Trade' }],
  deletes: []
}
```

**Use Case**: Trigger hooks only on specific RDF changes.

### 5. Threshold (Numeric Comparison)

Compares numeric values against threshold for quantitative governance.

```javascript
{
  kind: 'threshold',
  query: 'SELECT (COUNT(?s) as ?count) WHERE { ?s a ex:Trade }',
  operator: 'greaterThan',
  value: 100
}
```

**Operators**: `greaterThan`, `lessThan`, `equal`, `greaterThanOrEqual`, `lessThanOrEqual`

### 6. Count (Pattern Aggregation)

Counts matching patterns against expected value.

```javascript
{
  kind: 'count',
  pattern: { subject: '?s', predicate: 'ex:status', object: 'ex:Active' },
  expected: 5
}
```

**Use Case**: Enforce cardinality constraints.

### 7. Window (Time Range Evaluation)

Evaluates conditions within time windows for temporal governance.

```javascript
{
  kind: 'window',
  windowMs: 60000,
  maxMatches: 10,
  query: 'SELECT ?timestamp WHERE { ?s ex:timestamp ?timestamp }'
}
```

**Use Case**: Rate limiting, temporal constraints.

### 8. N3 Forward-Chaining Inference ⭐

Evaluates N3 rules via EYE reasoner for declarative inference.

```javascript
{
  kind: 'n3',
  rules: `
    { ?x a :RestrictedClass } => { ?x :requiresApproval true } .
    { ?x :riskScore ?score . ?score > 50 } => { ?x :requiresReview true } .
  `,
  askQuery: 'ASK { ?s :requiresApproval true }'
}
```

**Use Case**: Declarative rule application without imperative code.

### 9. Datalog Logic Programming ⭐

Evaluates Datalog goals via bottom-up fixpoint for constraint solving.

```javascript
{
  kind: 'datalog',
  facts: [
    'user(alice)',
    'admin(alice)',
    'group(admins)',
    'member(alice, admins)'
  ],
  rules: [
    'allowed(X) :- admin(X)',
    'allowed(X) :- member(X, admins)'
  ],
  goal: 'allowed(alice)'
}
```

**Use Case**: Access control, permission evaluation via logical rules.

## Effects (Transformations)

### SPARQL CONSTRUCT (RDF-Native)

RDF-native transformation via SPARQL. No JavaScript required.

```javascript
{
  kind: 'sparql-construct',
  query: `
    CONSTRUCT {
      ?s ex:processed true ;
         ex:processedAt ?now ;
         ex:delta ?deltaSize .
    }
    WHERE {
      ?s ?p ?o .
      BIND (NOW() as ?now)
      BIND (1 as ?deltaSize)
    }
  `
}
```

**Advantage**: Pure RDF transformation, deterministic, no execution risk.

### JavaScript Function (Legacy)

Execute custom logic. Included for backwards compatibility.

```javascript
{
  kind: 'function',
  inline: async (store, quad) => {
    // Custom transformation
    return { success: true };
  },
  timeout: 30000,
  sandbox: false
}
```

## SHACL Enforcement Modes (Priority 3)

### Block Mode (Default)

Prevents hook execution if SHACL validation fails. Strict governance.

```javascript
{ 
  kind: 'shacl', 
  ref: { uri: 'file:///shapes/strict.ttl' },
  enforcementMode: 'block' 
}
// Hook blocked if shape violations exist
// Result: Clean state or error
```

**Use Case**: Regulatory compliance, immutable audit trails.

### Annotate Mode (Soft-Fail)

Executes hook but adds SHACL violations as RDF triples. Audit trail with soft-fail.

```javascript
{
  kind: 'shacl',
  ref: { uri: 'file:///shapes/audit.ttl' },
  enforcementMode: 'annotate'
}
// Violations materialized as RDF quads
// Result: Store updated + violations tracked in store
```

**Use Case**: Logging, risk management, warning systems.

### Repair Mode (Self-Healing)

Auto-repairs violations via SPARQL CONSTRUCT, then re-validates.

```javascript
{
  kind: 'shacl',
  ref: { uri: 'file:///shapes/auto-fix.ttl' },
  enforcementMode: 'repair',
  repairConstruct: `
    CONSTRUCT {
      ?violation ex:repaired true .
      ?entity ex:status ex:Repaired .
    }
    WHERE { 
      ?violation a sh:ValidationResult . 
      ?violation sh:focusNode ?entity 
    }
  `
}
// Auto-fix violations, then re-validate
// Result: Clean state via automatic correction
```

**Use Case**: Data quality, auto-remediation, self-healing systems.

## Receipt Chaining (Priority 1 & 4)

Every hook execution produces a deterministic receipt with cryptographic proof:

```javascript
{
  receiptHash: 'a3f7...',              // BLAKE3(entire receipt)
  payloadHash: 'b9e2...',              // BLAKE3(payload/hook definitions)
  input_hash: 'c4d1...',               // BLAKE3(store state before)
  output_hash: 'e7f3...',              // BLAKE3(store state after)
  previousReceiptHash: '8f2a...',      // Links to prior operation
  timestamp: 1234567890000,
  nodeId: 'my-app',
  delta: {
    adds: [{ subject, predicate, object }],
    deletes: [{ subject, predicate, object }]
  },
  hooksExecuted: 3,
  successful: 2,
  failed: 0
}
```

**Verify Chain Integrity**:

```javascript
const receipt1 = await engine.execute(ctx1, hooks1);
const receipt2 = await engine.execute(
  createContext({
    ...ctx2,
    previousReceiptHash: receipt1.receipt.receiptHash
  }),
  hooks2
);

// receipt2.receipt.previousReceiptHash === receipt1.receipt.receiptHash ✅
// Proves immutable chain of operations
```

## CLI Integration

All 6 priorities exposed via `@unrdf/cli`:

```bash
# List all condition kinds and effects
unrdf hooks list-conditions

# Execute hooks with receipt chain
unrdf hooks execute \
  --store data.nq \
  --config hooks.json \
  --show-receipts \
  --output results.json

# Validate hook configuration
unrdf hooks define --config hooks.json --validate

# Test single condition
unrdf hooks evaluate-condition \
  --store data.nq \
  --condition shacl \
  --config '{"ref": {"uri": "file:///shapes/test.ttl"}, "enforcementMode": "block"}'

# Display receipt chain verification
unrdf hooks receipts --file results.json --verify

# Generate hooks from policy template
unrdf hooks template --type fibo --output hooks.json
```

## FIBO Case Study

Financial regulatory compliance using all 6 priorities.

See: `examples/fibo-jtbd-governance.mjs`

**5 Jobs-to-Be-Done**:

1. **Verify Regulatory Compliance** - SHACL validation + SPARQL construction
2. **Assess Counterparty Risk** - N3 forward-chaining inference
3. **Manage Liquidity Positions** - Datalog logic programming
4. **Maintain Audit Trail** - Receipt chaining with BLAKE3
5. **Auto-Repair Violations** - SHACL repair mode with CONSTRUCT

Run example:

```bash
node examples/fibo-jtbd-governance.mjs
```

## AtomVM Integration

Execute hooks from Erlang/BEAM processes via bridge.

See: `examples/atomvm-fibo-hooks-demo.mjs`

```javascript
import { HooksBridge } from '@unrdf/hooks/atomvm';

const bridge = new HooksBridge(store);

// From Erlang: register hook
const hookId = await bridge.registerHook({
  name: 'erlang-compliance',
  condition: { kind: 'shacl', ref: { uri: '...' } },
  effects: [{ kind: 'sparql-construct', query: '...' }]
});

// From Erlang: evaluate condition
const result = await bridge.evaluateCondition({
  kind: 'datalog',
  facts: ['user(alice)'],
  rules: ['allowed(X) :- user(X)'],
  goal: 'allowed(alice)'
});

// Full workflow with receipt
const receipt = await bridge.executeHooks(context, [hook]);
```

## Performance Characteristics

| Operation | Latency | Notes |
|-----------|---------|-------|
| Receipt Creation | <1ms | BLAKE3 hashing |
| SPARQL ASK | 1-5ms | Depends on query complexity |
| SPARQL SELECT | 2-8ms | Result binding overhead |
| SHACL Validation | 5-15ms | Shape size dependent |
| N3 Inference | 10-100ms | Rule complexity & triple count |
| Datalog Goal | 1-30ms | Fixpoint iterations |
| Full Hook Execution | <150ms | All conditions + effects + receipt |

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│ User Application / CLI (@unrdf/cli)                     │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│ KnowledgeHookEngine                                     │
│ ├─ Register hooks (6 priorities)                        │
│ ├─ Evaluate 9 condition kinds                           │
│ ├─ Execute effects (SPARQL/Function)                    │
│ ├─ SHACL enforcement (block/annotate/repair)           │
│ ├─ Track receipts (BLAKE3 chaining)                     │
│ └─ Manage deltas (adds/deletes)                         │
└─────────────────────────────────────────────────────────┘
         ↓                    ↓                    ↓
    ┌─────────┐        ┌───────────┐      ┌──────────────┐
    │ SPARQL  │        │ SHACL     │      │ Condition    │
    │ Engine  │        │ Validator │      │ Evaluator    │
    │(ASK/SEL)│        │ (3 modes) │      │(N3/Datalog)  │
    └─────────┘        └───────────┘      └──────────────┘
         ↓                    ↓                    ↓
         └────────────────────┬────────────────────┘
                              ↓
                   ┌──────────────────────┐
                   │ @unrdf/oxigraph      │
                   │ (RDF Store + SPARQL) │
                   └──────────────────────┘
                              ↓
                   ┌──────────────────────┐
                   │ v6-core Receipt      │
                   │ (BLAKE3 Hashing +    │
                   │  Deterministic Chain)│
                   └──────────────────────┘
```

## Documentation

- **[EXAMPLES.md](./EXAMPLES.md)** - All 9 condition kinds + effects + FIBO + AtomVM
- **[API-REFERENCE.md](./API-REFERENCE.md)** - Complete Zod schemas and API
- **[ARCHITECTURE.md](./ARCHITECTURE.md)** - Design decisions and data flows
- **[DEPLOYMENT.md](./DEPLOYMENT.md)** - Production checklist and monitoring

## Source Examples

Learn by example following the 80/20 principle:

- **[basic.mjs](./examples/basic.mjs)** - Minimal hook definition and execution (5 min)
- **[knowledge-hook-manager-usage.mjs](./examples/knowledge-hook-manager-usage.mjs)** - Manager API (10 min)
- **[validate-hooks.mjs](./examples/validate-hooks.mjs)** - Hook validation and debugging (15 min)

## Testing

```bash
# Full test suite
pnpm test

# Hooks-specific tests
pnpm test hooks.test.mjs

# Watch mode
pnpm test:watch

# Coverage
pnpm test --coverage

# Benchmarks
pnpm benchmark
```

## API Quick Reference

### KnowledgeHookEngine

```javascript
class KnowledgeHookEngine {
  // Register a hook with 6 priorities
  registerHook(hook: KnowledgeHook): string;
  
  // Evaluate any of 9 condition kinds
  async evaluateCondition(
    condition: Condition
  ): Promise<boolean>;
  
  // Execute hooks with receipt chaining
  async execute(
    context: ExecutionContext,
    hooks: KnowledgeHook[]
  ): Promise<ExecutionResult>;
  
  // Get receipt chain (BLAKE3 linked)
  getReceiptChain(): Receipt[];
}
```

### Hook Definition (Zod)

```javascript
const KnowledgeHook = {
  name: string;                    // Hook identifier
  condition: Condition;            // 9 kinds
  effects: Effect[];               // Transformations
  metadata?: Record<string, any>;
}
```

See [API-REFERENCE.md](./API-REFERENCE.md) for complete schema.

## Contributing

See [CONTRIBUTING.md](./docs/CONTRIBUTING.md) for development guidelines.

All code follows:
- 100% ESM (.mjs)
- JSDoc documentation
- Zod validation
- <500 lines per file
- 80%+ test coverage

## Key Files

- **src/hooks/knowledge-hook-engine.mjs** - Core executor (6 priorities)
- **src/hooks/condition-evaluator.mjs** - 9 condition kinds
- **src/hooks/validate.mjs** - SHACL validation (3 modes)
- **src/hooks/schemas.mjs** - Complete Zod schemas
- **test/comprehensive-hook-types.test.mjs** - Full test suite

## License

MIT
