# AtomVM + Hooks Integration Guide

**Version**: 26.4.3  
**Last Updated**: 2026-04-03  
**Target Audience**: UNRDF developers, knowledge graph engineers, FIBO ontology users

---

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [All 6 Hook Priorities](#all-6-hook-priorities)
4. [FIBO Integration](#fibo-integration)
5. [JTBDs (Jobs To Be Done)](#jtbds-jobs-to-be-done)
6. [Performance Characteristics](#performance-characteristics)
7. [Deployment Patterns](#deployment-patterns)
8. [Running the Demo](#running-the-demo)

---

## Overview

The AtomVM + Hooks integration bridges the **Erlang BEAM runtime** with UNRDF's **Knowledge Hooks Framework**, enabling:

- **Deterministic receipt generation** (BLAKE3 hashes) with chain integrity
- **Multi-priority hook execution** (all 6 priorities: receipt chain, SPARQL CONSTRUCT, SHACL, hash chain, N3 rules, Datalog)
- **FIBO ontology support** for financial knowledge graphs
- **JTBD workflows** (Jobs To Be Done) for complex business processes
- **Bi-directional communication** between Erlang/BEAM and JavaScript

### Key Benefits

| Aspect | Benefit |
|--------|---------|
| **Performance** | BEAM VM handles I/O, concurrency; hooks handle logic |
| **Reliability** | Erlang supervisor trees + receipt chain verification |
| **Scalability** | Distributed BEAM processes across multiple nodes |
| **Determinism** | BLAKE3 hashing ensures reproducible receipts |
| **Compliance** | SHACL + FIBO ontologies for regulatory requirements |

---

## Architecture

### System Layers

```
┌──────────────────────────────────────────────────────┐
│ Layer 5: APPLICATION                                 │
│ FIBO JTBDs, CLI, APIs                               │
├──────────────────────────────────────────────────────┤
│ Layer 4: HOOKS FRAMEWORK                             │
│ Priority 1-6: Receipt, CONSTRUCT, SHACL, Hash,      │
│              N3 Rules, Datalog                       │
├──────────────────────────────────────────────────────┤
│ Layer 3: ATOMVM HOOKS BRIDGE                         │
│ Erlang ↔ JavaScript Message Passing                 │
├──────────────────────────────────────────────────────┤
│ Layer 2: OXIGRAPH + SPARQL ENGINE                    │
│ RDF Store, Query Execution, CONSTRUCT/ASK           │
├──────────────────────────────────────────────────────┤
│ Layer 1: BEAM VM + ERLANG RUNTIME                    │
│ Processes, Supervision, Message Passing              │
└──────────────────────────────────────────────────────┘
```

### Component Interaction

```
User Application (e.g., FIBO compliance workflow)
        ↓
   FIBO JTBDs
        ↓
   Hooks Framework (Priority 1-6)
        ↓
   Hooks Bridge (AtomVM)
        ↓
   ┌─────────────────────┬──────────────────────┐
   ↓                     ↓                      ↓
BEAM/Erlang         Oxigraph Store        JS Evaluation
(Processes,      (RDF, SPARQL,        (N3 Rules,
 Supervision)     CONSTRUCT)           Datalog)
```

### Data Flow for Hook Execution

1. **User calls** `bridge.executeHooks(context)`
2. **Bridge serializes** hooks to Erlang message format
3. **Erlang process** receives message, validates schema
4. **Each hook evaluates** condition (SPARQL, SHACL, N3, Datalog)
5. **Effects execute** (CONSTRUCT, JS functions, receipts)
6. **Bridge receives** result with receipt hash
7. **Receipt chain** updates with BLAKE3 hash of previous receipt
8. **Return** to user with full execution trace

---

## All 6 Hook Priorities

### Priority 1: Receipt Genesis & Chaining

**Purpose**: Create deterministic receipts with BLAKE3 hashing and chain integrity.

**When to Use**:
- Genesis receipt on store initialization
- End of each major operation
- Audit trail generation

**Configuration**:

```yaml
hooks:
  - name: "receipt-chain-genesis"
    priority: 1
    condition:
      kind: "on-store-init"
    effects:
      - type: "receipt-create"
        hash_algorithm: "blake3"
        profile: "store-initialization"
```

**Example Code**:

```javascript
const payload = {
  operation: 'store-initialization',
  nodeId: 'node-1',
  timestamp: new Date().toISOString()
};

const payloadHash = await blake3Hash(canonicalize(payload));
const receiptHash = await blake3Hash({
  payload,
  previousHash: null,  // Genesis
  payloadHash
});

console.log('Genesis receipt:', receiptHash);
```

**Performance**: <1ms per receipt (BLAKE3 is extremely fast)

---

### Priority 2: SPARQL CONSTRUCT Effects

**Purpose**: Transform RDF graphs with SPARQL CONSTRUCT queries.

**When to Use**:
- Data enrichment (add inferred properties)
- Compliance marking (add checked timestamps)
- Risk classification (assign risk levels)

**Configuration**:

```yaml
hooks:
  - name: "fibo-compliance-mark"
    priority: 2
    condition:
      kind: "sparql-ask"
      query: |
        ASK {
          ?entity a fibo:LegalEntity .
          FILTER NOT EXISTS { ?entity fibo:checked true . }
        }
    effects:
      - type: "sparql-construct"
        query: |
          CONSTRUCT {
            ?entity fibo:checked true ;
                    fibo:checkedAt ?now .
          }
          WHERE {
            ?entity a fibo:LegalEntity .
            BIND (NOW() as ?now)
          }
```

**Example Code**:

```javascript
const constructResult = await store.executeConstruct(`
  CONSTRUCT {
    ?entity fibo:complianceStatus fibo:Compliant .
  }
  WHERE {
    ?entity a fibo:LegalEntity .
    ?entity fibo:creditRating fibo:AAA .
  }
`);
```

**Performance**: 10-50ms per query (Oxigraph SPARQL engine)

---

### Priority 3: SHACL Shape Validation

**Purpose**: Validate RDF graphs against SHACL shapes with enforcement modes.

**Enforcement Modes**:
- `annotate`: Add SHACL report to graph (non-blocking)
- `warn`: Log violations without blocking
- `enforce`: Block operations on violations

**Configuration**:

```yaml
hooks:
  - name: "fibo-legal-entity-shape"
    priority: 3
    condition:
      kind: "shacl"
      shape: "fibo:LegalEntityShape"
      enforcement_mode: "annotate"  # or "warn" or "enforce"
    effects:
      - type: "shacl-report"
        store_violations: true
```

**Example Code**:

```javascript
const shape = `
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix fibo: <http://purl.org/spec/fibo/ontology/> .
  
  fibo:LegalEntityShape
    a sh:NodeShape ;
    sh:targetClass fibo:LegalEntity ;
    sh:property [
      sh:path fibo:creditRating ;
      sh:required true ;
      sh:in (fibo:AAA fibo:AA fibo:A fibo:BBB)
    ] .
`;

const report = await shaclValidator.validate(shape, store);
```

**Performance**: 50-200ms per shape validation

---

### Priority 4: Hash Chain Integrity

**Purpose**: Verify receipt chain integrity and detect tampering.

**When to Use**:
- End of transaction (verify all receipts present)
- Before committing to storage
- Audit compliance checks

**Configuration**:

```yaml
hooks:
  - name: "receipt-chain-verify"
    priority: 4
    condition:
      kind: "receipt-verification"
      algorithm: "blake3"
      check_previous_hash: true
    effects:
      - type: "receipt-verify"
        require_chain_integrity: true
        enforce_determinism: true
```

**Example Code**:

```javascript
// Verify chain integrity
const chain = [receipt1, receipt2, receipt3];
let valid = true;
let previous = null;

for (const receipt of chain) {
  if (receipt.previousReceiptHash !== previous) {
    console.error('Chain broken at:', receipt.id);
    valid = false;
    break;
  }
  previous = receipt.receiptHash;
}

console.log('Chain valid:', valid);
```

**Performance**: O(n) where n = number of receipts in chain (~0.1ms per receipt)

---

### Priority 5: N3 Forward-Chaining Rules

**Purpose**: Execute N3 rules for complex inference on RDF graphs.

**When to Use**:
- Multi-step derivations
- Credit rating → Risk level mapping
- Complex business rules

**Configuration**:

```yaml
hooks:
  - name: "fibo-credit-to-risk"
    priority: 5
    condition:
      kind: "n3"
      rules: |
        @prefix fibo: <http://purl.org/spec/fibo/ontology/> .
        {
          ?cp fibo:creditRating ?rating .
          (?rating fibo:AAA fibo:AA) list:member ?rating .
        } => {
          ?cp fibo:riskLevel fibo:Low .
        } .
    effects:
      - type: "assert-inferred"
        add_to_store: true
```

**Example Code**:

```javascript
import { EyeReasoner } from '@unrdf/hooks';

const reasoner = new EyeReasoner();
const derivedFacts = await reasoner.apply(n3Rules, rdfGraph);

console.log('Derived facts:', derivedFacts.length);
```

**Performance**: 5-100ms depending on rule complexity

---

### Priority 6: Datalog Logic Programming

**Purpose**: Execute Datalog queries for constraint checking and goal solving.

**When to Use**:
- Account compliance checking (available >= threshold)
- Multi-condition queries
- Constraint satisfaction problems

**Configuration**:

```yaml
hooks:
  - name: "account-compliance"
    priority: 6
    condition:
      kind: "datalog"
      facts:
        - "account/1"
        - "available/2"
        - "threshold/2"
      rules:
        - "compliant(A) :- available(A, X), threshold(A, T), X >= T"
        - "risky(A) :- available(A, X), threshold(A, T), X < T"
      goal: "compliant(?Account)"
    effects:
      - type: "datalog-assert"
        assert_positive: true
```

**Example Code**:

```javascript
const facts = [
  'account(acc-001)',
  'available(acc-001, 1000000)',
  'threshold(acc-001, 100000)'
];

const rules = [
  'compliant(A) :- available(A, X), threshold(A, T), X >= T'
];

const goal = 'compliant(?Account)';
const results = await datalogEval.query(facts, rules, goal);

console.log('Compliant accounts:', results);
```

**Performance**: 1-10ms for simple queries, 50ms+ for complex constraint solving

---

## FIBO Integration

### FIBO Namespaces

```javascript
const FIBO = {
  // Core ontology
  LegalEntity: 'http://purl.org/spec/fibo/ontology/core/Parties/LegalEntity',
  
  // Credit/Risk
  creditRating: 'http://purl.org/spec/fibo/ontology/ext/creditRating',
  riskLevel: 'http://purl.org/spec/fibo/ontology/ext/riskLevel',
  
  // Compliance
  complianceStatus: 'http://purl.org/spec/fibo/ontology/ext/complianceStatus',
  checked: 'http://purl.org/spec/fibo/ontology/ext/checked',
  checkedAt: 'http://purl.org/spec/fibo/ontology/ext/checkedAt',
};
```

### FIBO Shapes (SHACL)

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix fibo: <http://purl.org/spec/fibo/ontology/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

fibo:LegalEntityShape
  a sh:NodeShape ;
  sh:targetClass fibo:LegalEntity ;
  
  # Credit rating is required and must be valid
  sh:property [
    sh:path fibo:creditRating ;
    sh:required true ;
    sh:in (fibo:AAA fibo:AA fibo:A fibo:BBB fibo:BB) ;
    sh:severity sh:Violation
  ] ;
  
  # Must be checked within 30 days
  sh:property [
    sh:path fibo:checkedAt ;
    sh:datatype xsd:dateTime ;
    sh:minInclusive "2026-03-04"^^xsd:dateTime ;
    sh:severity sh:Warning
  ] .
```

---

## JTBDs (Jobs To Be Done)

JTBDs represent high-level business processes that integrate multiple hooks and priorities.

### JTBD 1: Compliance Verification

**Goal**: Ensure entities comply with regulatory requirements.

**Hooks**:
1. Priority 2: Mark entity as checked (SPARQL CONSTRUCT)
2. Priority 3: Validate against SHACL shape
3. Priority 1: Create receipt for compliance verification
4. Priority 4: Verify receipt chain

**Workflow**:

```javascript
async function verifyCompliance(entity) {
  // 1. Mark as checked
  await executeConstructHook(entity);
  
  // 2. Validate SHACL
  const report = await validateSHACL(entity);
  if (report.conforms === false) {
    throw new Error('Compliance violation');
  }
  
  // 3. Create receipt
  const receipt = await createReceipt('compliance-verification', entity);
  
  // 4. Verify chain
  await verifyChain(receipt);
  
  return receipt;
}
```

### JTBD 2: Risk Assessment

**Goal**: Assess counterparty and transaction risk.

**Hooks**:
1. Priority 2: Assign risk levels (SPARQL CONSTRUCT)
2. Priority 5: Apply N3 rules for derived risk
3. Priority 1: Create risk assessment receipt

**Workflow**:

```javascript
async function assessRisk(counterparty) {
  // 1. Assign initial risk
  await executeConstructHook(counterparty);
  
  // 2. Apply inference rules
  const derivedRisk = await applyN3Rules(counterparty);
  
  // 3. Create receipt
  return await createReceipt('risk-assessment', {
    counterparty,
    derivedRisk
  });
}
```

### JTBD 3: Liquidity Check

**Goal**: Verify sufficient liquidity for settlement.

**Hooks**:
1. Priority 6: Check account compliance (Datalog)
2. Priority 2: Mark settlement status
3. Priority 1: Create settlement receipt

**Workflow**:

```javascript
async function checkLiquidity(account) {
  // 1. Check compliance
  const compliant = await checkDatalogGoal(account);
  if (!compliant) {
    throw new Error('Insufficient liquidity');
  }
  
  // 2. Mark settlement ready
  await executeConstructHook(account);
  
  // 3. Create receipt
  return await createReceipt('liquidity-check', account);
}
```

### JTBD 4: Audit Trail

**Goal**: Generate complete audit trail with receipt chain.

**Hooks**:
1. Priority 1: Create genesis receipt
2. Priority 4: Verify chain integrity after each operation
3. Priority 1: Create final audit receipt

**Workflow**:

```javascript
async function generateAuditTrail(caseId) {
  const chain = [];
  
  // 1. Genesis
  chain.push(await createGenesisReceipt(caseId));
  
  // 2. Verify after each operation
  for (const operation of operations) {
    const receipt = await executeOperation(operation);
    chain.push(receipt);
    
    await verifyChain(chain);
  }
  
  // 3. Final audit receipt
  chain.push(await createAuditReceipt(caseId, chain));
  
  return chain;
}
```

### JTBD 5: Repair & Recovery

**Goal**: Detect and repair invalid states in the graph.

**Hooks**:
1. Priority 3: Detect violations (SHACL)
2. Priority 2: Apply repair operations (CONSTRUCT)
3. Priority 1: Create repair receipt

**Workflow**:

```javascript
async function repairInvalidStates() {
  // 1. Detect violations
  const violations = await validateSHACL();
  
  // 2. Apply repairs
  for (const violation of violations) {
    await applyRepairConstructs(violation);
  }
  
  // 3. Create receipt
  return await createReceipt('repair-and-recovery');
}
```

---

## Performance Characteristics

### Latency by Priority (P95)

| Priority | Operation | P95 Latency | Notes |
|----------|-----------|-------------|-------|
| 1 | Receipt creation | <1ms | BLAKE3 hashing (no I/O) |
| 2 | SPARQL CONSTRUCT | 10-50ms | Oxigraph query execution |
| 3 | SHACL validation | 50-200ms | Shape complexity dependent |
| 4 | Hash chain verify | 0.1ms/receipt | Linear, no I/O |
| 5 | N3 rule inference | 5-100ms | Rule complexity dependent |
| 6 | Datalog evaluation | 1-50ms | Query complexity dependent |

### Throughput (ops/sec)

| Operation | Throughput |
|-----------|-----------|
| Receipt creation | ~10K/sec |
| SPARQL CONSTRUCT | ~20/sec (small graphs) |
| SHACL validation | ~5/sec |
| Hash chain verify | ~1M receipts/sec |
| N3 inference | ~100/sec |
| Datalog query | ~500-1K/sec |

### Memory Footprint

| Component | Memory (MB) |
|-----------|------------|
| BEAM VM (baseline) | 10-20 |
| Oxigraph store (100K triples) | 50-100 |
| Hooks engine | 5-10 |
| Receipt chain (1K receipts) | <1 |

### Scaling Characteristics

- **Horizontal**: BEAM processes scale across CPU cores (8-96 cores)
- **Vertical**: Single Oxigraph store supports ~100M triples
- **Receipt chain**: O(n) verification, sub-linear with batch verification
- **SPARQL queries**: Oxigraph indexing provides O(log n) lookups

---

## Deployment Patterns

### Development Environment

```bash
# Single-node, no persistence
node examples/atomvm-fibo-hooks-demo.mjs

# Watch for changes
pnpm --filter @unrdf/atomvm test:watch
```

### Staging Environment

```yaml
# 2-node cluster, persistence enabled
deployment:
  environment: "staging"
  nodes: 2
  persistence: "sqlite"
  replication_factor: 2
  receipt_verification: "strict"
```

### Production Environment

```yaml
# Multi-node cluster, high availability
deployment:
  environment: "production"
  nodes: 5
  persistence: "postgresql"
  replication_factor: 3
  receipt_verification: "strict"
  backup_frequency: "hourly"
  monitor_sla: true
  alert_thresholds:
    latency_p95_ms: 100
    chain_verification_errors: 0
```

### Docker Deployment

```dockerfile
FROM node:20-alpine

WORKDIR /app
COPY . .
RUN pnpm install --frozen-lockfile

EXPOSE 3000
CMD ["node", "examples/atomvm-fibo-hooks-demo.mjs"]
```

### Kubernetes Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: unrdf-atomvm-hooks
spec:
  replicas: 3
  selector:
    matchLabels:
      app: unrdf-atomvm-hooks
  template:
    metadata:
      labels:
        app: unrdf-atomvm-hooks
    spec:
      containers:
      - name: atomvm-hooks
        image: unrdf/atomvm-hooks:latest
        ports:
        - containerPort: 3000
        env:
        - name: NODE_ID
          valueFrom:
            fieldRef:
              fieldPath: metadata.name
        - name: PERSISTENCE
          value: "postgresql"
        resources:
          requests:
            memory: "256Mi"
            cpu: "250m"
          limits:
            memory: "1Gi"
            cpu: "1000m"
```

---

## Running the Demo

### Prerequisites

```bash
# Node.js 18+
node --version

# pnpm (required, not npm)
pnpm --version
```

### Quick Start

```bash
# Run demo with defaults
node examples/atomvm-fibo-hooks-demo.mjs

# Output:
# 🚀 AtomVM + FIBO Hooks Demo
# ===========================
# ✅ AtomVM initialized
# 📊 Demo 1: Receipt Chain with BLAKE3 Hashes
# ...
```

### With Configuration

```bash
# Run with custom config
export HOOKS_CONFIG=/path/to/atomvm-fibo-hooks-config.yaml
node examples/atomvm-fibo-hooks-demo.mjs
```

### Advanced: Enable OTEL Tracing

```bash
# Start OTEL collector (requires Docker)
docker run -d -p 6831:6831/udp jaegertracing/jaeger-collector:latest

# Run demo with tracing
OTEL_EXPORTER_JAEGER_AGENT_HOST=localhost \
OTEL_EXPORTER_JAEGER_AGENT_PORT=6831 \
OTEL_TRACES_EXPORTER=jaeger \
node examples/atomvm-fibo-hooks-demo.mjs

# View traces at http://localhost:16686
```

### Validation Output

Expected output shows all 6 priorities working:

```
📊 Demo 1: Receipt Chain with BLAKE3 Hashes
✅ Receipt 1 created
   Receipt Hash:  abc123def456...
   Payload Hash:  789ghi012jkl...
   Previous Hash: (genesis)

🔨 Demo 2: SPARQL CONSTRUCT Effects
✅ CONSTRUCT effects executed

🛡️  Demo 3: SHACL Enforcement Modes
✅ SHACL validation: annotate mode

📚 Demo 4: N3 Forward-Chaining Inference
✅ N3 inference evaluated

🧮 Demo 5: Datalog Logic Programming
✅ Datalog goal evaluated

🏦 Demo 6: Full FIBO JTBD Workflow
✅ JTBD 1 (Compliance): ...
✅ JTBD 2 (Risk): ...
... (all 5 JTBDs)

⛓️  Receipt Chain Integrity Verification
✅ Chain integrity: Valid
```

---

## Testing

### Unit Tests

```bash
# Test hooks framework
pnpm --filter @unrdf/hooks test

# Test atomvm package
pnpm --filter @unrdf/atomvm test

# Test integration
pnpm test:fast
```

### Integration Tests

```bash
# Run full demo
timeout 30s node examples/atomvm-fibo-hooks-demo.mjs

# Run with coverage
pnpm --filter @unrdf/atomvm test --coverage
```

### Performance Tests

```bash
# Benchmark receipt generation
pnpm benchmark

# Benchmark SPARQL queries
pnpm benchmark:sparql

# Benchmark Datalog evaluation
pnpm benchmark:datalog
```

---

## Troubleshooting

### Issue: "AtomVM initialization unavailable"

**Cause**: BEAM VM or Erlang not installed on system.

**Solution**: Demo falls back to JS-only execution (expected in development).

```bash
# To enable AtomVM, install Erlang
brew install erlang  # macOS
apt-get install erlang  # Linux
```

### Issue: Receipt hash mismatch

**Cause**: Non-deterministic data in payload.

**Solution**: Verify all timestamps use injected `t_ns`, no Date.now().

```javascript
// ❌ WRONG - Non-deterministic
const payload = { timestamp: new Date().toISOString() };

// ✅ CORRECT - Deterministic
const payload = { timestamp: context.timestamp_iso };
```

### Issue: SHACL validation timeout

**Cause**: Shape too complex or large graph.

**Solution**: Increase timeout in config.

```yaml
shacl:
  validation_timeout_ms: 10000  # Was 5000
```

---

## Further Reading

- [Hooks Framework Documentation](../packages/hooks/README.md)
- [FIBO Ontology Spec](https://spec.edmcouncil.org/fibo/)
- [SHACL Specification](https://www.w3.org/TR/shacl/)
- [N3 Logic Reasoning](https://w3c.github.io/n3/spec/)
- [Datalog Theory](https://en.wikipedia.org/wiki/Datalog)
- [Receipt Pattern Design](../packages/v6-core/src/receipt-pattern.mjs)

---

## Contributing

To extend this integration:

1. Add new FIBO JTBDs in `config.yaml`
2. Define Erlang callbacks in `packages/atomvm/erlang/fibo_hooks.erl`
3. Register hooks in `HooksBridge`
4. Add tests in `test/atomvm-integration.test.mjs`

---

## License

MIT - See LICENSE file for details.
