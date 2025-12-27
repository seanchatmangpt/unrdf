# @unrdf/kgc-probe - Visual Reference Guide

---

## 1. Complete Directory Tree (ASCII)

```
packages/kgc-probe/
│
├── src/
│   │
│   ├── index.mjs ................................. Main entry point (45 lines)
│   │
│   ├── schemas/
│   │   ├── index.mjs ............................. Schema exports (25 lines)
│   │   ├── observation.schema.mjs ............... Observation validator (50 lines)
│   │   ├── probe-result.schema.mjs .............. Result validator (45 lines)
│   │   ├── guard-policy.schema.mjs .............. Policy validator (40 lines)
│   │   ├── merge-config.schema.mjs .............. Merge config validator (35 lines)
│   │   └── receipt-metadata.schema.mjs .......... Receipt validator (50 lines)
│   │
│   ├── agents/
│   │   ├── index.mjs ............................. Probe registry & factory (60 lines)
│   │   ├── security-probe.mjs ................... Security validations (120 lines)
│   │   ├── performance-probe.mjs ................ Performance metrics (120 lines)
│   │   ├── correctness-probe.mjs ................ Correctness checks (120 lines)
│   │   ├── structure-probe.mjs .................. RDF structure validation (100 lines)
│   │   ├── completeness-probe.mjs ............... Graph completeness (100 lines)
│   │   ├── consistency-probe.mjs ................ Semantic consistency (120 lines)
│   │   ├── compliance-probe.mjs ................. Standards compliance (120 lines)
│   │   ├── coverage-probe.mjs ................... Code/test coverage (100 lines)
│   │   ├── mutation-probe.mjs ................... Mutation testing (120 lines)
│   │   └── integration-probe.mjs ................ Integration validation (120 lines)
│   │
│   ├── guards/
│   │   ├── index.mjs ............................. Guard registry & composer (50 lines)
│   │   ├── observation-guard.mjs ................ Observation validator (80 lines)
│   │   ├── result-guard.mjs ..................... Result validator (70 lines)
│   │   ├── graph-guard.mjs ....................... Graph validator (70 lines)
│   │   ├── merge-guard.mjs ....................... Merge validator (60 lines)
│   │   └── receipt-guard.mjs .................... Receipt validator (80 lines)
│   │
│   ├── storage/
│   │   ├── index.mjs ............................. Storage API (30 lines)
│   │   ├── probe-store.mjs ....................... KnowledgeStore wrapper (150 lines)
│   │   ├── graph-builder.mjs .................... RDF graph builder (120 lines)
│   │   ├── triple-generator.mjs ................. Triple generator (100 lines)
│   │   └── namespaces.mjs ....................... RDF namespaces (40 lines)
│   │
│   ├── orchestrator/
│   │   ├── index.mjs ............................. Orchestrator API (30 lines)
│   │   ├── probe-orchestrator.mjs ............... Probe coordinator (180 lines)
│   │   ├── merge-engine.mjs ..................... Merge strategy engine (140 lines)
│   │   ├── conflict-resolver.mjs ................ Conflict resolution (90 lines)
│   │   └── aggregator.mjs ....................... Result aggregation (80 lines)
│   │
│   ├── cli/
│   │   ├── index.mjs ............................. CLI registration (40 lines)
│   │   ├── run-probe.command.mjs ................ Run probe command (100 lines)
│   │   ├── validate-observation.command.mjs .... Validate observation (80 lines)
│   │   ├── merge-results.command.mjs ........... Merge results (80 lines)
│   │   └── export-receipt.command.mjs .......... Export receipt (80 lines)
│   │
│   ├── receipts/
│   │   ├── index.mjs ............................. Receipt API (25 lines)
│   │   ├── receipt-builder.mjs .................. Receipt constructor (120 lines)
│   │   ├── merkle-integrator.mjs ................ Merkle chain integration (110 lines)
│   │   └── verification.mjs ..................... Receipt verification (100 lines)
│   │
│   └── utils/
│       ├── index.mjs ............................. Utils exports (20 lines)
│       ├── logger.mjs ........................... Structured logging (80 lines)
│       ├── error-handler.mjs .................... Error handling (70 lines)
│       └── types.mjs ............................ JSDoc types (50 lines)
│
├── test/ ......................................... Jest test suite
│   ├── schemas/ ................................... Schema tests
│   ├── agents/ ..................................... Agent tests
│   ├── guards/ ..................................... Guard tests
│   ├── storage/ .................................... Storage tests
│   ├── orchestrator/ ............................... Orchestrator tests
│   ├── cli/ ........................................ CLI tests
│   ├── receipts/ ................................... Receipt tests
│   └── integration/ ................................ E2E tests
│
├── package.json .................................... Package metadata & deps
├── README.md ........................................ User documentation
└── CHANGELOG.md ..................................... Version history

Total: ~3,500 LoC (estimate)
Tests: ~5,000 LoC (2:1 code-to-test ratio target)
```

---

## 2. Dependency Graph (Detailed)

```
EXTERNAL PACKAGES
├── @unrdf/v6-core .................... v6 Receipt system
├── @unrdf/kgc-substrate .............. KnowledgeStore + ReceiptChain
├── @unrdf/oxigraph ................... RDF store + dataFactory
├── @unrdf/kgc-cli .................... Command registration
├── @unrdf/hooks ...................... Guard policies (proposed)
└── @unrdf/yawl ....................... Workflow engine (optional)

INTERNAL LAYERS (Bottom-Up)

Tier 1: Schemas (Validation)
├── schemas/observation.schema.mjs .... Zod(RDF terms + metadata)
├── schemas/probe-result.schema.mjs ... Zod(score, assertions)
├── schemas/guard-policy.schema.mjs ... Zod(enforce_mode, criteria)
├── schemas/merge-config.schema.mjs ... Zod(strategy, weights)
└── schemas/receipt-metadata.schema.mjs Zod(receipt structure)

Tier 2: Utilities & Types
├── utils/types.mjs ................... JSDoc type definitions
├── utils/error-handler.mjs ........... KGCProbeError, ValidationError
└── utils/logger.mjs .................. Pino + OTEL instrumentation

Tier 3: Guards (Poka-Yoke)
├── guards/observation-guard.mjs ...... Validates: observations
│   └─ depends: observation.schema
├── guards/result-guard.mjs ........... Validates: probe results
│   └─ depends: probe-result.schema
├── guards/graph-guard.mjs ............ Validates: RDF quads
│   └─ depends: error-handler
├── guards/merge-guard.mjs ............ Validates: merge operations
│   └─ depends: merge-config.schema
├── guards/receipt-guard.mjs .......... Validates: receipts
│   └─ depends: receipt-metadata.schema
└── guards/index.mjs .................. GuardComposer (all guards)

Tier 4: Storage (RDF Backend)
├── storage/namespaces.mjs ............ RDF ontology URIs (constants)
├── storage/triple-generator.mjs ...... Generates RDF quads
│   └─ depends: namespaces, @unrdf/oxigraph.dataFactory
├── storage/graph-builder.mjs ......... Builds OxigraphStore from results
│   └─ depends: triple-generator
├── storage/probe-store.mjs ........... Wraps KnowledgeStore
│   └─ depends: @unrdf/kgc-substrate, triple-generator
└── storage/index.mjs ................. Storage API

Tier 5: Agents (Probe Domains)
├── agents/security-probe.mjs ......... Probe 1: Security
├── agents/performance-probe.mjs ...... Probe 2: Performance
├── agents/correctness-probe.mjs ...... Probe 3: Correctness
├── agents/structure-probe.mjs ........ Probe 4: Structure
│   └─ depends: graph-guard, triple-generator
├── agents/completeness-probe.mjs ..... Probe 5: Completeness
├── agents/consistency-probe.mjs ...... Probe 6: Consistency
├── agents/compliance-probe.mjs ....... Probe 7: Compliance
├── agents/coverage-probe.mjs ......... Probe 8: Coverage
├── agents/mutation-probe.mjs ......... Probe 9: Mutation
├── agents/integration-probe.mjs ...... Probe 10: Integration
│   └─ depends: @unrdf/oxigraph.dataFactory
└── agents/index.mjs .................. ProbeRegistry & factory

Tier 6: Receipts (v6-core Integration)
├── receipts/receipt-builder.mjs ...... Creates BaseReceipt + signature
│   └─ depends: @unrdf/v6-core.BaseReceipt, receipt-metadata.schema
├── receipts/merkle-integrator.mjs .... Chains receipts via ReceiptChain
│   └─ depends: @unrdf/kgc-substrate.ReceiptChain
├── receipts/verification.mjs ......... Verifies receipt signatures
│   └─ depends: merkle-integrator, crypto (BLAKE3)
└── receipts/index.mjs ................ Receipt API

Tier 7: Orchestration (Coordination)
├── orchestrator/aggregator.mjs ....... Summarizes probe results
│   └─ depends: schemas/probe-result.schema
├── orchestrator/conflict-resolver.mjs Resolves merge conflicts
│   └─ depends: result-guard, logger
├── orchestrator/merge-engine.mjs ..... Merges multi-domain results
│   └─ depends: conflict-resolver, merge-guard
├── orchestrator/probe-orchestrator.mjs Coordinates all probes
│   └─ depends: agents/*, guards/*, storage/*, logger
└── orchestrator/index.mjs ............ Orchestrator API

Tier 8: CLI (Command Interface)
├── cli/run-probe.command.mjs ......... kgc probe run [domain]
│   └─ depends: probe-orchestrator, @unrdf/kgc-cli
├── cli/validate-observation.command.. kgc probe validate [file]
│   └─ depends: observation-guard, @unrdf/kgc-cli
├── cli/merge-results.command.mjs .... kgc probe merge [results]
│   └─ depends: merge-engine, @unrdf/kgc-cli
├── cli/export-receipt.command.mjs ... kgc probe export [id]
│   └─ depends: receipt-builder, @unrdf/kgc-cli
└── cli/index.mjs ..................... Command registration

Tier 9: Main Entry Point
└── index.mjs .......................... Exports all public APIs
    └─ depends: all tiers (re-exports)
```

---

## 3. Data Flow Diagram (Mermaid)

```mermaid
graph TD
    A["CLI Input<br/>kgc probe run"] -->|args| B["Run Probe<br/>Command"]

    B -->|load| C["Load Observations<br/>from file"]
    C -->|validate| D["Observation Guard<br/>Schema + RDF terms"]
    D -->|✓ valid| E["Probe Orchestrator<br/>Registry lookup"]

    E -->|dispatch| F["Run Probes<br/>Security, Performance,<br/>Correctness,<br/>Structure, ...]

    F -->|collect| G["Result Guard<br/>Validate score,<br/>assertions"]
    G -->|✓ valid| H["Graph Builder<br/>Results → RDF quads"]

    H -->|validate| I["Graph Guard<br/>RDF structure,<br/>ontology"]
    I -->|✓ valid| J["Probe Store<br/>Append to<br/>KnowledgeStore"]

    J -->|aggregate| K["Merge Engine<br/>Consensus,<br/>conflict resolution"]
    K -->|validate| L["Merge Guard<br/>Policy enforcement"]

    L -->|✓ valid| M["Receipt Builder<br/>Create receipt,<br/>sign"]
    M -->|chain| N["Merkle Integrator<br/>Link to chain,<br/>merkle root"]

    N -->|validate| O["Receipt Guard<br/>Schema, signature,<br/>merkle"]
    O -->|✓ valid| P["Output Receipt<br/>JSON + signature"]

    style D fill:#fff5e6
    style G fill:#fff5e6
    style I fill:#fff5e6
    style L fill:#fff5e6
    style O fill:#fff5e6
```

---

## 4. Guard Enforcement Points (Table)

```
┌────────────────────────┬─────────────────────────────────────┬──────────────┐
│ Guard                  │ Validation Points                   │ Error Action │
├────────────────────────┼─────────────────────────────────────┼──────────────┤
│ Observation Guard      │ - RDF term types                    │ Reject       │
│ (Entry point)          │ - Metadata presence                 │ + Log        │
│                        │ - Schema conformance (Zod)          │              │
├────────────────────────┼─────────────────────────────────────┼──────────────┤
│ Result Guard           │ - Score range [0.0, 1.0]            │ Reject       │
│ (After probe)          │ - Assertion invariants              │ + Log        │
│                        │ - Duration ≥ 0                      │              │
├────────────────────────┼─────────────────────────────────────┼──────────────┤
│ Graph Guard            │ - Valid S-P-O structure             │ Reject       │
│ (Before store)         │ - No blank node cycles              │ + Log        │
│                        │ - Namespace conformance             │              │
├────────────────────────┼─────────────────────────────────────┼──────────────┤
│ Merge Guard            │ - Config schema valid               │ Reject       │
│ (Before merge)         │ - Results non-empty                 │ + Log        │
│                        │ - Strategy available                │              │
├────────────────────────┼─────────────────────────────────────┼──────────────┤
│ Receipt Guard          │ - Receipt schema conformance        │ Reject       │
│ (Before output)        │ - Signature valid (RSA-4096)        │ + Log        │
│                        │ - Merkle root matches               │              │
│                        │ - Timestamp reasonable              │              │
└────────────────────────┴─────────────────────────────────────┴──────────────┘
```

---

## 5. Probe Domains (10 Agents)

```
┌──────────┬──────────────────────┬─────────────────────────┬────────────┐
│ # (ID)   │ Probe Name           │ Key Validations         │ Returns    │
├──────────┼──────────────────────┼─────────────────────────┼────────────┤
│ 1        │ Security Probe       │ Auth, encryption,       │ Score +    │
│          │ security-probe.mjs   │ secrets, injection      │ Assertions │
├──────────┼──────────────────────┼─────────────────────────┼────────────┤
│ 2        │ Performance Probe    │ Latency, throughput,    │ Score +    │
│          │ performance-probe    │ memory, CPU usage       │ Metrics    │
├──────────┼──────────────────────┼─────────────────────────┼────────────┤
│ 3        │ Correctness Probe    │ Logic correctness,      │ Score +    │
│          │ correctness-probe    │ invariant violations    │ Violations │
├──────────┼──────────────────────┼─────────────────────────┼────────────┤
│ 4        │ Structure Probe      │ RDF ontology, term      │ Score +    │
│          │ structure-probe      │ validity, canonicality  │ Issues     │
├──────────┼──────────────────────┼─────────────────────────┼────────────┤
│ 5        │ Completeness Probe   │ Missing properties,     │ Score +    │
│          │ completeness-probe   │ coverage gaps           │ Gaps       │
├──────────┼──────────────────────┼─────────────────────────┼────────────┤
│ 6        │ Consistency Probe    │ Semantic consistency,   │ Score +    │
│          │ consistency-probe    │ contradiction detection │ Issues     │
├──────────┼──────────────────────┼─────────────────────────┼────────────┤
│ 7        │ Compliance Probe     │ Standards adherence     │ Score +    │
│          │ compliance-probe     │ (SPARQL, OWL, etc)      │ Violations │
├──────────┼──────────────────────┼─────────────────────────┼────────────┤
│ 8        │ Coverage Probe       │ Code coverage, test     │ Score +    │
│          │ coverage-probe       │ coverage, branch cov    │ Metrics    │
├──────────┼──────────────────────┼─────────────────────────┼────────────┤
│ 9        │ Mutation Probe       │ Mutation testing,       │ Score +    │
│          │ mutation-probe       │ fault injection         │ Mutations  │
├──────────┼──────────────────────┼─────────────────────────┼────────────┤
│ 10       │ Integration Probe    │ Service integration,    │ Score +    │
│          │ integration-probe    │ dependency resolution   │ Issues     │
└──────────┴──────────────────────┴─────────────────────────┴────────────┘
```

---

## 6. Merge Strategies Comparison

```
┌──────────────────┬──────────────────────┬──────────────┬─────────────┐
│ Strategy         │ Calculation          │ Use Case     │ Example     │
├──────────────────┼──────────────────────┼──────────────┼─────────────┤
│ consensus        │ All must pass        │ Strict       │ score =     │
│                  │ (AND logic)          │ requirement  │ min(scores) │
├──────────────────┼──────────────────────┼──────────────┼─────────────┤
│ max              │ Take highest score   │ Best-case    │ score =     │
│                  │ (OR logic)           │ scenario     │ max(scores) │
├──────────────────┼──────────────────────┼──────────────┼─────────────┤
│ min              │ Take lowest score    │ Worst-case   │ score =     │
│                  │ (safety margin)      │ scenario     │ min(scores) │
├──────────────────┼──────────────────────┼──────────────┼─────────────┤
│ weighted_sum     │ Domain weights       │ Balanced     │ score =     │
│                  │ (w1*s1 + w2*s2) / Σ │ approach     │ Σ(wi*si)/Σw │
└──────────────────┴──────────────────────┴──────────────┴─────────────┘

Example Weights (security-heavy):
{
  "security": 0.4,
  "correctness": 0.3,
  "performance": 0.15,
  "completeness": 0.1,
  "compliance": 0.05
}

weighted_score = (0.4*0.95 + 0.3*0.88 + 0.15*0.92 + 0.1*0.85 + 0.05*0.80)
               = (0.38 + 0.264 + 0.138 + 0.085 + 0.04)
               = 0.907 ≈ 0.91
```

---

## 7. RDF Namespace Mappings

```
Prefix         URI                                  Used By
───────────────────────────────────────────────────────────────────────
kgc            http://kgc.io/ontology/             All modules
probe          http://kgc.io/probe/                Probe results
result         http://kgc.io/result/               Result storage
security       http://kgc.io/probe/security/       Security probe
performance    http://kgc.io/probe/performance/    Performance probe
correctness    http://kgc.io/probe/correctness/    Correctness probe
structure      http://kgc.io/probe/structure/      Structure probe
completeness   http://kgc.io/probe/completeness/   Completeness probe
consistency    http://kgc.io/probe/consistency/    Consistency probe
compliance     http://kgc.io/probe/compliance/     Compliance probe
coverage       http://kgc.io/probe/coverage/       Coverage probe
mutation       http://kgc.io/probe/mutation/       Mutation probe
integration    http://kgc.io/probe/integration/    Integration probe
rdf            http://www.w3.org/1999/02/22-...    RDF standard
rdfs           http://www.w3.org/2000/01/rdf-s..   RDFS standard
xsd            http://www.w3.org/2001/XMLSchema#   XML Schema
```

Example Triple:
```
<http://kgc.io/result/rcpt-2025-12-27-001>
  <http://kgc.io/ontology/score>
  "0.92"^^<http://www.w3.org/2001/XMLSchema#decimal>
```

---

## 8. Receipt Structure (v6-core Integration)

```javascript
// Receipt extends BaseReceipt
{
  // Base receipt fields (from v6-core)
  receipt_id: "rcpt-2025-12-27-001",
  type: "probe-result",
  timestamp_ns: 1735308000000000000n,

  // Probe-specific fields
  domain: "security",
  score: 0.92,
  state_hash: "blake3:9f86d08557674702e3c35e...",

  // Assertions (probe results)
  assertions: [
    {
      id: "sec-001",
      status: "pass",
      evidence: "Authentication enforcement verified",
      weight: 1.0
    },
    {
      id: "sec-002",
      status: "warning",
      evidence: "Encryption cipher strength 128-bit (256-bit recommended)",
      weight: 0.5
    }
  ],

  // Merkle chain fields (from merkle-integrator)
  merkle_root: "hash(receipts[0..n-1])",
  merkle_path: ["h0", "h1", "h2", ...],  // Path to root

  // Signature (RSA-4096)
  signature: "sig(receipt_data, private_key)",

  // Metadata
  quad_count: 127,
  duration_ms: 245,
  metadata: {
    agent_id: "probe-agent-1",
    environment: "production",
    version: "1.0.0"
  }
}
```

---

## 9. CLI Command Syntax Reference

```bash
# 1. Run probe on single domain
kgc probe run \
  --domain security \
  --observations observations.rdf \
  --output-format json

# 2. Validate observations before running
kgc probe validate observations.rdf --strict

# 3. Merge multiple probe results
kgc probe merge result1.json result2.json result3.json \
  --strategy weighted_sum \
  --weights '{"security": 0.4, "performance": 0.3, ...}' \
  --output-file merged.json

# 4. Export and verify receipt
kgc probe export rcpt-2025-12-27-001 \
  --verify true \
  --format json

# 5. Run all probes (default behavior when no domain specified)
kgc probe run \
  --observations observations.rdf \
  --parallel true

# 6. Short form commands
kgc probe run -d security -o obs.rdf
kgc probe validate obs.rdf -s
kgc probe merge *.json -s consensus
kgc probe export rcpt-001 -v
```

---

## 10. Integration Test Scenarios

```
Test Case                    Expected Flow                 Assertions
───────────────────────────────────────────────────────────────────────
Single Probe Run             Obs → Guard → Probe           Score [0,1]
(happy path)                 → Guard → Store → Receipt     Receipt signed

Multi-Probe Run              Obs → All probes parallel    Aggregated score
(10 domains)                 → Merge → Receipt            10 assertions

Observation Invalid          Obs → Guard rejects           Error raised
(guard rejection)            No probe runs                 Exit code 1

Probe Timeout                Timeout after 5s              Fail status
(poka-yoke)                  Result marked as timeout      Score 0.0

Merge Conflict               Multi results different      ConflictResolver
(multiple domains)           scores → Manual review       logs decision

Receipt Verification         Receipt → Guard verify       Signature valid
(merkle chain)               → Merkle path → Root         Tamper detected if

Graph Build Error            Result → Graph builder       Error logged
(RDF generation)             error → Reject                Guard rejects

CLI Command Missing           kgc probe unknown-cmd        Help shown
(CLI error)                                                Exit code 2
```

---

## 11. Performance Targets

```
Operation                  Target Time    Memory Usage    Notes
───────────────────────────────────────────────────────────────────
Parse observations        < 100ms        < 50MB         1000 quads
Observe validation        < 50ms         < 10MB         Per obs
Single probe execution    < 500ms        < 100MB        Depends on domain
All probes parallel       < 2000ms       < 300MB        10 domains
Graph building            < 200ms        < 80MB         Result → RDF
Merge operation           < 300ms        < 60MB         3+ results
Receipt generation        < 500ms        < 100MB        Sign + merkle
Full pipeline             < 5000ms       < 500MB        End-to-end
```

---

## 12. Error Handling Matrix

```
Error Type                 Source           Handler         Action
──────────────────────────────────────────────────────────────────────
ValidationError            Guard            error-handler   Log + reject
SchemaValidationError      Zod              error-handler   Log + reject
TimeoutError              Probe            error-handler   Fail status
RDFTermTypeError          triple-gen       error-handler   Log + reject
MergeConflictError        merge-engine     conflict-res    Manual review
ReceiptSignatureError     receipt-guard    error-handler   Reject receipt
KnowledgeStoreError       probe-store      error-handler   Log + retry
UncaughtError             Any              error-handler   Log + throw
```

---

**END OF VISUAL REFERENCE GUIDE**
