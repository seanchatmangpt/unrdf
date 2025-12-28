# @unrdf/kgc-probe Architecture Design

**Status**: Architecture Phase (SPARC)
**Date**: 2025-12-27
**Agent**: Agent-2 (System Architect)
**Scope**: Complete file structure, integration points, and data flow design for kgc-probe package

---

## Executive Summary

The `@unrdf/kgc-probe` package is a multi-domain knowledge graph validation and certification system that combines 10 specialized probe agents with cryptographic receipt generation and RDF-backed storage. It serves as the "auditor" layer that validates, measures, and certifies the quality of knowledge graphs across 10 orthogonal domains (security, performance, correctness, structure, completeness, consistency, compliance, coverage, mutation, integration).

**Core Architecture Principles**:
- **Separation of Concerns**: Each probe domain is isolated; guards enforce boundaries
- **Immutable Audit Trail**: All probe results stored in KnowledgeStore (append-only RDF log)
- **Cryptographic Verification**: Every result produces a receipt with merkle chain
- **Poka-Yoke Enforcement**: Guards validate inputs/outputs; no silent failures
- **CLI-First Integration**: Commands registered with @unrdf/kgc-cli; extensible via @unrdf/yawl

---

## 1. Directory Structure

```
packages/kgc-probe/src/
├── index.mjs                           # Main entry point (exports public API)
│
├── schemas/                            # Zod validators for all data types
│   ├── index.mjs                       # Re-exports all schemas
│   ├── observation.schema.mjs          # Input observations (RDF quads + metadata)
│   ├── probe-result.schema.mjs         # Probe execution result
│   ├── guard-policy.schema.mjs         # Guard configuration
│   ├── merge-config.schema.mjs         # Merge strategy rules
│   └── receipt-metadata.schema.mjs     # Receipt structure (v6-core integration)
│
├── agents/                             # 10 specialized probe domains
│   ├── index.mjs                       # Probe registry and factory
│   ├── security-probe.mjs              # 1. Security validations
│   ├── performance-probe.mjs           # 2. Performance metrics
│   ├── correctness-probe.mjs           # 3. Correctness validation
│   ├── structure-probe.mjs             # 4. RDF structure validation
│   ├── completeness-probe.mjs          # 5. Graph completeness
│   ├── consistency-probe.mjs           # 6. Semantic consistency
│   ├── compliance-probe.mjs            # 7. Standards compliance
│   ├── coverage-probe.mjs              # 8. Code/test coverage
│   ├── mutation-probe.mjs              # 9. Mutation testing
│   └── integration-probe.mjs           # 10. Integration validation
│
├── guards/                             # Poka-yoke enforcement gates
│   ├── index.mjs                       # Guard registry and composition
│   ├── observation-guard.mjs           # Validate observations on entry
│   ├── result-guard.mjs                # Validate probe results
│   ├── graph-guard.mjs                 # Validate RDF graphs
│   ├── merge-guard.mjs                 # Validate merge operations
│   └── receipt-guard.mjs               # Validate receipt generation
│
├── storage/                            # RDF graphs + KnowledgeStore integration
│   ├── index.mjs                       # Storage API and factory
│   ├── probe-store.mjs                 # KnowledgeStore wrapper + extensions
│   ├── graph-builder.mjs               # Build RDF graphs from observations
│   ├── triple-generator.mjs            # Generate RDF triples (N-Quads)
│   └── namespaces.mjs                  # RDF ontology namespaces
│
├── orchestrator/                       # Merge logic + coordination
│   ├── index.mjs                       # Orchestrator factory and API
│   ├── probe-orchestrator.mjs          # Run all probes (sequence/parallel)
│   ├── merge-engine.mjs                # Merge strategy and conflict resolution
│   ├── conflict-resolver.mjs           # Resolve conflicting probe results
│   └── aggregator.mjs                  # Aggregate results into summary
│
├── cli/                                # Command handlers (kgc-cli integration)
│   ├── index.mjs                       # Command registration
│   ├── run-probe.command.mjs           # CLI: kgc probe run [domain]
│   ├── validate-observation.command.mjs # CLI: kgc probe validate [file]
│   ├── merge-results.command.mjs       # CLI: kgc probe merge [results]
│   └── export-receipt.command.mjs      # CLI: kgc probe export [receiptId]
│
├── receipts/                           # v6-core integration
│   ├── index.mjs                       # Receipt API
│   ├── receipt-builder.mjs             # Build receipts from probe results
│   ├── merkle-integrator.mjs           # Merkle tree chaining (ReceiptChain)
│   └── verification.mjs                # Verify receipt signatures
│
└── utils/                              # Shared utilities
    ├── index.mjs                       # Utility exports
    ├── logger.mjs                      # Structured logging (OTEL-ready)
    ├── error-handler.mjs               # Centralized error handling
    └── types.mjs                       # JSDoc type definitions
```

---

## 2. Integration Points (Dependency Matrix)

### 2.1 Package Dependencies

```
@unrdf/kgc-probe
├── @unrdf/v6-core
│   ├── receipts/base-receipt.mjs           (extends BaseReceipt)
│   ├── receipts/merkle/                    (merkle-integrator uses)
│   └── Types: Receipt, StateCommitment
│
├── @unrdf/kgc-substrate
│   ├── KnowledgeStore                      (probe-store wraps)
│   ├── ReceiptChain                        (merkle-integrator uses)
│   └── Types: StorageSnapshot, StateCommitment
│
├── @unrdf/oxigraph
│   ├── createStore()                       (triple-generator uses)
│   ├── dataFactory                         (all agents use for quad creation)
│   └── OxigraphStore                       (graph-builder creates)
│
├── @unrdf/kgc-cli
│   ├── registerCommand()                   (cli/* exports commands)
│   └── CommandRegistry                     (cli/index.mjs registers)
│
├── @unrdf/hooks (proposed)
│   ├── GuardPolicy interface               (guards/ implement)
│   └── poka-yoke enforcement               (observation-guard uses)
│
└── @unrdf/yawl (optional, for orchestration)
    ├── WorkflowBuilder                     (orchestrator can extend)
    └── WorkflowEngine                      (async probe coordination)
```

### 2.2 Module Dependency Coupling

```
    CLI Layer
    ├─ run-probe.command.mjs
    │  └─ depends on: probe-orchestrator
    ├─ validate-observation.command.mjs
    │  └─ depends on: observation-guard, schemas
    ├─ merge-results.command.mjs
    │  └─ depends on: merge-engine
    └─ export-receipt.command.mjs
       └─ depends on: receipt-builder, verification

    Orchestration Layer
    ├─ probe-orchestrator.mjs
    │  ├─ depends on: all agents/*
    │  ├─ depends on: merge-engine
    │  └─ depends on: aggregator
    ├─ merge-engine.mjs
    │  ├─ depends on: conflict-resolver
    │  └─ depends on: merge-guard
    ├─ conflict-resolver.mjs
    │  └─ depends on: result-guard, logger
    └─ aggregator.mjs
       └─ depends on: schemas

    Agent Layer (10 Probes)
    ├─ security-probe.mjs
    │  ├─ depends on: observation-guard
    │  └─ depends on: logger
    ├─ performance-probe.mjs
    ├─ correctness-probe.mjs
    ├─ structure-probe.mjs
    │  └─ depends on: graph-guard, triple-generator
    ├─ completeness-probe.mjs
    ├─ consistency-probe.mjs
    ├─ compliance-probe.mjs
    ├─ coverage-probe.mjs
    ├─ mutation-probe.mjs
    └─ integration-probe.mjs
       └─ depends on: oxigraph/dataFactory

    Storage Layer
    ├─ probe-store.mjs
    │  ├─ wraps: @unrdf/kgc-substrate.KnowledgeStore
    │  └─ uses: @unrdf/oxigraph.dataFactory
    ├─ graph-builder.mjs
    │  ├─ depends on: triple-generator
    │  ├─ depends on: namespaces
    │  └─ creates: OxigraphStore quads
    └─ triple-generator.mjs
       ├─ depends on: namespaces
       └─ uses: @unrdf/oxigraph.dataFactory

    Guard Layer
    ├─ observation-guard.mjs
    │  ├─ depends on: observation.schema
    │  └─ depends on: error-handler
    ├─ result-guard.mjs
    │  ├─ depends on: probe-result.schema
    │  └─ depends on: error-handler
    ├─ graph-guard.mjs
    │  └─ depends on: error-handler
    ├─ merge-guard.mjs
    │  ├─ depends on: merge-config.schema
    │  └─ depends on: error-handler
    └─ receipt-guard.mjs
       ├─ depends on: receipt-metadata.schema
       └─ depends on: error-handler

    Receipt Layer
    ├─ receipt-builder.mjs
    │  ├─ wraps: @unrdf/v6-core.BaseReceipt
    │  ├─ depends on: receipt-metadata.schema
    │  └─ depends on: proof-of-work (for receipt sig)
    ├─ merkle-integrator.mjs
    │  ├─ uses: @unrdf/kgc-substrate.ReceiptChain
    │  └─ uses: @unrdf/v6-core.merkle/*
    └─ verification.mjs
       ├─ depends on: merkle-integrator
       └─ depends on: crypto (BLAKE3)

    Foundation Layers
    ├─ schemas/* (all depend on: zod)
    ├─ logger.mjs (depends on: pino, OTEL exporter)
    ├─ error-handler.mjs (no dependencies)
    └─ types.mjs (pure JSDoc, no deps)
```

---

## 3. Data Flow Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ CLIENT / CLI INPUT                                                  │
│ kgc probe run --domain security --observations observations.rdf    │
└──────────────────────────┬──────────────────────────────────────────┘
                           │
                           ▼
        ┌──────────────────────────────────────┐
        │ VALIDATION GATE (observation-guard)  │
        │ - Parse RDF quads                    │
        │ - Validate schema (Zod)              │
        │ - Reject if invalid                  │
        └──────────────────┬───────────────────┘
                           │ ✓ valid observation
                           ▼
        ┌──────────────────────────────────────┐
        │ PROBE ORCHESTRATOR                   │
        │ - Load requested domain probe agent  │
        │ - Run probe.execute(observation)     │
        │ - Collect results                    │
        └──────────────────┬───────────────────┘
                           │
            ┌──────────────┼──────────────┐
            │              │              │
            ▼              ▼              ▼
      ┌─────────────┐ ┌─────────────┐ ┌─────────────┐
      │   Agent 1   │ │   Agent 2   │ │   Agent N   │
      │ (Security)  │ │(Performance)│ │ (Mutation)  │
      │             │ │             │ │             │
      │ Validations │ │ Metrics     │ │ Mutations   │
      └──────┬──────┘ └──────┬──────┘ └──────┬──────┘
             │               │               │
             └───────────────┼───────────────┘
                             │
                             ▼
        ┌──────────────────────────────────────┐
        │ RESULT GUARD (result-guard)          │
        │ - Validate probe result structure    │
        │ - Check assertion invariants         │
        │ - Reject if invalid                  │
        └──────────────────┬───────────────────┘
                           │ ✓ valid result
                           ▼
        ┌──────────────────────────────────────┐
        │ RDF GRAPH BUILDER (graph-builder)    │
        │ - Transform results to RDF triples   │
        │ - Generate N-Quads                   │
        │ - Create OxigraphStore               │
        └──────────────────┬───────────────────┘
                           │
                           ▼
        ┌──────────────────────────────────────┐
        │ GRAPH GUARD (graph-guard)            │
        │ - Validate RDF quads                 │
        │ - Check ontology conformance         │
        │ - Reject if invalid                  │
        └──────────────────┬───────────────────┘
                           │ ✓ valid graph
                           ▼
        ┌──────────────────────────────────────┐
        │ KNOWLEDGE STORE (probe-store)        │
        │ - Append quads to KnowledgeStore     │
        │ - Immutable append-only log          │
        │ - Generate state commitment          │
        └──────────────────┬───────────────────┘
                           │
                           ▼
        ┌──────────────────────────────────────┐
        │ MERGE ENGINE (merge-engine)          │
        │ - Aggregate multi-domain results     │
        │ - Resolve conflicts                  │
        │ - Produce merged summary             │
        └──────────────────┬───────────────────┘
                           │
                           ▼
        ┌──────────────────────────────────────┐
        │ MERGE GUARD (merge-guard)            │
        │ - Validate merge operations          │
        │ - Check policy compliance            │
        │ - Enforce constraints                │
        └──────────────────┬───────────────────┘
                           │ ✓ valid merge
                           ▼
        ┌──────────────────────────────────────┐
        │ RECEIPT BUILDER (receipt-builder)    │
        │ - Create BaseReceipt (v6-core)       │
        │ - Sign proof-of-work                 │
        │ - Generate merkle commitment         │
        └──────────────────┬───────────────────┘
                           │
                           ▼
        ┌──────────────────────────────────────┐
        │ MERKLE INTEGRATOR (merkle-integrator)│
        │ - Chain with ReceiptChain            │
        │ - Generate merkle path               │
        │ - Return verified receipt            │
        └──────────────────┬───────────────────┘
                           │
                           ▼
        ┌──────────────────────────────────────┐
        │ RECEIPT GUARD (receipt-guard)        │
        │ - Validate receipt schema            │
        │ - Check merkle signatures            │
        │ - Reject if invalid                  │
        └──────────────────┬───────────────────┘
                           │ ✓ valid receipt
                           ▼
┌─────────────────────────────────────────────────────────────────────┐
│ OUTPUT                                                              │
│ {                                                                   │
│   "receipt_id": "rcpt-2025-12-27-001",                            │
│   "domain": "security",                                            │
│   "state_hash": "blake3:9f86d08...",                              │
│   "score": 0.92,                                                   │
│   "assertions": [                                                  │
│     { "id": "sec-001", "status": "pass", "evidence": "..." }     │
│   ],                                                               │
│   "merkle_root": "h(root)...",                                    │
│   "signature": "sig(...)",                                         │
│   "timestamp_ns": 1735308000000000000                             │
│ }                                                                   │
└─────────────────────────────────────────────────────────────────────┘
```

---

## 4. Module Entry Point Design

### 4.1 Index.mjs (Main Export)

```javascript
/**
 * @unrdf/kgc-probe
 *
 * Multi-domain knowledge graph validation and certification system.
 * Exports public API for probe execution, validation, and receipt generation.
 */

// 1. Schemas (validators)
export {
  ObservationSchema,
  ProbeResultSchema,
  GuardPolicySchema,
  MergeConfigSchema,
  ReceiptMetadataSchema,
} from './schemas/index.mjs';

// 2. Probes (agent registry)
export {
  createProbeOrchestrator,
  getProbeRegistry,
  SecurityProbe,
  PerformanceProbe,
  CorrectnessProbe,
  StructureProbe,
  CompletenessProbe,
  ConsistencyProbe,
  ComplianceProbe,
  CoverageProbe,
  MutationProbe,
  IntegrationProbe,
} from './agents/index.mjs';

// 3. Guards (poka-yoke enforcement)
export {
  createGuardComposer,
  ObservationGuard,
  ResultGuard,
  GraphGuard,
  MergeGuard,
  ReceiptGuard,
} from './guards/index.mjs';

// 4. Storage (KnowledgeStore integration)
export {
  createProbeStore,
  GraphBuilder,
  TripleGenerator,
  RDFNamespaces,
} from './storage/index.mjs';

// 5. Orchestration (merge logic)
export {
  ProbeOrchestrator,
  MergeEngine,
  ConflictResolver,
  Aggregator,
} from './orchestrator/index.mjs';

// 6. Receipts (v6-core integration)
export {
  createReceiptBuilder,
  MerkleIntegrator,
  ReceiptVerifier,
} from './receipts/index.mjs';

// 7. CLI (kgc-cli integration)
export {
  registerProbeCommands,
} from './cli/index.mjs';

// 8. Utilities
export {
  createLogger,
  KGCProbeError,
  ValidationError,
} from './utils/index.mjs';

// Default factory for quick start
export default {
  createProbeOrchestrator,
  createProbeStore,
  createReceiptBuilder,
  createLogger,
};
```

### 4.2 Execution Flow via Index.mjs

```javascript
// Example: Typical user flow
import KGCProbe, {
  ObservationSchema,
  createProbeOrchestrator,
  createProbeStore,
  createReceiptBuilder,
} from '@unrdf/kgc-probe';

// 1. Load observations
const observations = await fs.readFile('observations.jsonl', 'utf8');
const parsed = observations.split('\n').map(line => JSON.parse(line));

// 2. Validate observations
const validated = parsed.map(obs => ObservationSchema.parse(obs));

// 3. Create store
const store = await createProbeStore({ nodeId: 'probe-agent-1' });

// 4. Create orchestrator
const orchestrator = await createProbeOrchestrator({ store });

// 5. Run all probes
const results = await orchestrator.executeAllProbes(validated);

// 6. Merge results
const merged = await orchestrator.mergeResults(results);

// 7. Generate receipt
const receiptBuilder = await createReceiptBuilder();
const receipt = await receiptBuilder.build(merged);

// 8. Return receipt
console.log(receipt);
```

---

## 5. File Specifications

### 5.1 Schemas Directory

#### observation.schema.mjs
```
Exports: ObservationSchema, validateObservation()
Zod Validators:
- subject: Named node or blank node
- predicate: Named node (RDF property)
- object: Any RDF term (named node, blank node, literal)
- graph: Named node (default universe graph)
- metadata:
  - timestamp: ISO 8601 timestamp
  - source: string (agent name)
  - confidence: number (0.0-1.0)
  - context: object (optional metadata)

Pattern: Use z.object() with z.instanceof() for RDF terms from oxigraph
```

#### probe-result.schema.mjs
```
Exports: ProbeResultSchema, validateProbeResult()
Zod Validators:
- probe_id: string (UUID)
- domain: enum('security', 'performance', 'correctness', ...)
- status: enum('pass', 'fail', 'warning', 'skip')
- score: number (0.0-1.0)
- assertions: array of { id, status, evidence, weight }
- duration_ms: number
- timestamp_ns: bigint
- metadata: object

Pattern: All results immutable; score derived from assertions
```

#### guard-policy.schema.mjs
```
Exports: GuardPolicySchema, validatePolicy()
Zod Validators:
- enforce_mode: enum('strict', 'warn', 'skip')
- rejection_criteria: array of { path, condition, message }
- allowed_failures: number (fail threshold before reject)
- timeout_ms: number (default 5000)

Pattern: Policies applied at guard gates; composable via GuardComposer
```

#### merge-config.schema.mjs
```
Exports: MergeConfigSchema, validateMergeConfig()
Zod Validators:
- strategy: enum('consensus', 'max', 'min', 'weighted_sum')
- conflict_resolution: enum('highest_confidence', 'newest_first', 'manual')
- aggregate_by: string (grouping key)
- weights: object (domain -> weight mapping)

Pattern: Drives merge-engine behavior; ensures consistent results
```

#### receipt-metadata.schema.mjs
```
Exports: ReceiptMetadataSchema, validateReceiptMetadata()
Zod Validators:
- receipt_id: UUID
- domain: string
- state_hash: string (BLAKE3 hex)
- merkle_root: string
- signature: string (RSA-4096 signature)
- timestamp_ns: bigint
- quad_count: number

Pattern: Extends v6-core BaseReceipt structure; validated before signing
```

---

### 5.2 Agents Directory

#### agents/index.mjs
```
Exports:
- ProbeRegistry: Map<domain: string, ProbeClass: constructor>
- createProbeOrchestrator(options): ProbeOrchestrator instance
- getProbeRegistry(): Returns ProbeRegistry
- All 10 probe classes (for direct instantiation)

Implementation:
- Probe interface: { domain, execute(observation), name, version }
- Registry pattern: Map-based lookup
- Factory function: Lazy-loads probes on demand
```

#### security-probe.mjs
```
Exports: class SecurityProbe
Fields:
- domain: 'security'
- name: 'Security Probe'

Methods:
- execute(observation): Promise<ProbeResult>
  - Checks: auth, encryption, secrets, injection attacks
  - Evidence: findings, remediation_steps
  - Returns: ProbeResult with score [0.0-1.0]

Integration:
- Calls: observation-guard to validate input
- Calls: logger.debug() for metrics
```

#### [Similar structure for 9 other probes]

---

### 5.3 Guards Directory

#### observation-guard.mjs
```
Exports: class ObservationGuard
Methods:
- validate(observation): throws ValidationError or returns validated object
- enforcePolicy(policy, observation): Policy-driven validation

Validation Chain:
1. Schema validation (Zod)
2. RDF term type checking
3. Metadata validation
4. Rejection on policy violations

Pattern: Synchronous; fast rejection at entry point
```

#### result-guard.mjs
```
Exports: class ResultGuard
Methods:
- validate(probeResult): throws or returns validated result
- verifyAssertions(assertions): Checks invariants

Invariants:
- score is 0.0 <= score <= 1.0
- All assertions have unique IDs
- Duration is non-negative
- Status matches assertion summary

Pattern: Run after each probe executes; guards prevent invalid results propagating
```

#### graph-guard.mjs
```
Exports: class GraphGuard
Methods:
- validateQuads(quads): Validates RDF structure
- checkOntologyConformance(quads): Against RDF schema

Checks:
- Valid S-P-O structure
- No blank node cycles
- Namespace conformance
- Literal datatype validation

Pattern: Run before graph-builder appends to store
```

#### merge-guard.mjs
```
Exports: class MergeGuard
Methods:
- validateMergeOp(config, results): Validates merge operation

Checks:
- Config is valid MergeConfigSchema
- Results array is non-empty
- Strategy is available
- No data loss in merge

Pattern: Run before merge-engine executes; prevents invalid merges
```

#### receipt-guard.mjs
```
Exports: class ReceiptGuard
Methods:
- validateReceipt(receipt): Validates receipt structure
- verifySentSignature(receipt): Cryptographic verification

Checks:
- Receipt schema conformance
- Signature valid (RSA-4096)
- Merkle root matches
- Timestamp within acceptable range

Pattern: Final validation gate; prevents corrupted receipts from exiting system
```

---

### 5.4 Storage Directory

#### probe-store.mjs
```
Exports: class ProbeStore (extends KnowledgeStore)
Constructor:
- Wraps @unrdf/kgc-substrate.KnowledgeStore
- Adds probe-specific methods

Methods:
- appendProbeResult(probeResult): Appends as RDF triples
- getProbeHistory(domain): Returns all probes for domain
- generateProbeSnapshot(): Creates immutable snapshot
- queryByDomain(domain, predicate): Pattern query

Integration:
- Uses: @unrdf/kgc-substrate.KnowledgeStore
- Uses: @unrdf/oxigraph.dataFactory for quads
- Ensures: Append-only property for audit trail
```

#### graph-builder.mjs
```
Exports: class GraphBuilder
Methods:
- buildFromObservations(observations): Quads
- buildFromProbeResult(result): Quads
- buildFromMergeResult(merged): Quads

Process:
1. Transform domain object to RDF terms (namedNode, literal)
2. Generate triples: subject predicate object
3. Add provenance triples (source, timestamp)
4. Return array of quads

Integration:
- Uses: triple-generator for atomic triple creation
- Uses: namespaces for predicate URIs
- Uses: @unrdf/oxigraph.dataFactory for terms
```

#### triple-generator.mjs
```
Exports: class TripleGenerator
Methods:
- generateTriple(subject, predicate, object, graph): Quad
- generateProvenanceTriples(entity, source, timestamp): [Quad]
- generateMetadataTriples(entity, metadata): [Quad]

Utilities:
- slugify(string): Converts to safe URI component
- uriFromDomain(domain): Creates unique probe result URI
- timestampToLiteral(ns): Converts nanosecond timestamp to RDF literal

Pattern:
- All triples use canonical namespaces
- All triples are canonical (sorted S-P-O)
- All triples are quads (include graph term)
```

#### namespaces.mjs
```
Exports: RDFNamespaces object with URIs:
- kgc: http://kgc.io/ontology/
- probe: http://kgc.io/probe/
- result: http://kgc.io/result/
- security: http://kgc.io/probe/security/
- performance: http://kgc.io/probe/performance/
- ... (one per probe domain)
- rdf: http://www.w3.org/1999/02/22-rdf-syntax-ns#
- rdfs: http://www.w3.org/2000/01/rdf-schema#
- xsd: http://www.w3.org/2001/XMLSchema#

Pattern:
- Constants, no logic
- Used by all triple generators
- Extensible for custom domains
```

---

### 5.5 Orchestrator Directory

#### probe-orchestrator.mjs
```
Exports: class ProbeOrchestrator
Constructor:
- store: ProbeStore instance
- guards: GuardComposer (all guards)
- registry: ProbeRegistry

Methods:
- executeProbe(domain, observation): Promise<ProbeResult>
  - Load probe agent from registry
  - Call observation-guard
  - Call agent.execute()
  - Call result-guard
  - Append to store
  - Return result

- executeAllProbes(observations): Promise<ProbeResult[]>
  - Run all 10 probes in parallel (per observation)
  - Collect results
  - Return array

- mergeResults(results): Promise<MergedResult>
  - Calls: merge-engine.merge(results)
  - Calls: merge-guard
  - Returns: Aggregated result

Pattern:
- Coordination point for all probes
- Enforces guard policies
- Manages concurrency (Promise.all or sequential)
```

#### merge-engine.mjs
```
Exports: class MergeEngine
Constructor:
- config: MergeConfigSchema (strategy, weights, etc.)
- conflictResolver: ConflictResolver instance

Methods:
- merge(results): Promise<MergedResult>
  1. Group results by probe domain
  2. For each domain: Apply merge strategy (consensus, max, weighted_sum)
  3. Resolve conflicts via conflictResolver
  4. Aggregate overall score
  5. Return merged summary

Strategies:
- consensus: All same → use score; else → manual resolution
- max: Take highest score from any probe
- min: Take lowest score from any probe
- weighted_sum: score = sum(weight[i] * score[i]) / sum(weights)

Pattern:
- Stateless; pure function (config drives behavior)
- Deterministic (same inputs → same outputs)
- Auditable (all decisions logged)
```

#### conflict-resolver.mjs
```
Exports: class ConflictResolver
Methods:
- resolve(conflicts, strategy): Promise<ResolvedResult>
  Where conflicts: [{ domain, results: [ProbeResult] }]

Strategies:
- highest_confidence: Pick result with highest confidence metadata
- newest_first: Pick result with newest timestamp
- manual: Return conflict; require human review

Pattern:
- Called only when merge finds conflicts
- Returns augmented result with resolution method
- Logs conflict evidence for audit trail
```

#### aggregator.mjs
```
Exports: class Aggregator
Methods:
- aggregateResults(results): AggregatedSummary
  Summary structure:
  {
    overall_score: number (0.0-1.0),
    by_domain: { [domain]: score },
    assertions_passed: number,
    assertions_failed: number,
    assertions_warning: number,
    assertions_skipped: number,
    total_duration_ms: number,
    timestamp_ns: bigint,
  }

- getStatistics(results): Statistics
  Computes: mean, median, stddev, min, max of scores

Pattern:
- Produces summary for human consumption
- Feeds into receipt-builder
- Non-destructive (original results preserved)
```

---

### 5.6 CLI Directory

#### cli/index.mjs
```
Exports:
- registerProbeCommands(cli: CliRegistry): void
  - Registers all 4 commands with @unrdf/kgc-cli

Integration:
- Depends on: @unrdf/kgc-cli.registerCommand()
- Each command module exports: { command, handler }

Commands Registered:
- kgc probe run
- kgc probe validate
- kgc probe merge
- kgc probe export
```

#### run-probe.command.mjs
```
Exports: { command, handler }
Command: kgc probe run [domain]

Options:
--domain (required): security | performance | correctness | ...
--observations-file: Path to JSONL file
--output-format: json | jsonl | receipt
--parallel: true | false (default: true)

Handler:
1. Load observations from file
2. Validate with observation-guard
3. Call orchestrator.executeProbe()
4. Output result in requested format
5. Return exit code 0 (success) or 1 (failure)

Pattern:
- Entry point for users
- Error handling with user-friendly messages
```

#### validate-observation.command.mjs
```
Exports: { command, handler }
Command: kgc probe validate [file]

Options:
--file: Path to observation file (JSONL or JSON)
--strict: true | false (reject warnings as errors)

Handler:
1. Load file
2. Parse JSON
3. Call observation-guard.validate()
4. Report all violations
5. Return exit code 0 (valid) or 1 (invalid)

Pattern:
- Debugging utility
- Shows all validation errors
- Helps users fix observations before probe run
```

#### merge-results.command.mjs
```
Exports: { command, handler }
Command: kgc probe merge [results...]

Options:
--strategy: consensus | max | min | weighted_sum
--output-file: Path to write merged result
--weights: JSON object (domain -> weight)

Handler:
1. Load results from files
2. Validate with result-guard
3. Call merge-engine.merge()
4. Write to output file
5. Return exit code 0 or 1

Pattern:
- Useful for multi-run scenarios
- Allows strategy selection at CLI
```

#### export-receipt.command.mjs
```
Exports: { command, handler }
Command: kgc probe export [receipt-id]

Options:
--receipt-id: UUID of receipt to export
--verify: true | false (verify signature before export)
--format: json | jsonl | text

Handler:
1. Load receipt from store by receipt-id
2. Call receipt-guard.validateReceipt()
3. If --verify: Call receipt-guard.verifySentSignature()
4. Output in requested format
5. Return exit code

Pattern:
- Retrieves and displays stored receipts
- Optionally verifies merkle chain
- Supports multiple output formats
```

---

### 5.7 Receipts Directory

#### receipt-builder.mjs
```
Exports: class ReceiptBuilder
Constructor:
- Wraps @unrdf/v6-core.BaseReceipt
- Extends with merkle integration

Methods:
- build(aggregatedResult): Promise<Receipt>
  1. Create BaseReceipt from aggregated result
  2. Add probe-specific metadata
  3. Generate proof-of-work (hash puzzle)
  4. Call merkle-integrator.chain()
  5. Sign with RSA-4096 private key
  6. Return complete receipt

- sign(data, privateKey): Signature
  Uses RSA-4096 to sign receipt

Pattern:
- Extends @unrdf/v6-core.BaseReceipt
- Integrates with merkle-integrator for chaining
- Ensures receipt immutability
```

#### merkle-integrator.mjs
```
Exports: class MerkleIntegrator
Constructor:
- receiptChain: @unrdf/kgc-substrate.ReceiptChain
- hashAlgorithm: 'blake3' (default)

Methods:
- chain(receipt): Promise<ChainedReceipt>
  1. Append receipt to ReceiptChain
  2. Generate merkle path (hash(prev) -> hash(new))
  3. Compute merkle root
  4. Return receipt with merkle metadata

Integration:
- Uses: @unrdf/kgc-substrate.ReceiptChain
- Uses: @unrdf/v6-core.merkle/* for tree operations
- Ensures: All receipts linked in tamper-evident chain

Pattern:
- Stateful (maintains chain state)
- Deterministic (same input produces same merkle path)
```

#### verification.mjs
```
Exports: class ReceiptVerifier
Constructor:
- merkle-integrator: MerkleIntegrator instance

Methods:
- verify(receipt): Promise<VerificationResult>
  Checks:
  1. Receipt schema valid
  2. Signature valid (RSA-4096)
  3. Merkle root matches computed value
  4. Timestamp reasonable (not in future)
  5. State commitment matches proof-of-work

  Returns: { valid: boolean, errors: [string], warnings: [string] }

Pattern:
- Used by receipt-guard
- Called before receipt exits system
- Logs all verification steps
```

---

### 5.8 Utils Directory

#### logger.mjs
```
Exports:
- createLogger(options): Logger instance
- log levels: debug, info, warn, error
- OTEL instrumentation (metrics + traces)

Methods:
- debug(message, context): Detailed debugging
- info(message, context): Normal operations
- warn(message, context): Non-blocking issues
- error(message, context): Failures (stacktrace included)

OTEL Integration:
- Spans: One span per major operation
- Metrics: Counters (probes_run, errors_total), Histograms (probe_duration_ms)
- Context propagation: Trace ID threaded through async calls

Pattern:
- Uses pino for base logger
- Wraps with OTEL exporter
- No business logic; only instrumentation
```

#### error-handler.mjs
```
Exports:
- KGCProbeError(message, code, context): Error subclass
- ValidationError(message, errors): Validation-specific error
- handleError(error, context): Standardized error handling

Methods:
- formatError(error): Returns { code, message, details, context }
- isRecoverable(error): boolean
- retry(fn, options): Retry logic with exponential backoff

Pattern:
- Centralized error handling
- All guards throw KGCProbeError or ValidationError
- CLI catches and formats errors
```

#### types.mjs
```
JSDoc type definitions for:
- Observation: RDF term + metadata
- ProbeResult: Score, assertions, evidence
- MergedResult: Aggregated from multiple probes
- Receipt: Cryptographic proof
- GuardPolicy: Enforcement rules
- Various enums (domain, status, strategy)

Pattern:
- Pure type definitions
- No implementation
- Enables IDE autocomplete and type checking
```

---

## 6. Integration with Existing Packages

### 6.1 @unrdf/v6-core Integration

```javascript
// receipt-builder.mjs integrates:
import { BaseReceipt } from '@unrdf/v6-core/receipts/base-receipt.mjs';

export class ReceiptBuilder {
  async build(aggregatedResult) {
    // Create receipt extending BaseReceipt
    const receipt = new BaseReceipt({
      type: 'probe-result',
      payload: aggregatedResult,
      timestamp_ns: BigInt(Date.now()) * 1_000_000n,
    });

    // Add probe-specific metadata
    receipt.probe_domain = aggregatedResult.domain;
    receipt.score = aggregatedResult.score;

    // Extend with signature
    receipt.signature = await this.sign(receipt);

    return receipt;
  }
}
```

### 6.2 @unrdf/kgc-substrate Integration

```javascript
// probe-store.mjs wraps:
import { KnowledgeStore } from '@unrdf/kgc-substrate';

export class ProbeStore extends KnowledgeStore {
  async appendProbeResult(probeResult) {
    // Transform to RDF triples
    const { subject, predicate, object } = this._resultToTriple(probeResult);

    // Use parent's appendTriple method
    const { index } = await this.appendTriple('add', subject, predicate, object);

    return { index };
  }
}
```

### 6.3 @unrdf/oxigraph Integration

```javascript
// triple-generator.mjs uses:
import { dataFactory } from '@unrdf/oxigraph';

export class TripleGenerator {
  generateTriple(subject, predicate, object, graph = null) {
    // Create RDF terms using oxigraph dataFactory
    const s = dataFactory.namedNode(subject);
    const p = dataFactory.namedNode(predicate);
    const o = dataFactory.literal(object);
    const g = graph || dataFactory.defaultGraph();

    // Create quad
    return dataFactory.quad(s, p, o, g);
  }
}
```

### 6.4 @unrdf/kgc-cli Integration

```javascript
// cli/index.mjs registers:
import { registerCommand } from '@unrdf/kgc-cli';

export function registerProbeCommands(cliRegistry) {
  registerCommand(cliRegistry, {
    command: 'probe run [domain]',
    handler: runProbeHandler,
    description: 'Execute probe on domain',
  });
  // ... register other 3 commands
}
```

### 6.5 @unrdf/hooks Integration (Proposed)

```javascript
// guards/* implement GuardPolicy interface
import { GuardPolicy } from '@unrdf/hooks';

export class ObservationGuard implements GuardPolicy {
  validate(observation) {
    // Implement policy enforcement
  }
}

// guards/index.mjs composes:
export class GuardComposer {
  constructor() {
    this.guards = [
      new ObservationGuard(),
      new ResultGuard(),
      new GraphGuard(),
      new MergeGuard(),
      new ReceiptGuard(),
    ];
  }

  async enforceAll(stage, data) {
    // Run all guards for a stage
  }
}
```

### 6.6 @unrdf/yawl Integration (Optional)

```javascript
// orchestrator/probe-orchestrator.mjs can extend WorkflowBuilder
import { WorkflowBuilder, WorkflowEngine } from '@unrdf/yawl';

export class ProbeOrchestrator extends WorkflowBuilder {
  constructor(options) {
    super();

    // Define workflow steps as YAWL activities
    this.activity('validate-observations', observationGuard.validate);
    this.activity('run-probes', this.executeAllProbes);
    this.activity('merge-results', mergeEngine.merge);
    this.activity('generate-receipt', receiptBuilder.build);

    // Define routing (control flow)
    this.sequence([
      'validate-observations',
      'run-probes',
      'merge-results',
      'generate-receipt',
    ]);
  }
}
```

---

## 7. Dependency Matrix (Summary)

```
┌──────────────────────────┬─────────────────────────────────────────┐
│ Module                   │ External Dependencies                    │
├──────────────────────────┼─────────────────────────────────────────┤
│ schemas/*                │ zod                                      │
│ agents/*                 │ (none - pure logic)                      │
│ guards/*                 │ schemas/*, logger, error-handler         │
│ storage/*                │ @unrdf/oxigraph, @unrdf/kgc-substrate   │
│ orchestrator/*           │ agents/*, guards/*, storage/*, logger    │
│ cli/*                    │ @unrdf/kgc-cli, orchestrator/*           │
│ receipts/*               │ @unrdf/v6-core, @unrdf/kgc-substrate    │
│ utils/*                  │ pino, OTEL packages                      │
└──────────────────────────┴─────────────────────────────────────────┘
```

---

## 8. Deployment Architecture

### 8.1 Monorepo Integration

```
unrdf/
├── packages/
│   ├── core/              (existing)
│   ├── oxigraph/          (existing)
│   ├── v6-core/           (existing)
│   ├── kgc-substrate/     (existing)
│   ├── kgc-cli/           (existing)
│   └── kgc-probe/         (NEW)
│       ├── src/
│       │   ├── index.mjs
│       │   ├── schemas/
│       │   ├── agents/
│       │   ├── guards/
│       │   ├── storage/
│       │   ├── orchestrator/
│       │   ├── cli/
│       │   ├── receipts/
│       │   └── utils/
│       ├── test/          (Jest tests)
│       ├── package.json   (dependencies, exports)
│       └── README.md      (user documentation)
```

### 8.2 Build and Export Configuration

```json
{
  "name": "@unrdf/kgc-probe",
  "version": "1.0.0",
  "type": "module",
  "exports": {
    ".": "./src/index.mjs",
    "./schemas": "./src/schemas/index.mjs",
    "./agents": "./src/agents/index.mjs",
    "./guards": "./src/guards/index.mjs",
    "./storage": "./src/storage/index.mjs",
    "./orchestrator": "./src/orchestrator/index.mjs",
    "./cli": "./src/cli/index.mjs",
    "./receipts": "./src/receipts/index.mjs",
    "./utils": "./src/utils/index.mjs"
  },
  "dependencies": {
    "@unrdf/core": "*",
    "@unrdf/oxigraph": "*",
    "@unrdf/v6-core": "*",
    "@unrdf/kgc-substrate": "*",
    "@unrdf/kgc-cli": "*",
    "zod": "^3.22.0",
    "pino": "^8.0.0"
  }
}
```

---

## 9. Summary: Component Interaction Diagram

```
┌───────────────────────────────────────────────────────────────┐
│ USER / CLI LAYER                                              │
│ kgc probe run --domain security --observations obs.rdf       │
└───────────────────────┬─────────────────────────────────────┘
                        │
        ┌───────────────┴───────────────┐
        ▼                               ▼
    ┌────────────────┐         ┌───────────────────┐
    │ CLI/run-probe  │         │ observation-guard │ (guard)
    │ .command.mjs   │────────▶│ Validates input   │
    └────────────────┘         └────────┬──────────┘
                                        │ ✓ valid
                                        ▼
                          ┌──────────────────────────┐
                          │ probe-orchestrator       │ (orchestrator)
                          │ - Load probe agent       │
                          │ - Execute probe          │
                          │ - Collect result         │
                          └──────────────┬───────────┘
                                         │
                    ┌────────────────────┼────────────────────┐
                    ▼                    ▼                    ▼
              ┌──────────────┐   ┌──────────────┐    ┌──────────────┐
              │ SecurityProbe│   │PerfProbe     │ ..│ IntegrationP │
              │ .execute()   │   │.execute()    │    │.execute()    │
              └──────┬───────┘   └──────┬───────┘    └──────┬───────┘
                     │                  │                   │
                     └──────────┬───────┴───────────┬───────┘
                                │
                                ▼
                        ┌───────────────────┐
                        │ result-guard      │ (guard)
                        │ Validates result  │
                        └────────┬──────────┘
                                 │ ✓ valid
                                 ▼
                        ┌───────────────────┐
                        │ graph-builder     │ (storage)
                        │ Quads ← Results   │
                        └────────┬──────────┘
                                 │
                                 ▼
                        ┌───────────────────┐
                        │ graph-guard       │ (guard)
                        │ Validates quads   │
                        └────────┬──────────┘
                                 │ ✓ valid
                                 ▼
                        ┌───────────────────┐
                        │ probe-store       │ (storage)
                        │ Append to RDF log │
                        └────────┬──────────┘
                                 │
                                 ▼
                        ┌───────────────────┐
                        │ merge-engine      │ (orchestrator)
                        │ Aggregate results │
                        └────────┬──────────┘
                                 │
                                 ▼
                        ┌───────────────────┐
                        │ merge-guard       │ (guard)
                        │ Validate merge    │
                        └────────┬──────────┘
                                 │ ✓ valid
                                 ▼
                        ┌───────────────────┐
                        │ receipt-builder   │ (receipts)
                        │ Create receipt    │
                        └────────┬──────────┘
                                 │
                                 ▼
                        ┌───────────────────┐
                        │ merkle-integrator │ (receipts)
                        │ Chain + sign      │
                        └────────┬──────────┘
                                 │
                                 ▼
                        ┌───────────────────┐
                        │ receipt-guard     │ (guard)
                        │ Validate receipt  │
                        └────────┬──────────┘
                                 │ ✓ valid
                                 ▼
┌───────────────────────────────────────────────────────────────┐
│ OUTPUT / RECEIPT                                              │
│ {                                                             │
│   "receipt_id": "rcpt-2025-12-27-001",                       │
│   "domain": "security",                                       │
│   "score": 0.92,                                              │
│   "merkle_root": "h(...)",                                    │
│   "signature": "sig(...)"                                     │
│ }                                                             │
└───────────────────────────────────────────────────────────────┘
```

---

## 10. Next Steps (Implementation Roadmap)

**Phase 1: Foundation (Week 1)**
- [ ] Create package structure + package.json
- [ ] Implement schemas/ (Zod validators)
- [ ] Implement utils/ (logger, error-handler)
- [ ] Create test suite structure

**Phase 2: Core Layers (Week 2)**
- [ ] Implement guards/ (5 guard classes)
- [ ] Implement storage/ (probe-store, graph-builder, triple-generator)
- [ ] Add unit tests for guards and storage

**Phase 3: Agents (Week 3)**
- [ ] Implement agents/index.mjs (registry)
- [ ] Implement 10 probe agents (security, performance, ...)
- [ ] Add unit tests for each agent

**Phase 4: Orchestration (Week 4)**
- [ ] Implement orchestrator/ (probe-orchestrator, merge-engine, aggregator)
- [ ] Integration tests across layers
- [ ] Add integration with guard composition

**Phase 5: Receipts & CLI (Week 5)**
- [ ] Implement receipts/ (receipt-builder, merkle-integrator, verification)
- [ ] Implement cli/ (command registration and handlers)
- [ ] Integration with @unrdf/kgc-cli

**Phase 6: Validation & Documentation (Week 6)**
- [ ] Full integration tests
- [ ] OTEL instrumentation validation
- [ ] User documentation + examples
- [ ] Publish to npm

---

## Appendix: File Template Snippets

### Template: New Probe Agent

```javascript
/**
 * @unrdf/kgc-probe - [Domain] Probe
 *
 * Validates [domain] aspects of knowledge graphs.
 */

/**
 * @typedef {Object} [Domain]ProbeConfig
 * @property {number} timeout_ms - Max execution time
 * @property {boolean} detailed_evidence - Include detailed findings
 */

/**
 * [Domain] Probe - Validates [domain] properties
 */
export class [Domain]Probe {
  /** @type {string} */
  static domain = '[domain]';

  /** @type {string} */
  static name = '[Domain] Probe';

  /**
   * Execute probe on observation
   * @param {Observation} observation - Input observation
   * @param {[Domain]ProbeConfig} [config] - Configuration
   * @returns {Promise<ProbeResult>} Probe result with score and assertions
   */
  async execute(observation, config = {}) {
    try {
      // 1. Validate input
      const validated = ObservationSchema.parse(observation);

      // 2. Run validation logic
      const assertions = await this._validate(validated);

      // 3. Calculate score
      const passed = assertions.filter(a => a.status === 'pass').length;
      const score = assertions.length > 0 ? passed / assertions.length : 1.0;

      // 4. Create result
      return {
        probe_id: crypto.randomUUID(),
        domain: [Domain]Probe.domain,
        status: score >= 0.8 ? 'pass' : score >= 0.5 ? 'warning' : 'fail',
        score,
        assertions,
        duration_ms: Date.now() - startTime,
        timestamp_ns: BigInt(Date.now()) * 1_000_000n,
        metadata: {},
      };
    } catch (error) {
      return {
        domain: [Domain]Probe.domain,
        status: 'fail',
        score: 0.0,
        assertions: [],
        error: error.message,
      };
    }
  }

  /**
   * Run [domain] validation
   * @private
   */
  async _validate(observation) {
    const assertions = [];

    // Add validation logic here
    assertions.push({
      id: '[domain]-001',
      status: 'pass',
      evidence: 'Details...',
      weight: 1.0,
    });

    return assertions;
  }
}
```

---

**END OF ARCHITECTURE DESIGN DOCUMENT**
