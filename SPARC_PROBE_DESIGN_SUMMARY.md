# SPARC Pseudocode: KGC Probe Package Design Summary

## Overview

This document provides a high-level summary of the complete RDF/Oxigraph design for the KGC probe package, with references to detailed pseudocode documents.

---

## DELIVERABLES

Three comprehensive pseudocode design documents have been created:

### 1. SPARC_RDF_SCHEMA.md (Core Design)

**Location**: `/home/user/unrdf/SPARC_RDF_SCHEMA.md`

**Contents**:

- **Part 1**: Ontology prefixes and namespace definitions
  - Core classes: Observation, Event, SystemConfig, Capability, Domain, Conflict
  - 15+ properties with semantic definitions
  - RDF/RDFS/XSD/OWL references

- **Part 2**: Graph structures
  - **ProbeUniverse**: Materialized observations with domains and capabilities
    - Input: Observation objects
    - Schema: probe:Observation with properties
    - Canonicalization: Deterministic URI generation

  - **ProbeEventLog**: Append-only audit trail
    - Schema: probe:Event with timestamp_ns, logIndex, stateHashBefore/After
    - Immutability contract: Sequential indices, no deletion

  - **ProbeSystem**: System configuration and allowlists
    - Schema: probe:SystemConfig with RDF lists
    - Update pattern: Replace entire config atomically

- **Part 3**: KnowledgeStore integration
  - ProbeGraphManager class interface
  - Deterministic snapshot generation
  - State hash verification

- **Part 4**: SPARQL query patterns (6 queries)
  - Find capabilities in domain
  - Detect conflicts between agents
  - Timeline of changes (versioning)
  - Agent observation history
  - System configuration validation
  - State hash verification chain

- **Part 5**: Complexity analysis
  - Time/space complexity for each operation
  - Index recommendations
  - Performance characteristics

- **Part 6**: Design patterns
  - Canonical quad ordering
  - Immutable append-only log
  - Deterministic URI generation
  - RDF list configuration

- **Part 7**: Integration with KnowledgeStore
- **Part 8**: File paths and code organization

---

### 2. SPARC_RDF_ALGORITHMS.md (Detailed Algorithms)

**Location**: `/home/user/unrdf/SPARC_RDF_ALGORITHMS.md`

**Algorithms**:

1. **ObservationToQuads** - Convert Observation object to N3 quads
   - Input: Observation + KnowledgeStore
   - Output: {uri, indices, timestamp_ns, quadCount}
   - Time: O(k) where k = capabilities
   - Validates all required fields
   - Generates deterministic URIs
   - Creates 8+ quads per observation

2. **QueryCapabilitiesByDomain** - Retrieve capabilities in domain
   - Input: store, domainName
   - Output: {domain, capabilities}
   - Time: O(n) where n = observations
   - Follows graph links: domain -> hasCapability -> capability
   - Includes error handling for missing domains

3. **AppendEventToLog** - Add immutable event record
   - Input: store, operationType, agentId, payload
   - Output: {eventUri, logIndex, stateHashAfter}
   - Time: O(1) - constant quads per event
   - Enforces sequential log indices
   - Records state hash chain

4. **VerifyStateHashChain** - Cryptographic verification
   - Input: store
   - Output: {isValid, errors, chainLength}
   - Time: O(e) where e = events
   - Checks sequential indices
   - Verifies hash continuity

5. **QueryConflictingObservations** - Find conflicting claims
   - Input: store, claimHash (optional), agentA, agentB
   - Output: Array<{obsA, agentA, obsB, agentB, domain}>
   - Time: O(c) where c = conflicts
   - Supports optional filters

6. **TimelineOfChanges** - Get event history
   - Input: store, startIndex, endIndex, limit
   - Output: Array of events in order
   - Time: O(e) where e = events
   - Supports pagination with limit

---

### 3. SPARC_KNOWLEDGE_STORE_INTEGRATION.md (Integration Patterns)

**Location**: `/home/user/unrdf/SPARC_KNOWLEDGE_STORE_INTEGRATION.md`

**Contents**:

- **Part 1**: Storage architecture
  - Three-graph model with single KnowledgeStore
  - StateCommitment structure

- **Part 2**: Materialize observations to KnowledgeStore
  - High-level flow
  - Usage patterns

- **Part 3**: Generate deterministic snapshots
  - Snapshot generation algorithm
  - Snapshot verification
  - N-Quads serialization

- **Part 4**: Recover from snapshots
  - Snapshot recovery algorithm
  - Parsing N-Quads
  - Verification after recovery

- **Part 5**: State hash tracking
  - Track state transitions
  - Replay state to version
  - Chain verification

- **Part 6**: Testing and validation
  - Comprehensive integrity test
  - Quad count consistency
  - Snapshot generation as validation

- **Part 7**: Module integration pattern
  - ProbeGraphManager class with 11 methods
  - All operations integrated with KnowledgeStore

---

## KEY DESIGN DECISIONS

### 1. Three Separate Named Graphs

| Graph | Purpose | Mutability | Quads | Access Pattern |
|-------|---------|-----------|-------|-----------------|
| ProbeUniverse | Current O state | Read-only materialize | N | Query by domain/agent |
| ProbeEventLog | Audit trail | Append-only | E | Sequential scan |
| ProbeSystem | Configuration | Replace atomic | S | Direct read |

### 2. Canonical Quad Ordering

All quads are lexicographically sorted (Subject, Predicate, Object, Graph) to ensure:
- Deterministic hashing
- Same data → same hash
- Reproducible snapshots

### 3. Immutable Append-Only Log

ProbeEventLog enforces:
- Sequential log indices (0, 1, 2, ...)
- State hash chain (event[i].after == event[i+1].before)
- Never delete or modify past events
- Timestamp ordering (monotonically increasing)

### 4. Deterministic URIs

All observations and events get URIs based on content hash:
```
observation URI = http://unrdf.org/kgc/probe/observation#{SHA256(canonical_json)}
event URI = http://unrdf.org/kgc/probe/event#{SHA256(operation|agent|index|timestamp)}
```

This ensures:
- Same data always gets same URI
- Idempotent: materializing same observation twice is safe
- Replayable: recover snapshots deterministically

### 5. State Hash Chain

Each event records:
- `stateHashBefore`: State before operation
- `stateHashAfter`: State after operation

This creates a cryptographic chain where:
```
event[0].after ← event[0].before (modified by event[0])
event[1].after ← event[0].after (initial state for event[1])
event[1].after ← event[1].after (modified by event[1])
...
```

Verifying: `event[i].after == event[i+1].before` for all i

---

## INTEGRATION WITH EXISTING PACKAGES

### @unrdf/oxigraph

**Usage**:
```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';

// Create store
const store = createStore();

// Create terms
const subject = dataFactory.namedNode('http://example.org/obs#1');
const predicate = dataFactory.namedNode('http://unrdf.org/probe/ontology/timestamp_ns');
const object = dataFactory.literal('1735366800000000000', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#long'));
const graph = dataFactory.namedNode('http://unrdf.org/kgc/probe/ProbeUniverse');

// Create quad
const quad = dataFactory.quad(subject, predicate, object, graph);

// Add to store
store.add(quad);
```

**In Pseudocode**:
- Used in ObservationToQuads algorithm
- Used in all query algorithms
- Used in snapshot serialization

### @unrdf/kgc-substrate (KnowledgeStore)

**Usage**:
```javascript
import { KnowledgeStore } from '@unrdf/kgc-substrate';

// Create store
const store = new KnowledgeStore({ nodeId: 'probe-agent-1' });

// Append triple (from pseudocode appendTriple)
const { index, timestamp_ns } = await store.appendTriple(
    'add',
    subject,
    predicate,
    object,
    graph
);

// Get commitment (for state hash)
const commitment = await store.getStateCommitment();

// Generate snapshot
const snapshot = await store.generateSnapshot();
```

**In Pseudocode**:
- All append operations use store.appendTriple()
- State commitment used in VerifyStateHashChain
- Snapshots used in GenerateDeterministicProbeSnapshot

### @unrdf/kgc-4d

**Usage** (internal to KnowledgeStore):
- `KGCStore`: Event logging backend
- `freezeUniverse`: Snapshot generation
- `GitBackbone`: Immutable storage

---

## ALGORITHM COMPLEXITY MATRIX

| Operation | Time | Space | Parallelizable | Blocking |
|-----------|------|-------|-----------------|----------|
| MaterializeObservation | O(k) | O(k) | No (append-only) | Yes (log index) |
| QueryCapabilities | O(n) | O(m) | Yes | No |
| AppendEvent | O(1) | O(1) | No (sequential) | Yes (log index) |
| VerifyHashChain | O(e) | O(e) | No | No |
| FindConflicts | O(c) | O(k) | Yes | No |
| GetTimeline | O(e) | O(min(e,L)) | Yes | No |
| GenerateSnapshot | O(n+e+s) | O(n+e+s) | No (must be atomic) | Yes |
| RecoverSnapshot | O(n+e+s) | O(n+e+s) | No (must be sequential) | Yes |

---

## PSEUDOCODE FILE ORGANIZATION

```
/home/user/unrdf/
├── SPARC_RDF_SCHEMA.md                    // Part 1: Core design
├── SPARC_RDF_ALGORITHMS.md                // Part 2: Detailed algorithms
├── SPARC_KNOWLEDGE_STORE_INTEGRATION.md   // Part 3: Integration patterns
└── SPARC_PROBE_DESIGN_SUMMARY.md          // This file

packages/kgc-probe/ (FUTURE IMPLEMENTATION)
├── src/
│   ├── index.mjs                          // Main export
│   ├── ontology.mjs                       // Prefix definitions
│   ├── ProbeGraphManager.mjs              // Main interface class
│   ├── graphs/
│   │   ├── ProbeUniverse.mjs              // Observation materialization
│   │   ├── ProbeEventLog.mjs              // Event log operations
│   │   └── ProbeSystem.mjs                // Config management
│   ├── algorithms/
│   │   ├── observation-to-quads.mjs       // ObservationToQuads
│   │   ├── query-capabilities.mjs         // QueryCapabilitiesByDomain
│   │   ├── append-event.mjs               // AppendEventToLog
│   │   ├── verify-chain.mjs               // VerifyStateHashChain
│   │   ├── find-conflicts.mjs             // QueryConflictingObservations
│   │   └── timeline.mjs                   // TimelineOfChanges
│   ├── store-integration/
│   │   ├── materialize.mjs                // MaterializeObservationToKnowledgeStore
│   │   ├── snapshot.mjs                   // GenerateDeterministicProbeSnapshot
│   │   ├── recovery.mjs                   // RecoverProbeGraphsFromSnapshot
│   │   └── state-tracking.mjs             // State hash tracking
│   └── validation/
│       ├── snapshot-verify.mjs            // VerifySnapshot
│       └── integrity-test.mjs             // ValidateProbeStoreIntegrity
├── __tests__/
│   ├── graphs.test.mjs
│   ├── algorithms.test.mjs
│   ├── snapshot.test.mjs
│   ├── recovery.test.mjs
│   └── integration.test.mjs
├── examples/
│   ├── basic-observation.mjs
│   ├── query-capabilities.mjs
│   ├── event-log.mjs
│   ├── snapshot-generation.mjs
│   └── sparql-queries.mjs
└── README.md
```

---

## VERIFICATION STRATEGY

### Unit-Level Testing

Each algorithm should have tests covering:

```
Algorithm: ObservationToQuads
Tests:
  ✓ Valid observation with 0 capabilities
  ✓ Valid observation with 5 capabilities
  ✓ Null observation (should throw)
  ✓ Missing agentId (should throw)
  ✓ Invalid timestamp_ns (should throw)
  ✓ Idempotence (same observation → same quads)
  ✓ Determinism (same observation → same URI)
  Complexity:
    ✓ Time O(k) verified with N=1000, k=0..100
    ✓ Space O(k) verified with profiling
```

### Integration-Level Testing

```
Test: Full observation flow
  1. Create observation
  2. Materialize to store
  3. Query by domain
  4. Check event in log
  5. Verify state hash
  6. Generate snapshot
  7. Recover from snapshot
  8. Verify integrity
```

### Property-Based Testing

```
Property 1: Idempotence
  For all observations O:
    materialize(O) == materialize(O) // Same result

Property 2: Immutability
  For all events E1, E2 with indices i < j:
    E1.logIndex < E2.logIndex &&
    E1.timestamp_ns < E2.timestamp_ns

Property 3: Chain Validity
  For all i:
    events[i].stateHashAfter == events[i+1].stateHashBefore

Property 4: Recoverability
  For all snapshots S:
    snapshot(restore(S)) == S
```

---

## KNOWN COMPLEXITY HOTSPOTS

### 1. VerifyStateHashChain - O(e)

**Issue**: Must verify all events in log

**Mitigation**:
- Index on logIndex (for fast lookup)
- Batch verification with LIMIT/OFFSET
- Incremental verification (only verify new events)

### 2. QueryConflictingObservations - O(c)

**Issue**: Must scan all conflict edges

**Mitigation**:
- Index on probe:conflictsWith
- Cache conflict results
- Periodic conflict detection (not real-time)

### 3. GenerateSnapshot - O(n+e+s)

**Issue**: Must serialize entire graph state

**Mitigation**:
- Batched snapshots (only after N events)
- Incremental snapshots (delta from last)
- Parallel graph serialization

---

## FUTURE ENHANCEMENTS

### Phase 2: Query Optimization

- [ ] SPARQL query planner
- [ ] Graph statistics and cardinality estimation
- [ ] Query result caching
- [ ] Full-text search on evidence

### Phase 3: Scalability

- [ ] Sharded graph storage (multiple stores)
- [ ] Horizontal scaling (multiple nodes)
- [ ] Compression of old events
- [ ] Archive/cold storage for old snapshots

### Phase 4: Advanced Features

- [ ] Conflict resolution strategies
- [ ] Capability negotiation between agents
- [ ] Policy enforcement via RDF rules
- [ ] Explanations/tracing of conflicts

---

## CRITICAL INVARIANTS (MUST MAINTAIN)

1. **Canonical Ordering**
   ```
   sort(quads) always produces same order
   hash(quads) is deterministic
   ```

2. **Sequential Log Indices**
   ```
   for all i: event[i].logIndex == i
   no gaps, no reordering
   ```

3. **State Hash Chain**
   ```
   for all i: event[i].stateHashAfter == event[i+1].stateHashBefore
   chain is unbroken and tamper-evident
   ```

4. **Immutability**
   ```
   event log can only append, never modify/delete
   history is permanent
   ```

5. **Determinism**
   ```
   same observation always produces same URI
   same events always produce same snapshot hash
   replay always produces identical state
   ```

---

## DEPENDENCIES AND INTEGRATION

### Direct Dependencies

```
@unrdf/kgc-probe (this package)
├── @unrdf/kgc-substrate     (KnowledgeStore)
│   ├── @unrdf/kgc-4d        (KGCStore, freezeUniverse)
│   │   └── @unrdf/core
│   └── @unrdf/oxigraph      (SPARQL engine)
│       └── oxigraph (npm)
└── @unrdf/oxigraph          (dataFactory)
    └── oxigraph (npm)
```

### No Circular Dependencies

ProbeGraphManager depends on KnowledgeStore but NOT vice versa.

### Isolation

- All RDF operations use @unrdf/oxigraph (never native 'n3' package)
- All storage uses KnowledgeStore (no direct file I/O)
- All hashing uses BLAKE3 (via KnowledgeStore)

---

## SUMMARY CHECKLIST

**Pseudocode Complete**:
- [x] Three RDF graphs designed
- [x] Ontology with 15+ properties
- [x] 6 detailed algorithms with pseudocode
- [x] 6 SPARQL query patterns
- [x] Complexity analysis (time/space)
- [x] KnowledgeStore integration
- [x] Snapshot generation/recovery
- [x] State hash chain verification
- [x] Conflict detection
- [x] Event timeline tracking

**Ready for Implementation**:
- [x] All algorithms in detailed pseudocode
- [x] Clear input/output contracts
- [x] Error handling specified
- [x] Complexity documented
- [x] Dependencies mapped
- [x] File organization planned
- [x] Test strategy defined

**Files Generated**:
- [x] `/home/user/unrdf/SPARC_RDF_SCHEMA.md` (580+ lines)
- [x] `/home/user/unrdf/SPARC_RDF_ALGORITHMS.md` (620+ lines)
- [x] `/home/user/unrdf/SPARC_KNOWLEDGE_STORE_INTEGRATION.md` (540+ lines)
- [x] `/home/user/unrdf/SPARC_PROBE_DESIGN_SUMMARY.md` (this file, 400+ lines)

---

## NEXT STEPS FOR IMPLEMENTATION

1. **Create @unrdf/kgc-probe package structure**
   - Initialize package.json with oxigraph, kgc-substrate dependencies
   - Create src/ and __tests__/ directories

2. **Implement core algorithms**
   - Start with ObservationToQuads (minimal dependencies)
   - Test with unit tests for each algorithm
   - Build up to full integration

3. **Implement ProbeGraphManager**
   - Class wrapping KnowledgeStore
   - All 11 methods from pseudocode
   - Integration tests for full workflows

4. **Run OTEL validation**
   - Verify all SPARQL queries execute correctly
   - Measure actual vs. theoretical complexity
   - Benchmark against targets

5. **Document public API**
   - JSDoc comments
   - README with examples
   - Integration guide for probe agents

---

## CONTACT & REFERENCES

**Pseudocode Documents**:
- See `/home/user/unrdf/SPARC_RDF_SCHEMA.md`
- See `/home/user/unrdf/SPARC_RDF_ALGORITHMS.md`
- See `/home/user/unrdf/SPARC_KNOWLEDGE_STORE_INTEGRATION.md`

**Existing Substrate Packages**:
- `@unrdf/oxigraph` - SPARQL engine
- `@unrdf/kgc-substrate` - Deterministic KnowledgeStore
- `@unrdf/kgc-4d` - Event logging backend

**RDF Standards**:
- [N-Quads](https://www.w3.org/TR/n-quads/)
- [SPARQL 1.1](https://www.w3.org/TR/sparql11-query/)
- [RDF 1.1](https://www.w3.org/TR/rdf11-concepts/)

---

**Generated**: 2025-12-27
**Format**: SPARC Pseudocode (Language Agnostic)
**Status**: Ready for Implementation Phase
