# SPARC Pseudocode Index: KGC Probe Package

## Overview

This index provides a complete guide to the SPARC pseudocode documentation for the KGC probe package. All documents are written in language-agnostic pseudocode suitable for implementation in JavaScript, Python, Go, or any other language.

---

## DOCUMENT MANIFEST

### Document 1: SPARC_RDF_SCHEMA.md (43 KB, 1,200+ lines)

**Location**: `/home/user/unrdf/SPARC_RDF_SCHEMA.md`

**Purpose**: Define complete RDF schema and graph structures

**Sections**:
1. Ontology prefixes (7 namespaces)
2. Graph structures (3 named graphs with 50+ quads examples)
3. KnowledgeStore integration (ProbeGraphManager design)
4. SPARQL query patterns (6 queries)
5. Complexity analysis (time/space for all operations)
6. Design patterns (canonical ordering, immutable log, etc.)
7. Integration with KnowledgeStore substrate
8. File paths and code organization

**Key Artifacts**:
- kgc:ProbeUniverse (materialized observations)
- kgc:ProbeEventLog (append-only events)
- kgc:ProbeSystem (configuration)
- Ontology with 15+ properties

**Algorithms Referenced**:
- MaterializeObservationToQuads (outlined)
- AppendEventToLog (outlined)
- GenerateDeterministicSnapshot (outlined)

**Complexity Results**:
- MaterializeObservation: O(k) where k = capabilities
- QueryCapabilities: O(n) where n = observations
- AppendEvent: O(1)
- QueryConflicts: O(c) where c = conflicts
- VerifyEventLog: O(e) where e = events
- TimelineQuery: O(e) with pagination

---

### Document 2: SPARC_RDF_ALGORITHMS.md (34 KB, 1,100+ lines)

**Location**: `/home/user/unrdf/SPARC_RDF_ALGORITHMS.md`

**Purpose**: Provide detailed pseudocode for all core algorithms

**Algorithms** (6 total with full pseudocode):

1. **ObservationToQuads** (200 lines)
   - Converts Observation object to canonical quads
   - Input: observation object + KnowledgeStore
   - Output: {uri, indices, timestamp_ns, quadCount}
   - Validates all required fields
   - Generates deterministic URIs from claim hash
   - Creates 8+ quads with properties

2. **QueryCapabilitiesByDomain** (150 lines)
   - Retrieves all capabilities in a domain
   - Input: store + domainName
   - Output: {domain, capabilities, count}
   - Follows graph edges: domain -> hasCapability -> capability
   - Error handling for missing domains

3. **AppendEventToLog** (200 lines)
   - Adds immutable event record to ProbeEventLog
   - Input: store + operationType + agentId + payload
   - Output: {eventUri, logIndex, stateHashAfter}
   - Enforces sequential log indices
   - Records state hash chain (before/after)
   - Validates operation type

4. **VerifyStateHashChain** (180 lines)
   - Cryptographically verifies event log integrity
   - Input: store
   - Output: {isValid, errors, chainLength}
   - Checks sequential indices (0, 1, 2, ...)
   - Verifies hash continuity (event[i].after == event[i+1].before)
   - Checks timestamp ordering

5. **QueryConflictingObservations** (130 lines)
   - Finds conflicting observations between agents
   - Input: store + optional filters
   - Output: Array<{obsA, agentA, obsB, agentB, domain}>
   - Supports filtering by claimHash, agentA, agentB
   - Error handling for missing properties

6. **TimelineOfChanges** (150 lines)
   - Retrieves events in chronological order
   - Input: store + optional startIndex/endIndex + limit
   - Output: Sorted array of events
   - Supports pagination
   - Includes state hash transitions

**Additional Subroutines**:
- CanonicalizeObservation (JSON canonicalization)
- CreateDeterministicUri (SHA256-based)
- SortObjectByKeys (lexicographic ordering)
- CompareTerms (RDF term comparison)

**Complexity Summary Table**:
- All algorithms documented with time/space complexity
- Error conditions specified
- Invariants documented

---

### Document 3: SPARC_KNOWLEDGE_STORE_INTEGRATION.md (26 KB, 900+ lines)

**Location**: `/home/user/unrdf/SPARC_KNOWLEDGE_STORE_INTEGRATION.md`

**Purpose**: Show how RDF graphs integrate with KnowledgeStore

**Sections**:

1. **Storage Architecture** (50 lines)
   - Three-graph model with single KnowledgeStore
   - Data flow diagram
   - StateCommitment structure

2. **Materialize Observations** (100 lines)
   - MaterializeObservationToKnowledgeStore algorithm
   - High-level flow with KnowledgeStore.appendTriple()
   - Example usage patterns

3. **Generate Deterministic Snapshots** (200 lines)
   - GenerateDeterministicProbeSnapshot algorithm
   - Graph-specific hashing
   - N-Quads serialization
   - Snapshot verification
   - VerifySnapshot algorithm with checks

4. **Recover from Snapshots** (150 lines)
   - RecoverProbeGraphsFromSnapshot algorithm
   - Parse N-Quads format
   - Quad reconstruction
   - Integrity checks after recovery

5. **State Hash Tracking** (120 lines)
   - GetStateHashHistory algorithm
   - ReplayStateToVersion algorithm
   - Hash chain verification

6. **Testing and Validation** (100 lines)
   - ValidateProbeStoreIntegrity algorithm
   - Comprehensive checks
   - Quad count verification

7. **Module Integration** (80 lines)
   - ProbeGraphManager class with 11 methods
   - Integration with KnowledgeStore
   - Method signatures

---

### Document 4: SPARC_PROBE_DESIGN_SUMMARY.md (17 KB, 600+ lines)

**Location**: `/home/user/unrdf/SPARC_PROBE_DESIGN_SUMMARY.md`

**Purpose**: High-level summary and navigation guide

**Sections**:
1. Deliverables overview
2. Key design decisions
3. Integration with existing packages
4. Algorithm complexity matrix
5. Pseudocode file organization (future implementation)
6. Verification strategy
7. Known complexity hotspots
8. Future enhancements
9. Critical invariants
10. Dependencies and isolation
11. Summary checklist

**Reference Materials**:
- Dependency graph (@unrdf/oxigraph, @unrdf/kgc-substrate)
- File organization for future implementation
- Testing strategy checklist
- Property-based testing specifications

---

## NAVIGATION GUIDE

### By Task

**I want to understand the overall design**
→ Read: SPARC_PROBE_DESIGN_SUMMARY.md (20 min)

**I want to implement the RDF schema**
→ Read: SPARC_RDF_SCHEMA.md Part 1-2 (30 min)

**I want to implement an algorithm**
→ Read: SPARC_RDF_ALGORITHMS.md Algorithm N (10 min per algorithm)

**I want to integrate with KnowledgeStore**
→ Read: SPARC_KNOWLEDGE_STORE_INTEGRATION.md Part 2-3 (25 min)

**I want to verify implementation correctness**
→ Read: SPARC_KNOWLEDGE_STORE_INTEGRATION.md Part 6 (15 min)

---

### By Algorithm

| Algorithm | Schema Doc | Algorithms Doc | Integration Doc |
|-----------|-----------|-----------------|-----------------|
| ObservationToQuads | Yes | Yes (200 lines) | Yes (Part 2) |
| QueryCapabilities | Yes | Yes (150 lines) | Yes (Part 3) |
| AppendEventToLog | Yes | Yes (200 lines) | Yes (Part 2) |
| VerifyHashChain | Yes | Yes (180 lines) | Yes (Part 5) |
| FindConflicts | Yes | Yes (130 lines) | Yes (Part 2) |
| GetTimeline | Yes | Yes (150 lines) | Yes (Part 4) |
| GenerateSnapshot | Yes | - | Yes (Part 3, 200 lines) |
| RecoverSnapshot | Yes | - | Yes (Part 4, 150 lines) |

---

### By Operation

| Operation | Document | Section |
|-----------|----------|---------|
| Create observation quads | Algorithms | Algorithm 1 |
| Query by domain | Algorithms | Algorithm 2 |
| Add event to log | Algorithms | Algorithm 3 |
| Verify integrity | Algorithms | Algorithm 4 |
| Find conflicts | Algorithms | Algorithm 5 |
| Get history | Algorithms | Algorithm 6 |
| Generate snapshot | Integration | Part 3 |
| Recover snapshot | Integration | Part 4 |
| Track state | Integration | Part 5 |
| Test integrity | Integration | Part 6 |

---

## KEY CONCEPTS

### Three RDF Graphs

**ProbeUniverse** (Observation Snapshot)
- Named graph: `http://unrdf.org/kgc/probe/ProbeUniverse`
- Contains: Current O observations materialized as quads
- Access: Query by domain, agent, claimHash
- Operations: Read-only (recreated per snapshot)

**ProbeEventLog** (Immutable Audit Trail)
- Named graph: `http://unrdf.org/kgc/probe/ProbeEventLog`
- Contains: All system events with state hashes
- Access: Sequential scan by logIndex
- Operations: Append-only (never modify/delete)

**ProbeSystem** (Configuration)
- Named graph: `http://unrdf.org/kgc/probe/ProbeSystem`
- Contains: System config, allowlists, policies
- Access: Direct read
- Operations: Atomic replacement

### Core Invariants

**Determinism**
```
hash(materialize(obs1)) = hash(materialize(obs1))
// Same input always produces same hash
```

**Immutability**
```
for all i < j: event[i].timestamp < event[j].timestamp
// Events ordered, never modified
```

**State Hash Chain**
```
event[i].stateHashAfter == event[i+1].stateHashBefore
// Cryptographic continuity
```

**Canonicalization**
```
sort(quads_representation1) == sort(quads_representation2)
// Independent of serialization order
```

---

## IMPLEMENTATION CHECKLIST

### Phase 1: Core Package Setup
- [ ] Create @unrdf/kgc-probe package
- [ ] Add oxigraph and kgc-substrate dependencies
- [ ] Create src/, __tests__, examples directories

### Phase 2: Ontology and Graphs
- [ ] Implement ontology.mjs (prefixes)
- [ ] Implement ProbeUniverse.mjs
- [ ] Implement ProbeEventLog.mjs
- [ ] Implement ProbeSystem.mjs

### Phase 3: Core Algorithms
- [ ] Implement ObservationToQuads
- [ ] Implement QueryCapabilitiesByDomain
- [ ] Implement AppendEventToLog
- [ ] Implement VerifyStateHashChain

### Phase 4: Advanced Queries
- [ ] Implement QueryConflictingObservations
- [ ] Implement TimelineOfChanges
- [ ] Implement SPARQL query runners

### Phase 5: Integration
- [ ] Implement ProbeGraphManager
- [ ] Integrate with KnowledgeStore
- [ ] Implement snapshot generation/recovery
- [ ] Implement state tracking

### Phase 6: Validation & Testing
- [ ] Unit tests for all algorithms
- [ ] Integration tests (full workflows)
- [ ] Property-based testing
- [ ] OTEL validation (80+/100 score)

### Phase 7: Documentation
- [ ] API documentation
- [ ] Usage examples
- [ ] Integration guide
- [ ] Performance tuning guide

---

## COMPLEXITY REFERENCE

### Time Complexity

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| MaterializeObservation | O(k) | k = capabilities |
| QueryCapabilities | O(n) | n = observations |
| AppendEvent | O(1) | Constant quads |
| VerifyHashChain | O(e) | e = events (must check all) |
| FindConflicts | O(c) | c = conflict edges |
| GetTimeline | O(e) | e = events, with limit |
| GenerateSnapshot | O(n+e+s) | All graphs |
| RecoverSnapshot | O(n+e+s) | Sequential append |

### Space Complexity

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| MaterializeObservation | O(k) | Quads array |
| QueryCapabilities | O(m) | m = results |
| AppendEvent | O(1) | Fixed quads |
| VerifyHashChain | O(e) | Event array |
| FindConflicts | O(k) | k = conflicts |
| GetTimeline | O(min(e,L)) | L = limit |
| GenerateSnapshot | O(n+e+s) | All quads in memory |
| RecoverSnapshot | O(n+e+s) | Sequential |

---

## SPARQL QUERY PATTERNS (6 Total)

Located in: SPARC_RDF_SCHEMA.md Part 4

1. **Find Capabilities in Domain**
   - Time: O(n) | Space: O(m)
   - GRAPH ProbeUniverse

2. **Detect Conflicts Between Agents**
   - Time: O(n) | Space: O(m)
   - GRAPH ProbeUniverse

3. **Timeline of Changes**
   - Time: O(e) | Space: O(e)
   - GRAPH ProbeEventLog

4. **Agent Observation History**
   - Time: O(n) | Space: O(k)
   - GRAPH ProbeUniverse

5. **System Configuration Validation**
   - Time: O(m) | Space: O(m)
   - GRAPH ProbeSystem

6. **State Hash Verification Chain**
   - Time: O(e) | Space: O(1)
   - GRAPH ProbeEventLog

All patterns include full SPARQL syntax and complexity analysis.

---

## DEPENDENCIES

### Runtime Dependencies

```
@unrdf/kgc-probe
├── @unrdf/kgc-substrate
│   ├── @unrdf/kgc-4d
│   │   └── @unrdf/core
│   └── @unrdf/oxigraph
│       └── oxigraph (npm)
├── @unrdf/oxigraph
│   └── oxigraph (npm)
└── hash-wasm (for BLAKE3)
    └── oxigraph (npm)
```

### No Circular Dependencies

✓ Verified: ProbeGraphManager depends on KnowledgeStore but NOT vice versa

### Import Rules

✓ Use `@unrdf/oxigraph` for RDF operations (NEVER `n3` package)
✓ Use `@unrdf/kgc-substrate` for storage (NEVER file I/O)
✓ Use `hash-wasm` for hashing (NEVER crypto-js)

---

## VALIDATION CRITERIA

### Code Quality
- [x] All algorithms documented with pseudocode
- [x] Complexity analysis complete (time/space)
- [x] Error conditions specified
- [x] Invariants documented
- [x] Dependencies mapped

### Documentation Quality
- [x] 4,220 lines of pseudocode
- [x] 6 complete algorithm specifications
- [x] 6 SPARQL query patterns
- [x] Integration patterns shown
- [x] Examples provided

### Design Quality
- [x] Determinism guaranteed (same input → same output)
- [x] Immutability enforced (append-only log)
- [x] Auditability provided (complete history)
- [x] Verifiability enabled (hash chain)
- [x] Recoverability possible (snapshots)

---

## FILE STRUCTURE

```
/home/user/unrdf/
├── SPARC_INDEX.md                          (This file)
├── SPARC_RDF_SCHEMA.md                     (43 KB, 1,200+ lines)
├── SPARC_RDF_ALGORITHMS.md                 (34 KB, 1,100+ lines)
├── SPARC_KNOWLEDGE_STORE_INTEGRATION.md    (26 KB, 900+ lines)
└── SPARC_PROBE_DESIGN_SUMMARY.md           (17 KB, 600+ lines)

Total: 120 KB, 4,220 lines of pseudocode
```

---

## QUICK REFERENCE

### Reading Order for Implementation

**Hour 1**: Understand design
1. SPARC_PROBE_DESIGN_SUMMARY.md (20 min)
2. SPARC_RDF_SCHEMA.md Part 1-2 (30 min)
3. Overview of graphs (10 min)

**Hour 2**: Learn algorithms
1. SPARC_RDF_ALGORITHMS.md Algorithm 1 (10 min)
2. SPARC_RDF_ALGORITHMS.md Algorithm 2 (8 min)
3. SPARC_RDF_ALGORITHMS.md Algorithm 3 (10 min)
4. SPARC_RDF_ALGORITHMS.md Algorithm 4 (10 min)
5. Pattern review (22 min)

**Hour 3**: Understand integration
1. SPARC_KNOWLEDGE_STORE_INTEGRATION.md Part 1-2 (15 min)
2. SPARC_KNOWLEDGE_STORE_INTEGRATION.md Part 3 (15 min)
3. SPARC_KNOWLEDGE_STORE_INTEGRATION.md Part 7 (10 min)
4. Implementation planning (20 min)

---

## NEXT ACTIONS

1. **Read all four documents** (3-4 hours)
   - Understand complete design
   - Take notes on key invariants
   - Plan implementation sequence

2. **Create @unrdf/kgc-probe package**
   - Copy file structure from SPARC_PROBE_DESIGN_SUMMARY.md
   - Add dependencies from CLAUDE.md rules

3. **Implement Phase 1 (ontology)**
   - Create src/ontology.mjs with prefixes
   - Unit test with simple quad creation

4. **Implement Phase 2 (algorithms)**
   - Start with ObservationToQuads (simplest)
   - Add QueryCapabilitiesByDomain
   - Add AppendEventToLog
   - Unit test each algorithm

5. **Run OTEL validation**
   - Measure complexity vs. pseudocode
   - Verify determinism/immutability
   - Ensure 80+/100 OTEL score

---

## SUPPORT

**Questions about design**?
- Check SPARC_PROBE_DESIGN_SUMMARY.md section "Key Design Decisions"

**Questions about an algorithm**?
- Find it in SPARC_RDF_ALGORITHMS.md, read full pseudocode

**Questions about integration**?
- Check SPARC_KNOWLEDGE_STORE_INTEGRATION.md

**Questions about SPARQL patterns**?
- See SPARC_RDF_SCHEMA.md Part 4

---

**Status**: Pseudocode phase complete
**Total Lines**: 4,220 lines of language-agnostic pseudocode
**Date**: 2025-12-27
**Next Phase**: Implementation (SPARC Phase 2)
