# SPARC Pseudocode: KnowledgeStore Integration for RDF Probe Graphs

This document bridges the RDF algorithms with the KnowledgeStore substrate, showing how the three probe graphs are persisted, committed, and recovered.

---

## PART 1: STORAGE ARCHITECTURE

### Three-Graph Model with Single KnowledgeStore

```
┌─────────────────────────────────────────────────────────────────┐
│                     KnowledgeStore Instance                       │
│  (Immutable Append-Only Log + Hash-Stable Snapshots)             │
└─────────────────────────────────────────────────────────────────┘
                            │
                 ┌──────────┼──────────┐
                 │          │          │
                 ▼          ▼          ▼
        ┌────────────┐ ┌─────────┐ ┌──────────┐
        │ ProbeUni- │ │ProbeEvent│ │ProbeSystem
        │  verse    │ │   Log    │ │ Config   │
        │(N Quads)  │ │(E Quads) │ │(S Quads) │
        └────────────┘ └─────────┘ └──────────┘
              │              │            │
              └──────────────┼────────────┘
                             │
                   ┌─────────▼────────┐
                   │  KGCStore        │
                   │(Events + Deltas) │
                   └──────────────────┘
                             │
                   ┌─────────▼────────┐
                   │ BLAKE3 Hash      │
                   │ (Deterministic)  │
                   └──────────────────┘
                             │
                   ┌─────────▼────────────┐
                   │ Git Backbone         │
                   │ (Immutable Snapshot) │
                   └──────────────────────┘
```

### State Commitment Structure

```
StateCommitment {
    state_hash: string              // BLAKE3(sorted_quads)
    log_index: bigint               // Current append position
    timestamp_ns: bigint            // When state was captured
    quad_count: number              // Total quads in all graphs

    // Derived metrics
    universeQuadCount: number       // Quads in ProbeUniverse
    eventLogQuadCount: number       // Quads in ProbeEventLog
    systemQuadCount: number         // Quads in ProbeSystem
}
```

---

## PART 2: MATERIALIZE OBSERVATIONS TO KNOWLED STORE

### High-Level Flow

```
ALGORITHM: MaterializeObservationToKnowledgeStore

INPUT:
    observation: Observation         // From probe agent
    store: KnowledgeStore           // Substrate instance
    includeEventLog: boolean        // Whether to log event (default: true)

OUTPUT:
    {
        uri: string,
        logIndices: Set<bigint>,
        stateHashAfter: string,
        quadsAppended: number
    }

BEGIN

    // STEP 1: Convert Observation to Quads
    // (Uses ObservationToQuads algorithm from SPARC_RDF_ALGORITHMS.md)
    quadResult ← AWAIT ObservationToQuads(observation, store)

    obsUri ← quadResult.uri
    indices ← quadResult.indices
    quadCount ← quadResult.quadCount

    // STEP 2: Get State Commitment After Observation
    commitment ← AWAIT store.getStateCommitment()

    // STEP 3: Optional: Log Event in ProbeEventLog
    IF includeEventLog THEN
        eventResult ← AWAIT AppendEventToLog(
            'OBSERVATION',
            observation.agentId,
            {
                observationUri: obsUri,
                quadCount: quadCount,
                domainUri: quadResult.domainUri,
                claimHash: observation.claimHash
            },
            store
        )
    END IF

    // STEP 4: Return complete result
    RETURN {
        uri: obsUri,
        logIndices: indices,
        stateHashAfter: commitment.state_hash,
        quadsAppended: quadCount,
        logIndex: store.getLogIndex(),
        epoch: store.getEpoch()
    }

END
```

### Example Usage Pattern

```
// Pseudocode for application layer
CLASS ProbeManager:
    store: KnowledgeStore

    METHOD async recordObservation(observation: Observation):
        // Step 1: Validate observation
        IF NOT IsValidObservation(observation) THEN
            THROW ValidationError("Invalid observation")
        END IF

        // Step 2: Materialize to RDF
        result ← AWAIT MaterializeObservationToKnowledgeStore(
            observation,
            this.store,
            true  // includeEventLog = true
        )

        // Step 3: Return with proof
        RETURN {
            success: true,
            observationUri: result.uri,
            stateCommitment: {
                hash: result.stateHashAfter,
                logIndex: result.logIndex,
                timestamp_ns: observation.timestamp_ns
            },
            quadsAdded: result.quadsAppended
        }
    END METHOD
END CLASS
```

---

## PART 3: GENERATE DETERMINISTIC SNAPSHOTS

### Snapshot Generation Algorithm

```
ALGORITHM: GenerateDeterministicProbeSnapshot

INPUT:
    store: KnowledgeStore
    includeMetrics: boolean (default: true)

OUTPUT:
    SnapshotMetadata {
        epoch: number
        timestamp_ns: bigint
        commitHash: string              // Git reference
        quadsHash: string               // BLAKE3 of all quads
        metrics: {
            universeQuadCount: number
            eventLogQuadCount: number
            systemQuadCount: number
            totalQuadCount: number
        }
        graphs: {
            universeHash: string        // BLAKE3 of ProbeUniverse
            eventLogHash: string        // BLAKE3 of ProbeEventLog
            systemHash: string          // BLAKE3 of ProbeSystem
        }
    }

BEGIN

    // STEP 1: Verify log immutability before snapshot
    IF NOT VerifyEventLogImmutability(store) THEN
        THROW Error("Cannot snapshot - event log is not immutable")
    END IF

    // STEP 2: Query all quads from each graph
    universeGraphNode ← dataFactory.namedNode('http://unrdf.org/kgc/probe/ProbeUniverse')
    eventLogGraphNode ← dataFactory.namedNode('http://unrdf.org/kgc/probe/ProbeEventLog')
    systemGraphNode ← dataFactory.namedNode('http://unrdf.org/kgc/probe/ProbeSystem')

    universeQuads ← store.selectTriples({
        graph: universeGraphNode
    })

    eventLogQuads ← store.selectTriples({
        graph: eventLogGraphNode
    })

    systemQuads ← store.selectTriples({
        graph: systemGraphNode
    })

    // STEP 3: Canonicalize each graph
    canonicalUniverse ← CanonicalizeQuads(universeQuads)
    canonicalEventLog ← CanonicalizeQuads(eventLogQuads)
    canonicalSystem ← CanonicalizeQuads(systemQuads)

    // STEP 4: Hash each graph individually
    universeHash ← BLAKE3(SerializeQuadsToNQuads(canonicalUniverse))
    eventLogHash ← BLAKE3(SerializeQuadsToNQuads(canonicalEventLog))
    systemHash ← BLAKE3(SerializeQuadsToNQuads(canonicalSystem))

    // STEP 5: Combine all quads and hash globally
    allCanonical ← CanonicalizeQuads(
        [...canonicalUniverse, ...canonicalEventLog, ...canonicalSystem]
    )
    globalHash ← BLAKE3(SerializeQuadsToNQuads(allCanonical))

    // STEP 6: Generate snapshot via KnowledgeStore
    // This persists to Git via GitBackbone
    snapshot ← AWAIT store.generateSnapshot()

    // STEP 7: Augment with probe-specific metadata
    probeSnapshot ← {
        epoch: snapshot.epoch,
        timestamp_ns: snapshot.timestamp_ns,
        commitHash: snapshot.commit_hash,
        quadsHash: globalHash,
        metrics: {
            universeQuadCount: universeQuads.size,
            eventLogQuadCount: eventLogQuads.size,
            systemQuadCount: systemQuads.size,
            totalQuadCount: universeQuads.size + eventLogQuads.size + systemQuads.size
        },
        graphs: {
            universeHash: universeHash,
            eventLogHash: eventLogHash,
            systemHash: systemHash
        },
        verification: {
            isImmutable: VerifyEventLogImmutability(store).isValid,
            hashChainValid: VerifyStateHashChain(store).isValid
        }
    }

    RETURN probeSnapshot

END

SUBROUTINE: SerializeQuadsToNQuads
INPUT: canonicalQuads (sorted array)
OUTPUT: string in N-Quads format

BEGIN
    nquads ← ""
    FOR EACH quad IN canonicalQuads DO
        nquads ← nquads + SerializeQuadToNQuads(quad) + "\n"
    END FOR
    RETURN nquads
END

SUBROUTINE: SerializeQuadToNQuads
INPUT: quad (RDF quad)
OUTPUT: single N-Quads line

BEGIN
    // N-Quads format: <subject> <predicate> <object> <graph> .

    subject ← IF quad.subject.termType == "NamedNode" THEN
        "<" + quad.subject.value + ">"
    ELSE IF quad.subject.termType == "BlankNode" THEN
        "_:" + quad.subject.value
    END IF

    predicate ← "<" + quad.predicate.value + ">"

    object ← IF quad.object.termType == "NamedNode" THEN
        "<" + quad.object.value + ">"
    ELSE IF quad.object.termType == "BlankNode" THEN
        "_:" + quad.object.value
    ELSE IF quad.object.termType == "Literal" THEN
        "\"" + EscapeString(quad.object.value) + "\"" +
        (IF quad.object.datatype THEN
            "^^<" + quad.object.datatype.value + ">"
         ELSE IF quad.object.language THEN
            "@" + quad.object.language
         END IF)
    END IF

    graph ← "<" + quad.graph.value + ">"

    RETURN subject + " " + predicate + " " + object + " " + graph + " ."

END
```

### Snapshot Verification

```
ALGORITHM: VerifySnapshot

INPUT:
    snapshot: SnapshotMetadata
    expectedGraphHash: string (optional)
    expectedEventCount: number (optional)

OUTPUT:
    VerificationResult {
        isValid: boolean
        errors: Array<string>
        warnings: Array<string>
    }

BEGIN

    errors ← []
    warnings ← []

    // Check 1: Snapshot has required fields
    IF snapshot.quadsHash == null THEN
        errors.APPEND("Missing quadsHash")
    END IF

    IF snapshot.commitHash == null THEN
        errors.APPEND("Missing commitHash")
    END IF

    IF snapshot.metrics == null THEN
        errors.APPEND("Missing metrics")
    END IF

    // Check 2: Quad counts are non-negative
    IF snapshot.metrics.universeQuadCount < 0 THEN
        errors.APPEND("universeQuadCount is negative")
    END IF

    IF snapshot.metrics.eventLogQuadCount < 0 THEN
        errors.APPEND("eventLogQuadCount is negative")
    END IF

    IF snapshot.metrics.systemQuadCount < 0 THEN
        errors.APPEND("systemQuadCount is negative")
    END IF

    // Check 3: Total count matches sum
    expectedTotal ← snapshot.metrics.universeQuadCount +
                    snapshot.metrics.eventLogQuadCount +
                    snapshot.metrics.systemQuadCount

    IF snapshot.metrics.totalQuadCount != expectedTotal THEN
        errors.APPEND(
            f"totalQuadCount mismatch: {snapshot.metrics.totalQuadCount} != " +
            f"{expectedTotal}"
        )
    END IF

    // Check 4: Optional hash verification
    IF expectedGraphHash != null AND
       snapshot.quadsHash != expectedGraphHash THEN
        errors.APPEND(
            f"Graph hash mismatch: {snapshot.quadsHash} != " +
            f"{expectedGraphHash}"
        )
    END IF

    // Check 5: Optional event count verification
    IF expectedEventCount != null AND
       snapshot.metrics.eventLogQuadCount < expectedEventCount * 6 THEN  // min 6 quads per event
        warnings.APPEND(
            f"Event count seems low: {snapshot.metrics.eventLogQuadCount} quads " +
            f"for expected {expectedEventCount} events"
        )
    END IF

    // Check 6: Immutability verification
    IF snapshot.verification != null AND
       NOT snapshot.verification.isImmutable THEN
        errors.APPEND("Event log is not immutable")
    END IF

    IF snapshot.verification != null AND
       NOT snapshot.verification.hashChainValid THEN
        errors.APPEND("State hash chain is not valid")
    END IF

    RETURN {
        isValid: errors.length == 0,
        errors: errors,
        warnings: warnings
    }

END
```

---

## PART 4: RECOVER FROM SNAPSHOT

### Snapshot Recovery Algorithm

```
ALGORITHM: RecoverProbeGraphsFromSnapshot

INPUT:
    snapshotId: string              // Git commit reference
    targetStore: KnowledgeStore (optional, creates new if null)

OUTPUT:
    {
        store: KnowledgeStore,
        recoveredGraphs: {
            universeQuadCount: number,
            eventLogQuadCount: number,
            systemQuadCount: number
        },
        stateHash: string
    }

BEGIN

    // STEP 1: Create new store if needed
    store ← IF targetStore == null THEN
        new KnowledgeStore({nodeId: 'recovery-' + Date.now()})
    ELSE
        targetStore
    END IF

    // STEP 2: Retrieve snapshot from Git
    snapshotData ← AWAIT store.git.retrieveSnapshot(snapshotId)

    IF snapshotData == null THEN
        THROW Error(f"Snapshot {snapshotId} not found")
    END IF

    // STEP 3: Parse N-Quads from snapshot
    nquads ← snapshotData.content  // Serialized N-Quads
    quads ← ParseNQuads(nquads)

    // STEP 4: Separate quads by graph
    universeGraph ← 'http://unrdf.org/kgc/probe/ProbeUniverse'
    eventLogGraph ← 'http://unrdf.org/kgc/probe/ProbeEventLog'
    systemGraph ← 'http://unrdf.org/kgc/probe/ProbeSystem'

    universeQuads ← []
    eventLogQuads ← []
    systemQuads ← []
    otherQuads ← []

    FOR EACH quad IN quads DO
        IF quad.graph == universeGraph THEN
            universeQuads.APPEND(quad)
        ELSE IF quad.graph == eventLogGraph THEN
            eventLogQuads.APPEND(quad)
        ELSE IF quad.graph == systemGraph THEN
            systemQuads.APPEND(quad)
        ELSE
            otherQuads.APPEND(quad)
        END IF
    END FOR

    // STEP 5: Append quads back to store
    // Critical: Maintain order and immutability
    totalAppended ← 0

    FOR EACH quad IN universeQuads DO
        AWAIT store.appendTriple(
            'add',
            quad.subject,
            quad.predicate,
            quad.object,
            quad.graph
        )
        totalAppended ← totalAppended + 1
    END FOR

    FOR EACH quad IN eventLogQuads DO
        AWAIT store.appendTriple(
            'add',
            quad.subject,
            quad.predicate,
            quad.object,
            quad.graph
        )
        totalAppended ← totalAppended + 1
    END FOR

    FOR EACH quad IN systemQuads DO
        AWAIT store.appendTriple(
            'add',
            quad.subject,
            quad.predicate,
            quad.object,
            quad.graph
        )
        totalAppended ← totalAppended + 1
    END FOR

    // STEP 6: Verify recovered state
    commitment ← AWAIT store.getStateCommitment()

    IF commitment.quad_count != quads.length THEN
        THROW Error(
            f"Quad count mismatch after recovery: " +
            f"expected {quads.length}, got {commitment.quad_count}"
        )
    END IF

    // STEP 7: Generate new snapshot to verify integrity
    newSnapshot ← AWAIT store.generateSnapshot()

    RETURN {
        store: store,
        recoveredGraphs: {
            universeQuadCount: universeQuads.length,
            eventLogQuadCount: eventLogQuads.length,
            systemQuadCount: systemQuads.length
        },
        stateHash: commitment.state_hash,
        newSnapshotId: newSnapshot.snapshot_id,
        logIndex: store.getLogIndex()
    }

END

SUBROUTINE: ParseNQuads
INPUT: nquads (string)
OUTPUT: Array<Quad>

BEGIN
    // Parse N-Quads format into RDF quads
    lines ← nquads.SPLIT("\n")
    quads ← []

    FOR EACH line IN lines DO
        IF line.length == 0 OR line.startsWith("#") THEN
            CONTINUE  // Skip empty lines and comments
        END IF

        quad ← ParseNQuadsLine(line)
        IF quad != null THEN
            quads.APPEND(quad)
        END IF
    END FOR

    RETURN quads
END
```

---

## PART 5: STATE HASH TRACKING

### Track State Transitions

```
ALGORITHM: GetStateHashHistory

INPUT:
    store: KnowledgeStore
    startIndex: bigint (optional)
    endIndex: bigint (optional)

OUTPUT:
    Array<StateTransition> {
        logIndex: bigint
        timestamp_ns: bigint
        operationType: string
        stateHashBefore: string
        stateHashAfter: string
    }

BEGIN

    // STEP 1: Query event log for all events
    timeline ← TimelineOfChanges(
        store,
        startIndex,
        endIndex,
        null  // no limit
    )

    // STEP 2: Convert to state transitions
    transitions ← []

    FOR EACH event IN timeline DO
        transition ← {
            logIndex: event.logIndex,
            timestamp_ns: event.timestamp_ns,
            operationType: event.operationType,
            stateHashBefore: event.stateHashBefore,
            stateHashAfter: event.stateHashAfter
        }
        transitions.APPEND(transition)
    END FOR

    // STEP 3: Verify chain continuity
    FOR i = 0 TO transitions.length - 2 DO
        IF transitions[i].stateHashAfter != transitions[i + 1].stateHashBefore THEN
            THROW Error(
                f"State hash chain broken at index {i}: " +
                f"{transitions[i].stateHashAfter} != " +
                f"{transitions[i + 1].stateHashBefore}"
            )
        END IF
    END FOR

    RETURN transitions

END

ALGORITHM: ReplayStateToVersion

INPUT:
    store: KnowledgeStore
    targetLogIndex: bigint

OUTPUT:
    ReplayResult {
        targetHash: string
        stepsReplayed: number
        isValid: boolean
    }

BEGIN

    // STEP 1: Get all events up to target index
    events ← store.selectTriples({
        predicate: dataFactory.namedNode('http://unrdf.org/probe/ontology/logIndex'),
        graph: dataFactory.namedNode('http://unrdf.org/kgc/probe/ProbeEventLog')
    })

    // STEP 2: Filter events by index
    relevantEvents ← []

    FOR EACH eventQuad IN events DO
        index ← PARSE_BIGINT(eventQuad.object.value)
        IF index <= targetLogIndex THEN
            relevantEvents.APPEND({subject: eventQuad.subject, index: index})
        END IF
    END FOR

    // STEP 3: Sort by index
    relevantEvents.SORT((a, b) => a.index - b.index)

    // STEP 4: Trace state through events
    states ← []
    currentHash ← "0000000000000000000000000000000000000000"  // genesis hash

    FOR EACH event IN relevantEvents DO

        hashAfterQuads ← store.selectTriples({
            subject: event.subject,
            predicate: dataFactory.namedNode('http://unrdf.org/probe/ontology/stateHashAfter'),
            graph: dataFactory.namedNode('http://unrdf.org/kgc/probe/ProbeEventLog')
        })

        IF hashAfterQuads.size > 0 THEN
            nextHash ← hashAfterQuads[0].object.value
            states.APPEND({logIndex: event.index, stateHash: nextHash})
            currentHash ← nextHash
        END IF

    END FOR

    // STEP 5: Return result
    RETURN {
        targetHash: currentHash,
        stepsReplayed: relevantEvents.length,
        isValid: relevantEvents.length == states.length
    }

END
```

---

## PART 6: TESTING AND VALIDATION

### Comprehensive Integrity Test

```
ALGORITHM: ValidateProbeStoreIntegrity

INPUT:
    store: KnowledgeStore

OUTPUT:
    IntegrityReport {
        isValid: boolean
        errors: Array<string>
        warnings: Array<string>
        metrics: {
            totalQuads: number
            eventCount: number
            observationCount: number
            isImmutable: boolean
            hashChainValid: boolean
        }
    }

BEGIN

    report ← {
        isValid: true,
        errors: [],
        warnings: [],
        metrics: {}
    }

    // Test 1: Event log immutability
    immutabilityResult ← VerifyEventLogImmutability(store)
    IF NOT immutabilityResult.isValid THEN
        report.errors ← report.errors.CONCAT(immutabilityResult.errors)
        report.isValid ← false
    END IF
    report.metrics.isImmutable ← immutabilityResult.isValid

    // Test 2: State hash chain
    chainResult ← VerifyStateHashChain(store)
    IF NOT chainResult.isValid THEN
        report.errors ← report.errors.CONCAT(chainResult.errors)
        report.isValid ← false
    END IF
    report.metrics.hashChainValid ← chainResult.isValid
    report.metrics.eventCount ← chainResult.chainLength

    // Test 3: Observation consistency
    observations ← store.selectTriples({
        predicate: dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        object: dataFactory.namedNode('http://unrdf.org/probe/ontology/Observation'),
        graph: dataFactory.namedNode('http://unrdf.org/kgc/probe/ProbeUniverse')
    })

    report.metrics.observationCount ← observations.size

    FOR EACH obs IN observations DO
        // Check for required properties
        IF NOT HasRequiredProperties(obs, [
            'timestamp_ns',
            'agentId',
            'domain',
            'claimHash',
            'evidence'
        ]) THEN
            report.warnings.APPEND(
                f"Observation {obs.subject.value} missing required properties"
            )
        END IF
    END FOR

    // Test 4: Quad count consistency
    allQuads ← store.selectTriples({})
    report.metrics.totalQuads ← allQuads.size

    commitment ← AWAIT store.getStateCommitment()
    IF commitment.quad_count != allQuads.size THEN
        report.warnings.APPEND(
            f"Quad count mismatch: commitment reports {commitment.quad_count}, " +
            f"actual is {allQuads.size}"
        )
    END IF

    // Test 5: Generate snapshot as final integrity check
    TRY
        snapshot ← AWAIT store.generateSnapshot()
        report.metrics.snapshotEpoch ← snapshot.epoch
        report.metrics.snapshotHash ← snapshot.quads_hash
    CATCH error
        report.errors.APPEND(f"Snapshot generation failed: {error.message}")
        report.isValid ← false
    END TRY

    RETURN report

END
```

---

## PART 7: MODULE INTEGRATION PATTERN

### ProbeGraphManager with KnowledgeStore

```
CLASS ProbeGraphManager:
    store: KnowledgeStore
    nodeId: string

    METHOD constructor(nodeId: string):
        this.nodeId ← nodeId
        this.store ← new KnowledgeStore({nodeId: nodeId})
    END METHOD

    // Observation operations
    METHOD async recordObservation(observation: Observation):
        RETURN AWAIT MaterializeObservationToKnowledgeStore(
            observation,
            this.store,
            true
        )
    END METHOD

    METHOD queryCapabilities(domainName: string):
        RETURN QueryCapabilitiesByDomain(this.store, domainName)
    END METHOD

    // Event log operations
    METHOD async recordEvent(operationType: string, agentId: string, payload: object):
        RETURN AWAIT AppendEventToLog(operationType, agentId, payload, this.store)
    END METHOD

    METHOD getEventTimeline(startIndex: bigint, endIndex: bigint, limit: number):
        RETURN TimelineOfChanges(this.store, startIndex, endIndex, limit)
    END METHOD

    METHOD async verifyIntegrity():
        RETURN ValidateProbeStoreIntegrity(this.store)
    END METHOD

    // Snapshot operations
    METHOD async generateSnapshot():
        RETURN AWAIT GenerateDeterministicProbeSnapshot(
            this.store,
            true  // includeMetrics
        )
    END METHOD

    METHOD async recoverFromSnapshot(snapshotId: string):
        RETURN AWAIT RecoverProbeGraphsFromSnapshot(snapshotId, this.store)
    END METHOD

    // Conflict operations
    METHOD queryConflicts(claimHash: string, agentA: string, agentB: string):
        RETURN QueryConflictingObservations(this.store, claimHash, agentA, agentB)
    END METHOD

    // State tracking
    METHOD async getStateHistory(startIndex: bigint, endIndex: bigint):
        RETURN GetStateHashHistory(this.store, startIndex, endIndex)
    END METHOD

    METHOD async replayToVersion(logIndex: bigint):
        RETURN ReplayStateToVersion(this.store, logIndex)
    END METHOD
END CLASS
```

---

## SUMMARY

This integration ensures:

1. **Determinism**: All graphs produce identical hashes on same data
2. **Immutability**: Event log cannot be modified or reordered
3. **Atomicity**: Operations append all quads together or not at all
4. **Auditability**: Complete history in ProbeEventLog with state hash chain
5. **Recoverability**: Snapshots can be restored to exact prior state
6. **Verifiability**: Cryptographic verification of state transitions

### Key Files (Implementation)

```
// Main integration point
packages/kgc-probe/src/ProbeGraphManager.mjs

// Algorithms (pseudocode -> implementation)
packages/kgc-probe/src/graphs/
    ├── observation-to-quads.mjs
    ├── event-log-append.mjs
    ├── snapshot-generation.mjs
    └── recovery.mjs

// KnowledgeStore bridge
packages/kgc-probe/src/store-integration.mjs

// Testing
packages/kgc-probe/__tests__/
    ├── integration.test.mjs
    ├── snapshot.test.mjs
    └── recovery.test.mjs
```

### Critical Guarantees

```
Invariant 1: Determinism
    hash(materialize(obs)) = hash(materialize(obs))

Invariant 2: Immutability
    for all i < j: event[i].timestamp_ns < event[j].timestamp_ns

Invariant 3: State Hash Chain
    for all i: event[i].stateHashAfter == event[i+1].stateHashBefore

Invariant 4: Recoverability
    snapshot(t0) + replay(t0 → t1) = snapshot(t1)

Invariant 5: Auditability
    all changes are recorded with timestamps and agent IDs
```
