# SPARC Pseudocode: RDF Graph Algorithms for KGC Probe

This document provides detailed pseudocode algorithms for implementing the RDF graph operations defined in `SPARC_RDF_SCHEMA.md`.

---

## ALGORITHM 1: CREATE OBSERVATION QUADS

**Purpose**: Transform a JavaScript Observation object into canonical N3 quads for storage.

**Input**:
- `observation` (Observation object with agentId, domain, claimHash, evidence, timestamp_ns)
- `store` (KnowledgeStore instance)

**Output**:
- `{uri: string, indices: Set<bigint>, timestamp_ns: bigint}`

**Prerequisites**:
- Observation must have non-empty agentId, domain, claimHash
- Evidence must be serializable to JSON
- timestamp_ns must be a positive BigInt (nanoseconds since Unix epoch)

```
ALGORITHM: ObservationToQuads

CONSTANTS:
    NS_RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    NS_RDFS = "http://www.w3.org/2000/01/rdf-schema#"
    NS_PROBE = "http://unrdf.org/probe/ontology/"
    NS_KGC = "http://unrdf.org/kgc/probe/"
    NS_XSD = "http://www.w3.org/2001/XMLSchema#"
    UNIVERSE_GRAPH = NS_KGC + "ProbeUniverse"

VARIABLES:
    observation: Object          // Input observation
    store: KnowledgeStore        // Store reference
    obsUri: string              // Generated observation URI
    domainUri: string           // Generated domain URI
    quads: Array<Quad>          // Quads to append
    indices: Set<BigInt>        // Log indices of appended quads
    timestamp_ns: BigInt        // Current time in nanoseconds

BEGIN

    // INPUT VALIDATION
    IF observation == null THEN
        THROW TypeError("observation cannot be null")
    END IF

    IF typeof observation.agentId != "string" OR observation.agentId.length == 0 THEN
        THROW TypeError("observation.agentId must be non-empty string")
    END IF

    IF typeof observation.domain != "string" OR observation.domain.length == 0 THEN
        THROW TypeError("observation.domain must be non-empty string")
    END IF

    IF typeof observation.claimHash != "string" OR observation.claimHash.length != 64 THEN
        THROW TypeError("observation.claimHash must be 64-char SHA256 hex")
    END IF

    IF typeof observation.timestamp_ns != "bigint" OR observation.timestamp_ns <= 0n THEN
        THROW TypeError("observation.timestamp_ns must be positive BigInt")
    END IF

    IF typeof observation.evidence != "object" OR observation.evidence == null THEN
        THROW TypeError("observation.evidence must be object")
    END IF

    // STEP 1: CANONICALIZE OBSERVATION
    canonicalObs ← {
        agentId: observation.agentId,
        domain: observation.domain,
        timestamp_ns: observation.timestamp_ns,
        claimHash: observation.claimHash,
        evidence: SortObjectByKeys(observation.evidence)
    }

    // STEP 2: GENERATE DETERMINISTIC URIs
    // Use SHA256(canonical_json) to ensure same observation always gets same URI
    canonicalJson ← JSON.stringify(canonicalObs)
    obsHash ← SHA256(canonicalJson)
    obsUri ← NS_KGC + "observation#" + obsHash

    domainHash ← SHA256(observation.domain)
    domainUri ← NS_KGC + "domain#" + domainHash

    // STEP 3: CREATE QUADS
    universeGraphNode ← dataFactory.namedNode(UNIVERSE_GRAPH)
    obsNode ← dataFactory.namedNode(obsUri)
    domainNode ← dataFactory.namedNode(domainUri)

    quads ← []

    // Quad 1: Type assertion (observation is probe:Observation)
    quad1 ← dataFactory.quad(
        obsNode,
        dataFactory.namedNode(NS_RDF + "type"),
        dataFactory.namedNode(NS_PROBE + "Observation"),
        universeGraphNode
    )
    quads.APPEND(quad1)

    // Quad 2: Timestamp
    quad2 ← dataFactory.quad(
        obsNode,
        dataFactory.namedNode(NS_PROBE + "timestamp_ns"),
        dataFactory.literal(
            observation.timestamp_ns.toString(),
            dataFactory.namedNode(NS_XSD + "long")
        ),
        universeGraphNode
    )
    quads.APPEND(quad2)

    // Quad 3: Agent ID
    quad3 ← dataFactory.quad(
        obsNode,
        dataFactory.namedNode(NS_PROBE + "agentId"),
        dataFactory.literal(observation.agentId),
        universeGraphNode
    )
    quads.APPEND(quad3)

    // Quad 4: Domain reference
    quad4 ← dataFactory.quad(
        obsNode,
        dataFactory.namedNode(NS_PROBE + "domain"),
        domainNode,
        universeGraphNode
    )
    quads.APPEND(quad4)

    // Quad 5: Claim Hash
    quad5 ← dataFactory.quad(
        obsNode,
        dataFactory.namedNode(NS_PROBE + "claimHash"),
        dataFactory.literal(observation.claimHash),
        universeGraphNode
    )
    quads.APPEND(quad5)

    // Quad 6: Evidence (JSON serialized)
    evidenceJson ← JSON.stringify(observation.evidence)
    quad6 ← dataFactory.quad(
        obsNode,
        dataFactory.namedNode(NS_PROBE + "evidence"),
        dataFactory.literal(evidenceJson),
        universeGraphNode
    )
    quads.APPEND(quad6)

    // Quad 7: Domain type
    quad7 ← dataFactory.quad(
        domainNode,
        dataFactory.namedNode(NS_RDF + "type"),
        dataFactory.namedNode(NS_PROBE + "Domain"),
        universeGraphNode
    )
    quads.APPEND(quad7)

    // Quad 8: Domain label
    quad8 ← dataFactory.quad(
        domainNode,
        dataFactory.namedNode(NS_RDFS + "label"),
        dataFactory.literal(observation.domain),
        universeGraphNode
    )
    quads.APPEND(quad8)

    // STEP 4: ADD CAPABILITIES AS QUADS (if present)
    IF observation.evidence HAS PROPERTY "capabilities" AND
       observation.evidence.capabilities IS Array THEN

        FOR EACH capability IN observation.evidence.capabilities DO

            // Generate deterministic capability URI
            capHash ← SHA256(capability.name + "|" + capability.value)
            capUri ← NS_KGC + "capability#" + capHash
            capNode ← dataFactory.namedNode(capUri)

            // Quad: Domain has capability
            quadCapRef ← dataFactory.quad(
                domainNode,
                dataFactory.namedNode(NS_PROBE + "hasCapability"),
                capNode,
                universeGraphNode
            )
            quads.APPEND(quadCapRef)

            // Quad: Capability type
            quadCapType ← dataFactory.quad(
                capNode,
                dataFactory.namedNode(NS_RDF + "type"),
                dataFactory.namedNode(NS_PROBE + "Capability"),
                universeGraphNode
            )
            quads.APPEND(quadCapType)

            // Quad: Capability name
            quadCapName ← dataFactory.quad(
                capNode,
                dataFactory.namedNode(NS_RDFS + "label"),
                dataFactory.literal(capability.name),
                universeGraphNode
            )
            quads.APPEND(quadCapName)

            // Quad: Capability value
            quadCapVal ← dataFactory.quad(
                capNode,
                dataFactory.namedNode(NS_RDF + "value"),
                dataFactory.literal(capability.value.toString()),
                universeGraphNode
            )
            quads.APPEND(quadCapVal)

        END FOR

    END IF

    // STEP 5: APPEND ALL QUADS TO STORE (ATOMIC)
    indices ← EMPTY_SET

    TRY
        FOR EACH quad IN quads DO
            result ← AWAIT store.appendTriple(
                'add',
                quad.subject,
                quad.predicate,
                quad.object,
                quad.graph
            )
            indices.ADD(result.index)
        END FOR
    CATCH error
        THROW Error("Failed to append observation quads: " + error.message)
    END TRY

    // STEP 6: LOG EVENT
    AWAIT AppendEventToLog(
        'OBSERVATION',
        observation.agentId,
        {
            observationUri: obsUri,
            domainUri: domainUri,
            claimHash: observation.claimHash,
            quadCount: quads.length
        },
        store
    )

    // STEP 7: RETURN RESULT
    RETURN {
        uri: obsUri,
        domainUri: domainUri,
        indices: indices,
        timestamp_ns: observation.timestamp_ns,
        quadCount: quads.length
    }

END

SUBROUTINE: SortObjectByKeys
INPUT: obj (object)
OUTPUT: object with keys in lexicographic order

BEGIN
    keys ← SORT(Object.keys(obj))
    sortedObj ← {}
    FOR EACH key IN keys DO
        value ← obj[key]
        IF typeof value == "object" AND value != null THEN
            sortedObj[key] ← SortObjectByKeys(value)
        ELSE
            sortedObj[key] ← value
        END IF
    END FOR
    RETURN sortedObj
END

SUBROUTINE: SHA256
INPUT: data (string)
OUTPUT: hex string (64 chars)
// Implementation: Use crypto.subtle.digest or Node.js crypto module
END
```

**Time Complexity**: O(k) where k = number of capabilities
**Space Complexity**: O(k) for quads array
**Error Conditions**:
- observation is null or undefined
- Missing required fields (agentId, domain, claimHash)
- Invalid timestamp_ns (must be positive BigInt)
- Store.appendTriple fails

---

## ALGORITHM 2: QUERY CAPABILITIES IN DOMAIN

**Purpose**: Retrieve all capabilities available in a specific domain from ProbeUniverse.

**Input**:
- `store` (KnowledgeStore instance)
- `domainName` (string, e.g., "filesystem", "network")

**Output**:
- `{domain: Object, capabilities: Array<{uri, name, value}>}`

```
ALGORITHM: QueryCapabilitiesByDomain

CONSTANTS:
    NS_RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    NS_RDFS = "http://www.w3.org/2000/01/rdf-schema#"
    NS_PROBE = "http://unrdf.org/probe/ontology/"
    NS_KGC = "http://unrdf.org/kgc/probe/"
    UNIVERSE_GRAPH = NS_KGC + "ProbeUniverse"

VARIABLES:
    store: KnowledgeStore
    domainName: string
    domainUri: string
    domainNode: NamedNode
    capabilities: Array
    universeGraph: NamedNode

BEGIN

    // INPUT VALIDATION
    IF store == null THEN
        THROW TypeError("store cannot be null")
    END IF

    IF typeof domainName != "string" OR domainName.length == 0 THEN
        THROW TypeError("domainName must be non-empty string")
    END IF

    // STEP 1: COMPUTE DOMAIN URI
    // Must match the URI generation in ObservationToQuads
    domainHash ← SHA256(domainName)
    domainUri ← NS_KGC + "domain#" + domainHash
    domainNode ← dataFactory.namedNode(domainUri)
    universeGraph ← dataFactory.namedNode(UNIVERSE_GRAPH)

    // STEP 2: VERIFY DOMAIN EXISTS
    domainTypeQuads ← store.selectTriples({
        subject: domainNode,
        predicate: dataFactory.namedNode(NS_RDF + "type"),
        object: dataFactory.namedNode(NS_PROBE + "Domain"),
        graph: universeGraph
    })

    IF domainTypeQuads.size == 0 THEN
        THROW Error(f"Domain '{domainName}' not found in ProbeUniverse")
    END IF

    // STEP 3: QUERY DOMAIN LABEL
    domainLabelQuads ← store.selectTriples({
        subject: domainNode,
        predicate: dataFactory.namedNode(NS_RDFS + "label"),
        graph: universeGraph
    })

    domainLabel ← null
    IF domainLabelQuads.size > 0 THEN
        domainLabel ← domainLabelQuads[0].object.value
    END IF

    // STEP 4: FIND ALL CAPABILITIES FOR THIS DOMAIN
    // Query: domain -> hasCapability -> capability
    hasCapabilityPred ← dataFactory.namedNode(NS_PROBE + "hasCapability")

    capabilityRefQuads ← store.selectTriples({
        subject: domainNode,
        predicate: hasCapabilityPred,
        graph: universeGraph
    })

    capabilities ← []

    FOR EACH refQuad IN capabilityRefQuads DO

        capNode ← refQuad.object  // The capability URI

        // Query capability name (rdfs:label)
        capNameQuads ← store.selectTriples({
            subject: capNode,
            predicate: dataFactory.namedNode(NS_RDFS + "label"),
            graph: universeGraph
        })

        capName ← null
        IF capNameQuads.size > 0 THEN
            capName ← capNameQuads[0].object.value
        END IF

        // Query capability value (rdf:value)
        capValueQuads ← store.selectTriples({
            subject: capNode,
            predicate: dataFactory.namedNode(NS_RDF + "value"),
            graph: universeGraph
        })

        capValue ← null
        IF capValueQuads.size > 0 THEN
            capValue ← capValueQuads[0].object.value
        END IF

        // Add to results
        capability ← {
            uri: capNode.value,
            name: capName,
            value: capValue
        }
        capabilities.APPEND(capability)

    END FOR

    // STEP 5: RETURN STRUCTURED RESULT
    RETURN {
        domain: {
            uri: domainUri,
            name: domainName,
            label: domainLabel
        },
        capabilities: capabilities,
        count: capabilities.length
    }

END
```

**Time Complexity**: O(n) where n = number of (domain, hasCapability, capability) triples
**Space Complexity**: O(k) where k = number of capabilities found
**Error Conditions**:
- store is null
- domainName is empty or invalid
- Domain does not exist in ProbeUniverse
- Store.selectTriples fails

---

## ALGORITHM 3: APPEND EVENT TO IMMUTABLE LOG

**Purpose**: Add an immutable event record to ProbeEventLog with state hash chain.

**Input**:
- `store` (KnowledgeStore instance)
- `operationType` (string: "OBSERVATION" | "CONFLICT" | "CONFIG_UPDATE")
- `agentId` (string)
- `payload` (object, serializable to JSON)

**Output**:
- `{eventUri: string, logIndex: bigint, timestamp_ns: bigint, stateHashAfter: string}`

```
ALGORITHM: AppendEventToLog

CONSTANTS:
    NS_RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    NS_PROBE = "http://unrdf.org/probe/ontology/"
    NS_KGC = "http://unrdf.org/kgc/probe/"
    NS_XSD = "http://www.w3.org/2001/XMLSchema#"
    EVENT_LOG_GRAPH = NS_KGC + "ProbeEventLog"
    VALID_OPERATIONS = ["OBSERVATION", "CONFLICT", "CONFIG_UPDATE"]

VARIABLES:
    store: KnowledgeStore
    operationType: string
    agentId: string
    payload: object
    eventUri: string
    logIndex: bigint
    timestamp_ns: bigint
    stateBefore: StateCommitment
    stateAfter: StateCommitment
    eventLogGraph: NamedNode

BEGIN

    // INPUT VALIDATION
    IF store == null THEN
        THROW TypeError("store cannot be null")
    END IF

    IF typeof operationType != "string" OR NOT(operationType IN VALID_OPERATIONS) THEN
        THROW TypeError(f"operationType must be one of {VALID_OPERATIONS}")
    END IF

    IF typeof agentId != "string" OR agentId.length == 0 THEN
        THROW TypeError("agentId must be non-empty string")
    END IF

    IF payload == null OR typeof payload != "object" THEN
        THROW TypeError("payload must be an object")
    END IF

    // STEP 1: GET CURRENT STATE BEFORE
    TRY
        stateBefore ← AWAIT store.getStateCommitment()
    CATCH error
        THROW Error("Failed to get state commitment before: " + error.message)
    END TRY

    // STEP 2: GENERATE EVENT URI (DETERMINISTIC)
    logIndex ← store.getLogIndex()
    timestamp_ns ← CurrentTimeNanoseconds()

    // Ensure deterministic: hash(operation + agent + logIndex + timestamp)
    eventData ← f"{operationType}|{agentId}|{logIndex}|{timestamp_ns}"
    eventHash ← SHA256(eventData)
    eventUri ← NS_KGC + "event#" + eventHash
    eventNode ← dataFactory.namedNode(eventUri)
    eventLogGraphNode ← dataFactory.namedNode(EVENT_LOG_GRAPH)

    // STEP 3: CREATE QUADS FOR EVENT
    quads ← []

    // Quad 1: Type assertion
    quad1 ← dataFactory.quad(
        eventNode,
        dataFactory.namedNode(NS_RDF + "type"),
        dataFactory.namedNode(NS_PROBE + "Event"),
        eventLogGraphNode
    )
    quads.APPEND(quad1)

    // Quad 2: Timestamp
    quad2 ← dataFactory.quad(
        eventNode,
        dataFactory.namedNode(NS_PROBE + "timestamp_ns"),
        dataFactory.literal(
            timestamp_ns.toString(),
            dataFactory.namedNode(NS_XSD + "long")
        ),
        eventLogGraphNode
    )
    quads.APPEND(quad2)

    // Quad 3: Operation type
    quad3 ← dataFactory.quad(
        eventNode,
        dataFactory.namedNode(NS_PROBE + "operationType"),
        dataFactory.literal(operationType),
        eventLogGraphNode
    )
    quads.APPEND(quad3)

    // Quad 4: Agent ID
    quad4 ← dataFactory.quad(
        eventNode,
        dataFactory.namedNode(NS_PROBE + "agentId"),
        dataFactory.literal(agentId),
        eventLogGraphNode
    )
    quads.APPEND(quad4)

    // Quad 5: Log index (CRITICAL: must be sequential)
    quad5 ← dataFactory.quad(
        eventNode,
        dataFactory.namedNode(NS_PROBE + "logIndex"),
        dataFactory.literal(
            logIndex.toString(),
            dataFactory.namedNode(NS_XSD + "long")
        ),
        eventLogGraphNode
    )
    quads.APPEND(quad5)

    // Quad 6: Payload (JSON)
    payloadJson ← JSON.stringify(payload)
    quad6 ← dataFactory.quad(
        eventNode,
        dataFactory.namedNode(NS_PROBE + "payload"),
        dataFactory.literal(payloadJson),
        eventLogGraphNode
    )
    quads.APPEND(quad6)

    // Quad 7: State hash BEFORE
    quad7 ← dataFactory.quad(
        eventNode,
        dataFactory.namedNode(NS_PROBE + "stateHashBefore"),
        dataFactory.literal(stateBefore.state_hash),
        eventLogGraphNode
    )
    quads.APPEND(quad7)

    // STEP 4: APPEND ALL QUADS (ATOMIC)
    TRY
        FOR EACH quad IN quads DO
            AWAIT store.appendTriple(
                'add',
                quad.subject,
                quad.predicate,
                quad.object,
                quad.graph
            )
        END FOR
    CATCH error
        THROW Error("Failed to append event quads: " + error.message)
    END TRY

    // STEP 5: GET STATE AFTER
    TRY
        stateAfter ← AWAIT store.getStateCommitment()
    CATCH error
        THROW Error("Failed to get state commitment after: " + error.message)
    END TRY

    // Quad 8: State hash AFTER
    quad8 ← dataFactory.quad(
        eventNode,
        dataFactory.namedNode(NS_PROBE + "stateHashAfter"),
        dataFactory.literal(stateAfter.state_hash),
        eventLogGraphNode
    )

    AWAIT store.appendTriple(
        'add',
        quad8.subject,
        quad8.predicate,
        quad8.object,
        quad8.graph
    )

    // STEP 6: VERIFY APPEND-ONLY PROPERTY
    // Confirm this event's logIndex is logIndex, not logIndex + 1
    // (store.getLogIndex() advances after each append)
    actualIndex ← logIndex
    IF store.getLogIndex() != (logIndex + NUM_QUADS_APPENDED) THEN
        THROW Error("Event log index mismatch - log may be corrupted")
    END IF

    // STEP 7: RETURN RESULT
    RETURN {
        eventUri: eventUri,
        logIndex: logIndex,
        timestamp_ns: timestamp_ns,
        stateHashBefore: stateBefore.state_hash,
        stateHashAfter: stateAfter.state_hash
    }

END
```

**Time Complexity**: O(1) - constant number of quads appended
**Space Complexity**: O(1) - fixed quad count (8-9 quads per event)
**Error Conditions**:
- store is null
- operationType not in valid set
- agentId is empty
- payload is not serializable to JSON
- store.getStateCommitment fails
- Log index is not sequential

---

## ALGORITHM 4: VERIFY STATE HASH CHAIN

**Purpose**: Cryptographically verify integrity of ProbeEventLog by checking state hash chain.

**Input**:
- `store` (KnowledgeStore instance)

**Output**:
- `{isValid: boolean, errors: Array<string>, chainLength: number}`

```
ALGORITHM: VerifyStateHashChain

CONSTANTS:
    NS_RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    NS_PROBE = "http://unrdf.org/probe/ontology/"
    NS_KGC = "http://unrdf.org/kgc/probe/"
    EVENT_LOG_GRAPH = NS_KGC + "ProbeEventLog"

VARIABLES:
    store: KnowledgeStore
    events: Array<Quad>
    errors: Array<string>
    eventData: Array
    isValid: boolean

BEGIN

    // INPUT VALIDATION
    IF store == null THEN
        THROW TypeError("store cannot be null")
    END IF

    errors ← []
    eventLogGraphNode ← dataFactory.namedNode(EVENT_LOG_GRAPH)

    // STEP 1: RETRIEVE ALL EVENTS
    eventQuads ← store.selectTriples({
        predicate: dataFactory.namedNode(NS_RDF + "type"),
        object: dataFactory.namedNode(NS_PROBE + "Event"),
        graph: eventLogGraphNode
    })

    IF eventQuads.size == 0 THEN
        // Empty log is technically valid
        RETURN {
            isValid: true,
            errors: [],
            chainLength: 0
        }
    END IF

    // STEP 2: BUILD EVENT OBJECTS WITH METADATA
    eventData ← []

    FOR EACH eventQuad IN eventQuads DO

        eventUri ← eventQuad.subject
        eventObj ← {uri: eventUri, index: null, timestamp: null, hashBefore: null, hashAfter: null}

        // Get log index
        indexQuads ← store.selectTriples({
            subject: eventUri,
            predicate: dataFactory.namedNode(NS_PROBE + "logIndex"),
            graph: eventLogGraphNode
        })

        IF indexQuads.size > 0 THEN
            eventObj.index ← PARSE_INT(indexQuads[0].object.value)
        ELSE
            errors.APPEND(f"Event {eventUri} missing logIndex")
        END IF

        // Get timestamp
        tsQuads ← store.selectTriples({
            subject: eventUri,
            predicate: dataFactory.namedNode(NS_PROBE + "timestamp_ns"),
            graph: eventLogGraphNode
        })

        IF tsQuads.size > 0 THEN
            eventObj.timestamp ← PARSE_BIGINT(tsQuads[0].object.value)
        ELSE
            errors.APPEND(f"Event {eventUri} missing timestamp")
        END IF

        // Get state hash before
        hashBeforeQuads ← store.selectTriples({
            subject: eventUri,
            predicate: dataFactory.namedNode(NS_PROBE + "stateHashBefore"),
            graph: eventLogGraphNode
        })

        IF hashBeforeQuads.size > 0 THEN
            eventObj.hashBefore ← hashBeforeQuads[0].object.value
        ELSE
            errors.APPEND(f"Event {eventUri} missing stateHashBefore")
        END IF

        // Get state hash after
        hashAfterQuads ← store.selectTriples({
            subject: eventUri,
            predicate: dataFactory.namedNode(NS_PROBE + "stateHashAfter"),
            graph: eventLogGraphNode
        })

        IF hashAfterQuads.size > 0 THEN
            eventObj.hashAfter ← hashAfterQuads[0].object.value
        ELSE
            errors.APPEND(f"Event {eventUri} missing stateHashAfter")
        END IF

        eventData.APPEND(eventObj)

    END FOR

    // STEP 3: SORT BY LOG INDEX
    eventData.SORT((a, b) => a.index - b.index)

    // STEP 4: VERIFY SEQUENTIAL INDICES
    FOR i = 0 TO eventData.length - 1 DO
        IF eventData[i].index != i THEN
            errors.APPEND(f"Log index not sequential: expected {i}, got {eventData[i].index}")
        END IF
    END FOR

    // STEP 5: VERIFY STATE HASH CHAIN
    FOR i = 0 TO eventData.length - 2 DO

        currentEvent ← eventData[i]
        nextEvent ← eventData[i + 1]

        // Critical: current's hashAfter must equal next's hashBefore
        IF currentEvent.hashAfter != nextEvent.hashBefore THEN
            errors.APPEND(
                f"State hash chain broken at event {i}: " +
                f"current.hashAfter={currentEvent.hashAfter} != " +
                f"next.hashBefore={nextEvent.hashBefore}"
            )
        END IF

    END FOR

    // STEP 6: VERIFY TIMESTAMPS ARE INCREASING
    FOR i = 0 TO eventData.length - 2 DO

        currentEvent ← eventData[i]
        nextEvent ← eventData[i + 1]

        IF currentEvent.timestamp >= nextEvent.timestamp THEN
            errors.APPEND(
                f"Timestamp not increasing at event {i}: " +
                f"current={currentEvent.timestamp} >= next={nextEvent.timestamp}"
            )
        END IF

    END FOR

    // STEP 7: DETERMINE VALIDITY
    isValid ← (errors.length == 0)

    // STEP 8: RETURN RESULT
    RETURN {
        isValid: isValid,
        errors: errors,
        chainLength: eventData.length,
        firstEventIndex: IF eventData.length > 0 THEN eventData[0].index ELSE null,
        lastEventIndex: IF eventData.length > 0 THEN eventData[eventData.length - 1].index ELSE null,
        totalHashErrors: COUNT_ERRORS_WITH("State hash chain broken")
    }

END
```

**Time Complexity**: O(e) where e = number of events (must verify all links)
**Space Complexity**: O(e) for eventData array
**Returns**: Detailed validation report with all errors
**Error Conditions**:
- store is null
- Quads cannot be parsed
- State hash chain is broken
- Log indices not sequential

---

## ALGORITHM 5: QUERY CONFLICTS BETWEEN AGENTS

**Purpose**: Find conflicting observations where different agents claim incompatible facts about the same domain.

**Input**:
- `store` (KnowledgeStore instance)
- `claimHash` (string, SHA256 hex - optional for specific claim)
- `agentA` (string - optional filter)
- `agentB` (string - optional filter)

**Output**:
- `Array<{obsA: string, agentA: string, obsB: string, agentB: string, domain: string, claimHash: string}>`

```
ALGORITHM: QueryConflictingObservations

CONSTANTS:
    NS_RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    NS_PROBE = "http://unrdf.org/probe/ontology/"
    NS_KGC = "http://unrdf.org/kgc/probe/"
    UNIVERSE_GRAPH = NS_KGC + "ProbeUniverse"

VARIABLES:
    store: KnowledgeStore
    claimHash: string (optional)
    agentA: string (optional)
    agentB: string (optional)
    conflicts: Array
    observations: Array

BEGIN

    // INPUT VALIDATION
    IF store == null THEN
        THROW TypeError("store cannot be null")
    END IF

    universeGraphNode ← dataFactory.namedNode(UNIVERSE_GRAPH)
    conflictsWithPred ← dataFactory.namedNode(NS_PROBE + "conflictsWith")

    // STEP 1: FIND ALL OBSERVATIONS WITH CONFLICTS
    // Query: ?obs1 probe:conflictsWith ?obs2
    conflictQuads ← store.selectTriples({
        predicate: conflictsWithPred,
        graph: universeGraphNode
    })

    IF conflictQuads.size == 0 THEN
        RETURN []
    END IF

    conflicts ← []

    // STEP 2: FOR EACH CONFLICT, GET METADATA
    FOR EACH conflictQuad IN conflictQuads DO

        obsA ← conflictQuad.subject
        obsB ← conflictQuad.object

        // Get agentId for obsA
        agentAQuads ← store.selectTriples({
            subject: obsA,
            predicate: dataFactory.namedNode(NS_PROBE + "agentId"),
            graph: universeGraphNode
        })

        agentAId ← IF agentAQuads.size > 0 THEN agentAQuads[0].object.value ELSE null

        // Get agentId for obsB
        agentBQuads ← store.selectTriples({
            subject: obsB,
            predicate: dataFactory.namedNode(NS_PROBE + "agentId"),
            graph: universeGraphNode
        })

        agentBId ← IF agentBQuads.size > 0 THEN agentBQuads[0].object.value ELSE null

        // Get domain for obsA
        domainAQuads ← store.selectTriples({
            subject: obsA,
            predicate: dataFactory.namedNode(NS_PROBE + "domain"),
            graph: universeGraphNode
        })

        domainA ← IF domainAQuads.size > 0 THEN domainAQuads[0].object.value ELSE null

        // Get claimHash for obsA
        claimHashQuads ← store.selectTriples({
            subject: obsA,
            predicate: dataFactory.namedNode(NS_PROBE + "claimHash"),
            graph: universeGraphNode
        })

        claimHashVal ← IF claimHashQuads.size > 0 THEN claimHashQuads[0].object.value ELSE null

        // STEP 3: APPLY OPTIONAL FILTERS
        shouldInclude ← true

        IF claimHash != null AND claimHashVal != claimHash THEN
            shouldInclude ← false
        END IF

        IF agentA != null AND agentAId != agentA THEN
            shouldInclude ← false
        END IF

        IF agentB != null AND agentBId != agentB THEN
            shouldInclude ← false
        END IF

        IF shouldInclude THEN
            conflictRecord ← {
                obsA: obsA.value,
                agentA: agentAId,
                obsB: obsB.value,
                agentB: agentBId,
                domain: domainA,
                claimHash: claimHashVal
            }
            conflicts.APPEND(conflictRecord)
        END IF

    END FOR

    RETURN conflicts

END
```

**Time Complexity**: O(c + k) where c = number of conflict quads, k = number of results
**Space Complexity**: O(k) for results array
**Filters**: Optional claimHash, agentA, agentB for narrowing results
**Returns**: Array of conflict records with all metadata

---

## ALGORITHM 6: TIMELINE OF EVENTS (VERSIONING)

**Purpose**: Retrieve events in chronological order to track system state changes between versions.

**Input**:
- `store` (KnowledgeStore instance)
- `startLogIndex` (bigint, optional - start from this index)
- `endLogIndex` (bigint, optional - end at this index)
- `limit` (number, optional - max results, default 1000)

**Output**:
- `Array<{logIndex, timestamp_ns, operationType, agentId, stateHashBefore, stateHashAfter}>`

```
ALGORITHM: TimelineOfChanges

CONSTANTS:
    NS_RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    NS_PROBE = "http://unrdf.org/probe/ontology/"
    NS_KGC = "http://unrdf.org/kgc/probe/"
    EVENT_LOG_GRAPH = NS_KGC + "ProbeEventLog"
    DEFAULT_LIMIT = 1000

VARIABLES:
    store: KnowledgeStore
    startLogIndex: bigint
    endLogIndex: bigint
    limit: number
    events: Array
    timeline: Array

BEGIN

    // INPUT VALIDATION
    IF store == null THEN
        THROW TypeError("store cannot be null")
    END IF

    IF limit == null THEN
        limit ← DEFAULT_LIMIT
    END IF

    IF limit <= 0 OR limit > 100000 THEN
        THROW TypeError("limit must be between 1 and 100000")
    END IF

    eventLogGraphNode ← dataFactory.namedNode(EVENT_LOG_GRAPH)

    // STEP 1: RETRIEVE ALL EVENTS
    eventQuads ← store.selectTriples({
        predicate: dataFactory.namedNode(NS_RDF + "type"),
        object: dataFactory.namedNode(NS_PROBE + "Event"),
        graph: eventLogGraphNode
    })

    IF eventQuads.size == 0 THEN
        RETURN []
    END IF

    // STEP 2: BUILD EVENT OBJECTS
    timeline ← []

    FOR EACH eventQuad IN eventQuads DO

        eventUri ← eventQuad.subject

        // Get logIndex
        indexQuads ← store.selectTriples({
            subject: eventUri,
            predicate: dataFactory.namedNode(NS_PROBE + "logIndex"),
            graph: eventLogGraphNode
        })

        logIndex ← null
        IF indexQuads.size > 0 THEN
            logIndex ← PARSE_BIGINT(indexQuads[0].object.value)
        ELSE
            CONTINUE  // Skip events without logIndex
        END IF

        // Apply optional index filters
        IF startLogIndex != null AND logIndex < startLogIndex THEN
            CONTINUE
        END IF

        IF endLogIndex != null AND logIndex > endLogIndex THEN
            CONTINUE
        END IF

        // Get timestamp
        tsQuads ← store.selectTriples({
            subject: eventUri,
            predicate: dataFactory.namedNode(NS_PROBE + "timestamp_ns"),
            graph: eventLogGraphNode
        })

        timestamp_ns ← IF tsQuads.size > 0 THEN
            PARSE_BIGINT(tsQuads[0].object.value)
        ELSE
            null
        END IF

        // Get operation type
        opQuads ← store.selectTriples({
            subject: eventUri,
            predicate: dataFactory.namedNode(NS_PROBE + "operationType"),
            graph: eventLogGraphNode
        })

        operationType ← IF opQuads.size > 0 THEN
            opQuads[0].object.value
        ELSE
            null
        END IF

        // Get agent ID
        agentQuads ← store.selectTriples({
            subject: eventUri,
            predicate: dataFactory.namedNode(NS_PROBE + "agentId"),
            graph: eventLogGraphNode
        })

        agentId ← IF agentQuads.size > 0 THEN
            agentQuads[0].object.value
        ELSE
            null
        END IF

        // Get state hash before
        hashBeforeQuads ← store.selectTriples({
            subject: eventUri,
            predicate: dataFactory.namedNode(NS_PROBE + "stateHashBefore"),
            graph: eventLogGraphNode
        })

        stateHashBefore ← IF hashBeforeQuads.size > 0 THEN
            hashBeforeQuads[0].object.value
        ELSE
            null
        END IF

        // Get state hash after
        hashAfterQuads ← store.selectTriples({
            subject: eventUri,
            predicate: dataFactory.namedNode(NS_PROBE + "stateHashAfter"),
            graph: eventLogGraphNode
        })

        stateHashAfter ← IF hashAfterQuads.size > 0 THEN
            hashAfterQuads[0].object.value
        ELSE
            null
        END IF

        // Build event record
        eventRecord ← {
            logIndex: logIndex,
            timestamp_ns: timestamp_ns,
            operationType: operationType,
            agentId: agentId,
            stateHashBefore: stateHashBefore,
            stateHashAfter: stateHashAfter
        }

        timeline.APPEND(eventRecord)

    END FOR

    // STEP 3: SORT BY LOG INDEX (SHOULD ALREADY BE ORDERED)
    timeline.SORT((a, b) => COMPARE_BIGINT(a.logIndex, b.logIndex))

    // STEP 4: APPLY LIMIT
    IF timeline.length > limit THEN
        timeline ← timeline.SLICE(0, limit)
    END IF

    RETURN timeline

END
```

**Time Complexity**: O(e) where e = number of events (must scan all)
**Space Complexity**: O(min(e, limit))
**Sorting**: Events should already be in logIndex order (append-only guarantee)
**Returns**: Chronologically ordered timeline with state transitions

---

## SUMMARY OF ALGORITHMS

| Algorithm | Time | Space | Purpose |
|-----------|------|-------|---------|
| ObservationToQuads | O(k) | O(k) | Create quads from Observation |
| QueryCapabilitiesByDomain | O(n) | O(m) | Find capabilities in domain |
| AppendEventToLog | O(1) | O(1) | Add immutable event record |
| VerifyStateHashChain | O(e) | O(e) | Cryptographically verify log |
| QueryConflictingObservations | O(c) | O(k) | Find conflicting observations |
| TimelineOfChanges | O(e) | O(min(e,limit)) | Retrieve state change history |

**Key Invariants Maintained**:
1. Canonical URI generation (same data → same URI)
2. Sequential log indices (no gaps, no reordering)
3. State hash chain (each event's after = next event's before)
4. Immutability (only append, never modify or delete events)
5. Atomicity (all quads for an operation append together)
