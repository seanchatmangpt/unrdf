# SPARC Pseudocode: RDF Schema for KGC Probe Package

## Overview

This document defines three RDF graphs using Oxigraph + N3 quads for the KGC probe package:

1. **kgc:ProbeUniverse** - Materialized observation objects (current O state)
2. **kgc:ProbeEventLog** - Append-only event log (immutable history)
3. **kgc:ProbeSystem** - System configuration and allowlists

All graphs are stored via `KnowledgeStore` from `@unrdf/kgc-substrate` for deterministic snapshots and state commitment.

---

## PART 1: ONTOLOGY PREFIXES AND NAMESPACE DEFINITIONS

### Prefix Declarations

```
PREFIX kgc: <http://unrdf.org/kgc/probe/>
PREFIX probe: <http://unrdf.org/probe/ontology/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
```

### Core Ontology Classes

```
probe:Observation
    rdfs:label "Observation"@en
    rdfs:comment "A single observation captured from a probe"@en
    rdf:type owl:Class

probe:Event
    rdfs:label "Event"@en
    rdfs:comment "An immutable event in the probe audit log"@en
    rdf:type owl:Class

probe:SystemConfig
    rdfs:label "SystemConfig"@en
    rdfs:comment "System configuration and policy"@en
    rdf:type owl:Class

probe:Capability
    rdfs:label "Capability"@en
    rdfs:comment "A capability or feature available in a domain"@en
    rdf:type owl:Class

probe:Domain
    rdfs:label "Domain"@en
    rdfs:comment "A domain (e.g., filesystem, network, process)"@en
    rdf:type owl:Class

probe:Conflict
    rdfs:label "Conflict"@en
    rdfs:comment "A conflict between agent claims"@en
    rdf:type owl:Class
```

### Core Ontology Properties

```
probe:timestamp_ns
    rdf:type owl:DatatypeProperty
    rdfs:domain probe:Observation, probe:Event
    rdfs:range xsd:long
    rdfs:label "Timestamp (nanoseconds)"@en

probe:agentId
    rdf:type owl:DatatypeProperty
    rdfs:domain probe:Event, probe:Observation
    rdfs:range xsd:string
    rdfs:label "Agent ID"@en

probe:domain
    rdf:type owl:ObjectProperty
    rdfs:domain probe:Observation, probe:Capability
    rdfs:range probe:Domain
    rdfs:label "Domain"@en

probe:claimHash
    rdf:type owl:DatatypeProperty
    rdfs:domain probe:Observation
    rdfs:range xsd:string
    rdfs:label "Claim Hash (SHA256)"@en

probe:evidence
    rdf:type owl:DatatypeProperty
    rdfs:domain probe:Observation
    rdfs:range xsd:string
    rdfs:label "Evidence (serialized JSON)"@en

probe:conflictsWith
    rdf:type owl:ObjectProperty
    rdfs:domain probe:Observation
    rdfs:range probe:Observation
    rdfs:label "Conflicts With"@en

probe:hasCapability
    rdf:type owl:ObjectProperty
    rdfs:domain probe:Domain
    rdfs:range probe:Capability
    rdfs:label "Has Capability"@en

probe:allowedNetworkUrls
    rdf:type owl:DatatypeProperty
    rdfs:domain probe:SystemConfig
    rdfs:range rdf:List
    rdfs:label "Allowed Network URLs"@en

probe:fsRoots
    rdf:type owl:DatatypeProperty
    rdfs:domain probe:SystemConfig
    rdfs:range rdf:List
    rdfs:label "Filesystem Roots"@en

probe:allowedAgents
    rdf:type owl:DatatypeProperty
    rdfs:domain probe:SystemConfig
    rdfs:range rdf:List
    rdfs:label "Allowed Agent IDs"@en

probe:operationType
    rdf:type owl:DatatypeProperty
    rdfs:domain probe:Event
    rdfs:range xsd:string
    rdfs:label "Operation Type"@en

probe:observationCount
    rdf:type owl:DatatypeProperty
    rdfs:domain probe:Event
    rdfs:range xsd:long
    rdfs:label "Observation Count"@en

probe:stateHash
    rdf:type owl:DatatypeProperty
    rdfs:domain probe:SystemConfig
    rdfs:range xsd:string
    rdfs:label "State Hash (BLAKE3)"@en
```

---

## PART 2: GRAPH STRUCTURES

### Graph 1: ProbeUniverse (Materialized Observations)

**Purpose**: Store current snapshot of all O observations with their claims, evidence, and conflicts.

**Named Graph IRI**: `kgc:ProbeUniverse`

**Canonical Quad Ordering**: (Subject, Predicate, Object, Graph)

#### Schema

```
GRAPH kgc:ProbeUniverse {
    ?observation rdf:type probe:Observation
    ?observation probe:timestamp_ns ?ts (xsd:long, nanoseconds)
    ?observation probe:agentId ?agentId (xsd:string)
    ?observation probe:domain ?domain (probe:Domain instance)
    ?observation probe:claimHash ?claimHash (xsd:string, SHA256)
    ?observation probe:evidence ?evidence (xsd:string, JSON serialized)
    ?observation probe:conflictsWith ?conflictingObs (optional, links to other observations)

    ?domain rdf:type probe:Domain
    ?domain rdfs:label ?domainLabel (xsd:string)
    ?domain probe:hasCapability ?capability+

    ?capability rdf:type probe:Capability
    ?capability rdfs:label ?capLabel (xsd:string)
    ?capability rdf:value ?capValue (xsd:string)
}
```

#### Example Quads

```javascript
// Observation 1: Agent-1 observes filesystem capability
observation:obs-uuid-1 rdf:type probe:Observation
    in graph kgc:ProbeUniverse

observation:obs-uuid-1 probe:timestamp_ns "1735366800000000000"^^xsd:long
    in graph kgc:ProbeUniverse

observation:obs-uuid-1 probe:agentId "agent-1"
    in graph kgc:ProbeUniverse

observation:obs-uuid-1 probe:domain domain:filesystem
    in graph kgc:ProbeUniverse

observation:obs-uuid-1 probe:claimHash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    in graph kgc:ProbeUniverse

observation:obs-uuid-1 probe:evidence "{
  \"paths\": [\"/etc/passwd\", \"/home\"],
  \"readable\": true,
  \"writable\": false,
  \"timestamp\": \"2025-12-27T10:00:00Z\"
}"
    in graph kgc:ProbeUniverse

domain:filesystem rdf:type probe:Domain
    in graph kgc:ProbeUniverse

domain:filesystem rdfs:label "Filesystem"
    in graph kgc:ProbeUniverse

domain:filesystem probe:hasCapability cap:read-files
    in graph kgc:ProbeUniverse

cap:read-files rdf:type probe:Capability
    in graph kgc:ProbeUniverse

cap:read-files rdfs:label "Read Files"
    in graph kgc:ProbeUniverse

cap:read-files rdf:value "true"
    in graph kgc:ProbeUniverse
```

**Insertion Algorithm**:

```
ALGORITHM: MaterializeObservationToQuads
INPUT: observation (Observation object), store (KnowledgeStore)
OUTPUT: Set of quad indices in append log

VARIABLES:
    obsUri ← URI for observation (e.g., observation:{UUID})
    domainUri ← URI for domain (e.g., domain:{domain_name})
    capUris ← Set of capability URIs
    quads ← List of quads to append
    universeGraph ← dataFactory.namedNode('kgc:ProbeUniverse')

BEGIN
    // Canonicalize observation object
    obsCanonical ← CanonicalizeObservation(observation)

    // Create observation URI (deterministic from claim hash)
    obsUri ← CreateDeterministicUri('observation', obsCanonical.claimHash)

    // Create domain URI
    domainUri ← CreateDeterministicUri('domain', obsCanonical.domain)

    // Quad 1: Type assertion
    quad_1 ← dataFactory.quad(
        dataFactory.namedNode(obsUri),
        dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        dataFactory.namedNode('http://unrdf.org/probe/ontology/Observation'),
        universeGraph
    )
    quads.append(quad_1)

    // Quad 2: Timestamp
    quad_2 ← dataFactory.quad(
        dataFactory.namedNode(obsUri),
        dataFactory.namedNode('http://unrdf.org/probe/ontology/timestamp_ns'),
        dataFactory.literal(observation.timestamp_ns.toString(),
            dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#long')),
        universeGraph
    )
    quads.append(quad_2)

    // Quad 3: Agent ID
    quad_3 ← dataFactory.quad(
        dataFactory.namedNode(obsUri),
        dataFactory.namedNode('http://unrdf.org/probe/ontology/agentId'),
        dataFactory.literal(observation.agentId),
        universeGraph
    )
    quads.append(quad_3)

    // Quad 4: Domain
    quad_4 ← dataFactory.quad(
        dataFactory.namedNode(obsUri),
        dataFactory.namedNode('http://unrdf.org/probe/ontology/domain'),
        dataFactory.namedNode(domainUri),
        universeGraph
    )
    quads.append(quad_4)

    // Quad 5: Claim Hash
    quad_5 ← dataFactory.quad(
        dataFactory.namedNode(obsUri),
        dataFactory.namedNode('http://unrdf.org/probe/ontology/claimHash'),
        dataFactory.literal(obsCanonical.claimHash),
        universeGraph
    )
    quads.append(quad_5)

    // Quad 6: Evidence (JSON serialized)
    quad_6 ← dataFactory.quad(
        dataFactory.namedNode(obsUri),
        dataFactory.namedNode('http://unrdf.org/probe/ontology/evidence'),
        dataFactory.literal(JSON.stringify(observation.evidence)),
        universeGraph
    )
    quads.append(quad_6)

    // Quads 7+: Domain type and capabilities
    quad_domain_type ← dataFactory.quad(
        dataFactory.namedNode(domainUri),
        dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        dataFactory.namedNode('http://unrdf.org/probe/ontology/Domain'),
        universeGraph
    )
    quads.append(quad_domain_type)

    // FOR EACH capability in domain
    FOR EACH capability IN observation.evidence.capabilities DO
        capUri ← CreateDeterministicUri('capability', capability.name)

        // Domain -> hasCapability edge
        quad_has_cap ← dataFactory.quad(
            dataFactory.namedNode(domainUri),
            dataFactory.namedNode('http://unrdf.org/probe/ontology/hasCapability'),
            dataFactory.namedNode(capUri),
            universeGraph
        )
        quads.append(quad_has_cap)

        // Capability type
        quad_cap_type ← dataFactory.quad(
            dataFactory.namedNode(capUri),
            dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
            dataFactory.namedNode('http://unrdf.org/probe/ontology/Capability'),
            universeGraph
        )
        quads.append(quad_cap_type)

        // Capability value
        quad_cap_value ← dataFactory.quad(
            dataFactory.namedNode(capUri),
            dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#value'),
            dataFactory.literal(capability.value.toString()),
            universeGraph
        )
        quads.append(quad_cap_value)
    END FOR

    // Append all quads to store
    indices ← SET()
    FOR EACH quad IN quads DO
        { index, timestamp_ns } ← await store.appendTriple(
            'add',
            quad.subject,
            quad.predicate,
            quad.object,
            quad.graph
        )
        indices.add(index)
    END FOR

    RETURN indices
END

SUBROUTINE: CanonicalizeObservation
INPUT: observation (Observation object)
OUTPUT: observation with canonical field order and string values

BEGIN
    canonicalObs ← {
        agentId: observation.agentId,
        domain: observation.domain,
        timestamp_ns: observation.timestamp_ns,
        claimHash: SHA256(SerializeCanonical(observation.claim)),
        evidence: SerializeCanonical(observation.evidence)
    }

    // Lexicographic sort of fields
    RETURN SortByKey(canonicalObs)
END

SUBROUTINE: CreateDeterministicUri
INPUT: prefix (string), value (string)
OUTPUT: deterministic URI string

BEGIN
    hash ← SHA256(value)
    RETURN f"http://unrdf.org/kgc/probe/{prefix}#{hash}"
END
```

---

### Graph 2: ProbeEventLog (Append-Only Event Log)

**Purpose**: Store immutable audit log of all probe system events (observations, conflicts, state changes).

**Named Graph IRI**: `kgc:ProbeEventLog`

**Immutability Contract**: Only append operations allowed. No deletion/modification.

#### Schema

```
GRAPH kgc:ProbeEventLog {
    ?event rdf:type probe:Event
    ?event probe:timestamp_ns ?ts (xsd:long, nanoseconds UTC)
    ?event probe:operationType ?opType (xsd:string: "OBSERVATION", "CONFLICT", "CONFIG_UPDATE")
    ?event probe:agentId ?agentId (xsd:string)
    ?event probe:logIndex ?index (xsd:long, sequential)
    ?event probe:payload ?payload (xsd:string, JSON serialized)
    ?event probe:stateHashBefore ?hashBefore (xsd:string, BLAKE3)
    ?event probe:stateHashAfter ?hashAfter (xsd:string, BLAKE3)
    ?event rdfs:comment ?description (xsd:string, optional)
}
```

#### Example Quads

```javascript
// Event 1: Initial observation logged
event:evt-001 rdf:type probe:Event
    in graph kgc:ProbeEventLog

event:evt-001 probe:timestamp_ns "1735366800000000000"^^xsd:long
    in graph kgc:ProbeEventLog

event:evt-001 probe:operationType "OBSERVATION"
    in graph kgc:ProbeEventLog

event:evt-001 probe:agentId "agent-1"
    in graph kgc:ProbeEventLog

event:evt-001 probe:logIndex "0"^^xsd:long
    in graph kgc:ProbeEventLog

event:evt-001 probe:payload "{
  \"observationId\": \"observation:obs-uuid-1\",
  \"domain\": \"filesystem\",
  \"claimHash\": \"e3b0c44...\",
  \"evidence\": {...}
}"
    in graph kgc:ProbeEventLog

event:evt-001 probe:stateHashBefore "0000000000000000000000000000000000000000"
    in graph kgc:ProbeEventLog

event:evt-001 probe:stateHashAfter "abc123def456..."
    in graph kgc:ProbeEventLog

// Event 2: Conflict detected
event:evt-002 rdf:type probe:Event
    in graph kgc:ProbeEventLog

event:evt-002 probe:timestamp_ns "1735366820000000000"^^xsd:long
    in graph kgc:ProbeEventLog

event:evt-002 probe:operationType "CONFLICT"
    in graph kgc:ProbeEventLog

event:evt-002 probe:agentId "system"
    in graph kgc:ProbeEventLog

event:evt-002 probe:logIndex "1"^^xsd:long
    in graph kgc:ProbeEventLog

event:evt-002 probe:payload "{
  \"type\": \"CLAIM_CONFLICT\",
  \"claimA\": \"observation:obs-uuid-1\",
  \"claimB\": \"observation:obs-uuid-2\",
  \"reason\": \"Domain capability mismatch\"
}"
    in graph kgc:ProbeEventLog

event:evt-002 probe:stateHashBefore "abc123def456..."
    in graph kgc:ProbeEventLog

event:evt-002 probe:stateHashAfter "def789ghi012..."
    in graph kgc:ProbeEventLog

rdfs:comment "Conflict between agent-1 and agent-2 claims on filesystem access"
```

**Append Algorithm**:

```
ALGORITHM: AppendEventToLog
INPUT:
    operation (string: "OBSERVATION" | "CONFLICT" | "CONFIG_UPDATE")
    agentId (string)
    payload (object)
    store (KnowledgeStore)
OUTPUT: Event URI and log index

VARIABLES:
    eventLog ← kgc:ProbeEventLog graph
    logIndex ← store.getLogIndex()
    timestamp_ns ← CurrentTimeNanoseconds()
    stateBefore ← await store.getStateCommitment()
    eventUri ← null

BEGIN
    // Create deterministic event URI
    eventHash ← SHA256(f"{operation}|{agentId}|{timestamp_ns}|{logIndex}")
    eventUri ← f"http://unrdf.org/kgc/probe/event#{eventHash}"

    // Verify append-only property: logIndex must be sequential
    IF logIndex != ExpectedNextIndex THEN
        THROW error("Event log is not sequential!")
    END IF

    // Quads to append
    quads ← [
        {subject: eventUri, predicate: rdf:type, object: probe:Event},
        {subject: eventUri, predicate: probe:timestamp_ns, object: timestamp_ns},
        {subject: eventUri, predicate: probe:operationType, object: operation},
        {subject: eventUri, predicate: probe:agentId, object: agentId},
        {subject: eventUri, predicate: probe:logIndex, object: logIndex},
        {subject: eventUri, predicate: probe:payload, object: JSON.stringify(payload)},
        {subject: eventUri, predicate: probe:stateHashBefore, object: stateBefore.state_hash},
    ]

    // Append to store
    FOR EACH quad IN quads DO
        await store.appendTriple('add', quad.subject, quad.predicate, quad.object, eventLog)
    END FOR

    // Calculate state hash AFTER
    stateAfter ← await store.getStateCommitment()

    // Record state hash after
    await store.appendTriple(
        'add',
        dataFactory.namedNode(eventUri),
        dataFactory.namedNode('http://unrdf.org/probe/ontology/stateHashAfter'),
        dataFactory.literal(stateAfter.state_hash),
        dataFactory.namedNode('kgc:ProbeEventLog')
    )

    RETURN {eventUri: eventUri, logIndex: logIndex, timestamp_ns: timestamp_ns}
END
```

**Verify Immutability Algorithm**:

```
ALGORITHM: VerifyEventLogImmutability
INPUT: store (KnowledgeStore)
OUTPUT: boolean (true if log is immutable)

BEGIN
    events ← store.selectTriples({
        predicate: rdf:type,
        object: probe:Event,
        graph: kgc:ProbeEventLog
    })

    // Extract log indices
    indices ← []
    FOR EACH event IN events DO
        indexQuads ← store.selectTriples({
            subject: event.subject,
            predicate: probe:logIndex,
            graph: kgc:ProbeEventLog
        })
        indices.append(GetLiteralValue(indexQuads[0].object))
    END FOR

    // Sort indices
    indices.sort()

    // Verify sequential: 0, 1, 2, ..., n
    FOR i = 0 TO indices.length - 1 DO
        IF indices[i] != i THEN
            RETURN false
        END IF
    END FOR

    RETURN true
END
```

---

### Graph 3: ProbeSystem (Configuration and Allowlists)

**Purpose**: Store system configuration, security policies, and allowlists.

**Named Graph IRI**: `kgc:ProbeSystem`

**Mutability**: Replace entire configuration on update (never partial updates).

#### Schema

```
GRAPH kgc:ProbeSystem {
    kgc:SystemConfig rdf:type probe:SystemConfig
    kgc:SystemConfig probe:allowedNetworkUrls ?urlList (rdf:List)
    kgc:SystemConfig probe:fsRoots ?fsList (rdf:List)
    kgc:SystemConfig probe:allowedAgents ?agentList (rdf:List)
    kgc:SystemConfig probe:stateHash ?hash (xsd:string, BLAKE3)
    kgc:SystemConfig probe:lastUpdated ?ts (xsd:long, nanoseconds)
    kgc:SystemConfig probe:updatedByAgent ?agentId (xsd:string)

    ?urlList rdf:first ?url
    ?urlList rdf:rest ?restUrlList

    ?fsList rdf:first ?fsRoot
    ?fsList rdf:rest ?restFsList

    ?agentList rdf:first ?agentId
    ?agentList rdf:rest ?restAgentList
}
```

#### Example Quads

```javascript
// System config instance
kgc:SystemConfig rdf:type probe:SystemConfig
    in graph kgc:ProbeSystem

kgc:SystemConfig probe:lastUpdated "1735366800000000000"^^xsd:long
    in graph kgc:ProbeSystem

kgc:SystemConfig probe:updatedByAgent "system-admin"
    in graph kgc:ProbeSystem

kgc:SystemConfig probe:stateHash "abc123def456..."
    in graph kgc:ProbeSystem

// RDF List for allowed network URLs
kgc:SystemConfig probe:allowedNetworkUrls kgc:AllowedNetworkList
    in graph kgc:ProbeSystem

kgc:AllowedNetworkList rdf:first "https://api.example.com"
    in graph kgc:ProbeSystem

kgc:AllowedNetworkList rdf:rest kgc:AllowedNetworkList-2
    in graph kgc:ProbeSystem

kgc:AllowedNetworkList-2 rdf:first "https://data.example.com"
    in graph kgc:ProbeSystem

kgc:AllowedNetworkList-2 rdf:rest kgc:AllowedNetworkList-3
    in graph kgc:ProbeSystem

kgc:AllowedNetworkList-3 rdf:first "https://logs.example.com"
    in graph kgc:ProbeSystem

kgc:AllowedNetworkList-3 rdf:rest rdf:nil
    in graph kgc:ProbeSystem

// RDF List for filesystem roots
kgc:SystemConfig probe:fsRoots kgc:FsRootsList
    in graph kgc:ProbeSystem

kgc:FsRootsList rdf:first "/home"
    in graph kgc:ProbeSystem

kgc:FsRootsList rdf:rest kgc:FsRootsList-2
    in graph kgc:ProbeSystem

kgc:FsRootsList-2 rdf:first "/tmp"
    in graph kgc:ProbeSystem

kgc:FsRootsList-2 rdf:rest rdf:nil
    in graph kgc:ProbeSystem

// RDF List for allowed agents
kgc:SystemConfig probe:allowedAgents kgc:AllowedAgentsList
    in graph kgc:ProbeSystem

kgc:AllowedAgentsList rdf:first "agent-1"
    in graph kgc:ProbeSystem

kgc:AllowedAgentsList rdf:rest kgc:AllowedAgentsList-2
    in graph kgc:ProbeSystem

kgc:AllowedAgentsList-2 rdf:first "agent-2"
    in graph kgc:ProbeSystem

kgc:AllowedAgentsList-2 rdf:rest rdf:nil
    in graph kgc:ProbeSystem
```

**Update Algorithm**:

```
ALGORITHM: UpdateSystemConfig
INPUT: newConfig (object with allowedNetworkUrls, fsRoots, allowedAgents), store (KnowledgeStore)
OUTPUT: newStateHash

VARIABLES:
    configGraph ← kgc:ProbeSystem
    configUri ← kgc:SystemConfig
    systemNode ← dataFactory.namedNode(configUri)
    quadsToDelete ← []
    quadsToAdd ← []

BEGIN
    // Step 1: Validate new config
    IF NOT IsValidConfig(newConfig) THEN
        THROW error("Invalid system configuration")
    END IF

    // Step 2: Delete old configuration
    oldConfigQuads ← store.selectTriples({
        subject: systemNode,
        graph: configGraph
    })

    FOR EACH quad IN oldConfigQuads DO
        // Skip if not updatable property
        IF quad.predicate IN [probe:allowedNetworkUrls, probe:fsRoots, probe:allowedAgents] THEN
            await store.appendTriple('delete', quad.subject, quad.predicate, quad.object, configGraph)
        END IF
    END FOR

    // Step 3: Create new RDF lists
    networkListUri ← CreateRdfList(newConfig.allowedNetworkUrls, "AllowedNetworkList")
    fsListUri ← CreateRdfList(newConfig.fsRoots, "FsRootsList")
    agentListUri ← CreateRdfList(newConfig.allowedAgents, "AllowedAgentsList")

    // Step 4: Add new configuration quads
    timestamp_ns ← CurrentTimeNanoseconds()

    await store.appendTriple('add',
        systemNode,
        dataFactory.namedNode('http://unrdf.org/probe/ontology/allowedNetworkUrls'),
        dataFactory.namedNode(networkListUri),
        dataFactory.namedNode('kgc:ProbeSystem')
    )

    await store.appendTriple('add',
        systemNode,
        dataFactory.namedNode('http://unrdf.org/probe/ontology/fsRoots'),
        dataFactory.namedNode(fsListUri),
        dataFactory.namedNode('kgc:ProbeSystem')
    )

    await store.appendTriple('add',
        systemNode,
        dataFactory.namedNode('http://unrdf.org/probe/ontology/allowedAgents'),
        dataFactory.namedNode(agentListUri),
        dataFactory.namedNode('kgc:ProbeSystem')
    )

    // Step 5: Update metadata
    stateCommitment ← await store.getStateCommitment()

    await store.appendTriple('add',
        systemNode,
        dataFactory.namedNode('http://unrdf.org/probe/ontology/stateHash'),
        dataFactory.literal(stateCommitment.state_hash),
        dataFactory.namedNode('kgc:ProbeSystem')
    )

    await store.appendTriple('add',
        systemNode,
        dataFactory.namedNode('http://unrdf.org/probe/ontology/lastUpdated'),
        dataFactory.literal(timestamp_ns.toString(),
            dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#long')),
        dataFactory.namedNode('kgc:ProbeSystem')
    )

    // Log configuration update in event log
    await AppendEventToLog(
        'CONFIG_UPDATE',
        'system-admin',
        {newConfig: newConfig, stateHashAfter: stateCommitment.state_hash},
        store
    )

    RETURN stateCommitment.state_hash
END

SUBROUTINE: CreateRdfList
INPUT: items (array), listNamePrefix (string)
OUTPUT: URI of head list node

BEGIN
    IF items.length == 0 THEN
        RETURN rdf:nil
    END IF

    listNodes ← []
    FOR i = items.length - 1 DOWN TO 0 DO
        nodeUri ← f"http://unrdf.org/kgc/probe/{listNamePrefix}-{i}"
        listNodes.append(nodeUri)
    END FOR

    // Build RDF list from tail to head
    currentNode ← rdf:nil
    FOR i = items.length - 1 DOWN TO 0 DO
        nodeUri ← listNodes[i]

        await store.appendTriple('add',
            dataFactory.namedNode(nodeUri),
            dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#first'),
            dataFactory.literal(items[i].toString()),
            dataFactory.namedNode('kgc:ProbeSystem')
        )

        await store.appendTriple('add',
            dataFactory.namedNode(nodeUri),
            dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#rest'),
            dataFactory.namedNode(currentNode),
            dataFactory.namedNode('kgc:ProbeSystem')
        )

        currentNode ← nodeUri
    END FOR

    RETURN currentNode
END
```

---

## PART 3: KNOWLEDGE STORE INTEGRATION

### Integration Pattern

```
ALGORITHM: ProbeGraphManager
INPUT: None
OUTPUT: Unified interface to all three graphs

CLASS ProbeGraphManager:

    store: KnowledgeStore  // From @unrdf/kgc-substrate
    universeGraphUri: NamedNode
    eventLogGraphUri: NamedNode
    systemGraphUri: NamedNode

    METHOD constructor(nodeId: string)
        this.store ← new KnowledgeStore({nodeId: nodeId})
        this.universeGraphUri ← dataFactory.namedNode('kgc:ProbeUniverse')
        this.eventLogGraphUri ← dataFactory.namedNode('kgc:ProbeEventLog')
        this.systemGraphUri ← dataFactory.namedNode('kgc:ProbeSystem')
    END METHOD

    METHOD async materializeObservation(observation: Observation): Promise<{uri, indices}>
        // Uses MaterializeObservationToQuads algorithm
        indices ← await MaterializeObservationToQuads(observation, this.store)
        RETURN {uri: obsUri, indices: indices}
    END METHOD

    METHOD async appendEvent(
        operation: string,
        agentId: string,
        payload: object
    ): Promise<{uri, index}>
        // Uses AppendEventToLog algorithm
        result ← await AppendEventToLog(operation, agentId, payload, this.store)
        RETURN result
    END METHOD

    METHOD async updateSystemConfig(newConfig: object): Promise<string>
        // Uses UpdateSystemConfig algorithm
        stateHash ← await UpdateSystemConfig(newConfig, this.store)
        RETURN stateHash
    END METHOD

    METHOD selectObservations(pattern: object): Set<Quad>
        // Query ProbeUniverse with pattern
        RETURN this.store.selectTriples({
            ...pattern,
            graph: this.universeGraphUri
        })
    END METHOD

    METHOD selectEvents(pattern: object): Set<Quad>
        // Query ProbeEventLog with pattern
        RETURN this.store.selectTriples({
            ...pattern,
            graph: this.eventLogGraphUri
        })
    END METHOD

    METHOD async getSystemConfig(): Promise<object>
        // Query ProbeSystem and materialize config object
        configQuads ← this.store.selectTriples({
            subject: dataFactory.namedNode('kgc:SystemConfig'),
            graph: this.systemGraphUri
        })
        RETURN DeserializeSystemConfig(configQuads)
    END METHOD

    METHOD async generateSnapshot(): Promise<StorageSnapshot>
        // Snapshot all three graphs via KnowledgeStore
        snapshot ← await this.store.generateSnapshot()
        RETURN snapshot
    END METHOD

    METHOD async getStateCommitment(): Promise<StateCommitment>
        // Get current state hash and metadata
        commitment ← await this.store.getStateCommitment()
        RETURN commitment
    END METHOD
END CLASS
```

### Deterministic Snapshot Generation

```
ALGORITHM: GenerateDeterministicSnapshot
INPUT: store (KnowledgeStore)
OUTPUT: SnapshotMetadata with reproducible hash

BEGIN
    // Step 1: Query all quads from all three graphs
    universeQuads ← store.selectTriples({
        graph: kgc:ProbeUniverse
    })

    eventLogQuads ← store.selectTriples({
        graph: kgc:ProbeEventLog
    })

    systemQuads ← store.selectTriples({
        graph: kgc:ProbeSystem
    })

    allQuads ← universeQuads UNION eventLogQuads UNION systemQuads

    // Step 2: Canonicalize quads (lexicographic S-P-O-G ordering)
    canonicalQuads ← CanonicalizeQuads(allQuads)

    // Step 3: Serialize to canonical N-Quads
    nquads ← ""
    FOR EACH quad IN canonicalQuads DO
        nquads ← nquads + SerializeQuadToNQuads(quad) + "\n"
    END FOR

    // Step 4: Hash with BLAKE3 (deterministic)
    graphHash ← BLAKE3(nquads)

    // Step 5: Create snapshot metadata
    snapshot ← {
        timestamp_ns: CurrentTimeNanoseconds(),
        graphs: {
            universeQuadCount: universeQuads.size,
            eventLogQuadCount: eventLogQuads.size,
            systemQuadCount: systemQuads.size,
            totalQuadCount: allQuads.size
        },
        hashes: {
            universeHash: BLAKE3(SerializeGraph(universeQuads)),
            eventLogHash: BLAKE3(SerializeGraph(eventLogQuads)),
            systemHash: BLAKE3(SerializeGraph(systemQuads)),
            globalHash: graphHash
        },
        logIndex: store.getLogIndex(),
        epoch: store.getEpoch()
    }

    // Step 6: Persist snapshot to Git (via KnowledgeStore)
    persistedSnapshot ← await store.generateSnapshot()

    RETURN snapshot
END

SUBROUTINE: CanonicalizeQuads
INPUT: quads (Set<Quad>)
OUTPUT: sorted array of quads

BEGIN
    // Custom comparator: S-P-O-G lexicographic order
    quads.sort((q1, q2) => {
        // Subject comparison
        sComp ← CompareTerms(q1.subject, q2.subject)
        IF sComp != 0 THEN RETURN sComp END IF

        // Predicate comparison
        pComp ← CompareTerms(q1.predicate, q2.predicate)
        IF pComp != 0 THEN RETURN pComp END IF

        // Object comparison
        oComp ← CompareTerms(q1.object, q2.object)
        IF oComp != 0 THEN RETURN oComp END IF

        // Graph comparison
        RETURN CompareTerms(q1.graph, q2.graph)
    })

    RETURN quads
END

SUBROUTINE: CompareTerms
INPUT: t1 (RDF term), t2 (RDF term)
OUTPUT: integer (-1, 0, or 1)

BEGIN
    // Order: NamedNode < BlankNode < Literal
    type1 ← GetTermType(t1)
    type2 ← GetTermType(t2)

    IF type1 != type2 THEN
        RETURN CompareTypes(type1, type2)
    END IF

    // Same type: compare by string value
    IF t1.value < t2.value THEN RETURN -1
    IF t1.value > t2.value THEN RETURN 1
    RETURN 0
END
```

### State Hash Verification

```
ALGORITHM: VerifyStateHash
INPUT: expectedHash (string), store (KnowledgeStore)
OUTPUT: boolean

BEGIN
    commitment ← await store.getStateCommitment()

    IF commitment.state_hash == expectedHash THEN
        RETURN true
    ELSE
        RETURN false
    END IF
END

ALGORITHM: TrackStateHashHistory
INPUT: store (KnowledgeStore)
OUTPUT: Array of {timestamp_ns, state_hash} tuples

BEGIN
    // Query all events and extract state hashes
    events ← store.selectTriples({
        predicate: rdf:type,
        object: probe:Event,
        graph: kgc:ProbeEventLog
    })

    hashes ← []

    FOR EACH event IN events DO
        // Get state hash after
        hashQuads ← store.selectTriples({
            subject: event.subject,
            predicate: probe:stateHashAfter,
            graph: kgc:ProbeEventLog
        })

        timestampQuads ← store.selectTriples({
            subject: event.subject,
            predicate: probe:timestamp_ns,
            graph: kgc:ProbeEventLog
        })

        IF hashQuads.size > 0 AND timestampQuads.size > 0 THEN
            hash ← hashQuads[0].object.value
            timestamp_ns ← timestampQuads[0].object.value
            hashes.append({timestamp_ns: timestamp_ns, state_hash: hash})
        END IF
    END FOR

    // Sort by timestamp
    hashes.sort((a, b) => a.timestamp_ns - b.timestamp_ns)

    RETURN hashes
END
```

---

## PART 4: SPARQL QUERY PATTERNS

### Query 1: Find Capabilities in Domain

```sparql
PREFIX probe: <http://unrdf.org/probe/ontology/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX kgc: <http://unrdf.org/kgc/probe/>

SELECT ?capability ?capLabel ?capValue
WHERE {
    GRAPH kgc:ProbeUniverse {
        ?domain rdf:type probe:Domain ;
                rdfs:label ?domainLabel ;
                probe:hasCapability ?capability .

        ?capability rdf:type probe:Capability ;
                    rdfs:label ?capLabel ;
                    rdf:value ?capValue .

        FILTER (?domainLabel = "filesystem")
    }
}
ORDER BY ?capLabel
```

**Complexity Analysis**:
- Time: O(n) where n = number of observations in ProbeUniverse
- Space: O(k) where k = number of capabilities returned
- Optimization: Index on domain properties

---

### Query 2: Detect Conflicts Between Agents

```sparql
PREFIX probe: <http://unrdf.org/probe/ontology/>
PREFIX kgc: <http://unrdf.org/kgc/probe/>

SELECT ?obsA ?agentA ?obsB ?agentB ?claim ?domainA ?domainB
WHERE {
    GRAPH kgc:ProbeUniverse {
        ?obsA rdf:type probe:Observation ;
              probe:agentId ?agentA ;
              probe:domain ?domainA ;
              probe:claimHash ?claim ;
              probe:conflictsWith ?obsB .

        ?obsB rdf:type probe:Observation ;
              probe:agentId ?agentB ;
              probe:domain ?domainB ;
              probe:claimHash ?claim .

        FILTER (?agentA != ?agentB)
    }
}
```

**Complexity Analysis**:
- Time: O(n) where n = number of observations
- Space: O(m) where m = number of conflicts found
- Optimization: Index on probe:conflictsWith

---

### Query 3: Timeline of Changes (Version Comparison)

```sparql
PREFIX probe: <http://unrdf.org/probe/ontology/>
PREFIX kgc: <http://unrdf.org/kgc/probe/>

SELECT ?eventIndex ?timestamp ?operation ?stateHashBefore ?stateHashAfter
WHERE {
    GRAPH kgc:ProbeEventLog {
        ?event rdf:type probe:Event ;
               probe:logIndex ?eventIndex ;
               probe:timestamp_ns ?timestamp ;
               probe:operationType ?operation ;
               probe:stateHashBefore ?stateHashBefore ;
               probe:stateHashAfter ?stateHashAfter .
    }
}
ORDER BY ?eventIndex
LIMIT 1000
```

**Complexity Analysis**:
- Time: O(e) where e = number of events
- Space: O(e) if no LIMIT
- Optimization: Index on probe:logIndex (sequential)

---

### Query 4: Agent Observation History

```sparql
PREFIX probe: <http://unrdf.org/probe/ontology/>
PREFIX kgc: <http://unrdf.org/kgc/probe/>

SELECT ?obsUri ?timestamp ?domain ?claimHash
WHERE {
    GRAPH kgc:ProbeUniverse {
        ?obsUri rdf:type probe:Observation ;
                probe:timestamp_ns ?timestamp ;
                probe:agentId "agent-1" ;
                probe:domain ?domain ;
                probe:claimHash ?claimHash .
    }
}
ORDER BY DESC(?timestamp)
```

**Complexity Analysis**:
- Time: O(n) where n = observations by agent
- Space: O(k) where k = observations returned
- Optimization: Index on (agentId, timestamp_ns)

---

### Query 5: System Configuration Validation

```sparql
PREFIX probe: <http://unrdf.org/probe/ontology/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX kgc: <http://unrdf.org/kgc/probe/>

SELECT ?agent
WHERE {
    GRAPH kgc:ProbeSystem {
        kgc:SystemConfig probe:allowedAgents ?list .

        ?list rdf:rest* / rdf:first ?agent .
    }
}
```

**Complexity Analysis**:
- Time: O(m) where m = length of agent list
- Space: O(m)
- Optimization: RDF list traversal is sequential

---

### Query 6: State Hash Verification Chain

```sparql
PREFIX probe: <http://unrdf.org/probe/ontology/>
PREFIX kgc: <http://unrdf.org/kgc/probe/>

SELECT ?prevEvent ?prevHash ?currEvent ?currHash
WHERE {
    GRAPH kgc:ProbeEventLog {
        ?prevEvent probe:logIndex ?prevIndex ;
                   probe:stateHashAfter ?prevHash .

        ?currEvent probe:logIndex ?currIndex ;
                   probe:stateHashBefore ?currHash .

        FILTER (?currIndex = ?prevIndex + 1)
        FILTER (?prevHash = ?currHash)
    }
}
```

**Complexity Analysis**:
- Time: O(e) where e = number of events
- Space: O(1) for verification
- Purpose: Cryptographically verify event log integrity

---

## PART 5: COMPLEXITY ANALYSIS

### Time Complexity Summary

| Operation | Time | Notes |
|-----------|------|-------|
| MaterializeObservation | O(k) | k = number of capabilities |
| AppendEvent | O(1) | Append-only, constant time |
| UpdateSystemConfig | O(m) | m = size of config lists |
| QueryCapabilities | O(n) | n = total observations |
| DetectConflicts | O(n) | Full scan for conflicts |
| VerifyEventLog | O(e) | e = number of events |
| GenerateSnapshot | O(n+e+s) | n,e,s = quads per graph |

### Space Complexity Summary

| Structure | Space | Notes |
|-----------|-------|-------|
| ProbeUniverse | O(n*k) | n observations × k capabilities |
| ProbeEventLog | O(e) | e = event count (append-only) |
| ProbeSystem | O(m) | m = total config size |
| Snapshot Hash | O(1) | BLAKE3 produces fixed 32-byte hash |
| State Commitment | O(1) | Fixed metadata size |

### Index Recommendations

```
// ProbeUniverse
INDEX ON (probe:agentId, probe:timestamp_ns)
INDEX ON (probe:domain)
INDEX ON (probe:claimHash)
INDEX ON (probe:conflictsWith)

// ProbeEventLog
INDEX ON (probe:logIndex) PRIMARY  // Enforce sequential
INDEX ON (probe:operationType)
INDEX ON (probe:timestamp_ns)
INDEX ON (probe:agentId)

// ProbeSystem
INDEX ON (rdf:type = probe:SystemConfig) UNIQUE
```

---

## PART 6: DESIGN PATTERNS

### Pattern 1: Canonical Quad Ordering

**Problem**: Different serialization orders produce different hashes.
**Solution**: Always sort quads lexicographically (S-P-O-G).

```
GUARANTEED INVARIANT:
    hash(CanonicalizeQuads(Q)) == hash(CanonicalizeQuads(permutation(Q)))
```

### Pattern 2: Immutable Append-Only Log

**Problem**: Events must never be modified or reordered.
**Solution**: Sequential log indices + state hash chain.

```
GUARANTEED INVARIANT:
    For all i < j: event[i].timestamp_ns < event[j].timestamp_ns
    For all i: event[i].stateHashAfter == event[i+1].stateHashBefore
```

### Pattern 3: Deterministic URIs

**Problem**: Same data should produce same URI across restarts.
**Solution**: Hash-based deterministic URI generation.

```
URI = f"http://unrdf.org/kgc/probe/{type}#{SHA256(canonical_value)}"
```

### Pattern 4: RDF List for Configuration

**Problem**: Variable-length lists in RDF.
**Solution**: Standard RDF list cons cells (rdf:first, rdf:rest).

```
[a, b, c] ==> a::b::c::nil
```

---

## PART 7: INTEGRATION WITH KNOWLEDGE STORE

### Data Flow

```
Observation Object
        ↓
        ↓ MaterializeObservationToQuads()
        ↓
    N3 Quads (4-tuples)
        ↓
        ↓ store.appendTriple()
        ↓
    KGCStore Event Log (immutable)
        ↓
        ↓ generate BLAKE3 hash
        ↓
    State Commitment {state_hash, log_index, timestamp_ns, quad_count}
        ↓
        ↓ freezeUniverse() + GitBackbone
        ↓
    Deterministic Snapshot {epoch, timestamp_ns, quads_hash, commit_hash}
```

### Module Dependencies

```
@unrdf/kgc-substrate
    ├── KnowledgeStore (main interface)
    ├── depends on @unrdf/kgc-4d (KGCStore, freezeUniverse)
    └── depends on @unrdf/oxigraph (dataFactory, createStore)

@unrdf/oxigraph
    ├── OxigraphStore (SPARQL engine)
    ├── createStore(quads) factory
    └── dataFactory {namedNode, literal, quad, ...}

@unrdf/kgc-probe (to be created)
    ├── ProbeGraphManager (unified interface)
    ├── uses @unrdf/kgc-substrate for storage
    ├── uses @unrdf/oxigraph for SPARQL queries
    └── implements three graph schemas
```

### Testing Strategy

```
ALGORITHM: VerifyProbeGraphIntegrity
INPUT: store (KnowledgeStore)
OUTPUT: IntegrityReport {errors, warnings, metrics}

BEGIN
    report ← {errors: [], warnings: [], metrics: {}}

    // Test 1: ProbeUniverse consistency
    observations ← store.selectTriples({
        predicate: rdf:type,
        object: probe:Observation,
        graph: kgc:ProbeUniverse
    })

    FOR EACH obs IN observations DO
        // Verify all required properties present
        IF NOT HasProperty(obs, probe:timestamp_ns) THEN
            report.errors.append(f"Observation {obs} missing timestamp")
        END IF
        IF NOT HasProperty(obs, probe:agentId) THEN
            report.errors.append(f"Observation {obs} missing agentId")
        END IF
        IF NOT HasProperty(obs, probe:claimHash) THEN
            report.errors.append(f"Observation {obs} missing claimHash")
        END IF
    END FOR

    // Test 2: ProbeEventLog immutability
    IF NOT VerifyEventLogImmutability(store) THEN
        report.errors.append("Event log is not immutable")
    END IF

    // Test 3: State hash chain validation
    hashes ← TrackStateHashHistory(store)
    FOR i = 1 TO hashes.length - 1 DO
        prevHash ← hashes[i-1].state_hash
        currHash ← hashes[i].state_hash
        IF prevHash == currHash THEN
            report.warnings.append(f"State hash unchanged at event {i}")
        END IF
    END FOR

    // Test 4: System config validity
    systemQuads ← store.selectTriples({
        graph: kgc:ProbeSystem
    })
    IF systemQuads.size == 0 THEN
        report.warnings.append("System config is empty")
    END IF

    // Metrics
    report.metrics ← {
        universeQuadCount: GetQuadCount(kgc:ProbeUniverse),
        eventLogQuadCount: GetQuadCount(kgc:ProbeEventLog),
        eventCount: GetEventCount(),
        observationCount: observations.size,
        systemStateHash: GetCurrentStateHash()
    }

    RETURN report
END
```

---

## PART 8: FILE PATHS AND CODE ORGANIZATION

### Expected File Structure

```
packages/kgc-probe/
├── src/
│   ├── index.mjs                      // Main export
│   ├── ontology.mjs                   // Prefix definitions
│   ├── graphs/
│   │   ├── ProbeUniverse.mjs          // Materialized observations
│   │   ├── ProbeEventLog.mjs          // Append-only events
│   │   └── ProbeSystem.mjs            // System config
│   ├── ProbeGraphManager.mjs          // Unified interface
│   ├── queries/
│   │   ├── capabilities.mjs           // SPARQL patterns
│   │   ├── conflicts.mjs              // Conflict detection
│   │   ├── timeline.mjs               // Version tracking
│   │   └── validation.mjs             // Integrity checks
│   └── utils/
│       ├── canonicalization.mjs       // Quad ordering
│       ├── serialization.mjs          // N-Quads output
│       └── hashing.mjs                // BLAKE3 hashing
├── __tests__/
│   ├── ProbeGraphManager.test.mjs
│   ├── graphs.test.mjs
│   ├── queries.test.mjs
│   └── integrity.test.mjs
├── examples/
│   ├── basic-observation.mjs
│   ├── event-log.mjs
│   └── sparql-queries.mjs
└── README.md
```

---

## SUMMARY

This pseudocode design provides:

1. **Three RDF Graphs**:
   - ProbeUniverse: Observation snapshot (materialized O)
   - ProbeEventLog: Immutable audit trail
   - ProbeSystem: Configuration and policies

2. **Ontology**: 15+ classes and properties with clear semantics

3. **Core Algorithms**:
   - Canonicalized quad generation from Observation objects
   - Append-only event logging
   - Configuration update with validation
   - Deterministic snapshot generation
   - State hash verification chain

4. **KnowledgeStore Integration**:
   - Uses BLAKE3 hashing for state commitments
   - Git-backed snapshots for reproducibility
   - Immutable append-only log guarantees

5. **SPARQL Query Patterns**:
   - Capability discovery
   - Conflict detection
   - Timeline reconstruction
   - Agent history
   - System validation
   - Hash chain verification

6. **Complexity Analysis**:
   - Time: O(n) for most queries where n = data size
   - Space: O(m) where m = results
   - Index recommendations for performance

7. **Design Patterns**:
   - Canonical quad ordering (deterministic hashing)
   - Immutable append-only log contract
   - Deterministic URI generation
   - RDF list for variable-length data

This design ensures:
- **Determinism**: Same data always produces same hash
- **Immutability**: Event log cannot be modified
- **Auditability**: Complete history in ProbeEventLog
- **Reproducibility**: Snapshots can be regenerated
