# UNRDF 2028 Component Interaction Diagrams

**Date:** 2025-11-18

## Overview

This document contains detailed component interaction diagrams for all major subsystems.

## 1. AI/ML Integration Layer - Component Interactions

### 1.1 Graph Embedding Generation Flow

```
┌──────────────┐
│  Application │
└──────┬───────┘
       │
       │ 1. generateEmbeddings(store, config)
       ▼
┌──────────────────────────────────────────┐
│          AIEngine                         │
│  ┌────────────────────────────────────┐  │
│  │  ModelRegistry                     │  │
│  │  - Load embedding model            │  │
│  └────────────────────────────────────┘  │
└──────────────┬───────────────────────────┘
               │
               │ 2. embed(store)
               ▼
┌──────────────────────────────────────────┐
│       GraphEmbedder                       │
│  ┌────────────────────────────────────┐  │
│  │  1. Build adjacency list from RDF │  │
│  │  2. Select algorithm (Node2Vec)    │  │
│  │  3. Train embeddings               │  │
│  └────────────────────────────────────┘  │
└──────────────┬───────────────────────────┘
               │
               │ 3. train(graph)
               ▼
┌──────────────────────────────────────────┐
│      InferenceAdapter                     │
│  ┌────────────────────────────────────┐  │
│  │  Backend Selection:                │  │
│  │  • ONNX Runtime (local)            │  │
│  │  • TensorFlow.js (browser)         │  │
│  │  • API Backend (cloud)             │  │
│  └────────────────────────────────────┘  │
└──────────────┬───────────────────────────┘
               │
               │ 4. Return embeddings
               ▼
┌──────────────────────────────────────────┐
│  Embedding Storage                        │
│  - Store embeddings for similarity search│
│  - Index for fast retrieval              │
└──────────────────────────────────────────┘
```

### 1.2 Natural Language to SPARQL Translation

```
User Query: "Find all researchers who published papers on knowledge graphs"
       │
       │ 1. translateQuery(naturalQuery, context)
       ▼
┌──────────────────────────────────────────────────────────┐
│              NLQueryTranslator                            │
│  ┌────────────────────────────────────────────────────┐  │
│  │  1. Build prompt with:                             │  │
│  │     • Schema context (ontology)                    │  │
│  │     • Few-shot examples                            │  │
│  │     • Natural language query                       │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────┬───────────────────────────────────────┘
                   │
                   │ 2. complete(prompt)
                   ▼
┌──────────────────────────────────────────────────────────┐
│             InferenceAdapter                              │
│  ┌────────────────────────────────────────────────────┐  │
│  │  LLM Backend:                                      │  │
│  │  • Transformers.js (local)                         │  │
│  │  • GPT-4 API (cloud)                               │  │
│  │  • LLaMA (ONNX)                                    │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────┬───────────────────────────────────────┘
                   │
                   │ 3. Return SPARQL query
                   ▼
┌──────────────────────────────────────────────────────────┐
│              QueryRefiner                                 │
│  ┌────────────────────────────────────────────────────┐  │
│  │  1. Parse SPARQL                                   │  │
│  │  2. Validate syntax                                │  │
│  │  3. Optimize (join order, filters)                 │  │
│  │  4. Format consistently                            │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────┬───────────────────────────────────────┘
                   │
                   │ 4. Return refined SPARQL
                   ▼
┌──────────────────────────────────────────────────────────┐
│          Query Execution (Knowledge Engine)               │
│  Execute SPARQL against RDF store                         │
└──────────────────────────────────────────────────────────┘
```

## 2. Distributed Federation Layer - Component Interactions

### 2.1 Federated Query Execution

```
Application
    │
    │ 1. query(sparqlQuery)
    ▼
┌──────────────────────────────────────────┐
│      FederationManager                    │
│  ┌────────────────────────────────────┐  │
│  │  NodeRegistry: List of peer nodes │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ 2. plan(sparqlQuery)
           ▼
┌──────────────────────────────────────────┐
│         QueryPlanner                      │
│  ┌────────────────────────────────────┐  │
│  │  1. Parse SPARQL                   │  │
│  │  2. Map patterns to data sources   │  │
│  │  3. Estimate costs                 │  │
│  │  4. Generate execution plan        │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ 3. execute(plan)
           ▼
┌──────────────────────────────────────────┐
│        QueryExecutor                      │
│  ┌────────────────────────────────────┐  │
│  │  Parallel Execution:               │  │
│  │  ┌──────────┐  ┌──────────┐       │  │
│  │  │ Node 1   │  │ Node 2   │       │  │
│  │  │ Subquery │  │ Subquery │       │  │
│  │  └──────────┘  └──────────┘       │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ 4. merge(results)
           ▼
┌──────────────────────────────────────────┐
│         ResultMerger                      │
│  ┌────────────────────────────────────┐  │
│  │  1. Collect partial results        │  │
│  │  2. Perform joins                  │  │
│  │  3. Apply filters                  │  │
│  │  4. Sort and limit                 │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ 5. Return merged results
           ▼
       Application
```

### 2.2 P2P Synchronization with CRDTs

```
Node A (Local Changes)          Node B (Remote Changes)
    │                                   │
    │ 1. add/delete quads               │
    ▼                                   ▼
┌────────────┐                   ┌────────────┐
│ Local Store│                   │ Local Store│
└────┬───────┘                   └────┬───────┘
     │                                 │
     │ 2. Track changes                │
     ▼                                 ▼
┌────────────────┐             ┌────────────────┐
│ VectorClock    │             │ VectorClock    │
│ [A:5, B:3]     │             │ [A:4, B:4]     │
└────┬───────────┘             └────┬───────────┘
     │                               │
     │ 3. Exchange changes           │
     │◄─────────────────────────────►│
     │                               │
     │ 4. Detect conflicts           │
     ▼                               ▼
┌────────────────────────────────────────────┐
│          CRDTResolver                       │
│  ┌──────────────────────────────────────┐  │
│  │  Conflict Resolution Strategies:     │  │
│  │  • Last-Write-Wins (timestamp)       │  │
│  │  • Add-Wins (for OR-Set)             │  │
│  │  • Custom resolution rules           │  │
│  └──────────────────────────────────────┘  │
└────┬───────────────────────────────────────┘
     │
     │ 5. Apply resolved changes
     ▼
┌────────────────┐             ┌────────────────┐
│ Merged Store   │             │ Merged Store   │
│ (Consistent)   │             │ (Consistent)   │
└────────────────┘             └────────────────┘
```

## 3. Real-time Streaming Layer - Component Interactions

### 3.1 Change Feed and Stream Processing

```
RDF Store Operations
    │
    │ add/delete quads
    ▼
┌──────────────────────────────────────────┐
│         ChangeFeed                        │
│  ┌────────────────────────────────────┐  │
│  │  Instrument store operations:      │  │
│  │  • Wrap store.add()                │  │
│  │  • Wrap store.delete()             │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ Publish change events
           ▼
┌──────────────────────────────────────────┐
│           EventBus                        │
│  ┌────────────────────────────────────┐  │
│  │  Stream: "store:changes"           │  │
│  │  Events: quad:added, quad:deleted  │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ Route to subscribers
           ├─────────────┬─────────────┬────────────
           ▼             ▼             ▼
    ┌────────────┐ ┌──────────┐ ┌──────────────┐
    │ Subscriber │ │ Window   │ │ Continuous   │
    │ 1          │ │ Manager  │ │ Query Engine │
    └────────────┘ └────┬─────┘ └──────────────┘
                        │
                        │ Window events (tumbling, sliding)
                        ▼
               ┌─────────────────────┐
               │  Window Aggregation │
               │  • Count            │
               │  • Time-based       │
               │  • Session-based    │
               └─────────────────────┘
```

### 3.2 Continuous SPARQL Query Evaluation

```
Application
    │
    │ continuousQuery(sparqlQuery, handler)
    ▼
┌──────────────────────────────────────────┐
│       RDFStreamProcessor                  │
│  ┌────────────────────────────────────┐  │
│  │  Register continuous query         │  │
│  │  Query ID: abc123                  │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ Listen to window events
           ▼
┌──────────────────────────────────────────┐
│          WindowManager                    │
│  ┌────────────────────────────────────┐  │
│  │  Emit "window:close" events        │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ On window close
           ▼
┌──────────────────────────────────────────┐
│     Continuous Query Evaluator            │
│  ┌────────────────────────────────────┐  │
│  │  1. Create temp store from window  │  │
│  │  2. Execute SPARQL query           │  │
│  │  3. Compare with last results      │  │
│  │  4. Detect changes (delta)         │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ If changes detected
           ▼
┌──────────────────────────────────────────┐
│         Handler Invocation                │
│  handler(changes)                         │
│  • New results                            │
│  • Updated results                        │
│  • Removed results                        │
└──────────────────────────────────────────┘
```

## 4. Privacy & Security Layer - Component Interactions

### 4.1 Encrypted Store Operations

```
Application
    │
    │ encryptedStore.add(quad)
    ▼
┌──────────────────────────────────────────┐
│         EncryptedStore                    │
│  ┌────────────────────────────────────┐  │
│  │  1. Check encryption rules         │  │
│  │     - Sensitive predicates?        │  │
│  │     - Classification rules?        │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ If should encrypt
           ▼
┌──────────────────────────────────────────┐
│        FieldEncryptor                     │
│  ┌────────────────────────────────────┐  │
│  │  1. Get encryption key from KMS    │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ 2. encrypt(value, key)
           ▼
┌──────────────────────────────────────────┐
│        CryptoProvider                     │
│  ┌────────────────────────────────────┐  │
│  │  AES-256-GCM Encryption:           │  │
│  │  • Generate IV                     │  │
│  │  • Encrypt plaintext               │  │
│  │  • Generate auth tag               │  │
│  │  • Return: IV + ciphertext + tag   │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ 3. Store encrypted quad
           ▼
┌──────────────────────────────────────────┐
│      Underlying N3 Store                  │
│  Quad with encrypted object value         │
└──────────────────────────────────────────┘
```

### 4.2 Access Control Enforcement

```
Request (user, resource, action)
    │
    │ enforce(context)
    ▼
┌──────────────────────────────────────────┐
│         ABACEngine                        │
│  ┌────────────────────────────────────┐  │
│  │  1. Load applicable policies       │  │
│  │     - Filter by resource type      │  │
│  │     - Filter by action             │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ For each policy
           ▼
┌──────────────────────────────────────────┐
│        PolicyEvaluator                    │
│  ┌────────────────────────────────────┐  │
│  │  Evaluate conditions:              │  │
│  │  • user.roles contains 'analyst'   │  │
│  │  • resource.classification != PII  │  │
│  │  • time.hour between 9 and 17      │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ All conditions match?
           ├──── YES ────┐
           │             │
           │             ▼
           │      ┌─────────────┐
           │      │ Effect:     │
           │      │ PERMIT      │
           │      └──────┬──────┘
           │             │
           ▼             │
      ┌─────────┐       │
      │ Effect: │       │
      │ DENY    │       │
      └────┬────┘       │
           │            │
           └────────────┴──► Combine results
                              │
                              │ Any DENY = DENY
                              │ All PERMIT = PERMIT
                              ▼
                        ┌──────────────┐
                        │ Decision:    │
                        │ granted=true │
                        └──────┬───────┘
                               │
                               │ Log decision
                               ▼
                        ┌──────────────┐
                        │ AuditLogger  │
                        └──────────────┘
```

## 5. Web3 Integration Layer - Component Interactions

### 5.1 Graph Registration on Blockchain

```
Application
    │
    │ registerGraph(store, metadata)
    ▼
┌──────────────────────────────────────────┐
│          Web3Manager                      │
│  ┌────────────────────────────────────┐  │
│  │  1. Serialize store to Turtle      │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ 2. Store on IPFS
           ▼
┌──────────────────────────────────────────┐
│         IPFSAdapter                       │
│  ┌────────────────────────────────────┐  │
│  │  ipfs.add(turtleData)              │  │
│  │  → Returns: CID (Content ID)       │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ 3. Generate Merkle root
           ▼
┌──────────────────────────────────────────┐
│       VerificationLayer                   │
│  ┌────────────────────────────────────┐  │
│  │  1. Canonicalize RDF graph         │  │
│  │  2. Hash each quad (SHA-256)       │  │
│  │  3. Build Merkle tree              │  │
│  │  4. Return root hash               │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ 4. Register on-chain
           ▼
┌──────────────────────────────────────────┐
│        ContractBridge                     │
│  ┌────────────────────────────────────┐  │
│  │  Smart Contract: RDFRegistry       │  │
│  │  Function: registerGraph()         │  │
│  │  Params: {                         │  │
│  │    ipfsCid: "Qm...",               │  │
│  │    merkleRoot: "0x...",            │  │
│  │    metadata: "{...}"               │  │
│  │  }                                 │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ 5. Wait for confirmation
           ▼
┌──────────────────────────────────────────┐
│         Blockchain Network                │
│  ┌────────────────────────────────────┐  │
│  │  Transaction mined                 │  │
│  │  Block: 12345678                   │  │
│  │  Event: GraphRegistered            │  │
│  │    graphId: 42                     │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ 6. Return result
           ▼
       Application
```

## 6. Enterprise Features Layer - Component Interactions

### 6.1 Multi-Tenant Request Flow

```
HTTP Request (with tenant context)
    │
    │ X-Tenant-ID: acme-corp
    ▼
┌──────────────────────────────────────────┐
│         TenantRouter                      │
│  ┌────────────────────────────────────┐  │
│  │  Extract tenant ID from:           │  │
│  │  • Header                          │  │
│  │  • JWT claim                       │  │
│  │  • Subdomain                       │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ getContext(tenantId)
           ▼
┌──────────────────────────────────────────┐
│        TenantIsolator                     │
│  ┌────────────────────────────────────┐  │
│  │  Retrieve tenant context:          │  │
│  │  • Dedicated store/graph           │  │
│  │  • Quotas                          │  │
│  │  • Metadata                        │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ checkQuotas()
           ▼
┌──────────────────────────────────────────┐
│      ResourceQuotaManager                 │
│  ┌────────────────────────────────────┐  │
│  │  Check:                            │  │
│  │  • Current triples < maxTriples    │  │
│  │  • Query count < maxQueries        │  │
│  │  • Storage < maxStorage            │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ evaluate(context, operation)
           ▼
┌──────────────────────────────────────────┐
│         PolicyEngine                      │
│  ┌────────────────────────────────────┐  │
│  │  Apply governance policies         │  │
│  └────────────────────────────────────┘  │
└──────────┬───────────────────────────────┘
           │
           │ execute(operation)
           ▼
┌──────────────────────────────────────────┐
│      Isolated RDF Store                   │
│  (Tenant-specific graph or store)         │
└──────────┬───────────────────────────────┘
           │
           │ track(operation, result)
           ▼
┌──────────────────────────────────────────┐
│        LineageTracker                     │
│  ┌────────────────────────────────────┐  │
│  │  Record:                           │  │
│  │  • Input resources                 │  │
│  │  • Output resources                │  │
│  │  • Operation type                  │  │
│  │  • Timestamp                       │  │
│  └────────────────────────────────────┘  │
└──────────────────────────────────────────┘
```

## Integration Flows

### Full Stack Example: AI-Enhanced Federated Query with Security

```
1. User Request (Natural Language)
   "Show me all confidential research data from 2024"
        │
        ▼
2. AI/ML Layer: NL→SPARQL Translation
   → Translates to SPARQL query
        │
        ▼
3. Security Layer: Access Control
   → Check user permissions for confidential data
   → PERMIT (user has analyst role)
        │
        ▼
4. Federation Layer: Query Planning
   → Determine which nodes have 2024 research data
   → Node1: Internal DB, Node2: Partner API
        │
        ▼
5. Federation Layer: Distributed Execution
   → Execute subqueries in parallel
   → Merge results
        │
        ▼
6. Security Layer: Decryption
   → Decrypt confidential fields
        │
        ▼
7. Enterprise Layer: Lineage Tracking
   → Record query as lineage operation
   → Input: federated sources
   → Output: result set
        │
        ▼
8. Response to User
   → Decrypted, merged results
   → Audit log entry created
```

---

## Summary

These component interaction diagrams demonstrate:

1. **Modularity**: Each layer is independent
2. **Integration Points**: Clear interfaces between components
3. **Data Flow**: How data moves through the system
4. **Security**: Security checks at multiple layers
5. **Observability**: OTEL spans at each interaction

All components integrate through well-defined interfaces, enabling independent development and testing while ensuring seamless operation in the complete system.
