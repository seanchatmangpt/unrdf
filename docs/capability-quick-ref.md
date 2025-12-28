# UNRDF Capability Quick Reference

**For:** Developers & Architects  
**Purpose:** Fast lookup of capability atoms and common compositions

---

## Capability Atoms Quick Lookup

### Foundation (oxigraph + core)
- **A1** - RDF Store Creation (`@unrdf/oxigraph`)
- **A2** - SPARQL Query (`@unrdf/oxigraph`)
- **A3** - Quad Operations (`@unrdf/core`)
- **A4** - Sync SPARQL (`@unrdf/core`)
- **A5** - RDF Canonicalization (`@unrdf/core`)
- **A6** - SPARQL ASK/SELECT/CONSTRUCT (`@unrdf/core`)
- **A7** - Error Recovery (`@unrdf/core`)

### Policy & Governance (hooks)
- **A8** - Hook Definition
- **A9** - Hook Chain Execution
- **A10** - JIT Compilation
- **A11** - SPARQL Conditions
- **A12** - SHACL Validation
- **A13** - Policy Packs
- **A14** - Quad Pool (Zero-Alloc)

### Time-Travel (kgc-4d)
- **A15** - Nanosecond Timestamps
- **A16** - Universe Freeze (Snapshots)
- **A17** - State Reconstruction
- **A18** - Git Backup
- **A19** - Vector Clock
- **A20** - HDIT Coordinates
- **A21** - Event Similarity

### Federation (federation + consensus)
- **A22** - Federated SPARQL
- **A23** - Peer Manager
- **A24** - RAFT Consensus
- **A25** - Cluster Membership
- **A26** - Distributed State Machine
- **A27** - Multi-Master Replication

### Streaming (streaming)
- **A28** - Change Feed
- **A29** - Subscriptions
- **A30** - Stream Processor
- **A31** - Real-Time Validation
- **A32** - Sync Protocol
- **A33** - RDF Stream Parser

### Workflow (yawl)
- **A34** - Workflow Definition (YAWL)
- **A35** - Case Management
- **A36** - Work Item Lifecycle
- **A37** - Cryptographic Receipts
- **A38** - Control Flow Patterns
- **A39** - Resource Allocation
- **A40** - Workflow-to-RDF

### Advanced (ml, blockchain, etc.)
- **A41** - BEAM/Erlang WASM
- **A42** - Service Worker Manager
- **A43** - Blockchain Anchoring
- **A44** - Multi-Layer Cache
- **A45** - SPARQL Cache
- **A46** - Vector Search
- **A47** - ONNX Inference
- **A48** - EYE Reasoner
- **A49** - Transformer Embeddings

---

## Common Composition Recipes

### Recipe 1: Add Governance to RDF Store
```javascript
import { createStore } from '@unrdf/core';
import { defineHook, executeHook } from '@unrdf/hooks';

const store = createStore(); // A1
const hook = defineHook({ /* policy */ }); // A8

async function addQuad(quad) {
  const result = await executeHook(hook, quad); // A9
  if (result.passed) store.add(quad); // A3
}
```
**Atoms:** A1 + A8 + A9 + A3  
**Value:** High governance, low overhead  
**Use Case:** Regulated industries (healthcare, finance)

---

### Recipe 2: Time-Travel Debugging
```javascript
import { freezeUniverse, reconstructState } from '@unrdf/kgc-4d';
import { executeQuery } from '@unrdf/core';

// Capture state
const receipt = await freezeUniverse(store, 'checkpoint-1'); // A16

// Later: replay to specific time
const pastStore = await reconstructState(receipt.id, timestamp); // A17
const results = await executeQuery(pastStore, query); // A6
```
**Atoms:** A16 + A17 + A6  
**Value:** Audit trails, debugging  
**Use Case:** Production debugging, compliance

---

### Recipe 3: Federated Query with Caching
```javascript
import { executeFederatedQuery } from '@unrdf/federation';
import { createSparqlCache } from '@unrdf/caching';

const cache = createSparqlCache(); // A45

async function query(sparql, peers) {
  const cached = await cache.get(sparql);
  if (cached) return cached;
  
  const results = await executeFederatedQuery({ query: sparql, peers }); // A22
  await cache.set(sparql, results);
  return results;
}
```
**Atoms:** A22 + A45  
**Value:** Performance + Distribution  
**Use Case:** Multi-tenant SaaS, CDN-like RDF

---

### Recipe 4: Real-Time ML on RDF Streams
```javascript
import { createChangeFeed } from '@unrdf/streaming';
import { createInferencePipeline } from '@unrdf/ml-inference';

const feed = createChangeFeed(store); // A28
const pipeline = createInferencePipeline({ model: './model.onnx' }); // A47

feed.subscribe(async (event) => {
  const prediction = await pipeline.infer(eventToTensor(event));
  store.add(predictionToQuad(prediction)); // A3
});
```
**Atoms:** A28 + A47 + A3  
**Value:** Real-time AI augmentation  
**Use Case:** IoT, anomaly detection

---

### Recipe 5: Blockchain-Anchored Workflow
```javascript
import { createWorkflow, completeTask } from '@unrdf/yawl';
import { anchorReceipt } from '@unrdf/blockchain';

const workflow = createWorkflow({ spec }); // A34
const receipt = await completeTask(workflow, 'task-1'); // A37
const txHash = await anchorReceipt(receipt); // A43
```
**Atoms:** A34 + A37 + A43  
**Value:** External auditability  
**Use Case:** Supply chain, legal contracts

---

## Dependency Chains (Import Order)

### Chain 1: Policy-Gated Store
```
oxigraph → core → hooks
```

### Chain 2: Time-Travel Workflow
```
oxigraph → core → kgc-4d → yawl
```

### Chain 3: Federated Consensus
```
oxigraph → core → hooks → federation → consensus
```

### Chain 4: Streaming ML
```
oxigraph → core → streaming → ml-inference
```

### Chain 5: AI Reasoning
```
oxigraph → core → streaming → knowledge-engine
```

---

## Package Selection Guide

| Use Case | Packages | Rationale |
|----------|----------|-----------|
| **Simple RDF CRUD** | `oxigraph` | Minimal overhead |
| **Add SPARQL** | `oxigraph + core` | Full query support |
| **Add Governance** | `+ hooks` | Policy enforcement |
| **Add Audit Trail** | `+ kgc-4d` | Time-travel + receipts |
| **Add Workflows** | `+ yawl` | Orchestration |
| **Add Federation** | `+ federation` | Multi-node queries |
| **Add Consensus** | `+ consensus` | Strong consistency |
| **Add Streaming** | `+ streaming` | Real-time sync |
| **Add AI** | `+ knowledge-engine` | Reasoning + ML |

---

## Performance Characteristics

| Atom | Latency | Throughput | Memory |
|------|---------|------------|--------|
| A1 (Store) | <1ms | 100K ops/s | O(n) |
| A6 (SPARQL) | 1-50ms | 10K q/s | O(n) |
| A9 (Hooks) | <5ms | 50K ops/s | O(1) |
| A16 (Freeze) | 10-100ms | 100 ops/s | O(n) |
| A22 (Federation) | 50-500ms | 1K q/s | O(n*peers) |
| A28 (Change Feed) | <10ms | 10K events/s | O(subscribers) |
| A47 (ML Inference) | 10-200ms | 100 batches/s | O(batch_size) |

**Key:**
- n = number of quads
- ops/s = operations per second
- q/s = queries per second

---

## Anti-Patterns to Avoid

1. **Don't:** Import `'n3'` directly in app code  
   **Do:** Use `@unrdf/core` wrappers

2. **Don't:** Skip hook validation for "performance"  
   **Do:** Use JIT compilation (A10) for zero-overhead hooks

3. **Don't:** Query without caching in federation  
   **Do:** Use SPARQL cache (A45) for repeated queries

4. **Don't:** Freeze universe after every mutation  
   **Do:** Batch freezes at logical checkpoints

5. **Don't:** Run ONNX inference on main thread  
   **Do:** Use streaming pipeline (A47) with batching

---

## Troubleshooting

### Issue: Hooks not executing
- **Check:** Hook trigger matches event type
- **Verify:** `packages/hooks/src/hooks/define-hook.mjs:10-18`

### Issue: Universe freeze fails
- **Check:** Store is not empty
- **Verify:** `packages/kgc-4d/src/freeze.mjs:9`

### Issue: Federated query timeout
- **Check:** Peer connectivity (A23)
- **Verify:** `packages/federation/src/federation/peer-manager.mjs:17`

### Issue: Change feed out-of-order
- **Check:** Sequence numbers (A28)
- **Verify:** `packages/streaming/src/streaming/change-feed.mjs:10`

---

**Last Updated:** 2025-12-28  
**For Full Details:** See `capability-basis.md`
