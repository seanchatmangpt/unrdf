# API Reference - Top 20% (80% Usage)

**This covers the 20% of APIs you'll use 80% of the time.**

For complete API docs, see package-specific READMEs. This is optimized for **scanning**.

---

## 📦 @unrdf/core - RDF Operations

### Store Creation

```javascript
import { createStore as createOxiStore } from '@unrdf/oxigraph';

const store = createOxiStore();
// ✅ In-memory Oxigraph store (FAST, production-ready)
```

**Why Oxigraph?** 100x faster than N3 for queries. See [adr/001-oxigraph-over-n3.md](adr/001-oxigraph-over-n3.md).

---

### Loading Data

```javascript
// Turtle format (most common)
store.load(`
  @prefix ex: <http://example.org/> .
  ex:Alice foaf:name "Alice" .
`);

// From file
import { readFileSync } from 'fs';
const ttl = readFileSync('data.ttl', 'utf-8');
store.load(ttl);

// N-Triples
store.load('<http://example.org/s> <http://example.org/p> "o" .', { format: 'ntriples' });
```

**Formats supported:** Turtle, N-Triples, RDF/XML, JSON-LD, N-Quads

---

### Querying with SPARQL

```javascript
import { executeSelectSync } from '@unrdf/core';

// SELECT query (returns bindings)
const results = executeSelectSync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name .
  }
`);

// Iterate results
for (const row of results) {
  console.log(row.get('name').value); // "Alice"
}

// Convert to array
const names = [...results].map(r => r.get('name').value);
```

**Query Types:**
- `executeSelectSync` - Tabular results (most common)
- `executeAskSync` - Boolean (true/false)
- `executeConstructSync` - New RDF graph

---

### ASK Queries (Boolean)

```javascript
import { executeAskSync } from '@unrdf/core';

const exists = executeAskSync(store, `
  ASK { ?s foaf:name "Alice" }
`);

console.log(exists); // true or false
```

**Use case:** Check if data exists before inserting.

---

### CONSTRUCT Queries (Build Graphs)

```javascript
import { executeConstructSync } from '@unrdf/core';

const graph = executeConstructSync(store, `
  CONSTRUCT {
    ?person ex:hasName ?name .
  } WHERE {
    ?person foaf:name ?name .
  }
`);

// graph is array of quads
console.log(graph.length); // Number of triples
```

**Use case:** Transform RDF data to different schema.

---

### Manual Quad Operations

```javascript
import { namedNode, literal, quad } from '@unrdf/core';

// Create terms
const alice = namedNode('http://example.org/Alice');
const name = namedNode('http://xmlns.com/foaf/0.1/name');
const aliceLit = literal('Alice');

// Create quad
const q = quad(alice, name, aliceLit);

// Add to store
store.add(q);

// Remove from store
store.delete(q);

// Query by pattern
const matches = store.match(alice, null, null); // All quads with alice as subject
```

**Use case:** Programmatic triple manipulation (less common than SPARQL).

---

### Common Prefixes

```javascript
import { FOAF, DCTERMS, RDF, RDFS, OWL, XSD, COMMON_PREFIXES } from '@unrdf/core';

// Use constants
const person = namedNode(FOAF + 'Person');
const type = namedNode(RDF + 'type');

// COMMON_PREFIXES for SPARQL
const query = `
  ${COMMON_PREFIXES}
  SELECT ?name WHERE {
    ?person foaf:name ?name .
  }
`;
```

---

## 📦 @unrdf/yawl - Workflow Engine

### Create Workflow

```javascript
import { createWorkflow } from '@unrdf/yawl';

const workflow = createWorkflow({
  id: 'order-fulfillment',
  name: 'Order Fulfillment Process',

  tasks: [
    { id: 'validate', name: 'Validate Order', kind: 'atomic' },
    { id: 'charge', name: 'Charge Payment', kind: 'atomic' },
    { id: 'ship', name: 'Ship Product', kind: 'atomic' }
  ],

  flows: [
    { from: 'validate', to: 'charge' },
    { from: 'charge', to: 'ship' }
  ]
});
```

**Task kinds:** `atomic`, `composite`, `multiple-instance`, `automated`

---

### Create Case (Workflow Instance)

```javascript
import { createCase } from '@unrdf/yawl';

const orderCase = await createCase(workflow, {
  caseId: 'ORD-12345',
  data: {
    orderId: '12345',
    customerId: 'alice',
    items: [{ sku: 'ABC', qty: 2 }]
  }
});

console.log(orderCase.id);     // 'ORD-12345'
console.log(orderCase.status); // 'active'
```

---

### Task Lifecycle

```javascript
import { enableTask, startTask, completeTask } from '@unrdf/yawl';

// 1. Enable task (ready to execute)
await enableTask(orderCase, 'validate');

// 2. Start task (assign to worker)
await startTask(orderCase, 'validate', {
  assignedTo: 'worker:bob'
});

// 3. Complete task (mark done + output data)
await completeTask(orderCase, 'validate', {
  valid: true,
  validatedBy: 'worker:bob'
});

// Next task auto-enabled by control flow
console.log(orderCase.enabledTasks); // ['charge']
```

**States:** `enabled` → `started` → `completed`

---

### Query Case State

```javascript
// Check case status
console.log(orderCase.status); // 'active', 'completed', 'failed'

// List completed tasks
console.log(orderCase.completedTasks); // ['validate', 'charge']

// List enabled tasks (ready to execute)
console.log(orderCase.enabledTasks); // ['ship']

// Access case data
console.log(orderCase.data.customerId); // 'alice'
```

---

### Cancel Work Item

```javascript
import { cancelWorkItem } from '@unrdf/yawl';

await cancelWorkItem(orderCase, 'ship', {
  reason: 'Customer cancelled order'
});

console.log(orderCase.status); // 'cancelled'
```

---

### Workflow Patterns (Van der Aalst)

```javascript
import { parallelSplit, synchronization, exclusiveChoice } from '@unrdf/yawl';

// Parallel split: Execute tasks A and B concurrently
const wf1 = createWorkflow({
  tasks: [
    { id: 'start', name: 'Start' },
    { id: 'taskA', name: 'Task A' },
    { id: 'taskB', name: 'Task B' },
    { id: 'end', name: 'End' }
  ],
  flows: [
    { from: 'start', to: 'taskA' },
    { from: 'start', to: 'taskB' },
    { from: 'taskA', to: 'end' },
    { from: 'taskB', to: 'end' }
  ],
  splitBehavior: 'AND', // Parallel split
  joinBehavior: 'AND'   // Synchronization (wait for both)
});

// Exclusive choice: Either task A OR task B
const wf2 = createWorkflow({
  /* ... */
  splitBehavior: 'XOR', // Exclusive choice
  joinBehavior: 'XOR'   // Simple merge
});
```

**Patterns:** AND (parallel), XOR (exclusive), OR (multi-choice)

---

### Replay Case (Event Sourcing)

```javascript
import { replayCase } from '@unrdf/yawl';

// Reconstruct case from event log
const reconstructed = await replayCase(workflow, 'ORD-12345');

console.log(reconstructed.completedTasks); // All tasks completed historically
```

**Use case:** Audit, debugging, time-travel

---

### Cryptographic Receipts

Every state transition returns a cryptographic receipt:

```javascript
const receipt = await completeTask(store, {
  caseId: 'case-123',
  workItemId: 'wi-456',
});

console.log({
  receipt_id: receipt.receipt_id,     // Unique receipt ID
  hash: receipt.hash,                 // SHA-256 hash
  previous_hash: receipt.previous_hash, // Previous receipt hash (chain)
  timestamp: receipt.timestamp,       // ISO 8601 timestamp
  decision: receipt.decision,         // ACCEPT | REJECT
  justification: receipt.justification, // Human-readable explanation
  enabled_tasks: receipt.enabled_tasks, // Next tasks enabled
});
```

---

## 📦 @unrdf/kgc-4d - Event Sourcing & Time-Travel

### Initialize Store

```javascript
import { KGCStore } from '@unrdf/kgc-4d';

const kgc = new KGCStore({
  gitDir: './kgc-data', // Optional: persist to disk
  autoCommit: true      // Optional: Git commit every freeze
});
```

### Record Events

```javascript
// Basic event
await kgc.record({
  type: 'USER_CREATED',
  subject: 'user:alice',
  data: { email: 'alice@example.com', role: 'admin' }
});

// Event with custom timestamp
await kgc.record({
  type: 'ORDER_PAID',
  subject: 'order:12345',
  data: { amount: 99.99 },
  timestamp: Date.now() * 1_000_000 // nanoseconds
});
```

**Timestamp precision:** Nanoseconds (unique ordering guaranteed)

---

### Freeze Universe (Snapshot)

```javascript
import { freezeUniverse } from '@unrdf/kgc-4d';

// Create immutable snapshot
const snapshot = await freezeUniverse(kgc, {
  message: 'End of day snapshot',
  tag: 'EOD-2024-12-25'
});

console.log(snapshot.commit);    // Git SHA
console.log(snapshot.timestamp); // Freeze time
console.log(snapshot.eventCount); // Events in snapshot
```

**Use case:** Daily/hourly snapshots, compliance audit points

---

### Time-Travel Reconstruction

```javascript
import { reconstructState } from '@unrdf/kgc-4d';

// Reconstruct at specific commit
const pastState = await reconstructState(snapshot.commit);

console.log(pastState.events.length); // All events up to that point
console.log(pastState.timestamp);     // Snapshot time

// Access events
pastState.events.forEach(event => {
  console.log(event.type, event.subject, event.data);
});
```

**Use case:** Debugging, compliance, "what was state at 2pm yesterday?"

---

### Query Events

```javascript
// All events for a subject
const userEvents = kgc.queryEvents({ subject: 'user:alice' });

// Events by type
const creations = kgc.queryEvents({ type: 'USER_CREATED' });

// Events in time range
const recentEvents = kgc.queryEvents({
  after: Date.now() - 3600000, // Last hour
  before: Date.now()
});
```

---

### Verify Receipt (Cryptographic Proof)

```javascript
import { verifyReceipt } from '@unrdf/kgc-4d';

// Generate receipt when recording
const receipt = await kgc.record({
  type: 'PAYMENT',
  subject: 'txn:789',
  data: { amount: 500 },
  generateReceipt: true
});

// Later: verify authenticity
const isValid = verifyReceipt(receipt, kgc);
console.log(isValid); // true/false

// Receipt contains cryptographic hash chain
console.log(receipt.hash);      // SHA-256 of event
console.log(receipt.prevHash);  // Links to previous event
```

**Use case:** Tamper-proof audit trails (financial, medical records)

---

## 📦 @unrdf/hooks - Reactive Knowledge Behaviors

### Define Hook

```javascript
import { defineHook } from '@unrdf/hooks';

const validateAge = defineHook({
  meta: {
    name: 'validate-minimum-age',
    description: 'Ensure age >= 18'
  },

  trigger: 'INSERT', // Or 'UPDATE', 'DELETE'

  pattern: '?person <http://example.org/age> ?age .',

  validate(context) {
    const age = parseInt(context.quad.object.value, 10);

    if (age < 18) {
      return {
        passed: false,
        error: 'Must be 18 or older'
      };
    }

    return { passed: true };
  }
});
```

**Trigger types:** `INSERT`, `UPDATE`, `DELETE`, `QUERY`

---

### Register & Execute Hook

```javascript
import { registerHook, executeHook } from '@unrdf/hooks';

// Register globally
registerHook(validateAge);

// Execute on quad
const result = await executeHook(validateAge, {
  quad: myQuad,
  trigger: 'INSERT',
  store: myStore
});

if (!result.passed) {
  console.error(result.error);
}
```

---

### Transform Hook (Modify Data)

```javascript
const normalizeEmail = defineHook({
  meta: { name: 'normalize-email' },
  trigger: 'INSERT',
  pattern: '?person foaf:mbox ?email .',

  transform(context) {
    const email = context.quad.object.value.toLowerCase();

    return {
      ...context.quad,
      object: literal(email)
    };
  }
});
```

**Use case:** Data normalization, enrichment

---

### Hook Chain (Multiple Hooks)

```javascript
import { executeHookChain } from '@unrdf/hooks';

const hooks = [validateAge, normalizeEmail, logInsertion];

const results = await executeHookChain(hooks, {
  quad: myQuad,
  trigger: 'INSERT'
});

// Check if all passed
const allPassed = results.every(r => r.passed);
```

---

### Built-in Hooks

```javascript
import {
  validateSubjectIRI,
  validatePredicateIRI,
  normalizeLanguageTag,
  trimLiterals,
  rejectBlankNodes
} from '@unrdf/hooks';

registerHook(validateSubjectIRI); // Ensures subject is valid IRI
registerHook(trimLiterals);       // Trim whitespace from literals
```

**Available:** IRI validation, language tag normalization, blank node rejection

---

## 📦 @unrdf/oxigraph - High-Performance RDF Store

### Create Store

```javascript
import { createStore } from '@unrdf/oxigraph';

const store = createStore();
// Fast, WASM-based RDF triple store
```

---

### Load & Query

```javascript
// Load Turtle data
store.load(turtleData, { format: 'text/turtle' });

// SPARQL query
const results = store.query('SELECT ?s WHERE { ?s ?p ?o }');

// Iterate
for (const binding of results) {
  console.log(binding.get('s').value);
}
```

---

## 🔍 Common Patterns (Copy-Paste Ready)

### Pattern 1: Load → Query → Export

```javascript
import { createStore as createOxiStore } from '@unrdf/oxigraph';
import { executeSelectSync } from '@unrdf/core';

// Load
const store = createOxiStore();
store.load(myTurtleData);

// Query
const results = executeSelectSync(store, mySparql);

// Export as array
const rows = [...results].map(r => ({
  name: r.get('name')?.value,
  age: parseInt(r.get('age')?.value, 10)
}));

console.log(rows);
```

---

### Pattern 2: Event Sourcing with Snapshots

```javascript
import { KGCStore, freezeUniverse, reconstructState } from '@unrdf/kgc-4d';

const kgc = new KGCStore();

// Record events throughout day
await kgc.record({ type: 'EVENT_1', subject: 'entity:1', data: {} });
await kgc.record({ type: 'EVENT_2', subject: 'entity:2', data: {} });

// End of day: snapshot
const snapshot = await freezeUniverse(kgc, { message: 'EOD' });

// Next day: start fresh or time-travel
const yesterday = await reconstructState(snapshot.commit);
```

---

### Pattern 3: Hook Pipeline (Validation + Transform)

```javascript
import { defineHook, executeHookChain } from '@unrdf/hooks';

const validateHook = defineHook({
  meta: { name: 'validate' },
  trigger: 'INSERT',
  pattern: '?s ?p ?o .',
  validate(ctx) { /* ... */ }
});

const transformHook = defineHook({
  meta: { name: 'transform' },
  trigger: 'INSERT',
  pattern: '?s ?p ?o .',
  transform(ctx) { /* ... */ }
});

// Execute pipeline
const results = await executeHookChain([validateHook, transformHook], {
  quad: myQuad,
  trigger: 'INSERT'
});
```

---

### Pattern 4: Workflow with Data Flow

```javascript
import { createWorkflow, createCase, enableTask, completeTask } from '@unrdf/yawl';

const wf = createWorkflow({
  id: 'data-flow-example',
  tasks: [
    { id: 'input', name: 'Get Input' },
    { id: 'process', name: 'Process Data' },
    { id: 'output', name: 'Output Result' }
  ],
  flows: [
    { from: 'input', to: 'process' },
    { from: 'process', to: 'output' }
  ]
});

const wfCase = await createCase(wf, { caseId: 'CASE-1' });

// Task 1: Output data
await completeTask(wfCase, 'input', { inputValue: 42 });

// Task 2: Access previous task output
await completeTask(wfCase, 'process', {
  processedValue: wfCase.data.inputValue * 2
});

// Task 3: Final output
await completeTask(wfCase, 'output', {
  result: wfCase.data.processedValue
});
```

---

### Pattern 5: Zod Validation

All inputs validated with Zod schemas:

```javascript
import { WorkflowSpecSchema } from '@unrdf/yawl';

// Validate before creating
const validatedSpec = WorkflowSpecSchema.parse(workflowSpec);
const receipt = await createWorkflow(store, validatedSpec);
```

---

### Pattern 6: SPARQL Prefixes

Use consistent namespace prefixes:

```javascript
const query = `
  PREFIX yawl: <http://yawl.sourceforge.net/ontology/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

  SELECT ?task ?status
  WHERE {
    ?workItem yawl:taskRef ?task ;
              yawl:status ?status .
  }
`;
```

---

## 🚀 Performance Tips

### 1. Use Sync APIs Where Possible

```javascript
// ✅ FASTER (no async overhead)
const results = executeSelectSync(store, sparql);

// ❌ SLOWER (async overhead)
const results = await executeSelect(store, sparql);
```

**When sync is OK:** Node.js, no I/O in query

---

### 2. Batch Operations

```javascript
import { executeBatch } from '@unrdf/hooks';

// ✅ Batch 1000 quads at once
const results = await executeBatch(hooks, quads);

// ❌ Loop 1000 times
for (const quad of quads) {
  await executeHook(hook, { quad });
}
```

**Speedup:** 10-100x for large datasets

---

### 3. Use Oxigraph, Not N3

```javascript
// ✅ Oxigraph (Rust, fast)
import { createStore } from '@unrdf/oxigraph';

// For reference: N3 (legacy approach, not recommended)
// import { Store } from 'n3';
```

**Speedup:** 100x for SPARQL queries (see ADR-001)

---

### 4. Prewarm Hook Cache

```javascript
import { prewarmHookCache } from '@unrdf/hooks';

// Load hooks into JIT cache
await prewarmHookCache(hooks);

// Now execution is 2-5x faster
```

---

### 5. Pool Quads (Zero-Allocation Transforms)

```javascript
import { createPooledTransform, quadPool } from '@unrdf/hooks';

const pooledHook = createPooledTransform(myTransformHook);

// Reuses quad objects (no GC pressure)
const result = await executeHook(pooledHook, { quad });
```

**Use case:** High-throughput streaming (millions of quads/sec)

---

### 6. Batch Load Data

Load data in bulk rather than one triple at a time:

```javascript
// ✅ Good: Load Turtle in bulk
store.load(turtleData, { format: 'text/turtle' });

// ❌ Bad: Add triples one by one
quads.forEach(quad => store.add(quad));
```

---

### 7. Use SPARQL Efficiently

Index-friendly queries with specific subjects/predicates:

```javascript
// ✅ Good: Specific subject
const results = store.query(`
  SELECT ?name WHERE {
    <http://example.com/alice> foaf:name ?name
  }
`);

// ❌ Less efficient: No constraints
const results = store.query(`
  SELECT * WHERE { ?s ?p ?o }
`);
```

---

## 🔧 Error Handling

### Validation Errors

```javascript
import { z } from 'zod';

try {
  const validated = WorkflowSpecSchema.parse(spec);
} catch (error) {
  if (error instanceof z.ZodError) {
    console.error('Validation failed:', error.errors);
  }
}
```

### SPARQL Errors

```javascript
try {
  const results = store.query(sparqlQuery);
} catch (error) {
  console.error('SPARQL error:', error.message);
}
```

### Receipt Rejection

```javascript
const receipt = await completeTask(store, options);

if (receipt.decision === 'REJECT') {
  console.error('Task rejected:', receipt.justification);
  // Handle rejection
} else {
  console.log('Task completed:', receipt.enabled_tasks);
}
```

---

## 📖 Full Documentation

| Package | Full API Docs |
|---------|---------------|
| **@unrdf/core** | [packages/core/README.md](../packages/core/README.md) |
| **@unrdf/yawl** | [packages/yawl/README.md](../packages/yawl/README.md) |
| **@unrdf/kgc-4d** | [packages/kgc-4d/README.md](../packages/kgc-4d/README.md) |
| **@unrdf/hooks** | [packages/hooks/README.md](../packages/hooks/README.md) |
| **@unrdf/oxigraph** | [packages/oxigraph/README.md](../packages/oxigraph/README.md) |

---

## TypeScript Support

All packages include JSDoc type definitions:

```javascript
/**
 * @typedef {Object} WorkflowSpec
 * @property {string} id - Workflow identifier
 * @property {string} name - Human-readable name
 * @property {Task[]} tasks - Task definitions
 * @property {Flow[]} flow - Control flow
 */

/**
 * Create workflow definition
 * @param {Store} store - RDF store
 * @param {WorkflowSpec} spec - Workflow specification
 * @returns {Promise<Receipt>} Creation receipt
 */
export async function createWorkflow(store, spec) {
  // ...
}
```

---

## Next Steps

- [Quick Start Guide](./guides/yawl-quickstart.md)
- [YAWL Examples](../examples/yawl/)
- [Use Case Guides](./guides/yawl-use-cases.md)
- [Package READMEs](../packages/)

---

**Need migration help?** → [MIGRATION.md](MIGRATION.md)
**Understand design decisions?** → [adr/](adr/)
**Questions?** See [UNRDF Documentation](https://github.com/unrdf/unrdf) or open an issue.
