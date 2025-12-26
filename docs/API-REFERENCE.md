# UNRDF API Reference

Complete API documentation for all UNRDF packages.

## Core Packages

### @unrdf/core

**RDF graph operations and SPARQL execution**

```bash
pnpm add @unrdf/core
```

**Key APIs:**

- [Store Operations](../packages/core/README.md#store-operations) - Create, add, query RDF triples
- [SPARQL Queries](../packages/core/README.md#sparql-queries) - SELECT, ASK, CONSTRUCT, DESCRIBE
- [Data Import/Export](../packages/core/README.md#data-importexport) - Load/dump RDF in multiple formats
- [RDF Term Factory](../packages/core/README.md#rdf-term-factory) - Create namedNodes, literals, quads

**Quick Example:**

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';
const store = createStore();
store.add(dataFactory.quad(subject, predicate, object));
const results = store.query('SELECT * WHERE { ?s ?p ?o }');
```

**Documentation:** [packages/core/README.md](../packages/core/README.md)

---

### @unrdf/yawl

**Workflow engine with event sourcing and cryptographic receipts**

```bash
pnpm add @unrdf/yawl
```

**Key APIs:**

- [Workflow API](../packages/yawl/README.md#core-api) - createWorkflow, createCase
- [Task Execution](../packages/yawl/README.md#task-execution) - enableTask, startTask, completeTask
- [Time Travel](../packages/yawl/README.md#time-travel--replay) - replayCase, event sourcing
- [Workflow Patterns](../packages/yawl/README.md#common-workflow-patterns) - Sequential, parallel, conditional

**Quick Example:**

```javascript
import { createWorkflow, createCase, completeTask } from '@unrdf/yawl';
const { workflow_id } = await createWorkflow(store, workflowSpec);
const { case_id } = await createCase(store, { workflowId: workflow_id });
await completeTask(store, { caseId: case_id, workItemId, outputData });
```

**Documentation:** [packages/yawl/README.md](../packages/yawl/README.md)

**Examples:** [examples/yawl/](../examples/yawl/)

**Guides:**

- [Quick Start](./guides/yawl-quickstart.md) - Get started in 10 minutes
- [Use Cases](./guides/yawl-use-cases.md) - Production scenarios

---

### @unrdf/oxigraph

**High-performance WASM-based RDF store**

```bash
pnpm add @unrdf/oxigraph
```

**Key APIs:**

- [createStore()](../packages/oxigraph/README.md#createstorequads) - Create RDF store
- [SPARQL 1.1](../packages/oxigraph/README.md#sparql-queries) - Full query and update support
- [Load/Dump](../packages/oxigraph/README.md#loading-rdf-data) - Multiple RDF formats

**Quick Example:**

```javascript
import { createStore } from '@unrdf/oxigraph';
const store = createStore();
store.load(turtleData, { format: 'text/turtle' });
const results = store.query('SELECT ?s WHERE { ?s ?p ?o }');
```

**Documentation:** [packages/oxigraph/README.md](../packages/oxigraph/README.md)

---

## Extended Packages

### @unrdf/hooks

**Policy enforcement and validation with declarative rules**

```bash
pnpm add @unrdf/hooks
```

**Use for:** Access control, validation rules, workflow policies

### @unrdf/kgc-4d

**KGC-4D event sourcing with time-travel**

```bash
pnpm add @unrdf/kgc-4d
```

**Use for:** Audit trails, event replay, temporal queries

### @unrdf/dark-matter

**Query optimization and performance analytics**

```bash
pnpm add @unrdf/dark-matter
```

**Use for:** Query performance tuning, index recommendations

### @unrdf/composables

**Reactive RDF with Vue.js composables**

```bash
pnpm add @unrdf/composables
```

**Use for:** Vue.js applications, reactive queries

### @unrdf/cli

**Command-line tools for RDF operations**

```bash
pnpm add -g @unrdf/cli
```

**Use for:** RDF conversion, graph commands, automation

---

## API Design Patterns

### Pattern 1: Store Creation

All packages use Oxigraph as the RDF store:

```javascript
import { createStore } from '@unrdf/oxigraph';

const store = createStore();
// Now use with any UNRDF package
```

### Pattern 2: Async Operations

All mutating operations are async:

```javascript
// Workflow operations
const receipt = await createWorkflow(store, spec);
const caseReceipt = await createCase(store, options);

// SPARQL queries (synchronous)
const results = store.query('SELECT * WHERE { ?s ?p ?o }');
```

### Pattern 3: Receipts

State-changing operations return cryptographic receipts:

```javascript
const receipt = await completeTask(store, options);

console.log({
  receipt_id: receipt.receipt_id,
  hash: receipt.hash,
  previous_hash: receipt.previous_hash,
  decision: receipt.decision, // ACCEPT | REJECT
  justification: receipt.justification,
  timestamp: receipt.timestamp,
});
```

### Pattern 4: Zod Validation

All inputs validated with Zod schemas:

```javascript
import { WorkflowSpecSchema } from '@unrdf/yawl';

// Validate before creating
const validatedSpec = WorkflowSpecSchema.parse(workflowSpec);
const receipt = await createWorkflow(store, validatedSpec);
```

### Pattern 5: SPARQL Prefixes

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

## Common Operations

### Creating a Store

```javascript
import { createStore } from '@unrdf/oxigraph';
const store = createStore();
```

### Adding RDF Data

```javascript
import { dataFactory } from '@unrdf/oxigraph';
const { namedNode, literal, quad } = dataFactory;

store.add(
  quad(
    namedNode('http://example.com/alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice')
  )
);
```

### Querying with SPARQL

```javascript
const results = store.query(`
  SELECT ?name WHERE {
    ?person foaf:name ?name
  }
`);

results.forEach(row => {
  console.log(row.name);
});
```

### Creating a Workflow

```javascript
import { createWorkflow } from '@unrdf/yawl';

const { workflow_id } = await createWorkflow(store, {
  id: 'my-workflow',
  name: 'My Workflow',
  tasks: [{ id: 'task1', name: 'Task 1', kind: 'atomic' }],
  flow: [],
});
```

### Executing Tasks

```javascript
import { createCase, enableTask, startTask, completeTask } from '@unrdf/yawl';

const { case_id } = await createCase(store, { workflowId: workflow_id });
const { work_item_id } = await enableTask(store, { caseId: case_id, taskId: 'task1' });
await startTask(store, { caseId: case_id, workItemId: work_item_id });
const receipt = await completeTask(store, { caseId: case_id, workItemId: work_item_id });
```

---

## Error Handling

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

## Performance Tips

### 1. Batch Operations

Load data in bulk rather than one triple at a time:

```javascript
// Good: Load Turtle in bulk
store.load(turtleData, { format: 'text/turtle' });

// Bad: Add triples one by one
quads.forEach(quad => store.add(quad));
```

### 2. Use SPARQL Efficiently

Index-friendly queries with specific subjects/predicates:

```javascript
// Good: Specific subject
const results = store.query(`
  SELECT ?name WHERE {
    <http://example.com/alice> foaf:name ?name
  }
`);

// Less efficient: No constraints
const results = store.query(`
  SELECT * WHERE { ?s ?p ?o }
`);
```

### 3. Cache Workflow Definitions

```javascript
const workflowCache = new Map();

function getWorkflow(workflowId) {
  if (!workflowCache.has(workflowId)) {
    const workflow = loadWorkflowFromStore(store, workflowId);
    workflowCache.set(workflowId, workflow);
  }
  return workflowCache.get(workflowId);
}
```

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

**Questions?** See [UNRDF Documentation](https://github.com/unrdf/unrdf) or open an issue.
