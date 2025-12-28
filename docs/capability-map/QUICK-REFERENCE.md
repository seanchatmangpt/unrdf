# UNRDF Quick Reference

**One-page cheat sheet** for common UNRDF operations across all packages.

---

## Core RDF Operations

### Create Store
```javascript
import { createStore } from '@unrdf/oxigraph';
const store = createStore();
```

### Add Data
```javascript
import { namedNode, literal } from '@unrdf/oxigraph';
store.insert(store.dataFactory.quad(
  namedNode('http://example.org/s'),
  namedNode('http://example.org/p'),
  literal('object')
));
```

### Query Data
```javascript
import { executeSelect } from '@unrdf/core';
const results = await executeSelect(store, `
  SELECT ?s ?p ?o WHERE { ?s ?p ?o }
`);
```

---

## SPARQL Query Types

| Type | Purpose | Returns | Example |
|------|---------|---------|---------|
| **SELECT** | Retrieve bindings | Array of objects | `SELECT ?name WHERE { ?person foaf:name ?name }` |
| **ASK** | Boolean test | true/false | `ASK { ?s rdf:type foaf:Person }` |
| **CONSTRUCT** | Build new graph | Quads | `CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }` |
| **DESCRIBE** | Get resource description | Quads | `DESCRIBE <http://example.org/resource>` |

---

## Validation & Hooks

### Define Validation Hook
```javascript
import { defineHook } from '@unrdf/hooks';
const hook = defineHook({
  trigger: 'before:insert',
  validate: (quad) => quad.object.value.length < 1000,
  onFailure: (quad) => console.error('Validation failed', quad)
});
```

### Execute Hook
```javascript
import { executeHook } from '@unrdf/hooks';
if (await executeHook(hook, quad)) {
  await store.insert(quad);
}
```

---

## Workflow Orchestration

### Create Workflow
```javascript
import { createWorkflow } from '@unrdf/yawl';
const workflow = createWorkflow({
  id: 'my-workflow',
  tasks: [
    { id: 'task1', execute: async () => { /* ... */ } },
    { id: 'task2', execute: async () => { /* ... */ } }
  ]
});
```

### Execute Workflow
```javascript
import { executeWorkflow } from '@unrdf/yawl';
await executeWorkflow(workflow, { /* context */ });
```

---

## Temporal Events (KGC-4D)

### Create Temporal Store
```javascript
import { createTemporalStore } from '@unrdf/kgc-4d';
const store = createTemporalStore();
```

### Record Event
```javascript
import { recordEvent } from '@unrdf/kgc-4d';
await recordEvent(store, {
  type: 'DataInserted',
  quad: /* quad */,
  timestamp: Date.now()
});
```

### Time Travel Query
```javascript
import { queryAtTime } from '@unrdf/kgc-4d';
const historicalData = await queryAtTime(store, timestamp, query);
```

---

## Common Namespaces

```javascript
import { RDF, RDFS, OWL, DCTERMS } from '@unrdf/core';

RDF.type          // http://www.w3.org/1999/02/22-rdf-syntax-ns#type
RDFS.label        // http://www.w3.org/2000/01/rdf-schema#label
OWL.sameAs        // http://www.w3.org/2002/07/owl#sameAs
DCTERMS.created   // http://purl.org/dc/terms/created
```

---

## Package Categories Cheat Sheet

| Category | Key Packages | Primary Use Cases |
|----------|-------------|-------------------|
| **RDF & Storage** | core, oxigraph, caching | Store and query RDF data |
| **Governance** | hooks, validation | Policy enforcement, validation |
| **Temporal** | kgc-4d, blockchain | Event sourcing, time-travel |
| **Workflow** | yawl, yawl-* | Orchestration, execution |
| **AI & ML** | knowledge-engine, ml-inference | Reasoning, embeddings |
| **Infrastructure** | cli, observability | Tooling, monitoring |

---

## Performance Tips

1. **Batch Operations**: Insert multiple quads at once for better throughput
2. **Use Indexes**: Query with specific predicates to leverage indexes
3. **Cache Results**: Use `@unrdf/caching` for frequently accessed queries
4. **Stream Large Data**: Use streaming parsers for files >10MB
5. **Enable OTEL**: Monitor performance with OpenTelemetry spans

---

## Error Handling

```javascript
import { ValidationError } from '@unrdf/core';

try {
  await store.insert(quad);
} catch (error) {
  if (error instanceof ValidationError) {
    console.error('Invalid RDF:', error.message);
  } else {
    console.error('Unexpected error:', error);
  }
}
```

---

## API Maturity Legend

- ✅ **mature** - Production-ready, stable
- ⚡ **stable** - API finalized, minor changes possible
- 📝 **documented** - API defined, may evolve

---

**Last Updated**: 2025-12-28
**Full API Reference**: [API-REFERENCE.md](./API-REFERENCE.md)
