# Quick Start

Get up and running with UNRDF in under 5 minutes. This tutorial will walk you through creating a simple knowledge graph, adding data, and querying it.

## Installation

First, install UNRDF using your preferred package manager:

```bash
pnpm add unrdf
```

```admonish info
UNRDF requires **Node.js ≥ 18.0.0**. Check your version with `node --version`.
```

## Your First Knowledge Graph

Create a new file `example.mjs` and add the following code:

```javascript
import { createDarkMatterCore } from 'unrdf';
import { namedNode, quad, literal } from '@rdfjs/data-model';

// 1. Create the knowledge engine
const system = await createDarkMatterCore();

// 2. Add some RDF data
await system.executeTransaction({
  additions: [
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Alice')
    ),
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/knows'),
      namedNode('http://example.org/bob')
    )
  ],
  removals: [],
  actor: 'system'
});

// 3. Query the data
const results = await system.query({
  query: 'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }',
  type: 'sparql-select'
});

console.log(results);
// Output: [{ name: 'Alice' }]

// 4. Cleanup
await system.cleanup();
```

Run it:

```bash
node example.mjs
```

**Expected output:**

```javascript
[{ name: 'Alice' }]
```

```admonish success title="Congratulations!"
You've just created your first UNRDF knowledge graph! In just 4 steps, you:
1. ✅ Created a Dark Matter core instance
2. ✅ Added RDF triples via a transaction
3. ✅ Queried data with SPARQL
4. ✅ Cleaned up resources
```

## What Just Happened?

Let's break down each step:

### Step 1: Create the Knowledge Engine

```javascript
const system = await createDarkMatterCore();
```

This creates a complete knowledge graph system with:

- **RDF Store** - In-memory quad storage using N3.js
- **Transaction Manager** - ACID guarantees with rollback support
- **Query Engine** - Full SPARQL 1.1 support via Comunica
- **Hook Manager** - Policy enforcement system
- **Observability** - OpenTelemetry instrumentation
- **Performance Optimizations** - Query caching, hook batching, parallel execution

All of this is initialized with sensible defaults and ready to use.

### Step 2: Add RDF Data

```javascript
await system.executeTransaction({
  additions: [
    quad(subject, predicate, object),
    // ... more quads
  ],
  removals: [],
  actor: 'system'
});
```

Transactions in UNRDF are **ACID-compliant**:

- **Atomic** - All additions/removals succeed or fail together
- **Consistent** - Knowledge Hooks enforce invariants
- **Isolated** - Concurrent transactions don't interfere
- **Durable** - Changes are persisted (if lockchain is enabled)

The `actor` parameter records who made the change - essential for audit trails.

```admonish tip
Use descriptive actor names like `'user@example.org'` or `'import-job-123'` to make audit trails meaningful.
```

### Step 3: Query the Data

```javascript
const results = await system.query({
  query: 'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }',
  type: 'sparql-select'
});
```

UNRDF supports all SPARQL 1.1 query types:

- `sparql-select` - Returns bindings (array of objects)
- `sparql-ask` - Returns boolean
- `sparql-construct` - Returns RDF graph
- `sparql-describe` - Returns RDF description

Query results are automatically optimized through:

- **LRU Query Cache** - Frequently-used queries are cached
- **Parallel Execution** - Independent subqueries run in parallel
- **Smart Indexing** - Optimized quad store indexes

### Step 4: Cleanup

```javascript
await system.cleanup();
```

Always cleanup your system to:

- Close database connections
- Clear caches and free memory
- Flush pending metrics
- Stop background workers

```admonish warning
Failing to cleanup can lead to memory leaks and resource exhaustion in long-running applications.
```

## Next Steps

Now that you've created your first knowledge graph, explore these topics:

### Learn Core Concepts

- **[Installation](installation.md)** - Detailed setup and configuration
- **[Basic Usage](basic-usage.md)** - RDF operations, SPARQL queries, SHACL validation
- **[First Hook](first-hook.md)** - Create your first Knowledge Hook

### Explore Features

**RDF Operations:**
```javascript
import { parseTurtle, toTurtle } from 'unrdf';

const store = await parseTurtle(`
  @prefix ex: <http://example.org/> .
  ex:alice ex:name "Alice" .
`);

const turtle = await toTurtle(store);
```

**SPARQL Queries:**
```javascript
// ASK query
const exists = await system.query({
  query: 'ASK { ?s ?p ?o }',
  type: 'sparql-ask'
});

// CONSTRUCT query
const graph = await system.query({
  query: 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }',
  type: 'sparql-construct'
});
```

**Knowledge Hooks:**
```javascript
import { defineHook, registerHook } from 'unrdf';

const hook = defineHook({
  meta: { name: 'validation', description: 'Ensure data quality' },
  when: {
    kind: 'sparql-ask',
    query: 'ASK { ?person a foaf:Person . FILTER NOT EXISTS { ?person foaf:name ?name } }'
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('All persons must have names');
    }
  }
});

await registerHook(hook);
```

**Cryptographic Audit Trails:**
```javascript
import { LockchainWriter } from 'unrdf';

const lockchain = new LockchainWriter({
  repoPath: './audit-trail',
  enableMerkle: true
});

await lockchain.init();

const receipt = await lockchain.writeReceipt({
  actor: 'alice@example.org',
  action: 'add-data',
  delta: { additions, removals },
  timestamp: new Date()
});
```

## Common Patterns

### Working with Turtle Files

```javascript
import { parseTurtle } from 'unrdf';
import { readFileSync } from 'node:fs';

// Load Turtle from file
const ttl = readFileSync('./data.ttl', 'utf8');
const store = await parseTurtle(ttl);

// Add to knowledge graph
await system.executeTransaction({
  additions: [...store],
  removals: [],
  actor: 'file-import'
});
```

### Complex SPARQL Queries

```javascript
const results = await system.query({
  query: `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX ex: <http://example.org/>

    SELECT ?person ?name ?friend ?friendName
    WHERE {
      ?person a foaf:Person ;
              foaf:name ?name ;
              foaf:knows ?friend .
      ?friend foaf:name ?friendName .
    }
    ORDER BY ?name
  `,
  type: 'sparql-select'
});
```

### Error Handling

```javascript
try {
  await system.executeTransaction({
    additions: [quad(s, p, o)],
    removals: [],
    actor: 'user'
  });
} catch (error) {
  console.error('Transaction failed:', error.message);
  // Transaction automatically rolled back
  // Original store state preserved
}
```

## Troubleshooting

### "Module not found" Error

```admonish danger title="Error"
Error [ERR_MODULE_NOT_FOUND]: Cannot find package 'unrdf'
```

**Solution:** Install UNRDF with `pnpm add unrdf` and ensure you're using Node.js ≥ 18.0.0.

### Query Timeout

```admonish danger title="Error"
Error: Query execution timeout after 30000ms
```

**Solution:** Optimize your SPARQL query or increase the timeout:

```javascript
const system = await createDarkMatterCore({
  timeoutMs: 60000 // 60 seconds
});
```

### Memory Issues

```admonish danger title="Error"
FATAL ERROR: Reached heap limit Allocation failed - JavaScript heap out of memory
```

**Solution:** For large graphs, configure Node.js with more memory:

```bash
NODE_OPTIONS="--max-old-space-size=4096" node example.mjs
```

## What's Next?

Continue your UNRDF journey:

1. **[Installation](installation.md)** - Learn about production setup and configuration
2. **[Basic Usage](basic-usage.md)** - Master RDF operations and SPARQL queries
3. **[First Hook](first-hook.md)** - Build your first autonomic Knowledge Hook

```admonish tip
Join our [GitHub Discussions](https://github.com/unrdf/unrdf/discussions) to ask questions and share your projects!
```

---

**Ready to dive deeper?** Head to [Installation](installation.md) for detailed setup instructions.
