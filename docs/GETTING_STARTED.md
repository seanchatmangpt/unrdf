# Getting Started with UNRDF

Get productive in 15 minutes with this hands-on guide. By the end, you'll have created your first knowledge graph application with SPARQL queries and validation.

## Table of Contents

1. [Quick Start (5 minutes)](#quick-start-5-minutes)
2. [Core Concepts (5 minutes)](#core-concepts-5-minutes)
3. [Your First Application (5 minutes)](#your-first-application-5-minutes)
4. [Next Steps](#next-steps)
5. [Troubleshooting](#troubleshooting)

---

## Quick Start (5 minutes)

### 1. Install UNRDF

```bash
# For new projects
mkdir my-rdf-app && cd my-rdf-app
pnpm init
pnpm add @unrdf/core

# For existing projects
pnpm add @unrdf/core

# Using npm
npm install @unrdf/core

# Using yarn
yarn add @unrdf/core
```

Add `"type": "module"` to `package.json`:

```json
{
  "name": "my-rdf-app",
  "type": "module",
  "dependencies": {
    "@unrdf/core": "^5.0.0"
  }
}
```

### 2. Run Your First Example

Create `hello-rdf.mjs`:

```javascript
import { createStore, namedNode, literal, FOAF } from '@unrdf/core';

// Create an RDF store
const store = createStore();

// Add some triples
store.addQuad(namedNode('http://example.org/Alice'), FOAF.name, literal('Alice Smith'));

store.addQuad(
  namedNode('http://example.org/Alice'),
  FOAF.knows,
  namedNode('http://example.org/Bob')
);

// Query the store
const quads = store.getQuads(namedNode('http://example.org/Alice'), null, null);

console.log(`Alice has ${quads.length} properties:`);
for (const quad of quads) {
  console.log(`  ${quad.predicate.value} -> ${quad.object.value}`);
}

// Output:
// Alice has 2 properties:
//   http://xmlns.com/foaf/0.1/name -> Alice Smith
//   http://xmlns.com/foaf/0.1/knows -> http://example.org/Bob
```

Run it:

```bash
node hello-rdf.mjs
```

You should see:

```
Alice has 2 properties:
  http://xmlns.com/foaf/0.1/name -> Alice Smith
  http://xmlns.com/foaf/0.1/knows -> http://example.org/Bob
```

### 3. Start Developing

For monorepo contributors:

```bash
# Clone and install
git clone https://github.com/unrdf/unrdf.git
cd unrdf
pnpm install
pnpm build

# Start development mode
pnpm dev

# In another terminal, run tests
pnpm test:fast
```

---

## Core Concepts (5 minutes)

### What is UNRDF?

UNRDF is a knowledge graph platform that combines:

- **RDF Triple Store** - Store semantic data as subject-predicate-object triples
- **SPARQL Queries** - SQL-like queries for graphs
- **Knowledge Hooks** - Reactive behaviors triggered by data changes
- **SHACL Validation** - Schema validation for data quality
- **Real-time Observability** - OpenTelemetry integration

### Key Components

#### 1. @unrdf/core - RDF store and SPARQL

```javascript
import { createStore, namedNode, literal } from '@unrdf/core';

const store = createStore();

// Add a triple: Alice -> knows -> Bob
store.addQuad(
  namedNode('http://example.org/Alice'),
  namedNode('http://xmlns.com/foaf/0.1/knows'),
  namedNode('http://example.org/Bob')
);
```

#### 2. @unrdf/hooks - Reactive behaviors

```javascript
import { defineHook } from '@unrdf/hooks';

const hook = defineHook({
  meta: { name: 'notify-on-new-person' },
  trigger: 'INSERT',
  pattern: '?person a foaf:Person .',

  run(event) {
    console.log(`New person: ${event.quad.subject.value}`);
  },
});
```

#### 3. @unrdf/oxigraph - Persistent storage

```javascript
import { createStore } from '@unrdf/oxigraph';

const store = await createStore({
  path: './my-knowledge-base.db',
});
```

### RDF Basics

**Triple**: The fundamental unit of RDF

```
Subject  -> Predicate -> Object
Alice    -> knows     -> Bob
Alice    -> name      -> "Alice Smith"
Bob      -> age       -> 30
```

**Quad**: A triple + graph (context)

```javascript
import { quad, namedNode, literal, defaultGraph } from '@unrdf/core';

const myQuad = quad(
  namedNode('http://example.org/Alice'), // subject
  namedNode('http://xmlns.com/foaf/0.1/name'), // predicate
  literal('Alice Smith'), // object
  defaultGraph() // graph
);
```

**Common Namespaces** (shortcuts for long URIs)

```javascript
import { FOAF, RDF, RDFS, XSD, OWL } from '@unrdf/core';

// FOAF = http://xmlns.com/foaf/0.1/
// RDF = http://www.w3.org/1999/02/22-rdf-syntax-ns#
// RDFS = http://www.w3.org/2000/01/rdf-schema#
// XSD = http://www.w3.org/2001/XMLSchema#

store.addQuad(
  namedNode('http://example.org/Alice'),
  FOAF.name, // Shortcut for http://xmlns.com/foaf/0.1/name
  literal('Alice Smith')
);
```

---

## Your First Application (5 minutes)

Let's build a team directory knowledge graph with SPARQL queries.

### Step 1: Create the Data Model

Create `team-directory.mjs`:

```javascript
import { createStore, namedNode, literal, executeSelectSync, FOAF, RDF } from '@unrdf/core';

// Create store
const store = createStore();

// Helper function
const ex = name => namedNode(`http://example.org/${name}`);

// Add team members
function addPerson(id, name, email, role) {
  store.addQuad(ex(id), RDF.type, FOAF.Person);
  store.addQuad(ex(id), FOAF.name, literal(name));
  store.addQuad(ex(id), FOAF.mbox, literal(email));
  store.addQuad(ex(id), ex('role'), literal(role));
}

addPerson('alice', 'Alice Smith', 'alice@example.com', 'Backend Developer');
addPerson('bob', 'Bob Johnson', 'bob@example.com', 'Frontend Developer');
addPerson('carol', 'Carol White', 'carol@example.com', 'DevOps Engineer');

// Add relationships
store.addQuad(ex('alice'), FOAF.knows, ex('bob'));
store.addQuad(ex('alice'), FOAF.knows, ex('carol'));
store.addQuad(ex('bob'), FOAF.knows, ex('carol'));

console.log(`✅ Knowledge graph created with ${store.size} triples\n`);
```

### Step 2: Query with SPARQL

Add to `team-directory.mjs`:

```javascript
// Query all team members
const teamMembers = executeSelectSync(
  store,
  `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  SELECT ?name ?role ?email WHERE {
    ?person a foaf:Person ;
            foaf:name ?name ;
            foaf:mbox ?email ;
            ex:role ?role .
  }
  ORDER BY ?name
`
);

console.log('Team Members:');
teamMembers.forEach(row => {
  console.log(`  ${row.get('name').value}`);
  console.log(`    Role: ${row.get('role').value}`);
  console.log(`    Email: ${row.get('email').value}\n`);
});

// Find who Alice knows
const aliceFriends = store.getQuads(ex('alice'), FOAF.knows, null);

console.log('Alice knows:');
for (const quad of aliceFriends) {
  const friend = quad.object;
  const nameQuads = store.getQuads(friend, FOAF.name, null);
  if (nameQuads.length > 0) {
    console.log(`  - ${nameQuads[0].object.value}`);
  }
}
```

### Step 3: Run It

```bash
node team-directory.mjs
```

**Expected Output:**

```
✅ Knowledge graph created with 12 triples

Team Members:
  Alice Smith
    Role: Backend Developer
    Email: alice@example.com

  Bob Johnson
    Role: Frontend Developer
    Email: bob@example.com

  Carol White
    Role: DevOps Engineer
    Email: carol@example.com

Alice knows:
  - Bob Johnson
  - Carol White
```

### Step 4: Add Advanced Queries

```javascript
// Find friends of friends
const foaf = executeSelectSync(
  store,
  `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT DISTINCT ?person ?friendOfFriend WHERE {
    ?person foaf:knows ?friend .
    ?friend foaf:knows ?friendOfFriend .
    FILTER(?person != ?friendOfFriend)
  }
`
);

console.log('\nFriends of Friends:');
foaf.forEach(row => {
  const personName = store.getQuads(row.get('person'), FOAF.name, null)[0].object.value;
  const foafName = store.getQuads(row.get('friendOfFriend'), FOAF.name, null)[0].object.value;
  console.log(`  ${personName} -> ${foafName}`);
});
```

---

## Next Steps

### Learn by Example

UNRDF provides ready-to-run examples in the `examples/` directory:

- **[01-hello-rdf.mjs](../examples/01-hello-rdf.mjs)** - Basic RDF operations
- **[02-sparql-queries.mjs](../examples/02-sparql-queries.mjs)** - SPARQL query patterns
- **[03-knowledge-hooks.mjs](../examples/03-knowledge-hooks.mjs)** - Reactive behaviors
- **[04-validation.mjs](../examples/04-validation.mjs)** - SHACL validation
- **[05-advanced-patterns.mjs](../examples/05-advanced-patterns.mjs)** - Advanced use cases

Run any example:

```bash
node examples/01-hello-rdf.mjs
```

### Common Use Cases

Explore production patterns in the documentation:

- **Knowledge Management** - Build organizational knowledge graphs
- **Semantic Search** - Implement intelligent search
- **Policy Enforcement** - Define and enforce business rules
- **Data Integration** - Combine data from multiple sources

### Development Workflow

For monorepo contributors:

```bash
# Run tests in watch mode
pnpm test:watch

# Run fast tests only (< 5s timeout)
pnpm test:fast

# Lint and fix
pnpm lint:fix

# Format code
pnpm format

# Build all packages
pnpm build

# Run OTEL validation
node validation/run-all.mjs comprehensive
```

### Testing Your Applications

```javascript
import { describe, it, expect } from 'vitest';
import { createStore, namedNode, literal, FOAF } from '@unrdf/core';

describe('Team Directory', () => {
  it('should add and query person data', () => {
    const store = createStore();
    const alice = namedNode('http://example.org/alice');

    store.addQuad(alice, FOAF.name, literal('Alice Smith'));

    const quads = store.getQuads(alice, FOAF.name, null);
    expect(quads).toHaveLength(1);
    expect(quads[0].object.value).toBe('Alice Smith');
  });

  it('should execute SPARQL queries', () => {
    const store = createStore();
    store.addQuad(namedNode('http://example.org/alice'), FOAF.name, literal('Alice'));

    const results = executeSelectSync(
      store,
      `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT ?name WHERE {
        ?person foaf:name ?name .
      }
    `
    );

    expect(results.length).toBe(1);
    expect(results[0].get('name').value).toBe('Alice');
  });
});
```

Run tests:

```bash
pnpm test
```

---

## Troubleshooting

### Common Issues

#### "Cannot find module '@unrdf/core'"

**Solution:**

```bash
# Install the package
pnpm add @unrdf/core

# Verify package.json has "type": "module"
cat package.json | grep '"type"'
```

#### "SyntaxError: Cannot use import statement"

**Problem:** Not using ES modules

**Solution:**

Add to `package.json`:

```json
{
  "type": "module"
}
```

Or use `.mjs` file extension.

#### "Store.match is not a function"

**Problem:** Using wrong API (N3.js instead of UNRDF)

**Solution:**

```javascript
// ❌ Wrong
import { Store } from 'n3';
const store = new Store();

// ✅ Correct
import { createStore } from '@unrdf/core';
const store = createStore();
```

#### "SPARQL query returns no results"

**Problem:** Prefix mismatch or typo

**Solution:**

```javascript
// Debug: Check what's in the store
for (const quad of store) {
  console.log(quad.subject.value, quad.predicate.value, quad.object.value);
}

// Verify prefixes match your data
const results = executeSelectSync(
  store,
  `
  SELECT * WHERE { ?s ?p ?o }
  LIMIT 10
`
);
console.log('Sample results:', results);
```

Enable debug logging:

```bash
DEBUG=unrdf:* node your-app.mjs
```

#### "Tests timing out"

**Problem:** Async operations or slow queries

**Solution:**

```bash
# Run fast tests only
pnpm test:fast

# Run specific package
pnpm -C packages/core test
```

Increase timeout for specific tests:

```javascript
it('slow operation', async () => {
  // ...
}, 10000); // 10 second timeout
```

#### "Memory issues with large graphs"

**Problem:** Loading too much data

**Solution:**

Use persistent storage instead of in-memory:

```javascript
import { createStore } from '@unrdf/oxigraph';

const store = await createStore({
  path: './my-db.db',
  // Data stored on disk, not in memory
});
```

Or use streaming for large files:

```javascript
import { createStreamParser } from '@unrdf/streaming';
import { createReadStream } from 'fs';

const parser = createStreamParser();
const stream = createReadStream('large-file.ttl');

stream.pipe(parser).on('data', quad => {
  processQuad(quad);
});
```

---

## Get Help

### Documentation

- **[Full Documentation](https://unrdf.dev/docs)** - Complete guides
- **[API Reference](./API-REFERENCE.md)** - Detailed API
- **[Architecture Guide](./ARCHITECTURE.md)** - System design
- **[Packages Overview](./PACKAGES.md)** - All 17 packages

### Community

- **[GitHub Issues](https://github.com/unrdf/unrdf/issues)** - Bug reports
- **[GitHub Discussions](https://github.com/unrdf/unrdf/discussions)** - Q&A
- **[Discord Community](https://discord.gg/unrdf)** - Chat

### External Resources

- **[RDF Primer](https://www.w3.org/TR/rdf11-primer/)** - W3C introduction
- **[SPARQL Tutorial](https://www.w3.org/TR/sparql11-query/)** - Learn SPARQL
- **[SHACL Specification](https://www.w3.org/TR/shacl/)** - Validation guide

---

## Quick Reference

### Essential Imports

```javascript
// Core RDF operations
import {
  createStore, // Create RDF store
  namedNode, // Create URI node
  literal, // Create literal value
  blankNode, // Create anonymous node
  quad, // Create quad
  executeSelectSync, // Execute SPARQL SELECT
} from '@unrdf/core';

// Common namespaces
import {
  RDF, // http://www.w3.org/1999/02/22-rdf-syntax-ns#
  RDFS, // http://www.w3.org/2000/01/rdf-schema#
  FOAF, // http://xmlns.com/foaf/0.1/
  XSD, // http://www.w3.org/2001/XMLSchema#
  OWL, // http://www.w3.org/2002/07/owl#
} from '@unrdf/core';

// Knowledge Hooks
import {
  defineHook, // Define reactive hook
  executeHook, // Execute hook manually
} from '@unrdf/hooks';
```

### Common Operations

```javascript
// Create store
const store = createStore();

// Add triple
store.addQuad(subject, predicate, object);

// Query (low-level)
const quads = store.getQuads(subject, predicate, object);

// Remove
store.removeQuad(quad);

// Count
const count = store.size;

// SPARQL (high-level)
const results = executeSelectSync(
  store,
  `
  SELECT ?s ?p ?o WHERE {
    ?s ?p ?o .
  }
  LIMIT 10
`
);
```

### SPARQL Query Types

```javascript
// SELECT - Get bindings
const rows = executeSelectSync(
  store,
  `
  SELECT ?name WHERE { ?person foaf:name ?name }
`
);

// ASK - Boolean check
const exists = executeAskSync(
  store,
  `
  ASK { ?person foaf:name "Alice" }
`
);

// CONSTRUCT - Create new graph
const newStore = executeConstructSync(
  store,
  `
  CONSTRUCT { ?p ex:knows ?o }
  WHERE { ?p foaf:knows ?o }
`
);
```

---

## What's Next?

You're ready to build! Some ideas:

1. **Build a knowledge base** for your domain
2. **Add semantic search** to your application
3. **Implement policy enforcement** with hooks
4. **Integrate UNRDF** into existing apps
5. **Contribute** to the project

Happy coding!

---

**Need more depth?** Check out:

- [ARCHITECTURE.md](./ARCHITECTURE.md) - System design
- [PACKAGES.md](./PACKAGES.md) - All 17 packages explained
- [API-REFERENCE.md](./API-REFERENCE.md) - Complete API docs
