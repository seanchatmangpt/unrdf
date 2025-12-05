# START-HERE: Quick Orientation Guide

Welcome to UNRDF! This guide will help you understand what UNRDF is and get you started in 5 minutes.

> **New to the UNRDF monorepo?** Start here for users. If you're contributing to UNRDF itself, check [MONOREPO-QUICK-REFERENCE.md](MONOREPO-QUICK-REFERENCE.md) instead.

## What is UNRDF?

UNRDF is a **knowledge graph platform for JavaScript/Node.js**. It lets you:
- Store semantic data as RDF (Resource Description Framework)
- Query it with SPARQL (like SQL for knowledge graphs)
- Validate it with SHACL
- Define autonomous behaviors with Knowledge Hooks
- Build production-grade applications

### Think of it as:
- **Semantic Database** - store relationships and facts
- **Graph Query Engine** - ask questions about your data
- **Autonomous Agent System** - define behaviors that react to changes
- **Interoperable Platform** - export to standard RDF formats

---

## The 80/20 - What You Really Need

### Level 1: Knowledge Substrate (START HERE)

Use `createKnowledgeSubstrateCore()` - it gives you everything:

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

// One call gives you everything
const core = await createKnowledgeSubstrateCore();

// Parse some data
const store = core.parseRdf(`
  @prefix ex: <http://example.org/> .
  ex:Alice ex:knows ex:Bob .
`);

// Query it
const results = await core.query(store, `
  SELECT ?person WHERE { ?person ?p ?o }
`);

console.log(results);
```

**That's it.** This single API gives you:
- RDF triple storage
- SPARQL querying
- SHACL validation
- Transactions (ACID operations)
- Knowledge Hooks (autonomous behaviors)
- Streaming
- Type safety
- Observability

**👉 Use this 99% of the time.**

### Level 2: Low-level APIs

Only if you need advanced control (rare):

```javascript
import { parseTurtle, query } from '@unrdf/core';

const store = parseTurtle(turtleData);
const results = query(store, sparqlQuery);
```

### Level 3: Individual Packages

Only for edge cases. See [PACKAGES.md](../PACKAGES.md).

---

## 5-Minute Quickstart

### 1. Install

```bash
npm install @unrdf/core
# or
pnpm add @unrdf/core
```

### 2. Create a Knowledge Base

Create `knowledge-base.mjs`:

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore();

// Define some people
const data = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:Alice foaf:name "Alice" ; foaf:knows ex:Bob, ex:Charlie .
  ex:Bob foaf:name "Bob" ; foaf:knows ex:Alice .
  ex:Charlie foaf:name "Charlie" ; foaf:knows ex:Alice .
`;

const store = core.parseRdf(data);
console.log('Store loaded:', store.size, 'triples');

export { core, store };
```

### 3. Query Your Data

Create `query.mjs`:

```javascript
import { core, store } from './knowledge-base.mjs';

// Find everyone and who they know
const results = await core.query(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?name ?friend
  WHERE {
    ?person foaf:name ?name ;
            foaf:knows ?knownPerson .
    ?knownPerson foaf:name ?friend .
  }
  ORDER BY ?name
`);

console.log('Friends:');
for (const row of results) {
  console.log(`  ${row.get('name').value} knows ${row.get('friend').value}`);
}
```

### 4. Run It

```bash
node query.mjs
```

Output:
```
Store loaded: 8 triples
Friends:
  Alice knows Bob
  Alice knows Charlie
  Bob knows Alice
  Charlie knows Alice
```

---

## Common Questions

### Q: Where do I put my data?

**In-memory** (default, for development):
```javascript
const store = core.parseRdf(turtleData);
```

**From a file**:
```javascript
import { readFileSync } from 'fs';
const rdfData = readFileSync('data.ttl', 'utf-8');
const store = core.parseRdf(rdfData);
```

**From a URL/SPARQL endpoint**:
```javascript
const results = await core.query(remoteEndpoint, sparqlQuery);
```

### Q: How do I add more data?

```javascript
// Add individual triples
import { namedNode, literal } from '@rdfjs/data-model';

store.addQuad(
  namedNode('http://example.org/Alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice Smith')
);
```

### Q: How do I validate my data?

Define SHACL shapes:

```javascript
const shapes = `
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix ex: <http://example.org/> .

  ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
      sh:path ex:name ;
      sh:minCount 1 ;
      sh:datatype xsd:string ;
    ] .
`;

const shapesStore = core.parseRdf(shapes);
const report = await core.validateShacl(store, shapesStore);
console.log(report);
```

### Q: How do I update data?

Use transactions:

```javascript
const tx = await core.beginTransaction();
try {
  // Make changes inside transaction
  store.addQuad(...);
  store.deleteQuad(...);
  await tx.commit();
} catch (error) {
  await tx.rollback();
}
```

### Q: How do I run this in the browser?

Use `@unrdf/browser`:

```html
<script type="module">
  import { createKnowledgeSubstrateCore } from 'https://cdn.jsdelivr.net/npm/@unrdf/browser';

  const core = await createKnowledgeSubstrateCore();
  const store = core.parseRdf(turtleData);
  const results = await core.query(store, sparqlQuery);

  console.log(results);
</script>
```

### Q: How do I define behaviors that react to changes?

Use Knowledge Hooks:

```javascript
// Define a hook that fires when data changes
const myHook = defineHook({
  meta: { name: 'log-changes' },
  trigger: 'INSERT',
  pattern: '?person ex:status ?status .',

  run(event) {
    console.log(`${event.quad.subject.value} status: ${event.quad.object.value}`);
  }
});

registerHook(myHook);

// Now when you add a triple matching the pattern, the hook runs
store.addQuad(...);  // Hook fires automatically!
```

---

## Next Steps

### For Users (Using UNRDF)
1. **Read** [ARCHITECTURE.md](../ARCHITECTURE.md) to understand how pieces fit together
2. **Explore** [PACKAGES.md](../PACKAGES.md) to see what each package does
3. **Try** [GETTING-STARTED/QUICK-START.md](GETTING-STARTED/QUICK-START.md) for more examples
4. **Learn** [GETTING-STARTED/INSTALLATION.md](GETTING-STARTED/INSTALLATION.md) for detailed setup
5. **Reference** [API-REFERENCE.md](../API-REFERENCE.md) for complete API docs

### For Contributors (Working on UNRDF)
1. **Quick overview** → [MONOREPO-QUICK-REFERENCE.md](MONOREPO-QUICK-REFERENCE.md) - What are all 17 packages?
2. **Set up locally** → [LOCAL-DEVELOPMENT.md](LOCAL-DEVELOPMENT.md) - Dev environment, tests, builds
3. **Understand layout** → [WORKSPACE-STRUCTURE.md](WORKSPACE-STRUCTURE.md) - File structure of packages
4. **Contribute code** → [PACKAGE-DEVELOPMENT.md](PACKAGE-DEVELOPMENT.md) - How to modify/add packages
5. **Test properly** → [TESTING-STRATEGY.md](TESTING-STRATEGY.md) - Cross-package testing

---

## Need Help?

- **Examples** - See [EXAMPLES.md](../EXAMPLES.md)
- **API Docs** - See [API-REFERENCE.md](../API-REFERENCE.md)
- **Issues** - https://github.com/unrdf/unrdf/issues
- **Discussions** - https://github.com/unrdf/unrdf/discussions

---

**Ready?** → [ARCHITECTURE.md](../ARCHITECTURE.md)
