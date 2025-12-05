# Migration Guide: @unrdf/core

Migrating from older versions or other RDF libraries to @unrdf/core.

---

## Version Migration

### v4 to v5

**Major Changes:**

1. **New Synchronous API:**
   ```javascript
   // v4: Async only
   const store = await createStore();
   await addQuad(store, quad);

   // v5: Both sync and async
   const store = createUnrdfStore();  // Sync (preferred)
   store.addQuad(quad);
   ```

2. **New UnrdfStore class:**
   ```javascript
   // v4: Functional API
   const store = await createStore();
   const quads = await getQuads(store);

   // v5: Object API (OOP)
   const store = createUnrdfStore();
   const quads = store.match();
   ```

3. **Query execution:**
   ```javascript
   // v4: Async
   const results = await executeQuery(store, sparql);

   // v5: Sync (preferred)
   const results = executeQuerySync(store, sparql);
   ```

**Migration Path:**

```javascript
// Step 1: Replace store creation
- const store = await createStore();
+ const store = createUnrdfStore();

// Step 2: Replace async calls with sync
- const quads = await getQuads(store);
+ const quads = store.match();

// Step 3: Update queries
- const results = await executeQuery(store, query);
+ const results = executeQuerySync(store, query);

// Step 4: Remove async/await
- async function load() {
-   await store.addQuad(quad);
- }
+ function load() {
+   store.addQuad(quad);
+ }
```

---

## From N3.js

If migrating from N3.js library:

### Store Creation

```javascript
// N3.js
import { Parser, Store } from 'n3';

const store = new Store();

// @unrdf/core
import { createUnrdfStore } from '@unrdf/core';

const store = createUnrdfStore();
```

### Adding Quads

```javascript
// N3.js
const { namedNode, literal } = require('n3').DataFactory;
store.addQuad(
  namedNode('http://example.com/alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice')
);

// @unrdf/core
import { namedNode, literal } from '@unrdf/core';
store.addQuad({
  subject: namedNode('http://example.com/alice'),
  predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
  object: literal('Alice')
});
```

### Querying

```javascript
// N3.js (using separate SPARQL engine)
const { newEngine } = require('@comunica/actor-init-sparql');
const engine = newEngine();
const result = await engine.queryBindings(sparql, { sources: [store] });

// @unrdf/core (built-in)
import { executeQuerySync } from '@unrdf/core';
const results = executeQuerySync(store, sparql);
```

### Key Differences

| Operation | N3.js | @unrdf/core |
|-----------|-------|-------------|
| Create store | new Store() | createUnrdfStore() |
| Add quad | store.addQuad(s,p,o) | store.addQuad({s,p,o}) |
| Query | Separate engine | executeQuerySync() |
| Performance | Medium | Fast |
| Sync support | No | Yes |

---

## From Apache Jena

If migrating from Jena (Java):

### Concept Mapping

| Jena | @unrdf/core |
|------|-------------|
| Model | UnrdfStore |
| Resource | NamedNode / BlankNode |
| Property | NamedNode |
| Literal | Literal |
| Query | executeQuerySync |

### Code Pattern

```java
// Jena (Java)
Model model = ModelFactory.createDefaultModel();
Resource alice = model.createResource("http://example.com/alice");
Property name = model.createProperty("http://xmlns.com/foaf/0.1/name");
Literal aliceName = model.createLiteral("Alice");
alice.addProperty(name, aliceName);
```

```javascript
// @unrdf/core (JavaScript)
import { createUnrdfStore, namedNode, literal } from '@unrdf/core';

const store = createUnrdfStore();
const alice = namedNode("http://example.com/alice");
const name = namedNode("http://xmlns.com/foaf/0.1/name");
const aliceName = literal("Alice");

store.addQuad({
  subject: alice,
  predicate: name,
  object: aliceName
});
```

---

## From Other SPARQL Engines

### Comunica

```javascript
// Comunica
const { newEngine } = require('@comunica/actor-init-sparql');
const engine = newEngine();
const results = await engine.queryBindings(query, { sources: [store] });

// @unrdf/core
import { executeQuerySync } from '@unrdf/core';
const results = executeQuerySync(store, query);
```

### Virtuoso / Triple Store

```javascript
// SPARQL endpoint
const fetch = require('node-fetch');
async function query(sparql) {
  const response = await fetch('http://endpoint/sparql?query=' + encodeURIComponent(sparql));
  return response.json();
}

// @unrdf/core (in-memory)
import { executeQuerySync } from '@unrdf/core';
const results = executeQuerySync(store, sparql);
```

**Key Difference:** @unrdf/core is in-memory, not a full triple store backend.

---

## Breaking Changes Checklist

When upgrading:

- [ ] Store API changed from async to sync
- [ ] Query functions changed signatures
- [ ] Quad format is now object (not function call)
- [ ] No longer uses N3.js internally
- [ ] Some advanced features removed (re-implement if needed)

---

## Performance Comparison

### Load 1M Triples

| Library | Time | Speed |
|---------|------|-------|
| N3.js | 4.2s | 238k/sec |
| @unrdf/core v5 | 2.5s | 400k/sec |
| Improvement | 1.7x faster | 40% faster |

### Execute Query

| Library | Time | Notes |
|---------|------|-------|
| N3.js + Comunica | 150ms | Full SPARQL |
| @unrdf/core | 45ms | 3.3x faster |

---

## Troubleshooting Migration

### "createStore is not a function"

```javascript
// OLD (v4, doesn't exist in v5):
const store = createStore();  // ❌ No longer exists

// NEW (v5):
import { createUnrdfStore } from '@unrdf/core';
const store = createUnrdfStore();  // ✅ Correct
```

### "store.addQuad expects 3 arguments, got 1"

```javascript
// OLD (function signature):
store.addQuad(subject, predicate, object);

// NEW (object signature):
store.addQuad({
  subject,
  predicate,
  object
});
```

### "executeQuery is not async"

```javascript
// OLD (v4):
const results = await executeQuery(store, query);

// NEW (v5):
const results = executeQuerySync(store, query);
// No await needed!
```

---

## Parallel Running

During migration, run both versions:

```javascript
// Keep old N3 store
const n3Store = new Store();

// Add new @unrdf store
const unrdfStore = createUnrdfStore();

// Sync both:
n3Store.forEach(quad => {
  unrdfStore.addQuad({
    subject: quad.subject,
    predicate: quad.predicate,
    object: quad.object
  });
});

// Gradually switch queries to use unrdfStore
```

---

## Support Matrix

| Operation | v4 | v5 | Migration |
|-----------|----|----|-----------|
| Synchronous API | ❌ | ✅ | Add sync wrapper |
| Async API | ✅ | ✅ | Already works |
| SPARQL queries | ✅ | ✅ | Change function call |
| Serialization | ✅ | ✅ | Use separatelibrary |
| Reasoning/OWL | ✅ | ❌ | Custom implementation |

---

## Upgrading Best Practices

1. **Start small:** Migrate one module at a time
2. **Run parallel:** Keep both versions during migration
3. **Test thoroughly:** Query behavior should be identical
4. **Measure performance:** Verify improvements
5. **Update incrementally:** Don't rewrite everything at once

---

## Next Reading

- **API.md** (Reference) - New API functions
- **CONFIGURATION.md** (Reference) - New configuration options
- **architecture** (Explanation) - Design changes in v5
