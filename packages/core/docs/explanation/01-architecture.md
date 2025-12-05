# Architecture: @unrdf/core

Understanding how @unrdf/core is designed.

---

## Layered Architecture

```
┌─────────────────────────────┐
│  Application Code (User)    │
├─────────────────────────────┤
│  Query API (executeQuerySync)│
├─────────────────────────────┤
│  SPARQL Engine              │
├─────────────────────────────┤
│  Store (Pattern Matching)   │
├─────────────────────────────┤
│  Term Representation        │
└─────────────────────────────┘
```

**Layer 1: Term Representation**
- NamedNode, Literal, BlankNode, Variable
- Immutable term objects
- Minimal data structure

**Layer 2: Store (RDF Graph)**
- Stores quads in memory
- Implements pattern matching (S-P-O-G)
- Indexed for fast queries
- Core data structure

**Layer 3: SPARQL Engine**
- Parses SPARQL queries
- Executes query patterns
- Handles JOINs, FILTERs, aggregates
- Returns bindings

**Layer 4: Query API**
- executeQuerySync, executeSelectSync, etc.
- Simple entry points
- Handles different result types

**Layer 5: Application Code**
- Your program using the store

---

## Core Design Principles

### 1. Synchronous by Default

```javascript
// Fast execution without async overhead
const results = executeQuerySync(store, query);
```

**Why:** JavaScript event loop simplicity, no promise overhead, predictable timing.

### 2. In-Memory Storage

```javascript
// All data in RAM
const store = createUnrdfStore();
```

**Why:** Eliminates I/O delays, fast queries, deterministic behavior. Trade-off: Limited by available RAM.

### 3. Simple Data Model

```javascript
// Quads are just objects
{
  subject: NamedNode,
  predicate: NamedNode,
  object: Term,
  graph?: NamedNode
}
```

**Why:** Easy to understand, no hidden complexity, maps directly to RDF spec.

### 4. No Magic

```javascript
// What you call is what executes
store.addQuad(quad);  // Directly adds
executeQuerySync(...) // Directly executes
```

**Why:** Predictable behavior, easy to debug, no hidden side effects.

---

## Why This Design?

### Trade-off: Speed vs. Completeness

**What @unrdf/core optimizes for:**
- ✅ Fast in-memory queries
- ✅ Simple synchronous API
- ✅ Small codebase
- ✅ Easy to understand

**What it doesn't provide:**
- ❌ Disk persistence
- ❌ Full SPARQL standard compliance
- ❌ Advanced reasoning/OWL
- ❌ Distributed querying

**Alternative:** For those features, use Virtuoso, GraphDB, or Jena.

### Performance Characteristics

| Operation | Time | Notes |
|-----------|------|-------|
| Add 1 quad | <0.1ms | O(1) with indexing |
| Query 1M store | 5-50ms | O(n) scan worst case |
| Type query | 1-5ms | O(1) with index |
| Complex join | 50-500ms | O(n²) worst case |

---

## Indexing Strategy

### Default: Predicate Index

```javascript
const store = createUnrdfStore({ indexing: 'predicate' });
```

**How it works:**
- Map: predicate URI → [quads with that predicate]
- Fast for: `?x rdf:type ?class` queries
- Benefits: 90% of queries use predicates

**Memory:** +10% overhead

### Alternative: Object Index

```javascript
const store = createUnrdfStore({ indexing: 'object' });
```

**How it works:**
- Map: object value → [quads with that object]
- Fast for: Finding all subjects with literal value
- Benefits: Value lookup queries

**Memory:** +20% overhead

---

## Query Execution Flow

```
Input: "SELECT ?name WHERE { ?x foaf:name ?name }"
   ↓
Parse SPARQL (check syntax)
   ↓
Extract patterns: [(?x, foaf:name, ?name)]
   ↓
Match pattern against store
   ↓
Build bindings: [{ name: "Alice" }, { name: "Bob" }]
   ↓
Output: [Binding, Binding, ...]
```

**Time breakdown for 1M quad store:**
- Parse: 1-2ms
- Match: 40-80ms (depends on selectivity)
- Build: 5-10ms
- **Total: 50-100ms for typical query**

---

## Why Synchronous?

**Async (traditional):**
```javascript
async function query() {
  const results = await executeQuery(store, sparql);
  // Wait for results...
}
```

**Synchronous (new):**
```javascript
function query() {
  const results = executeQuerySync(store, sparql);
  // Results immediately available
}
```

**Advantage:** No promise overhead, no event loop scheduling delays, simpler code.

**Disadvantage:** Blocks thread (but single operations are <100ms, acceptable).

---

## Memory Model

```javascript
// Quad storage
store.match() // Returns array (all in memory)

// Not:
store.match() // Returns generator/iterator (streaming)
```

**Why?**
- Simpler API (no async iteration)
- Easier to understand
- Works well for <10M quads
- No streaming complexity

**Trade-off:** All results loaded at once. Use LIMIT for large sets.

---

## Concurrency

```javascript
// ❌ NOT thread-safe
const store = createUnrdfStore();
// Can't use from multiple threads simultaneously
```

**Design:** Single-threaded. Use one store per thread.

**Pattern:**
```javascript
// Worker threads in Node.js
const { Worker } = require('worker_threads');
const worker = new Worker('./query-worker.js');
// Each worker has own store instance
```

---

## Extension Points

### Custom Query Types

```javascript
// @unrdf/core doesn't have plugins, but you can wrap:
function customQuery(store, type, data) {
  if (type === 'custom') {
    return handleCustom(store, data);
  }
  return executeQuerySync(store, data);
}
```

### Custom Indexing

```javascript
// Create your own index:
class CustomIndex {
  build(quads) {
    // Index by whatever logic you want
  }
}
```

---

## Comparison: @unrdf/core vs Alternatives

| Feature | @unrdf/core | N3.js | Comunica |
|---------|-------------|-------|----------|
| Sync API | ✅ | ❌ | ❌ |
| In-memory | ✅ | ✅ | ✅ |
| SPARQL | ✅ Basic | ❌ | ✅ Full |
| Speed | Fast | Medium | Slower |
| Complexity | Low | Medium | High |
| Size | Small | Medium | Large |

**Choose @unrdf/core if:** You need fast in-memory SPARQL with simple code.

---

## Future Directions

Potential extensions (not in core):
- Persistent storage (@unrdf/storage)
- More SPARQL features (@unrdf/sparql-extended)
- RDF validation (@unrdf/validation)
- Distributed queries (@unrdf/federation)

Core stays minimal: Add features via separate packages.

---

## Key Takeaway

**@unrdf/core = Fast, simple, in-memory RDF + SPARQL**

Not a replacement for production triple stores, but perfect for:
- Development and testing
- Small to medium graphs
- Embedded RDF
- Educational use

---

## Next Reading

- **design-decisions** (Explanation) - Why specific choices were made
- **query-execution** (Explanation) - How queries actually run
- **architecture** (Reference) - More technical details
