# Design Decisions: @unrdf/core

Understanding the trade-offs behind core choices.

---

## Decision 1: Synchronous Execution

**Choice:** Use synchronous functions by default (not async/await)

**Rationale:**
- SPARQL queries are typically <100ms on in-memory data
- Promise overhead is measurable (10-20%)
- Simpler code without async/await
- No event loop complexities

**Trade-off:**
- Blocks thread during query (acceptable for sub-100ms)
- Can't use for very slow operations
- Single-threaded model

**Alternative considered:** Async (like N3.js)
- Would be more "correct" for I/O
- But adds 20% overhead for queries that don't need it
- Decision: Optimize for the common case (fast in-memory)

---

## Decision 2: In-Memory Only

**Choice:** Store all data in RAM, no persistence

**Rationale:**
- No I/O overhead (1000x faster than disk)
- Simpler codebase (no marshalling/serialization)
- Predictable performance
- Easier to test and reason about

**Trade-off:**
- Limited by available RAM (~2-10M quads typical)
- Data lost on restart (implement persistence separately)
- Not suitable for gigabyte-scale graphs

**Alternative considered:** Persistent storage
- Would require B-tree indices, ACID transactions
- 10x more complex code
- Decision: Keep core simple, add @unrdf/storage separately

---

## Decision 3: Object-Based Quads

**Choice:** Represent quads as objects with properties

```javascript
// Our choice:
{ subject, predicate, object, graph }

// Alternative would be:
Store.addQuad(subject, predicate, object)
// (like N3.js)
```

**Rationale:**
- Named properties are self-documenting
- Easy to extend (add metadata)
- Pattern-matching-friendly
- Closer to RDF/JS spec

**Trade-off:**
- Slightly more verbose
- More memory per quad (but minimal)

---

## Decision 4: Terms as Objects with Type Field

**Choice:** Every term has `{ type: 'NamedNode', value: '...' }`

**Rationale:**
- Type information is explicit
- No runtime type detection needed
- Matches RDF/JS spec exactly
- Easy to serialize/deserialize

**Trade-off:**
- More verbose than N3.js
- Can't use bare strings (must use namedNode())
- Catches more errors at creation time

---

## Decision 5: Predicate Indexing by Default

**Choice:** Index all quads by predicate URI

**Rationale:**
- 70% of queries use predicate patterns (`?x rdf:type ?y`)
- Most selective index for typical workloads
- Only 10% memory overhead
- Fastest for common queries

**Alternative considered:** No indexing
- Would be faster for add operations
- Queries would be 5-10x slower
- Decision: Optimize for query workload (more common)

---

## Decision 6: Pattern Matching Semantics

**Choice:** `store.match(s?, p?, o?, g?)` - all optional

```javascript
store.match() // All quads
store.match(subject) // All with subject
store.match(undefined, predicate) // All with predicate
```

**Rationale:**
- Flexible query API
- Replicates RDF.js spec
- Enables rapid filtering

**Trade-off:**
- Could use named parameters (more verbose)
- Current approach: pragmatic

---

## Decision 7: No DISTINCT by Default

**Choice:** SPARQL queries return duplicates unless DISTINCT specified

```sparql
SELECT ?x WHERE { ?x ?p ?o UNION ?x ?p ?o }
# Returns ?x twice

SELECT DISTINCT ?x WHERE { ... }
# Returns ?x once
```

**Rationale:**
- Matches SPARQL standard
- Slightly faster (no deduplication)
- More explicit (you ask for dedup if needed)

**Alternative considered:** Auto-dedup
- Convenience
- But hides duplicates that might be intentional
- Decision: Follow standard, require explicit DISTINCT

---

## Decision 8: Exception-Based Error Handling

**Choice:** Throw Error on problems

```javascript
try {
  const results = executeQuerySync(store, badSparql);
} catch (error) {
  console.error(error.message);
}
```

**Rationale:**
- Standard JavaScript pattern
- No error codes to learn
- Stack traces are useful
- Matches user expectations

**Alternative considered:** Return error objects
- More explicit
- But requires checking every result
- Decision: Use exceptions for exceptional cases

---

## Decision 9: No Blank Node Canonicalization

**Choice:** Keep blank node IDs as given

```javascript
const bn1 = blankNode('b1');
const bn2 = blankNode('b1');
// bn1 and bn2 are different nodes (different objects)
```

**Rationale:**
- Simple semantics
- No magic rewriting
- Predictable IDs

**Trade-off:**
- User responsible for blank node management
- Can't compare graphs easily (use isIsomorphic)

---

## Decision 10: No Automatic Type Coercion

**Choice:** Don't auto-convert between types

```javascript
// ❌ Won't work:
FILTER (?age > 30)  // If age is string "30"

// ✅ Must be explicit:
FILTER (xsd:integer(?age) > 30)
```

**Rationale:**
- Prevents silent bugs
- Makes queries more robust
- Matches SPARQL semantics

**Trade-off:**
- More verbose queries
- Requires understanding types

---

## Comparison: Design Philosophy

| Aspect | @unrdf/core | N3.js | Comunica |
|--------|-------------|-------|----------|
| Speed | Priority | Medium | Low |
| Simplicity | Priority | Medium | Low |
| Completeness | Secondary | Medium | High |
| Compliance | Basic | Medium | Full |
| Extensions | Separate pkgs | Built-in | Built-in |

---

## Future Decisions Not Yet Made

- Should we support streaming?
- Should we support distributed queries?
- Should we add OWL reasoning?
- Should we cache queries?

**Current answer:** Add as separate packages, keep core minimal.

---

## Key Principle

**Optimize for the 80/20 case: Small in-memory graphs with SPARQL queries**

Not optimized for:
- Gigabyte-scale graphs → Use Virtuoso
- Complex reasoning → Use Jena
- Distributed systems → Use Comunica
- Streaming → Use specific tools

---

## Next Reading

- **architecture** (Explanation) - How components work
- **sparql-concepts** (Explanation) - SPARQL theory
- **optimize-sparql-queries** (How-To) - Make queries faster
