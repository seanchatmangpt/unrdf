# Agent 2: Gaps, Observations, and Follow-Up Questions

---

## Critical Gaps

### 1. Turtle Serialization Blocked by Format Constraints

**File**: `/home/user/unrdf/packages/oxigraph/src/store.mjs` (line 188-198)

**Issue**: `dump({ format: 'text/turtle' })` fails with:

```
Dump operation failed: A RDF format supporting datasets was expected, Turtle found
```

**Root Cause**: Oxigraph distinguishes between:

- **Quad formats** (support named graphs): N-Quads, N-Triples, TriG
- **Triple formats** (no graph info): Turtle, RDF/XML, RDF/JSON

**Impact**: Cannot round-trip data with graph information through Turtle serialization.

**Recommended Fix**:

- Implement TriG format support (Turtle + graphs)
- Or provide `store.dumpTriples()` for triple-only serialization

---

### 2. SPARQL COUNT Returns Wrong Quad Count

**File**: `/home/user/unrdf/packages/oxigraph/src/store.mjs` (line 132-142)

**Issue**: Query `SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }` returns 1 instead of 5.

**Evidence**:

```
Count query result: 1
Actual quads (via store.match()): 5
```

**Root Cause**: Likely SPARQL aggregation semantics or default graph filtering.

**Investigation Needed**:

- Does query default to unnamed graph only?
- Does COUNT aggregation collapse all results to one?
- What's the correct SPARQL to count quads across all graphs?

**Current Workaround**: Use `store.match()` and check `.length`

---

### 3. Format Support Matrix Incomplete

**File**: `/home/user/unrdf/packages/oxigraph/src/store.mjs` (line 168-181, 188-198)

**Tested Formats**:

- ✅ text/turtle (load)
- ✅ application/n-quads (load + dump)
- ❌ text/turtle (dump)
- ❌ application/rdf+xml (dump)
- ❓ application/ld+json (not tested)
- ❓ application/n-triples (not tested)
- ❓ text/trig (not tested)

**Recommendation**: Document format matrix in API reference

---

## Observations

### 1. Canonicalization Works Perfectly

The lexicographic sort by (subject, predicate, object, graph) produces:

- ✅ Deterministic ordering
- ✅ Stable hashes
- ✅ Round-trip consistency

**Proof**: Original hash == Round-trip hash after reload

**Insight**: This is suitable for:

- Deduplication
- Integrity checks
- Data fingerprinting
- SPARQL result normalization

---

### 2. Oxigraph Quad Objects Are Not Plain JavaScript Objects

**Discovery**: Creating quads as plain objects fails:

```javascript
// WRONG - causes "Reflect.get called on non-object" error
const quad = {
  subject: { value: 'http://example.org/alice', termType: 'NamedNode' },
  predicate: { value: 'http://example.org/knows', termType: 'NamedNode' },
  object: { value: 'http://example.org/bob', termType: 'NamedNode' },
  graph: { value: 'http://example.org/graph1', termType: 'NamedNode' },
};
store.add(quad); // ❌ Fails with Reflect.get error
```

**Correct Approach**: Use `dataFactory`:

```javascript
const quad = dataFactory.quad(
  dataFactory.namedNode('http://example.org/alice'),
  dataFactory.namedNode('http://example.org/knows'),
  dataFactory.namedNode('http://example.org/bob'),
  dataFactory.namedNode('http://example.org/graph1')
);
store.add(quad); // ✅ Works
```

**Insight**: Oxigraph quads are WASM objects, not plain JS objects. Need to use factory methods.

---

### 3. N-Quads is the Safe Serialization Format

**Evidence**:

- ✅ Loads successfully from N-Quads text
- ✅ Dumps to N-Quads with all quads intact
- ✅ Preserves named graphs
- ✅ Deterministic output (suitable for hashing)
- ✅ Perfect round-trip preservation

**Recommendation**: For canonical storage and interchange, use N-Quads.

---

## Open Questions for Investigation

### Q1: Can we use SPARQL to count quads across named graphs?

**Why**: SPARQL COUNT seems to return 1 instead of 5. Is there a SPARQL pattern that counts correctly?

**To Test**:

```sparql
SELECT (COUNT(?g) as ?graphCount) WHERE { ?s ?p ?o ?g }
SELECT (COUNT(DISTINCT ?s) as ?subjectCount) WHERE { ?s ?p ?o }
SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o ?g }
```

**File to Update**: `/home/user/unrdf/packages/oxigraph/src/store.mjs` (countQuads function)

---

### Q2: Does oxigraph support SPARQL UPDATE?

**Why**: Canonicalization might be doable with SPARQL CONSTRUCT + reinsert

**To Test**:

```javascript
const updateQuery = `
CONSTRUCT {
  ?s ?p ?o ?g
} WHERE {
  ?s ?p ?o ?g
}
ORDER BY ?s ?p ?o ?g
`;
store.update(updateQuery); // Does this work?
```

**File to Check**: `/home/user/unrdf/packages/oxigraph/src/store.mjs` (update method)

---

### Q3: What's the performance impact of canonicalization?

**Why**: Need to understand scalability for production use

**To Test**:

- Load 1K quads
- Canonicalize and hash (time each step)
- Load 10K quads (same test)
- Report memory usage and timing

**Current Status**: Only tested with 5 quads

---

### Q4: How to serialize quads to Turtle while preserving graph context?

**Why**: Some users may want Turtle output with graph metadata

**Options**:

1. TriG format (Turtle + graphs)
2. Turtle + separate metadata file
3. Turtle with reification (graph as subject)
4. Use N-Quads for interchange, Turtle for display

---

### Q5: Are blank nodes handled correctly in canonicalization?

**Why**: Blank node IDs are typically not deterministic

**Current Approach**: Only tested with named nodes and literals

**To Test**:

- Create quads with `dataFactory.blankNode('b1')`
- Load and canonicalize
- Check if blank node IDs are stable

---

## Architectural Insights

### Store Architecture

```
User Code
    ↓
createStore() [oxigraph/src/index.mjs]
    ↓
OxigraphStore Wrapper [oxigraph/src/store.mjs]
    ↓
oxigraph.Store (WASM) [node_modules/.pnpm/oxigraph]
    ↓
RocksDB backend (persistent, optional)
```

### Data Flow for Canonicalization

```
Input Quads (any order)
    ↓ store.match() → Array
    ↓ canonicalizeQuads() → sorted Array
    ↓ serializeToCanonical() → N-Quads text
    ↓ hashCanonical() → SHA256
    ↓ store.load() → re-ingest
    ↓ Verify hash matches
```

---

## Recommendations for Next Agents

### Agent 3 (Suggested): SHACL Validation

- Validate RDF against SHACL shapes
- Check @unrdf/validation package
- Test shape constraint enforcement

### Agent 4 (Suggested): Graph Querying

- Benchmark SPARQL performance
- Test pattern matching across graphs
- Measure query optimization

### Agent 5 (Suggested): Streaming & Change Detection

- Explore @unrdf/streaming capabilities
- Test change feeds and subscriptions
- Investigate real-time sync protocols

---

## Code Snippets for Future Reference

### Canonical Serialization

```javascript
const { createStore, dataFactory } = await import('@unrdf/oxigraph');
const store = createStore();

// ... add quads ...

const allQuads = store.match();
const sorted = allQuads.sort((a, b) => String(a.subject).localeCompare(String(b.subject)));
const canonical = sorted
  .map(
    q =>
      `${serializeTerm(q.subject)} ${serializeTerm(q.predicate)} ${serializeTerm(q.object)} ${serializeTerm(q.graph)} .`
  )
  .join('\n');

const hash = crypto.createHash('sha256').update(canonical).digest('hex');
```

### Safe Quad Creation

```javascript
function createQuad(subject, predicate, object, graph, dataFactory) {
  return dataFactory.quad(
    dataFactory.namedNode(subject),
    dataFactory.namedNode(predicate),
    typeof object === 'string' && object.startsWith('http')
      ? dataFactory.namedNode(object)
      : dataFactory.literal(object),
    dataFactory.namedNode(graph)
  );
}
```

---

## Files Modified/Created

- ✅ Created: `/home/user/unrdf/exploration/agents/agent-2/index.mjs` (456 lines)
- ✅ Created: `/home/user/unrdf/exploration/agents/agent-2/README.md`
- ✅ Created: `/home/user/unrdf/exploration/agents/agent-2/evidence.json`
- ✅ Created: `/home/user/unrdf/exploration/agents/agent-2/notes.md`

---

## Known Limitations

1. **No SHACL validation** - Not tested in this exploration
2. **No blank node canonicalization** - Only named nodes/literals tested
3. **No performance benchmarking** - Small dataset only (5 quads)
4. **No persistence testing** - In-memory store only
5. **No concurrent access** - Single-threaded test
6. **No error recovery** - No rollback or transaction tests

---

**Last Updated**: 2025-12-27
**Agent**: agent-2
**Status**: Complete - Ready for next exploration phase
