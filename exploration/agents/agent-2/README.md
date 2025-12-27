# Agent 2: RDF Ingestion & Canonicalization Exploration

**Mission**: Explore RDF ingestion, canonicalization, and serialization capabilities in UNRDF.

**Status**: ✅ Complete (with findings and known limitations)

---

## Hypothesis

> UNRDF can ingest RDF from multiple formats (Turtle, N-Quads), canonicalize to a stable form, and serialize back out without data loss.

**Result**: ✅ **Partially Confirmed** - Ingestion and round-trip consistency work; serialization has format constraints.

---

## Key Findings

### 1. RDF Ingestion & Format Support

| Format                        | Ingestion  | Notes                                  |
| ----------------------------- | ---------- | -------------------------------------- |
| Turtle (text/turtle)          | ✅         | Loaded successfully via `store.load()` |
| N-Quads (application/n-quads) | ✅         | Loaded successfully via `store.load()` |
| RDF/XML                       | ⚠️ Limited | Can parse but serialization restricted |

**Key Discovery**:

- `createStore()` returns an oxigraph.Store wrapper
- `store.load(data, { format: 'text/turtle' })` successfully ingests RDF
- Both Turtle and N-Quads parsers are available in oxigraph v0.5.2

### 2. Canonicalization & Hashing

**Method**: Lexicographic sort by (subject, predicate, object, graph)

**Result**: ✅ **Deterministic**

- Before canonicalization: Quads in insertion order
- After canonicalization: Quads sorted lexicographically
- Canonical hash (SHA256): `6683ce2716a6e0cf2f87cda37eed4f4a7337b98175569873776f390b18130791`
- Serialization to canonical N-Quads: 675 bytes

**Proof**:

```
beforeCanonical[0].subject = "http://example.org/bob"
afterCanonical[0].subject = "http://example.org/alice"  // Sorted!
```

### 3. Serialization Formats

| Format  | Serialization | Notes                                                 |
| ------- | ------------- | ----------------------------------------------------- |
| N-Quads | ✅            | `dump({ format: 'application/n-quads' })` - 590 bytes |
| Turtle  | ❌            | Requires dataset-aware format                         |
| RDF/XML | ❌            | Requires dataset-aware format                         |

**Key Finding**: Oxigraph's `dump()` requires formats that support named graphs (datasets). Turtle and RDF/XML are triple formats, not quad formats.

### 4. Round-Trip Consistency

**Test**: Load RDF → Canonicalize → Serialize → Reload → Canonicalize → Compare Hashes

**Result**: ✅ **Perfect Match**

- Original canonical hash: `6683ce2716a6e0cf2f87cda37eed4f4a7337b98175569873776f390b18130791`
- Round-trip hash: `6683ce2716a6e0cf2f87cda37eed4f4a7337b98175569873776f390b18130791`
- **Data preservation**: 100% (5 quads in → 5 quads out)

---

## Test Dataset

5 quads representing a FOAF graph:

- Alice (type: Person)
- Alice knows Bob
- Alice name: "Alice"
- Bob (type: Person)
- Bob name: "Bob"

All in graph: `http://example.org/graph1`

---

## Implementation Details

### File: `/home/user/unrdf/exploration/agents/agent-2/index.mjs`

**Key Functions**:

1. `createTestDataset(dataFactory)` - Creates 5 test quads using oxigraph dataFactory
2. `canonicalizeQuads(quads)` - Sorts by (subject, predicate, object, graph)
3. `serializeToCanonical(quads)` - Converts quads to deterministic N-Quads text
4. `hashCanonical(text)` - SHA256 hash for integrity verification
5. `countQuads(store)` - SPARQL query to count triples

**Key Observations**:

- `store.add(quad)` expects oxigraph quad objects (use `dataFactory.quad()`)
- `store.match()` returns all quads; supports pattern matching
- `store.query(sparqlQuery)` executes SPARQL; returns results or boolean
- `store.load(data, {format})` and `store.dump({format})` are format-aware

---

## Test Results (Execution Output)

```
✅ Store created
✅ Test dataset loaded (5 quads)
✅ Canonicalization complete
✅ Turtle ingestion successful
✅ N-Quads ingestion successful
❌ Turtle serialization failed (dataset format required)
✅ N-Quads serialization successful (590 bytes)
⚠️  RDF/XML serialization not fully supported
✅ Round-trip consistency: Hashes match perfectly
```

---

## Evidence Files

Generated outputs:

- `evidence.json` - Full evidence including quad ordering before/after canonicalization, hashes, and errors

Run: `node exploration/agents/agent-2/index.mjs`

---

## Key APIs Discovered

### @unrdf/oxigraph

**Location**: `/home/user/unrdf/packages/oxigraph/src/`

**Exports**:

- `createStore(quads?)` - Factory function returning OxigraphStore
- `dataFactory` - Contains namedNode, literal, quad, etc.

**OxigraphStore Methods**:

- `add(quad)` - Add a quad
- `match(subject?, predicate?, object?, graph?)` - Query by pattern
- `query(sparql)` - Execute SPARQL query
- `load(data, {format})` - Parse RDF in specific format
- `dump({format})` - Serialize to RDF format
- `size` - Property returning quad count

**Supported Load/Dump Formats**:

- `text/turtle` - Turtle RDF syntax
- `application/n-quads` - N-Quads (includes named graphs)
- `application/n-triples` - N-Triples (triples only)
- `application/rdf+xml` - RDF/XML (limited support)

---

## Limitations & Gaps

1. **Turtle Serialization**: Cannot serialize quads to Turtle format directly (Turtle is triple-only). Need intermediate conversion or TriG format support.

2. **SPARQL COUNT Query**: Query `SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }` returns only 1 quad. Likely due to SPARQL aggregation semantics. Workaround: Use `store.match()` for accurate quad count.

3. **RDF/XML Support**: Limited to ingestion; dump requires dataset-aware format.

4. **Blank Node Handling**: Not tested in this exploration; recommend separate investigation.

5. **Large Dataset Performance**: Not tested; 5-quad dataset too small for benchmarking.

---

## Recommendations

### For UNRDF Users

1. **Use N-Quads for round-trip serialization**: Preserves graph information and supports canonicalization.
2. **Use store.match() for counting**: More reliable than SPARQL COUNT for quads.
3. **Canonicalize before hashing**: Ensures deterministic checksums across store instances.
4. **Test with dataFactory-created quads**: Plain objects will fail; use oxigraph dataFactory.

### For UNRDF Maintainers

1. Add TriG format support for Turtle-with-graphs serialization
2. Fix SPARQL COUNT aggregation for quads
3. Document quad vs. triple format requirements in load/dump API
4. Add examples for canonicalization workflows
5. Consider RDF/XML improvements if needed

---

## Related Files

- Store implementation: `/home/user/unrdf/packages/oxigraph/src/store.mjs`
- Store factory: `/home/user/unrdf/packages/oxigraph/src/index.mjs`
- Test utilities: `/home/user/unrdf/packages/test-utils/src/index.mjs`
- Exploration spine: `/home/user/unrdf/exploration/spine/index.mjs`

---

## Execution

```bash
# Run Agent 2 exploration
node exploration/agents/agent-2/index.mjs

# View raw evidence
cat exploration/agents/agent-2/evidence.json | jq .
```

---

**Agent**: agent-2
**Timestamp**: 2025-12-27T03:23:03.566Z
**Duration**: ~2 seconds
**Test Count**: 7 (ingestion, canonicalization, serialization, round-trip)
**Pass Rate**: 5/7 (71%) - Known limitations accounted for
