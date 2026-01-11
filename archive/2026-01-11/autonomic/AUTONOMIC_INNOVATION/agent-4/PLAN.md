# Agent 4: Impact Set Computation

## Mission
Compute the **Impact Set** of a capsule delta — which resources, predicates, and graphs are touched.

## Core Concepts

### Impact Set
When a capsule is applied, it modifies RDF quads. The **impact set** is the set of all:
- **Subjects** (resource IRIs) that are modified
- **Predicates** (property IRIs) that are used
- **Graphs** (named graph IRIs) where changes occur

### Cardinality Analysis
For each dimension (subjects, predicates, graphs), compute:
- **Unique count**: How many distinct entities touched
- **Total quads**: How many quad operations in the delta
- **Density**: Quads per subject (how concentrated changes are)

### Use Cases
1. **Audit Trail**: "This capsule modified 5 resources using 3 predicates"
2. **Conflict Detection**: Two capsules with overlapping impact sets may conflict
3. **Performance Estimation**: Large impact sets = more validation work
4. **Diff Summarization**: Human-readable summary of what changed

## Implementation

### 1. `impact-set.mjs`
**Primary interface for impact set computation.**

- `computeImpactSet(capsule)` → `ImpactSet`
  - Input: Capsule object with `delta` field (array of quads)
  - Output: ImpactSet object with subjects, predicates, graphs Sets, cardinality metrics
  - Algorithm:
    1. Initialize empty Sets for subjects, predicates, graphs
    2. Iterate through all quads in capsule.delta
    3. Add quad.subject, quad.predicate, quad.graph to respective Sets
    4. Compute cardinality metrics
    5. Return structured ImpactSet object

- `summarizeImpactSet(impactSet)` → `string`
  - Human-readable: "Modified 5 subjects, 3 predicates, 2 graphs. Total 12 quads."

- `hashImpactSet(impactSet)` → `string`
  - Deterministic SHA-256 hash of impact set
  - Used for mismatch reporting, caching, versioning
  - Sort all Sets before hashing for determinism

### 2. `cardinality.mjs`
**Detailed cardinality and structural analysis.**

- `groupByPredicate(capsule)` → `Map<predicate, count>`
  - For each predicate, count how many quads use it
  - Useful for understanding "what kind of changes" (e.g., 80% `rdf:type` assertions)

- `groupBySubject(capsule)` → `Map<subject, count>`
  - For each subject, count how many modifications
  - Identifies "hotspots" — resources with many changes

- `analyzeGraphStructure(capsule)` → `{ density, spanCount, orphanedSubjects }`
  - **Density**: totalQuads / uniqueSubjects (avg modifications per resource)
  - **Span count**: Number of distinct graphs touched
  - **Orphaned subjects**: Subjects with only object references (no predicates where subject is modified)

### 3. `index.mjs`
**Public API exports.**

```javascript
export { computeImpactSet, summarizeImpactSet, hashImpactSet } from './impact-set.mjs'
export { groupByPredicate, groupBySubject, analyzeGraphStructure } from './cardinality.mjs'
```

### 4. `test.mjs`
**Comprehensive test suite.**

Tests:
1. Impact set computation (10 quads → correct cardinality)
2. Deterministic hashing (same capsule → same hash, 100x)
3. Predicate grouping (3 predicates → correct grouping)
4. Empty capsule (0 quads → empty impact set)
5. Large capsule (1000 quads → computed in <100ms)

## Output Format

```javascript
{
  subjects: Set(['http://example.org/resource1', 'http://example.org/resource2']),
  predicates: Set(['http://example.org/property1', 'http://example.org/property2']),
  graphs: Set(['http://example.org/graph1']),
  cardinality: {
    subjects: 2,
    predicates: 2,
    graphs: 1,
    totalQuads: 10
  },
  summary: "Modified 2 subjects, 2 predicates, 1 graph. Total 10 quads."
}
```

## Determinism Guarantee

**Critical**: Impact sets MUST be deterministic.
- Same capsule → same impact set → same hash
- Hash computation sorts all Sets before serialization
- No reliance on iteration order, timestamps, randomness

## Performance

- **Target**: <10ms for typical capsules (10-100 quads)
- **Acceptable**: <100ms for large capsules (1000+ quads)
- **Algorithm**: O(n) where n = quad count (single pass, Set operations)

## Integration

Impact sets are **passive observers** — they compute metadata about capsules but do NOT modify them.

Used by:
- **Agent 5**: Conflict detection (compare impact sets)
- **Agent 6**: Audit logging (record impact sets)
- **Agent 7**: Performance monitoring (impact set size → validation cost)

## Evidence-Based Validation

Before declaring complete:
- ✅ All tests pass (`timeout 5s node test.mjs`)
- ✅ Determinism verified (100 runs → same hash)
- ✅ Large capsule performance (<100ms for 1000 quads)
- ✅ No linting errors
- ✅ Type hints 100% coverage (JSDoc)

---

**Next**: Implement per specification, test exhaustively, measure performance.
