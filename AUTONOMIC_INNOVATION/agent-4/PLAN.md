# Agent 4: Impact Set Extraction

**Role**: Implement "Diff as Program: Impact Sets" - extract what a capsule touches in the graph.

## Overview

Impact sets provide a comprehensive analysis of what resources, properties, and graphs are affected by a capsule (diff). This enables:
- **Change impact analysis**: Understand the scope of a modification
- **Conflict detection**: Identify overlapping impact sets (Agent 5 dependency)
- **Audit trails**: Track what parts of the graph were modified
- **Performance optimization**: Determine which caches to invalidate

## Files to Create

### Core Implementation
1. **`./src/impact.mjs`** - Impact set computation engine
   - `computeImpactSet(capsule)` - Main computation function
   - `mergeImpactSets(impacts)` - Combine multiple impact sets
   - `impactSetIntersection(a, b)` - Find overlapping impacts

2. **`./src/format.mjs`** - Impact summary formatting & serialization
   - `formatImpactSummary(impact)` - Human-readable text output
   - `impactSetToJSON(impact)` - Deterministic JSON serialization
   - `impactSetFromJSON(json)` - Deserialization with Set restoration

3. **`./src/index.mjs`** - Public API exports
   - Re-export all public functions
   - Type definitions (JSDoc)

### Testing
4. **`./test/impact.test.mjs`** - Core impact computation tests
   - Simple capsule with add/delete operations
   - Multiple quads across different subjects/predicates/graphs
   - Empty capsule (no changes)
   - Deterministic output verification

5. **`./test/format.test.mjs`** - Formatting and serialization tests
   - JSON round-trip (serialize → deserialize → compare)
   - Deterministic ordering (run twice, same output)
   - Human-readable summary format

## Data Structures

### Input: Capsule Structure
```javascript
/**
 * @typedef {Object} Capsule
 * @property {Object} delta - The change set
 * @property {Array<Quad>} delta.add - Quads to add
 * @property {Array<Quad>} delta.del - Quads to delete
 * @property {string} [id] - Optional capsule identifier
 * @property {Object} [metadata] - Optional metadata
 */
```

### Output: ImpactSet Structure
```javascript
/**
 * @typedef {Object} ImpactSet
 * @property {Set<string>} subjects - All touched subject IRIs
 * @property {Set<string>} predicates - All touched predicate IRIs
 * @property {Set<string>} graphs - All touched graph IRIs
 * @property {Object} cardinality - Quad counts
 * @property {number} cardinality.quadsAdded - Number of quads added
 * @property {number} cardinality.quadsDeleted - Number of quads deleted
 * @property {number} cardinality.netsChange - Net change (added - deleted)
 * @property {Object} resourcesAffected - Resource-level metrics
 * @property {number} resourcesAffected.directAffected - Unique subjects touched
 * @property {number} [resourcesAffected.transitive] - (Future) Transitive closure count
 */
```

### Serialized Form (JSON)
```javascript
/**
 * @typedef {Object} ImpactSetJSON
 * @property {Array<string>} subjects - Sorted array of subject IRIs
 * @property {Array<string>} predicates - Sorted array of predicate IRIs
 * @property {Array<string>} graphs - Sorted array of graph IRIs
 * @property {Object} cardinality - Same as ImpactSet
 * @property {Object} resourcesAffected - Same as ImpactSet
 */
```

## Algorithms

### 1. Impact Set Computation

```javascript
/**
 * Compute impact set from capsule
 *
 * Algorithm:
 * 1. Initialize empty Sets for subjects, predicates, graphs
 * 2. Walk delta.add:
 *    - Extract subject.value, predicate.value, graph.value
 *    - Add to respective Sets
 *    - Increment quadsAdded
 * 3. Walk delta.del:
 *    - Extract subject.value, predicate.value, graph.value
 *    - Add to respective Sets
 *    - Increment quadsDeleted
 * 4. Compute derived metrics:
 *    - netsChange = quadsAdded - quadsDeleted
 *    - directAffected = subjects.size
 * 5. Return ImpactSet object
 *
 * Complexity: O(n) where n = |delta.add| + |delta.del|
 * Space: O(k) where k = unique IRIs
 */
function computeImpactSet(capsule) {
  // Implementation in src/impact.mjs
}
```

### 2. Set Merging

```javascript
/**
 * Merge multiple impact sets (e.g., for batch operations)
 *
 * Algorithm:
 * 1. Create union of all subjects, predicates, graphs
 * 2. Sum cardinality metrics
 * 3. Recompute resourcesAffected from merged subjects
 *
 * Complexity: O(m * k) where m = number of impact sets, k = avg size
 */
function mergeImpactSets(impacts) {
  // Implementation in src/impact.mjs
}
```

### 3. Set Intersection

```javascript
/**
 * Find overlapping impacts (for conflict detection)
 *
 * Algorithm:
 * 1. Compute intersection of subjects, predicates, graphs
 * 2. Return new ImpactSet with intersected Sets
 * 3. Cardinality is undefined (intersection != addition)
 *
 * Use case: Agent 5 (Commutativity) checks if two capsules
 *           have non-empty intersection → potential conflict
 */
function impactSetIntersection(a, b) {
  // Implementation in src/impact.mjs
}
```

### 4. Deterministic Serialization

```javascript
/**
 * Convert ImpactSet to deterministic JSON
 *
 * Algorithm:
 * 1. Convert Sets to Arrays
 * 2. Sort arrays lexicographically
 * 3. Return plain object (serializable)
 *
 * CRITICAL: Must be deterministic for hashing/receipts
 * Two identical impact sets MUST produce identical JSON
 */
function impactSetToJSON(impact) {
  return {
    subjects: Array.from(impact.subjects).sort(),
    predicates: Array.from(impact.predicates).sort(),
    graphs: Array.from(impact.graphs).sort(),
    cardinality: impact.cardinality,
    resourcesAffected: impact.resourcesAffected,
  };
}
```

### 5. Human-Readable Formatting

```javascript
/**
 * Format impact set as human-readable summary
 *
 * Output format:
 * ```
 * Impact Set Summary
 * ==================
 * Resources Affected: 12 subjects
 * Properties Used: 5 predicates
 * Graphs Modified: 2 named graphs
 *
 * Changes:
 *   + 15 quads added
 *   - 3 quads deleted
 *   Δ 12 net change
 *
 * Subjects:
 *   - http://example.org/Person/1
 *   - http://example.org/Person/2
 *   ...
 * ```
 */
function formatImpactSummary(impact) {
  // Implementation in src/format.mjs
}
```

## Implementation Details

### Dependencies

```javascript
// src/impact.mjs
import { dataFactory } from '@unrdf/oxigraph';
import { z } from 'zod';

// Validation schema for capsule input
const CapsuleSchema = z.object({
  delta: z.object({
    add: z.array(z.any()),  // Quad objects
    del: z.array(z.any()),  // Quad objects
  }),
  id: z.string().optional(),
  metadata: z.any().optional(),
});
```

### Error Handling

```javascript
/**
 * Validate capsule structure before processing
 * @throws {Error} If capsule is malformed
 */
function validateCapsule(capsule) {
  try {
    CapsuleSchema.parse(capsule);
  } catch (error) {
    throw new Error(`Invalid capsule: ${error.message}`);
  }
}
```

### Quad Term Extraction

```javascript
/**
 * Extract IRI value from RDF term
 * @param {Object} term - RDF term (NamedNode, BlankNode, Literal)
 * @returns {string} - Term value/IRI
 */
function extractTermValue(term) {
  // Handle different term types
  if (term.termType === 'NamedNode') {
    return term.value;
  }
  if (term.termType === 'BlankNode') {
    return `_:${term.value}`;
  }
  if (term.termType === 'Literal') {
    return `"${term.value}"`;
  }
  return term.value || String(term);
}
```

### Default Graph Handling

```javascript
/**
 * Some quads may not have explicit graph property
 * Default to standard RDF default graph IRI
 */
const DEFAULT_GRAPH = 'http://kgc.io/Universe'; // Or use standard RDF default graph
```

## Test Specifications

### Test 1: Simple Capsule
```javascript
it('should compute impact set for simple add operation', () => {
  const capsule = {
    delta: {
      add: [
        {
          subject: { termType: 'NamedNode', value: 'http://ex.org/s1' },
          predicate: { termType: 'NamedNode', value: 'http://ex.org/p1' },
          object: { termType: 'Literal', value: 'value1' },
          graph: { termType: 'NamedNode', value: 'http://ex.org/g1' },
        },
      ],
      del: [],
    },
  };

  const impact = computeImpactSet(capsule);

  expect(impact.subjects.size).toBe(1);
  expect(impact.subjects.has('http://ex.org/s1')).toBe(true);
  expect(impact.predicates.has('http://ex.org/p1')).toBe(true);
  expect(impact.graphs.has('http://ex.org/g1')).toBe(true);
  expect(impact.cardinality.quadsAdded).toBe(1);
  expect(impact.cardinality.quadsDeleted).toBe(0);
  expect(impact.cardinality.netsChange).toBe(1);
  expect(impact.resourcesAffected.directAffected).toBe(1);
});
```

### Test 2: Multiple Quads with Add/Delete
```javascript
it('should handle multiple quads across add and delete', () => {
  const capsule = {
    delta: {
      add: [
        quad('http://ex.org/s1', 'http://ex.org/p1', 'v1', 'http://ex.org/g1'),
        quad('http://ex.org/s2', 'http://ex.org/p2', 'v2', 'http://ex.org/g1'),
        quad('http://ex.org/s1', 'http://ex.org/p3', 'v3', 'http://ex.org/g2'),
      ],
      del: [
        quad('http://ex.org/s3', 'http://ex.org/p1', 'old', 'http://ex.org/g1'),
      ],
    },
  };

  const impact = computeImpactSet(capsule);

  expect(impact.subjects.size).toBe(3); // s1, s2, s3
  expect(impact.predicates.size).toBe(3); // p1, p2, p3
  expect(impact.graphs.size).toBe(2); // g1, g2
  expect(impact.cardinality.quadsAdded).toBe(3);
  expect(impact.cardinality.quadsDeleted).toBe(1);
  expect(impact.cardinality.netsChange).toBe(2);
  expect(impact.resourcesAffected.directAffected).toBe(3);
});
```

### Test 3: Deterministic Serialization
```javascript
it('should produce deterministic JSON output', () => {
  const capsule = createTestCapsule(); // Helper function

  const impact1 = computeImpactSet(capsule);
  const json1 = impactSetToJSON(impact1);

  const impact2 = computeImpactSet(capsule);
  const json2 = impactSetToJSON(impact2);

  // JSON serialization should be identical
  expect(JSON.stringify(json1)).toBe(JSON.stringify(json2));

  // Arrays should be sorted
  expect(json1.subjects).toEqual([...json1.subjects].sort());
  expect(json1.predicates).toEqual([...json1.predicates].sort());
  expect(json1.graphs).toEqual([...json1.graphs].sort());
});
```

### Test 4: Empty Capsule
```javascript
it('should handle empty capsule', () => {
  const capsule = {
    delta: { add: [], del: [] },
  };

  const impact = computeImpactSet(capsule);

  expect(impact.subjects.size).toBe(0);
  expect(impact.predicates.size).toBe(0);
  expect(impact.graphs.size).toBe(0);
  expect(impact.cardinality.quadsAdded).toBe(0);
  expect(impact.cardinality.quadsDeleted).toBe(0);
  expect(impact.cardinality.netsChange).toBe(0);
  expect(impact.resourcesAffected.directAffected).toBe(0);
});
```

### Test 5: JSON Round-Trip
```javascript
it('should correctly round-trip through JSON', () => {
  const capsule = createComplexCapsule();
  const original = computeImpactSet(capsule);
  const json = impactSetToJSON(original);
  const restored = impactSetFromJSON(json);

  // Restored should have Set objects
  expect(restored.subjects).toBeInstanceOf(Set);
  expect(restored.predicates).toBeInstanceOf(Set);
  expect(restored.graphs).toBeInstanceOf(Set);

  // Content should match
  expect(restored.subjects.size).toBe(original.subjects.size);
  expect([...restored.subjects].sort()).toEqual([...original.subjects].sort());
});
```

### Test 6: Impact Set Merging
```javascript
it('should merge multiple impact sets', () => {
  const impact1 = computeImpactSet(capsule1);
  const impact2 = computeImpactSet(capsule2);

  const merged = mergeImpactSets([impact1, impact2]);

  // Union of all subjects
  expect(merged.subjects.size).toBeGreaterThanOrEqual(
    Math.max(impact1.subjects.size, impact2.subjects.size)
  );

  // Cardinality is summed
  expect(merged.cardinality.quadsAdded).toBe(
    impact1.cardinality.quadsAdded + impact2.cardinality.quadsAdded
  );
});
```

### Test 7: Impact Set Intersection
```javascript
it('should compute intersection of impact sets', () => {
  const capsule1 = createCapsuleWithSubjects(['s1', 's2', 's3']);
  const capsule2 = createCapsuleWithSubjects(['s2', 's3', 's4']);

  const impact1 = computeImpactSet(capsule1);
  const impact2 = computeImpactSet(capsule2);
  const intersection = impactSetIntersection(impact1, impact2);

  // Only s2 and s3 are in common
  expect(intersection.subjects.size).toBe(2);
  expect(intersection.subjects.has('http://ex.org/s2')).toBe(true);
  expect(intersection.subjects.has('http://ex.org/s3')).toBe(true);
});
```

### Test 8: Human-Readable Formatting
```javascript
it('should format impact set as human-readable text', () => {
  const capsule = createTestCapsule();
  const impact = computeImpactSet(capsule);
  const summary = formatImpactSummary(impact);

  expect(summary).toContain('Impact Set Summary');
  expect(summary).toContain('Resources Affected:');
  expect(summary).toContain('quads added');
  expect(summary).toContain('quads deleted');
  expect(typeof summary).toBe('string');
});
```

## Public API (Exports)

```javascript
// AUTONOMIC_INNOVATION/agent-4/src/index.mjs

/**
 * Impact Set Extraction - Public API
 * @module @autonomic-innovation/impact
 */

export { computeImpactSet, mergeImpactSets, impactSetIntersection } from './impact.mjs';
export { formatImpactSummary, impactSetToJSON, impactSetFromJSON } from './format.mjs';

/**
 * @typedef {import('./impact.mjs').ImpactSet} ImpactSet
 * @typedef {import('./impact.mjs').Capsule} Capsule
 * @typedef {import('./format.mjs').ImpactSetJSON} ImpactSetJSON
 */
```

## Integration Points

### Agent 2 (Capsule Planning) → Agent 4
- Agent 2 creates capsules with `.delta.add` and `.delta.del`
- Agent 4 consumes capsules to compute impact sets

### Agent 4 → Agent 5 (Commutativity)
- Agent 5 uses `impactSetIntersection` to detect conflicts
- Non-empty intersection → potential non-commutativity

### Agent 4 → Agent 8 (Application)
- Agent 8 may use impact sets to determine which caches to invalidate

### Agent 4 → Agent 10 (Quality Gates)
- Quality gates may verify impact set determinism
- Run `computeImpactSet` twice, compare JSON serialization

## Performance Characteristics

| Operation | Time Complexity | Space Complexity |
|-----------|-----------------|------------------|
| `computeImpactSet` | O(n) | O(k) |
| `mergeImpactSets` | O(m * k) | O(k_total) |
| `impactSetIntersection` | O(min(k1, k2)) | O(k_intersection) |
| `impactSetToJSON` | O(k log k) | O(k) |

Where:
- n = total quads in capsule
- k = unique IRIs
- m = number of impact sets to merge
- k_total = total unique IRIs after merge

## Constraints & Guarantees

### Determinism
- **CRITICAL**: `impactSetToJSON` MUST produce identical output for identical inputs
- Sets are converted to sorted arrays
- Object property iteration order is stable (modern JS)
- No floating-point arithmetic (all integers)

### Validation
- All capsules validated with Zod before processing
- Invalid capsules throw descriptive errors
- Malformed quads are caught early

### Immutability
- Impact sets are immutable once created
- Merge/intersection operations return new objects
- Original capsules are never modified

## Success Criteria

- [ ] All 3 core files created (`impact.mjs`, `format.mjs`, `index.mjs`)
- [ ] All 2 test files created (`impact.test.mjs`, `format.test.mjs`)
- [ ] Minimum 8 tests implemented and passing
- [ ] Deterministic serialization verified (run twice, same output)
- [ ] JSDoc coverage 100% on public APIs
- [ ] Zod validation on all inputs
- [ ] No runtime errors on malformed capsules (throw descriptive errors)
- [ ] Exports compatible with Agent 1 orchestrator integration

## Future Enhancements (Out of Scope for Phase 1)

1. **Transitive Closure**: Compute `resourcesAffected.transitive` via property paths
2. **Impact Visualization**: Generate graph visualizations of affected resources
3. **Impact Diffing**: Compare two impact sets and show what changed
4. **Semantic Impact**: Use reasoning to determine semantic impact beyond syntactic
5. **Impact Metrics**: Add metrics like "centrality" of affected resources

## References

- Agent 1 PLAN.md - Integration requirements
- Agent 2 PLAN.md (TBD) - Capsule structure definition
- Agent 5 PLAN.md (TBD) - Commutativity detection requirements
- Delta Sync Reducer - `/packages/kgc-4d/src/core/patterns/delta-sync-reducer.mjs`
- KGC-4D Package - Time-domain event sourcing patterns

---

**Status**: Plan Complete, Ready for Implementation
**Next Step**: Implement `src/impact.mjs`, `src/format.mjs`, `src/index.mjs`
**Estimated LoC**: ~400 (200 implementation + 200 tests)
