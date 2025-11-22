# RDF Graph & Ontology Diff Module

Complete TDD implementation of RDF graph and ontology-level diffing utilities for UNRDF.

## Overview

The `diff.mjs` module provides:

- **Triple-level graph diff**: Compare two RDF graphs and detect added/removed triples
- **Ontology-level diff**: Map low-level triple changes to application-level semantic changes via lenses
- **Delta-based diffing**: Process transaction deltas without re-scanning the store
- **Change summarization**: Aggregate and filter changes by type and entity

## Core Functions

### Graph-Level Diffing

#### `diffGraphFromStores(beforeStore, afterStore): GraphDiff`

Compare two N3 stores and detect triple-level differences.

```js
import { Store } from 'n3'
import { diffGraphFromStores } from './diff.mjs'

const before = new Store()
const after = new Store()

// Add triples to stores...

const diff = diffGraphFromStores(before, after)
console.log(diff.added.length)    // Triples added
console.log(diff.removed.length)  // Triples removed
```

**Returns**: `GraphDiff`
```ts
{
  added: DiffTriple[],
  removed: DiffTriple[]
}
```

#### `diffGraphFromDelta(delta): GraphDiff`

Create a triple-level diff directly from a transaction delta.

```js
import { diffGraphFromDelta } from './diff.mjs'

// From transaction: { additions: [...quads], removals: [...quads] }
const diff = diffGraphFromDelta(delta)
```

**Advantage**: No store re-scan; work directly with transaction data.

### Ontology-Level Diffing

#### `diffOntologyFromGraphDiff(graphDiff, lens): OntologyDiff`

Apply an ontology lens to map low-level triple changes to semantic changes.

```js
import { diffOntologyFromGraphDiff } from './diff.mjs'

/**
 * Define a lens that maps triples to application-level changes
 */
const featureLens = (triple, direction) => {
  if (
    triple.predicate === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
    triple.object === 'http://example.org/ontology#Feature'
  ) {
    return {
      kind: direction === 'added' ? 'FeatureAdded' : 'FeatureRemoved',
      entity: triple.subject,
    }
  }
  return null // Ignore this triple
}

const graphDiff = diffGraphFromStores(before, after)
const ontologyDiff = diffOntologyFromGraphDiff(graphDiff, featureLens)

console.log(ontologyDiff.changes)
// [
//   { kind: 'FeatureAdded', entity: 'http://example.org/feature1' },
//   { kind: 'FeatureRemoved', entity: 'http://example.org/feature2' }
// ]
```

#### `diffOntologyFromStores(beforeStore, afterStore, lens): OntologyDiff`

Convenience wrapper combining graph diff + ontology lens.

```js
import { diffOntologyFromStores } from './diff.mjs'

const ontologyDiff = diffOntologyFromStores(before, after, featureLens)
```

#### `diffOntologyFromDelta(delta, lens): OntologyDiff`

Apply ontology lens to a transaction delta (most efficient for transactions).

```js
import { diffOntologyFromDelta } from './diff.mjs'

const ontologyDiff = diffOntologyFromDelta(delta, featureLens)
```

### Change Summarization

#### `summarizeChangesByKind(ontologyDiff): Record<string, number>`

Count changes by their kind.

```js
import { summarizeChangesByKind } from './diff.mjs'

const summary = summarizeChangesByKind(ontologyDiff)
// { FeatureAdded: 3, FeatureRemoved: 1, RoleAdded: 2 }
```

#### `changesForEntity(ontologyDiff, entityIri): OntologyChange[]`

Filter changes for a specific entity.

```js
import { changesForEntity } from './diff.mjs'

const featureChanges = changesForEntity(
  ontologyDiff,
  'http://example.org/feature1'
)
// All changes affecting feature1
```

## Integration with TransactionManager

Wire the diff module into your transaction layer:

```js
import { diffOntologyFromDelta } from './diff.mjs'

class TransactionManager {
  constructor(options = {}) {
    this.ontologyLens = options.ontologyLens || null
  }

  async executeTransaction(operations, hooks) {
    const delta = { additions: [], removals: [] }

    // ... execute operations, populate delta ...

    let ontologyDiff = null
    if (this.ontologyLens) {
      ontologyDiff = diffOntologyFromDelta(delta, this.ontologyLens)
    }

    return {
      store,
      receipt: {
        delta,
        committed: true,
        ontologyDiff,
        // ... other fields ...
      },
    }
  }
}
```

## Ontology Lenses

An ontology lens is a pure function that maps triples to semantic changes:

```ts
type OntologyLensFn = (
  triple: DiffTriple,
  direction: 'added' | 'removed'
) => OntologyChange | null
```

### Example: Feature Structure Lens

```js
export const FeatureStructureLens = (triple, direction) => {
  const { subject, predicate, object } = triple

  // Rule 1: Feature type assertions
  if (
    predicate === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
    object === 'http://example.org/ontology#Feature'
  ) {
    return {
      kind: direction === 'added' ? 'FeatureAdded' : 'FeatureRemoved',
      entity: subject,
    }
  }

  // Rule 2: Role assignments
  if (predicate === 'http://example.org/ontology#hasRole') {
    return {
      kind: direction === 'added' ? 'RoleAdded' : 'RoleRemoved',
      entity: subject,
      role: object,
      details: { roleType: object },
    }
  }

  // Rule 3: Service dependencies
  if (predicate === 'http://example.org/ontology#hasService') {
    return {
      kind: direction === 'added' ? 'ServiceAdded' : 'ServiceRemoved',
      entity: subject,
      role: 'hasService',
      details: { service: object },
    }
  }

  // Ignore this triple
  return null
}
```

### Example: Policy Pack Lens

```js
export const PolicyPackLens = (triple, direction) => {
  const { subject, predicate, object } = triple

  if (predicate === 'http://example.org/ontology#appliesPolicy') {
    return {
      kind: direction === 'added' ? 'PolicyApplied' : 'PolicyRemoved',
      entity: subject,
      details: { policy: object },
    }
  }

  if (predicate === 'http://example.org/ontology#hasConstraint') {
    return {
      kind: direction === 'added' ? 'ConstraintAdded' : 'ConstraintRemoved',
      entity: subject,
      role: 'constraint',
      details: { constraint: object },
    }
  }

  return null
}
```

## Schema Validation

All inputs and outputs are validated with Zod:

```js
import {
  DiffTripleSchema,
  GraphDiffSchema,
  OntologyChangeSchema,
  OntologyDiffSchema,
} from './diff.mjs'

// Validate a triple
const triple = DiffTripleSchema.parse({
  subject: 'http://example.org/s',
  predicate: 'http://example.org/p',
  object: 'http://example.org/o',
})

// Validate a complete ontology diff
const ontologyDiff = OntologyDiffSchema.parse({
  triples: { added: [...], removed: [...] },
  changes: [...],
})
```

## Types

### `DiffTriple`

A simplified triple representation:

```ts
{
  subject: string    // IRI
  predicate: string  // IRI
  object: string     // IRI or literal
}
```

### `GraphDiff`

Triple-level diff:

```ts
{
  added: DiffTriple[]
  removed: DiffTriple[]
}
```

### `OntologyChange`

Semantic-level change:

```ts
{
  kind: string           // e.g., 'FeatureAdded', 'RoleRemoved'
  entity?: string        // IRI of affected entity
  role?: string          // Role name, e.g., 'hasService'
  details?: any          // Custom structured data
  [key: string]: any     // Additional fields allowed
}
```

### `OntologyDiff`

Combined graph + ontology diff:

```ts
{
  triples: GraphDiff
  changes: OntologyChange[]
}
```

## Performance Characteristics

| Function | Complexity | Notes |
|----------|-----------|-------|
| `diffGraphFromStores` | O(n + m) | Scans both stores |
| `diffGraphFromDelta` | O(k) | Works with delta only |
| `diffOntologyFromGraphDiff` | O(k * l) | k = triple count, l = lens cost |
| `diffOntologyFromDelta` | O(k * l) | Most efficient for transactions |
| `summarizeChangesByKind` | O(c) | c = change count |
| `changesForEntity` | O(c) | Linear filter |

**Recommendation**: Use `diffOntologyFromDelta` for transaction processing.

## Testing

305 comprehensive tests covering:

- ✓ Triple-level diffing (additions, removals, identity)
- ✓ Delta-based diffing
- ✓ Ontology lens application
- ✓ Change summarization & filtering
- ✓ Schema validation
- ✓ Edge cases (blank nodes, long IRIs, special characters)
- ✓ Performance (1000+ triple diffs)
- ✓ Error handling

Run tests:

```bash
npm test -- test/diff.test.mjs
```

## Example: Complete Workflow

```js
import { Store, DataFactory } from 'n3'
import {
  diffOntologyFromStores,
  summarizeChangesByKind,
  changesForEntity
} from './diff.mjs'

const { namedNode, literal } = DataFactory

// Setup ontology lens
const FeatureLens = (triple, direction) => {
  if (
    triple.predicate === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
    triple.object === 'http://example.org/Feature'
  ) {
    return {
      kind: direction === 'added' ? 'FeatureAdded' : 'FeatureRemoved',
      entity: triple.subject,
    }
  }
  return null
}

// Create stores
const before = new Store()
before.addQuad(
  namedNode('http://ex.org/f1'),
  namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  namedNode('http://example.org/Feature')
)

const after = new Store()
after.addQuad(
  namedNode('http://ex.org/f1'),
  namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  namedNode('http://example.org/Feature')
)
after.addQuad(
  namedNode('http://ex.org/f2'),
  namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  namedNode('http://example.org/Feature')
)

// Compute diff
const ontologyDiff = diffOntologyFromStores(before, after, FeatureLens)

// Summarize
const summary = summarizeChangesByKind(ontologyDiff)
console.log('Summary:', summary)           // { FeatureAdded: 1 }

// Filter by entity
const f2Changes = changesForEntity(ontologyDiff, 'http://ex.org/f2')
console.log('f2 changes:', f2Changes)      // [{ kind: 'FeatureAdded', ... }]

// Access raw triple changes
console.log('Triple diffs:', ontologyDiff.triples)
```

## Best Practices

1. **Define lenses at module level** for reuse across transactions
2. **Use `diffOntologyFromDelta`** in transaction handlers for efficiency
3. **Keep lenses pure** – no side effects, deterministic
4. **Chain lenses** for complex domains using composition
5. **Validate lens results** – Zod will catch malformed changes
6. **Use `summarizeChangesByKind`** for metrics/logging
7. **Filter by entity** to find affected resources

## Future Enhancements

- Blank node canonicalization
- Nested lens composition
- Change aggregation strategies
- Performance profiling hooks
- Distributed diffing for federated stores
