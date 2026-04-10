# Scoped Rule: RDF/N3 Import Discipline

**Scope**: All RDF data loading and N3 quad processing

## Quad Cloning Pattern

**CRITICAL**: Use `cloneQuad()` for quad mutations. NEVER spread `{...quad}`.

### Why This Matters

RDF quads from `N3.Parser` are frozen objects. Spreading them loses the prototype chain, breaking `cloneQuad()` and quad equality checks.

### Correct Pattern

```javascript
import { cloneQuad } from '@unrdf/core';

// Correct: cloneQuad with overrides
const modified = cloneQuad(originalQuad, {
  object: DataFactory.namedNode('new-value')
});

// Wrong: spread operator (breaks prototype)
const broken = { ...originalQuad, object: DataFactory.namedNode('new-value') };
```

## RDF Load Operations

### Parse with Format Specifier

```javascript
// Correct
await store.load(format: 'application/n-triples', data);

// Wrong - format:turtle serializer has issues
await store.load(format: 'text/turtle', data);
```

### Property Assertions

Use `set(obj.prop)` for comparisons since RDF property order isn't guaranteed:

```javascript
// Correct
expect(entity.get('type').value).toBe('schema:Person');

// Wrong - assumes order
expect(entity.get('type')[0].value).toBe('schema:Person');
```

## SPARQL Query Discipline

### ASK Queries: Separate Subjects

```sparql
# Correct - subjects separated by .
?s p o . ?s2 p2 o2

# Wrong - using ; when subjects differ
?s p o ; ?s2 p2 o2
```

### CONSTRUCT for Data Generation

Always use SPARQL CONSTRUCT for data generation. Never INSERT:

```sparql
# Correct - CONSTRUCT
CONSTRUCT { ?s a :ImprovedType }
WHERE { ?s a :OldType }

# Wrong - INSERT (ad hoc)
INSERT DATA { :s a :NewType }
```

## Files Using Quad Operations

- `packages/hooks/src/n3-spread.js` - N3 forward-chaining hooks
- `packages/core/src/rdf/*` - RDF core utilities
- Any code processing `N3.Parser` output
