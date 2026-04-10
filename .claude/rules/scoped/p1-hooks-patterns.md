# Scoped Rule: Hooks Package Patterns

**Scope**: `@unrdf/hooks` - SPARQL CONSTRUCT and N3 forward-chaining

## ChainResult Structure

`executeHooksByTrigger()` returns `ChainResult`. Access validation correctly:

```javascript
// Correct
const result = await executeHooksByTrigger('before-insert', quad);
if (!result.valid) {
  console.error('Validation failed:', result.errors);
}

// Wrong - old array pattern
const [valid, errors] = result;
if (!valid) { /* ... */ }
```

## Hook Types

### SPARQL CONSTRUCT Hooks

Generate new quads from existing data:

```javascript
// packages/hooks/sparql/infer-types.rq
CONSTRUCT {
  ?s a :InferredType .
}
WHERE {
  ?s a :BaseType ;
     :hasProperty ?value .
  FILTER (?value > 10)
}
```

### N3 Forward-Chaining Hooks

N3 rules with implication:

```
@prefix log: <http://www.w3.org/2000/10/swap/log#>.
@prefix rule: <urn:rule:>.

{ ?s a :BaseType . ?s :hasProperty ?v }
  => { ?s a :InferredType } .
```

## Validation vs. Transformation

### Validation Hooks (Soft-Fail)

Return `ChainResult` with `valid: false` but don't throw:

```javascript
// Correct - soft validation
if (invalid) {
  return { valid: false, errors: ['Reason'], quads: [] };
}
```

### Transformation Hooks

Always return new quads (never mutate input):

```javascript
// Correct - new quads
const newQuads = inputQuads.map(q =>
  cloneQuad(q, { object: transformObject(q.object) })
);
return { valid: true, quads: newQuads };
```

## Hook Triggers

| Trigger | When |
|---------|------|
| `before-insert` | Before quads added to store |
| `after-insert` | After quads added to store |
| `before-update` | Before quad updates |
| `after-update` | After quad updates |
| `before-delete` | Before quads removed |
| `after-delete` | After quads removed |

## File Locations

```
packages/hooks/
├── sparql/          # SPARQL CONSTRUCT hooks (*.rq)
├── n3/              # N3 rules (*.n3)
└── src/             # JavaScript hook handlers
```

## Gotchas

- **Infinite loops**: N3 hooks can loop forever if rules create new quads that trigger same hook
- **cloneQuad required**: Always use `cloneQuad()` for quad modifications (see p0-rdf-imports.md)
- **ChainResult structure**: Check `result.valid`, not `result[0].valid`
