# Minimal unrdf Knowledge Hooks Core

A minimal, opinionated Knowledge Hooks system for RDF graphs built with N3.js and Comunica.

## Features

- **One Store Rule**: Single N3 store + Comunica engine
- **Pure Functions**: `defineHook()`, `evaluateHook()`, `planHook()`
- **Predicate Types**: ASK, THRESHOLD, DELTA, SHACL (stub), WINDOW (tumbling)
- **Deterministic Receipts**: SHA1 hashes for data/spec, timings, evidence
- **Front-matter Loader**: Load hooks from Markdown files
- **Extensible**: Register custom predicate types

## Quick Start

```javascript
import { initStore, defineHook, evaluateHook } from './src/hooks.mjs'

// Initialize store with RDF data
const { store, engine } = initStore(turtleData)

// Define a hook
const hook = defineHook({
  id: 'ex:HealthMonitor',
  select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
  ],
  combine: 'OR'
})

// Evaluate the hook
const receipt = await evaluateHook(hook, { store, engine })
console.log(receipt.fired ? '🔥 Alert!' : '— All good')
```

## Predicate Types

### ASK Predicate
Boolean SPARQL queries that return true/false.

```javascript
{ kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } }
```

### THRESHOLD Predicate
Numeric comparisons on query result variables.

```javascript
{ kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
```

**Operators**: `>`, `>=`, `<`, `<=`, `==`, `!=`

### DELTA Predicate
Change detection using stable hashes of key variables.

```javascript
{ kind: 'DELTA', spec: { key: ['service', 'errorRate'], prev: [] } }
```

### SHACL Predicate
Shape validation (currently a stub).

```javascript
{ kind: 'SHACL', spec: { shapes: 'ex:ServiceShape', strict: true } }
```

### WINDOW Predicate
Tumbling window aggregations with comparisons.

```javascript
{ 
  kind: 'WINDOW', 
  spec: { 
    var: 'requests', 
    size: '5m', 
    op: 'count', 
    cmp: { op: '>', value: 100 } 
  } 
}
```

**Operations**: `count`, `sum`, `avg`

## Front-matter Hooks

Load hooks from Markdown files with YAML front-matter:

```markdown
---
hook:
  id: 'ex:ServiceHealth'
  select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }'
  predicates:
    - kind: 'THRESHOLD'
      spec:
        var: 'errorRate'
        op: '>'
        value: 0.02
  combine: 'OR'
---

# Service Health Monitor

This hook monitors service error rates...
```

```javascript
import { loadFrontmatterHook } from './src/hooks.mjs'

const hook = await loadFrontmatterHook('service-health.md')
```

## Custom Predicates

Register your own predicate types:

```javascript
import { registerPredicate } from './src/hooks.mjs'

registerPredicate('CUSTOM_COUNT', async (spec, ctx) => {
  const count = ctx.rows.length
  return { 
    ok: count >= spec.threshold, 
    meta: { count, threshold } 
  }
})
```

## Receipts

Every hook evaluation returns a deterministic receipt:

```javascript
{
  id: 'ex:HealthMonitor',
  fired: true,
  predicates: [
    { kind: 'THRESHOLD', ok: true, meta: { matched: 1 } }
  ],
  durations: { totalMs: 15 },
  provenance: {
    hookId: 'ex:HealthMonitor',
    qHash: 'a733bbe87c611e3eb31c2639f6500bc2fb7dfcca',
    pHash: 'ec2b13e61e0f521738c03267f5229abe6ef5470c',
    sHash: 'd435a6cdd786300dff204ee7c2ef942d3e9034e2'
  },
  at: '2025-09-24T23:19:44.825Z'
}
```

## CLI Usage

```bash
# Test the system
node test-hooks-system.mjs

# Use the CLI
node hooks-cli.mjs eval examples/hooks/service-health.md
node hooks-cli.mjs create my-hook.md
node hooks-cli.mjs demo
```

## API Reference

### Core Functions

- `initStore(turtle?, options?)` - Initialize N3 store and Comunica engine
- `defineHook(spec)` - Create a frozen hook specification
- `evaluateHook(hook, env)` - Evaluate hook and return receipt
- `planHook(hook)` - Generate execution plan without evaluation
- `loadFrontmatterHook(filePath)` - Load hook from Markdown file
- `registerPredicate(kind, impl)` - Register custom predicate type

### Environment

```javascript
const env = { store: Store, engine: QueryEngine }
```

### Hook Specification

```javascript
{
  id: string,
  select?: string,        // SPARQL SELECT query
  ask?: string,          // SPARQL ASK query
  predicates: Array<PredicateSpec>,
  combine?: 'AND' | 'OR', // Default: 'AND'
  effect?: Function      // Side effect when fired
}
```

## Examples

See the `examples/hooks/` directory for complete examples:

- `service-health.md` - Service monitoring hook
- `test-hooks-system.mjs` - Comprehensive test suite
- `hooks-cli.mjs` - Command-line interface

## Dependencies

- `n3` - RDF store and parser
- `@comunica/query-sparql` - SPARQL query engine
- `gray-matter` - Front-matter parsing

## License

MIT
