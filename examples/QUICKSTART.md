# UNRDF Quick Start Guide

**Get running with UNRDF in 5 minutes.**

## Prerequisites

- Node.js 18+ installed
- Basic understanding of RDF (Turtle format)
- Familiarity with JavaScript/ESM modules

## Installation

```bash
# Clone the repository
git clone https://github.com/unrdf/unrdf.git
cd unrdf

# Install dependencies
pnpm install

# Build packages
pnpm build
```

## Your First UNRDF Program

Create a file called `my-first-graph.mjs`:

```javascript
import { createKnowledgeSubstrateCore } from 'unrdf'

// One function gives you everything
const core = await createKnowledgeSubstrateCore()

// Parse some RDF data
const ttl = `
  @prefix ex: <http://example.org/> .
  ex:Alice ex:knows ex:Bob .
  ex:Bob ex:knows ex:Charlie .
`

const store = core.parseTurtle(ttl)

// Query it with SPARQL
const results = core.query(store, `
  SELECT ?person ?friend WHERE {
    ?person <http://example.org/knows> ?friend .
  }
`)

// Print results
for (const binding of results) {
  console.log(
    `${binding.get('person').value} knows ${binding.get('friend').value}`
  )
}

// Cleanup
await core.cleanup()
```

Run it:

```bash
node my-first-graph.mjs
```

**Expected Output:**

```
http://example.org/Alice knows http://example.org/Bob
http://example.org/Bob knows http://example.org/Charlie
```

## What Just Happened?

1. **Created a core instance** - `createKnowledgeSubstrateCore()` initializes the RDF engine
2. **Parsed Turtle** - `parseTurtle()` converts RDF text to a queryable store
3. **Executed SPARQL** - `query()` returns bindings that match the pattern
4. **Cleaned up** - `cleanup()` releases resources

## Next Steps

### Add Data Validation

```javascript
import { createKnowledgeSubstrateCore } from 'unrdf'
import { defineHook } from '@unrdf/hooks'

// Define a validation hook
defineHook('validate-friends', {
  type: 'validate-before-write',
  async check(quad) {
    // Only allow "knows" relationships
    return quad.predicate.value === 'http://example.org/knows'
  }
})

const core = await createKnowledgeSubstrateCore()

// Now invalid data will be rejected
```

**See:** [basic-knowledge-hook.mjs](./basic-knowledge-hook.mjs) for full example.

### Query Performance Optimization

```javascript
import { createKnowledgeSubstrateCore } from 'unrdf'
import { DarkMatterOptimizer } from '@unrdf/dark-matter'

const core = await createKnowledgeSubstrateCore()

// Enable query optimization
const optimizer = new DarkMatterOptimizer()
const optimizedQuery = optimizer.optimize(sparqlQuery)

const results = core.query(store, optimizedQuery)
```

**See:** [dark-matter-80-20.mjs](./dark-matter-80-20.mjs) for full example.

### Add Audit Trails

```javascript
import { createKnowledgeSubstrateCore } from 'unrdf'
import { LockchainValidator } from '@unrdf/lockchain'

const core = await createKnowledgeSubstrateCore()

// Enable cryptographic provenance
const lockchain = new LockchainValidator()
lockchain.track(store, 'compliance-audit')

// All changes are now signed and immutable
```

**See:** [lockchain-demo.mjs](./lockchain-demo.mjs) for full example.

## Common Patterns

### Pattern 1: Parse → Query → Export

```javascript
const core = await createKnowledgeSubstrateCore()

// 1. Parse data
const store = core.parseTurtle(turtleData)

// 2. Query it
const results = core.query(store, sparqlQuery)

// 3. Export results as JSON-LD
const jsonld = core.exportJSONLD(results)

await core.cleanup()
```

### Pattern 2: Validate → Transform → Store

```javascript
import { defineHook } from '@unrdf/hooks'

// 1. Validate before writing
defineHook('validate', {
  type: 'validate-before-write',
  check: (quad) => isValid(quad)
})

// 2. Transform after validation
defineHook('transform', {
  type: 'transform-after-validate',
  transform: (quad) => normalizeQuad(quad)
})

// 3. Store with hooks applied
const core = await createKnowledgeSubstrateCore()
const store = core.parseTurtle(data) // Hooks auto-apply
```

### Pattern 3: Real-time Updates

```javascript
import { createKnowledgeSubstrateCore } from 'unrdf'
import { ChangeStream } from '@unrdf/streaming'

const core = await createKnowledgeSubstrateCore()
const stream = new ChangeStream(store)

stream.on('quad-added', (quad) => {
  console.log('New quad:', quad)
})

stream.on('quad-removed', (quad) => {
  console.log('Removed quad:', quad)
})
```

## Troubleshooting

### Import Errors

**Problem:** `Cannot find module 'unrdf'`

**Solution:** Make sure you've run `pnpm build` in the project root.

```bash
cd /path/to/unrdf
pnpm build
```

### SPARQL Syntax Errors

**Problem:** Query returns no results

**Solution:** Check your SPARQL syntax and URI prefixes:

```javascript
// ❌ Wrong: Missing angle brackets
const query = `SELECT ?s WHERE { ?s knows ?o }`

// ✅ Right: Full URIs in angle brackets
const query = `SELECT ?s WHERE { ?s <http://example.org/knows> ?o }`
```

### Memory Leaks

**Problem:** Process runs out of memory on large datasets

**Solution:** Always call `cleanup()` and use streaming for large files:

```javascript
import { StreamingParser } from '@unrdf/streaming'

const parser = new StreamingParser()
parser.on('quad', (quad) => {
  // Process one quad at a time
})

await parser.parse(largeFile)
```

## Understanding Results

### SPARQL Bindings

```javascript
const results = core.query(store, `SELECT ?s ?p ?o WHERE { ?s ?p ?o }`)

for (const binding of results) {
  // Each binding is a Map
  console.log(binding.get('s').value) // Subject URI
  console.log(binding.get('p').value) // Predicate URI
  console.log(binding.get('o').value) // Object value
}
```

### RDF Terms

```javascript
// Named Node (URI)
console.log(term.termType) // 'NamedNode'
console.log(term.value) // 'http://example.org/Alice'

// Literal (String)
console.log(term.termType) // 'Literal'
console.log(term.value) // 'Alice'
console.log(term.datatype.value) // 'http://www.w3.org/2001/XMLSchema#string'

// Blank Node (anonymous)
console.log(term.termType) // 'BlankNode'
console.log(term.value) // '_:b1'
```

## What to Learn Next

| Goal | Next Example | Time |
|------|--------------|------|
| Understand the system architecture | [context-example.mjs](./context-example.mjs) | 10 min |
| Add autonomous behaviors | [basic-knowledge-hook.mjs](./basic-knowledge-hook.mjs) | 15 min |
| Optimize query performance | [dark-matter-80-20.mjs](./dark-matter-80-20.mjs) | 30 min |
| Add compliance audit trails | [lockchain-demo.mjs](./lockchain-demo.mjs) | 20 min |
| Implement governance policies | [policy-pack-demo.mjs](./policy-pack-demo.mjs) | 25 min |

## Getting Help

- **Examples**: [examples/](./README.md) - All example code
- **API Docs**: [packages/core/README.md](../packages/core/README.md) - Core API reference
- **Issues**: [GitHub Issues](https://github.com/unrdf/unrdf/issues) - Report bugs
- **Discord**: [UNRDF Community](https://discord.gg/unrdf) - Ask questions

## Best Practices

1. **Always cleanup** - Call `await core.cleanup()` when done
2. **Use hooks for validation** - Don't validate manually
3. **Optimize queries** - Use Dark Matter for large datasets
4. **Stream large files** - Don't load entire files into memory
5. **Test with real data** - Examples use toy data; test with your actual use case

---

**Ready to dive deeper?** Head to [examples/README.md](./README.md) to explore all examples.
