# UNRDF Examples Guide

**Comprehensive guide to all UNRDF examples, organized by use case.**

## 📋 Table of Contents

1. [Getting Started](#getting-started)
2. [Core Functionality](#core-functionality)
3. [Knowledge Hooks](#knowledge-hooks)
4. [Query Optimization](#query-optimization)
5. [Governance & Compliance](#governance--compliance)
6. [Streaming & Real-time](#streaming--real-time)
7. [Browser & Client-side](#browser--client-side)
8. [CLI Automation](#cli-automation)
9. [Full-Stack Integration](#full-stack-integration)
10. [Advanced Topics](#advanced-topics)

---

## Getting Started

### 01-minimal-parse-query.mjs
**[View Code](../../examples/01-minimal-parse-query.mjs)**

**What It Shows:** The minimal UNRDF entry point - parse RDF and run SPARQL queries.

**Prerequisites:** None - this is the starting point.

**How to Run:**
```bash
node examples/01-minimal-parse-query.mjs
```

**Expected Output:**
```
http://example.org/Alice knows http://example.org/Bob
http://example.org/Bob knows http://example.org/Charlie
http://example.org/Charlie knows http://example.org/Diana
Transaction manager ready: true
Core status: { status: 'active', components: 1 }
```

**Key Code:**
```javascript
import { createKnowledgeSubstrateCore } from 'unrdf'

const core = await createKnowledgeSubstrateCore()
const store = core.parseTurtle(ttl)
const results = core.query(store, sparqlQuery)

await core.cleanup()
```

**Next Steps:** Add validation with [basic-knowledge-hook.mjs](../../examples/basic-knowledge-hook.mjs)

---

## Core Functionality

### minimal-core-example.mjs
**[View Code](../../examples/minimal-core-example.mjs)**

**What It Shows:** Direct usage of @unrdf/core without the substrate wrapper.

**Prerequisites:** Understanding of RDF terms and stores.

**How to Run:**
```bash
node examples/minimal-core-example.mjs
```

**Expected Output:**
```
Parsed 3 quads from Turtle
Query results: [
  { s: 'http://example.org/Alice', name: 'Alice' }
]
Exported to JSON-LD: {...}
```

**Key Code:**
```javascript
import { parseTurtle, executeQuery, exportJSONLD } from '@unrdf/core'

const store = await parseTurtle(ttl)
const results = await executeQuery(store, sparql)
const jsonld = await exportJSONLD(store)
```

**When to Use:** When you need low-level control without the substrate lifecycle.

---

## Knowledge Hooks

### basic-knowledge-hook.mjs
**[View Code](../../examples/basic-knowledge-hook.mjs)**

**What It Shows:** Define and execute hooks for validation and transformation.

**Prerequisites:** Completed [01-minimal-parse-query.mjs](../../examples/01-minimal-parse-query.mjs)

**How to Run:**
```bash
node examples/basic-knowledge-hook.mjs
```

**Expected Output:**
```
✅ Valid quad passed hook
❌ Invalid quad rejected by hook
Transformed quad: { predicate: normalized-value }
```

**Key Code:**
```javascript
import { defineHook, executeHook } from '@unrdf/hooks'

defineHook('validate-pii', {
  type: 'validate-before-write',
  async check(quad) {
    return !containsPII(quad)
  }
})

const isValid = await executeHook('validate-pii', quad)
```

**Common Mistakes:**
- Forgetting to `await executeHook()` (it's async)
- Defining hooks after data is already written
- Not handling hook errors

**Next Steps:** Combine multiple hooks in [define-hook-example.mjs](../../examples/define-hook-example.mjs)

### define-hook-example.mjs
**[View Code](../../examples/define-hook-example.mjs)**

**What It Shows:** Advanced hook patterns - composition, chaining, conditional execution.

**Prerequisites:** Completed [basic-knowledge-hook.mjs](../../examples/basic-knowledge-hook.mjs)

**How to Run:**
```bash
node examples/define-hook-example.mjs
```

**Key Concepts:**
- Hook composition (multiple hooks in sequence)
- Conditional hooks (only execute if condition met)
- Hook metadata and debugging

---

## Query Optimization

### dark-matter-80-20.mjs
**[View Code](../../examples/dark-matter-80-20.mjs)**

**What It Shows:** Query optimization with Dark Matter - 5-10x performance improvement.

**Prerequisites:** Understanding of SPARQL query performance.

**How to Run:**
```bash
node examples/dark-matter-80-20.mjs
```

**Expected Output:**
```
Unoptimized query: 1250ms
Optimized query: 180ms
Speedup: 6.9x
```

**Key Code:**
```javascript
import { DarkMatterOptimizer } from '@unrdf/dark-matter'

const optimizer = new DarkMatterOptimizer()
const optimizedQuery = optimizer.optimize(sparqlQuery)

const results = core.query(store, optimizedQuery)
```

**When to Use:**
- Queries taking >500ms
- Large datasets (>100k triples)
- Complex JOIN operations

**Performance Tips:**
- Use indexes for frequently queried predicates
- Cache intermediate results
- Push down filters early

### dark-matter-query-optimization.mjs
**[View Code](../../examples/dark-matter-query-optimization.mjs)**

**What It Shows:** Advanced Dark Matter features - custom indexes, caching strategies.

**Prerequisites:** Completed [dark-matter-80-20.mjs](../../examples/dark-matter-80-20.mjs)

---

## Governance & Compliance

### lockchain-demo.mjs
**[View Code](../../examples/lockchain-demo.mjs)**

**What It Shows:** Cryptographic provenance and audit trails with Lockchain.

**Prerequisites:** Understanding of data compliance requirements.

**How to Run:**
```bash
node examples/lockchain-demo.mjs
```

**Expected Output:**
```
Lockchain initialized
✅ Quad signed: hash=abc123, signature=xyz789
✅ Verification passed
❌ Tampering detected: signature mismatch
```

**Key Code:**
```javascript
import { LockchainValidator } from '@unrdf/lockchain'

const lockchain = new LockchainValidator()
lockchain.track(store, 'audit-trail')

// All changes are now cryptographically signed
```

**Use Cases:**
- GDPR compliance (data lineage)
- Healthcare (HIPAA audit trails)
- Financial services (SOX compliance)
- Government (regulatory compliance)

### policy-pack-demo.mjs
**[View Code](../../examples/policy-pack-demo.mjs)**

**What It Shows:** Declarative governance with Policy Packs.

**Prerequisites:** Completed [basic-knowledge-hook.mjs](../../examples/basic-knowledge-hook.mjs)

**How to Run:**
```bash
node examples/policy-pack-demo.mjs
```

**Expected Output:**
```
Policy pack loaded: gdpr-compliance
✅ Consent verified
✅ Encryption applied
✅ Retention policy enforced
```

**Key Code:**
```javascript
import { PolicyPack } from '@unrdf/policy'

const gdprPack = PolicyPack.load('gdpr-compliance')
gdprPack.enforce(store)

// Policies auto-apply to all operations
```

**Policy Pack Examples:**
- GDPR compliance (consent, encryption, right-to-delete)
- HIPAA compliance (PHI protection, access control)
- PCI-DSS (payment card data security)

### policy-pack-usage.mjs
**[View Code](../../examples/policy-pack-usage.mjs)**

**What It Shows:** Creating custom policy packs for domain-specific governance.

**Prerequisites:** Completed [policy-pack-demo.mjs](../../examples/policy-pack-demo.mjs)

---

## Streaming & Real-time

### examples/streaming/basic-stream.mjs
**[View Code](../../examples/streaming/basic-stream.mjs)**

**What It Shows:** Real-time change feeds with @unrdf/streaming.

**Prerequisites:** Understanding of event-driven programming.

**How to Run:**
```bash
node examples/streaming/basic-stream.mjs
```

**Expected Output:**
```
Streaming started
Change detected: quad-added { subject, predicate, object }
Change detected: quad-removed { subject, predicate, object }
```

**Key Code:**
```javascript
import { ChangeStream } from '@unrdf/streaming'

const stream = new ChangeStream(store)

stream.on('quad-added', (quad) => {
  console.log('New quad:', quad)
})

stream.on('quad-removed', (quad) => {
  console.log('Removed quad:', quad)
})
```

**Use Cases:**
- Live dashboards
- Collaborative editing
- Event-driven architectures
- Notifications

### examples/streaming/advanced-filters.mjs
**[View Code](../../examples/streaming/advanced-filters.mjs)**

**What It Shows:** Filtering change streams by predicate, subject, or pattern.

---

## Browser & Client-side

### examples/browser/indexeddb-store.html
**[View Code](../../examples/browser/indexeddb-store.html)**

**What It Shows:** Persistent RDF storage in the browser with IndexedDB.

**Prerequisites:** Basic HTML/JavaScript knowledge.

**How to Run:**
```bash
# Open in browser
open examples/browser/indexeddb-store.html
```

**Expected Output:**
```
IndexedDB store created
3 quads stored locally
Query results: [...]
Data persists across page reloads
```

**Key Code:**
```javascript
import { IndexedDBStore } from '@unrdf/browser'

const store = new IndexedDBStore('my-graph')

// Data persists across sessions
await store.addQuad(quad)
const results = await store.match(pattern)
```

**Use Cases:**
- Progressive web apps (PWAs)
- Offline-first applications
- Local-first software
- Performance (avoid server roundtrips)

### browser-react.jsx
**[View Code](../../examples/browser-react.jsx)**

**What It Shows:** React integration with @unrdf/composables.

**How to Run:**
```bash
cd examples/browser/react-example
npm install
npm start
```

**Key Code:**
```javascript
import { useQuery, useStore } from '@unrdf/composables'

function MyComponent() {
  const store = useStore()
  const results = useQuery(store, sparqlQuery)

  return <div>{results.map(...)}</div>
}
```

### browser-vue.vue
**[View Code](../../examples/browser-vue.vue)**

**What It Shows:** Vue 3 integration with Composition API.

---

## CLI Automation

### cli-automation-script.mjs
**[View Code](../../examples/cli-automation-script.mjs)**

**What It Shows:** Automating RDF workflows with @unrdf/cli.

**Prerequisites:** Familiarity with command-line tools.

**How to Run:**
```bash
node examples/cli-automation-script.mjs
```

**Expected Output:**
```
Running: unrdf parse data.ttl
✅ Parsed 1,234 triples
Running: unrdf query data.ttl "SELECT * WHERE { ?s ?p ?o }"
✅ 1,234 results
Running: unrdf validate data.ttl schema.shacl
✅ Validation passed
```

**Key Code:**
```javascript
import { runCLI } from '@unrdf/cli'

// Parse
await runCLI(['parse', 'data.ttl'])

// Query
await runCLI(['query', 'data.ttl', sparqlQuery])

// Validate
await runCLI(['validate', 'data.ttl', 'schema.shacl'])
```

**Use Cases:**
- CI/CD pipelines
- Batch processing
- Data validation workflows
- Automated testing

### cli-scaffolding-demo.mjs
**[View Code](../../examples/cli-scaffolding-demo.mjs)**

**What It Shows:** Project scaffolding and code generation.

---

## Full-Stack Integration

### comprehensive-feature-test.mjs
**[View Code](../../examples/comprehensive-feature-test.mjs)**

**What It Shows:** All UNRDF features integrated together.

**Prerequisites:** Familiarity with all previous examples.

**How to Run:**
```bash
node examples/comprehensive-feature-test.mjs
```

**What It Demonstrates:**
1. Parse multiple RDF formats
2. Execute complex SPARQL queries
3. Apply validation hooks
4. Optimize with Dark Matter
5. Track with Lockchain
6. Stream changes in real-time
7. Sync to browser storage
8. Export results

**Architecture Flow:**
```
Data ingestion → Validation → Storage → Optimization → Query → Export
      ↓              ↓           ↓           ↓          ↓        ↓
   Hooks        Lockchain    Streaming   DarkMatter  Browser  CLI
```

---

## Advanced Topics

### knowledge-engine-example.mjs
**[View Code](../../examples/knowledge-engine-example.mjs)**

**What It Shows:** AI-powered semantic analysis and query building.

**Prerequisites:** Understanding of NLP and embedding models.

**Key Features:**
- Semantic search (find similar concepts)
- Natural language to SPARQL
- Entity extraction
- Knowledge graph enrichment

### sparql-query-advanced.mjs
**[View Code](../../examples/sparql-query-advanced.mjs)**

**What It Shows:** Complex SPARQL patterns and optimizations.

**Topics Covered:**
- Property paths
- Federated queries
- OPTIONAL patterns
- FILTER expressions
- Aggregations (COUNT, SUM, AVG)
- Subqueries

### production-hook-test.mjs
**[View Code](../../examples/production-hook-test.mjs)**

**What It Shows:** Production-ready hook testing and debugging.

**Key Patterns:**
- Hook error handling
- Performance profiling
- Debugging techniques
- Testing strategies

---

## Troubleshooting Examples

### Common Issues

**Issue 1: Import errors**
```bash
Error: Cannot find module 'unrdf'
```

**Solution:** Run `pnpm build` in project root.

**Issue 2: Query returns no results**
```javascript
// ❌ Wrong
const query = `SELECT ?s WHERE { ?s knows ?o }`

// ✅ Right
const query = `SELECT ?s WHERE { ?s <http://example.org/knows> ?o }`
```

**Issue 3: Memory leak with large files**
```javascript
// ❌ Wrong: Loads entire file into memory
const store = core.parseTurtle(hugeFile)

// ✅ Right: Stream the file
const parser = new StreamingParser()
parser.on('quad', (quad) => store.addQuad(quad))
```

---

## Example Categories by Use Case

| Use Case | Examples | Time |
|----------|----------|------|
| **Learning UNRDF** | 01-minimal, basic-knowledge-hook, context-example | 30 min |
| **Building an app** | browser-react, cli-automation, streaming/basic-stream | 1 hour |
| **Performance tuning** | dark-matter-80-20, profiling-example | 45 min |
| **Compliance** | lockchain-demo, policy-pack-demo | 45 min |
| **Advanced features** | knowledge-engine, sparql-advanced | 2 hours |

---

## Next Steps

1. Start with [01-minimal-parse-query.mjs](../../examples/01-minimal-parse-query.mjs)
2. Add features incrementally based on your needs
3. Consult [ARCHITECTURE.md](./ARCHITECTURE.md) to understand system design
4. Read package-specific READMEs for API details

---

**Need help?** Join our [Discord](https://discord.gg/unrdf) or [open an issue](https://github.com/unrdf/unrdf/issues).
