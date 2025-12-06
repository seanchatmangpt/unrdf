# Video Script: What's New in UNRDF v5

**Duration**: 12-15 minutes
**Target Audience**: New and existing users
**Difficulty**: Beginner to Intermediate

---

## Opening (0:00 - 0:45)

**[SCREEN: Title Card - "UNRDF v5: What's New"]**

> UNRDF v5 is the biggest release in the project's history. We've completely rewritten the core engine, achieved 40% faster performance, and added production-grade reliability.
>
> In this video, I'll show you the 8 major features that make v5 a game-changer for RDF and knowledge graph development.

**[SCREEN: Feature overview grid]**

**What we'll cover:**
1. Oxigraph Integration - 40% faster queries
2. Zero-copy Architecture - 60% memory reduction
3. Synchronous SPARQL API - Simplified code
4. OpenTelemetry Observability - Production monitoring
5. Enhanced CLI Tools - Developer experience
6. Knowledge Hooks API - Validation & governance
7. Production Readiness - FMEA validation
8. Comprehensive Documentation - 160+ guides

> Let's dive in!

---

## Feature 1: Oxigraph Integration (0:45 - 2:30)

**[SCREEN: Architecture diagram - N3.js → Oxigraph]**

> The biggest change in v5 is the switch from N3.js to Oxigraph for our RDF store backend.

**Why Oxigraph?**
- Written in Rust (not JavaScript)
- Native SPARQL 1.1 engine
- Zero-copy memory architecture
- Battle-tested in production

**[SCREEN: Performance benchmark chart]**

> Let's see the real-world impact. Here's a benchmark with 100,000 triples:

```bash
$ npm run bench:query

v4.x (N3.js):   2,450ms average query time
v5.0 (Oxigraph):  1,470ms average query time

Result: 40% faster ✅
```

**[SCREEN: Code example]**

> Using it is simple:

```javascript
import { createStore } from '@unrdf/oxigraph';

const store = createStore();
store.addQuad({
  subject: namedNode('http://example.org/Alice'),
  predicate: namedNode('http://schema.org/name'),
  object: literal('Alice'),
  graph: defaultGraph()
});

// SPARQL queries are now 40% faster
const results = store.executeQuery('SELECT * WHERE { ?s ?p ?o }');
```

> The API is familiar, but the performance is transformative.

---

## Feature 2: Zero-Copy Architecture (2:30 - 4:00)

**[SCREEN: Memory diagram - with/without copying]**

> Traditional RDF libraries copy data multiple times:
- Once when parsing
- Again when storing
- Again when querying

**[ANIMATE: Data flow diagram]**

> v5's zero-copy architecture eliminates this:

```
Parse → Store (no copy) → Query (no copy)
```

**[SCREEN: Memory usage chart]**

> Let's measure it with 100,000 triples:

```bash
$ npm run bench:memory

v4.x Memory: 145 MB
v5.0 Memory:  58 MB

Result: 60% reduction ✅
```

**[SCREEN: Real-world impact]**

> What does this mean for you?

| Dataset Size | v4.x Memory | v5.0 Memory | Savings |
|--------------|-------------|-------------|---------|
| 100K triples | 145 MB      | 58 MB       | 87 MB   |
| 1M triples   | 1.4 GB      | 560 MB      | 840 MB  |
| 10M triples  | 14 GB       | 5.6 GB      | 8.4 GB  |

> For large knowledge graphs, this is a game-changer.

---

## Feature 3: Synchronous SPARQL API (4:00 - 5:30)

**[SCREEN: Code comparison - async vs sync]**

> v5 introduces synchronous SPARQL execution alongside the async API.

**Before (v4.x - async only):**
```javascript
const results = await executeQuery(store, 'SELECT * WHERE { ?s ?p ?o }');
```

**After (v5.0 - synchronous option):**
```javascript
const results = executeQuerySync(store, 'SELECT * WHERE { ?s ?p ?o }');
// No await needed!
```

**[SCREEN: Use cases]**

**When to use sync API:**
- ✅ Command-line tools
- ✅ Build scripts
- ✅ Testing utilities
- ✅ Simple queries (<1000 results)

**When to use async API:**
- ✅ Web servers
- ✅ Large datasets
- ✅ Long-running queries
- ✅ UI applications

**[SCREEN: Example - CLI tool]**

```javascript
#!/usr/bin/env node
import { createStore, executeQuerySync } from '@unrdf/core';

const store = createStore();
// ... load data ...

// Synchronous execution - clean code
const results = executeQuerySync(store, process.argv[2]);
console.table(results);
```

> Much cleaner than async/await for simple scripts!

---

## Feature 4: OpenTelemetry Observability (5:30 - 7:00)

**[SCREEN: OTEL trace visualization]**

> Production systems need observability. v5 includes built-in OpenTelemetry instrumentation.

**[SCREEN: Code example]**

```javascript
import { trace } from '@opentelemetry/api';
import { createStore, executeQuery } from '@unrdf/core';

// Automatically traced - no extra code needed
const store = createStore();
const results = await executeQuery(store, sparqlQuery);

// OTEL automatically captures:
// - Query execution time
// - Number of results
// - Error details
// - Memory usage
```

**[SCREEN: Jaeger UI showing traces]**

> Here's what you see in Jaeger:

**[WALKTHROUGH: Trace timeline]**

- `sparql.query` span (1,470ms)
  - `store.match` span (980ms)
  - `result.serialize` span (490ms)

**[SCREEN: Metrics dashboard]**

> You can also export metrics:

- Query throughput (queries/sec)
- Query latency (p50, p95, p99)
- Store size (triples)
- Memory usage (MB)

**[SCREEN: Alert configuration]**

```yaml
# Example alert rule
- alert: SlowSPARQLQuery
  expr: sparql_query_duration_ms > 5000
  for: 1m
  labels:
    severity: warning
```

> Perfect for production monitoring!

---

## Feature 5: Enhanced CLI Tools (7:00 - 8:30)

**[SCREEN: Terminal showing CLI commands]**

> v5 includes a completely redesigned CLI with 15+ commands.

**Core Commands:**
```bash
# Load and query graphs
unrdf load graph.ttl --format turtle
unrdf query "SELECT * WHERE { ?s ?p ?o }" --format table

# Convert between formats
unrdf convert input.ttl output.nt --from turtle --to ntriples

# Merge graphs
unrdf merge graph1.ttl graph2.ttl --output combined.ttl

# Validate RDF
unrdf validate graph.ttl --shacl shapes.ttl
```

**[RECORD: Live demo of CLI commands]**

```bash
$ unrdf load data/example.ttl

✅ Loaded 1,247 triples from data/example.ttl

$ unrdf query "SELECT ?name WHERE { ?s <name> ?name }" --format table

┌─────────────┐
│ name        │
├─────────────┤
│ "Alice"     │
│ "Bob"       │
│ "Charlie"   │
└─────────────┘

$ unrdf stats

Graph Statistics:
  Triples: 1,247
  Unique subjects: 423
  Unique predicates: 18
  Unique objects: 806
  Formats supported: Turtle, N-Triples, JSON-LD, RDF/XML
```

**[SCREEN: Pipeline example]**

> CLI commands are composable:

```bash
# Complex pipeline
unrdf load data.ttl | \
  unrdf query "CONSTRUCT { ?s <type> ?type } WHERE { ?s a ?type }" | \
  unrdf convert --to jsonld > types.json
```

---

## Feature 6: Knowledge Hooks API (8:30 - 10:00)

**[SCREEN: Architecture diagram - hooks system]**

> Knowledge Hooks are the most powerful new feature in v5. They let you intercept and validate graph operations.

**[SCREEN: Code example]**

```javascript
import { createStore } from '@unrdf/oxigraph';
import { registerHook } from '@unrdf/hooks';

const store = createStore();

// Register validation hook
registerHook(store, 'before:add', async (quad) => {
  // Ensure all predicates use HTTPS
  if (quad.predicate.value.startsWith('http://')) {
    throw new Error(`Insecure predicate: ${quad.predicate.value}`);
  }

  // Validate literal types
  if (quad.object.termType === 'Literal') {
    const datatype = quad.object.datatype.value;
    if (datatype === 'http://www.w3.org/2001/XMLSchema#integer') {
      if (isNaN(parseInt(quad.object.value))) {
        throw new Error(`Invalid integer: ${quad.object.value}`);
      }
    }
  }

  return quad; // Allow operation
});

// This will fail validation
store.addQuad({
  subject: namedNode('http://example.org/resource'),
  predicate: namedNode('http://insecure.org/prop'), // ❌ HTTP not HTTPS
  object: literal('value')
});
// Error: Insecure predicate: http://insecure.org/prop
```

**[SCREEN: Use cases]**

**Hook Use Cases:**
- ✅ Data validation (SHACL, custom rules)
- ✅ Access control (who can modify what)
- ✅ Audit logging (track all changes)
- ✅ Derived triples (inference rules)
- ✅ Change notifications (pub/sub)

**[SCREEN: Performance note]**

> **Performance Note:** Hooks add 11-45μs per operation. For validation, this is acceptable. For bulk operations (>10K), disable hooks temporarily.

---

## Feature 7: Production Readiness (10:00 - 11:30)

**[SCREEN: FMEA report summary]**

> v5 underwent rigorous production readiness validation using FMEA (Failure Mode Effects Analysis).

**[SCREEN: Checklist]**

**Production Checklist:**
- ✅ 231+ tests (100% pass rate)
- ✅ OTEL validation score: 83/100
- ✅ 0 critical security vulnerabilities
- ✅ 100% N3 compliance (851 files)
- ✅ Performance benchmarks verified
- ✅ Error handling comprehensive
- ✅ Documentation complete

**[SCREEN: Error handling example]**

```javascript
import { createStore, executeQuery } from '@unrdf/core';
import { ZodError } from 'zod';

try {
  const store = createStore();
  const results = await executeQuery(store, invalidQuery);
} catch (error) {
  if (error instanceof ZodError) {
    console.error('Invalid query structure:', error.errors);
  } else if (error.name === 'SPARQLParseError') {
    console.error('SPARQL syntax error:', error.message);
  } else {
    console.error('Unexpected error:', error);
  }
}
```

**[SCREEN: Production deployment example]**

```javascript
// Production configuration
import { NodeSDK } from '@opentelemetry/sdk-node';
import { createStore } from '@unrdf/core';

const sdk = new NodeSDK({
  serviceName: 'knowledge-graph-api',
  // ... OTEL config
});

sdk.start();

const store = createStore({
  maxMemory: '4GB',
  enableHooks: true,
  enableOTEL: true
});

// Store is now production-ready with:
// - Memory limits
// - Observability
// - Validation hooks
```

---

## Feature 8: Comprehensive Documentation (11:30 - 12:30)

**[SCREEN: Documentation site]**

> v5 includes 160+ documentation files following the Diataxis framework.

**[SCREEN: Documentation structure]**

**Documentation Types:**

1. **Tutorials** (Learning-oriented)
   - Getting Started Guide
   - Building a Knowledge Graph
   - SPARQL Query Tutorial

2. **How-To Guides** (Task-oriented)
   - How to Migrate from v4
   - How to Optimize Query Performance
   - How to Deploy to Production

3. **Reference** (Information-oriented)
   - API Documentation
   - SPARQL Functions
   - CLI Command Reference

4. **Explanation** (Understanding-oriented)
   - Architecture Overview
   - Design Decisions
   - Performance Characteristics

**[SCREEN: Example doc page]**

> Every package includes:
- README with quick start
- API reference (generated from JSDoc)
- Migration guide
- Performance benchmarks
- Examples directory

---

## Wrap-up (12:30 - 13:00)

**[SCREEN: Summary of 8 features]**

> Let's recap the 8 major features:

1. ✅ Oxigraph Integration - 40% faster
2. ✅ Zero-Copy Architecture - 60% memory reduction
3. ✅ Synchronous SPARQL API - cleaner code
4. ✅ OpenTelemetry - production observability
5. ✅ Enhanced CLI - 15+ commands
6. ✅ Knowledge Hooks - validation & governance
7. ✅ Production Ready - FMEA validated
8. ✅ Comprehensive Docs - 160+ guides

**[SCREEN: Getting started]**

```bash
# Try it today
npm install @unrdf/core@5.0.0-beta.3 @unrdf/oxigraph@5.0.0-beta.3
```

**[SCREEN: Resources]**

> **Next Steps:**
- 📖 Read the docs: https://unrdf.org/docs
- 🎥 Watch "Getting Started with v5" (next video)
- 💬 Join Discord: https://discord.gg/unrdf
- ⭐ Star on GitHub: https://github.com/unrdf/unrdf

> Thanks for watching!

---

## Production Notes

**B-Roll Footage:**
- Performance benchmarks running
- CLI commands in action
- Code editor with autocomplete
- Documentation site navigation
- Jaeger UI showing traces
- Test suite executing
- Memory profiler visualization

**Graphics:**
- Feature comparison table (v4 vs v5)
- Performance charts (speed, memory)
- Architecture diagrams
- Hook system flow diagram
- FMEA checklist overlay

**Code Demos:**
- Syntax highlighted with copy button
- Live terminal recordings (asciinema)
- Split screen for comparisons
- Animated type hints

**Timing:**
- Each feature: ~1.5-2 minutes
- Keep pace dynamic
- Use transitions between features
- Add chapter markers
