# Introduction

Welcome to **UNRDF** - the production-ready RDF knowledge graph library that transforms static data into intelligent, reactive systems.

## What is UNRDF?

UNRDF is a composable RDF knowledge graph library built on battle-tested foundations ([N3.js](https://github.com/rdfjs/N3.js), [Comunica](https://github.com/comunica/comunica), [SHACL](https://github.com/zazuko/rdf-validate-shacl)) that adds three revolutionary capabilities:

1. **Knowledge Hooks** - Policy-driven autonomic behavior that reacts to graph changes
2. **Cryptographic Provenance** - Immutable audit trails with SHA3-256 Merkle verification
3. **Dark Matter 80/20 Optimization** - Performance-optimized critical path delivering 85% value from 20% of code

## Why Use UNRDF?

### Production-Ready from Day One

```javascript
import { createDarkMatterCore } from 'unrdf';

const system = await createDarkMatterCore();
// You now have:
// ✅ Full SPARQL 1.1 support
// ✅ SHACL validation
// ✅ OpenTelemetry observability
// ✅ Cryptographic audit trails
// ✅ Performance optimizations
```

### Autonomic Knowledge Graphs

Traditional RDF libraries are passive - they store and query data. UNRDF is **reactive** - it enforces policies, maintains invariants, and adapts to changes automatically.

```javascript
import { defineHook, registerHook } from 'unrdf';

// Define a policy: all persons must have names
const hook = defineHook({
  meta: {
    name: 'data-quality-gate',
    description: 'Ensures all persons have names'
  },
  when: {
    kind: 'sparql-ask',
    query: `
      ASK {
        ?person a <http://xmlns.com/foaf/0.1/Person> .
        FILTER NOT EXISTS { ?person <http://xmlns.com/foaf/0.1/name> ?name }
      }
    `
  },
  run: async (event) => {
    if (event.result === true) {
      throw new Error('All persons must have names');
    }
  }
});

await registerHook(hook);
// Now this policy is enforced on every transaction
```

### Cryptographic Provenance

Every change is recorded in an immutable, cryptographically verifiable audit trail using Git-based lockchains with Merkle tree verification.

```javascript
import { LockchainWriter } from 'unrdf';

const lockchain = new LockchainWriter({
  repoPath: './audit-trail',
  enableMerkle: true
});

await lockchain.init();

// Every transaction creates a cryptographically signed receipt
const receipt = await lockchain.writeReceipt({
  actor: 'alice@example.org',
  action: 'add-user',
  delta: { additions: [/* quads */], removals: [] },
  timestamp: new Date(),
  metadata: { reason: 'User registration' }
});

// Receipt includes SHA3-256 Merkle root for tamper detection
console.log(receipt.merkleRoot); // "abc123..."
```

## Key Features

### 1. **Complete RDF Operations** 📚

- **Parsing**: Turtle, N-Triples, N-Quads, JSON-LD
- **Serialization**: Convert between any RDF format
- **SPARQL**: Full SPARQL 1.1 query support (SELECT, ASK, CONSTRUCT, DESCRIBE)
- **SHACL**: Shape-based validation
- **N3 Reasoning**: Rule-based inference

### 2. **Knowledge Hooks System** 🪝

Autonomic, policy-driven triggers that react to graph changes:

- `sparql-ask` - Boolean condition checking
- `shacl` - Shape validation
- `delta` - Change pattern detection
- `threshold` - Numeric comparisons
- `count` - Cardinality constraints
- `window` - Time-based aggregations

### 3. **ACID Transactions** 🔒

Full ACID guarantees with rollback support:

```javascript
try {
  await system.executeTransaction({
    additions: [quad(s, p, o)],
    removals: [],
    actor: 'user@example.org'
  });
} catch (error) {
  // Transaction automatically rolled back
  console.error('Transaction failed:', error);
}
```

### 4. **OpenTelemetry Observability** 📊

Production-grade instrumentation out of the box:

- Distributed tracing with spans
- Performance metrics (latency, throughput, cache hit rate)
- Error tracking and analysis
- Custom instrumentation support

### 5. **Performance Optimizations** ⚡

Dark Matter 80/20 framework delivers:

- **50% faster** hook execution via batching
- **60% faster** queries via LRU caching
- **20% faster** transactions via optimized paths
- <100ms p95 latency for core operations

### 6. **Security by Default** 🛡️

- Effect sandboxing for hook execution
- SHA3-256 Merkle tree verification
- Git-based immutable audit logs
- Input validation with Zod schemas
- Safe SPARQL with timeout and complexity limits

## What Makes UNRDF Different?

### vs. Traditional RDF Libraries

| Feature | Traditional RDF | UNRDF |
|---------|----------------|-------|
| Data Storage | ✅ | ✅ |
| SPARQL Queries | ✅ | ✅ |
| SHACL Validation | Sometimes | ✅ |
| **Autonomic Behavior** | ❌ | ✅ Knowledge Hooks |
| **Cryptographic Audit** | ❌ | ✅ Lockchain + Merkle |
| **Performance Optimization** | Basic | ✅ Dark Matter 80/20 |
| **Observability** | Manual | ✅ Built-in OTEL |
| **Production Ready** | DIY | ✅ Batteries Included |

### Real-World Example

**Traditional approach** (100+ lines of code):
```javascript
// Manually validate data
// Manually log changes
// Manually check business rules
// Manually handle errors
// Manually optimize queries
// Manually instrument metrics
```

**UNRDF approach** (5 lines of code):
```javascript
const system = await createDarkMatterCore();
await registerHook(dataQualityHook);
await system.executeTransaction({ additions, removals, actor });
const results = await system.query({ query, type: 'sparql-select' });
await system.cleanup();
```

## Use Cases

### Knowledge Graph Applications

- **Semantic data platforms** - Store and query linked data
- **Data integration** - Merge data from multiple sources
- **Ontology management** - Version and govern vocabularies
- **Triple stores** - Scalable RDF storage and retrieval

### Policy-Driven Systems

- **Compliance enforcement** - Ensure regulatory requirements
- **Data quality gates** - Maintain data integrity invariants
- **Business rule automation** - Encode domain logic as hooks
- **Audit and provenance** - Track all changes cryptographically

### AI and Machine Learning

- **Knowledge bases** - Structured knowledge for AI systems
- **Feature engineering** - Extract features from knowledge graphs
- **Explainable AI** - Provide semantic context for decisions
- **Data lineage** - Track provenance for ML pipelines

## Architecture Overview

```
┌─────────────────────────────────────┐
│      Dark Matter 80/20 Core         │
│  (Performance-Optimized Critical)   │
└──────────────┬──────────────────────┘
               │
    ┌──────────┴──────────┐
    │                     │
┌───▼────────┐    ┌──────▼─────────┐
│ Knowledge  │    │  Transaction   │
│   Hooks    │◄───┤    Manager     │
└────┬───────┘    └────────────────┘
     │
┌────▼──────────────────────────────┐
│      Knowledge Engine Core        │
├───────────────────────────────────┤
│ • SPARQL (Comunica)               │
│ • SHACL (rdf-validate-shacl)      │
│ • N3 Reasoning                    │
│ • RDF Store (N3.js)               │
└───────────────────────────────────┘
```

## Performance Benchmarks

| Operation | Baseline | Optimized | Improvement |
|-----------|----------|-----------|-------------|
| Hook execution (independent) | 200ms | 100ms | **50% faster** |
| Query optimization | 500ms | 200ms | **60% faster** |
| Transaction commit | 150ms | 120ms | **20% faster** |

**Production Metrics:**
- ✅ <100ms p95 hook execution latency
- ✅ <500ms p95 query execution latency
- ✅ <500ms p95 transaction commit latency
- ✅ 50%+ cache hit rate after warmup

## Getting Started

Ready to build your first knowledge graph? Head to the [Quick Start](getting-started/quick-start.md) guide to create your first UNRDF application in under 5 minutes.

### What's Next?

- **[Quick Start](getting-started/quick-start.md)** - Build your first app in 5 minutes
- **[Installation](getting-started/installation.md)** - Detailed setup instructions
- **[Basic Usage](getting-started/basic-usage.md)** - Learn core concepts
- **[First Hook](getting-started/first-hook.md)** - Create your first Knowledge Hook

## Philosophy

UNRDF is built on three core principles:

1. **Composability** - Use only what you need, when you need it
2. **Correctness** - Runtime validation with Zod, not TypeScript illusions
3. **Production-First** - Batteries included: observability, security, performance

> "Good code is written for humans to read, and only incidentally for machines to execute."
>
> — Harold Abelson

UNRDF is designed to be **readable**, **maintainable**, and **correct** - in that order.

## License

MIT License - see [LICENSE](https://github.com/unrdf/unrdf/blob/main/LICENSE) for details.

---

**Let's build intelligent knowledge graphs together.**
