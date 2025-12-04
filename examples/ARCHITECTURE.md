# UNRDF Architecture Overview

**How all the pieces fit together.**

## System Architecture (ASCII)

```
┌─────────────────────────────────────────────────────────────────┐
│                        Application Layer                         │
│  (Your code using UNRDF - CLI, web apps, agents, APIs)          │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Knowledge Substrate Core                      │
│  createKnowledgeSubstrateCore() - Main entry point              │
│  • Parse, Query, Export                                          │
│  • Component registry                                            │
│  • Lifecycle management                                          │
└─────────────────────────────────────────────────────────────────┘
                              │
          ┌───────────────────┼───────────────────┐
          ▼                   ▼                   ▼
┌──────────────────┐ ┌──────────────────┐ ┌──────────────────┐
│   @unrdf/core    │ │  @unrdf/hooks    │ │ @unrdf/dark-matter│
│                  │ │                  │ │                  │
│ • RDF parsing    │ │ • Hook registry  │ │ • Query optimizer│
│ • SPARQL engine  │ │ • Execution      │ │ • Index manager  │
│ • Serialization  │ │ • Validation     │ │ • Performance    │
│ • Store ops      │ │ • Transformation │ │ • Caching        │
└──────────────────┘ └──────────────────┘ └──────────────────┘
          │                   │                   │
          └───────────────────┼───────────────────┘
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                      Cross-cutting Concerns                      │
├─────────────────────────────────────────────────────────────────┤
│  @unrdf/streaming  │  Change feeds, real-time updates           │
│  @unrdf/federation │  Distributed queries, remote stores        │
│  @unrdf/browser    │  IndexedDB, client-side persistence        │
│  @unrdf/cli        │  Command-line interface                    │
│  @unrdf/composables│  React/Vue/Svelte hooks                    │
└─────────────────────────────────────────────────────────────────┘
```

## Data Flow Diagram

```
User Input (RDF data)
         │
         ▼
   ┌──────────┐
   │  Parser  │  parseTurtle() / parseJSONLD()
   └──────────┘
         │
         ▼
   ┌──────────┐
   │   Store  │  N3.Store (in-memory graph)
   └──────────┘
         │
         ▼
   ┌──────────┐
   │  Hooks   │  Pre-write validation, transformation
   └──────────┘
         │
         ▼
   ┌──────────┐
   │  Query   │  SPARQL execution (Comunica)
   └──────────┘
         │
         ▼
   ┌──────────┐
   │ Results  │  Bindings (Map objects)
   └──────────┘
         │
         ▼
   ┌──────────┐
   │ Export   │  Turtle / JSON-LD / N-Triples
   └──────────┘
         │
         ▼
User Output (structured data)
```

## Component Interaction Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                     Full-Stack Example Flow                      │
└─────────────────────────────────────────────────────────────────┘

1. User submits RDF data (Turtle)
         │
         ▼
2. CLI parses and validates
   (@unrdf/cli → @unrdf/core)
         │
         ▼
3. Hooks execute validation rules
   (@unrdf/hooks → defineHook → check())
         │
         ├─ Valid? → Continue
         └─ Invalid? → Reject with error
         │
         ▼
4. Store persists data
   (@unrdf/core → N3.Store)
         │
         ▼
5. Dark Matter optimizes query
   (@unrdf/dark-matter → optimize())
         │
         ▼
6. SPARQL query executes
   (@unrdf/core → Comunica)
         │
         ▼
7. Results serialized
   (@unrdf/core → exportJSONLD())
         │
         ▼
8. Real-time updates broadcast
   (@unrdf/streaming → ChangeStream)
         │
         ▼
9. Browser updates UI
   (@unrdf/browser → IndexedDB sync)
         │
         ▼
10. User sees results
```

## Package Dependencies

```
@unrdf/core (foundation)
    │
    ├─→ @unrdf/hooks (depends on core)
    │       │
    │       └─→ @unrdf/knowledge-engine (depends on hooks)
    │
    ├─→ @unrdf/dark-matter (depends on core)
    │
    ├─→ @unrdf/streaming (depends on core)
    │       │
    │       └─→ @unrdf/browser (depends on streaming)
    │
    ├─→ @unrdf/federation (depends on core)
    │
    ├─→ @unrdf/cli (depends on core, hooks, dark-matter)
    │
    └─→ @unrdf/composables (depends on core, streaming)
```

## Key Architectural Patterns

### 1. Component Registry Pattern

```javascript
// Core maintains a registry of all components
const core = await createKnowledgeSubstrateCore()

// Components are lazily initialized
const txManager = core.getComponent('TransactionManager')
const hookExecutor = core.getComponent('HookExecutor')

// Components communicate via well-defined interfaces
```

**Why:** Loose coupling, testability, extensibility.

### 2. Hook Pipeline Pattern

```javascript
// Hooks are composed into a pipeline
defineHook('validate', { type: 'validate-before-write', check })
defineHook('transform', { type: 'transform-after-validate', transform })
defineHook('audit', { type: 'audit-after-write', log })

// Pipeline executes in order
quad → validate → transform → store → audit
```

**Why:** Separation of concerns, reusable policies.

### 3. Streaming Parser Pattern

```javascript
// Don't load entire file into memory
const parser = new StreamingParser()

parser.on('quad', (quad) => {
  // Process one quad at a time
  store.addQuad(quad)
})

await parser.parse('huge-file.ttl')
```

**Why:** Memory efficiency, scalability.

### 4. Lazy Initialization Pattern

```javascript
// Components are only created when needed
core.getComponent('TransactionManager') // Created on first access

// Cleanup only destroys what was created
await core.cleanup() // Safe even if some components unused
```

**Why:** Performance, resource efficiency.

## Integration Architecture

### CLI Integration

```
User runs: unrdf query data.ttl "SELECT * WHERE { ?s ?p ?o }"
         │
         ▼
    ┌─────────┐
    │   CLI   │  @unrdf/cli
    └─────────┘
         │
         ▼
    ┌─────────┐
    │  Core   │  createKnowledgeSubstrateCore()
    └─────────┘
         │
         ▼
    ┌─────────┐
    │ Results │  Formatted output
    └─────────┘
```

### Browser Integration

```
User loads web app
         │
         ▼
    ┌─────────────┐
    │  IndexedDB  │  @unrdf/browser
    └─────────────┘
         │
         ▼
    ┌─────────────┐
    │   Store     │  Persistent RDF graph
    └─────────────┘
         │
         ▼
    ┌─────────────┐
    │   Sync      │  @unrdf/streaming (ChangeStream)
    └─────────────┘
         │
         ▼
    ┌─────────────┐
    │   Server    │  Remote federation
    └─────────────┘
```

### Federated Queries

```
User queries multiple stores
         │
         ▼
    ┌──────────────┐
    │  Federation  │  @unrdf/federation
    └──────────────┘
         │
    ┌────┴────┐
    ▼         ▼
┌────────┐ ┌────────┐
│ Store1 │ │ Store2 │  Parallel query execution
└────────┘ └────────┘
    │         │
    └────┬────┘
         ▼
    ┌──────────┐
    │ Merge    │  Results combined
    └──────────┘
```

## Performance Optimizations

### 1. Dark Matter Query Optimization

```
Original Query:
  SELECT ?person ?friend ?location WHERE {
    ?person foaf:knows ?friend .
    ?friend foaf:based_near ?location .
    ?location geo:lat ?lat .
  }

Dark Matter Optimized:
  1. Push down filters (lat constraints first)
  2. Reorder joins (smallest result set first)
  3. Use indexes (geo:lat indexed)
  4. Cache intermediate results
```

**Result:** 5-10x faster query execution.

### 2. Streaming Ingestion

```
Without streaming:
  1. Read entire file → OOM error (10GB file)

With streaming:
  1. Read 1 quad → Process → Discard
  2. Read 1 quad → Process → Discard
  3. ... (Constant memory usage)
```

**Result:** Process unlimited file sizes.

### 3. Federated Query Parallelization

```
Sequential:
  Query Store1 (2s) → Query Store2 (3s) → Total: 5s

Parallel:
  Query Store1 (2s) ┐
                    ├→ Merge → Total: 3s
  Query Store2 (3s) ┘
```

**Result:** Near-linear speedup with store count.

## Security Architecture

### 1. Hook-Based Validation

```
User data → Pre-write hooks → Validation → Store
                 │
                 └─ Invalid? → Reject (no data stored)
```

**Protection:** No malicious data enters the store.

### 2. Sandboxed Execution

```
Hook execution → Isolated context (no filesystem access)
                 │
                 └─ Errors caught and logged
```

**Protection:** Malicious hooks cannot damage system.

### 3. Cryptographic Provenance (Lockchain)

```
Quad written → Hash computed → Signature created → Audit log
                 │
                 └─ Tampering detected → Reject change
```

**Protection:** All changes are immutable and verifiable.

## Scalability Considerations

| Component | Scalability Strategy | Limits |
|-----------|---------------------|--------|
| **Core** | In-memory store | ~1M triples |
| **Dark Matter** | Indexes + caching | ~10M triples |
| **Federation** | Distributed queries | Unlimited (horizontal scaling) |
| **Streaming** | Chunked processing | Unlimited (constant memory) |
| **Browser** | IndexedDB | ~100MB (browser limit) |

## Extension Points

1. **Custom hooks** - Add validation, transformation, audit logic
2. **Custom parsers** - Support new RDF formats
3. **Custom stores** - Integrate external databases
4. **Custom optimizers** - Implement domain-specific query optimization
5. **Custom federation** - Connect to proprietary data sources

## Examples That Demonstrate Each Layer

| Layer | Example | File |
|-------|---------|------|
| **Core parsing** | Parse Turtle and query | [01-minimal-parse-query.mjs](./01-minimal-parse-query.mjs) |
| **Hooks** | Validation and transformation | [basic-knowledge-hook.mjs](./basic-knowledge-hook.mjs) |
| **Dark Matter** | Query optimization | [dark-matter-80-20.mjs](./dark-matter-80-20.mjs) |
| **Lockchain** | Audit trails | [lockchain-demo.mjs](./lockchain-demo.mjs) |
| **Policy Packs** | Governance | [policy-pack-demo.mjs](./policy-pack-demo.mjs) |
| **Streaming** | Real-time updates | [examples/streaming/](./streaming/) |
| **Browser** | Client-side storage | [examples/browser/](./browser/) |
| **CLI** | Command-line usage | [cli-automation-script.mjs](./cli-automation-script.mjs) |

---

**Next:** See [examples/README.md](./README.md) for code examples of each component.
