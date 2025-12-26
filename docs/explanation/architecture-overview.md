# Architecture Overview

Understanding the design philosophy, structure, and decision-making behind UNRDF.

## The Big Picture

UNRDF is not just another RDF library - it's a fundamental rethinking of how developers should work with knowledge graphs. While most RDF libraries focus on *implementing specifications*, UNRDF focuses on *eliminating complexity*. The architecture reflects a single driving principle: **eliminate the 80% of "dark matter" code that connects RDF specifications to real applications**.

Traditional RDF development follows a pattern: use a triple store, write SPARQL queries, parse/serialize formats, validate with SHACL, reason with rules. Each step requires glue code - the "dark matter" that doesn't appear in specifications but dominates codebases. UNRDF's architecture systematically eliminates this dark matter through:

1. **Substrate thinking** - Treat RDF as infrastructure, not application code
2. **Declarative composition** - Define what you want, not how to get it
3. **80/20 optimization** - Focus on the 20% that delivers 80% of value
4. **Observable operations** - Every operation is traceable and measurable

## What Is UNRDF?

UNRDF is a **knowledge graph substrate platform** - a foundation layer that makes RDF development as ergonomic as working with JSON or SQL databases. It's built as a monorepo containing multiple specialized packages:

```
┌─────────────────────────────────────────────────────────────────┐
│                    UNRDF Monorepo Structure                     │
│                                                                 │
│  Foundation Layer (No Dependencies)                            │
│  ┌────────────────────────────────────────────────────────┐    │
│  │  @unrdf/oxigraph                                       │    │
│  │  - Oxigraph WASM bindings                              │    │
│  │  - High-performance SPARQL engine                      │    │
│  │  - Production-grade graph database                     │    │
│  └────────────────────────────────────────────────────────┘    │
│                            │                                    │
│                            ▼                                    │
│  Core Layer (Depends on oxigraph)                              │
│  ┌────────────────────────────────────────────────────────┐    │
│  │  @unrdf/core                                           │    │
│  │  - RDF operations (parse, serialize, validate)         │    │
│  │  - SPARQL query execution                              │    │
│  │  - N3.js integration (streaming fallback)              │    │
│  │  - Zod validation schemas                              │    │
│  └────────────────────────────────────────────────────────┘    │
│                            │                                    │
│               ┌────────────┼────────────┐                       │
│               │            │            │                       │
│               ▼            ▼            ▼                       │
│  Extension Layer (Depends on core)                             │
│  ┌───────────┐  ┌───────────┐  ┌──────────────────┐           │
│  │ @unrdf/   │  │ @unrdf/   │  │ @unrdf/          │           │
│  │ hooks     │  │ streaming │  │ knowledge-engine │           │
│  │           │  │           │  │                  │           │
│  │ - Policy  │  │ - Async   │  │ - Reasoning      │           │
│  │ - Triggers│  │ - Streams │  │ - AI Search      │           │
│  │ - Audit   │  │ - Backprs │  │ - Inference      │           │
│  └───────────┘  └───────────┘  └──────────────────┘           │
│                                                                 │
│  Integration Layer                                             │
│  ┌────────────────────────────────────────────────────────┐    │
│  │  @unrdf/federation  @unrdf/composables  @unrdf/cli    │    │
│  │  @unrdf/dark-matter @unrdf/yawl-*                      │    │
│  └────────────────────────────────────────────────────────┘    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

This layered architecture enables:
- **Minimal installs** - Use only what you need
- **Clear dependencies** - No circular references
- **Progressive complexity** - Start simple, add features as needed
- **Framework agnostic** - Core is pure JavaScript

## How It Works

### The Dual-Store Strategy

UNRDF uses *two* RDF stores, not one. This might seem redundant, but it's a deliberate architectural choice that solves multiple problems simultaneously:

```
┌─────────────────────────────────────────────────────────────────┐
│                    Dual-Store Architecture                      │
│                                                                 │
│  ┌─────────────────────────┐      ┌─────────────────────────┐  │
│  │     N3.js Store         │      │   Oxigraph Store        │  │
│  │                         │      │                         │  │
│  │  Use Case:              │      │  Use Case:              │  │
│  │  - Streaming parsing    │      │  - Production queries   │  │
│  │  - In-memory operations │      │  - Complex SPARQL       │  │
│  │  - JavaScript ecosystem │      │  - High performance     │  │
│  │  - Small graphs         │      │  - Large graphs         │  │
│  │                         │      │                         │  │
│  │  Pros:                  │      │  Pros:                  │  │
│  │  - 100% JavaScript      │      │  - Native performance   │  │
│  │  - Easy debugging       │      │  - Full SPARQL 1.1      │  │
│  │  - Streaming support    │      │  - Persistent storage   │  │
│  │                         │      │                         │  │
│  │  Cons:                  │      │  Cons:                  │  │
│  │  - Limited SPARQL       │      │  - WASM overhead        │  │
│  │  - Slower queries       │      │  - Harder to debug      │  │
│  └─────────────────────────┘      └─────────────────────────┘  │
│              │                              │                   │
│              └──────────┬───────────────────┘                   │
│                         │                                       │
│                         ▼                                       │
│              Unified API (@unrdf/core)                         │
│              - User sees one interface                         │
│              - Auto-selects best store                         │
│              - Converts between stores                         │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Why both?**

- **N3.js** provides streaming and JavaScript-native operations
- **Oxigraph** provides production-grade query performance
- Together, they cover 100% of use cases without compromise

This isn't duplication - it's *strategic redundancy*. Users never need to know which store is being used; the architecture makes the optimal choice automatically.

### Data Flow Philosophy

UNRDF follows a **unidirectional data flow** pattern inspired by reactive programming:

```
Input (RDF Data)
    │
    ▼
┌─────────────────┐
│  Parse          │  Transform external formats → internal graph
│  (@unrdf/core)  │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Store          │  Hold graph in memory or disk
│  (N3/Oxigraph)  │
└────────┬────────┘
         │
         ├─────────────┬─────────────┬─────────────┐
         │             │             │             │
         ▼             ▼             ▼             ▼
    ┌────────┐   ┌────────┐   ┌────────┐   ┌────────┐
    │ Query  │   │Validate│   │ Reason │   │ Hooks  │
    │(SPARQL)│   │(SHACL) │   │ (N3)   │   │(Policy)│
    └────────┘   └────────┘   └────────┘   └────────┘
         │             │             │             │
         └─────────────┴─────────────┴─────────────┘
                       │
                       ▼
                  Results
                  (Bindings, Reports, Graphs)
                       │
                       ▼
                  Serialize
                  (Turtle, JSON-LD, N-Quads)
                       │
                       ▼
                  Output
```

**Key principles:**

1. **Immutability** - Stores don't mutate; operations return new stores
2. **Composition** - Operations chain naturally
3. **Observability** - Every step creates OpenTelemetry spans
4. **Type safety** - Zod validates inputs and outputs

This flow makes debugging trivial: inspect the data at any step, examine the telemetry spans, and trace exactly what happened.

### The Knowledge Hooks System

Knowledge Hooks are UNRDF's answer to "How do we make RDF graphs *reactive*?" Traditional RDF is passive - you query it, it responds. Knowledge Hooks make graphs *active* - they observe changes and trigger actions.

```
┌─────────────────────────────────────────────────────────────────┐
│              Knowledge Hook Execution Model                     │
│                                                                 │
│  Graph Changes                                                  │
│       │                                                         │
│       ▼                                                         │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  Channel Filter                                         │   │
│  │  - Watch specific named graphs                          │   │
│  │  - Delta view (what changed?)                           │   │
│  └────────────────────────┬────────────────────────────────┘   │
│                           │                                     │
│                           ▼                                     │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  Condition Evaluation                                   │   │
│  │  - SPARQL ASK (boolean)                                 │   │
│  │  - SPARQL SELECT (bindings)                             │   │
│  │  - SHACL validation                                     │   │
│  │  - Content-addressed (SHA-256 integrity)                │   │
│  └────────────────────────┬────────────────────────────────┘   │
│                           │                                     │
│                      Condition true?                            │
│                           │                                     │
│              ┌────────────┴────────────┐                        │
│              │ No                      │ Yes                    │
│              ▼                         ▼                        │
│         Skip hook              ┌──────────────┐                │
│                                │  before()    │  Optional gate  │
│                                └──────┬───────┘                │
│                                       │                         │
│                                  Gate passes?                   │
│                                       │                         │
│                                       ▼                         │
│                                ┌──────────────┐                │
│                                │   run()      │  Main action   │
│                                └──────┬───────┘                │
│                                       │                         │
│                                       ▼                         │
│                                ┌──────────────┐                │
│                                │  after()     │  Cleanup        │
│                                └──────┬───────┘                │
│                                       │                         │
│                                       ▼                         │
│                                ┌──────────────┐                │
│                                │  Receipt     │  Audit trail   │
│                                │ (git-notes)  │                │
│                                └──────────────┘                │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Why this architecture?**

- **Content-addressed conditions** - SHA-256 prevents tampering
- **Lifecycle hooks** - before/run/after pattern enables composition
- **Receipt anchoring** - Audit trails for compliance
- **Determinism support** - Reproducible execution with seeds
- **Declarative** - Define what triggers, not how to poll

This turns RDF graphs into *event-driven systems*. When data matches a pattern, code runs automatically. No polling, no manual triggers, no glue code.

## Why This Design?

### JavaScript Over TypeScript

UNRDF source code is **JavaScript + JSDoc**, not TypeScript. This is a deliberate architectural choice based on three insights:

**1. Runtime validation > Compile-time types**

```javascript
// TypeScript approach
interface StoreConfig {
  backend: 'memory' | 'oxigraph';
  maxSize: number;
}

// UNRDF approach
const StoreConfigSchema = z.object({
  backend: z.enum(['memory', 'oxigraph']),
  maxSize: z.number().int().positive()
});
```

TypeScript types disappear at runtime. Zod schemas validate at runtime *and* provide type inference. In a library that processes untrusted RDF data from the web, runtime validation is mandatory.

**2. Zero compilation overhead**

JavaScript ESM runs directly in Node.js and browsers. No build step means:
- Faster iteration during development
- Easier debugging (source maps aren't perfect)
- Simpler CI/CD pipelines
- Users can read actual source code

**3. Universal compatibility**

UNRDF runs anywhere JavaScript runs:
- Node.js (server-side)
- Browsers (client-side)
- Deno (alternative runtime)
- Bun (fast runtime)
- Edge functions (Cloudflare Workers, Vercel Edge)

TypeScript would require compilation targets for each environment. JavaScript just works.

**Type safety isn't sacrificed** - JSDoc provides editor autocomplete and type checking. Zod provides runtime guarantees. Together, they exceed TypeScript's safety without the compilation overhead.

### Monorepo Over Multi-Repo

UNRDF uses a **pnpm workspace monorepo** rather than separate repositories for each package. This architecture choice reflects how knowledge graph systems actually work:

**Problems with multi-repo:**
- Dependency hell when packages need synchronized updates
- Circular dependencies become impossible to manage
- Testing integration scenarios requires complex setup
- Documentation becomes fragmented

**Monorepo benefits:**
- Atomic changes across packages
- Shared tooling and configuration
- Integrated testing
- Single source of truth

```
unrdf/
├── packages/
│   ├── oxigraph/        (foundation)
│   ├── core/            (depends: oxigraph)
│   ├── streaming/       (depends: core, oxigraph)
│   ├── knowledge-engine/(depends: core, streaming, oxigraph)
│   ├── hooks/           (depends: core, oxigraph)
│   ├── federation/      (depends: core, streaming)
│   └── ...
├── docs/                (shared documentation)
├── benchmarks/          (cross-package benchmarks)
└── validation/          (OTEL validation)
```

**Workspace protocol** (`workspace:*`) ensures packages always use the latest local version during development, but resolve to specific versions when published to npm.

### The 80/20 Architectural Principle

Every design decision in UNRDF follows the **Pareto principle**: identify the 20% of features that deliver 80% of value, then optimize ruthlessly for those features.

**Applied to APIs:**

Instead of exposing 50 configuration options:
```javascript
// Traditional RDF library - overwhelming
createStore({
  backend: 'oxigraph',
  persistence: true,
  persistencePath: './data',
  maxMemory: 1024 * 1024 * 1024,
  cacheSize: 10000,
  journalMode: 'WAL',
  synchronous: 'NORMAL',
  // ... 40 more options
});
```

UNRDF provides smart defaults:
```javascript
// UNRDF - simple with escape hatch
import { createStore } from '@unrdf/oxigraph';

// 80% use case
const store = createStore();

// Advanced use case - full control available
const customStore = createStore({
  backend: 'oxigraph',
  options: { /* full oxigraph options */ }
});
```

**Applied to React Hooks (40 total):**

```
TIER 1 (5 hooks) → 60% of usage
TIER 2 (2 hooks) → 20% of usage
TIER 3 (9 hooks) → 15% of usage
TIER 4 (24 hooks) → 5% of usage
```

Default imports give you Tier 1 & 2 (80% coverage). Category imports give you Tier 3 & 4 (advanced use cases).

This isn't about limiting features - it's about **progressive disclosure**. Simple things are simple, complex things are possible.

### Content-Addressed Security

Knowledge Hooks use **SHA-256 content addressing** for SPARQL conditions. This design decision prevents an entire class of security vulnerabilities:

```javascript
defineHook({
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'https://example.com/conditions/check-price.rq',
      sha256: 'a3f5e8c2...', // Hash of condition content
      mediaType: 'application/sparql-query'
    }
  },
  run: async (context) => {
    // This only runs if the fetched condition matches the hash
  }
});
```

**Why content-addressing?**

Traditional approach:
```javascript
// UNSAFE - what if the URL content changes?
defineHook({
  condition: 'https://example.com/condition.rq',
  run: dangerousAction
});
```

If an attacker compromises `example.com`, they can change the condition to trigger `dangerousAction` inappropriately.

Content-addressing makes this impossible:
1. Developer specifies expected hash
2. Runtime fetches condition
3. Runtime computes SHA-256 of fetched content
4. If hash mismatch → security error
5. If hash matches → safe to execute

This pattern comes from distributed systems (IPFS, Git, Blockchain) and brings the same integrity guarantees to knowledge graphs.

## Performance Considerations

### Query Performance Strategy

UNRDF uses **adaptive query routing** - simple queries use N3.js, complex queries use Oxigraph:

```javascript
// Simple triple pattern - N3.js is faster (pure JavaScript)
store.query('SELECT ?s WHERE { ?s rdf:type foaf:Person }');

// Complex query with aggregates - Oxigraph is faster (native code)
store.query(`
  SELECT ?category (COUNT(?item) as ?count)
  WHERE { ?item rdf:type ?category }
  GROUP BY ?category
  HAVING (?count > 10)
  ORDER BY DESC(?count)
`);
```

**Heuristics for routing:**
- Triple patterns → N3.js
- UNION, OPTIONAL, FILTER → Oxigraph
- Aggregates (COUNT, SUM, AVG) → Oxigraph
- ORDER BY, GROUP BY → Oxigraph
- Simple graph traversal → N3.js

This gives the best of both worlds without forcing users to choose.

### Memory Management

UNRDF stores are **immutable by default** but provide **mutable modes** for bulk loading:

```javascript
// Immutable - safe, memory-efficient for small changes
const store1 = createStore();
const store2 = store1.add(triple1);
const store3 = store2.add(triple2);

// Mutable - faster for bulk loading
const store = createStore({ mutable: true });
for (const triple of millionsOfTriples) {
  store.add(triple);  // In-place mutation
}
store.freeze();  // Make immutable after loading
```

**Why immutability by default?**

- Prevents accidental mutations
- Enables structural sharing (memory efficiency)
- Makes debugging easier (old states still exist)
- Enables time-travel debugging

**Why mutable mode exists?**

- Bulk loading is 10-100x faster with mutation
- Some use cases need maximum performance
- Progressive enhancement: start mutable, freeze when done

### Streaming Architecture

The `@unrdf/streaming` package uses **backpressure-aware streams** to handle arbitrarily large RDF datasets:

```
┌─────────────────────────────────────────────────────────────────┐
│                  Streaming Data Flow                            │
│                                                                 │
│  HTTP Response                                                  │
│       │                                                         │
│       ▼                                                         │
│  ┌─────────────┐                                                │
│  │ N3 Parser   │  Emits quads as they're parsed                │
│  │ (streaming) │  Not buffered in memory                       │
│  └──────┬──────┘                                                │
│         │                                                       │
│         ▼                                                       │
│  ┌─────────────┐                                                │
│  │ Transform   │  Filter, map, validate quads                  │
│  │ Stream      │  Backpressure propagates upstream             │
│  └──────┬──────┘                                                │
│         │                                                       │
│         ▼                                                       │
│  ┌─────────────┐                                                │
│  │ Store Sink  │  Writes to store in batches                   │
│  │             │  Pauses upstream if store is slow             │
│  └──────┬──────┘                                                │
│         │                                                       │
│         ▼                                                       │
│   Store (Complete)                                              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Key insight:** You can process a 10GB RDF file with only 100MB of RAM because data flows through the system rather than being buffered.

### OpenTelemetry Overhead

Every operation creates OpenTelemetry spans. This adds ~1-5% performance overhead, which is **intentionally accepted** because:

1. **Production visibility** - Can't optimize what you can't measure
2. **Debugging speed** - Finding bugs 10x faster compensates for 5% slower execution
3. **Accountability** - OTEL spans prove operations completed correctly

The architecture makes OTEL optional for performance-critical code paths:

```javascript
// OTEL enabled (default)
const result = await query(store, sparql);

// OTEL disabled (maximum performance)
const result = await query(store, sparql, { telemetry: false });
```

Most code should use OTEL. The option exists for the <1% of hot paths where microseconds matter.

## Design Patterns Applied

### FMEA (Failure Mode and Effects Analysis)

Every component anticipates failure modes:

| Component | Failure Mode | Detection | Mitigation |
|-----------|--------------|-----------|------------|
| Parse | Malformed RDF | Zod validation | Detailed error messages with line numbers |
| Query | SPARQL timeout | Query engine timeout | Configurable limits, cancel tokens |
| Validate | Invalid SHACL shapes | Shape parsing errors | Schema versioning, CI validation |
| Hooks | Condition tampering | SHA-256 mismatch | Fail-secure (deny execution) |
| Streaming | Backpressure overflow | Stream pause events | Automatic backpressure propagation |
| Store | Out of memory | Process monitoring | Streaming mode for large datasets |

### TRIZ (Theory of Inventive Problem Solving)

UNRDF applies TRIZ principles to eliminate contradictions:

**Contradiction:** "We want type safety, but we can't afford TypeScript compilation."
**TRIZ Solution (Segmentation):** Separate type-checking (JSDoc) from runtime validation (Zod).

**Contradiction:** "We want fast queries, but we want streaming support."
**TRIZ Solution (Asymmetry):** Use different stores (N3.js for streaming, Oxigraph for queries).

**Contradiction:** "We want reactive graphs, but we don't want polling overhead."
**TRIZ Solution (Prior Action):** Content-addressed conditions checked only when graphs change.

**Contradiction:** "We want to track everything, but observability has overhead."
**TRIZ Solution (Parameter Changes):** Make OTEL optional for hot paths, mandatory for everything else.

### DFLSS (Design for Lean Six Sigma)

Quality targets drive architectural decisions:

| Metric | Target | Architectural Support |
|--------|--------|-----------------------|
| Parse latency | <50ms for 10KB | Streaming parser, minimal buffering |
| Query latency | <100ms for simple SELECT | Adaptive routing (N3/Oxigraph) |
| Test coverage | >80% | Vitest integration, coverage enforcement |
| API stability | Semver compliant | Automated semver checking in CI |
| Documentation | 100% JSDoc coverage | ESLint enforcement, type inference |

These aren't aspirational - they're **measured in CI** and block releases if violated.

## Real-World Analogy

Think of UNRDF like a modern database ORM (e.g., Prisma, Drizzle) but for knowledge graphs:

**Without UNRDF (like raw SQL):**
```javascript
// Manual connection management
const connection = await createConnection({ /* 20 options */ });
const result = await connection.query('SELECT ...');
await connection.close();

// Manual validation
if (typeof result.rows[0].name !== 'string') {
  throw new Error('Invalid data');
}
```

**With UNRDF (like an ORM):**
```javascript
// Automatic connection management
const result = await query(store, 'SELECT ...');

// Automatic validation
const validated = ResultSchema.parse(result);
```

UNRDF doesn't hide RDF or SPARQL - it just removes the tedious parts while preserving full control when needed.

## When (and When Not) to Use UNRDF

### UNRDF is excellent for:

- **Knowledge graph applications** - CMS, recommendation engines, data integration
- **Semantic web projects** - FOAF, schema.org, custom ontologies
- **Reactive systems** - Event-driven architectures with Knowledge Hooks
- **JavaScript ecosystems** - Node.js, browsers, edge functions
- **Teams that value velocity** - 80/20 defaults, minimal configuration
- **Production systems** - OTEL observability, proven patterns

### UNRDF is NOT suitable for:

- **Pure research** - If you need cutting-edge RDF features before standardization
- **Non-JavaScript** - If your stack is Java, Python, Rust (use native RDF libs)
- **Tiny bundles** - If every kilobyte matters (UNRDF is ~200KB minified)
- **Maximum control** - If you want to configure every detail of the RDF engine
- **Pure triple stores** - If you just need storage without query/validation

UNRDF optimizes for **developer experience** and **production reliability**, not academic purity or minimal bundle size.

## Related Concepts

### Package Dependency Graph

Understanding how packages relate:

```
                    @unrdf/oxigraph
                    (Foundation)
                          │
                          ▼
                    @unrdf/core
                    (RDF Operations)
                          │
            ┌─────────────┼─────────────┐
            │             │             │
            ▼             ▼             ▼
      @unrdf/hooks  @unrdf/streaming  @unrdf/knowledge-engine
      (Reactive)    (Async I/O)       (Inference & AI)
            │             │             │
            └─────────────┼─────────────┘
                          │
                          ▼
              Integration Packages
              @unrdf/federation
              @unrdf/composables
              @unrdf/dark-matter
              @unrdf/yawl-*
```

Each layer depends only on layers below it. No circular dependencies.

### Extension Points

UNRDF is designed for extension without modification:

1. **Custom stores** - Implement the store interface
2. **Custom parsers** - Register new RDF formats
3. **Custom hooks** - Define domain-specific triggers
4. **Custom validators** - Add validation beyond SHACL
5. **Custom backends** - Swap Oxigraph for another engine

The architecture uses **dependency inversion** - core abstractions don't depend on implementations.

## Migration Path

UNRDF v5 represents a complete architectural rethinking from v4. The migration path reflects our learnings:

**v4 Architecture (Monolithic):**
- Single package
- Tight coupling
- Limited extensibility
- TypeScript compilation required

**v5 Architecture (Modular):**
- Multiple specialized packages
- Clear dependency boundaries
- Extension-friendly
- JavaScript-native

If you're migrating from v4, read [docs/reference/migration-guide.md](../reference/migration-guide.md).

If you're migrating from other RDF libraries (rdflib.js, N3.js, Comunica), the architecture is intentionally compatible - you can adopt UNRDF incrementally.

## See Also

- [System Design Explanation](./system-design.md) - Deep dive into design rationale
- [Knowledge Hooks Architecture](./knowledge-hooks-architecture.md) - Reactive system details
- [RDF & SPARQL Concepts](./rdf-sparql-concepts.md) - Foundational RDF knowledge
- [API Reference](../reference/api-reference.md) - Complete API documentation
- [Getting Started Tutorial](../../README.md) - Hands-on introduction
- [Big Bang 80/20 Methodology](../bb80-20-methodology.md) - Development philosophy
