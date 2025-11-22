# Understanding UNRDF System Design

Deep explanation of UNRDF's architecture, design decisions, and implementation strategies.

## Design Philosophy

### The 80/20 Principle

UNRDF eliminates **80% of RDF "dark matter"** - the boilerplate code developers typically write:

```
Traditional RDF Development:
┌─────────────────────────────────────────────────────────────────┐
│  20% Business Logic   │      80% Glue Code ("Dark Matter")     │
│                       │                                         │
│  - Domain models      │  - Store setup and configuration        │
│  - Business rules     │  - Parser initialization                │
│  - Core algorithms    │  - Query engine wiring                  │
│                       │  - Validation setup                     │
│                       │  - Serialization handling               │
│                       │  - Error handling boilerplate           │
│                       │  - Event system implementation          │
│                       │  - Audit trail infrastructure          │
└─────────────────────────────────────────────────────────────────┘

UNRDF Development:
┌─────────────────────────────────────────────────────────────────┐
│  80%+ Business Logic  │  20% UNRDF Configuration                │
│                       │                                         │
│  - Domain models      │  - Import UNRDF                        │
│  - Business rules     │  - Configure engine                    │
│  - Core algorithms    │  - Define hooks                        │
│  - Knowledge Hooks    │                                         │
│  - Validation rules   │                                         │
└─────────────────────────────────────────────────────────────────┘
```

### Opinionated Defaults

Rather than offering multiple options, UNRDF makes **one choice** for each concern:

| Concern | UNRDF Choice | Alternatives Not Chosen | Rationale |
|---------|--------------|------------------------|-----------|
| RDF Store | N3.Store | rdflib, rdf-ext, graphy | Fast, memory-efficient, well-maintained |
| Query Engine | Comunica | SPARQL.js, custom | Standards-compliant, performant |
| Validation | SHACL | ShEx, custom | W3C standard, comprehensive |
| Reasoning | EYE | OWL reasoners | N3 rules, fast, reliable |
| Canonization | URDNA2015 | Custom | W3C standard, deterministic |
| Config | Turtle | JSON, YAML | RDF-native, self-describing |

### No TypeScript Policy

UNRDF uses **JSDoc for types** and **Zod for runtime validation**:

```javascript
/**
 * Parse Turtle format RDF.
 * @param {string} turtle - Turtle format data
 * @param {string} [baseIRI] - Base IRI for relative IRIs
 * @returns {Promise<N3.Store>} Parsed RDF store
 */
export async function parseTurtle(turtle, baseIRI) {
  // Runtime validation with Zod
  const validated = TurtleInputSchema.parse({ turtle, baseIRI });
  // ...implementation
}
```

**Why no TypeScript?**

- TypeScript types are **compile-time only** - they disappear at runtime
- Zod provides **runtime validation** at the boundaries that matter
- JSDoc gives **IDE support** without compilation complexity
- Simpler build toolchain (no tsc, no source maps)

## Module Architecture

```
unrdf/
├── src/
│   ├── index.mjs                 # Main entry point
│   ├── knowledge-engine/         # Core RDF operations
│   │   ├── index.mjs            # Engine exports
│   │   ├── parse.mjs            # Parsing & serialization
│   │   ├── query.mjs            # SPARQL queries
│   │   ├── validate.mjs         # SHACL validation
│   │   ├── reason.mjs           # N3 reasoning
│   │   ├── canonicalize.mjs     # URDNA2015
│   │   ├── define-hook.mjs      # Knowledge Hooks
│   │   ├── schemas.mjs          # Zod schemas
│   │   └── transaction.mjs      # Transaction management
│   │
│   ├── react-hooks/              # React integration
│   │   ├── index.mjs            # Hook exports (40 hooks)
│   │   ├── core/                # Essential hooks
│   │   ├── streaming/           # Real-time updates
│   │   ├── federation/          # Distributed queries
│   │   └── ...                  # Other categories
│   │
│   ├── composables/              # Vue-style composables
│   │   ├── use-graph.mjs
│   │   ├── use-turtle.mjs
│   │   └── ...
│   │
│   ├── cli/                      # Command-line interface
│   │   ├── index.mjs
│   │   └── commands/
│   │
│   └── utils/                    # Utility functions
│       ├── quad-utils.mjs
│       ├── term-utils.mjs
│       └── ...
│
├── docs/                         # Documentation
├── test/                         # Test suites
└── examples/                     # Example applications
```

## Layer Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                      Application Layer                          │
│                                                                 │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐            │
│  │ React Hooks │  │ Composables │  │    CLI      │            │
│  │  (40 hooks) │  │ (Vue-style) │  │ (Commands)  │            │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘            │
│         │                │                │                    │
│         └────────────────┴────────────────┘                    │
│                          │                                      │
├──────────────────────────┼──────────────────────────────────────┤
│                    Knowledge Engine                             │
│                          │                                      │
│  ┌────────────┬──────────┴──────────┬────────────┐            │
│  │   Parse    │       Query         │  Validate  │            │
│  │  (N3.js)   │    (Comunica)       │  (SHACL)   │            │
│  └────────────┴─────────────────────┴────────────┘            │
│  ┌────────────┬─────────────────────┬────────────┐            │
│  │   Reason   │    Canonicalize     │   Hooks    │            │
│  │   (EYE)    │    (URDNA2015)      │ (defineHook│            │
│  └────────────┴─────────────────────┴────────────┘            │
│                          │                                      │
├──────────────────────────┼──────────────────────────────────────┤
│                   Foundation Layer                              │
│                          │                                      │
│  ┌─────────────────┬─────┴───────┬─────────────────┐          │
│  │    N3.Store     │  DataFactory │    Utilities    │          │
│  │  (RDF Storage)  │ (Term Create)│  (Helpers)      │          │
│  └─────────────────┴─────────────┴─────────────────┘          │
└─────────────────────────────────────────────────────────────────┘
```

## Data Flow

### Parse → Query → Validate Flow

```
Input (Turtle/JSON-LD)
         │
         ▼
┌─────────────────┐
│   parseTurtle   │────────────────────────────────────────┐
│   parseJsonLd   │                                        │
└────────┬────────┘                                        │
         │                                                 │
         ▼                                                 │
┌─────────────────┐                                        │
│    N3.Store     │◄───────────────────────────────────────┤
│  (RDF Graph)    │                                        │
└────────┬────────┘                                        │
         │                                                 │
         ├─────────────┬─────────────┬─────────────┐      │
         ▼             ▼             ▼             ▼      │
┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌───────────┐
│   select    │ │ validateShacl│ │   reason    │ │ toTurtle │
│   ask       │ │             │ │             │ │ toJsonLd │
│  construct  │ │             │ │             │ │ toNQuads │
└──────┬──────┘ └──────┬──────┘ └──────┬──────┘ └─────┬─────┘
       │               │               │              │
       ▼               ▼               ▼              ▼
   Results        Report          Inferred       Output
  (Bindings)   (Violations)       (Store)       (String)
```

### Knowledge Hook Flow

```
Graph Change
     │
     ▼
┌─────────────────┐
│ Channel Filter  │  ← channel: { graphs, view }
│ (Named Graphs)  │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Condition Check │  ← when: { kind, ref }
│ (SPARQL/SHACL)  │
└────────┬────────┘
         │
    [true/false]
         │
    ┌────┴────┐
    │ true    │ false → Exit
    ▼
┌─────────────────┐
│    before()     │  ← Validation, transformation
└────────┬────────┘
         │
    [payload/cancel]
         │
    ┌────┴────────┐
    │ payload     │ cancel → after({ cancelled: true })
    ▼
┌─────────────────┐
│     run()       │  ← Core business logic
└────────┬────────┘
         │
    [result, assertions]
         │
         ▼
┌─────────────────┐
│    after()      │  ← Cleanup, notification
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Receipt Anchor  │  ← git-notes, audit log
└─────────────────┘
```

## Error Handling Strategy

### Layered Error Types

```javascript
// Layer 1: Validation errors (Zod)
class ValidationError extends Error {
  constructor(zodError) {
    super(`Validation failed: ${zodError.message}`);
    this.issues = zodError.issues;
  }
}

// Layer 2: RDF errors (Parse, Query)
class RDFError extends Error {
  constructor(message, { line, column, context }) {
    super(message);
    this.line = line;
    this.column = column;
    this.context = context;
  }
}

// Layer 3: Business errors (Hooks)
class HookError extends Error {
  constructor(message, { hookId, phase, payload }) {
    super(message);
    this.hookId = hookId;
    this.phase = phase;
    this.payload = payload;
  }
}
```

### Error Recovery Patterns

```javascript
// Graceful degradation
async function safeQuery(store, sparql) {
  try {
    return await select(store, sparql);
  } catch (error) {
    console.warn('Query failed:', error.message);
    return [];  // Return empty results
  }
}

// Retry with backoff
async function resilientParse(turtle, retries = 3) {
  for (let i = 0; i < retries; i++) {
    try {
      return await parseTurtle(turtle);
    } catch (error) {
      if (i === retries - 1) throw error;
      await delay(Math.pow(2, i) * 100);
    }
  }
}
```

## Performance Design

### Caching Strategy

```
┌─────────────────────────────────────────────────────────────┐
│                    Cache Layers                             │
│                                                             │
│  L1: Query Results Cache (LRU, 1000 entries)               │
│      ├── Key: sparql + store.hash                          │
│      └── TTL: 60 seconds                                   │
│                                                             │
│  L2: Parsed Condition Cache (Map)                          │
│      ├── Key: condition.ref.sha256                         │
│      └── TTL: Permanent (content-addressed)                │
│                                                             │
│  L3: Validation Report Cache (WeakMap)                     │
│      ├── Key: store + shapes                               │
│      └── TTL: Until store modified                         │
└─────────────────────────────────────────────────────────────┘
```

### Memory Management

- **N3.Store** uses indexed structures for efficient triple lookup
- **WeakMap** caches tied to store lifetime
- **LRU eviction** for query result caches
- **Streaming parsers** for large files

### Async Operations

All potentially slow operations are async:

```javascript
// Parsing - may involve network for JSON-LD context
const store = await parseTurtle(data);

// Querying - Comunica is async
const results = await select(store, sparql);

// Reasoning - EYE runs in worker
const inferred = await reason(store, rules);
```

## Security Design

### Input Validation

All public APIs validate input with Zod:

```javascript
const TurtleInputSchema = z.object({
  turtle: z.string().min(1),
  baseIRI: z.string().url().optional()
});

export async function parseTurtle(turtle, baseIRI) {
  const input = TurtleInputSchema.parse({ turtle, baseIRI });
  // ... safe to use input
}
```

### Hook Sandboxing

Hook functions execute in isolated contexts:

```javascript
// Hooks cannot access:
// - File system
// - Network (unless explicitly enabled)
// - Global state
// - Other hooks' data

// Hooks CAN access:
// - payload (immutable copy)
// - context.graph (read-only view)
// - context.env (whitelisted variables)
```

### Condition Integrity

SHA-256 verification prevents tampering:

```javascript
async function loadCondition(ref) {
  const content = await readFile(ref.uri);
  const hash = sha256(content);

  if (hash !== ref.sha256) {
    throw new SecurityError('Condition file integrity check failed');
  }

  return parseCondition(content, ref.kind);
}
```

## Testing Strategy

### Test Pyramid

```
                    ┌────────────┐
                    │    E2E     │  ← Full system tests
                    │   Tests    │     (10%)
                    └────────────┘
               ┌────────────────────┐
               │   Integration      │  ← Module interactions
               │      Tests         │     (30%)
               └────────────────────┘
          ┌──────────────────────────────┐
          │        Unit Tests            │  ← Individual functions
          │                              │     (60%)
          └──────────────────────────────┘
```

### Test Categories

1. **Unit Tests** - Individual function behavior
2. **Integration Tests** - Module interactions
3. **E2E Tests** - Full workflows
4. **OTEL Validation** - Span-based verification
5. **Performance Tests** - Benchmarks

## Observability

### OpenTelemetry Integration

```javascript
// All operations create spans
const span = tracer.startSpan('parseTurtle', {
  attributes: {
    'unrdf.operation': 'parse',
    'unrdf.format': 'turtle',
    'unrdf.size': turtle.length
  }
});

try {
  const result = await doParse(turtle);
  span.setStatus({ code: SpanStatusCode.OK });
  return result;
} catch (error) {
  span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
  throw error;
} finally {
  span.end();
}
```

### Metrics

- Parse duration
- Query execution time
- Validation result counts
- Hook execution stats
- Memory usage

## Summary

UNRDF's system design prioritizes:

1. **Developer Productivity** - 80/20 principle eliminates dark matter
2. **Correctness** - Runtime validation over compile-time types
3. **Performance** - Layered caching, async operations
4. **Security** - Input validation, sandboxed execution
5. **Observability** - OpenTelemetry integration throughout

## Related

- [RDF & SPARQL Concepts](./rdf-sparql-concepts.md) - Foundation concepts
- [Knowledge Hooks Architecture](./knowledge-hooks-architecture.md) - Reactive system design
- [API Reference](../reference/api-reference.md) - Full API documentation
