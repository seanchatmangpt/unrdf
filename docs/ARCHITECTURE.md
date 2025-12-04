# UNRDF Architecture

Complete system architecture guide for UNRDF v5.0.0+.

## Executive Summary

UNRDF is a production-ready RDF knowledge graph library that transforms static data into intelligent, reactive systems. Built on the **80/20 principle**, it eliminates 80% of the "dark matter" glue code typically required in RDF development.

**Key Architectural Decisions:**

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Language | JavaScript (ESM) | Universal runtime, no compilation |
| Type Safety | JSDoc + Zod | Runtime validation > compile-time types |
| RDF Store | N3.Store | Fast, memory-efficient, well-maintained |
| Query Engine | Comunica | SPARQL 1.1 compliant, performant |
| Reactivity | Knowledge Hooks | Declarative, auditable triggers |

---

## System Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           UNRDF Architecture                            │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                     Application Interfaces                       │   │
│  │                                                                   │   │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐              │   │
│  │  │ React Hooks │  │ Composables │  │    CLI      │              │   │
│  │  │  (40 hooks) │  │ (Vue-style) │  │ (citty)    │              │   │
│  │  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘              │   │
│  │         └────────────────┴────────────────┘                      │   │
│  └─────────────────────────────┬───────────────────────────────────┘   │
│                                │                                        │
│  ┌─────────────────────────────┴───────────────────────────────────┐   │
│  │                      Knowledge Engine                            │   │
│  │                                                                   │   │
│  │  ┌───────────┐ ┌───────────┐ ┌───────────┐ ┌───────────┐        │   │
│  │  │  Parse    │ │   Query   │ │ Validate  │ │  Reason   │        │   │
│  │  │  (N3.js)  │ │ (Comunica)│ │  (SHACL)  │ │   (EYE)   │        │   │
│  │  └───────────┘ └───────────┘ └───────────┘ └───────────┘        │   │
│  │                                                                   │   │
│  │  ┌───────────┐ ┌───────────┐ ┌───────────┐ ┌───────────┐        │   │
│  │  │Canonicalize│ │Transaction│ │ Knowledge │ │  Schemas  │        │   │
│  │  │ (URDNA)   │ │  Manager  │ │   Hooks   │ │   (Zod)   │        │   │
│  │  └───────────┘ └───────────┘ └───────────┘ └───────────┘        │   │
│  └─────────────────────────────┬───────────────────────────────────┘   │
│                                │                                        │
│  ┌─────────────────────────────┴───────────────────────────────────┐   │
│  │                     Foundation Layer                             │   │
│  │                                                                   │   │
│  │  ┌───────────────────┐  ┌───────────────────┐                   │   │
│  │  │     N3.Store      │  │    DataFactory    │                   │   │
│  │  │  (RDF Storage)    │  │  (Term Creation)  │                   │   │
│  │  └───────────────────┘  └───────────────────┘                   │   │
│  │                                                                   │   │
│  │  ┌───────────────────┐  ┌───────────────────┐                   │   │
│  │  │    Utilities      │  │   OpenTelemetry   │                   │   │
│  │  │   (Helpers)       │  │  (Observability)  │                   │   │
│  │  └───────────────────┘  └───────────────────┘                   │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Component Architecture

### Knowledge Engine

The core RDF processing engine providing all fundamental operations.

#### Parse Module (`knowledge-engine/parse.mjs`)

**Responsibility:** RDF serialization and deserialization

**Supported Formats:**

| Format | Parse | Serialize | Implementation |
|--------|-------|-----------|----------------|
| Turtle | Yes | Yes | N3.js |
| N-Quads | Yes | Yes | N3.js |
| JSON-LD | Yes | Yes | jsonld |

**Key Functions:**

```javascript
parseTurtle(turtle, baseIRI?) -> Promise<N3.Store>
toTurtle(store, options?) -> Promise<string>
parseJsonLd(jsonld, options?) -> Promise<N3.Store>
toJsonLd(store, options?) -> Promise<object>
toNQuads(store, options?) -> Promise<string>
```

#### Query Module (`knowledge-engine/query.mjs`)

**Responsibility:** SPARQL query execution

**Query Types:**

| Type | Function | Returns |
|------|----------|---------|
| SELECT | `select()` | `Object[]` (bindings) |
| ASK | `ask()` | `boolean` |
| CONSTRUCT | `construct()` | `N3.Store` |
| DESCRIBE | `describe()` | `N3.Store` |
| UPDATE | `update()` | `void` |

**Implementation:** Comunica query-sparql

#### Validate Module (`knowledge-engine/validate.mjs`)

**Responsibility:** SHACL validation

**Key Functions:**

```javascript
validateShacl(store, shapes, options?) -> Promise<ValidationReport>
formatValidationReport(report, options?) -> string
hasValidationErrors(report) -> boolean
getValidationErrors(report) -> ValidationResult[]
getValidationWarnings(report) -> ValidationResult[]
```

**Implementation:** rdf-validate-shacl

#### Reason Module (`knowledge-engine/reason.mjs`)

**Responsibility:** N3 rule-based reasoning

**Key Functions:**

```javascript
reason(store, rules, options?) -> Promise<N3.Store>
extractInferred(original, reasoned) -> Promise<N3.Store>
getReasoningStats(original, reasoned) -> ReasoningStats
```

**Implementation:** eyereasoner

#### Canonicalize Module (`knowledge-engine/canonicalize.mjs`)

**Responsibility:** Deterministic graph hashing and comparison

**Key Functions:**

```javascript
canonicalize(store, options?) -> Promise<N3.Store>
isIsomorphic(storeA, storeB) -> Promise<boolean>
getCanonicalHash(store) -> Promise<string>
```

**Implementation:** rdf-canonize (URDNA2015)

---

### Knowledge Hooks System

The reactive trigger system for knowledge graphs.

#### Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Knowledge Hook System                        │
│                                                                 │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                   Hook Definition                        │   │
│  │                                                           │   │
│  │  defineHook({                                            │   │
│  │    meta: { name, description, ontology },                │   │
│  │    channel: { graphs, view },                            │   │
│  │    when: { kind, ref: { uri, sha256, mediaType } },      │   │
│  │    determinism: { seed },                                │   │
│  │    receipt: { anchor },                                  │   │
│  │    before, run, after                                    │   │
│  │  })                                                      │   │
│  └─────────────────────────────────────────────────────────┘   │
│                            │                                    │
│                            ▼                                    │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                  Evaluation Pipeline                     │   │
│  │                                                           │   │
│  │  Channel Filter → Condition Check → Lifecycle Execution  │   │
│  │       │                │                    │             │   │
│  │       │                │          ┌────────┴────────┐    │   │
│  │       │                │          │                 │    │   │
│  │       ▼                ▼          ▼                 ▼    │   │
│  │  Named Graphs    SPARQL/SHACL   before()         after() │   │
│  │  Filtering       Evaluation       │                │    │   │
│  │                                   │                │    │   │
│  │                                   ▼                │    │   │
│  │                                 run()              │    │   │
│  │                                   │                │    │   │
│  │                                   └────────────────┘    │   │
│  └─────────────────────────────────────────────────────────┘   │
│                            │                                    │
│                            ▼                                    │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                    Receipt System                        │   │
│  │                                                           │   │
│  │  ┌─────────────┐  ┌─────────────┐                       │   │
│  │  │  git-notes  │  │    none     │                       │   │
│  │  │  (audit)    │  │  (no-op)    │                       │   │
│  │  └─────────────┘  └─────────────┘                       │   │
│  └─────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

#### Hook Contract (80/20)

The contract covers 80% of use cases with minimal configuration:

| Property | Required | Description |
|----------|----------|-------------|
| `meta.name` | Yes | Unique hook identifier |
| `meta.description` | No | Human-readable description |
| `when.kind` | Yes | Condition type (sparql-ask, sparql-select, shacl) |
| `when.ref` | Yes | Content-addressed condition reference |
| `run` | Yes | Main execution function |
| `before` | No | Pre-execution gate |
| `after` | No | Post-execution cleanup |
| `channel` | No | Graph observation scope |
| `determinism` | No | Reproducibility config |
| `receipt` | No | Audit trail strategy |

---

### React Hooks (40 Hooks)

React integration organized by the 80/20 principle.

#### Tier Structure

```
┌─────────────────────────────────────────────────────────────────┐
│                    React Hooks (40 Total)                       │
│                                                                 │
│  TIER 1: Essential (5 hooks, 60% usage)                        │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ useKnowledgeEngine, useTransaction, useKnowledgeHook,   │   │
│  │ useChangeFeed, useDarkMatterCore                        │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
│  TIER 2: Important (2 hooks, 20% usage)                        │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ useGraphDiff, useSPARQLEditor                           │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
│  TIER 3: Standard (9 hooks, 15% usage)                         │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ useFederatedSystem, useStreamProcessor, useOptimizer,   │   │
│  │ useSemanticAnalyzer, useGraphMerge, usePolicyPack,      │   │
│  │ useRecovery, useGraphVisualizer, useResultsPaginator    │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
│  TIER 4: Advanced (24 hooks, 5% usage)                         │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ Available via category imports for advanced use cases   │   │
│  └─────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

#### Category Imports

```javascript
// Tier 1 & 2 (default export)
import { useKnowledgeEngine, useChangeFeed } from 'unrdf/react-hooks';

// Category imports (Tier 3 & 4)
import * as Federation from 'unrdf/react-hooks/federation';
import * as Streaming from 'unrdf/react-hooks/streaming';
import * as DarkMatter from 'unrdf/react-hooks/dark-matter';
import * as AISemantic from 'unrdf/react-hooks/ai-semantic';
```

---

## Data Flow

### Parse → Query → Validate Pipeline

```
┌───────────────────────────────────────────────────────────────────┐
│                      Data Flow Pipeline                           │
│                                                                   │
│  Input (Turtle/JSON-LD)                                          │
│       │                                                           │
│       ▼                                                           │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐          │
│  │   Parse     │───▶│  N3.Store   │───▶│   Query     │          │
│  │             │    │  (Graph)    │    │  (SPARQL)   │          │
│  └─────────────┘    └──────┬──────┘    └─────────────┘          │
│                            │                                      │
│                            ├─────────────────────┐                │
│                            │                     │                │
│                            ▼                     ▼                │
│                     ┌─────────────┐       ┌─────────────┐        │
│                     │  Validate   │       │   Reason    │        │
│                     │  (SHACL)    │       │   (EYE)     │        │
│                     └─────────────┘       └─────────────┘        │
│                            │                     │                │
│                            ▼                     ▼                │
│                     ┌─────────────┐       ┌─────────────┐        │
│                     │   Report    │       │  Inferred   │        │
│                     │ (Violations)│       │   (Store)   │        │
│                     └─────────────┘       └─────────────┘        │
│                                                                   │
└───────────────────────────────────────────────────────────────────┘
```

---

## Quality Frameworks Applied

### FMEA (Failure Mode and Effects Analysis)

| Component | Failure Mode | Effect | Mitigation |
|-----------|--------------|--------|------------|
| Parse | Invalid syntax | Parse error | Detailed error messages with line numbers |
| Query | Timeout | Hanging application | Configurable timeouts |
| Validate | Wrong shapes | False positives | Schema versioning, CI validation |
| Hooks | Condition tampering | Security breach | SHA-256 integrity checks |
| Hooks | Missing context | Runtime error | Defensive context checking |

### TRIZ Principles Applied

| Principle | Application |
|-----------|-------------|
| Segmentation | Separate before/run/after lifecycle |
| Prior Action | Content-addressed conditions |
| Parameter Changes | Determinism seed for reproducibility |
| Feedback | Receipt anchoring for audit trails |
| Dynamism | Delta view for change detection |

### DFLSS Quality Targets

| Metric | Target | Measurement |
|--------|--------|-------------|
| Parse latency | <50ms for 10KB | Performance tests |
| Query latency | <100ms for simple SELECT | Performance tests |
| Validation latency | <200ms for 100 shapes | Performance tests |
| Test coverage | >80% | vitest coverage |
| API stability | Semver compliant | Automated checks |

---

## Security Architecture

### Input Validation

All public APIs validate input with Zod schemas:

```javascript
const TurtleInputSchema = z.object({
  turtle: z.string().min(1),
  baseIRI: z.string().url().optional()
});
```

### Hook Sandboxing

Hook functions execute in isolated contexts:

- No filesystem access
- No network access (unless configured)
- Resource limits enforced
- Isolated from other hooks

### Condition Integrity

SHA-256 verification prevents tampering:

```javascript
if (sha256(conditionContent) !== hook.when.ref.sha256) {
  throw new SecurityError('Condition integrity check failed');
}
```

---

## Observability

### OpenTelemetry Integration

All operations create spans for tracing:

```javascript
const span = tracer.startSpan('parseTurtle', {
  attributes: {
    'unrdf.operation': 'parse',
    'unrdf.format': 'turtle',
    'unrdf.size': turtle.length
  }
});
```

### Metrics Collected

- Operation duration (parse, query, validate, reason)
- Store size (triple count)
- Validation results (pass/fail counts)
- Hook execution stats

---

## Deployment Architecture

### Package Structure

```
npm package: unrdf
├── unrdf (main)
├── unrdf/knowledge-engine
├── unrdf/react-hooks
├── unrdf/react-hooks/core
├── unrdf/react-hooks/streaming
├── unrdf/react-hooks/federation
├── unrdf/react-hooks/dark-matter
├── unrdf/react-hooks/ai-semantic
├── unrdf/composables/*
└── unrdf/cli
```

### Environment Requirements

- Node.js >= 18.0.0
- pnpm >= 7.0.0 (recommended)
- ESM module system

### Container Support

```dockerfile
# Dockerfile
FROM node:18-alpine
WORKDIR /app
COPY package.json pnpm-lock.yaml ./
RUN pnpm install --frozen-lockfile
COPY . .
CMD ["node", "src/index.mjs"]
```

---

## Related Documentation

- [System Design Explanation](./explanation/system-design.md) - In-depth design rationale
- [Knowledge Hooks Architecture](./explanation/knowledge-hooks-architecture.md) - Reactive system design
- [API Reference](./reference/api-reference.md) - Complete API documentation
- [Getting Started](./GETTING_STARTED.md) - Quick start guide
