# Supplementary Materials for Thesis

Comprehensive reference materials including glossary, acronyms, index, and quick reference cards.

---

## Glossary of Terms

**Adversarial PM**
: A self-critical methodology that questions every claim and demands empirical evidence before declaring work complete. Separates assertions from reality by asking: "Did you RUN it?" and "Can you PROVE it?"

**Big Bang 80/20 (BB80/20)**
: Single-pass feature implementation methodology using Pareto optimization. Implements 20% of features that deliver 80% of value in one pass with zero rework, validated by static analysis and pattern reuse (≥60% LoC).

**Content-Addressed Condition**
: A governance artifact (SPARQL query, SHACL shape) stored externally and referenced by cryptographic hash (SHA-256). Ensures condition integrity and enables versioned governance policies.

**Cryptographic Receipt**
: Tamper-proof audit record of hook execution containing: hookId, timestamp, condition hash, input hash, output hash, decision, and optional digital signature. Anchored to external systems (git-notes, blockchain) for verifiability.

**Delta Layer (KGC-4D)**
: Named graph storing only change events (quad additions/removals) with temporal metadata. Forms basis for event sourcing and time-travel queries.

**Determinism Seed**
: Fixed random number generator seed ensuring reproducible hook execution. Enables consistent behavior across environments and auditable probabilistic algorithms.

**Hook**
: Declarative reactive function triggered by RDF graph changes. Executes validation, transformation, or side effects following before/run/after lifecycle pattern.

**Hook-Native Architecture**
: Design paradigm where workflow control flow, validation, and governance are implemented as reactive hooks rather than imperative code. Eliminates 80% "dark matter" glue code.

**Information-Theoretic Correctness**
: Probabilistic guarantee that implementation matches specification, computed as `P(Correct) = 1 - 2^(-H_error)` where `H_error = H_spec - log(reuse_rate) - log(static_coverage)`.

**KGC-4D (Knowledge Graph Context - 4 Dimensional)**
: Temporal RDF architecture with four orthogonal named graphs: Delta (changes), State (current view), Shape (constraints), Provenance (audit trail).

**Microframework**
: Minimal, single-purpose library (typically <500 LoC) addressing one concern. Composable building blocks following Unix philosophy: do one thing well.

**Oxigraph**
: Native Rust RDF store with SPARQL 1.1 query engine, compiled to WebAssembly. Provides 6-28x performance improvement over JavaScript-based N3.js.

**Pareto Frontier**
: Set of features with optimal value/cost ratio. In BB80/20, features on the Pareto frontier are implemented in v1; others deferred to future releases.

**Pattern Reuse**
: Copy-exact usage of proven code patterns from existing codebase. BB80/20 methodology targets ≥60% pattern reuse to achieve zero-defect outcome.

**Policy Pack**
: Collection of related hooks with shared configuration, bundled for distribution and versioning. Example: YAWL workflow translated to enablement + completion + cancellation hooks.

**Provenance Layer (KGC-4D)**
: Named graph storing cryptographic receipts and signatures proving who changed what, when, and why. Enables forensic UX and regulatory compliance.

**Reflex Arc (Hook Lifecycle)**
: Three-phase execution model: before() (afferent gate, validation), run() (core processing), after() (efferent cleanup). Mirrors autonomic nervous system pattern.

**Shape Layer (KGC-4D)**
: Named graph containing SHACL constraints and validation reports. Enforces data quality and schema compliance.

**SPARQL-ASK Query**
: Boolean SPARQL query returning true/false. Used in YAWL hooks for control flow predicates (XOR-split decisions) and enablement conditions.

**Specification Entropy (H_spec)**
: Information-theoretic measure of problem complexity, computed as `-Σ p(f_i) log p(f_i)` over feature set. BB80/20 applicable when H_spec ≤ 16 bits.

**State Layer (KGC-4D)**
: Named graph containing materialized current view of knowledge graph. Auto-updated by applying delta layer events.

**Static Analysis**
: Validation without code execution. Includes linting (ESLint), type checking (JSDoc + TypeScript), and security scanning (npm audit). BB80/20 relies on 98%+ static coverage.

**Vector Clock**
: Distributed timestamp mechanism tracking causal dependencies between events. Used in federated KGC-4D for conflict-free replicated data types (CRDTs).

**YAWL (Yet Another Workflow Language)**
: Workflow specification language supporting XOR/AND/OR splits, cancellation regions, resource allocation, and timeout enforcement. Implemented hook-natively in UNRDF.

---

## List of Acronyms

| Acronym | Full Form | Description |
|---------|-----------|-------------|
| **API** | Application Programming Interface | Programmatic interface for library interaction |
| **ASK** | SPARQL ASK Query | Boolean SPARQL query returning true/false |
| **BB80/20** | Big Bang 80/20 Methodology | Single-pass implementation using Pareto optimization |
| **BPMN** | Business Process Model and Notation | Workflow modeling standard (Camunda uses this) |
| **CI/CD** | Continuous Integration / Continuous Deployment | Automated testing and deployment pipeline |
| **CPU** | Central Processing Unit | Processor executing computations |
| **CRDT** | Conflict-Free Replicated Data Type | Data structure for distributed systems |
| **CRUD** | Create, Read, Update, Delete | Basic data operations |
| **DAG** | Directed Acyclic Graph | Graph structure without cycles (git commits, workflow tasks) |
| **DSL** | Domain-Specific Language | Specialized language for specific problem domain |
| **ED25519** | Edwards-curve Digital Signature Algorithm | Elliptic curve cryptography for signatures |
| **FOAF** | Friend of a Friend | RDF ontology for social networks |
| **FMEA** | Failure Mode and Effects Analysis | Risk analysis methodology |
| **H_spec** | Specification Entropy | Information-theoretic complexity measure |
| **HTTP** | Hypertext Transfer Protocol | Web communication protocol |
| **IEEE** | Institute of Electrical and Electronics Engineers | Professional standards organization |
| **IRI** | Internationalized Resource Identifier | Generalized URI supporting Unicode |
| **JSON** | JavaScript Object Notation | Data interchange format |
| **JSDoc** | JavaScript Documentation | Type annotation system for JavaScript |
| **KGC-4D** | Knowledge Graph Context - 4 Dimensional | Temporal RDF architecture |
| **LAN** | Local Area Network | Network within limited geographic area |
| **LoC** | Lines of Code | Metric for code volume |
| **LWW** | Last Write Wins | Conflict resolution strategy |
| **ML** | Machine Learning | AI subdiscipline for pattern recognition |
| **OTEL** | OpenTelemetry | Observability framework for tracing and metrics |
| **P(Correct)** | Probability of Correctness | Information-theoretic correctness measure |
| **PDF** | Portable Document Format | Document file format |
| **PM** | Product Manager / Project Manager | Role responsible for requirements and delivery |
| **QA** | Quality Assurance | Testing and validation discipline |
| **RAM** | Random Access Memory | Computer volatile memory |
| **RDF** | Resource Description Framework | W3C semantic web standard |
| **RDFS** | RDF Schema | Ontology language for RDF |
| **RSS** | Resident Set Size | Memory usage metric (RAM consumption) |
| **SHACL** | Shapes Constraint Language | RDF validation language |
| **SHA-256** | Secure Hash Algorithm 256-bit | Cryptographic hash function |
| **SLA** | Service Level Agreement | Performance guarantee contract |
| **SPARQL** | SPARQL Protocol and RDF Query Language | Query language for RDF |
| **SPOG** | Subject-Predicate-Object-Graph Index | RDF index structure |
| **TDD** | Test-Driven Development | Red-green-refactor methodology |
| **TRIZ** | Theory of Inventive Problem Solving | Innovation methodology |
| **URI** | Uniform Resource Identifier | Unique identifier for resources |
| **URL** | Uniform Resource Locator | Web address (subset of URI) |
| **UUID** | Universally Unique Identifier | 128-bit unique identifier |
| **W3C** | World Wide Web Consortium | Web standards organization |
| **WAN** | Wide Area Network | Network over large geographic area |
| **WASM** | WebAssembly | Binary instruction format for web |
| **XOR** | Exclusive OR | Logical operation: one or the other, not both |
| **YAWL** | Yet Another Workflow Language | Workflow specification language |
| **Zod** | Zod Schema Validation | TypeScript-first schema validation library |

---

## Index of Key Concepts

### A
- Adversarial PM methodology → see Glossary
- AND-join synchronization → YAWL control flow (Table 1)
- AND-split parallelization → YAWL control flow (Diagram 1)
- Anchoring (receipt) → Cryptographic audit trail (Listing 3)
- AS OF query operator → KGC-4D temporal queries (Listing 5)
- Audit trail → Provenance layer (Diagram 2), Receipt generation (Listing 3)

### B
- Batching (hook execution) → Performance overhead (Table 4)
- Big Bang 80/20 methodology → Development comparison (Table 3)
- Blockchain anchoring → Receipt anchoring strategies
- BlankNode validation → Hook definition example (Listing 1)

### C
- Cancellation hooks → YAWL execution flow (Diagram 1)
- Causal consistency → Federation sync (Table 6)
- CHANGES SINCE operator → Delta queries (Listing 5)
- Cold start latency → YAWL performance (Table 1)
- Completion hooks → Control flow routing (Listing 4)
- Composability → Microframework overhead (Table 7)
- Condition evaluation → SPARQL-ASK performance (Table 1)
- Conflict resolution → Vector clocks (Table 6)
- Content-addressed conditions → Hook architecture (Diagram 4), SPARQL integration (Listing 2)
- Control flow routing → YAWL XOR-split (Listing 2, Listing 4)
- Cryptographic receipts → Audit trail generation (Listing 3)

### D
- Defect density → BB80/20 vs TDD (Table 3)
- Delta layer → KGC-4D architecture (Diagram 2)
- Delta sync → Federation topology (Diagram 3)
- Determinism seed → Hook lifecycle (Listing 4)
- Distributed systems → Federation topology (Diagram 3)

### E
- Enablement hooks → Task validation (Diagram 1, Listing 4)
- Event sourcing → Delta layer (Diagram 2)
- Eventual consistency → Federation sync (Table 6)

### F
- Federation gateway → Distributed architecture (Diagram 3)
- FILTER NOT EXISTS → SPARQL negation (Listing 2)

### G
- Git notes → Receipt anchoring (Listing 3)
- Graph views (before/after/delta) → KGC-4D channels (Diagram 2)

### H
- Hook chain compilation → Optimization (Listing 6)
- Hook-native architecture → Core paradigm (all diagrams)
- Hook registry → Centralized distribution (Diagram 3)
- Hyperdimensional feature embedding → BB80/20 methodology

### I
- Information-theoretic correctness → P(Correct) calculation (Table 3)
- IRI validation → Hook definition (Listing 1)

### J
- JSDoc type hints → Code documentation (all listings)

### K
- KGC-4D architecture → Four-dimensional graphs (Diagram 2)

### L
- Lifecycle functions (before/run/after) → Reflex arc (Diagram 4)

### M
- Memoization → Hook optimization (Listing 6)
- Microframework composition → Hub pattern (Diagram 5)

### O
- Offline-first execution → Federation topology (Diagram 3), YAWL capability (Table 1)
- OR-split routing → YAWL control flow (Diagram 1)
- Oxigraph performance → Store comparison (Table 5)

### P
- Parallelization (hook execution) → Compilation (Listing 6)
- Pareto frontier → BB80/20 feature selection (Table 3)
- Pattern reuse → BB80/20 methodology (Table 3)
- Policy pack → YAWL workflow translation (Listing 4)
- Polling vs hooks → Reactivity comparison (Table 2)
- Provenance layer → Audit trail (Diagram 2)

### Q
- Query optimization → SPARQL indexes (Table 8)

### R
- Receipt generation → Cryptographic proof (Listing 3)
- Reflex arc → Hook lifecycle (Diagram 4)
- Resource allocation → YAWL capacity constraints (Listing 4)

### S
- SHA-256 integrity → Content-addressing (Listing 2)
- SHACL validation → Shape layer (Diagram 2)
- Shape layer → KGC-4D constraints (Diagram 2)
- SPARQL-ASK queries → Control flow decisions (Listing 2)
- Specification entropy → H_spec calculation (Table 3)
- State layer → KGC-4D materialized view (Diagram 2)
- Static analysis → BB80/20 validation (Table 3)

### T
- Temporal queries → Time-travel SPARQL (Listing 5)
- Timeout enforcement → Cancellation hooks (Diagram 1, Listing 4)
- TRIZ innovation → Design principles

### V
- Vector clocks → Distributed sync (Table 6)

### W
- Workflow orchestration → YAWL patterns (Diagram 1, Listing 4)

### X
- XOR-join synchronization → YAWL control flow
- XOR-split routing → Conditional branching (Listing 2, Listing 4)

### Y
- YAWL execution model → Hook-native implementation (Diagram 1)

### Z
- Zod validation → Schema enforcement (Listing 3)

---

## Quick Reference Card: Hook Definition

**Minimal Hook Template**

```javascript
import { defineHook } from '@unrdf/hooks';

const myHook = defineHook({
  name: 'my-hook-name',           // Required: Unique identifier
  trigger: 'before-add',          // Required: When to execute
  validate: (quad) => true,       // Optional: Return bool
  transform: (quad) => quad,      // Optional: Return modified quad
  metadata: {}                    // Optional: Extensible metadata
});
```

**Trigger Types (33 total)**

| Category | Triggers |
|----------|----------|
| **CRUD** | `before-add`, `after-add`, `before-query`, `after-query`, `before-remove`, `after-remove` |
| **Transaction** | `before-commit`, `after-commit`, `before-rollback`, `after-rollback` |
| **Error/Event** | `on-error`, `on-validation-fail`, `on-transform`, `on-timeout`, `on-circuit-open` |
| **Async/IO** | `before-fetch`, `after-fetch`, `before-sync`, `after-sync`, `before-import`, `after-import` |
| **Cron/Time** | `on-schedule`, `on-interval`, `on-idle`, `on-startup` |
| **Quality** | `quality-gate`, `defect-detection`, `continuous-improvement`, `spc-control`, `capability-analysis`, `root-cause`, `kaizen-event`, `audit-trail` |

**Common Patterns**

```javascript
// 1. Validation (deny on condition)
validate: (quad) => quad.subject.termType === 'NamedNode'

// 2. Transformation (modify quad)
transform: (quad) => quad.with({ graph: namedNode('default') })

// 3. Async validation (SPARQL query)
validate: async (quad) => {
  const result = await store.query('ASK { ... }');
  return result;
}

// 4. Side effects (logging, notifications)
validate: async (quad) => {
  await logToAuditSystem(quad);
  return true;  // Always allow, just log
}
```

---

## Quick Reference Card: YAWL Workflow

**Minimal Workflow Specification**

```javascript
import { createYAWLPolicyPack } from '@unrdf/yawl';

const workflow = {
  name: 'my-workflow',
  version: '1.0.0',
  tasks: [
    {
      id: 'task-a',
      kind: 'AtomicTask',
      name: 'Task A',
      inputConditions: []  // Entry point
    },
    {
      id: 'task-b',
      kind: 'AtomicTask',
      name: 'Task B',
      inputConditions: ['task-a-complete']
    }
  ],
  controlFlow: [
    {
      source: 'task-a',
      target: 'task-b',
      predicate: 'true',  // Always proceed
      splitType: 'AND'    // Unconditional
    }
  ]
};

const policyPack = createYAWLPolicyPack(workflow);
```

**Task Kinds**

- `AtomicTask`: Indivisible unit of work
- `CompositeTask`: Decomposes to sub-workflow
- `MultiInstanceTask`: Multiple concurrent instances
- `AutomatedTask`: No human intervention
- `ManualTask`: Requires human action

**Split Types**

| Type | Behavior | Use Case |
|------|----------|----------|
| `XOR` | Exactly one outgoing edge fires | Conditional branching (if/else) |
| `AND` | All outgoing edges fire | Parallel tasks (fork) |
| `OR` | One or more edges fire | Optional parallelism |

**Control Flow Predicates**

```javascript
// Positive condition
predicate: 'approved'  // ASK { ?var yawl:name "approved" ; yawl:value true }

// Negative condition
predicate: '!rejected'  // ASK { FILTER NOT EXISTS { ... } }

// Always true (unconditional)
predicate: 'true'

// SPARQL expression (custom)
predicate: '?amount > 1000'  // Custom SPARQL logic
```

---

## Quick Reference Card: KGC-4D Temporal Queries

**Four Dimensions**

| Dimension | Graph URI | Purpose | Example |
|-----------|-----------|---------|---------|
| **Delta** | `<urn:kgc4d:delta>` | Change events | Event sourcing, audit trail |
| **State** | `<urn:kgc4d:state>` | Current view | Normal SPARQL queries |
| **Shape** | `<urn:kgc4d:shape>` | SHACL constraints | Data validation |
| **Provenance** | `<urn:kgc4d:provenance>` | Receipts + signatures | Forensic UX |

**Temporal Operators**

```sparql
# AS OF: State at specific timestamp
SELECT ?s ?p ?o
WHERE { ?s ?p ?o }
AS OF "2024-01-01T00:00:00Z"^^xsd:dateTime

# BETWEEN: Changes in time range
SELECT ?s ?p ?o ?timestamp
FROM NAMED <urn:kgc4d:delta>
WHERE {
  GRAPH <urn:kgc4d:delta> {
    ?event kgc:subject ?s ; kgc:predicate ?p ; kgc:object ?o ; kgc:timestamp ?timestamp .
  }
}
BETWEEN "2024-01-01"^^xsd:dateTime AND "2024-12-31"^^xsd:dateTime

# CHANGES SINCE: Delta stream from timestamp
SELECT ?event
FROM NAMED <urn:kgc4d:delta>
WHERE {
  GRAPH <urn:kgc4d:delta> {
    ?event kgc:timestamp ?ts .
    FILTER (?ts > "2024-01-01T00:00:00Z"^^xsd:dateTime)
  }
}
ORDER BY ?ts
```

---

## Quick Reference Card: Performance Targets

**Latency Targets (p99)**

| Operation | Target | Acceptable | Poor |
|-----------|--------|------------|------|
| Hook evaluation (simple) | <1 ms | <5 ms | >10 ms |
| SPARQL-ASK (100 triples) | <10 ms | <50 ms | >100 ms |
| Control flow routing | <5 ms | <20 ms | >50 ms |
| Receipt generation | <5 ms | <10 ms | >20 ms |
| Quad insertion (1K batch) | <10 ms | <50 ms | >100 ms |

**Throughput Targets**

| Workload | Target | Acceptable | Poor |
|----------|--------|------------|------|
| Workflow tasks/sec | >5,000 | >1,000 | <500 |
| Quad insertions/sec | >10,000 | >5,000 | <1,000 |
| SPARQL queries/sec | >1,000 | >500 | <100 |
| Federation deltas/sec | >5,000 | >2,000 | <1,000 |

**Resource Targets**

| Metric | Target | Acceptable | Poor |
|--------|--------|------------|------|
| Memory (per workflow) | <200 KB | <1 MB | >5 MB |
| Bundle size (core + hooks) | <600 KB | <1 MB | >2 MB |
| Startup time (cold) | <20 ms | <100 ms | >500 ms |
| CPU utilization (idle) | <0.1% | <1% | >5% |

---

## Citation Guide

**BibTeX Entries**

```bibtex
@software{unrdf2024,
  title = {UNRDF: Hook-Native Knowledge Graphs},
  author = {UNRDF Contributors},
  year = {2024},
  url = {https://github.com/unrdf/unrdf},
  version = {5.0.0}
}

@article{bigbang8020,
  title = {Big Bang 80/20: Information-Theoretic Single-Pass Development},
  journal = {UNRDF Technical Reports},
  year = {2024},
  note = {H\_spec ≤ 16 bits enables zero-rework implementation}
}

@techreport{kgc4d,
  title = {KGC-4D: Four-Dimensional Temporal Knowledge Graphs},
  institution = {UNRDF Project},
  year = {2024},
  note = {Delta/State/Shape/Provenance architecture}
}

@inproceedings{yawl-hooks,
  title = {Hook-Native YAWL: Declarative Workflow Orchestration},
  booktitle = {UNRDF Case Studies},
  year = {2024},
  note = {37-71x performance improvement over Temporal.io}
}
```

**IEEE Citation Style**

```
[1] UNRDF Contributors, "UNRDF: Hook-Native Knowledge Graphs," 2024. [Online].
    Available: https://github.com/unrdf/unrdf

[2] "Big Bang 80/20: Information-Theoretic Single-Pass Development," UNRDF
    Technical Reports, 2024.

[3] "KGC-4D: Four-Dimensional Temporal Knowledge Graphs," UNRDF Project,
    Tech. Rep., 2024.

[4] "Hook-Native YAWL: Declarative Workflow Orchestration," in UNRDF Case
    Studies, 2024.
```

---

## LaTeX Integration Examples

**Include Glossary**

```latex
\usepackage{glossaries}

\newglossaryentry{hook}{
  name=hook,
  description={Declarative reactive function triggered by RDF graph changes}
}

\makeglossaries

% In text
Knowledge \glspl{hook} eliminate polling overhead.

% Print glossary
\printglossaries
```

**Include Acronyms**

```latex
\usepackage{acronym}

\begin{acronym}
  \acro{YAWL}{Yet Another Workflow Language}
  \acro{KGC-4D}{Knowledge Graph Context - 4 Dimensional}
  \acro{SPARQL}{SPARQL Protocol and RDF Query Language}
\end{acronym}

% In text
\ac{YAWL} supports XOR/AND/OR splits.
```

**Include Index**

```latex
\usepackage{makeidx}
\makeindex

% In text
Hook-native architecture\index{Hook-native architecture} eliminates glue code.

% Print index
\printindex
```

---

## Related Files

- **DIAGRAMS.md**: Mermaid architecture diagrams
- **TABLES.md**: Performance comparison tables
- **CODE-LISTINGS.md**: Syntax-highlighted code examples
- **CLAUDE.md**: Project-specific guidelines and methodology
- **docs/bb80-20-methodology.md**: Detailed BB80/20 explanation
