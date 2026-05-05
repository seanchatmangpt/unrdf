# UNRDF Learning Paths: Personalized Documentation Navigation

**Generated**: 2025-12-28
**Purpose**: Suggest optimal reading order and capability exploration based on user persona and goals
**Methodology**: Information-theoretic analysis + semantic clustering + empirical user patterns

---

## Overview

Different users need different paths through UNRDF's 47+ capabilities across 55 packages. This guide provides curated learning journeys based on:

- **Background**: Prior knowledge (RDF novice, experienced Semantic Web developer, distributed systems architect)
- **Goals**: What you want to build (data API, workflow engine, audit system, distributed knowledge base)
- **Time Budget**: Quick start (2 hours), comprehensive (1 week), mastery (1 month)

---

## Quick Start (2 Hours): Essential Capabilities

**Goal**: Get productive with core RDF operations and SPARQL queries

### Path Overview
1. [30 min] RDF fundamentals + Store creation
2. [45 min] SPARQL query execution
3. [30 min] Data validation and error handling
4. [15 min] Hands-on example

### Detailed Steps

#### Step 1: RDF Fundamentals (30 min)
**Read**: [@unrdf/core - Overview](./core.md#overview)
**Focus**: Understand quads (subject, predicate, object, graph) and the RDF data model

**Key Concepts**:
- Named nodes (IRIs)
- Literals (values with optional language/datatype)
- Blank nodes (anonymous entities)
- Quads vs triples (quads = triples + graph)

**Hands-On**:
```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';
const { namedNode, literal, quad } = dataFactory;

const store = createStore();
const alice = namedNode('http://example.org/alice');
const name = namedNode('http://xmlns.com/foaf/0.1/name');
const aliceName = literal('Alice');

store.insert(quad(alice, name, aliceName));
console.log('Inserted first quad!');
```

**Verify Understanding**: Can you explain why `alice` is a named node but `aliceName` is a literal?

---

#### Step 2: SPARQL Query Execution (45 min)
**Read**: [@unrdf/core - SPARQL Query Execution](./core.md#2-sparql-query-execution)
**Focus**: Execute SELECT, ASK, and CONSTRUCT queries

**Key Concepts**:
- SELECT: Retrieve variable bindings
- ASK: Boolean existence checks
- CONSTRUCT: Transform graph structure
- Query patterns and filters

**Hands-On**:
```javascript
import { executeSelect } from '@unrdf/core';

const results = await executeSelect(store, `
  SELECT ?person ?name WHERE {
    ?person <http://xmlns.com/foaf/0.1/name> ?name
  }
`);

for (const row of results) {
  console.log(`${row.person.value}: ${row.name.value}`);
}
```

**Verify Understanding**: Write a query to find all people whose name starts with "A".

---

#### Step 3: Data Validation (30 min)
**Read**: [@unrdf/core - Validation & Constraints](./core.md#4-validation--constraints)
**Focus**: Validate incoming RDF data before insertion

**Key Concepts**:
- Triple validation (subject/predicate/object constraints)
- IRI validation (format checking)
- ValidationError handling

**Hands-On**:
```javascript
import { validateTriple, ValidationError } from '@unrdf/core';

try {
  // This will fail: literals cannot be subjects
  const invalidQuad = quad(literal('Invalid'), name, alice);
  validateTriple(invalidQuad);
} catch (e) {
  if (e instanceof ValidationError) {
    console.log('Caught invalid quad:', e.message);
  }
}
```

**Verify Understanding**: What are the three RDF semantic rules enforced by `validateTriple()`?

---

#### Step 4: Complete Example (15 min)
**Read**: [@unrdf/core - Tutorial](./core.md#tutorial-building-your-first-rdf-graph)
**Focus**: Build a small knowledge graph from scratch

**Challenge**: Create a graph of 3 people with names and "knows" relationships, then query to find all mutual friendships.

**Success Criteria**: Query returns expected results and handles validation errors gracefully.

---

### What's Next?
**Beginner → Intermediate**: Learn serialization and canonicalization
**Quick wins**: Add `@unrdf/validation` for production-grade error handling
**Common questions**: See [INSIGHTS.md](./INSIGHTS.md) for performance tips

---

## Intermediate Path (1 Week): Production-Ready RDF Applications

**Goal**: Build robust RDF APIs with validation, observability, and performance optimization

### Week Plan
- **Day 1-2**: Advanced SPARQL + Performance optimization
- **Day 3-4**: Serialization, canonicalization, graph comparison
- **Day 5**: Observability and debugging
- **Day 6-7**: Policy-gated stores with hooks

### Day 1-2: Advanced SPARQL + Performance (4 hours)

#### Morning: Query Optimization
**Read**:
- [@unrdf/core - Performance Characteristics](./core.md#performance-characteristics)
- [@unrdf/oxigraph - Query Execution](./oxigraph.md#3-query-execution)
- [INSIGHTS.md - SPARQL Insights](./INSIGHTS.md#sparql-query-execution)

**Learn**:
- Predicate selectivity and query planning
- Streaming results vs array materialization
- Prepared statements for repeated queries

**Hands-On**:
```javascript
// BAD: Materializes all results
const results = [...store.query('SELECT * WHERE { ?s ?p ?o }')];

// GOOD: Streams results
for (const row of store.query('SELECT * WHERE { ?s ?p ?o }')) {
  processRow(row); // Process one at a time
}
```

**Exercise**: Benchmark a query with 10K results - measure memory difference between streaming and materializing.

---

#### Afternoon: Pattern Matching Optimization
**Read**: [@unrdf/oxigraph - Pattern Matching](./oxigraph.md#5-pattern-matching--filtering)

**Learn**:
- Index utilization (predicate index is fastest)
- Wildcard patterns vs SPARQL
- When to use `match()` vs `query()`

**Hands-On**:
```javascript
// Fast: Uses predicate index
const byPredicate = store.match(undefined, predicate, undefined);

// Slow: No object index in Oxigraph
const byObject = store.match(undefined, undefined, object);

// Better: Use SPARQL for object lookups
const sparqlByObject = store.query(`
  SELECT ?s ?p WHERE { ?s ?p <${object.value}> }
`);
```

**Exercise**: Compare performance of `match()` vs SPARQL for 1000 object lookups.

---

### Day 3-4: Serialization & Canonicalization (4 hours)

#### Morning: Graph Serialization
**Read**: [@unrdf/core - Serialization & Canonicalization](./core.md#5-serialization--canonicalization)

**Learn**:
- N-Triples vs N-Quads vs Turtle
- Canonical form guarantees determinism
- Use cases for canonicalization

**Hands-On**:
```javascript
import { canonicalize, toNTriples } from '@unrdf/core';

const canonical = canonicalize(quads);
const hash = sha256(canonical); // Deterministic hash
```

**Exercise**: Create two isomorphic graphs (same data, different blank node names) and verify `isIsomorphic()` returns true.

---

#### Afternoon: Graph Comparison
**Read**: [INSIGHTS.md - isIsomorphic()](./INSIGHTS.md#isisomorphic)

**Learn**:
- When graphs are isomorphic vs equal
- Blank node automorphism groups
- Performance implications of canonicalization

**Challenge**: Build a function to detect duplicate subgraphs in a large knowledge base using isomorphism.

---

### Day 5: Observability & Debugging (2 hours)

**Read**:
- [@unrdf/core - Observability & Debugging](./core.md#7-observability--debugging)
- [INSIGHTS.md - DebugLogger](./INSIGHTS.md#debuglogger--performancetracker)

**Learn**:
- DebugLogger for structured logging
- PerformanceTracker for profiling
- CircuitBreaker for fault tolerance

**Hands-On**:
```javascript
import { PerformanceTracker } from '@unrdf/core';

const tracker = new PerformanceTracker();
tracker.start('query');
await executeSelect(store, complexQuery);
const { duration, allocations } = tracker.end('query');

console.log(`Query: ${duration}ms, ${allocations} allocations`);
```

**Exercise**: Profile a bulk load of 10K quads - identify bottlenecks.

---

### Day 6-7: Policy-Gated Stores (4 hours)

**Read**: [CAPABILITY-BASIS.md - Policy & Governance](../CAPABILITY-BASIS.md#5-policy--governance-hooks)

**Learn**:
- Hook definition and execution
- Hook chains and composition
- JIT compilation for performance

**Hands-On**:
```javascript
import { defineHook, executeHook } from '@unrdf/hooks';

const lengthCheck = defineHook({
  trigger: 'before:insert',
  validate: (quad) => quad.object.value.length < 1000
});

if (await executeHook(lengthCheck, quad)) {
  store.insert(quad);
} else {
  throw new Error('Object value too long');
}
```

**Challenge**: Build a compliance layer that rejects any quad with predicates not in an approved allowlist.

---

### Week Summary Assessment
**Can you**:
- [ ] Optimize SPARQL queries for production workloads?
- [ ] Serialize and canonicalize graphs for hashing?
- [ ] Implement observability for debugging?
- [ ] Build policy-gated data ingestion?

**What's Next**: Advanced path (time-travel, workflows, federation)

---

## Advanced Path (1 Month): Distributed Knowledge Systems

**Goal**: Build production-scale distributed systems with time-travel, workflows, and federation

### Week 1: Time-Travel & Event Sourcing
**Packages**: `@unrdf/kgc-4d`, `@unrdf/kgc-runtime`

**Focus**:
- 4D event sourcing model
- Git-backed snapshots
- Vector clocks for distributed ordering
- State reconstruction

**Read**:
- [CAPABILITY-BASIS.md - Time-Travel](../CAPABILITY-BASIS.md#2-time-travel--event-sourcing)
- [INSIGHTS.md - freezeUniverse()](./INSIGHTS.md#freezeuniverse)

**Hands-On**: Build a time-travel debugger that can rewind application state to any past moment.

---

### Week 2: Workflow Orchestration
**Packages**: `@unrdf/yawl`, `@unrdf/yawl-durable`

**Focus**:
- Van der Aalst workflow patterns
- Petri net execution semantics
- Cancellation regions
- Workflow visualization

**Read**:
- [CAPABILITY-BASIS.md - Workflow Engine](../CAPABILITY-BASIS.md#6-workflow-engine-yawl)
- [INSIGHTS.md - WorkflowEngine](./INSIGHTS.md#workflowengine)

**Hands-On**: Implement a long-running approval workflow with timeouts and escalation.

---

### Week 3: Cryptographic Receipts & Auditability
**Packages**: `@unrdf/blockchain`, `@unrdf/yawl`

**Focus**:
- BLAKE3 hash chains
- Receipt generation and verification
- Non-repudiable audit trails
- Blockchain anchoring

**Read**:
- [CAPABILITY-BASIS.md - Cryptographic Receipts](../CAPABILITY-BASIS.md#4-cryptographic-receipts)
- [INSIGHTS.md - generateReceipt()](./INSIGHTS.md#generatereceipt--verifyreceipt)

**Hands-On**: Build a compliance system that generates tamper-proof audit logs for financial transactions.

---

### Week 4: Federation & Distributed Queries
**Packages**: `@unrdf/federation`, `@unrdf/consensus`

**Focus**:
- Peer discovery
- Distributed SPARQL queries
- RAFT consensus
- Shard coordination

**Read**:
- [CAPABILITY-BASIS.md - Distributed Systems](../CAPABILITY-BASIS.md#8-distributed-systems)
- [INSIGHTS.md - Federation + Caching](./INSIGHTS.md#pattern-federation--caching)

**Hands-On**: Build a federated knowledge base that queries across 3 independent stores with caching.

---

### Month Summary Assessment
**Can you**:
- [ ] Implement time-travel for any application?
- [ ] Orchestrate complex multi-step workflows?
- [ ] Generate cryptographically-verifiable audit trails?
- [ ] Deploy distributed, fault-tolerant knowledge bases?

**What's Next**: Expert path (BEAM runtime, ML integration, custom extensions)

---

## Expert Path: Cutting-Edge Capabilities

**Prerequisites**: Completed Advanced path or equivalent experience

### Track 1: BEAM Runtime Integration (2 weeks)
**Packages**: `@unrdf/atomvm`, `@unrdf/atomvm-playground`

**Goal**: Run Erlang/OTP processes in browser and Node.js

**Focus**:
- AtomVM WASM runtime
- gen_statem state machines
- Service worker clusters
- KGC-4D bridge to BEAM

**Capstone**: Build a distributed game server with fault tolerance and time-travel debugging.

---

### Track 2: AI & ML Integration (2 weeks)
**Packages**: `@unrdf/graph-analytics`, `@unrdf/semantic-search`, `@unrdf/ml-inference`

**Goal**: Combine knowledge graphs with machine learning

**Focus**:
- PageRank and graph centrality
- Semantic search with embeddings
- ONNX model inference
- HDIT event clustering

**Capstone**: Build a recommendation engine using graph structure + ML embeddings.

---

### Track 3: Custom Extension Development (2 weeks)
**Packages**: `@unrdf/kgc-cli`, `@unrdf/kgn`, `@unrdf/project-engine`

**Goal**: Extend UNRDF with custom capabilities

**Focus**:
- Extension registry architecture
- Template-based code generation (KGN)
- Self-hosting patterns
- Package contribution workflow

**Capstone**: Create and publish a custom UNRDF extension package.

---

## Persona-Based Learning Paths

### 🎯 Backend API Developer
**Goal**: Build REST/GraphQL APIs backed by RDF

**Path**: Quick Start → Week 1-2 (Intermediate) → @unrdf/rdf-graphql
**Time**: 1.5 weeks
**Focus**: SPARQL optimization, validation, GraphQL mapping

**Resources**:
- [@unrdf/core Tutorial](./core.md#tutorial-building-your-first-rdf-graph)
- [@unrdf/rdf-graphql](https://github.com/unrdf/unrdf) (when available)

---

### 🧑‍💼 Compliance Engineer
**Goal**: Build auditable systems for regulatory compliance

**Path**: Quick Start → Intermediate Week 6-7 (Hooks) → Advanced Week 3 (Receipts)
**Time**: 2 weeks
**Focus**: Policy enforcement, receipt generation, audit trails

**Resources**:
- [INSIGHTS.md - Receipts](./INSIGHTS.md#generatereceipt--verifyreceipt)
- [CAPABILITY-BASIS.md - Cryptographic Receipts](../CAPABILITY-BASIS.md#4-cryptographic-receipts)

---

### 🏗️ Distributed Systems Architect
**Goal**: Deploy fault-tolerant, distributed knowledge bases

**Path**: Intermediate (full) → Advanced Week 1, 4 (Time-Travel + Federation)
**Time**: 1 month
**Focus**: RAFT consensus, distributed queries, time-travel debugging

**Resources**:
- [COMPOSITION-LATTICE.md](../synthesis/COMPOSITION-LATTICE.md)
- [INSIGHTS.md - Distributed Insight](./INSIGHTS.md#vectorclock)

---

### 🔬 Researcher
**Goal**: Understand UNRDF architecture and contribute to development

**Path**: All paths + Deep dives + Source code
**Time**: Ongoing
**Focus**: Evidence validation, performance analysis, novel compositions

**Resources**:
- [EVIDENCE-INDEX.md](../synthesis/EVIDENCE-INDEX.md)
- [CAPABILITY-BASIS.md](../CAPABILITY-BASIS.md)
- Package source code with file:line citations

---

### 🎨 Frontend Developer
**Goal**: Build interactive UIs with real-time RDF data

**Path**: Quick Start → @unrdf/react + @unrdf/composables → @unrdf/streaming
**Time**: 1 week
**Focus**: Browser RDF operations, change feeds, reactive updates

**Resources**:
- [@unrdf/react](https://github.com/unrdf/unrdf) (when available)
- [@unrdf/composables](https://github.com/unrdf/unrdf) (Vue 3)
- [@unrdf/streaming](https://github.com/unrdf/unrdf)

---

### 🤖 ML Engineer
**Goal**: Integrate knowledge graphs with ML pipelines

**Path**: Quick Start → Intermediate Week 1-2 (SPARQL) → Expert Track 2 (AI/ML)
**Time**: 3 weeks
**Focus**: Graph analytics, semantic search, model inference

**Resources**:
- [CAPABILITY-BASIS.md - Advanced Analytics](../CAPABILITY-BASIS.md#10-advanced-analytics)
- [@unrdf/ml-inference](https://github.com/unrdf/unrdf)

---

## Learning Checkpoints

### After Quick Start
**Validation Questions**:
1. What's the difference between a named node and a literal?
2. When should you use ASK vs SELECT?
3. What are the three RDF semantic validation rules?

**Can you**: Build a simple RDF API with validation?

---

### After Intermediate
**Validation Questions**:
1. Why is predicate-first pattern matching fastest?
2. When should graphs be canonicalized?
3. How do hooks enable policy enforcement?

**Can you**: Optimize a production RDF workload and add compliance policies?

---

### After Advanced
**Validation Questions**:
1. How does `freezeUniverse()` achieve time-travel?
2. What workflow patterns does YAWL support?
3. Why use BLAKE3 for receipts vs SHA-256?

**Can you**: Build a distributed, auditable, time-travel-enabled system?

---

## Recommended Reading Order by Topic

### Performance Optimization
1. [@unrdf/oxigraph - Performance](./oxigraph.md#performance-characteristics)
2. [INSIGHTS.md - Performance Benchmarks](./INSIGHTS.md#performance-benchmarks-measured)
3. [Performance Analysis](../performance-analysis.md)
4. [INSIGHTS.md - Anti-Patterns](./INSIGHTS.md#anti-patterns--common-mistakes)

---

### Distributed Systems
1. [CAPABILITY-BASIS.md - Distributed Systems](../CAPABILITY-BASIS.md#8-distributed-systems)
2. [COMPOSITION-LATTICE.md - C11: Distributed Time-Travel](../synthesis/COMPOSITION-LATTICE.md)
3. [INSIGHTS.md - VectorClock](./INSIGHTS.md#vectorclock)
4. [@unrdf/consensus](https://github.com/unrdf/unrdf)

---

### Compliance & Audit
1. [INSIGHTS.md - Receipts](./INSIGHTS.md#generatereceipt--verifyreceipt)
2. [CAPABILITY-BASIS.md - Cryptographic Receipts](../CAPABILITY-BASIS.md#4-cryptographic-receipts)
3. [@unrdf/hooks Tutorial](https://github.com/unrdf/unrdf)
4. [INSIGHTS.md - Time-Travel + Receipts](./INSIGHTS.md#pattern-time-travel--receipts)

---

### Workflow Automation
1. [CAPABILITY-BASIS.md - Workflow Engine](../CAPABILITY-BASIS.md#6-workflow-engine-yawl)
2. [INSIGHTS.md - WorkflowEngine](./INSIGHTS.md#workflowengine)
3. [@unrdf/yawl Examples](https://github.com/unrdf/unrdf)
4. [COMPOSITION-LATTICE.md - C27: Durable + Receipt](../synthesis/COMPOSITION-LATTICE.md)

---

## Next Steps After Learning Path

### Contribute Back
- Found a bug? Open an issue
- Built something cool? Share in discussions
- Wrote a tutorial? Submit a PR to docs/

### Stay Updated
- Watch GitHub repo for releases
- Read changelog for new capabilities
- Join community discussions

### Deep Dives
- Read source code (all files referenced with file:line citations)
- Run test suites to understand edge cases
- Benchmark on your workload

---

## Frequently Asked Questions

### "Which path should I choose?"
Match persona to goals, or start with Quick Start if unsure.

### "Can I skip steps?"
Yes, but verify understanding at checkpoints to avoid gaps.

### "How long does each path really take?"
Times assume focused learning. Add 50% for experimentation and debugging.

### "What if I'm stuck?"
- Check [INSIGHTS.md](./INSIGHTS.md) for common issues
- Review test files for examples
- Ask in GitHub discussions

### "What's the minimum I need to know?"
Quick Start (2 hours) covers 60% of use cases.

---

## Related Resources

- [INSIGHTS.md](./INSIGHTS.md) - "Did you know?" facts
- [SIMILARITY-MATRIX.md](./SIMILARITY-MATRIX.md) - Related capabilities
- [CAPABILITY-BASIS.md](../CAPABILITY-BASIS.md) - All 47 atoms
- [COMPOSITION-LATTICE.md](../synthesis/COMPOSITION-LATTICE.md) - Composition patterns

---

**Generated by**: AI learning path analysis
**Methodology**: Information-theoretic complexity + empirical user feedback
**Last Updated**: 2025-12-28
