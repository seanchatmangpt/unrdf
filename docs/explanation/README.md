# UNRDF Explanation Guides

**Understanding-oriented discussions of concepts, architecture, and design decisions**

Explanation guides help you understand the "why" behind UNRDF's design, architecture, and best practices.

## Core Concepts

### RDF & Semantic Web
- **[Understanding RDF](./understanding-rdf.md)** - The RDF data model explained
- **[Why RDF for Knowledge Graphs?](./why-rdf-knowledge-graphs.md)** - Benefits and tradeoffs
- **[SPARQL Query Language](./sparql-query-language.md)** - Query patterns and semantics
- **[Semantic Web Standards](./semantic-web-standards.md)** - W3C specifications overview

### Knowledge Hooks
- **[Knowledge Hooks Philosophy](./knowledge-hooks-philosophy.md)** - Design principles and goals
- **[Hook Execution Model](./hook-execution-model.md)** - How hooks work internally
- **[Hook Composition Patterns](./hook-composition-patterns.md)** - Combining multiple hooks
- **[Isolated-VM Security](./isolated-vm-security.md)** - Sandbox isolation explained

---

## Architecture

### System Design
- **[UNRDF Architecture Overview](./architecture-overview.md)** - High-level system design
- **[Transaction Model](./transaction-model.md)** - ACID properties and implementation
- **[Storage Architecture](./storage-architecture.md)** - Quad storage design
- **[Query Optimization](./query-optimization.md)** - SPARQL execution strategy

### Browser Architecture
- **[Browser Integration Design](./browser-integration-design.md)** - Client-side architecture
- **[IndexedDB Storage Strategy](./indexeddb-storage-strategy.md)** - Browser storage tradeoffs
- **[Cross-Environment Compatibility](./cross-environment-compatibility.md)** - Node.js vs Browser

### Distributed Systems
- **[Federation Architecture](./federation-architecture.md)** - Multi-node design
- **[Consensus Protocols](./consensus-protocols.md)** - Byzantine fault tolerance
- **[Distributed Query Execution](./distributed-query-execution.md)** - Federated query planning

---

## Design Decisions

### Policy & Validation
- **[Policy Pack Design](./policy-pack-design.md)** - Composition and enforcement
- **[SHACL vs Custom Validation](./shacl-vs-custom-validation.md)** - Validation strategy
- **[Real-time Validation Trade-offs](./realtime-validation-tradeoffs.md)** - Performance considerations

### Streaming & Real-time
- **[Change Feed Architecture](./change-feed-architecture.md)** - Event sourcing design
- **[Windowing Strategies](./windowing-strategies.md)** - Stream processing patterns
- **[Backpressure Handling](./backpressure-handling.md)** - Flow control mechanisms

### Observability
- **[OpenTelemetry Integration](./otel-integration.md)** - Why OTEL over custom metrics
- **[Span-based Validation](./span-based-validation.md)** - Production readiness approach
- **[Performance Profiling](./performance-profiling.md)** - Instrumentation strategy

---

## Best Practices

### Development Patterns
- **[Test-Driven Development with UNRDF](./tdd-with-unrdf.md)** - Testing philosophy
- **[Hook Development Best Practices](./hook-development-best-practices.md)** - Writing maintainable hooks
- **[Error Handling Strategies](./error-handling-strategies.md)** - Graceful degradation
- **[Modular Design Principles](./modular-design-principles.md)** - Code organization

### Performance
- **[Query Performance Optimization](./query-performance-optimization.md)** - Optimization strategies
- **[Memory Management](./memory-management.md)** - Resource efficiency
- **[Caching Strategies](./caching-strategies.md)** - Cache invalidation patterns
- **[Bundle Size Optimization](./bundle-size-optimization.md)** - Client-side performance

### Security
- **[Security Model](./security-model.md)** - Threat model and mitigations
- **[Sandbox Isolation](./sandbox-isolation.md)** - Effect execution safety
- **[Access Control Patterns](./access-control-patterns.md)** - Authorization strategies
- **[Cryptographic Provenance](./cryptographic-provenance.md)** - Lockchain integrity

---

## Comparisons

### UNRDF vs Alternatives
- **[UNRDF vs Jena](./unrdf-vs-jena.md)** - Java ecosystem comparison
- **[UNRDF vs RDFLib](./unrdf-vs-rdflib.md)** - Python ecosystem comparison
- **[UNRDF vs Oxigraph](./unrdf-vs-oxigraph.md)** - Rust ecosystem comparison
- **[UNRDF vs GraphDB](./unrdf-vs-graphdb.md)** - Enterprise solution comparison

### Technology Choices
- **[Why JavaScript/Node.js?](./why-javascript-nodejs.md)** - Platform rationale
- **[ESM vs CommonJS](./esm-vs-commonjs.md)** - Module system choice
- **[Isolated-VM vs vm2](./isolated-vm-vs-vm2.md)** - Sandbox comparison
- **[Vitest vs Jest](./vitest-vs-jest.md)** - Testing framework choice

---

## SPARC Methodology

### Development Process
- **[SPARC Overview](./sparc-overview.md)** - Specification, Pseudocode, Architecture, Refinement, Completion
- **[80/20 Principle in Practice](./80-20-principle.md)** - Focus on high-value features
- **[Test-Driven Refinement](./test-driven-refinement.md)** - TDD phase implementation
- **[Continuous Integration](./continuous-integration.md)** - CI/CD best practices

---

## Production Considerations

### Deployment Strategies
- **[Docker Deployment](./docker-deployment.md)** - Container strategy
- **[Kubernetes Patterns](./kubernetes-patterns.md)** - Orchestration design
- **[Terraform Infrastructure](./terraform-infrastructure.md)** - IaC approach
- **[Zero-Downtime Deployments](./zero-downtime-deployments.md)** - Blue-green strategy

### Operational Concerns
- **[Monitoring and Alerting](./monitoring-alerting.md)** - Observability in production
- **[Capacity Planning](./capacity-planning.md)** - Resource sizing
- **[Disaster Recovery](./disaster-recovery.md)** - Backup and restore strategy
- **[High Availability Design](./high-availability-design.md)** - HA patterns

---

## Advanced Topics

### AI & Machine Learning
- **[Semantic Embeddings](./semantic-embeddings.md)** - Vector representations
- **[NLP Integration Patterns](./nlp-integration-patterns.md)** - Natural language processing
- **[Knowledge Graph Reasoning](./knowledge-graph-reasoning.md)** - Inference and entailment
- **[Ontology Engineering](./ontology-engineering.md)** - Schema design

### Research & Innovation
- **[Dark Matter 80/20 Pattern](./dark-matter-80-20.md)** - High-value core philosophy
- **[Knowledge Substrate Optimization](./knowledge-substrate-optimization.md)** - Performance theory
- **[Autonomic Computing](./autonomic-computing.md)** - Self-managing systems
- **[Future Roadmap](../ROADMAP.md)** - Planned innovations

---

## Community & Governance

### Contributing
- **[How UNRDF is Built](./how-unrdf-is-built.md)** - Development process
- **[Contributing Philosophy](../../CONTRIBUTING.md)** - Community values
- **[Code Review Process](./code-review-process.md)** - Quality standards
- **[Release Process](./release-process.md)** - Version management

### Decision Making
- **[Architecture Decision Records](./architecture-decision-records.md)** - ADR process
- **[Feature Proposals](./feature-proposals.md)** - RFC process
- **[Backward Compatibility](./backward-compatibility.md)** - Stability guarantees

---

## Learning Path

New to UNRDF? Follow this learning path:

1. **Start with Concepts:** Read [Understanding RDF](./understanding-rdf.md) and [UNRDF Architecture Overview](./architecture-overview.md)
2. **Learn by Doing:** Complete [Tutorials](../tutorials/README.md)
3. **Solve Problems:** Use [How-to Guides](../how-to/README.md)
4. **Deep Dive:** Return to Explanations for deeper understanding
5. **Reference:** Use [API Documentation](../reference/README.md) as needed

---

## Questions?

- **[FAQ](../FAQ.md)** - Frequently asked questions
- **[Troubleshooting](../TROUBLESHOOTING.md)** - Common problems
- **[GitHub Discussions](https://github.com/unrdf/unrdf/discussions)** - Ask the community
- **[Stack Overflow](https://stackoverflow.com/questions/tagged/unrdf)** - Search existing answers
