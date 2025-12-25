# UNRDF Documentation Hub

**Quick navigation to all UNRDF documentation.**

---

## 🚀 Getting Started (Start Here!)

| Document | Purpose | Time |
|----------|---------|------|
| **[QUICK-START.md](QUICK-START.md)** | 5-minute guide with working examples | 5 min |
| **[API-REFERENCE.md](API-REFERENCE.md)** | Top 20% of APIs (80% usage) | 15 min |
| **[MIGRATION.md](MIGRATION.md)** | Migrate from N3, RDFLib, Jena | 10 min |
| **[ARCHITECTURE.md](ARCHITECTURE.md)** | System design and structure | 20 min |

---

## 📦 Package Documentation

### Essential Packages (Most Users Need These)

| Package | Description | Docs |
|---------|-------------|------|
| **@unrdf/core** | RDF storage, SPARQL, validation | [packages/core/README.md](../packages/core/README.md) |
| **@unrdf/oxigraph** | High-performance Rust triple store | [packages/oxigraph/README.md](../packages/oxigraph/README.md) |
| **@unrdf/hooks** | Reactive knowledge behaviors | [packages/hooks/README.md](../packages/hooks/README.md) |
| **@unrdf/kgc-4d** | Event sourcing with time-travel | [packages/kgc-4d/README.md](../packages/kgc-4d/README.md) |
| **@unrdf/yawl** | Workflow engine (Van der Aalst) | [packages/yawl/README.md](../packages/yawl/README.md) |

### Extended Packages (Optional)

| Package | Description | Docs |
|---------|-------------|------|
| **@unrdf/streaming** | Large graph streaming | [packages/streaming/README.md](../packages/streaming/README.md) |
| **@unrdf/federation** | Distributed query execution | [packages/federation/README.md](../packages/federation/README.md) |
| **@unrdf/cli** | Command-line tools | [packages/cli/README.md](../packages/cli/README.md) |

**See all packages:** [PACKAGES.md](PACKAGES.md)

---

## 🎓 Architecture Decision Records (ADRs)

**Understand WHY UNRDF was built this way.**

| ADR | Decision | Status |
|-----|----------|--------|
| **[ADR-001](adr/001-oxigraph-over-n3.md)** | Oxigraph over N3 for RDF storage | ✅ Accepted |
| **[ADR-002](adr/002-yawl-workflow-patterns.md)** | YAWL over BPMN for workflows | ✅ Accepted |
| **[ADR-003](adr/003-otel-observability.md)** | OpenTelemetry for observability | ✅ Accepted |
| **[ADR-004](adr/004-bigbang-80-20-methodology.md)** | Big Bang 80/20 development methodology | ✅ Accepted |
| **[ADR-005](adr/005-hook-native-reactive.md)** | Hook-native reactive architecture | ✅ Accepted |

---

## 📖 Tutorials

| Tutorial | Level | Time |
|----------|-------|------|
| **[Build a Knowledge Graph](tutorials/build-knowledge-graph.md)** | Beginner | 30 min |
| **[SPARQL Queries 101](tutorials/sparql-queries.md)** | Beginner | 20 min |
| **[Hook-Based Validation](tutorials/hook-validation.md)** | Intermediate | 25 min |
| **[Workflow Orchestration](tutorials/workflow-orchestration.md)** | Intermediate | 40 min |
| **[Event Sourcing with KGC-4D](tutorials/event-sourcing.md)** | Advanced | 45 min |

**See all tutorials:** [tutorials/](tutorials/)

---

## 🔍 How-To Guides

**Task-oriented guides for specific problems.**

| Guide | Purpose |
|-------|---------|
| **[Query RDF Data](how-to/query-rdf-data.md)** | SPARQL queries, filtering, joins |
| **[Validate Data with Hooks](how-to/validate-with-hooks.md)** | Policy enforcement, data quality |
| **[Build Workflows](how-to/build-workflows.md)** | Multi-step business processes |
| **[Time-Travel Debugging](how-to/time-travel-debugging.md)** | Reconstruct past states |
| **[Deploy to Production](how-to/deploy-production.md)** | Docker, Kubernetes, monitoring |

**See all guides:** [how-to/](how-to/)

---

## 📚 Reference Documentation

| Reference | Purpose |
|-----------|---------|
| **[API Reference](API-REFERENCE.md)** | Top 20% of APIs |
| **[SPARQL Reference](reference/sparql-reference.md)** | SPARQL 1.1 syntax |
| **[Hook Schema Reference](reference/hook-schema.md)** | Hook definition schemas |
| **[YAWL Patterns Reference](reference/yawl-patterns.md)** | Van der Aalst workflow patterns |
| **[RDF Vocabularies](reference/rdf-vocabularies.md)** | FOAF, DCTERMS, SKOS, etc. |

---

## 🧪 Examples

**Copy-paste ready code examples.**

| Example | Description |
|---------|-------------|
| **[Basic SPARQL](../examples/basic-sparql.mjs)** | Load + query RDF data |
| **[Email Validation Hook](../examples/email-validation-hook.mjs)** | Validate email format |
| **[Order Fulfillment Workflow](../examples/order-workflow.mjs)** | Multi-step workflow |
| **[Event Audit Trail](../examples/event-audit.mjs)** | KGC-4D event sourcing |
| **[Federated Query](../examples/federated-query.mjs)** | Query multiple stores |

**See all examples:** [examples/](../examples/)

---

## 🏗️ Architecture

| Document | Purpose |
|----------|---------|
| **[Architecture Overview](ARCHITECTURE.md)** | System design principles |
| **[Monorepo Structure](WORKSPACE-STRUCTURE.md)** | File layout, naming conventions |
| **[Package Development](PACKAGE-DEVELOPMENT.md)** | Create new packages |
| **[Testing Strategy](TESTING-STRATEGY.md)** | Test approach, coverage |

---

## 🛠️ Development

**For contributors and advanced users.**

| Document | Purpose |
|----------|---------|
| **[Local Development](LOCAL-DEVELOPMENT.md)** | Setup dev environment |
| **[Contributing Guide](CONTRIBUTING.md)** | How to contribute |
| **[Testing Guide](TESTING.md)** | Run and write tests |
| **[Big Bang 80/20 Methodology](bb80-20-methodology.md)** | Development philosophy |

---

## 📊 Performance & Benchmarks

| Document | Purpose |
|----------|---------|
| **[Performance Analysis](../PERFORMANCE-ANALYSIS.md)** | Benchmark results |
| **[Optimization Guide](guides/optimization.md)** | Performance tuning |
| **[Benchmarking Suite](../benchmarks/)** | Run benchmarks yourself |

---

## 🔐 Production & Operations

| Document | Purpose |
|----------|---------|
| **[Production Readiness](../PRODUCTION-READINESS-FINAL.md)** | Production checklist |
| **[Deployment Guide](how-to/deploy-production.md)** | Deploy UNRDF apps |
| **[Monitoring with OTEL](guides/otel-monitoring.md)** | Observability setup |
| **[Security Best Practices](guides/security.md)** | Secure your deployment |

---

## ❓ Troubleshooting

| Document | Purpose |
|----------|---------|
| **[Common Issues](TROUBLESHOOTING.md)** | FAQ and solutions |
| **[Debug Guide](guides/debugging.md)** | Debug UNRDF apps |
| **[OTEL Validation](guides/otel-validation.md)** | Verify with OTEL |

---

## 📜 Thesis & Research

| Document | Purpose |
|----------|---------|
| **[PhD Thesis (Final)](thesis-publication/PHD-THESIS-UNRDF-2028-REVOLUTION-FINAL.md)** | Complete research thesis |
| **[Big Bang 80/20 Methodology](bb80-20-methodology.md)** | Development methodology |
| **[Hyper-Frameworks](HYPER-FRAMEWORKS.md)** | Framework design patterns |

---

## 🗺️ Documentation Map

### By User Type

**🟢 New Users** (never used UNRDF):
1. [QUICK-START.md](QUICK-START.md) - 5 minutes
2. [API-REFERENCE.md](API-REFERENCE.md) - 15 minutes
3. [examples/](../examples/) - Copy-paste code

**🟡 Migrating Users** (from N3, RDFLib, etc.):
1. [MIGRATION.md](MIGRATION.md) - 10 minutes
2. [ADR-001: Oxigraph vs N3](adr/001-oxigraph-over-n3.md) - Why different
3. [API-REFERENCE.md](API-REFERENCE.md) - New APIs

**🔵 Advanced Users** (building production apps):
1. [ARCHITECTURE.md](ARCHITECTURE.md) - System design
2. [ADRs](adr/) - Understand decisions
3. [Production Guide](../PRODUCTION-READINESS-FINAL.md) - Deploy
4. [OTEL Monitoring](guides/otel-monitoring.md) - Observability

**🟣 Contributors** (want to help develop UNRDF):
1. [CONTRIBUTING.md](CONTRIBUTING.md) - Contribution guide
2. [LOCAL-DEVELOPMENT.md](LOCAL-DEVELOPMENT.md) - Setup
3. [Big Bang 80/20](bb80-20-methodology.md) - Methodology
4. [TESTING-STRATEGY.md](TESTING-STRATEGY.md) - Tests

---

### By Task

**"I want to query RDF data"**
→ [QUICK-START.md § SPARQL](QUICK-START.md#querying-with-sparql)
→ [API-REFERENCE.md § Core](API-REFERENCE.md#-unrdfcore---rdf-operations)

**"I want to build workflows"**
→ [QUICK-START.md § YAWL](QUICK-START.md#4-yawl-workflow-engine)
→ [ADR-002: Why YAWL](adr/002-yawl-workflow-patterns.md)

**"I want to validate data"**
→ [QUICK-START.md § Hooks](QUICK-START.md#3-hooks-reactive-knowledge-behaviors)
→ [Hook Validation Guide](how-to/validate-with-hooks.md)

**"I want event sourcing"**
→ [QUICK-START.md § KGC-4D](QUICK-START.md#2-kgc-4d-time-travel-event-sourcing)
→ [Event Sourcing Tutorial](tutorials/event-sourcing.md)

**"I want to deploy to production"**
→ [Production Readiness](../PRODUCTION-READINESS-FINAL.md)
→ [Deployment Guide](how-to/deploy-production.md)

---

## 🔗 External Resources

- **GitHub Repository:** [github.com/unrdf/unrdf](https://github.com/unrdf/unrdf)
- **RDF Specification:** [w3.org/RDF](https://www.w3.org/RDF/)
- **SPARQL 1.1:** [w3.org/TR/sparql11-query](https://www.w3.org/TR/sparql11-query/)
- **YAWL Website:** [yawlfoundation.org](http://www.yawlfoundation.org/)
- **OpenTelemetry:** [opentelemetry.io](https://opentelemetry.io/)

---

## 📝 Documentation Status

| Document Type | Count | Status |
|---------------|-------|--------|
| **Quick Start** | 1 | ✅ Complete |
| **API Reference** | 1 | ✅ Complete |
| **Migration Guides** | 1 | ✅ Complete |
| **ADRs** | 5 | ✅ Complete |
| **Tutorials** | 5+ | ⏳ In Progress |
| **How-To Guides** | 5+ | ⏳ In Progress |
| **Examples** | 10+ | ✅ Complete |

**Last Updated:** 2024-12-25

---

## 🤔 Can't Find What You Need?

1. **Search docs:** Use browser find (Cmd+F / Ctrl+F)
2. **Check examples:** [examples/](../examples/)
3. **Ask community:** [GitHub Discussions](https://github.com/unrdf/unrdf/discussions)
4. **File issue:** [GitHub Issues](https://github.com/unrdf/unrdf/issues)

---

**Happy coding! 🚀**
