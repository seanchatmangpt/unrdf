# UNRDF Documentation Map

Welcome! This page helps you find exactly what you need in the UNRDF documentation.

---

## 🎯 Choose Your Path

### 👶 New to UNRDF?

**Start here:** [START-HERE.md](START-HERE.md) (5 minutes)

Then follow this path:
1. **[Getting Started Tutorial](TUTORIALS/getting-started.md)** - Your first SPARQL query
2. **[First RDF App](TUTORIALS/first-rdf-app.md)** - Build a simple app
3. **[Core Package Docs](../packages/core/docs/TUTORIALS/)** - Learn @unrdf/core in depth

### 🏗️ Building an Application

**Quick start:** [Quick-Start Tutorial](TUTORIALS/getting-started.md)

Then solve your specific problem:
- Want to store data persistently? → [How-To: Using Oxigraph](../packages/oxigraph/docs/HOW-TO/)
- Want reactive behaviors? → [How-To: Defining Hooks](../packages/hooks/docs/HOW-TO/)
- Need large graph support? → [How-To: Streaming Large Graphs](../packages/streaming/docs/HOW-TO/)
- Building a React app? → [How-To: React Integration](../packages/react/docs/HOW-TO/)

### 📚 Looking Up API Details

**Find it here:** [REFERENCE section](#reference) below

By package:
- [@unrdf/core API](../packages/core/docs/REFERENCE/API.md)
- [@unrdf/hooks API](../packages/hooks/docs/REFERENCE/API.md)
- [@unrdf/streaming API](../packages/streaming/docs/REFERENCE/API.md)
- [All packages →](PACKAGE-GUIDES/package-index.md)

### 💡 Understanding How It Works

**Read these:**
- [System Architecture](EXPLANATION/architecture-overview.md) - How UNRDF is organized
- [Design Decisions](EXPLANATION/design-decisions.md) - Why we chose this approach
- [@unrdf/core: How Queries Work](../packages/core/docs/EXPLANATION/sparql-execution.md)
- [@unrdf/hooks: Hook Architecture](../packages/hooks/docs/EXPLANATION/architecture.md)

### 🤝 Contributing to UNRDF

**Read these first:**
1. [Monorepo Structure](MONOREPO-QUICK-REFERENCE.md) - Overview of 17 packages
2. [Local Development](LOCAL-DEVELOPMENT.md) - Set up your environment
3. [Workspace Structure](WORKSPACE-STRUCTURE.md) - File layout
4. [Package Development](PACKAGE-DEVELOPMENT.md) - Create/modify packages
5. [Testing Strategy](TESTING-STRATEGY.md) - How to test
6. [Contributing Guide](../CONTRIBUTING.md) - Code of conduct & process

---

## 📁 Documentation by Type

### 🎓 Tutorials (Learn by Doing)

**Root-level tutorials:**
- [Getting Started](TUTORIALS/getting-started.md) - Your first SPARQL query (15 min)
- [Build Your First App](TUTORIALS/first-rdf-app.md) - Complete working example (30 min)
- [Knowledge Hooks Deep Dive](TUTORIALS/knowledge-hooks.md) - Autonomous behaviors (20 min)
- [Federated Queries](TUTORIALS/federated-queries.md) - Query multiple stores (25 min)

**By package:**
| Package | Tutorial | Time |
|---------|----------|------|
| [@unrdf/core](../packages/core/docs/TUTORIALS/) | Getting started with RDF | 15 min |
| [@unrdf/hooks](../packages/hooks/docs/TUTORIALS/) | Creating your first hook | 15 min |
| [@unrdf/streaming](../packages/streaming/docs/TUTORIALS/) | Processing large graphs | 20 min |
| [@unrdf/browser](../packages/browser/docs/TUTORIALS/) | Running UNRDF in browser | 20 min |
| [@unrdf/react](../packages/react/docs/TUTORIALS/) | React integration | 15 min |
| [All packages →](PACKAGE-GUIDES/package-index.md) | See all package tutorials | — |

### 🔧 How-To Guides (Problem Solving)

**Common problems:**
- [Performance Optimization](HOW-TO/performance-optimization.md) - Speed up your queries
- [Debugging](HOW-TO/debugging.md) - Debug across packages
- [Deployment](HOW-TO/deployment.md) - Deploy UNRDF apps
- [Troubleshooting](HOW-TO/troubleshooting.md) - Common issues
- [Migrating Data](HOW-TO/migrating-data.md) - Import RDF from other sources

**By package:**
| Package | How-To Guides | Examples |
|---------|---------------|----------|
| [@unrdf/core](../packages/core/docs/HOW-TO/) | SPARQL, validation, formats | Query optimization, SHACL |
| [@unrdf/hooks](../packages/hooks/docs/HOW-TO/) | Hook patterns, composition | Debugging, performance |
| [@unrdf/streaming](../packages/streaming/docs/HOW-TO/) | Large graphs, backpressure | Memory optimization |
| [@unrdf/federation](../packages/federation/docs/HOW-TO/) | Multi-store queries | Cross-database joins |
| [All packages →](PACKAGE-GUIDES/package-index.md) | Complete list | — |

### 📖 Reference (Complete Information)

**API References:**
- [@unrdf/core API](../packages/core/docs/REFERENCE/API.md) - RDF operations
- [@unrdf/hooks API](../packages/hooks/docs/REFERENCE/API.md) - Hook definitions
- [@unrdf/streaming API](../packages/streaming/docs/REFERENCE/API.md) - Streaming API
- [@unrdf/browser API](../packages/browser/docs/REFERENCE/API.md) - Browser API
- [@unrdf/react API](../packages/react/docs/REFERENCE/API.md) - React hooks
- [@unrdf/cli API](../packages/cli/docs/REFERENCE/API.md) - CLI commands
- [All APIs →](REFERENCE/API-INDEX.md)

**Query & Schema References:**
- [SPARQL Query Reference](REFERENCE/SPARQL-REFERENCE.md) - Query language
- [SHACL Validation Reference](REFERENCE/SHACL-REFERENCE.md) - Validation shapes
- [CLI Command Reference](../packages/cli/docs/REFERENCE/API.md) - Command reference
- [Error Reference](HOW-TO/troubleshooting.md#error-codes) - Error codes

**Configuration:**
- [@unrdf/core Configuration](../packages/core/docs/REFERENCE/CONFIGURATION.md)
- [Oxigraph Configuration](../packages/oxigraph/docs/REFERENCE/CONFIGURATION.md)
- [All configs →](REFERENCE/) - By package

**Type Definitions:**
- [@unrdf/core Types](../packages/core/docs/REFERENCE/TYPES.md)
- [@unrdf/hooks Types](../packages/hooks/docs/REFERENCE/TYPES.md)
- [All types →](REFERENCE/)

### 💡 Explanation (Understanding Concepts)

**System-wide concepts:**
- [Architecture Overview](EXPLANATION/architecture-overview.md) - How UNRDF is organized
- [Design Decisions](EXPLANATION/design-decisions.md) - Why this approach
- [Data Flow](EXPLANATION/data-flow.md) - How data moves through the system
- [Monorepo Structure](EXPLANATION/monorepo-structure.md) - Why 17 packages?
- [Comparison with Alternatives](EXPLANATION/comparison-with-alternatives.md) - vs GraphDB, Virtuoso, etc.

**By concept:**
| Concept | Explanation | Related |
|---------|-------------|---------|
| RDF & Triples | [What is RDF?](EXPLANATION/rdf-concepts.md) | [SPARQL Guide](REFERENCE/SPARQL-REFERENCE.md) |
| Knowledge Hooks | [How hooks work](../packages/hooks/docs/EXPLANATION/) | [Hook tutorial](../packages/hooks/docs/TUTORIALS/) |
| SPARQL | [Query execution](../packages/core/docs/EXPLANATION/sparql-execution.md) | [SPARQL reference](REFERENCE/SPARQL-REFERENCE.md) |
| Streaming | [Stream design](../packages/streaming/docs/EXPLANATION/) | [Streaming tutorial](../packages/streaming/docs/TUTORIALS/) |
| Federation | [Federated queries](../packages/federation/docs/EXPLANATION/) | [Federation guide](../packages/federation/docs/HOW-TO/) |

---

## 📦 By Package

All 17 packages organized by documentation type:

### Essential Packages
- **[@unrdf/core](../packages/core/docs/)** - RDF storage, SPARQL, SHACL
  - [Tutorial](../packages/core/docs/TUTORIALS/) | [How-To](../packages/core/docs/HOW-TO/) | [Reference](../packages/core/docs/REFERENCE/) | [Explanation](../packages/core/docs/EXPLANATION/)

- **[@unrdf/oxigraph](../packages/oxigraph/docs/)** - Persistent storage backend
  - [Tutorial](../packages/oxigraph/docs/TUTORIALS/) | [How-To](../packages/oxigraph/docs/HOW-TO/) | [Reference](../packages/oxigraph/docs/REFERENCE/)

- **[@unrdf/hooks](../packages/hooks/docs/)** - Autonomous behaviors
  - [Tutorial](../packages/hooks/docs/TUTORIALS/) | [How-To](../packages/hooks/docs/HOW-TO/) | [Reference](../packages/hooks/docs/REFERENCE/) | [Explanation](../packages/hooks/docs/EXPLANATION/)

### Extended Features
- [@unrdf/streaming](../packages/streaming/docs/) - Large graphs
- [@unrdf/federation](../packages/federation/docs/) - Distributed queries
- [@unrdf/knowledge-engine](../packages/knowledge-engine/docs/) - Inference & reasoning
- [@unrdf/browser](../packages/browser/docs/) - Browser runtime
- [@unrdf/cli](../packages/cli/docs/) - Command-line interface
- [@unrdf/react](../packages/react/docs/) - React integration

### Optional/Alpha
- [@unrdf/composables](../packages/composables/docs/) - Vue composables
- [@unrdf/dark-matter](../packages/dark-matter/docs/) - Query optimization
- [@unrdf/project-engine](../packages/project-engine/docs/) - Workspace management
- [@unrdf/engine-gateway](../packages/engine-gateway/docs/) - API gateway

### Internal Packages
- [@unrdf/domain](../packages/domain/docs/) - Type definitions
- [@unrdf/test-utils](../packages/test-utils/docs/) - Testing infrastructure
- [@unrdf/validation](../packages/validation/docs/) - Validation framework

👉 [Full package guide →](PACKAGE-GUIDES/package-index.md)

---

## 🎯 By Use Case

### Use Case: Building a Blog Platform
1. [Tutorial: First RDF App](TUTORIALS/first-rdf-app.md) - Get started
2. [@unrdf/core: How-To](../packages/core/docs/HOW-TO/) - Query & store posts
3. [@unrdf/react: Integration](../packages/react/docs/TUTORIALS/) - Build UI
4. [How-To: Deployment](HOW-TO/deployment.md) - Deploy it

### Use Case: Knowledge Management System
1. [Tutorial: Knowledge Hooks](TUTORIALS/knowledge-hooks.md) - Learn hooks
2. [@unrdf/hooks: Deep Dive](../packages/hooks/docs/TUTORIALS/) - Advanced hooks
3. [@unrdf/knowledge-engine: Reference](../packages/knowledge-engine/docs/REFERENCE/) - Inference
4. [How-To: Performance](HOW-TO/performance-optimization.md) - Optimize

### Use Case: Real-Time Collaboration
1. [@unrdf/streaming: Tutorial](../packages/streaming/docs/TUTORIALS/) - Learn streaming
2. [@unrdf/hooks: Reactive Patterns](../packages/hooks/docs/HOW-TO/) - Event handling
3. [@unrdf/browser: Integration](../packages/browser/docs/) - Client-side sync
4. [Tutorial: Federated Queries](TUTORIALS/federated-queries.md) - Multi-store

### Use Case: Semantic Search Engine
1. [Tutorial: Getting Started](TUTORIALS/getting-started.md) - Basics
2. [@unrdf/federation: Distributed Search](../packages/federation/docs/TUTORIALS/) - Multi-store queries
3. [@unrdf/knowledge-engine: Inference](../packages/knowledge-engine/docs/) - Reasoning
4. [How-To: Performance](HOW-TO/performance-optimization.md) - Speed it up

---

## 🔍 Search by Topic

### RDF & Data
- [What is RDF?](EXPLANATION/rdf-concepts.md)
- [@unrdf/core: Format Support](../packages/core/docs/REFERENCE/API.md#formats)
- [Parsing & Serialization](../packages/core/docs/HOW-TO/)

### Querying
- [SPARQL Query Reference](REFERENCE/SPARQL-REFERENCE.md)
- [@unrdf/core: SPARQL Tutorial](../packages/core/docs/TUTORIALS/02-basic-workflow.md)
- [@unrdf/federation: Distributed Queries](../packages/federation/docs/TUTORIALS/)

### Validation
- [SHACL Validation Reference](REFERENCE/SHACL-REFERENCE.md)
- [@unrdf/core: SHACL How-To](../packages/core/docs/HOW-TO/)
- [Validation Concepts](EXPLANATION/validation.md)

### Performance
- [Performance Optimization Guide](HOW-TO/performance-optimization.md)
- [@unrdf/streaming: Large Graphs](../packages/streaming/docs/)
- [@unrdf/dark-matter: Query Optimization](../packages/dark-matter/docs/)

### Persistence
- [@unrdf/oxigraph: Storage Backend](../packages/oxigraph/docs/)
- [How-To: Persistent Storage](../packages/core/docs/HOW-TO/)

### Reactivity & Automation
- [@unrdf/hooks: Complete Guide](../packages/hooks/docs/)
- [Tutorial: Knowledge Hooks](TUTORIALS/knowledge-hooks.md)

### Integration
- [@unrdf/react: React Integration](../packages/react/docs/)
- [@unrdf/browser: Client-Side UNRDF](../packages/browser/docs/)
- [@unrdf/cli: Command-Line Tools](../packages/cli/docs/)

### Deployment & DevOps
- [Deployment How-To](HOW-TO/deployment.md)
- [Docker Integration](HOW-TO/deployment.md#docker)
- [Production Checklist](HOW-TO/deployment.md#production)

---

## 🛠️ For Contributors

**Want to contribute to UNRDF?**

1. [Monorepo Overview](MONOREPO-QUICK-REFERENCE.md) - What are the 17 packages?
2. [Local Development](LOCAL-DEVELOPMENT.md) - Set up your dev environment
3. [Workspace Structure](WORKSPACE-STRUCTURE.md) - Where things go
4. [Package Development](PACKAGE-DEVELOPMENT.md) - Create/modify packages
5. [Testing Strategy](TESTING-STRATEGY.md) - How to test
6. [Contributing Guide](../CONTRIBUTING.md) - Code standards & process

**Want to improve documentation?**

1. [Diataxis Plan](DIATAXIS-PLAN.md) - Long-term docs strategy
2. [Diataxis Guide](DIATAXIS-GUIDE.md) - How to write each doc type
3. [PACKAGE-INDEX Template](/_templates/PACKAGE-INDEX.md) - Template for package docs

---

## 📚 Documentation Organization

```
docs/
├── 📋 This file (DIATAXIS-MAP.md)
│
├── TUTORIALS/                  # Learning by doing
│   ├── getting-started.md
│   ├── first-rdf-app.md
│   └── ...
│
├── HOW-TO/                     # Problem solving
│   ├── performance-optimization.md
│   ├── debugging.md
│   └── ...
│
├── REFERENCE/                  # Complete information
│   ├── API-INDEX.md
│   ├── SPARQL-REFERENCE.md
│   └── ...
│
├── EXPLANATION/                # Understanding concepts
│   ├── architecture-overview.md
│   ├── design-decisions.md
│   └── ...
│
├── MONOREPO-QUICK-REFERENCE.md # Package overview
├── LOCAL-DEVELOPMENT.md        # Dev setup
├── WORKSPACE-STRUCTURE.md      # File layout
├── PACKAGE-DEVELOPMENT.md      # Create packages
├── TESTING-STRATEGY.md         # Testing
│
├── DIATAXIS-PLAN.md           # Implementation plan
├── DIATAXIS-GUIDE.md          # Writing guide
│
└── _templates/                # Document templates
    ├── PACKAGE-INDEX.md
    ├── TUTORIAL.md
    ├── HOW-TO.md
    ├── REFERENCE.md
    └── EXPLANATION.md
```

---

## ⚡ Quick Links

| Need | Find here | Time |
|------|-----------|------|
| **Get started quickly** | [Getting Started Tutorial](TUTORIALS/getting-started.md) | 15 min |
| **Build something** | [How-To Guides](#-how-to-guides-problem-solving) | 5-10 min |
| **Look up an API** | [API References](#-reference-complete-information) | 2 min |
| **Understand the design** | [Explanations](#-explanation-understanding-concepts) | 15 min |
| **Contribute code** | [Contributing](../CONTRIBUTING.md) | 30 min |
| **Set up dev environment** | [Local Development](LOCAL-DEVELOPMENT.md) | 15 min |
| **Install UNRDF** | [Getting Started](TUTORIALS/getting-started.md#installation) | 5 min |
| **Find error codes** | [Troubleshooting](HOW-TO/troubleshooting.md) | 5 min |
| **Optimize performance** | [Performance Guide](HOW-TO/performance-optimization.md) | 20 min |
| **Deploy an app** | [Deployment](HOW-TO/deployment.md) | 30 min |

---

## 🎓 Recommended Reading Order by Role

### **Role: End User (Using UNRDF)**
1. START-HERE.md (5 min)
2. Getting Started Tutorial (15 min)
3. Choose based on your use case
4. Use how-to guides to solve problems
5. Reference for details

### **Role: JavaScript Developer**
1. START-HERE.md (5 min)
2. First RDF App Tutorial (30 min)
3. Package-specific tutorials as needed
4. API references for implementation
5. How-to guides for problems

### **Role: Contributor**
1. MONOREPO-QUICK-REFERENCE.md (10 min)
2. LOCAL-DEVELOPMENT.md (15 min)
3. Package documentation for the package you're contributing to
4. CONTRIBUTING.md for standards
5. Code-specific docs as needed

### **Role: DevOps/SRE**
1. Architecture Overview (15 min)
2. Deployment Guide (30 min)
3. Performance Optimization (20 min)
4. @unrdf/cli: CLI Reference
5. Troubleshooting & monitoring

---

## 📞 Getting Help

- **📚 Can't find something?** Use the search above or check [PACKAGE-GUIDES](PACKAGE-GUIDES/)
- **🤔 Have a question?** [GitHub Discussions](https://github.com/unrdf/unrdf/discussions)
- **🐛 Found a bug?** [GitHub Issues](https://github.com/unrdf/unrdf/issues)
- **💬 Want to chat?** [Discord Community](https://discord.gg/unrdf)

---

**Last updated:** 2025-12-05
**Documentation status:** 🟢 Diataxis implementation in progress
**Version:** UNRDF 5.0.0-alpha.0

---

**Ready?** Pick your path above and get started! 👆
