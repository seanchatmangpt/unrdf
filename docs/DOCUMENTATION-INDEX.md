# UNRDF Documentation Index

**Complete guide to all UNRDF documentation.**

## 🚀 Getting Started (5 minutes)

**New to UNRDF?** Start here:

1. **[examples/QUICKSTART.md](../examples/QUICKSTART.md)** - 5-minute getting started guide
2. **[examples/01-minimal-parse-query.mjs](../examples/01-minimal-parse-query.mjs)** - Your first UNRDF program (3 min)
3. **[examples/README.md](../examples/README.md)** - Examples index and navigation

## 📚 Core Documentation

### System Overview
- **[examples/ARCHITECTURE.md](../examples/ARCHITECTURE.md)** - System architecture and design patterns
- **[docs/examples/EXAMPLES-GUIDE.md](./examples/EXAMPLES-GUIDE.md)** - Comprehensive examples guide

### Package Documentation
Each package has detailed README with examples:

| Package | Purpose | README | Examples |
|---------|---------|--------|----------|
| **@unrdf/core** | RDF parsing, SPARQL, serialization | [README](../packages/core/README.md) | [01-minimal](../examples/01-minimal-parse-query.mjs), [minimal-core](../examples/minimal-core-example.mjs) |
| **@unrdf/hooks** | Policy enforcement framework | [README](../packages/hooks/README.md) | [basic-hook](../examples/basic-knowledge-hook.mjs), [define-hook](../examples/define-hook-example.mjs) |
| **@unrdf/dark-matter** | Query optimization | [README](../packages/dark-matter/README.md) | [80-20](../examples/dark-matter-80-20.mjs), [optimization](../examples/dark-matter-query-optimization.mjs) |
| **@unrdf/knowledge-engine** | AI semantic analysis | [README](../packages/knowledge-engine/README.md) | [knowledge-engine](../examples/knowledge-engine-example.mjs), [ai-semantic](../examples/ai-semantic-example.mjs) |
| **@unrdf/streaming** | Real-time change feeds | [README](../packages/streaming/README.md) | [streaming/](../examples/streaming/) |
| **@unrdf/browser** | Client-side storage | [README](../packages/browser/README.md) | [browser/](../examples/browser/) |
| **@unrdf/federation** | Distributed queries | [README](../packages/federation/README.md) | [federation/](../examples/federation/) |
| **@unrdf/cli** | Command-line interface | [README](../packages/cli/README.md) | [cli-automation](../examples/cli-automation-script.mjs) |
| **@unrdf/composables** | React/Vue/Svelte hooks | [README](../packages/composables/README.md) | [browser-react](../examples/browser-react.jsx) |
| **@unrdf/project-engine** | Build system integration | [README](../packages/project-engine/README.md) | - |

## 📖 Learning Paths

### Path 1: Beginner (30 minutes)
**Goal:** Understand core concepts

1. Read [QUICKSTART.md](../examples/QUICKSTART.md) (10 min)
2. Run [01-minimal-parse-query.mjs](../examples/01-minimal-parse-query.mjs) (3 min)
3. Run [basic-knowledge-hook.mjs](../examples/basic-knowledge-hook.mjs) (15 min)

**You'll learn:** Parse RDF, execute SPARQL, add validation

### Path 2: Intermediate (2 hours)
**Goal:** Build real applications

1. Complete Beginner path
2. Read [ARCHITECTURE.md](../examples/ARCHITECTURE.md) (30 min)
3. Run [dark-matter-80-20.mjs](../examples/dark-matter-80-20.mjs) (30 min)
4. Run [lockchain-demo.mjs](../examples/lockchain-demo.mjs) (20 min)
5. Run [policy-pack-demo.mjs](../examples/policy-pack-demo.mjs) (25 min)
6. Run [browser examples](../examples/browser/) (15 min)

**You'll learn:** Optimization, compliance, governance, browser integration

### Path 3: Advanced (4 hours)
**Goal:** Master the entire system

1. Complete Intermediate path
2. Read [EXAMPLES-GUIDE.md](./examples/EXAMPLES-GUIDE.md) (1 hour)
3. Run [knowledge-engine-example.mjs](../examples/knowledge-engine-example.mjs) (1 hour)
4. Run [sparql-query-advanced.mjs](../examples/sparql-query-advanced.mjs) (1 hour)
5. Run [comprehensive-feature-test.mjs](../examples/comprehensive-feature-test.mjs) (1 hour)

**You'll learn:** AI integration, complex queries, full-stack architecture

## 🎯 Find What You Need

### By Use Case

| Goal | Documentation | Examples |
|------|---------------|----------|
| **Just parse and query RDF** | [QUICKSTART](../examples/QUICKSTART.md) | [01-minimal](../examples/01-minimal-parse-query.mjs) |
| **Add validation** | [Hooks README](../packages/hooks/README.md) | [basic-hook](../examples/basic-knowledge-hook.mjs) |
| **Optimize queries** | [Dark Matter README](../packages/dark-matter/README.md) | [80-20](../examples/dark-matter-80-20.mjs) |
| **Build web app** | [Browser README](../packages/browser/README.md) | [browser/](../examples/browser/) |
| **Add compliance** | [Examples Guide](./examples/EXAMPLES-GUIDE.md#governance--compliance) | [lockchain](../examples/lockchain-demo.mjs) |
| **Real-time updates** | [Streaming README](../packages/streaming/README.md) | [streaming/](../examples/streaming/) |
| **CLI automation** | [CLI README](../packages/cli/README.md) | [cli-automation](../examples/cli-automation-script.mjs) |
| **AI semantic search** | [Knowledge Engine README](../packages/knowledge-engine/README.md) | [knowledge-engine](../examples/knowledge-engine-example.mjs) |

### By Package

**Core Packages** (always needed):
- [@unrdf/core](../packages/core/README.md) - RDF operations

**Feature Packages** (add as needed):
- [@unrdf/hooks](../packages/hooks/README.md) - Validation
- [@unrdf/streaming](../packages/streaming/README.md) - Real-time
- [@unrdf/browser](../packages/browser/README.md) - Client-side
- [@unrdf/cli](../packages/cli/README.md) - Command-line

**Optional Extensions** (use only if needed):
- [@unrdf/dark-matter](../packages/dark-matter/README.md) - Optimization
- [@unrdf/knowledge-engine](../packages/knowledge-engine/README.md) - AI/ML
- [@unrdf/federation](../packages/federation/README.md) - Distribution

### By Topic

**Architecture & Design:**
- [ARCHITECTURE.md](../examples/ARCHITECTURE.md) - System design
- [EXAMPLES-GUIDE.md](./examples/EXAMPLES-GUIDE.md) - Integration patterns

**Performance:**
- [dark-matter-80-20.mjs](../examples/dark-matter-80-20.mjs) - Query optimization
- [profiling-example.mjs](../examples/profiling-example.mjs) - Performance profiling

**Security & Compliance:**
- [lockchain-demo.mjs](../examples/lockchain-demo.mjs) - Audit trails
- [policy-pack-demo.mjs](../examples/policy-pack-demo.mjs) - Governance

**Web Development:**
- [browser/](../examples/browser/) - Browser integration
- [browser-react.jsx](../examples/browser-react.jsx) - React patterns
- [browser-vue.vue](../examples/browser-vue.vue) - Vue patterns

## 📝 Example Categories

### By Complexity

**Beginner** (3-15 min):
- [01-minimal-parse-query.mjs](../examples/01-minimal-parse-query.mjs)
- [minimal-core-example.mjs](../examples/minimal-core-example.mjs)
- [basic-knowledge-hook.mjs](../examples/basic-knowledge-hook.mjs)

**Intermediate** (20-30 min):
- [dark-matter-80-20.mjs](../examples/dark-matter-80-20.mjs)
- [lockchain-demo.mjs](../examples/lockchain-demo.mjs)
- [policy-pack-demo.mjs](../examples/policy-pack-demo.mjs)

**Advanced** (1+ hour):
- [knowledge-engine-example.mjs](../examples/knowledge-engine-example.mjs)
- [sparql-query-advanced.mjs](../examples/sparql-query-advanced.mjs)
- [comprehensive-feature-test.mjs](../examples/comprehensive-feature-test.mjs)

### By Feature

**Core RDF Operations:**
- Parse: [01-minimal](../examples/01-minimal-parse-query.mjs)
- Query: [sparql-advanced](../examples/sparql-query-advanced.mjs)
- Serialize: [minimal-core](../examples/minimal-core-example.mjs)

**Knowledge Hooks:**
- Basics: [basic-hook](../examples/basic-knowledge-hook.mjs)
- Advanced: [define-hook](../examples/define-hook-example.mjs)
- Production: [production-hook-test](../examples/production-hook-test.mjs)

**Performance:**
- Optimization: [dark-matter-80-20](../examples/dark-matter-80-20.mjs)
- Profiling: [profiling-example](../examples/profiling-example.mjs)

**Governance:**
- Audit: [lockchain-demo](../examples/lockchain-demo.mjs)
- Policy: [policy-pack-demo](../examples/policy-pack-demo.mjs)

**Streaming:**
- Basics: [streaming/basic-stream.mjs](../examples/streaming/basic-stream.mjs)
- Filters: [streaming/advanced-filters.mjs](../examples/streaming/advanced-filters.mjs)

**Browser:**
- Storage: [browser/indexeddb-store.html](../examples/browser/indexeddb-store.html)
- React: [browser-react.jsx](../examples/browser-react.jsx)
- Vue: [browser-vue.vue](../examples/browser-vue.vue)

## 🔍 Troubleshooting

### Common Issues

**Problem:** Examples not running
- **Solution:** See [QUICKSTART Troubleshooting](../examples/QUICKSTART.md#troubleshooting)

**Problem:** Understanding architecture
- **Solution:** Read [ARCHITECTURE.md](../examples/ARCHITECTURE.md)

**Problem:** Finding the right example
- **Solution:** Use [EXAMPLES-GUIDE.md](./examples/EXAMPLES-GUIDE.md)

**Problem:** Package-specific questions
- **Solution:** See package README in [packages/](../packages/)

## 📚 Documentation Statistics

- **Total documentation files:** 13 (7 new, 6 updated)
- **Total lines:** 1,732+ lines of documentation
- **Examples documented:** 50+ examples
- **Packages covered:** 10/10 packages
- **Learning paths:** 3 paths (Beginner, Intermediate, Advanced)
- **Architecture diagrams:** 5 ASCII diagrams
- **Code patterns:** 30+ common patterns

## 🎓 Additional Resources

### Internal Documentation
- [Project README](../README.md) - Project overview
- [Contributing Guide](../CONTRIBUTING.md) - How to contribute
- [Changelog](../CHANGELOG.md) - Version history

### External Resources
- [GitHub Repository](https://github.com/unrdf/unrdf)
- [Issue Tracker](https://github.com/unrdf/unrdf/issues)
- [Discord Community](https://discord.gg/unrdf)

## 🤝 Contributing

Found a typo or want to improve documentation? See [CONTRIBUTING.md](../CONTRIBUTING.md).

---

**Remember:** Start with [QUICKSTART.md](../examples/QUICKSTART.md) and [01-minimal-parse-query.mjs](../examples/01-minimal-parse-query.mjs). Add complexity only when needed.
