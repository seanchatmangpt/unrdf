# UNRDF Examples

**Start with #01. Add complexity only if needed.**

> **📊 Examples Status**: 10/11 critical examples working (91% success rate)
> **Version**: v6.0.0-rc.3
> **Last Updated**: 2026-01-19
> **See**: [EXAMPLES-STATUS.md](./EXAMPLES-STATUS.md) for detailed status report

## 🎯 NEW: 15-Minute Quick Start Guide

**Brand new to UNRDF?** Follow this progressive learning path:

| Step | Example                                                    | Time   | What You'll Learn               |
| ---- | ---------------------------------------------------------- | ------ | ------------------------------- |
| 1    | **[01-hello-rdf.mjs](./01-hello-rdf.mjs)**                 | 5 min  | RDF basics, triples, namespaces |
| 2    | **[02-sparql-queries.mjs](./02-sparql-queries.mjs)**       | 10 min | SPARQL SELECT, ASK, CONSTRUCT   |
| 3    | **[03-knowledge-hooks.mjs](./03-knowledge-hooks.mjs)**     | 10 min | Reactive behaviors, validation  |
| 4    | **[04-validation.mjs](./04-validation.mjs)**               | 10 min | Data quality, validation rules  |
| 5    | **[05-advanced-patterns.mjs](./05-advanced-patterns.mjs)** | 15 min | Production patterns, analytics  |

**Total time: 50 minutes to full productivity!**

Then read: **[Getting Started Guide](../docs/GETTING_STARTED.md)** for comprehensive tutorials.

---

## 🚀 Quick Navigation

- **New to UNRDF?** → [GETTING_STARTED.md](../docs/GETTING_STARTED.md) (15 minutes) ⭐
- **Legacy quickstart** → [QUICKSTART.md](./QUICKSTART.md) (5 minutes)
- **How does it work?** → [ARCHITECTURE.md](./ARCHITECTURE.md) (system design)
- **All examples detailed** → [docs/examples/EXAMPLES-GUIDE.md](../docs/examples/EXAMPLES-GUIDE.md)
- **Package docs** → [Per-package READMEs](#per-package-documentation)

## Beginner Examples (Start Here)

| #   | Example                                                    | Time  | Status | What You'll Learn                              |
| --- | ---------------------------------------------------------- | ----- | ------ | ---------------------------------------------- |
| 01  | [01-minimal-parse-query.mjs](./01-minimal-parse-query.mjs) | 3 min | ✅ | Parse RDF, execute SPARQL (the pit of success) |

**After #01:** Read [QUICKSTART.md](./QUICKSTART.md) for next steps.

## Intermediate Examples (Only If Needed)

| #   | Example                                                | Time   | Status | When You Need It                     |
| --- | ------------------------------------------------------ | ------ | ------ | ------------------------------------ |
| 10  | [basic-knowledge-hook.mjs](./basic-knowledge-hook.mjs) | 15 min | ⚠️ | Autonomous behaviors (needs v6 migration) |
| 11  | [context-example.mjs](./context-example.mjs)           | 10 min | ✅ | Store operations and pattern matching |
| 12  | [define-hook-example.mjs](./define-hook-example.mjs)   | 20 min | ✅ | Advanced hook composition patterns   |

## Advanced Examples (Skip Unless Required)

| #   | Example                                          | Time   | Status | When You Need It                  |
| --- | ------------------------------------------------ | ------ | ------ | --------------------------------- |
| 20  | [dark-matter-80-20.mjs](./dark-matter-80-20.mjs) | 30 min | ✅ | Query optimization and 80/20 analysis |
| 21  | [lockchain-demo.mjs](./lockchain-demo.mjs)       | 20 min | ? | Compliance audit trails           |
| 22  | [policy-pack-demo.mjs](./policy-pack-demo.mjs)   | 25 min | ? | Declarative governance            |
| 23  | [policy-pack-usage.mjs](./policy-pack-usage.mjs) | 25 min | ? | Custom policy pack creation       |

## Full Feature Showcase (Reference Only)

| Example                                                            | Lines | Purpose                        |
| ------------------------------------------------------------------ | ----- | ------------------------------ |
| [knowledge-engine-example.mjs](./knowledge-engine-example.mjs)     | 363   | Complete feature demonstration |
| [sparql-query-advanced.mjs](./sparql-query-advanced.mjs)           | 453   | Complex query patterns         |
| [comprehensive-feature-test.mjs](./comprehensive-feature-test.mjs) | 345   | All features integrated        |

## Examples by Category

### 📊 Core Functionality

- [01-minimal-parse-query.mjs](./01-minimal-parse-query.mjs) - Parse & query (start here)
- [minimal-core-example.mjs](./minimal-core-example.mjs) - Low-level core usage
- [context-example.mjs](./context-example.mjs) - Context system

### 🪝 Knowledge Hooks

- [basic-knowledge-hook.mjs](./basic-knowledge-hook.mjs) - Hook basics
- [define-hook-example.mjs](./define-hook-example.mjs) - Advanced hooks
- [production-hook-test.mjs](./production-hook-test.mjs) - Production testing

### ⚡ Performance Optimization

- [dark-matter-80-20.mjs](./dark-matter-80-20.mjs) - Query optimization
- [dark-matter-query-optimization.mjs](./dark-matter-query-optimization.mjs) - Advanced optimization
- [profiling-example.mjs](./profiling-example.mjs) - Performance profiling

### 🔒 Governance & Compliance

- [lockchain-demo.mjs](./lockchain-demo.mjs) - Audit trails
- [policy-pack-demo.mjs](./policy-pack-demo.mjs) - Policy enforcement
- [policy-pack-usage.mjs](./policy-pack-usage.mjs) - Custom policies

### 📡 Streaming & Real-time

- [streaming/basic-stream.mjs](./streaming/basic-stream.mjs) - Change feeds
- [streaming/advanced-filters.mjs](./streaming/advanced-filters.mjs) - Stream filtering

### 🌐 Browser & Client-side

- [browser/indexeddb-store.html](./browser/indexeddb-store.html) - IndexedDB storage
- [browser-react.jsx](./browser-react.jsx) - React integration
- [browser-vue.vue](./browser-vue.vue) - Vue integration

### 🛠️ CLI & Automation

- [cli-automation-script.mjs](./cli-automation-script.mjs) - CLI workflows
- [cli-scaffolding-demo.mjs](./cli-scaffolding-demo.mjs) - Project scaffolding

### 🧠 AI & Semantics

- [knowledge-engine-example.mjs](./knowledge-engine-example.mjs) - AI semantic analysis
- [ai-semantic-example.mjs](./ai-semantic-example.mjs) - Embedding search

---

## Quick Start

```bash
# Run the minimal example
node examples/01-minimal-parse-query.mjs

# Output:
# http://example.org/Alice knows http://example.org/Bob
# http://example.org/Bob knows http://example.org/Charlie
# http://example.org/Charlie knows http://example.org/Diana
# Transaction manager ready: true
# Core status: { status: 'active', components: 1 }
```

---

## Which Example Should I Run?

| Your Goal                | Start With                                                 |
| ------------------------ | ---------------------------------------------------------- |
| Just parse and query RDF | [01-minimal-parse-query.mjs](./01-minimal-parse-query.mjs) |
| Add validation           | [basic-knowledge-hook.mjs](./basic-knowledge-hook.mjs)     |
| Add hooks                | [define-hook-example.mjs](./define-hook-example.mjs)       |
| Optimize performance     | [dark-matter-80-20.mjs](./dark-matter-80-20.mjs)           |
| Add audit trails         | [lockchain-demo.mjs](./lockchain-demo.mjs)                 |
| Build a web app          | [browser-react.jsx](./browser-react.jsx)                   |
| Automate workflows       | [cli-automation-script.mjs](./cli-automation-script.mjs)   |

**When in doubt, start with 01.**

## Per-Package Documentation

Each package has comprehensive README files:

- **[@unrdf/core](../packages/core/README.md)** - RDF parsing, SPARQL, serialization
- **[@unrdf/hooks](../packages/hooks/README.md)** - Policy enforcement framework
- **[@unrdf/dark-matter](../packages/dark-matter/README.md)** - Query optimization
- **[@unrdf/knowledge-engine](../packages/knowledge-engine/README.md)** - AI semantic analysis
- **[@unrdf/streaming](../packages/streaming/README.md)** - Real-time change feeds
- **[@unrdf/browser](../packages/browser/README.md)** - Client-side storage
- **[@unrdf/federation](../packages/federation/README.md)** - Distributed queries
- **[@unrdf/cli](../packages/cli/README.md)** - Command-line interface
- **[@unrdf/composables](../packages/composables/README.md)** - React/Vue/Svelte hooks
- **[@unrdf/project-engine](../packages/project-engine/README.md)** - Build system integration

## How to Run Examples

### Prerequisites

```bash
# Install dependencies
pnpm install

# Build packages
pnpm build
```

### Running Examples

```bash
# Run any example
node examples/[example-name].mjs

# Run with debugging
DEBUG=unrdf:* node examples/[example-name].mjs

# Run in watch mode (auto-reload)
nodemon examples/[example-name].mjs
```

## Learning Path

**Path 1: Beginner (30 minutes)**

1. [01-minimal-parse-query.mjs](./01-minimal-parse-query.mjs) (3 min)
2. [QUICKSTART.md](./QUICKSTART.md) (10 min)
3. [basic-knowledge-hook.mjs](./basic-knowledge-hook.mjs) (15 min)

**Path 2: Intermediate (2 hours)**

1. Complete Path 1
2. [context-example.mjs](./context-example.mjs) (10 min)
3. [dark-matter-80-20.mjs](./dark-matter-80-20.mjs) (30 min)
4. [lockchain-demo.mjs](./lockchain-demo.mjs) (20 min)
5. [policy-pack-demo.mjs](./policy-pack-demo.mjs) (25 min)
6. [ARCHITECTURE.md](./ARCHITECTURE.md) (30 min)

**Path 3: Advanced (4 hours)**

1. Complete Path 2
2. [knowledge-engine-example.mjs](./knowledge-engine-example.mjs) (1 hour)
3. [sparql-query-advanced.mjs](./sparql-query-advanced.mjs) (1 hour)
4. [comprehensive-feature-test.mjs](./comprehensive-feature-test.mjs) (1 hour)
5. [docs/examples/EXAMPLES-GUIDE.md](../docs/examples/EXAMPLES-GUIDE.md) (1 hour)

## Documentation Index

- **[QUICKSTART.md](./QUICKSTART.md)** - 5-minute getting started guide
- **[ARCHITECTURE.md](./ARCHITECTURE.md)** - System architecture and design patterns
- **[docs/examples/EXAMPLES-GUIDE.md](../docs/examples/EXAMPLES-GUIDE.md)** - Comprehensive examples guide
- **[Per-package READMEs](#per-package-documentation)** - API documentation

## Need Help?

- **Examples not working?** Check [QUICKSTART.md](./QUICKSTART.md) troubleshooting section
- **Architecture questions?** Read [ARCHITECTURE.md](./ARCHITECTURE.md)
- **API questions?** See package-specific READMEs
- **Community support:** [GitHub Issues](https://github.com/unrdf/unrdf/issues)

---

## 📝 Recent Updates (2026-01-19)

### Fixed Examples (80/20 Approach)
Focused on the top 20% of examples that deliver 80% of value:

**Completed Fixes**:
- ✅ `01-minimal-parse-query.mjs` - Updated to use `@unrdf/core` instead of deprecated `unrdf` package
- ✅ `context-example.mjs` - Rewritten to use current API (removed deprecated `initStore`, `useStore`)
- ✅ `minimal-core-example.mjs` - Rewritten to use `@unrdf/core` instead of missing knowledge-engine modules
- ✅ `dark-matter-80-20.mjs` - Rewritten to demonstrate practical query optimization
- ✅ `define-hook-example.mjs` - Updated imports to use `@unrdf/hooks` and `@unrdf/core`

**Status**: 10/11 critical examples working (91% success rate)

**Known Issues**:
- ⚠️ `basic-knowledge-hook.mjs` - Needs hook schema migration (advanced feature, not blocking)

See [EXAMPLES-STATUS.md](./EXAMPLES-STATUS.md) for detailed report.

---

**Remember:** Start with [01-minimal-parse-query.mjs](./01-minimal-parse-query.mjs) and add complexity only when needed.
