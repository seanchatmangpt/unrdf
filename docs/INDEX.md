# UNRDF v26.4.4 Documentation Index

**Welcome to UNRDF documentation.** Choose your path below:

---

## 🚀 Getting Started (First Time?)

1. **[START-HERE.md](START-HERE.md)** — Orientation guide (5 min read)
2. **[GETTING_STARTED.md](GETTING_STARTED.md)** — Install and run first example (15 min)
3. **[QUICK-START.md](QUICK-START.md)** — Copy-paste working examples

---

## 📚 Core Documentation

### For Users Building Apps

- **[ARCHITECTURE.md](ARCHITECTURE.md)** — System design and how things work together
- **[MONOREPO-QUICK-REFERENCE.md](MONOREPO-QUICK-REFERENCE.md)** — Package overview matrix
- **[TROUBLESHOOTING.md](TROUBLESHOOTING.md)** — Common issues and fixes
- **[TESTING-STRATEGY.md](TESTING-STRATEGY.md)** — How to write tests

### For Contributors

- **[CONTRIBUTING.md](CONTRIBUTING.md)** — How to contribute code
- **[LOCAL-DEVELOPMENT.md](LOCAL-DEVELOPMENT.md)** — Set up dev environment
- **[PACKAGE-DEVELOPMENT.md](PACKAGE-DEVELOPMENT.md)** — Create new packages
- **[adr/](adr/)** — Architecture Decision Records

### For Operations

- **[SECURITY-POLICY.md](SECURITY-POLICY.md)** — Security practices
- **[MIGRATION.md](MIGRATION.md)** — Upgrade from previous versions
- **[CHANGELOG.md](CHANGELOG.md)** — Version history and release notes

---

## 📦 Package Documentation

Each package has its own documentation:

```
packages/
├── core/README.md               ← RDF storage, SPARQL, SHACL
├── oxigraph/README.md           ← Persistent backend
├── hooks/README.md              ← Knowledge Hooks framework
├── streaming/README.md          ← Large graph streaming
├── federation/README.md         ← Distributed queries
├── browser/README.md            ← Browser runtime
├── cli/README.md                ← Command-line tools
├── react/README.md              ← React integration
└── [more packages]
```

**Start with `@unrdf/core` — it's the foundation for everything.** See [MONOREPO-QUICK-REFERENCE.md](MONOREPO-QUICK-REFERENCE.md) for the full package matrix.

---

## 🎓 API Reference

- **`packages/core/API-REFERENCE.md`** — Core RDF operations
- **`packages/hooks/API-REFERENCE.md`** — Knowledge Hooks API
- **`packages/streaming/API-REFERENCE.md`** — Streaming operations

---

## 📋 Quick Navigation

| Need                            | Go To                                                      |
| ------------------------------- | ---------------------------------------------------------- |
| "How do I...?"                  | [TROUBLESHOOTING.md](TROUBLESHOOTING.md)                   |
| "What's the architecture?"      | [ARCHITECTURE.md](ARCHITECTURE.md)                         |
| "How do I set up development?"  | [LOCAL-DEVELOPMENT.md](LOCAL-DEVELOPMENT.md)               |
| "I want to contribute"          | [CONTRIBUTING.md](CONTRIBUTING.md)                         |
| "What changed in this version?" | [CHANGELOG.md](CHANGELOG.md)                               |
| "How do I upgrade?"             | [MIGRATION.md](MIGRATION.md)                               |
| "What packages are available?"  | [MONOREPO-QUICK-REFERENCE.md](MONOREPO-QUICK-REFERENCE.md) |
| "Security/compliance"           | [SECURITY-POLICY.md](SECURITY-POLICY.md)                   |

---

## 📁 Documentation Structure

```
docs/
├── INDEX.md                      ← You are here
├── START-HERE.md                 ← Entry point
├── GETTING_STARTED.md            ← Hands-on tutorial
├── ARCHITECTURE.md               ← System design
├── LOCAL-DEVELOPMENT.md          ← Dev setup
├── CONTRIBUTING.md               ← Contribution guide
├── MONOREPO-QUICK-REFERENCE.md   ← Package matrix
├── TESTING-STRATEGY.md           ← Testing guide
├── TROUBLESHOOTING.md            ← Help/support
├── SECURITY-POLICY.md            ← Security info
├── MIGRATION.md                  ← Upgrade guide
├── CHANGELOG.md                  ← Version history
├── PUBLICATION-CHECKLIST.md      ← This release checklist
├── adr/                          ← Architecture decisions
├── api/                          ← API reference by package
├── archive/                      ← Historical docs
└── [package-specific docs]
```

---

## ❓ Still Lost?

- **Quick path:** Root README → GETTING_STARTED.md → your-first-example
- **Deep dive:** Root README → START-HERE.md → ARCHITECTURE.md → package README
- **Help:** TROUBLESHOOTING.md or GitHub Issues

---

## Architecture Analysis

Understanding the reasoning behind the design:

6. **[v5-substrate-voc-analysis.md](./v5-substrate-voc-analysis.md)** (15 min read)
   - 4 AI Agent VOCs in detail
   - 3 Human Developer/Operator VOCs in detail
   - Core substrate boundaries
   - What to keep vs move out
   - Implementation priorities

7. **[v5-voc-to-implementation.md](./v5-voc-to-implementation.md)** (20 min read)
   - VOC-1 through VOC-7 detailed
   - What each VOC gets from UNRDF
   - What each VOC builds themselves
   - Code examples per VOC
   - Architecture patterns per VOC

## Development & Setup

Getting your development environment ready:

8. **[MONOREPO-SETUP.md](./MONOREPO-SETUP.md)** (10 min read)
   - Installation prerequisites
   - Quick start (3 commands)
   - Development workflow
   - Testing across packages
   - Linting and formatting
   - Adding new packages
   - Troubleshooting

## Migration from v4

If you're upgrading from v4.x:

9. **[MONOREPO-MIGRATION.md](./MONOREPO-MIGRATION.md)** (10 min read)
   - Overview of changes
   - Package structure changes
   - Import statement updates
   - CLI references
   - Breaking changes by scenario
   - Migration checklist
   - Dependency resolution
   - Troubleshooting

## Implementation Roadmap

Long-term planning and strategy:

10. **[v5-substrate-refactoring-roadmap.md](./v5-substrate-refactoring-roadmap.md)** (20 min read)
    - Component classification (Tier 1, 2, 3)
    - What stays in core
    - What moves to extensions
    - File structure after refactoring
    - Migration path (non-breaking)
    - Package ecosystem
    - Size reduction targets
    - 5-week timeline

## Validation & Status

Verification that everything is ready:

11. **[MONOREPO-VALIDATION-CHECKLIST.md](./MONOREPO-VALIDATION-CHECKLIST.md)** (5 min read)
    - Structure validation
    - Package configuration checklist
    - Dependencies validation
    - Installation testing results
    - Documentation completeness
    - Scripts validation
    - Size validation
    - VOC alignment
    - Final validation status

## External References

- **[MONOREPO-IMPLEMENTATION-SUMMARY.md](../MONOREPO-IMPLEMENTATION-SUMMARY.md)** - What was done
- **[QUICK-REFERENCE.md](../QUICK-REFERENCE.md)** - Quick commands and tips

## Document Purpose Matrix

| Document                            | Purpose              | Read Time | Audience        |
| ----------------------------------- | -------------------- | --------- | --------------- |
| v5-README.md                        | Overview & FAQ       | 5 min     | Everyone        |
| QUICK-REFERENCE.md                  | Copy-paste commands  | 2 min     | Everyone        |
| v5-MONOREPO-SUMMARY.md              | Complete picture     | 15 min    | Decision makers |
| MONOREPO-STRUCTURE.md               | Architecture details | 10 min    | Developers      |
| MONOREPO-VISUALS.md                 | Visual explanations  | 10 min    | Visual learners |
| v5-substrate-voc-analysis.md        | Design reasoning     | 15 min    | Architects      |
| v5-voc-to-implementation.md         | Implementation plans | 20 min    | Developers      |
| MONOREPO-SETUP.md                   | Dev environment      | 10 min    | Developers      |
| MONOREPO-MIGRATION.md               | Upgrading from v4    | 10 min    | Current users   |
| v5-substrate-refactoring-roadmap.md | Long-term plan       | 20 min    | Project leads   |
| MONOREPO-VALIDATION-CHECKLIST.md    | Status check         | 5 min     | QA/validation   |

## Quick Navigation

### By Role

**I'm a user starting fresh:**
→ QUICK-REFERENCE.md → v5-README.md → MONOREPO-SETUP.md

**I'm upgrading from v4:**
→ MONOREPO-MIGRATION.md → QUICK-REFERENCE.md

**I'm a developer setting up:**
→ MONOREPO-SETUP.md → QUICK-REFERENCE.md

**I'm a decision maker:**
→ v5-README.md → v5-MONOREPO-SUMMARY.md

**I'm an architect:**
→ v5-substrate-voc-analysis.md → v5-voc-to-implementation.md

**I need to understand why:**
→ v5-substrate-voc-analysis.md → v5-substrate-refactoring-roadmap.md

### By Topic

**Installation & Setup**

- QUICK-REFERENCE.md
- MONOREPO-SETUP.md
- v5-README.md

**Architecture & Design**

- MONOREPO-STRUCTURE.md
- MONOREPO-VISUALS.md
- v5-substrate-voc-analysis.md

**Implementation**

- v5-voc-to-implementation.md
- v5-substrate-refactoring-roadmap.md

**Migration**

- MONOREPO-MIGRATION.md
- v5-README.md#FAQ

**Validation**

- MONOREPO-VALIDATION-CHECKLIST.md

## Key Concepts

### The 10 Packages

- **@unrdf/core** - Essential RDF substrate
- **@unrdf/hooks** - Policy enforcement framework
- **@unrdf/federation** - Peer queries and discovery
- **@unrdf/streaming** - Real-time change feeds
- **@unrdf/browser** - Browser SDK with IndexedDB
- **@unrdf/cli** - Command-line tools
- **@unrdf/knowledge-engine** - Optional rule engine
- **@unrdf/dark-matter** - Optional query optimization
- **@unrdf/composables** - Optional Vue 3 composables
- **@unrdf/project-engine** - Development-only self-hosting

### The 7 VOCs (Voices of Customer)

1. Autonomous Knowledge Agent
2. Real-time Sync Agent
3. ML Pattern Learning Agent
4. Compliance Audit Agent
5. Data Engineering (ETL)
6. App Development (Web/Mobile)
7. DevOps Operations

### Design Principles

1. Substrate is minimum
2. No forced dependencies
3. Clear package boundaries
4. Independent versioning
5. Community extensible
6. Single git repo, multiple npm packages

## Statistics

- **Documents**: 11 comprehensive guides
- **Total Pages**: ~150 pages of documentation
- **Packages**: 10 focused modules
- **VOCs Analyzed**: 7 synthetic customer personas
- **Size Reduction**: 68-69% (2.5MB → 150KB-880KB)
- **Dependencies**: Acyclic, workspace-linked

## Next Steps

1. **Read**: Start with QUICK-REFERENCE.md
2. **Install**: `pnpm install`
3. **Explore**: `cd packages/core && pnpm test`
4. **Learn**: Read appropriate docs for your role
5. **Build**: Create features in your needed packages

---

**Status**: ✅ Complete and validated
**Last Updated**: December 3, 2025
**Version**: v5.0-alpha.0

See [../MONOREPO-IMPLEMENTATION-SUMMARY.md](../MONOREPO-IMPLEMENTATION-SUMMARY.md) for what was accomplished.
