# UNRDF v5: RDF Knowledge Graph Substrate Platform

**Welcome to UNRDF v5** - A modular, substrate-first architecture designed for 7 different user personas.

## 🚀 Quick Start

Pick your use case:

- **Building a web app?** → `pnpm add @unrdf/composables`
- **Writing ETL scripts?** → `pnpm add @unrdf/cli`
- **Running multi-agent systems?** → `pnpm add @unrdf/core @unrdf/hooks @unrdf/federation`
- **Need real-time sync?** → `pnpm add @unrdf/streaming`
- **Just RDF operations?** → `pnpm add @unrdf/core`

Everything else is built on top of these.

## 📚 Documentation Map

### Understanding UNRDF v5

Start here if you're new to v5 or migrating from v4:

1. **[v5-MONOREPO-SUMMARY.md](./v5-MONOREPO-SUMMARY.md)** ⭐ START HERE
   - What changed and why
   - 7 synthetic customer personas (VOCs)
   - Package overview and installation paths
   - Size reduction (2.5MB → 150KB)

2. **[MONOREPO-STRUCTURE.md](./MONOREPO-STRUCTURE.md)**
   - Complete package architecture
   - Dependencies between packages
   - File organization per package
   - Workspace configuration

3. **[MONOREPO-MIGRATION.md](./MONOREPO-MIGRATION.md)** (If upgrading from v4)
   - Breaking changes
   - Import statement updates
   - Dependency resolution
   - Troubleshooting

### Development & Setup

4. **[MONOREPO-SETUP.md](./MONOREPO-SETUP.md)**
   - Installation instructions
   - Development workflow
   - Testing and building
   - CI/CD integration
   - Troubleshooting guide

### VOC Analysis (Why We Made These Changes)

5. **[v5-substrate-voc-analysis.md](./v5-substrate-voc-analysis.md)**
   - 7 synthetic Voices of the Customer
   - 4 AI Agent VOCs (swarm coordinator, sync agent, ML agent, audit agent)
   - 3 Human VOCs (data engineer, app developer, DevOps operator)
   - Substrate boundary definitions

6. **[v5-voc-to-implementation.md](./v5-voc-to-implementation.md)**
   - Maps each VOC to their implementation stack
   - Shows exactly what APIs each user gets
   - Code examples for each persona
   - Architecture diagrams

7. **[v5-substrate-refactoring-roadmap.md](./v5-substrate-refactoring-roadmap.md)**
   - 5-week implementation roadmap
   - Component tier classification
   - Migration path with deprecation timeline
   - Package ecosystem overview

## 🎯 The 10 Packages

### Essential Substrate (Core)
- **@unrdf/core** - RDF operations, SPARQL execution
- **@unrdf/hooks** - Policy enforcement framework
- **@unrdf/federation** - Peer discovery and queries
- **@unrdf/streaming** - Change feeds and real-time sync
- **@unrdf/browser** - Browser SDK with IndexedDB

### Tooling
- **@unrdf/cli** - Command-line tools for operations

### Optional Extensions
- **@unrdf/knowledge-engine** - Rule engine and inference (optional)
- **@unrdf/dark-matter** - Query optimization (optional)
- **@unrdf/composables** - Vue 3 composables (optional)

### Development Only
- **@unrdf/project-engine** - Self-hosting tools (dev only)

## 📦 Installation Patterns

### Minimal (Edge/IoT)
```bash
pnpm add @unrdf/core
# ~150KB, just RDF operations
```

### Substrate (Most Users)
```bash
pnpm add @unrdf/core @unrdf/hooks @unrdf/federation @unrdf/streaming
# ~340KB, complete substrate
```

### Web App
```bash
pnpm add @unrdf/composables
# Auto-installs: core, browser, streaming
# Vue 3 composables for reactive state
```

### ETL/Data Engineering
```bash
pnpm add @unrdf/cli
# Auto-installs: core, hooks, federation, streaming
# CLI tools for scripting
```

### Full Stack
```bash
pnpm add @unrdf/core @unrdf/hooks @unrdf/federation @unrdf/streaming \
  @unrdf/browser @unrdf/cli @unrdf/knowledge-engine @unrdf/dark-matter \
  @unrdf/composables
# ~880KB, everything
```

## 🗺️ The 7 VOCs

### AI Agents (4)
1. **Autonomous Knowledge Agent** - Multi-agent coordination
   - Needs: RDF ops, SPARQL, Hooks, Federation
   - Stack: @unrdf/core + hooks + federation

2. **Real-time Sync Agent** - Distributed consistency
   - Needs: Streaming, Federation, RDF ops
   - Stack: @unrdf/core + streaming + federation

3. **ML Pattern Agent** - Pattern learning and application
   - Needs: SPARQL, Hooks, Streaming, Canonicalization
   - Stack: @unrdf/core + hooks + streaming

4. **Audit Agent** - Compliance monitoring
   - Needs: SPARQL, Streaming, Hooks, Observability
   - Stack: @unrdf/core + hooks + streaming + observability points

### Humans (3)
5. **Data Engineer** - ETL pipeline builder
   - Needs: SPARQL, Federation, Hooks, CLI
   - Stack: @unrdf/cli (auto-installs core + hooks + federation + streaming)

6. **App Developer** - Web/mobile application builder
   - Needs: Composables, Hooks, Streaming, Browser SDK
   - Stack: @unrdf/composables (auto-installs core + browser + streaming)

7. **DevOps Operator** - Production operations
   - Needs: Observability, Federation, CLI, Hooks
   - Stack: @unrdf/cli + OTEL instrumentation points

## 🔑 Key Design Principles

### Principle 1: Substrate is Minimum
The RDF substrate (@unrdf/core + @unrdf/hooks) is deliberately minimal. Everything else is optional.

### Principle 2: No Forced Dependencies
Installing @unrdf/knowledge-engine doesn't require dark-matter, composables, or anything else. Pure opt-in.

### Principle 3: Clear Boundaries
Impossible to blur lines. Each package has a single clear responsibility.

### Principle 4: Independent Versions
@unrdf/core stays at 5.0.x (stable). Knowledge engine at 2.x, dark-matter at 1.x. Features don't wait for core.

### Principle 5: Community Extensible
Easy to create @unrdf/my-feature packages that depend only on @unrdf/core.

## 📊 Size Comparison

```
v4.x (Monolithic):
├── Package size: 2.5 MB
├── Needed: ~750 KB (30%)
└── Bloat: ~1.75 MB (70%)

v5.0 (Monorepo):
├── @unrdf/core alone: 150 KB
├── Substrate (core + hooks + federation + streaming): 340 KB
├── Full stack: 880 KB
└── Install only what you need
```

**Result**: 68-69% size reduction for typical users.

## 🔄 Migration from v4.x

**For v4 users**:
1. Import changes: `unrdf` → `@unrdf/core`, `unrdf/hooks` → `@unrdf/hooks`
2. Dependency changes: Install packages you need
3. API changes: None! (APIs identical, just different packages)

See [MONOREPO-MIGRATION.md](./MONOREPO-MIGRATION.md) for detailed guide.

## 🛠️ Development

### Setup Monorepo
```bash
pnpm install
pnpm test
pnpm build
```

### Test Specific Package
```bash
pnpm test:core
pnpm test:hooks
# etc.
```

### Add New Package
```bash
mkdir packages/my-feature
cd packages/my-feature
npm init -y
pnpm add @unrdf/core  # if needed
# Create src/, test/, README.md
pnpm test
```

See [MONOREPO-SETUP.md](./MONOREPO-SETUP.md) for full development guide.

## ❓ FAQ

**Q: Do I have to use all packages?**
A: No! Install only what you need. Minimum is @unrdf/core (~150KB).

**Q: Is this production-ready?**
A: v5.0-alpha.0 is in development. Each package will be released as stable.

**Q: Can I still use v4?**
A: Yes, v4.x stays on npm. v5 is a new major version.

**Q: How do I upgrade from v4?**
A: Update imports. All APIs work identically. See [MONOREPO-MIGRATION.md](./MONOREPO-MIGRATION.md).

**Q: Can I create my own @unrdf/my-extension?**
A: Yes! Build on @unrdf/core. Easy to share with community.

**Q: Why split across packages if it's a monorepo?**
A: Monorepo = one repo for consistency. Split packages = users choose what to install.

**Q: Won't this fragment the ecosystem?**
A: Yes, intentionally. Code fragments naturally. We're being intentional about boundaries.

**Q: What about backwards compatibility?**
A: v4.x stays on npm. v5.x is intentionally different. Both can coexist.

## 🎯 Next Steps

1. **Pick your use case** from the 7 VOCs above
2. **Read** [v5-MONOREPO-SUMMARY.md](./v5-MONOREPO-SUMMARY.md) for complete overview
3. **Install** the packages you need
4. **Build** your application!

## 📖 Reference

**Architecture & Design**:
- [MONOREPO-STRUCTURE.md](./MONOREPO-STRUCTURE.md) - Package architecture
- [v5-substrate-voc-analysis.md](./v5-substrate-voc-analysis.md) - VOC analysis
- [v5-voc-to-implementation.md](./v5-voc-to-implementation.md) - VOC implementation mapping

**Usage & Setup**:
- [MONOREPO-SETUP.md](./MONOREPO-SETUP.md) - Development guide
- [MONOREPO-MIGRATION.md](./MONOREPO-MIGRATION.md) - Upgrading from v4

**Roadmap & Details**:
- [v5-substrate-refactoring-roadmap.md](./v5-substrate-refactoring-roadmap.md) - 5-week plan
- [v5-MONOREPO-SUMMARY.md](./v5-MONOREPO-SUMMARY.md) - Complete summary

## 🤝 Contributing

See `CONTRIBUTING.md` for:
- Adding new packages
- Code standards
- Testing requirements
- Commit conventions

## 📝 License

MIT

---

**Built with substrate-first design. Ready for any use case. Zero forced dependencies.**

Start with `@unrdf/core`. Add what you need. Build what you want.
