# UNRDF Monorepo - Quick Reference

**Tired of searching through docs?** This page is your cheat sheet for the entire UNRDF monorepo in one place.

## Quick Stats

- **73+ packages** (monorepo with core, extended, and internal packages)
- **Root version:** 26.4.3
- **Package manager:** pnpm 8.15.0+
- **Node:** 18.19.0+
- **Module type:** ESM with JSDoc (no TypeScript source)

---

## The Essential 3 (Start Here)

| Package             | Purpose                                          | Install                       | Status        | Use When                                      |
| ------------------- | ------------------------------------------------ | ----------------------------- | ------------- | --------------------------------------------- |
| **@unrdf/core**     | ⭐ Main RDF library - storage, SPARQL, SHACL     | `npm install @unrdf/core`     | ✅ Production | **ALWAYS** - foundation for everything        |
| **@unrdf/oxigraph** | Rust-based persistent triple store               | `npm install @unrdf/oxigraph` | ✅ Production | You need persistent storage beyond memory     |
| **@unrdf/hooks**    | Knowledge Hooks framework (autonomous behaviors) | `npm install @unrdf/hooks`    | ✅ Production | You want behaviors that react to data changes |

---

## Extended Features (Build On Top)

| Package                   | Purpose                                   | Install                             | Status            | Use When                                           |
| ------------------------- | ----------------------------------------- | ----------------------------------- | ----------------- | -------------------------------------------------- |
| **@unrdf/streaming**      | Large graph streaming & real-time sync    | `npm install @unrdf/streaming`      | ✅ Production     | Processing multi-GB graphs without bloating memory |
| **@unrdf/federation**     | Federated/distributed query execution     | `npm install @unrdf/federation`     | ✅ Production     | Querying across multiple RDF stores simultaneously |
| **@unrdf/browser**        | Browser runtime with IndexedDB support    | `npm install @unrdf/browser`        | ✅ Production     | Building browser-based RDF applications            |
| **@unrdf/cli**            | Command-line interface & tools            | `npm install @unrdf/cli`            | ✅ Production     | Running RDF ops from the terminal                  |
| **@unrdf/react**          | React hooks & integration layer           | `npm install @unrdf/react`          | v4.1.1 Production | Building React apps with RDF data binding          |
| **@unrdf/composables**    | Vue 3 composables (optional extension)    | `npm install @unrdf/composables`    | 🔶 Alpha          | Using UNRDF in Vue 3 applications                  |
| **@unrdf/dark-matter**    | Query optimization & performance analysis | `npm install @unrdf/dark-matter`    | 🔶 Alpha          | Optimizing slow SPARQL queries                     |
| **@unrdf/project-engine** | Project/workspace management (dev-only)   | `npm install @unrdf/project-engine` | 🔶 Alpha          | Managing UNRDF projects & workspaces               |
| **@unrdf/engine-gateway** | API gateway & μ(O) enforcement layer      | `npm install @unrdf/engine-gateway` | v0.1.0 Production | Building API endpoints over RDF engines            |

---

## Internal Packages (For Contributors Only)

| Package               | Purpose                                  | Location              | Status | Public     |
| --------------------- | ---------------------------------------- | --------------------- | ------ | ---------- |
| **@unrdf/test-utils** | Testing utilities, fixtures, test stores | `packages/test-utils` | v1.0.0 | ❌ Private |
| **@unrdf/validation** | OTEL validation framework (internal)     | `packages/validation` | v1.0.0 | ❌ Private |
| **@unrdf/domain**     | Domain models & type definitions         | `packages/domain`     | v1.0.0 | ❌ Private |

---

## Quick Package Matrix

### By Status

- **✅ Production Ready (9):** core, oxigraph, hooks, streaming, federation, browser, cli, react, engine-gateway
- **🔶 Alpha (3):** composables, dark-matter, project-engine
- **🔒 Internal (3):** test-utils, validation, domain

### By Category

**RDF Core:**

- `@unrdf/core` (main)
- `@unrdf/oxigraph` (backend)

**Autonomous Behaviors:**

- `@unrdf/hooks` (framework)

**Scalability:**

- `@unrdf/streaming` (large graphs)
- `@unrdf/federation` (distributed)

**Integrations:**

- `@unrdf/browser` (client-side)
- `@unrdf/react` (React)
- `@unrdf/composables` (Vue)
- `@unrdf/cli` (terminal)

**Utilities:**

- `@unrdf/engine-gateway` (API layer)
- `@unrdf/dark-matter` (optimization)
- `@unrdf/project-engine` (workspace)

---

## Dependency Tree

```
Every App
├── @unrdf/core (required foundation)
│   ├── @unrdf/oxigraph (for persistence)
│   ├── @unrdf/hooks (for behaviors)
│   ├── @unrdf/streaming (optional, for scale)
│   └── @unrdf/federation (optional, for distribution)
│
└── Integration layers (pick one or more):
    ├── @unrdf/browser (for client-side)
    ├── @unrdf/react (for React)
    ├── @unrdf/cli (for terminal)
    └── @unrdf/engine-gateway (for API)
```

---

## Common Patterns

### Pattern 1: Simple In-Memory App

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';

const core = await createKnowledgeSubstrateCore();
const store = core.parseRdf(turtleData);
const results = await core.query(store, sparqlQuery);
```

**Packages needed:** `@unrdf/core` only

---

### Pattern 2: Persistent + Reactive

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';
import { createOxigraphStore } from '@unrdf/oxigraph';
import { defineHook, registerHook } from '@unrdf/hooks';

const store = await createOxigraphStore('my-db.db');
const core = await createKnowledgeSubstrateCore({ store });

defineHook({
  trigger: 'INSERT',
  pattern: '?s ex:status ?o',
  run: event => console.log('Status changed!', event),
});

registerHook(hook);
```

**Packages needed:** `@unrdf/core`, `@unrdf/oxigraph`, `@unrdf/hooks`

---

### Pattern 3: Large Graph + Browser

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/browser';
import { streamLargeGraph } from '@unrdf/streaming';

const core = await createKnowledgeSubstrateCore({
  backend: 'indexeddb',
});

// Stream multi-GB graph without memory issues
const stream = streamLargeGraph('huge-graph.nt');
for await (const quad of stream) {
  store.addQuad(quad);
}
```

**Packages needed:** `@unrdf/browser`, `@unrdf/streaming`

---

### Pattern 4: Federated Query + React

```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/core';
import { useRdfQuery } from '@unrdf/react';
import { federatedQuery } from '@unrdf/federation';

// In React component
const { data, loading } = useRdfQuery(`
  SELECT * WHERE { ?s ?p ?o }
`);
```

**Packages needed:** `@unrdf/core`, `@unrdf/federation`, `@unrdf/react`

---

## Directory Structure

```
/home/user/unrdf/
├── packages/                          # 73+ monorepo packages
│   ├── core/                          # @unrdf/core (essential)
│   ├── oxigraph/                      # @unrdf/oxigraph (persistent backend)
│   ├── hooks/                         # @unrdf/hooks
│   ├── streaming/                     # @unrdf/streaming
│   ├── federation/                    # @unrdf/federation
│   ├── knowledge-engine/              # @unrdf/knowledge-engine
│   ├── browser/                       # @unrdf/browser
│   ├── cli/                           # @unrdf/cli
│   ├── react/                         # @unrdf/react (React integration)
│   ├── composables/                   # @unrdf/composables (Vue)
│   ├── dark-matter/                   # @unrdf/dark-matter
│   ├── project-engine/                # @unrdf/project-engine
│   ├── engine-gateway/                # @unrdf/engine-gateway
│   ├── test-utils/                    # @unrdf/test-utils (private)
│   ├── validation/                    # @unrdf/validation (private)
│   └── domain/                        # @unrdf/domain (private)
│
├── docs/                              # Documentation (100+ files)
├── src/                               # Legacy source code
├── test/                              # Test utilities
├── examples/                          # Example projects
├── scripts/                           # Build & automation scripts
├── root package.json                  # Workspace config
└── CLAUDE.md                          # Code guidelines
```

---

## Getting Help

| Question                            | Answer                                   | Resource                                         |
| ----------------------------------- | ---------------------------------------- | ------------------------------------------------ |
| What should I use to build my app?  | Start with `@unrdf/core` + what you need | [START-HERE.md](START-HERE.md)                   |
| How do I set up my dev environment? | Follow pnpm setup & test commands        | [LOCAL-DEVELOPMENT.md](LOCAL-DEVELOPMENT.md)     |
| What does each package contain?     | Detailed structure and exports           | [WORKSPACE-STRUCTURE.md](WORKSPACE-STRUCTURE.md) |
| How do I add a new package?         | Step-by-step package creation guide      | [PACKAGE-DEVELOPMENT.md](PACKAGE-DEVELOPMENT.md) |
| How do I test the monorepo?         | Cross-package testing strategies         | [TESTING-STRATEGY.md](TESTING-STRATEGY.md)       |

---

## Pro Tips

1. **Always start with `@unrdf/core`** - it's the foundation
2. **Use `@unrdf/oxigraph` for persistence** - memory alone won't scale
3. **Knowledge Hooks react to changes** - use them for automation
4. **Streaming handles large graphs** - IndexedDB fills browser memory
5. **Federation queries multiple stores** - scale horizontally
6. **CLI is your REPL** - quick testing from terminal
7. **OTEL validation is truth** - run validation before deploying

---

## Version Matrix

| Package               | Version | Status        |
| --------------------- | ------- | ------------- |
| Root                  | 26.4.3  | ✅ Production |
| @unrdf/core           | latest  | ✅ Production |
| @unrdf/oxigraph       | latest  | ✅ Production |
| @unrdf/hooks          | latest  | ✅ Production |
| @unrdf/streaming      | latest  | ✅ Production |
| @unrdf/federation     | latest  | ✅ Production |
| @unrdf/browser        | latest  | ✅ Production |
| @unrdf/cli            | latest  | ✅ Production |
| @unrdf/react          | v4.1.1  | ✅ Production |
| @unrdf/composables    | latest  | 🔶 Alpha      |
| @unrdf/dark-matter    | latest  | 🔶 Alpha      |
| @unrdf/project-engine | latest  | 🔶 Alpha      |
| @unrdf/engine-gateway | v0.1.0  | ✅ Production |
| @unrdf/test-utils     | v1.0.0  | 🔒 Private    |
| @unrdf/validation     | v1.0.0  | 🔒 Private    |
| @unrdf/domain         | v1.0.0  | 🔒 Private    |

---

**Need the full story?** → [PACKAGES.md](PACKAGES.md) | **Want to contribute?** → [PACKAGE-DEVELOPMENT.md](PACKAGE-DEVELOPMENT.md)
