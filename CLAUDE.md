# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## UNRDF v26.4.7 - Research-Grade RDF Knowledge Graph Platform

> **Status**: Research Prototype - Architecturally complete, not production-validated  
> **Language**: JavaScript ESM (.mjs), Zod runtime validation  
> **Package Manager**: pnpm (required - see package.json engines)  
> **Test Framework**: Vitest 4.0.16

---

## Quick Facts (Verified)

| Fact               | Source                                                     |
| ------------------ | ---------------------------------------------------------- |
| **Version**        | `cat package.json` → `"version": "26.4.5"`                 |
| **Packages**       | 59+ publishable packages (pnpm workspace)                  |
| **Core**           | @unrdf/core, @unrdf/oxigraph, @unrdf/hooks, @unrdf/v6-core |
| **Test Pass Rate** | 100% (core packages, see README.md consolidation)          |
| **Node.js**        | >=18.0.0 (volta: 18.19.0)                                  |
| **pnPM**           | >=7.0.0 (volta: 8.15.0)                                    |

---

## Critical Rules (Non-Negotiable)

### FIX FORWARD ONLY

- NEVER `git reset --hard` or destructive git operations
- Fix issues in place → debug → apply targeted fixes
- Commits are immutable; solve problems by adding commits
- Exception: `git revert` (creates new commit) is allowed if absolutely necessary

### Code Standards

- **Format**: ESM only (`.mjs` extension)
- **Validation**: Zod schemas for all public APIs
- **File Size**: Max 500 lines per file
- **Test Coverage**: Minimum 80% (line/branch/function)

### Verify, Don't Assume

- Run ALL commands before claiming success
- Read actual output — don't assume test pass
- Use authoritative sources: package.json, README.md, actual CLI output
- Measure performance with OTEL validation

---

## Essential Commands

### Testing (Most Common)

```bash
# All tests (includes coverage, slow ~2min)
pnpm test

# Fast pre-push suite (excludes kgc-probe)
pnpm test:fast

# Single package tests
pnpm --filter @unrdf/core test
pnpm --filter @unrdf/hooks test

# Single test file (fast iteration)
pnpm --filter @unrdf/core exec vitest run test/sparql.test.mjs

# Watch mode for development
pnpm --filter @unrdf/core exec vitest

# Coverage reports
pnpm test:coverage
```

### Quality & Build

```bash
# Linting
pnpm lint              # Check all packages
pnpm lint:fix          # Auto-fix issues
pnpm --filter @unrdf/core lint  # Single package

# Building
pnpm build             # Build all packages
pnpm clean             # Clean artifacts

# Format
pnpm format            # Prettier format all
pnpm format:check      # Check formatting
```

### Development Workflow

```bash
# Quick verification before commit
timeout 5s pnpm test:fast    # Pre-push tests
timeout 30s pnpm lint        # Linting check
timeout 60s pnpm build       # Build verification

# MCP sync (required before commit)
pnpm mcp:sync            # Sync MCP definitions
pnpm mcp:sync:dry        # Dry-run with verbose output
```

---

## Architecture: O\* Innovations 4-6

UNRDF implements three core innovations for autonomous knowledge graph management:

### Innovation 4: Federation Quorum

- M-of-N voting with BLAKE3 receipt chaining
- Deterministic quorum computation
- See: `packages/federation/`

### Innovation 5: Hooks Marketplace

- SPARQL CONSTRUCT normalization
- N3 forward-chaining dependency resolution
- SHACL soft-fail validation
- See: `packages/hooks/`

### Innovation 6: Streaming Admission

- Delta receipts with chaining
- Input/output/delta BLAKE3 hashes
- See: `packages/streaming/`

### System Layers

```
Application Layer (CLI, APIs, Browser, MCP)
        ↓
O* Innovations (Federation, Marketplace, Streaming)
        ↓
Knowledge Substrate (Hooks, Transactions, Validation)
        ↓
RDF Core (SPARQL, SHACL, Storage)
        ↓
Backends (Memory, Oxigraph, Remote)
```

**Key Integration**: All innovations use `@unrdf/v6-core` for receipt chaining and BLAKE3 hashing.

---

## Package Structure (59+ Packages)

### Core (Production Ready)

- **`@unrdf/core`** - RDF storage, SPARQL, SHACL validation
- **`@unrdf/oxigraph`** - Rust-based persistent backend
- **`@unrdf/hooks`** - Autonomous behavior framework
- **`@unrdf/v6-core`** - Receipt chaining, BLAKE3 hashing

### Extended (Production Ready)

- **`@unrdf/daemon`** - Background orchestrator, MCP server, Groq LLM integration
- **`@unrdf/streaming`** - Large graph streaming & sync
- **`@unrdf/federation`** - Distributed query execution, quorum voting
- **`@unrdf/cli`** - Command-line interface
- **`@unrdf/otel`** - OpenTelemetry integration (generated from Weaver)

### Optional/Alpha

- **`@unrdf/browser`** - Browser runtime with IndexedDB
- **`@unrdf/react`** - React hooks & components
- **`@unrdf/composables`** - Vue 3 composables
- **`@unrdf/dark-matter`** - Query optimization
- **`@unrdf/knowledge-engine`** - ⚠️ REMOVED (use @unrdf/core instead)

### Internal

- **`@unrdf/test-utils`** - Shared testing infrastructure
- **`@unrdf/validation`** - OTEL validation & compliance

### YAWL Ecosystem (12+ packages)

- `@unrdf/yawl` - Core workflow engine
- `@unrdf/yawl-api`, `@unrdf/yawl-observability`, `@unrdf/yawl-kafka`, etc.

### KGC Ecosystem (10+ packages)

- Knowledge Graph Construction tools
- `@unrdf/kgc-4d`, `@unrdf/kgc-runtime`, `@unrdf/kgc-swarm`, etc.

---

## MCP Integration & Self-Play Autonomics

UNRDF v26.4.4+ introduces **Knowledge Self-Play Autonomics** — a closed-loop system where RDF graphs autonomously improve themselves:

```javascript
import { createKnowledgeSelfPlayLoop } from '@unrdf/daemon/knowledge-self-play';

const loop = createKnowledgeSelfPlayLoop(store, engine);
const result = await loop.run();
// result.converged === true when graph reaches stable state
```

**Key Files**:

- `packages/daemon/src/mcp/` - MCP protocol implementation
- `packages/daemon/GROQ-INTEGRATION.md` - Groq LLM provider docs
- `packages/daemon/LOCAL-AGENTS-GUIDE.md` - Local agent deployment
- `docs/MCP_INTEGRATION.md` - Complete MCP guide

---

## Gotchas (Organized by Category)

### Zod v4 Gotchas

- **`.args()` / `.returns()` removed**: Use bare `z.function()` instead
- **`parse()` returns plain object**: Prototype methods lost on class instances; use validate-only pattern
- **`eslint-env` comments ignored**: Use `/* global window, document */` for flat-config ESLint

### Streaming / Transform Gotchas

- **Object-mode Transform stalls**: Call `stream.resume()` before `stream.pipe()` if no downstream consumer
- **Hash tamper tests flaky (1/16)**: Use `const last = hash[63]; hash.slice(0, 63) + (last === 'x' ? 'y' : 'x')`

### Hooks Package Gotchas

- **N3 quad spread broken**: `{...quad}` doesn't copy prototype properties; use explicit `{ subject: quad.subject, predicate: quad.predicate, ... }`
- **Infinite loops hang vitest**: `while(true)` blocks event loop; use async hooks with `await`
- **Performance thresholds flaky**: Relax μs-level assertions to 50–200ms for CI stability
- **`executeHooksByTrigger` returns `ChainResult`**: Use `result.valid`, not `result[0].valid`

### OTel Weaver Gotchas

- **Weaver binary**: `~/.cargo/bin/weaver` v0.22.1+ (commands: `registry check`, `registry generate`, `registry live-check`)
- **Template path**: `templates/{registry_basename}/{target}/` (e.g., `--registry registry/` → `templates/registry/js/`)
- **`application_mode: single`**: Required in `otel/templates/registry/js/weaver.yaml`
- **Template context**: Iterate `ctx.groups`, NOT `ctx` (which is a dict)
- **Attribute field**: Use `attr.name`, not `attr.id`
- **Manifest format**: Use `schema_url: https://opentelemetry.io/schemas/1.28.0`
- **Live-check JSON**: Tagged entity format `[{"span":{...}},{"metric":{...}}]`
- **Stability**: Use `stability: stable` for production (experimental triggers per-attribute advisories)
- **Generated code**: `@unrdf/otel` is generated; don't edit `packages/otel/src/generated/` directly

### Test Utils (`@unrdf/test-utils`)

- **Location**: `packages/test-utils/src/index.mjs` (private workspace package)
- **Import pattern**: Use relative imports (pnpm workspace linking unreliable)
- **Exports**: `namedNode, literal, quad, blankNode, defaultGraph, df, createTestStore, createStoreWith, VOCAB, terms, entities, SPARQL, createMockFetch, createMockSpan, createMockTracer, benchmarkSync, assertPerf`
- **pnpm install blocker**: Full `pnpm install` may fail on unrelated packages; use `pnpm install --filter @unrdf/X`
- **Pre-existing failures**: `federation/test/metrics.test.mjs` (21 failures) - do NOT fix unless asked

### Open-Ontologies Integration (v26.4.7+)

- **Binary location**: `~/.local/bin/open-ontologies` — must be installed for MCP tools to work
- **Data directory**: `~/.open-ontologies/` (SQLite store for in-memory ontology data)
- **15 MCP tools**: `onto_validate`, `onto_stats`, `onto_query`, `onto_load`, `onto_marketplace`, `onto_reason`, `onto_shacl`, `onto_save`, `onto_clear`, `onto_convert`, `onto_align`, `onto_drift`, `onto_plan`, `onto_apply`, `onto_version`
- **Integration test**: `node playground/examples/open-ontologies-integration.mjs` — requires `disney-governed-universe.ttl`
- **Quad spreading fix**: Use `cloneQuad(quad, {...overrides})` instead of `{...quad}` to preserve N3 prototype getters

### Groq LLM Integration (v26.4.7+)

- **API key requirement**: `GROQ_API_KEY` environment variable must be set
- **Ensemble provider**: Multi-model reasoning with automatic fallback
- **Integration tests**: 23/23 tests passing (12 MCP + 11 ensemble)
- **MCP tools**: AI-powered RDF reasoning via Groq LLM API
- **Test location**: `packages/daemon/test/groq-mcp-integration.test.mjs`, `packages/daemon/test/ensemble-groq.test.mjs`

### OTEL & Kubernetes Deployment (v26.4.7+)

- **Helm chart**: `k8s/helm/unrdf-observability/` — full observability stack
- **Kind cluster**: `k8s/kind-config.yaml` — local development cluster
- **Services**: Prometheus (9091), Grafana (3001), Tempo (13200), Loki (13100), Pyroscope (14040)
- **OTLP endpoint**: `localhost:14317` (gRPC), `localhost:14318` (HTTP)
- **Deployment guide**: `playground/OTEL-K8S-DEPLOYMENT-GUIDE.md`
- **One-command deploy**: `make k8s-create && make k8s-up`

---

## Git Workflow

- Branch from `main`
- Conventional commits: `type(scope): description`
- Always pull before push
- Never force-push to `main`
- Pre-commit: `pnpm mcp:sync && pnpm lint && pnpm test:fast`

---

## Key Documentation

| Document                                | Purpose                                      |
| --------------------------------------- | -------------------------------------------- |
| **package.json**                        | Authoritative version, dependencies, scripts |
| **README.md**                           | Project status, use cases, examples          |
| **ARCHITECTURE.md**                     | O\* Innovations 4-6 design                   |
| **docs/LOCAL-DEVELOPMENT.md**           | Dev environment setup                        |
| **docs/GETTING_STARTED.md**             | Installation and first example               |
| **packages/daemon/GROQ-INTEGRATION.md** | Groq LLM integration                         |
| **packages/daemon/AUTHENTICATION.md**   | Enterprise security (v26.4.4+)               |

---

## Session Quality Checklist

- [ ] Did I RUN commands, not just read code?
- [ ] Did I read FULL output (not stop at first pass)?
- [ ] Can I prove it works? (show output, not "should work")
- [ ] Are claims verifiable? (use authoritative sources)
- [ ] Did I sync MCP definitions before committing?

---

## Personal Notes

- **Mark** (~6yr Dachshund) - Noise-sensitive, guards workspace
