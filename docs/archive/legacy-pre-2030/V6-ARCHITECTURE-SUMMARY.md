# UNRDF V6 Architecture - Executive Summary

## The Problem

**Current State**: 57 packages, 417K LOC, unclear boundaries, massive duplication

**Specific Issues**:
- 4 packages doing cryptographic receipts (kgc-4d, receipts, blockchain, kgc-multiverse)
- 9 packages for thin YAWL adapters (400-1500 LOC each)
- 11 KGC packages with overlapping governance concerns
- 3 separate CLI packages
- Unclear what "core" vs "v6-core" vs "kgc-substrate" actually means

## The Solution

**Target State**: 12 packages, ~120K LOC, 3 clean layers, zero duplication

### 12 Packages (71% LOC reduction)

**LAYER 1: FOUNDATION** (no interdependencies)
1. **@unrdf/store** - Oxigraph bindings + SPARQL execution (8K LOC)
2. **@unrdf/rdf** - Data model + parsers + validation (6K LOC)
3. **@unrdf/governance** - Provenance + receipts + time-travel (12K LOC)

**LAYER 2: RUNTIME** (depends on Layer 1 only)
4. **@unrdf/workflows** - YAWL engine + durable execution (35K LOC)
5. **@unrdf/runtime** - Streaming + federation + consensus (10K LOC)
6. **@unrdf/hooks** - Policies + rules + inference (12K LOC)
7. **@unrdf/observability** - Metrics + tracing + logging (5K LOC)

**LAYER 3: APPLICATIONS** (depends on Layer 1 + 2)
8. **@unrdf/cli** - Command-line tools (8K LOC)
9. **@unrdf/integrations** - Kafka/REST/GraphQL adapters (6K LOC)
10. **@unrdf/ai** - ML inference + semantic search (5K LOC)
11. **@unrdf/ui** - React/Vue components + visualization (4K LOC)
12. **@unrdf/tools** - Testing + docs + benchmarks (8K LOC)

## Key Consolidations

| What We Did | Why | LOC Reduction |
|-------------|-----|---------------|
| **5 → 1 governance** | kgc-4d, receipts, blockchain, kgc-multiverse, kgc-substrate all do provenance | 14,881 → 12,000 |
| **9 → 1 integrations** | Thin adapters don't justify packages, use plugin model | 7,113 → 6,000 |
| **4 → 1 runtime** | Streaming, federation, consensus all distributed concerns | 11,886 → 10,000 |
| **5 → 1 ai** | Share embedding infrastructure across all AI features | 6,903 → 5,000 |
| **3 → 1 cli** | Single CLI with plugin system for extensions | 22,865 → 8,000 |
| **core → store + rdf** | Separate execution from data model | 23,616 → 14,000 |

## What We Killed (17 packages)

**Moved to separate repos**:
- @unrdf/kgc-claude (23K LOC) - Claude integration not core RDF
- @unrdf/kgn (18K LOC) - Template system separate concern
- @unrdf/kgc-swarm (8K LOC) - Multi-agent orchestration

**Premature/vaporware**:
- @unrdf/atomvm, caching, graph-analytics, fusion
- 5 packages with 0 LOC (dark-matter, engine-gateway, composables, etc.)

**Temporary**:
- @unrdf/v6-compat - Migration tool, not part of v6 core

## Architecture Principles

1. **Layered dependencies** - Layer 3 → Layer 2 → Layer 1 (no cycles)
2. **Foundation has zero internal deps** - store, rdf, governance are parallel
3. **One concern = one package** - No more "core" dumping ground
4. **Plugin model for integrations** - Not package-per-external-library
5. **Observability is cross-cutting** - Can be imported by any layer

## Success Metrics

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Packages | 57 | 12 | 79% reduction |
| LOC | 417K | 120K | 71% reduction |
| Layers | 5 | 3 | 40% reduction |
| Circular deps | Unknown | 0 | 100% elimination |
| Onboarding time | 2-3 days | 4 hours | 83% faster |

## Migration Timeline

- **Week 1-2**: Foundation (store, rdf, governance)
- **Week 3-4**: Runtime (workflows, runtime, hooks, observability)
- **Week 5-6**: Applications (cli, integrations, ai, ui, tools)
- **Week 7**: Validation (tests, benchmarks, security audit)

## The Litmus Test

**Question**: Can a new developer understand the entire architecture in 1 hour?

**v5 Answer**: NO - 57 packages with unclear boundaries, takes 2-3 days

**v6 Answer**: YES - 12 packages in 3 layers, read this doc in 15 minutes

---

**Next Steps**:
1. Review this proposal with team
2. Create ADRs for controversial decisions
3. Start Phase 1 implementation (foundation packages)
4. Set up v6 benchmarks and quality gates
