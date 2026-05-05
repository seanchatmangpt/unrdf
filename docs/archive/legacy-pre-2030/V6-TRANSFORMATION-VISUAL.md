# V6 Transformation Visualization

## Before: 57 Packages (Chaos)

```
UNRDF v5 - Package Explosion
┌─────────────────────────────────────────────────────────────────────┐
│                         APPLICATIONS LAYER                          │
│  @unrdf/cli                 @unrdf/kgc-cli         @unrdf/kgc-tools │
│  @unrdf/react               @unrdf/composables     @unrdf/yawl-viz  │
│  @unrdf/rdf-graphql         @unrdf/serverless      @unrdf/yawl-api  │
│                                                                      │
│  Problem: 3 CLIs? Why separate react and composables?               │
├─────────────────────────────────────────────────────────────────────┤
│                      INTEGRATION LAYER (9 packages!)                │
│  @unrdf/yawl-kafka          @unrdf/yawl-queue     @unrdf/yawl-api   │
│  @unrdf/yawl-langchain      @unrdf/yawl-realtime  @unrdf/yawl-viz   │
│  @unrdf/yawl-observability  @unrdf/yawl-ai        @unrdf/yawl-durable│
│                                                                      │
│  Problem: 9 packages for thin adapters (400-1500 LOC each)          │
├─────────────────────────────────────────────────────────────────────┤
│                      AI/ML LAYER (5 packages)                        │
│  @unrdf/ml-inference        @unrdf/ml-versioning                    │
│  @unrdf/semantic-search     @unrdf/decision-fabric @unrdf/yawl-ai   │
│                                                                      │
│  Problem: All need shared vector infrastructure, why separate?      │
├─────────────────────────────────────────────────────────────────────┤
│                    WORKFLOW LAYER (1 package)                        │
│  @unrdf/yawl (39K LOC - largest package)                            │
├─────────────────────────────────────────────────────────────────────┤
│                  RUNTIME LAYER (4 packages)                          │
│  @unrdf/streaming ←→ @unrdf/federation ←→ @unrdf/consensus          │
│  @unrdf/collab                                                      │
│                                                                      │
│  Problem: Circular dependencies! Federation needs consensus,        │
│           consensus needs federation                                │
├─────────────────────────────────────────────────────────────────────┤
│               GOVERNANCE LAYER (11 packages!)                        │
│  @unrdf/kgc-4d             @unrdf/receipts        @unrdf/blockchain │
│  @unrdf/kgc-multiverse     @unrdf/kgc-substrate   @unrdf/v6-core    │
│  @unrdf/kgc-claude         @unrdf/kgc-runtime     @unrdf/kgc-swarm  │
│  @unrdf/kgc-cli            @unrdf/kgc-tools                         │
│                                                                      │
│  Problem: All do cryptographic receipts! Why 11 packages?           │
│  - kgc-4d has freezeUniverse + verifyReceipt                        │
│  - receipts has generateBatchReceipt + verifyBatchReceipt           │
│  - blockchain has MerkleProofGenerator                              │
│  - ALL DUPLICATE MERKLE TREE CODE!                                  │
├─────────────────────────────────────────────────────────────────────┤
│                    KNOWLEDGE LAYER (2 packages)                      │
│  @unrdf/hooks              @unrdf/knowledge-engine                  │
│                                                                      │
│  Problem: Both do rule-based pattern matching. Pick one abstraction │
├─────────────────────────────────────────────────────────────────────┤
│                      CORE LAYER (unclear!)                           │
│  @unrdf/core (23K LOC - kitchen sink)                               │
│  @unrdf/oxigraph                                                    │
│  @unrdf/v6-core (13K LOC - what's the difference from core?)        │
│                                                                      │
│  Problem: What IS core? Why 2 core packages?                        │
├─────────────────────────────────────────────────────────────────────┤
│                    TOOLING LAYER (5 packages)                        │
│  @unrdf/test-utils         @unrdf/validation      @unrdf/diataxis-kit│
│  @unrdf/kgc-docs           @unrdf/kgc-probe (16K LOC!)              │
│                                                                      │
│  Problem: All dev tooling, why separate packages?                   │
├─────────────────────────────────────────────────────────────────────┤
│                    CRUFT LAYER (12+ packages)                        │
│  @unrdf/fusion (marketing)     @unrdf/atomvm (niche, 8K LOC)        │
│  @unrdf/kgn (template, 18K LOC)                                     │
│  @unrdf/caching (premature)    @unrdf/graph-analytics (thin wrapper)│
│  @unrdf/dark-matter (0 LOC)    @unrdf/engine-gateway (0 LOC)        │
│  @unrdf/composables (0 LOC)    @unrdf/domain (0 LOC)                │
│  ... and more                                                        │
│                                                                      │
│  Problem: Vaporware, premature optimization, or wrong scope         │
└─────────────────────────────────────────────────────────────────────┘

Total: 57 packages, 417K LOC, 10+ layers, circular dependencies
Time to understand: 2-3 days
```

---

## After: 12 Packages (Clarity)

```
UNRDF v6 - Clean Architecture
┌─────────────────────────────────────────────────────────────────────┐
│                    LAYER 3: APPLICATIONS                            │
│                                                                      │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐             │
│  │  @unrdf/cli  │  │ @unrdf/tools │  │   @unrdf/ui  │             │
│  │              │  │              │  │              │             │
│  │  8K LOC      │  │  8K LOC      │  │  4K LOC      │             │
│  │              │  │              │  │              │             │
│  │ Consolidated │  │ Testing +    │  │ React/Vue +  │             │
│  │ 3 CLIs into  │  │ Docs +       │  │ Viz all in   │             │
│  │ one with     │  │ Benchmarks   │  │ one package  │             │
│  │ plugins      │  │              │  │              │             │
│  └──────────────┘  └──────────────┘  └──────────────┘             │
│                                                                      │
│  ┌──────────────┐  ┌──────────────┐                                │
│  │@unrdf/integr.│  │  @unrdf/ai   │                                │
│  │              │  │              │                                │
│  │  6K LOC      │  │  5K LOC      │                                │
│  │              │  │              │                                │
│  │ Kafka/REST/  │  │ ML inference │                                │
│  │ GraphQL/     │  │ Semantic     │                                │
│  │ Queue all    │  │ search with  │                                │
│  │ as plugins   │  │ shared       │                                │
│  │              │  │ embeddings   │                                │
│  └──────────────┘  └──────────────┘                                │
│                                                                      │
│  Benefit: 17 packages → 5 packages                                  │
│  Import pattern: @unrdf/integrations/kafka (plugin model)           │
├─────────────────────────────────────────────────────────────────────┤
│                    LAYER 2: RUNTIME                                 │
│                                                                      │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐             │
│  │ @unrdf/      │  │  @unrdf/     │  │  @unrdf/     │             │
│  │ workflows    │  │  runtime     │  │  hooks       │             │
│  │              │  │              │  │              │             │
│  │  35K LOC     │  │  10K LOC     │  │  12K LOC     │             │
│  │              │  │              │  │              │             │
│  │ YAWL engine  │  │ Streaming +  │  │ Policies +   │             │
│  │ + durable    │  │ Federation + │  │ Rules        │             │
│  │ execution    │  │ Consensus +  │  │ unified      │             │
│  │              │  │ Collab       │  │              │             │
│  └──────────────┘  └──────────────┘  └──────────────┘             │
│                                                                      │
│  ┌──────────────┐                                                   │
│  │ @unrdf/      │                                                   │
│  │observability │  ← Cross-cutting: Can be imported by ANY layer   │
│  │              │                                                   │
│  │  5K LOC      │                                                   │
│  │              │                                                   │
│  │ Single OTEL  │                                                   │
│  │ metrics/     │                                                   │
│  │ tracing      │                                                   │
│  │ layer        │                                                   │
│  └──────────────┘                                                   │
│                                                                      │
│  Benefit: 16 packages → 4 packages                                  │
│  NO circular dependencies - runtime consolidates all distributed    │
│  concerns (streaming, federation, consensus, collab)                │
├─────────────────────────────────────────────────────────────────────┤
│                    LAYER 1: FOUNDATION                              │
│                                                                      │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐             │
│  │ @unrdf/store │  │  @unrdf/rdf  │  │ @unrdf/      │             │
│  │              │  │              │  │ governance   │             │
│  │  8K LOC      │  │  6K LOC      │  │              │             │
│  │              │  │              │  │  12K LOC     │             │
│  │ Oxigraph +   │  │ Data model + │  │              │             │
│  │ SPARQL       │  │ Parsers +    │  │ Provenance + │             │
│  │ execution    │  │ Validation   │  │ Receipts +   │             │
│  │              │  │              │  │ Time-travel  │             │
│  │ ZERO deps ───┼──┤ ZERO deps ───┼──┤ ZERO deps    │             │
│  └──────────────┘  └──────────────┘  └──────────────┘             │
│        ↑                  ↑                  ↑                       │
│        │                  │                  │                       │
│        └──────────────────┴──────────────────┘                       │
│              PARALLEL, NO INTERDEPENDENCIES                          │
│                                                                      │
│  Benefit: 18 packages → 3 packages                                  │
│  - Consolidated 5 governance packages (kgc-4d, receipts,            │
│    blockchain, kgc-multiverse, kgc-substrate) into ONE              │
│  - Single Merkle tree implementation (was duplicated 3x)            │
│  - Split "core" into store (execution) + rdf (data model)           │
│  - Foundation has ZERO internal dependencies = maximum reusability  │
└─────────────────────────────────────────────────────────────────────┘

Total: 12 packages, 120K LOC, 3 layers, ZERO circular dependencies
Time to understand: 1 hour
```

---

## Side-by-Side Comparison

### Package Count by Category

| Category | v5 Packages | v6 Packages | Reduction |
|----------|-------------|-------------|-----------|
| **Governance/Provenance** | 11 (kgc-4d, receipts, blockchain, kgc-multiverse, kgc-substrate, v6-core, kgc-runtime, kgc-claude, kgc-swarm, kgc-cli, kgc-tools) | 1 (@unrdf/governance) | 91% |
| **YAWL Integrations** | 9 (yawl-api, yawl-kafka, yawl-queue, yawl-langchain, yawl-observability, yawl-realtime, yawl-viz, yawl-ai, yawl-durable) | 1 (@unrdf/integrations) + merged obs | 89% |
| **Runtime Distribution** | 4 (streaming, federation, consensus, collab) | 1 (@unrdf/runtime) | 75% |
| **AI/ML** | 5 (ml-inference, ml-versioning, semantic-search, decision-fabric, yawl-ai) | 1 (@unrdf/ai) | 80% |
| **UI** | 4 (react, composables, yawl-viz, yawl-realtime) | 1 (@unrdf/ui) | 75% |
| **CLI** | 3 (cli, kgc-cli, kgc-tools) | 1 (@unrdf/cli) | 67% |
| **Tools** | 5 (test-utils, validation, diataxis-kit, kgc-docs, kgc-probe) | 1 (@unrdf/tools) | 80% |
| **Core/RDF** | 3 (core, oxigraph, v6-core) | 2 (@unrdf/store, @unrdf/rdf) | 33% |
| **Hooks/Rules** | 2 (hooks, knowledge-engine) | 1 (@unrdf/hooks) | 50% |
| **Workflows** | 1 (yawl) | 1 (@unrdf/workflows) | 0% (kept as-is) |
| **Observability** | 2 (observability, yawl-observability) | 1 (@unrdf/observability) | 50% |
| **KILLED** | 17 (atomvm, fusion, kgc-claude, kgn, caching, etc.) | 0 | 100% |
| **TOTAL** | **57** | **12** | **79%** |

---

## Lines of Code Reduction

```
v5 LOC Distribution (417K total):

█████████████████████ yawl (39K)
███████████████████ kgc-claude (23K) ────────┐
███████████████████ core (23K)              │
███████████ kgn (18K) ──────────────────┐   │
█████████ kgc-cli (17K)                 │   │
█████████ kgc-probe (16K)               │   │  KILLED
████████ v6-core (13K)                  │   │  or moved to
████████ kgc-runtime (11K) ─────────────┤   │  separate
████████ hooks (10K)                    │   │  repos
██████ kgc-swarm (8K) ──────────────────┘   │
██████ atomvm (8K) ─────────────────────────┘
██████ fusion (8K)
█████ kgc-4d (6K)
█████ knowledge-engine (5K)
████ cli (4K)
████ federation (4K)
████ kgc-multiverse (4K)
███ streaming (3K)
... (40 more packages)


v6 LOC Distribution (120K total):

███████████████████ workflows (35K) ← yawl core kept
████████ governance (12K) ← Consolidated 5 packages
████████ hooks (12K) ← Merged knowledge-engine
██████ runtime (10K) ← Consolidated 4 packages
██████ store (8K) ← Split from core
██████ cli (8K) ← Consolidated 3 packages
██████ tools (8K) ← Consolidated 5 packages
████ rdf (6K) ← Split from core
████ integrations (6K) ← Consolidated 6 packages
████ observability (5K) ← Consolidated 2 packages
████ ai (5K) ← Consolidated 5 packages
███ ui (4K) ← Consolidated 4 packages

71% reduction through deduplication and elimination
```

---

## Dependency Graph Transformation

### Before: Circular Dependencies

```
@unrdf/federation ←──────┐
        │                │
        │ depends on     │
        ↓                │
@unrdf/consensus ────────┘  CIRCULAR!
        │
        │
        ↓
@unrdf/streaming

Problem: Federation needs consensus for coordination,
         consensus needs federation for distributed queries
         Result: Build order issues, unclear ownership
```

### After: Clean Layers

```
LAYER 3: Applications
  ↓ depends on
LAYER 2: Runtime
  ↓ depends on
LAYER 1: Foundation (parallel, no interdeps)

@unrdf/runtime (Layer 2)
    ├── includes federation
    ├── includes consensus
    ├── includes streaming
    └── includes collab
         ↓ depends on
@unrdf/store + @unrdf/rdf (Layer 1, parallel)

Result: NO circular dependencies, clear build order
```

---

## Import Pattern Transformation

### Before: Confusing Imports

```javascript
// Which package has receipts?
import { verifyReceipt } from '@unrdf/kgc-4d'; // or is it...
import { verifyBatchReceipt } from '@unrdf/receipts'; // this one?
import { ReceiptAnchorer } from '@unrdf/blockchain'; // or this?

// Which package has Merkle trees?
import { buildMerkleTree } from '@unrdf/receipts/merkle-batcher';
import { MerkleProofGenerator } from '@unrdf/blockchain/merkle';
// DUPLICATE IMPLEMENTATIONS!

// What's the difference?
import { createStore } from '@unrdf/core';
import { createStore } from '@unrdf/v6-core'; // ???

// Why separate packages?
import { useQuery } from '@unrdf/react';
import { useRdfStore } from '@unrdf/composables';
```

### After: Intuitive Imports

```javascript
// All provenance in one place
import { verifyReceipt, buildMerkleTree, anchorToBlockchain }
  from '@unrdf/governance';

// Clear separation: execution vs. data model
import { createStore, executeQuery } from '@unrdf/store';
import { quad, namedNode, parseTurtle } from '@unrdf/rdf';

// Plugin model for integrations
import { createRestServer } from '@unrdf/integrations/rest';
import { KafkaProducer } from '@unrdf/integrations/kafka';

// Framework-specific UI via subpaths
import { useQuery } from '@unrdf/ui/react';
import { useRdfStore } from '@unrdf/ui/vue';
```

---

## Migration Path Visualization

```
Phase 1: Foundation (Weeks 1-2)
├─ Create @unrdf/store
│  └─ Migrate from: oxigraph + core/sparql
├─ Create @unrdf/rdf
│  └─ Migrate from: core/rdf + parsers
└─ Create @unrdf/governance
   └─ Consolidate: kgc-4d, receipts, blockchain, kgc-multiverse, kgc-substrate
   └─ Dedup: 3 Merkle tree implementations → 1

Phase 2: Runtime (Weeks 3-4)
├─ Create @unrdf/workflows
│  └─ Migrate from: yawl + yawl-durable
├─ Create @unrdf/runtime
│  └─ Consolidate: streaming, federation, consensus, collab
│  └─ Eliminate: Circular dependency between federation ↔ consensus
├─ Create @unrdf/hooks
│  └─ Merge: hooks + knowledge-engine
└─ Create @unrdf/observability
   └─ Consolidate: observability + yawl-observability

Phase 3: Applications (Weeks 5-6)
├─ Create @unrdf/integrations
│  └─ Consolidate: yawl-api, yawl-kafka, yawl-queue, yawl-langchain, rdf-graphql, serverless
├─ Create @unrdf/ai
│  └─ Consolidate: ml-inference, ml-versioning, semantic-search, decision-fabric, yawl-ai
├─ Create @unrdf/ui
│  └─ Consolidate: react, composables, yawl-viz, yawl-realtime
├─ Create @unrdf/cli
│  └─ Consolidate: cli, kgc-cli, kgc-tools
└─ Create @unrdf/tools
   └─ Consolidate: test-utils, validation, diataxis-kit, kgc-docs, kgc-probe

Phase 4: Validation (Week 7)
└─ Quality gates: Tests, benchmarks, security, docs
```

---

## Key Metrics Transformation

| Metric | v5 (Before) | v6 (After) | Change |
|--------|-------------|------------|--------|
| **Packages** | 57 | 12 | -79% |
| **Total LOC** | 417,144 | ~120,000 | -71% |
| **Layers** | 10+ (unclear) | 3 (clean) | -70% |
| **Circular deps** | 2+ known | 0 | -100% |
| **Duplicate code** | High (3x Merkle trees) | Zero | -100% |
| **Build time** | ~60s | <20s target | -67% |
| **Test time** | ~45s | <15s target | -67% |
| **Onboarding** | 2-3 days | 4 hours | -83% |
| **Time to understand architecture** | 2-3 days | 1 hour | -95% |
| **"What package do I need?"** | Unclear | Obvious | ∞% improvement |

---

## The Transformation in One Sentence

**Before**: "We have 57 packages with 11 doing cryptographic receipts, 9 for thin adapters, 3 CLIs, and nobody knows what 'core' means."

**After**: "We have 12 packages in 3 clean layers: foundation (store, rdf, governance), runtime (workflows, runtime, hooks, observability), and applications (cli, integrations, ai, ui, tools)."

---

## Questions Answered

### v5: Which package should I use for...
- ❓ Receipts? (kgc-4d, receipts, blockchain, v6-core all have receipt functions)
- ❓ Time-travel? (kgc-4d, kgc-multiverse, or kgc-substrate?)
- ❓ CLI? (cli, kgc-cli, or kgc-tools?)
- ❓ React hooks? (react or composables?)
- ❓ Kafka? (Is it yawl-kafka or somewhere in integrations?)

### v6: Which package should I use for...
- ✅ Receipts? → **@unrdf/governance**
- ✅ Time-travel? → **@unrdf/governance/time-travel**
- ✅ CLI? → **@unrdf/cli**
- ✅ React hooks? → **@unrdf/ui/react**
- ✅ Kafka? → **@unrdf/integrations/kafka**

**No ambiguity. Every concern has ONE package.**

---

This transformation represents a fundamental architectural cleanup: from 57 fragmented packages with unclear boundaries to 12 well-defined packages with zero duplication and clean layer separation.
