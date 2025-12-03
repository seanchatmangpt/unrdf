# UNRDF v5 Substrate Refactoring Roadmap

## Executive Summary
Move UNRDF from a **monolithic framework** (v4.x) to a **substrate platform** (v5.0+). Users build applications ON the substrate, not WITH the framework.

**Scope**: Remove 60-70% of code from core, keep 30-40% as essential substrate.

---

## Component Classification

### TIER 1: CORE SUBSTRATE (Keep in `/src`, essential for all VOCs)

#### RDF Operations (`/src/rdf/*`)
```
KEEP:
├── store-wrapping.mjs         // N3 Store wrapper
├── quad-operations.mjs        // Add/remove/iterate
├── canonicalize.mjs           // RDF canonicalization
├── namespaces.mjs            // Common namespaces
└── rdf-ext-adapters.mjs       // RDF-EXT integration

MOVE OUT: None (all core)
```
**Rationale**: All 7 VOCs need graph mutations. Essential substrate.

#### SPARQL Execution (`/src/sparql/*`)
```
KEEP:
├── comunica-wrapper.mjs       // Comunica executor
├── query-parser.mjs           // Parse SPARQL syntax
├── result-formatter.mjs       // Format results
└── schemas.mjs                // Validation schemas

MOVE OUT: None (all core)
```
**Rationale**: VOCs 1-5 all need SPARQL. Cannot remove.

#### Knowledge Hooks (`/src/knowledge-engine/`)
```
KEEP:
├── define-hook.mjs            // Hook definition API
├── hook-executor.mjs          // Simple executor
├── hook-management.mjs        // Hook registry
└── schemas.mjs                // Hook validation

MOVE OUT:
├── hook-executor-batching.mjs → @unrdf/advanced-hooks
├── effect-sandbox-browser.mjs → @unrdf/hooks-browser
└── All hook examples           → Docs only
```
**Rationale**: Simple hooks are core policy mechanism. Batching/sandboxing are optimizations.

#### Federation (`/src/knowledge-engine/federation/*`)
```
KEEP:
├── federation-coordinator.mjs // Peer connection
├── distributed-query-engine.mjs // Route queries
└── schemas.mjs                // Federation schemas

MOVE OUT:
├── data-replication.mjs        → @unrdf/federation-sync
└── Replication policies        → User code
```
**Rationale**: Discovery + querying are core. Replication strategy varies by deployment.

#### Streaming (`/src/knowledge-engine/streaming/*`)
```
KEEP:
├── change-feed.mjs            // Quad change events
├── subscription-manager.mjs    // Subscribe/unsubscribe
├── stream-processor.mjs        // Basic processing
└── schemas.mjs                // Stream schemas

MOVE OUT:
├── real-time-validator.mjs     → @unrdf/stream-validators
└── Advanced processing         → User code
```
**Rationale**: Change feed is essential for sync, streaming apps. Validation is optional.

#### Browser SDK (`/src/browser/*`)
```
KEEP:
├── indexeddb-store.mjs        // Persistent browser storage
├── comunica-browser-adapter.mjs // SPARQL in browser
├── fs-adapter.mjs             // File access shim
└── browser-shim.mjs           // Node.js API shims

MOVE OUT: None (all core for browser users)
```
**Rationale**: VOC-6 (app developers) need this for web apps.

#### CLI Core (`/src/cli/commands/`)
```
KEEP:
├── graph/create.mjs           // Create graphs
├── graph/delete.mjs           // Delete graphs
├── graph/describe.mjs         // Inspect graphs
├── graph/export.mjs           // Export data
├── context/*.mjs              // Context management
└── hook/eval.mjs              // Evaluate hooks

MOVE OUT:
├── Advanced query tools        → Docs
├── Project analysis            → @unrdf/project-engine
└── Optimization tools          → @unrdf/dark-matter
```
**Rationale**: CRUD + context + hook eval are essential. Analysis is optional.

#### Observability (`/src/observability/*`, `/src/profiling/*`)
```
KEEP:
├── OTEL instrumentation points // Trace creation
├── Metric collection hooks     // For deployments
└── Error sanitizer             // Security

MOVE OUT:
├── Domain-specific analysis    → User code
├── Performance profilers       → @unrdf/dark-matter
└── Custom reporters            → User code
```
**Rationale**: Instrumentation is essential for VOC-7. Analysis tools are optional.

#### Security (`/src/security/*`)
```
KEEP:
├── sandbox-restrictions.mjs    // Execution limits
├── error-sanitizer.mjs         // No data leaks
└── validation.mjs              // Input validation

MOVE OUT: None (all essential)
```
**Rationale**: All users need security controls.

#### Core Domain (`/src/domain/`)
```
KEEP:
├── types.mjs                   // Type definitions
├── constants.mjs               // Constants
└── formatters/                 // Output formatting

MOVE OUT:
├── models/paper.mjs            → Examples only
├── models/thesis.mjs           → Examples only
└── models/config.mjs           → User code
```
**Rationale**: Types + formatters are utilities. Domain models are specific to applications.

---

### TIER 2: COMPOSABLE EXTENSIONS (Extract to separate packages)

#### Knowledge Engine (`/src/knowledge-engine/*`)
```
STATUS: Move to @unrdf/knowledge-engine (separate package)

MODULES TO EXTRACT:
├── ai-semantic/               // AI enrichment
├── dark-matter/               // Query optimization
├── inference/ (if exists)     // Reasoning
├── monitoring/                // Graph monitoring
└── performance-optimizer.mjs  // Caching layer

REASON:
- VOCs 1-4 (agents) bring their own rule engines
- VOC-5 (data engineer) doesn't need this
- VOC-6 (app developer) brings own business logic
- Optional for deployments

PACKAGE: @unrdf/knowledge-engine
LOCATION: github.com/unrdf/knowledge-engine
VERSION: v2.0.0+ (separate version track)
```

#### Composables (`/src/composables/*`)
```
STATUS: Move to @unrdf/composables (separate package)

MODULES TO EXTRACT:
├── use-graph.mjs              // Graph state
├── use-delta.mjs              // Change tracking
├── use-prefixes.mjs           // Namespace mgmt
├── use-terms.mjs              // Term utilities
├── use-validator.mjs          // Validation
└── use-zod.mjs                // Zod integration

REASON:
- Specific to web applications (VOC-6)
- Data engineers (VOC-5) use CLI/scripts
- Agents (VOCs 1-4) build custom layers

PACKAGE: @unrdf/composables
LOCATION: github.com/unrdf/composables
DEPENDS ON: Vue 3+ (or framework agnostic wrapper)
```

#### Project Engine (`/src/project-engine/*`)
```
STATUS: Move to @unrdf/project-engine (separate package)

MODULES TO EXTRACT:
├── doc-generator.mjs          // Generate docs
├── gap-finder.mjs             // Find gaps
├── api-contract-validator.mjs // Validate contracts
├── autonomic-mapek.mjs        // Self-healing
└── All project analysis code

REASON:
- For self-hosting UNRDF itself, not using it
- Only UNRDF team needs initially
- Too specialized for general users

PACKAGE: @unrdf/project-engine
LOCATION: github.com/unrdf/project-engine
STATUS: Community contribution later
```

#### Dark Matter (`/src/knowledge-engine/dark-matter/*`)
```
STATUS: Move to @unrdf/dark-matter (separate package)

MODULES TO EXTRACT:
├── optimizer.mjs              // Query optimizer
├── critical-path.mjs          // Critical path analysis
├── query-analyzer.mjs         // Query analysis
└── All optimization logic

REASON:
- Optional performance improvement
- Many use cases don't need 80/20 optimization
- Can be added later if needed

PACKAGE: @unrdf/dark-matter
LOCATION: github.com/unrdf/dark-matter
VERSION: 1.0.0+ (separate)
```

#### Domain Models (`/src/domain/models/*`)
```
STATUS: Delete from core, move to examples

MODULES TO MOVE:
├── models/paper.mjs           → /examples/paper-model/
├── models/thesis.mjs          → /examples/thesis-model/
└── Related validators          → /examples/

REASON:
- Application-specific
- Not part of UNRDF substrate
- Show by example, not by code

LOCATION: /examples/domain-models/
DOCUMENTATION: How to define custom models
```

---

### TIER 3: CONSOLIDATE & SIMPLIFY

#### Delete Obsolete Code
```
DELETE:
├── /src/ken.mjs               // Legacy knowledge engine
├── /src/ken-parliament.mjs    // Obsolete parliament
├── /src/integration/          // Tightly coupled integrations
├── /src/profiling/            // Move to examples
└── All dead code branches
```

#### Simplify Remaining Code
```
CONSOLIDATE:
├── security/ → single validation module
├── formatters/ → single formatter with plugins
├── schemas/ → single zod schema library
├── utils/ → move to /src/utilities
└── validators/ → move to /src/validation
```

---

## File Structure After Refactoring

### Current (v4.x)
```
/src
├── knowledge-engine/          [TOO BIG]
│   ├── ai-semantic/
│   ├── dark-matter/
│   ├── federation/
│   ├── monitoring/
│   ├── streaming/
│   ├── And 20+ other dirs...
├── project-engine/            [UNUSED BY MOST]
├── domain/models/             [SPECIFIC TO EXAMPLES]
├── cli/                        [GOOD]
├── composables/               [FRAMEWORK-SPECIFIC]
├── profiling/                 [OPTIONAL]
└── ... (lots of other stuff)
```

### After Refactoring (v5.0)
```
/src
├── rdf/                       [CORE: All RDF operations]
│   ├── store.mjs
│   ├── quads.mjs
│   ├── canonicalize.mjs
│   └── namespaces.mjs
├── sparql/                    [CORE: SPARQL execution]
│   ├── comunica-wrapper.mjs
│   ├── parser.mjs
│   └── formatter.mjs
├── hooks/                     [CORE: Policy mechanism]
│   ├── define.mjs
│   ├── executor.mjs
│   └── registry.mjs
├── federation/                [CORE: Peer queries]
│   ├── coordinator.mjs
│   └── distributed-query.mjs
├── streaming/                 [CORE: Change feeds]
│   ├── change-feed.mjs
│   ├── subscription.mjs
│   └── processor.mjs
├── browser/                   [CORE: Browser SDK]
│   ├── store.mjs
│   └── shims.mjs
├── cli/                       [CORE: CLI tools]
│   ├── graph/
│   ├── context/
│   └── hook/
├── observability/             [CORE: OTEL points]
├── security/                  [CORE: Validation]
├── validation/                [CORE: Input checks]
├── utilities/                 [CORE: Helpers]
├── types.mjs                  [CORE: Types]
├── constants.mjs              [CORE: Constants]
└── index.mjs                  [CORE: Exports]

Total: ~50-60 core files (down from 150+)
```

---

## Migration Path (Non-Breaking)

### v5.0-rc.1: Soft Deprecation
```javascript
// Deprecated: Knowledge Engine
import { inferPatterns } from 'unrdf/knowledge-engine'
// ⚠️ Warning: Use @unrdf/knowledge-engine instead

// New substrate API
import { executeQuery, queryStore } from 'unrdf'
import { subscribeToChanges } from 'unrdf/streaming'
import { defineHook } from 'unrdf/hooks'
```

### v5.0-stable: Hard Move
```javascript
// OLD (no longer in unrdf):
import { inferPatterns } from 'unrdf/knowledge-engine'  // ❌ Error

// NEW (separate packages):
import { queryStore } from 'unrdf'
import { inferPatterns } from '@unrdf/knowledge-engine'
import { useGraph } from '@unrdf/composables'
import { optimizeQuery } from '@unrdf/dark-matter'
```

### Migration Guide
- Docs explaining what moved where
- Codemods to update imports
- Compatibility layer package (optional)

---

## Package Ecosystem (Post-v5.0)

```
Core Platform:
  unrdf v5.0+          // RDF substrate + Hooks + Federation + Streaming

Extensions (Official):
  @unrdf/knowledge-engine      // Rule engine + AI enrichment
  @unrdf/dark-matter           // Query optimization
  @unrdf/composables           // Vue 3 composables
  @unrdf/project-engine        // Self-hosting tools
  @unrdf/hooks-advanced        // Batching + sandboxing

Integrations (Community):
  @unrdf/federation-sync       // Custom replication
  @unrdf/stream-validators     // Custom validators
  @unrdf/hooks-policies        // Policy libraries
  @unrdf/ai-enrichment         // AI plugins
```

---

## Size Reduction

### Current (v4.x)
- npm package: ~2.5 MB
- Uncompressed: ~8 MB
- Essential to users: ~30%

### After v5.0
- npm package: ~0.8 MB (68% reduction)
- Uncompressed: ~2.5 MB (69% reduction)
- Essential to users: ~95%

### Trade-off
- Smaller core = faster installs, simpler
- Extensions opt-in = better for IoT, edge
- Clear boundaries = easier to understand

---

## Implementation Order

### Phase 1: v5.0-beta (Weeks 1-2)
1. Move Knowledge Engine to separate repo
2. Update imports in tests
3. Update documentation
4. Create migration guide

### Phase 2: v5.0-rc (Weeks 3-4)
1. Move Dark Matter, composables
2. Simplify remaining code
3. Update examples
4. Community feedback

### Phase 3: v5.0-stable (Week 5)
1. Final cleanup
2. Release all packages
3. Announce ecosystem
4. Support period begins

---

## Success Criteria

✅ **Core substrate is <50KB uncompressed**
✅ **All 7 VOCs have clear paths to their use cases**
✅ **Zero breaking changes for substrate users**
✅ **Extensions documented with examples**
✅ **Community can build on substrate**
✅ **Installation time <2 seconds**

---

## FAQ

**Q: Won't this fragment the ecosystem?**
A: Yes, but with intent. Monoliths fragment naturally as they grow. We're being intentional about it.

**Q: What about users who need everything?**
A: They install all packages. We provide a convenience `@unrdf/complete` that re-exports all.

**Q: Can agents/data engineers use this?**
A: YES. That's the whole point. They get raw substrate to build on.

**Q: What about backwards compatibility?**
A: v4.x stays in `npm unrdf@4`. v5.x is intentionally different.

**Q: Timeline?**
A: 4-5 weeks to stable with community feedback.
