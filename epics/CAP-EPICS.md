# Capability Atoms & API Surface EPICs

**Domain**: Capability Atoms & API Surface  
**Owner**: Capability Cartographer  
**Target**: V6.0.0 Release  
**Context**: Reduce from 400+ exports to 34, eliminate 40% dead code, establish 8 atoms + 5 compositions

---

## EPIC-CAP-001: Establish 8 Foundation Capability Atoms

**Goal**: Implement the 8 Tier-0 capability atoms with deterministic guarantees and <200 LoC per atom

**Value**: 
- Provides the minimal, non-redundant building blocks for all v6 functionality
- Eliminates 3 duplicate RDF store implementations
- Establishes performance baselines (receipt <1ms, SPARQL <10ms)

**Scope**: 
- `@unrdf/oxigraph` - RDF Store Creation + Term Factory
- `@unrdf/kgc-4d` - Freeze Universe
- `@unrdf/v6-core` - Receipt Generation + Delta Proposal
- `@unrdf/hooks` - Hook Execution
- `@unrdf/core` - SPARQL Query
- `@unrdf/yawl` - Workflow Transition

### Acceptance Criteria
- [ ] Each atom implemented in <200 LoC with pure functions
- [ ] 100% test coverage with deterministic tests (no mocks, no network)
- [ ] Performance targets met: Receipt creation <1ms (P95), SPARQL simple queries <10ms (P95)
- [ ] All atoms use Oxigraph (zero N3 direct imports in implementation)
- [ ] Zod schema validation for all atom inputs/outputs
- [ ] JSDoc documentation with runnable examples for each atom
- [ ] All atoms pass OTEL validation (score ≥80/100)
- [ ] Runtime compatibility matrix documented (Node/Browser/WASM)

### Key Stories
1. **A1: RDF Store Atom** - Expose `createStore()` and `dataFactory` from Oxigraph directly (no wrapper)
2. **A2: Receipt Atom** - Implement 4 receipt types (Execution, Allocation, Compile, Verification) with BLAKE3 chains
3. **A3: Delta Atom** - All-or-none mutation gate with conflict detection and reconciliation
4. **A4: Freeze Atom** - Nanosecond timestamp + Git snapshot integration with KGC-4D
5. **A5: Hook Atom** - Policy-gated execution with SPARQL condition evaluation
6. **A6: SPARQL Atom** - Sync-first query API with async wrapper
7. **A7: Workflow Atom** - Van der Aalst patterns via RDF event sourcing
8. **A8: Term Factory Atom** - Direct Oxigraph dataFactory for quads/triples

### Dependencies
- Blocked by: None (foundation layer)
- Blocks: EPIC-CAP-002 (compositions), EPIC-CAP-003 (API surface)

### Estimated Effort
- T-shirt size: **L**
- Weeks: **2-3**
- Risk: **MEDIUM** (Oxigraph WASM compatibility, Git integration performance)

---

## EPIC-CAP-002: Build 5 Pareto-Optimal Compositions

**Goal**: Create the 5 key compositions from atoms with runnable proofs demonstrating each pattern

**Value**: 
- Covers 80% of use cases with non-dominated compositions
- Provides reference implementations for users
- Validates atom composability and performance

**Scope**: 
- C1: Receipt-Gated Mutation (Delta + Receipt + Store)
- C2: Hook-Policy Gate (Hook + SPARQL + Store)
- C3: Freeze-Receipt Chain (Freeze + Receipt)
- C4: Workflow Event-Sourcing (Workflow + Freeze + Receipt)
- C5: Streaming Validation Pipeline (Streaming + Hook + Validation)

### Acceptance Criteria
- [ ] Each composition has runnable proof file (<100 LoC) in `/proofs/`
- [ ] All proofs execute successfully with expected output captured
- [ ] Integration tests for multi-atom interactions (e.g., Delta → Receipt → Store)
- [ ] Performance targets: C4 full workflow transition <50ms (P95)
- [ ] No composition dominates another (Pareto frontier validated)
- [ ] Documentation shows before/after code examples vs v5
- [ ] Break conditions identified and documented for each composition
- [ ] All compositions produce receipts (audit trail mandatory)

### Key Stories
1. **C1: Receipt-Gated Mutation** - Implement atomic store updates with cryptographic audit trail
2. **C2: Hook-Policy Gate** - Build SPARQL-based access control with deny/allow policies
3. **C3: Freeze-Receipt Chain** - Create Git-backed time-travel with receipt chaining
4. **C4: Workflow Event-Sourcing** - Integrate YAWL tasks with KGC-4D freeze + receipts
5. **C5: Streaming Validation** - Real-time quad validation via hook pipeline

### Dependencies
- Blocked by: EPIC-CAP-001 (atoms must exist)
- Blocks: EPIC-CAP-005 (missing capabilities depend on composition patterns)

### Estimated Effort
- T-shirt size: **M**
- Weeks: **1-2**
- Risk: **LOW** (atoms already proven in capability-basis analysis)

---

## EPIC-CAP-003: Minimal API Surface (34 Exports)

**Goal**: Reduce public API from 400+ exports to exactly 34 across 7 core packages

**Value**: 
- 91% reduction in API surface (cognitive load)
- Eliminates confusion from duplicate/overlapping APIs
- Enables aggressive tree-shaking (smaller bundles)

**Scope**: 
- Define final export list for each package (Oxigraph: 3, KGC-4D: 8, v6-core: 6, hooks: 5, YAWL: 7, streaming: 4, CLI: 1)
- Deprecate legacy exports with migration warnings
- Generate API reference documentation
- Create export usage tracking (prevent API bloat)

### Acceptance Criteria
- [ ] Exactly 34 exports documented across 7 packages (no more, no less)
- [ ] All 34 exports are actively used (0% dead code in minimal API)
- [ ] ESLint rule blocks new exports without approval
- [ ] Deprecation warnings in v5 packages pointing to v6 equivalents
- [ ] API reference generated from JSDoc (TypeDoc or similar)
- [ ] Export inventory tracked in `/docs/api-surface.md` with usage counts
- [ ] Breaking change guide documents every removed export with migration path
- [ ] Zero default exports (all named exports for tree-shaking)

### Key Stories
1. **API Inventory** - Audit current 400+ exports, identify usage counts, mark dead code
2. **Export Consolidation** - Merge duplicate APIs (e.g., 3 Merkle implementations → 1)
3. **Deprecation Plan** - Add warnings to v5, communicate sunset timeline
4. **API Documentation** - Generate reference docs for 34 exports with examples
5. **Export Enforcement** - ESLint rule + CI check to prevent API bloat

### Dependencies
- Blocked by: EPIC-CAP-001 (atoms define core exports), EPIC-CAP-004 (dead code removal)
- Blocks: EPIC-CAP-007 (documentation needs stable API)

### Estimated Effort
- T-shirt size: **M**
- Weeks: **1-2**
- Risk: **LOW** (mostly documentation + linting)

---

## EPIC-CAP-004: Dead Code Elimination (40% Reduction)

**Goal**: Remove ~1,074 lines of dead code (exported but never imported) and consolidate 45 packages

**Value**: 
- Reduces maintenance burden (fewer files, less code)
- Improves build times and bundle size
- Eliminates confusing/misleading APIs

**Scope**: 
- High-confidence dead exports (0 imports): `CircuitBreaker`, `retry()`, `HookScheduler`, `DarkMatterCore`, etc.
- Package consolidation: 9 YAWL packages → 1, 8 KGC packages → 1-2
- Vaporware removal: dark-matter, engine-gateway, composables (0 LoC)
- Move non-core to separate repos: kgc-claude, kgn, kgc-swarm

### Acceptance Criteria
- [ ] All exports with 0 imports removed (verified via grep analysis)
- [ ] Package count reduced from 57 to ≤12
- [ ] LoC reduced from 417K to ≤120K (71% reduction)
- [ ] No circular dependencies introduced during consolidation
- [ ] All tests still passing after removal (no hidden dependencies)
- [ ] Git history preserved (use `git mv` for package consolidation)
- [ ] Dead code analysis automated in CI (alert if dead exports appear)
- [ ] Deprecation log documents all removed capabilities

### Key Stories
1. **Dead Export Analysis** - Run grep/glob to find all 0-import exports, generate kill list
2. **YAWL Consolidation** - Merge 9 YAWL packages into plugin-based `@unrdf/yawl`
3. **KGC Consolidation** - Merge kgc-4d, receipts, blockchain, multiverse into `@unrdf/governance`
4. **Vaporware Removal** - Delete 0-LoC packages (dark-matter, engine-gateway, etc.)
5. **External Migration** - Move kgc-claude, kgn, kgc-swarm to `unrdf-extensions` repo

### Dependencies
- Blocked by: EPIC-CAP-003 (need final API surface to know what's safe to remove)
- Blocks: EPIC-CAP-006 (runtime matrix depends on final package set)

### Estimated Effort
- T-shirt size: **XL**
- Weeks: **3-4**
- Risk: **HIGH** (risk of breaking hidden dependencies, requires thorough testing)

---

## EPIC-CAP-005: Missing Capabilities Implementation

**Goal**: Implement 5 missing capabilities identified in capability-basis analysis (M1-M5)

**Value**: 
- Closes functional gaps in v6 (browser persistence, conflict resolution, etc.)
- Enables production use cases (IndexedDB receipts, parallel MI tasks)
- Prevents regression from v5 features

**Scope**: 
- M1: Browser receipt persistence (IndexedDB adapter)
- M2: Delta conflict resolver (basic strategies: current-wins, strict, merge)
- M3: Receipt Merkle anchor (stub for blockchain integration)
- M4: YAWL multi-instance task parallelization
- M5: HDIT vector search index (optional HNSW.js integration)

### Acceptance Criteria
- [ ] M1: Receipts persist in IndexedDB and survive browser refresh
- [ ] M2: Delta conflicts resolved via user-selected strategy (3 strategies implemented)
- [ ] M3: Merkle root anchor API defined (stub implementation, no blockchain dependency yet)
- [ ] M4: YAWL MI tasks execute in parallel via `Promise.allSettled()`
- [ ] M5: HDIT k-NN queries use HNSW index (O(log n) vs O(n²) brute-force)
- [ ] All missing capabilities have tests (≥80% coverage)
- [ ] Performance improvement measured for M4 (parallel vs serial) and M5 (indexed vs scan)
- [ ] Breaking conditions documented (e.g., if IndexedDB unavailable, fallback to in-memory)

### Key Stories
1. **M1: Browser Receipts** - Implement IndexedDB adapter for `@unrdf/v6-core/receipts`
2. **M2: Conflict Resolution** - Add interactive conflict resolver to DeltaGate
3. **M3: Blockchain Anchor** - Define API for Merkle root anchoring (future Ethereum/Solana integration)
4. **M4: Parallel MI Tasks** - Refactor YAWL `MultipleInstanceTask` to use Promise.allSettled
5. **M5: Vector Index** - Integrate HNSW.js for KGC-4D HDIT event similarity queries

### Dependencies
- Blocked by: EPIC-CAP-002 (compositions provide patterns), EPIC-CAP-001 (atoms must be stable)
- Blocks: None (enhancements, not blockers for v6.0.0)

### Estimated Effort
- T-shirt size: **L**
- Weeks: **2-3**
- Risk: **MEDIUM** (M5 HNSW integration complexity, M2 UI/UX decisions)

---

## EPIC-CAP-006: Runtime Target Matrix & WASM Strategy

**Goal**: Ensure all 8 atoms and 5 compositions work across Node.js, Browser, and WASM runtimes

**Value**: 
- Enables universal deployment (server + browser + edge)
- Validates WASM performance claims (10-100x speedup)
- Prevents runtime-specific bugs

**Scope**: 
- Node.js, Browser (Chrome/Firefox/Safari), WASM (Oxigraph, AtomVM)
- Runtime compatibility matrix for all 12 packages
- WASM boundary definition (what stays in JS, what compiles to WASM)
- Memory model for WASM (copy-on-call, streaming, SharedArrayBuffer)

### Acceptance Criteria
- [ ] Runtime matrix documented for all packages (Node ✅/⚠️/❌, Browser ✅/⚠️/❌, WASM ✅/⚠️/❌)
- [ ] Oxigraph WASM bundle ≤20MB (current: ~8MB)
- [ ] Oxigraph WASM init time <500ms (cold start)
- [ ] All browser tests pass in Chrome, Firefox, Safari (via Playwright)
- [ ] WASM modules use copy-on-call for safety (no SharedArrayBuffer unless justified)
- [ ] KGC-4D Git dependency isolated (Node-only, browser uses in-memory snapshot)
- [ ] WASM candidates prioritized: SPARQL compiler (HIGH), SHACL validator (HIGH), Merkle tree (MEDIUM)
- [ ] JS boundary enforced: orchestration, OTEL, file I/O, error handling stay in JS

### Key Stories
1. **Runtime Matrix** - Test all packages across Node 18/20/22 + browsers + WASM
2. **WASM Bundle Optimization** - Ensure Oxigraph WASM ≤20MB with tree-shaking
3. **Browser Testing** - Set up Playwright tests for browser compatibility
4. **WASM Boundary** - Define which code compiles to WASM vs stays in JS
5. **Memory Model** - Implement copy-on-call pattern for WASM safety

### Dependencies
- Blocked by: EPIC-CAP-001 (atoms must exist to test), EPIC-CAP-004 (final package set)
- Blocks: EPIC-CAP-007 (docs need runtime constraints)

### Estimated Effort
- T-shirt size: **M**
- Weeks: **1-2**
- Risk: **MEDIUM** (WASM compatibility issues, browser quirks)

---

## EPIC-CAP-007: API Documentation & Migration Guide

**Goal**: Generate complete API reference for 34 exports and v5→v6 migration guide

**Value**: 
- Reduces onboarding time (1 hour to understand architecture)
- Prevents migration confusion (every removed export has documented alternative)
- Enables self-service (users find answers in docs, not issues)

**Scope**: 
- API reference for all 34 exports (generated from JSDoc)
- Migration guide for breaking changes (v5 API → v6 equivalent)
- Runtime constraints (Node-only features, browser limitations)
- Runnable examples for each atom and composition

### Acceptance Criteria
- [ ] API reference generated via TypeDoc/JSDoc for all 34 exports
- [ ] Each export has: description, parameters, return type, example, runtime compatibility
- [ ] Migration guide documents all 400+ → 34 export changes with code examples
- [ ] Runnable examples in `/examples/v6/` for each atom (8 examples) and composition (5 examples)
- [ ] Migration guide tested: all v5 code samples have working v6 equivalents
- [ ] Breaking changes log in `/BREAKING-CHANGES.md` with GitHub issue links
- [ ] Tutorial: "Getting Started with v6" (15 min) passes user testing
- [ ] Diataxis compliance: API reference in `/docs/reference/`, migration in `/docs/how-to/`

### Key Stories
1. **API Reference Generation** - Set up TypeDoc/JSDoc build for `/docs/reference/api/`
2. **Migration Guide** - Document v5→v6 for top 20 most-used v5 exports
3. **Runnable Examples** - Create 13 examples (8 atoms + 5 compositions) in `/examples/v6/`
4. **Breaking Changes Log** - Document all removed exports with alternatives
5. **Tutorial Update** - Rewrite "Getting Started" for v6 minimal API

### Dependencies
- Blocked by: EPIC-CAP-003 (API must be stable), EPIC-CAP-006 (runtime constraints)
- Blocks: None (documentation is last step)

### Estimated Effort
- T-shirt size: **M**
- Weeks: **1-2**
- Risk: **LOW** (mostly writing + automation setup)

---

## Summary

| Epic | Focus | Size | Weeks | Blocks |
|------|-------|------|-------|--------|
| CAP-001 | 8 Atoms | L | 2-3 | CAP-002, CAP-003 |
| CAP-002 | 5 Compositions | M | 1-2 | CAP-005 |
| CAP-003 | 34 Exports | M | 1-2 | CAP-007 |
| CAP-004 | Dead Code | XL | 3-4 | CAP-006 |
| CAP-005 | Missing Caps | L | 2-3 | - |
| CAP-006 | Runtime Matrix | M | 1-2 | CAP-007 |
| CAP-007 | Documentation | M | 1-2 | - |

**Total Estimated Effort**: 11-16 weeks (parallelizable with other v6 tracks)

**Critical Path**: CAP-001 → CAP-002 → CAP-003 → CAP-007

**Parallel Tracks**:
- Track A: CAP-001 → CAP-002 (atoms + compositions)
- Track B: CAP-004 → CAP-006 (consolidation + runtime)
- Track C: CAP-005 (missing capabilities, can start after CAP-001 atoms exist)
- Track D: CAP-007 (documentation, waits for stable API)

---

**Next Actions**:
1. Review EPICs with system architect (validate dependencies)
2. Create GitHub issues for each story
3. Set up project board with story assignments
4. Begin CAP-001 (foundation atoms) immediately
