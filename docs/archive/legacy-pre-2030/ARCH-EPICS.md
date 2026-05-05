# UNRDF v6 Architecture EPICs
# Package Architecture & Consolidation Domain

**Context**: Consolidating 57 packages → 12 packages with 3-layer architecture
**Vision**: [V6-COMPLETE-REWRITE-VISION.md](../V6-COMPLETE-REWRITE-VISION.md)
**Proposal**: [V6-ARCHITECTURE-PROPOSAL.md](../V6-ARCHITECTURE-PROPOSAL.md)
**Created**: 2025-12-28

---

## EPIC-ARCH-001: Foundation Layer Consolidation

**Goal**: Create the 3 foundation packages (@unrdf/store, @unrdf/rdf, @unrdf/governance) with zero interdependencies

**Value**: Establishes the clean dependency foundation for v6, eliminates 11 source packages, removes duplicate cryptographic receipt implementations

**Scope**:
- Source packages: `core`, `oxigraph`, `kgc-4d`, `receipts`, `blockchain`, `kgc-multiverse`, `kgc-substrate`, `v6-core`, `validation` (partial)
- Target packages: `@unrdf/store`, `@unrdf/rdf`, `@unrdf/governance`
- ~30K LOC → 26K LOC (13% reduction through deduplication)

### Acceptance Criteria
- [ ] `@unrdf/store` (8K LOC) created with Oxigraph bindings + SPARQL execution, zero internal dependencies
- [ ] `@unrdf/rdf` (6K LOC) created with data model + parsers + SHACL validation, zero internal dependencies
- [ ] `@unrdf/governance` (12K LOC) created consolidating all provenance/receipt logic
- [ ] All 3 packages have NO dependencies on each other (parallel, not hierarchical)
- [ ] Duplicate Merkle tree implementations eliminated (currently in `kgc-4d/freeze.mjs`, `receipts/merkle-batcher.mjs`, `blockchain/merkle/*`)
- [ ] Receipt verification unified to single API (`createReceipt`, `verifyReceipt`, `freezeUniverse`)
- [ ] 100% of existing tests migrated and passing (currently ~150 test files for these packages)
- [ ] Performance benchmarks match or exceed v5 baselines (Oxigraph query <10ms P95, receipt creation <1ms)
- [ ] Zero N3 direct imports in new packages (must use Oxigraph via @unrdf/store)
- [ ] ESLint rule enforcing layer 1 cannot depend on each other

### Key Stories
1. **Extract @unrdf/store from oxigraph + core** - Pure SPARQL execution, no parsing/validation (migrate 1,746 LOC from oxigraph + ~4K from core)
2. **Extract @unrdf/rdf from core** - RDF data model, parsers (Turtle/JSON-LD/N-Triples), SHACL validation (~6K from core)
3. **Consolidate 6 provenance packages into @unrdf/governance** - Merge kgc-4d (6,693), receipts (730), blockchain (945), kgc-multiverse (4,179), kgc-substrate (2,334), v6-core (parts)
4. **Eliminate duplicate Merkle implementations** - Single BLAKE3-based Merkle tree in governance, remove duplicates
5. **Unify receipt schemas** - Single Zod schema for receipts (9 fields: id, receiptType, t_ns, timestamp_iso, previousHash, payloadHash, receiptHash, payload, signature?)
6. **Create foundation test suite** - 80/20 fast suite (<10s execution) with 100% pass rate
7. **Establish performance baselines** - Document P95 targets: store creation <2ms, triple insert <1ms, SPARQL simple <10ms, receipt creation <1ms
8. **Ban N3 direct imports** - ESLint rule `no-restricted-imports` preventing `from 'n3'` in new packages

### Dependencies
- **Blocked by**: None (foundation layer starts first)
- **Blocks**: EPIC-ARCH-002 (Runtime), EPIC-ARCH-003 (Applications), EPIC-ARCH-004 (Circular dependencies)

### Estimated Effort
- **T-shirt size**: XL
- **Weeks**: 3-4 weeks
- **Rationale**: Largest consolidation, involves splitting monolithic `core` package (23,616 LOC) and merging 6 provenance packages with careful deduplication

---

## EPIC-ARCH-002: Runtime Layer Consolidation

**Goal**: Create the 4 runtime packages (@unrdf/workflows, @unrdf/runtime, @unrdf/hooks, @unrdf/observability) that depend only on Layer 1

**Value**: Consolidates 14 source packages into 4, eliminates streaming/federation/consensus circular dependencies, unifies YAWL workflow execution

**Scope**:
- Source packages: `yawl`, `yawl-durable`, `streaming`, `federation`, `consensus`, `collab`, `hooks`, `knowledge-engine`, `observability`, `yawl-observability`
- Target packages: `@unrdf/workflows`, `@unrdf/runtime`, `@unrdf/hooks`, `@unrdf/observability`
- ~69K LOC → 62K LOC (10% reduction)

### Acceptance Criteria
- [ ] `@unrdf/workflows` (35K LOC) created from yawl + yawl-durable with all 23 Van der Aalst patterns
- [ ] `@unrdf/runtime` (10K LOC) created consolidating streaming, federation, consensus, collab (eliminates circular dependencies)
- [ ] `@unrdf/hooks` (12K LOC) created merging hooks + knowledge-engine (unified rule-based reasoning)
- [ ] `@unrdf/observability` (5K LOC) consolidates observability + yawl-observability with single OTEL layer
- [ ] All runtime packages depend ONLY on Layer 1 (store, rdf, governance) - enforced via ESLint
- [ ] Circular dependency between streaming ↔ federation ↔ consensus eliminated (all in @unrdf/runtime)
- [ ] YAWL durable execution (Saga pattern) integrated into workflows core
- [ ] All workflow tests passing (currently 61 test files in yawl packages)
- [ ] RAFT consensus integration tests passing (leader election, log replication)
- [ ] Hook execution P95 latency <100ms maintained
- [ ] OpenTelemetry spans standardized across all runtime packages (receipt.create, delta.apply, sparql.query, hook.execute)

### Key Stories
1. **Refactor @unrdf/workflows from yawl + yawl-durable** - Merge Saga pattern into core (39,123 + 1,712 LOC → 35K)
2. **Create @unrdf/runtime consolidating 4 packages** - streaming (3,298) + federation (4,070) + consensus (2,143) + collab (2,375) → 10K
3. **Merge hooks + knowledge-engine** - Unified policy/rule API (10,567 + 5,419 → 12K)
4. **Eliminate streaming ↔ federation circular dependency** - Both in @unrdf/runtime with clear internal boundaries
5. **Consolidate OTEL instrumentation** - Single tracer/metrics layer (2,184 + 1,896 → 5K)
6. **Establish runtime test suite** - Workflow execution, federation queries, RAFT consensus, hook chains
7. **Document workflow patterns** - All 23 Van der Aalst patterns with examples
8. **Create OTEL validation gates** - ≥80/100 score required for all runtime operations

### Dependencies
- **Blocked by**: EPIC-ARCH-001 (Foundation layer must exist first)
- **Blocks**: EPIC-ARCH-003 (Applications depend on runtime)

### Estimated Effort
- **T-shirt size**: XL
- **Weeks**: 3-4 weeks
- **Rationale**: Complex consolidation involving workflow engine (largest codebase component at 39K LOC) and circular dependency resolution

---

## EPIC-ARCH-003: Applications Layer Consolidation

**Goal**: Create the 5 application packages (@unrdf/cli, @unrdf/integrations, @unrdf/ai, @unrdf/ui, @unrdf/tools) that depend on L1 + L2

**Value**: Consolidates 22 source packages into 5, eliminates 9 thin YAWL adapter packages via plugin model, unifies developer tooling

**Scope**:
- Source packages: `cli`, `kgc-cli`, `kgc-tools`, `yawl-api`, `yawl-kafka`, `yawl-queue`, `yawl-langchain`, `rdf-graphql`, `serverless`, `ml-inference`, `ml-versioning`, `semantic-search`, `yawl-ai`, `decision-fabric`, `react`, `composables`, `yawl-viz`, `yawl-realtime`, `test-utils`, `diataxis-kit`, `kgc-docs`, `kgc-probe`
- Target packages: `@unrdf/cli`, `@unrdf/integrations`, `@unrdf/ai`, `@unrdf/ui`, `@unrdf/tools`
- ~79K LOC → 31K LOC (61% reduction through plugin model)

### Acceptance Criteria
- [ ] `@unrdf/cli` (8K LOC) created consolidating cli + kgc-cli + kgc-tools (4,814 + 17,697 + 354 → 8K)
- [ ] `@unrdf/integrations` (6K LOC) created with plugin model for Kafka/REST/GraphQL/Lambda/Queue/LangChain
- [ ] `@unrdf/ai` (5K LOC) created consolidating 5 ML packages with shared vector infrastructure
- [ ] `@unrdf/ui` (4K LOC) created with React/Vue subpath exports (`@unrdf/ui/react`, `@unrdf/ui/vue`)
- [ ] `@unrdf/tools` (8K LOC) created consolidating test-utils + diataxis-kit + kgc-docs + kgc-probe
- [ ] All application packages depend on L1 + L2, NOT on each other
- [ ] CLI plugin system working (deterministic extension registry from kgc-cli)
- [ ] Integration adapters load dynamically (no hard dependencies on Kafka/etc unless used)
- [ ] All 53 existing examples migrated and executing successfully
- [ ] React hooks (`useQuery`, `useWorkflow`, `useMutation`) tested with example app
- [ ] Test utilities support all 12 new packages (mock stores, fixture loaders)
- [ ] Documentation generation (Diataxis kit) works with new package structure

### Key Stories
1. **Consolidate 3 CLI packages into @unrdf/cli** - Unified command-line with plugin registry (22,865 → 8K)
2. **Create @unrdf/integrations with plugin model** - 6 thin adapters (7,113 LOC) → plugin architecture (6K LOC)
3. **Merge 5 ML packages into @unrdf/ai** - Shared embeddings/vector infrastructure (6,903 → 5K)
4. **Create @unrdf/ui with framework subpaths** - React/Vue/viz in single package (2,314 → 4K)
5. **Consolidate dev tooling into @unrdf/tools** - Testing + docs + benchmarks + kgc-probe (22,411 → 8K)
6. **Migrate all 53 examples to new package structure** - Update imports, verify execution
7. **Create application test suite** - E2E tests covering CLI, integrations, UI components
8. **Document plugin extension points** - How to add CLI commands, integration adapters, UI components

### Dependencies
- **Blocked by**: EPIC-ARCH-001 (Foundation), EPIC-ARCH-002 (Runtime)
- **Blocks**: EPIC-ARCH-006 (Migration infrastructure needs finalized package structure)

### Estimated Effort
- **T-shirt size**: L
- **Weeks**: 2-3 weeks
- **Rationale**: Many small packages to consolidate, but less complex than Foundation/Runtime layers. Plugin model simplifies adapter consolidation.

---

## EPIC-ARCH-004: Circular Dependency Elimination

**Goal**: Establish and enforce strict 3-layer dependency rules with zero circular dependencies

**Value**: Enables parallel development, faster builds, cleaner mental model, prevents architecture decay

**Scope**:
- All 12 packages in v6
- ESLint rules for dependency enforcement
- CI/CD gates blocking circular deps
- Dependency graph visualization

### Acceptance Criteria
- [ ] Layer 1 (Foundation) packages have ZERO internal dependencies between them (store ⊥ rdf ⊥ governance)
- [ ] Layer 2 (Runtime) packages depend ONLY on Layer 1 (no L2 ↔ L2 dependencies except observability)
- [ ] Layer 3 (Applications) packages depend on L1 + L2 but NOT on each other
- [ ] `@unrdf/observability` is special-cased (can be imported by any layer as cross-cutting concern)
- [ ] ESLint rule `import/no-restricted-paths` enforces layer boundaries
- [ ] CI/CD fails if circular dependency detected (using madge or similar)
- [ ] Dependency graph diagram auto-generated and committed to docs (mermaid format)
- [ ] Build time reduced to <20s (down from ~60s) due to parallel compilation
- [ ] Dependency depth ≤3 layers (down from 4-5 in v5)
- [ ] Zero violations in `pnpm -r exec madge --circular src`

### Key Stories
1. **Define ESLint dependency rules** - Configure `import/no-restricted-paths` for 3 layers
2. **Break streaming ↔ federation circular dependency** - Both move to @unrdf/runtime with internal boundaries
3. **Remove hooks → workflows back-reference** - Workflows can use hooks, but not vice versa
4. **Special-case observability imports** - Allow OTEL in any layer without violating boundaries
5. **Create dependency graph generator** - Script to visualize package dependencies (mermaid output)
6. **Add CI gate for circular dependencies** - GitHub Action fails on `madge --circular` violations
7. **Document dependency rules in CLAUDE.md** - Clear guidance on layer boundaries
8. **Validate zero violations** - Run madge across all packages and verify clean output

### Dependencies
- **Blocked by**: EPIC-ARCH-001, EPIC-ARCH-002, EPIC-ARCH-003 (need packages to exist first)
- **Blocks**: None (can be validated after consolidation)

### Estimated Effort
- **T-shirt size**: M
- **Weeks**: 1-2 weeks
- **Rationale**: Primarily tooling and enforcement, most circular deps resolved during consolidation

---

## EPIC-ARCH-005: Package Kill List Execution

**Goal**: Remove or archive 45 packages being consolidated or eliminated from the monorepo

**Value**: Reduces cognitive load, eliminates vaporware and dead code, prevents accidental imports from old packages

**Scope**:
- 28 packages merged into new v6 packages
- 17 packages killed entirely (vaporware, separate repos, temporary)
- Archive strategy for git history preservation
- Import blocking via ESLint

### Acceptance Criteria
- [ ] 28 merged packages moved to `archive/v5-packages/` directory with git history preserved
- [ ] 17 killed packages documented with removal rationale in ADR
- [ ] `kgc-claude` (23,621 LOC) moved to separate `unrdf-claude` repository
- [ ] `kgn` (18,581 LOC) moved to separate `kgn` repository
- [ ] `kgc-swarm` (8,677 LOC) moved to separate repository (part of unrdf-claude)
- [ ] All 0-LOC vaporware packages removed (dark-matter, engine-gateway, composables, domain, yawl-viz, nextra, docs)
- [ ] Premature packages removed (atomvm, caching, graph-analytics, fusion)
- [ ] Temporary packages removed (v6-compat, project-engine, integration-tests moved to /test)
- [ ] ESLint rule blocks imports from archived packages (`no-restricted-imports` for old package names)
- [ ] Package count verified: `ls -1 packages | wc -l` returns 12 (down from 57)
- [ ] Git submodules created for moved repositories (kgc-claude, kgn, kgc-swarm)
- [ ] CHANGELOG.md documents all removed packages with migration paths

### Key Stories
1. **Archive 28 merged packages** - Move to `archive/v5-packages/` with git history (kgc-4d, receipts, blockchain, streaming, federation, etc.)
2. **Extract kgc-claude to separate repo** - 23,621 LOC moved to `unrdf-claude` repository
3. **Extract kgn to separate repo** - 18,581 LOC moved to `kgn` template system repository
4. **Extract kgc-swarm to separate repo** - Part of unrdf-claude multi-agent system
5. **Remove 17 vaporware/premature packages** - Delete dark-matter, atomvm, caching, fusion, etc. with ADR rationale
6. **Create ESLint blocking rules** - Prevent imports from archived/removed packages
7. **Document kill list in ADR** - Architecture decision record for each removal
8. **Verify package count reduction** - Bash check: 57 → 12 packages

### Dependencies
- **Blocked by**: EPIC-ARCH-001, EPIC-ARCH-002, EPIC-ARCH-003 (need to complete consolidation before archiving sources)
- **Blocks**: None (cleanup task)

### Estimated Effort
- **T-shirt size**: M
- **Weeks**: 1-2 weeks
- **Rationale**: Primarily git operations and repository setup, careful archiving to preserve history

---

## EPIC-ARCH-006: Migration Infrastructure & Validation

**Goal**: Create migration tools, CI/CD updates, and validation gates to ensure v6 matches or exceeds v5 quality

**Value**: Enables safe migration, prevents performance regression, automates quality gates, provides confidence in rewrite

**Scope**:
- Automated migration tool for user codebases
- Performance benchmark parity validation
- CI/CD pipeline updates for 3-layer architecture
- Quality gates (10 production gates from vision doc)

### Acceptance Criteria
- [ ] Automated migration tool created (`npx @unrdf/migrate v5-to-v6`) that rewrites imports
- [ ] Performance benchmarks show parity or improvement vs v5 (within ±5% tolerance)
- [ ] All 10 production gates passing (tests 100%, OTEL ≥80/100, lint 0, coverage ≥80%, perf P95 <50ms, examples 100%, build <60s, no mocks, security 0 HIGH/CRITICAL, docs 95%)
- [ ] CI/CD matrix updated for 12 packages (parallel builds leveraging layer independence)
- [ ] Benchmark baseline file updated (`benchmarks/baselines/baseline.json`)
- [ ] Regression detection in CI (fails if P95 latency >+20% or memory >+30%)
- [ ] Build time reduced to <20s (67% faster than v5's ~60s)
- [ ] Test execution <15s for fast suite (67% faster than v5's ~45s)
- [ ] Zero circular dependencies in CI check (`madge --circular`)
- [ ] Migration guide published in docs (Diataxis how-to guide)

### Key Stories
1. **Create migration tool** - Codemod to rewrite imports from v5 → v6 package names
2. **Establish performance baselines** - Document v6 targets in `benchmarks/baselines/baseline.json`
3. **Update CI/CD for 3-layer architecture** - Parallel builds for Layer 1, then Layer 2, then Layer 3
4. **Implement 10 production gates** - Enforce all gates from vision doc (tests, OTEL, lint, coverage, perf, examples, build, mocks, security, docs)
5. **Create regression detection** - CI fails on >20% latency increase or >30% memory increase
6. **Validate benchmark parity** - Run all benchmarks and verify ≥95% parity with v5
7. **Write migration guide** - Diataxis how-to: "Migrating from v5 to v6" with examples
8. **Create rollback plan** - Document rollback criteria and process if issues found

### Dependencies
- **Blocked by**: EPIC-ARCH-001, EPIC-ARCH-002, EPIC-ARCH-003 (need finalized package structure)
- **Blocks**: Public v6 release

### Estimated Effort
- **T-shirt size**: L
- **Weeks**: 2-3 weeks
- **Rationale**: Complex automation for migration tool, comprehensive validation across all packages

---

## EPIC-ARCH-007: Architecture Documentation & ADRs

**Goal**: Document all architecture decisions, create migration guides, and update package structure documentation

**Value**: Enables team understanding, justifies decisions, provides reference for future changes, reduces onboarding time

**Scope**:
- Architecture Decision Records (ADRs) for all major decisions
- Updated package documentation (README for each package)
- Migration guide (v5 → v6)
- Diataxis documentation updates

### Acceptance Criteria
- [ ] 10 ADRs created documenting major decisions (12 packages, layer boundaries, kill list, etc.)
- [ ] Each of 12 packages has comprehensive README.md (purpose, responsibilities, dependencies, exports, examples)
- [ ] Migration guide published (how-to guide in docs/diataxis/how-to/migrate-v5-to-v6.md)
- [ ] Architecture explanation updated (docs/diataxis/explanation/v6-architecture.md)
- [ ] Dependency graph diagram in docs (mermaid format, auto-generated)
- [ ] CLAUDE.md updated with v6 package structure and rules
- [ ] All 34 public exports documented with JSDoc and examples
- [ ] Onboarding time measured: new developer can understand architecture in <1 hour
- [ ] Package comparison table published (v5 package → v6 package mapping)
- [ ] CHANGELOG.md documents v6.0.0 release with breaking changes

### Key Stories
1. **Write 10 Architecture Decision Records** - Document why 12 packages, layer boundaries, consolidations, kill list, etc.
2. **Create README for each package** - Purpose, responsibilities, dependencies, key exports, examples
3. **Write migration guide** - Step-by-step v5 → v6 with automated tool + manual changes
4. **Update architecture explanation** - Deep-dive on 3-layer model, dependency rules, package boundaries
5. **Generate dependency graph** - Automated mermaid diagram showing all 12 packages and dependencies
6. **Update CLAUDE.md** - Add v6 package structure, layer rules, import restrictions
7. **Document all 34 public exports** - JSDoc with @example tags for each export
8. **Create v6 launch blog post** - Explain rationale, benefits, migration path
9. **Update package comparison table** - v5 package → v6 package mapping in appendix
10. **Measure onboarding time** - Validate new developer can understand architecture in 1 hour

### Dependencies
- **Blocked by**: EPIC-ARCH-001, EPIC-ARCH-002, EPIC-ARCH-003, EPIC-ARCH-004 (need finalized architecture to document)
- **Blocks**: None (documentation task)

### Estimated Effort
- **T-shirt size**: M
- **Weeks**: 1-2 weeks
- **Rationale**: Comprehensive documentation across all packages, but straightforward writing task

---

## Summary

| EPIC | T-shirt | Weeks | Key Metric |
|------|---------|-------|------------|
| ARCH-001: Foundation Layer | XL | 3-4 | 11 packages → 3, zero interdependencies |
| ARCH-002: Runtime Layer | XL | 3-4 | 14 packages → 4, circular deps eliminated |
| ARCH-003: Applications Layer | L | 2-3 | 22 packages → 5, 61% LOC reduction |
| ARCH-004: Circular Dependency Elimination | M | 1-2 | Zero circular deps, build <20s |
| ARCH-005: Package Kill List | M | 1-2 | 57 → 12 packages, vaporware removed |
| ARCH-006: Migration Infrastructure | L | 2-3 | 10/10 production gates, benchmark parity |
| ARCH-007: Documentation & ADRs | M | 1-2 | 10 ADRs, onboarding <1 hour |

**Total Effort**: 13-18 weeks (3-4.5 months) with parallel tracks

**Critical Path**: ARCH-001 → ARCH-002 → ARCH-003 → ARCH-006 → Release

**Parallel Tracks**:
- Track A (Foundation): ARCH-001 (weeks 1-4)
- Track B (Runtime): ARCH-002 (weeks 5-8, blocked by A)
- Track C (Applications): ARCH-003 (weeks 9-11, blocked by B)
- Track D (Validation): ARCH-004 + ARCH-005 + ARCH-006 (weeks 12-15, blocked by C)
- Track E (Documentation): ARCH-007 (weeks 13-14, parallel with D)

**Success Criteria**:
- ✅ 57 packages → 12 packages (79% reduction)
- ✅ 417K LOC → 120K LOC (71% reduction through deduplication)
- ✅ Zero circular dependencies
- ✅ Build time <20s (67% faster)
- ✅ Test suite <15s (67% faster)
- ✅ All 10 production gates passing
- ✅ Onboarding time <1 hour (vs 2-3 days in v5)
