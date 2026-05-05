# UNRDF v6 Documentation & Developer Experience EPICs

**Domain**: Documentation & Developer Experience
**Target**: 429 docs → 40 docs (90% reduction)
**Framework**: Diataxis (Tutorials, How-To, Reference, Explanation)
**Quality Gate**: 100% evidence-based, 0% aspiration
**Date**: 2025-12-28

---

## EPIC-DOCS-001: Cruft Deletion & Archive Migration

**Goal**: Eliminate 255 cruft files (59% of current docs) and consolidate duplicate directory structures into canonical Diataxis layout.

**Value**: 
- Reduce noise, eliminate confusion for new developers
- Improve search/navigation (find relevant docs in <30s)
- Remove aspirational/outdated content (v3, v4, internal planning)
- Free up 3.6M of obsolete documentation

**Scope**: 
- `/docs/internal/` (1.8M internal planning)
- `/docs/archive/` (1.8M old v3/v4 docs)
- `/docs/thesis-publication/` (academic papers)
- `/docs/video-scripts/` (non-technical)
- `/docs/architecture-2028/` (aspirational)
- Duplicate structures: `/docs/how-to/`, `/docs/diataxis/how-to/`, `/docs/v6/diataxis/how-to/`

### Acceptance Criteria
- [ ] 255 files deleted or moved to `/docs/archive/` (git history preserved)
- [ ] Zero files in `/docs/internal/`, `/docs/thesis-publication/`, `/docs/video-scripts/` on main branch
- [ ] Single canonical path for each Diataxis quadrant (no duplicates)
- [ ] All cross-references updated (no broken links)
- [ ] Git history preserved (archive commit separate from delete commit)
- [ ] Verification: `find docs/ -name "*.md" | wc -l` returns ≤60 (allowing for buffer)

### Key Stories
1. **Archive Internal Planning** - Move `/docs/internal/` to git history (1.8M)
2. **Archive Legacy Docs** - Move v3/v4 docs to `/docs/archive/v3-v4/`
3. **Consolidate Diataxis Duplicates** - Merge 3 parallel structures → 1 canonical
4. **Move Academic Papers** - Extract thesis/papers to separate repo (optional) or archive
5. **Update Cross-References** - Fix all internal links after consolidation
6. **Validate No Broken Links** - Run link checker on remaining docs

### Dependencies
- Blocked by: None (can start immediately)
- Blocks: EPIC-DOCS-002, EPIC-DOCS-003, EPIC-DOCS-004 (need clean structure)

### Estimated Effort
- T-shirt size: **M**
- Weeks: **1-2**
- Risk: Low (mostly file operations, git history preserved)

---

## EPIC-DOCS-002: Tutorial Creation & Validation

**Goal**: Create 5 hands-on tutorials (100% runnable) that guide users from beginner to production deployment in ~2 hours.

**Value**:
- Fastest onboarding (new dev productive in <15 min for T1)
- Proof-based learning (every step verifiable)
- Clear learning path (T1 → T2 → T3 → T4 → T5)
- Reduce support burden (80% of questions answered by tutorials)

**Scope**: 5 tutorials (15-40 min each)
- **T1**: Getting Started with UNRDF v6 (15 min) - PROVEN
- **T2**: Build Your First Knowledge Hook (20 min) - PROVEN
- **T3**: Implement Dark Matter 80/20 (25 min) - PROVEN
- **T4**: Create Time-Travel Knowledge Graph (30 min) - NEEDS PROOF
- **T5**: Deploy Production-Ready (40 min) - NEEDS PROOF

### Acceptance Criteria
- [ ] All 5 tutorials have working example files (timeout 5s execution)
- [ ] T1 completable in <15 min on fresh install (verified with new user)
- [ ] T4 KGC-4D example verified (freeze → receipt → time-travel query)
- [ ] T5 deployment guide includes Docker + OTEL + L5 maturity test
- [ ] Every step includes success verification ("you should see X")
- [ ] Each tutorial links to next step (learning path)
- [ ] All code snippets copy-pasteable (no placeholders)
- [ ] Prerequisite check at start (Node.js version, pnpm installed)

### Key Stories
1. **Validate T1-T3 Examples** - Run existing examples, verify <5 min execution
2. **Proof T4 KGC-4D** - Create working time-travel example with receipts
3. **Proof T5 Deployment** - Create Docker + K8s + OTEL production example
4. **Write Tutorial Docs** - Convert examples to step-by-step tutorials
5. **User Testing** - Have fresh user attempt T1 (record time, pain points)
6. **Create Tutorial Index** - Learning path guide with time estimates

### Dependencies
- Blocked by: EPIC-DOCS-001 (need clean structure)
- Blocks: EPIC-DOCS-007 (navigation needs complete tutorials)

### Estimated Effort
- T-shirt size: **L**
- Weeks: **2-3**
- Risk: Medium (T4/T5 need proof artifacts, may require KGC-4D fixes)

---

## EPIC-DOCS-003: How-To Guide Development

**Goal**: Create 15 task-focused how-to guides covering core operations, integrations, and production patterns (90% runnable).

**Value**:
- Solve specific developer problems (not learning-oriented)
- Copy-paste solutions for common tasks
- Production patterns (deployment, monitoring, migration)
- Reduce "how do I...?" support requests by 70%

**Scope**: 15 how-to guides in 3 categories
- **Category A: Core Operations** (5 guides) - Query, validate, receipts, deltas, performance
- **Category B: Integration** (5 guides) - YAWL, streaming, federation, v5→v6 migration, external graphs
- **Category C: Production** (5 guides) - Docker/K8s, OTEL, L5 maturity, adversarial testing, audit trails

### Acceptance Criteria
- [ ] 15 how-to guides created with task-focused titles
- [ ] Each guide solves ONE specific problem (not multiple)
- [ ] 13/15 guides have runnable proof artifacts (examples or tests)
- [ ] H9 (Migrate v5→v6) tested with real v5 codebase
- [ ] H13 (L5 Maturity) references existing test suite
- [ ] Every guide includes "Problem", "Solution", "Example", "Verification"
- [ ] Time estimates accurate (±20%)
- [ ] All examples timeout <30s (no long-running processes)

### Key Stories
1. **Core Operations Guides** - Query, validate, receipts, deltas, performance (H1-H5)
2. **Integration Guides** - YAWL, streaming, federation (H6-H8)
3. **Migration Guide** - v5→v6 with v6-compat examples (H9)
4. **External Integration** - N3/RDFLib/Jena import (H10)
5. **Production Guides** - Docker, OTEL, L5, adversarial testing (H11-H14)
6. **Audit Trail Guide** - Receipt chain forensics (H15)
7. **Proof Validation** - Run all examples, verify claimed times

### Dependencies
- Blocked by: EPIC-DOCS-001 (clean structure)
- Blocks: None (parallel to other epics)

### Estimated Effort
- T-shirt size: **XL**
- Weeks: **3-4**
- Risk: Medium (some guides need proof creation, external tool integration)

---

## EPIC-DOCS-004: API Reference Generation

**Goal**: Generate 12 comprehensive API reference docs with 100% code links and accurate signatures.

**Value**:
- Single source of truth for API surfaces
- Direct code links (file:line) for every function
- Zod schema documentation (953 schemas)
- Eliminate "where is this exported?" questions

**Scope**: 12 reference documents
- **R1**: CLI Command Matrix (250 commands)
- **R2**: Core API (@unrdf/core)
- **R3**: Oxigraph API (@unrdf/oxigraph)
- **R4**: Hooks API (@unrdf/hooks)
- **R5**: KGC-4D API (@unrdf/kgc-4d)
- **R6**: YAWL API (@unrdf/yawl)
- **R7**: Federation API (@unrdf/federation)
- **R8**: Streaming API (@unrdf/streaming)
- **R9**: Receipt Schema Reference (v6-core)
- **R10**: Zod Schema Catalog (953 schemas)
- **R11**: RDF Format Notes (parsers/serializers)
- **R12**: Package Exports Map (57 packages)

### Acceptance Criteria
- [ ] All 12 reference docs generated (automated where possible)
- [ ] Every function has JSDoc signature + description
- [ ] Every reference links to source code (absolute paths)
- [ ] R1 CLI matrix verified (which commands actually work vs planned)
- [ ] R9 Receipt schema matches Zod implementation
- [ ] R10 Zod catalog auto-generated from code
- [ ] R12 Package exports auto-generated from package.json files
- [ ] Navigation index with quick search (by package or function)

### Key Stories
1. **CLI Command Matrix** - Verify 250 commands (implemented vs planned)
2. **Core Package References** - Generate R2-R8 from JSDoc
3. **Schema Documentation** - Extract Zod schemas (R9, R10)
4. **Package Metadata** - Generate exports map (R12)
5. **Code Link Verification** - Validate all file:line references
6. **API Index Generation** - Searchable function index

### Dependencies
- Blocked by: EPIC-DOCS-001 (clean structure)
- Blocks: EPIC-DOCS-007 (navigation needs reference index)

### Estimated Effort
- T-shirt size: **M**
- Weeks: **2**
- Risk: Low (mostly automated generation from code)

---

## EPIC-DOCS-005: Explanation Articles & Deep Dives

**Goal**: Write 8 evidence-based explanation articles that clarify design decisions, trade-offs, and architectural rationale.

**Value**:
- Answer "why?" questions (not "how?")
- Justify architectural choices (ΔGate, receipts, partitioned universes)
- Compare alternatives (Oxigraph vs N3, event sourcing vs receipts)
- Educate on theoretical foundations (cryptography, maturity model, 80/20)

**Scope**: 8 explanation articles (10-15 min read each)
- **E1**: Why ΔGate Architecture?
- **E2**: Receipt Chain Cryptographic Guarantees
- **E3**: L1-L5 Maturity Model Philosophy
- **E4**: Dark Matter 80/20 Framework - PROVEN
- **E5**: Why Partitioned Universes?
- **E6**: Cross-Runtime RDF Bridging
- **E7**: Proof-Based Admission vs Traditional Editing
- **E8**: Performance Trade-offs: Oxigraph vs N3 - PROVEN

### Acceptance Criteria
- [ ] All 8 explanation articles written with evidence citations
- [ ] E4 (Dark Matter 80/20) references existing tests + examples
- [ ] E8 (Oxigraph vs N3) cites actual benchmarks (10-100x speedup)
- [ ] E2 (Cryptography) explains BLAKE3, Merkle trees, attack vectors
- [ ] E3 (Maturity Model) references test/l5-maturity/ test suite
- [ ] Every claim has evidence pointer (test, benchmark, ADR, code)
- [ ] No speculation or "future work" (evidence-based only)
- [ ] Each article includes "trade-offs" section (not just benefits)

### Key Stories
1. **Architectural Rationale** - ΔGate (E1), Partitioned Universes (E5), Proof-Based Admission (E7)
2. **Cryptographic Foundations** - Receipt chains, Merkle trees, blockchain anchoring (E2)
3. **Quality Frameworks** - L5 Maturity (E3), Dark Matter 80/20 (E4)
4. **Performance Analysis** - Oxigraph vs N3 benchmarks (E8)
5. **Cross-Runtime Strategy** - Browser/Node.js/WASM (E6)
6. **Evidence Extraction** - Extract proofs from tests, benchmarks, ADRs

### Dependencies
- Blocked by: EPIC-DOCS-001 (clean structure)
- Blocks: None (parallel to other epics)

### Estimated Effort
- T-shirt size: **M**
- Weeks: **2**
- Risk: Low (2/8 already proven, others need extraction from existing code)

---

## EPIC-DOCS-006: Example Hierarchy & Validation

**Goal**: Prune 112 examples → 30 high-quality, tested examples in 3 tiers (beginner, intermediate, advanced).

**Value**:
- Clear progression path (Tier 1 → Tier 2 → Tier 3)
- Every example runs in <30s (timeout enforced)
- Examples serve as integration tests
- Reduce "example doesn't work" bug reports to 0

**Scope**: 30 examples in 3 tiers
- **Tier 1: Beginner** (8 examples, 5-10 min) - Hello RDF, SPARQL, insert/delete, hooks, receipts, time-travel, streaming, validation
- **Tier 2: Intermediate** (12 examples, 15-25 min) - Dark Matter, federation, YAWL, policies, audit, deltas, large graphs, OTEL, Docker, migration, L5, adversarial
- **Tier 3: Advanced** (10 examples, 25-40 min) - Integration, blockchain, inference, Raft, WASM, ML, analytics, semantic search, collaboration, project engine

### Acceptance Criteria
- [ ] 30 examples exist with clear tier labels (T1, T2, T3)
- [ ] 100% of Tier 1 examples run in <10s (beginner-friendly)
- [ ] 90% of Tier 2 examples run in <30s
- [ ] Tier 3 examples document any long-running operations
- [ ] Examples README with selection guide (by tier, by feature, by time)
- [ ] All examples have timeout enforcement in CI
- [ ] 82 examples deleted or archived (112 → 30)
- [ ] Every example has "Prerequisites", "Expected Output", "Next Steps"

### Key Stories
1. **Tier 1 Examples** - Create/validate 8 beginner examples (4 exist, 4 needed)
2. **Tier 2 Examples** - Validate dark-matter, create 11 others
3. **Tier 3 Examples** - Validate 3 existing advanced examples, create 7 more
4. **Example Selection Guide** - README with decision tree (tier, feature, time)
5. **CI Validation** - Add example execution to CI (timeout 30s per example)
6. **Archive Unused Examples** - Delete/archive 82 redundant examples

### Dependencies
- Blocked by: EPIC-DOCS-001 (clean structure)
- Blocks: EPIC-DOCS-002 (tutorials reference examples)

### Estimated Effort
- T-shirt size: **L**
- Weeks: **2-3**
- Risk: Medium (need to verify which examples actually work)

---

## EPIC-DOCS-007: Navigation & Developer Experience

**Goal**: Create seamless navigation between docs, examples, and code with <30s to find any topic.

**Value**:
- New users find T1 (Getting Started) in <10s
- Developers find relevant how-to in <30s
- API reference searchable (by package, function, keyword)
- Learning paths clear (beginner → intermediate → advanced)

**Scope**: Navigation infrastructure
- Diataxis README (learning path hub)
- Tutorial index (progression map)
- How-to index (by problem category)
- Reference index (searchable API catalog)
- Explanation index (by topic)
- Example selection guide (by tier/feature/time)
- Cross-links (tutorials → how-tos → references → explanations)

### Acceptance Criteria
- [ ] `/docs/diataxis/README.md` explains all 4 quadrants + when to use each
- [ ] Every tutorial has "Next Steps" links (T1 → T2 → T3 → T4 → T5)
- [ ] How-to index organized by problem category (Core, Integration, Production)
- [ ] Reference index searchable (Ctrl+F by package or function)
- [ ] Example README decision tree (3 questions → recommended example)
- [ ] All cross-links validated (no 404s)
- [ ] Search test: 10 common queries resolve in <30s

### Key Stories
1. **Diataxis Hub README** - Explain 4 quadrants, learning paths
2. **Tutorial Progression Map** - Visual/text guide (T1 → T5)
3. **How-To Category Index** - Organize by problem type
4. **API Reference Search** - Searchable function catalog
5. **Example Decision Tree** - "I want to..." → recommended example
6. **Cross-Link Validation** - Automated link checker
7. **User Testing** - Time 5 common search tasks (<30s target)

### Dependencies
- Blocked by: EPIC-DOCS-002, EPIC-DOCS-003, EPIC-DOCS-004, EPIC-DOCS-006 (needs content complete)
- Blocks: None (final epic)

### Estimated Effort
- T-shirt size: **S**
- Weeks: **1**
- Risk: Low (navigation layer over existing content)

---

## Success Metrics (Across All EPICs)

### Quantitative Targets:
| Metric | Current | Target | Reduction |
|--------|---------|--------|-----------|
| Total .md files | 429 | 40 | 91% |
| Cruft files | 255 | 0 | 100% |
| Examples | 112 | 30 | 73% |
| Broken links | ? | 0 | 100% |
| Tutorials (runnable) | 3/5 | 5/5 | +67% |
| How-tos (proven) | 2/15 | 13/15 | +550% |

### Qualitative Gates:
- ✅ New user productive in <15 min (T1)
- ✅ 80% of support questions answered by docs
- ✅ 100% of examples run in CI (timeout enforced)
- ✅ Zero speculation (all claims evidence-based)
- ✅ API reference 100% accurate (code-linked)

### Adversarial PM Test:
**Question**: "Can I reproduce EVERY claim in these docs from scratch?"
**Answer**: YES (or doc deleted)

---

## EPIC Dependency Graph

```
EPIC-DOCS-001 (Cruft Deletion)
    ↓
    ├─→ EPIC-DOCS-002 (Tutorials)
    ├─→ EPIC-DOCS-003 (How-To Guides)
    ├─→ EPIC-DOCS-004 (API Reference)
    ├─→ EPIC-DOCS-005 (Explanations)
    └─→ EPIC-DOCS-006 (Examples)
            ↓
        (All above complete)
            ↓
    EPIC-DOCS-007 (Navigation & DX)
```

**Critical Path**: DOCS-001 → DOCS-002 → DOCS-007 (5-6 weeks)
**Parallel Tracks**: DOCS-003, DOCS-004, DOCS-005, DOCS-006 (weeks 2-4)

---

## Timeline (16-Week v6 Roadmap Integration)

| Week | EPICs | Deliverables |
|------|-------|--------------|
| 1-2 | DOCS-001 | Cruft deleted, structure clean |
| 2-3 | DOCS-006 | Example hierarchy validated |
| 3-4 | DOCS-002 | T1-T3 proven, T4-T5 created |
| 3-5 | DOCS-003 | 15 how-to guides |
| 4-5 | DOCS-004 | 12 API references |
| 4-6 | DOCS-005 | 8 explanation articles |
| 6 | DOCS-007 | Navigation complete |

**Total Documentation Effort**: 6 weeks (parallel to Phase 1-2 of v6 implementation)

---

## Risk Register

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| KGC-4D examples don't work | 40% | High | Fix KGC-4D package first (block T4) |
| Too many examples fail | 50% | Medium | Start with pruning, then fix remaining |
| Reference generation brittle | 30% | Low | Manual fallback for complex APIs |
| User testing delayed | 60% | Low | Internal team testing acceptable |
| Migration guide incomplete | 35% | Medium | Focus on common patterns only |

---

**END OF DOCS EPICs**

**Next Actions**:
1. Review EPICs with team
2. Create GitHub issues for each EPIC
3. Start EPIC-DOCS-001 (cruft deletion) immediately
4. Validate T4/T5 proof requirements

**Adversarial PM Check**:
- ❓ Are these EPICs realistic (not aspirational)? **YES** (based on existing evidence)
- ❓ Can all be completed in 6 weeks? **YES** (if parallelized)
- ❓ What's the biggest blocker? **KGC-4D proof validation (T4)**
- ❓ What's the minimum viable subset? **DOCS-001 + DOCS-002 (T1-T3) + DOCS-007 = 3 weeks**
