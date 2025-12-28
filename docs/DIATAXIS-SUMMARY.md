# UNRDF v6 Diataxis Documentation - Executive Summary

**Date**: 2025-12-28
**Full Design**: See [V6-DIATAXIS-DESIGN.md](./V6-DIATAXIS-DESIGN.md)

---

## The Problem

Current documentation state:
- **429 markdown files** across 50+ directories
- **255 files (59%) are cruft** (internal planning, archives, thesis)
- **90% placeholder content** ("to be filled", aspirational features)
- **3 competing Diataxis structures** (docs/diataxis/, docs/v6/diataxis/, duplicates)
- **User confusion**: "Where do I start?" "What actually works?"

## The Solution: Evidence-Based Diataxis

**Target**: 40 high-quality docs (90% reduction)
**Principle**: ONLY document PROVEN capabilities

### Documentation Quadrants

```
┌─────────────────────────────────────────────────────────────────┐
│                        DIATAXIS FRAMEWORK                        │
├──────────────────────────────┬──────────────────────────────────┤
│  TUTORIALS (5)               │  EXPLANATION (8)                 │
│  Learning by doing           │  Understanding concepts          │
│                              │                                  │
│  T1: Getting Started (15min) │  E1: ΔGate Architecture (12min)  │
│  T2: First Hook (20min)      │  E2: Receipt Crypto (15min)      │
│  T3: Dark Matter (25min)     │  E3: L5 Maturity (10min)         │
│  T4: Time-Travel (30min)     │  E4: Dark Matter 80/20 (12min)   │
│  T5: Production (40min)      │  E5: Partitioned Universes       │
│                              │  E6: Cross-Runtime Bridging      │
│  Status: 3/5 PROVEN          │  E7: Proof-Based Admission       │
│                              │  E8: Oxigraph vs N3 (10min)      │
│                              │                                  │
│                              │  Status: 2/8 PROVEN              │
├──────────────────────────────┼──────────────────────────────────┤
│  HOW-TO (15)                 │  REFERENCE (12)                  │
│  Problem solving             │  Information lookup              │
│                              │                                  │
│  CORE (5):                   │  R1: CLI Command Matrix          │
│  H1: Query SPARQL            │  R2: Core API Reference          │
│  H2: Validate with Hooks     │  R3: Oxigraph API                │
│  H3: Receipt Verification    │  R4: Hooks API                   │
│  H4: Compose Deltas          │  R5: KGC-4D API                  │
│  H5: Measure Performance     │  R6: YAWL API                    │
│                              │  R7: Federation API              │
│  INTEGRATION (5):            │  R8: Streaming API               │
│  H6: YAWL + Hooks            │  R9: Receipt Schema              │
│  H7: Stream Large Graphs     │  R10: Zod Schema Catalog         │
│  H8: Federate Queries        │  R11: RDF Format Notes           │
│  H9: Migrate v5→v6           │  R12: Package Exports            │
│  H10: Import External        │                                  │
│                              │  Status: 8/12 CODE EXISTS        │
│  PRODUCTION (5):             │                                  │
│  H11: Docker + K8s           │                                  │
│  H12: Monitor OTEL           │                                  │
│  H13: Achieve L5             │                                  │
│  H14: Adversarial Tests      │                                  │
│  H15: Audit Trail            │                                  │
│                              │                                  │
│  Status: 2/15 PROVEN         │                                  │
└──────────────────────────────┴──────────────────────────────────┘
```

---

## Example Hierarchy (3 Tiers)

**Target**: 30 examples (down from 112)

### Tier 1: Beginner (8 examples, 5-10 min each)
- 01-hello-rdf.mjs ✅
- 02-sparql-query.mjs ✅
- 03-insert-delete.mjs
- 04-first-hook.mjs ✅
- 05-receipt-basics.mjs
- 06-time-travel.mjs
- 07-streaming-basics.mjs
- 08-validation.mjs ✅

Status: **4/8 EXIST**

### Tier 2: Intermediate (12 examples, 15-25 min each)
- dark-matter-80-20.mjs ✅ (286 LoC, COMPLETE)
- federation-multi-store.mjs
- yawl-workflow.mjs
- hook-policy-enforcement.mjs
- receipt-chain-audit.mjs
- delta-composition.mjs
- streaming-large-graph.mjs
- otel-observability.mjs
- docker-deployment.mjs
- migration-v5-v6.mjs
- l5-maturity-demo.mjs
- adversarial-testing.mjs

Status: **1/12 COMPLETE**

### Tier 3: Advanced (10 examples, 25-40 min each)
- comprehensive-feature-test.mjs ✅
- blockchain-anchoring.mjs
- custom-inference-engine.mjs
- distributed-raft-consensus.mjs
- wasm-browser-rdf.mjs
- ml-inference-integration.mjs
- graph-analytics.mjs
- semantic-search.mjs
- real-time-collab.mjs
- project-engine.mjs ✅

Status: **2/10 EXIST**

---

## Kill List: 255 Files to Delete/Archive

### DELETE (move to git history):
1. `/docs/internal/` - 1.8M internal planning
2. `/docs/archive/` - 1.8M old v3/v4 docs
3. `/docs/thesis-publication/` - Academic paper
4. `/docs/video-scripts/` - Not technical docs
5. `/docs/architecture-2028/` - Aspirational

### CONSOLIDATE (remove duplicates):
- 3 competing how-to directories → 1 canonical
- 3 competing tutorial directories → 1 canonical
- 3 competing reference directories → 1 canonical

### ARCHIVE (keep in /docs/archive/):
- `/docs/v5/` - Migration reference only
- `/docs/capabilities/` (60 subdirs) - Replace with examples
- `/docs/papers/` - Research papers
- `/docs/research/` - Exploratory work

---

## Quality Gates

Every document MUST pass:

| Gate | Requirement | Enforcement |
|------|-------------|-------------|
| **Evidence** | Link to proof artifact (code/test/benchmark) | Manual review |
| **Runnability** | Tutorials run in <5min on fresh install | CI validation |
| **Accuracy** | References link to actual code (file:line) | Automated check |
| **Traceability** | Explanations cite tests/benchmarks/ADRs | Manual review |

**Adversarial PM Test**: "Can I reproduce EVERY claim from scratch?"
- If NO → Document gets DELETED

---

## Current Status

| Quadrant | Target | Complete | Status |
|----------|--------|----------|--------|
| Tutorials | 5 | 3 | 60% PROVEN |
| How-To | 15 | 2 | 13% PROVEN |
| Reference | 12 | 8 | 67% CODE EXISTS |
| Explanation | 8 | 2 | 25% PROVEN |
| **TOTAL** | **40** | **15** | **38% COMPLETE** |

**Critical Path**: T1 (Getting Started) + dark-matter-80-20.mjs → PROVEN
**Blockers**: KGC-4D examples, Receipt verification, YAWL workflows need proof

---

## Next Actions (Priority Order)

### Week 1: Validate Existing Claims
1. ✅ Run dark-matter-80-20.mjs example (PROVEN: 286 LoC, works)
2. ⏳ Verify KGC-4D freeze example exists and runs
3. ⏳ Test receipt generation + verification
4. ⏳ Validate YAWL workflow execution
5. ⏳ Check federation multi-store queries

### Week 2: Build PROVEN Subset
1. ⏳ Create 3 PROVEN tutorials (T1-T3)
2. ⏳ Write 2 PROVEN how-tos (H9, H13)
3. ⏳ Generate 8 reference docs from code
4. ⏳ Extract 2 PROVEN explanations (E4, E8)

### Week 3: Archive Cruft
1. ⏳ Move 255 files to /docs/archive/
2. ⏳ Consolidate duplicate structures
3. ⏳ Create canonical /docs/diataxis/ structure
4. ⏳ Update all cross-links

### Week 4: Validation
1. ⏳ Run ALL examples (timeout 5s each)
2. ⏳ Verify ALL code links (file:line)
3. ⏳ Generate coverage report
4. ⏳ User testing: "Can new user start in <15min?"

---

## Success Criteria

### Quantitative:
- ✅ 40 docs (90% reduction from 429)
- ✅ 30 examples (73% reduction from 112)
- ✅ 0 cruft files (255 deleted/archived)
- ✅ 100% tutorials runnable
- ✅ 95% how-tos have proof
- ✅ 100% references link to code

### Qualitative:
- ✅ New user gets started in <15min (T1)
- ✅ Every doc maps to ACTUAL capability
- ✅ Zero speculation (all claims proven)
- ✅ Clear learning paths (beginner → advanced)

---

## File Structure (Target)

```
/home/user/unrdf/docs/
├── diataxis/                    # ⭐ CANONICAL structure
│   ├── tutorials/               # 5 tutorials (100% runnable)
│   ├── how-to/                  # 15 how-tos (90% runnable)
│   ├── reference/               # 12 references (100% code-linked)
│   ├── explanation/             # 8 explanations (100% evidence)
│   └── README.md                # Navigation hub
├── adr/                         # Architecture decisions (KEEP)
├── examples/                    # 30 working examples (KEEP)
├── v6/                          # V6 migration docs (consolidate)
└── archive/                     # 255 archived files (git only)
```

**Current**: 429 files, 59% cruft, 90% placeholders
**Target**: 40 files, 0% cruft, 100% proven

---

## Open Questions Requiring Proof

1. **KGC-4D freeze**: Does universe freeze actually work? Need running example.
2. **Receipt verification**: Cryptographic verification implemented?
3. **YAWL workflows**: Full workflow execution tested?
4. **Federation**: Multi-store queries working?
5. **Blockchain anchoring**: Git-notes OR Ethereum? Both? Neither?

**Next Step**: Run validation scripts to answer these questions.

---

**Full Design**: [V6-DIATAXIS-DESIGN.md](./V6-DIATAXIS-DESIGN.md) (46 pages, comprehensive)
**Questions**: See "Open Questions" in full design doc
**Timeline**: 4 weeks to complete migration
