# UNRDF V6 Architecture - Complete Documentation Index

**Complete architectural redesign proposal: 57 packages → 12 packages**

---

## Start Here

**New to this proposal?** Read in this order:

1. **V6-ARCHITECTURE-SUMMARY.md** (4KB, 15 min read)
   - Executive summary with all key decisions
   - Perfect elevator pitch for stakeholders

2. **V6-TRANSFORMATION-VISUAL.md** (25KB, 20 min read)
   - Before/after visual comparison
   - Shows exactly what changes and why

3. **V6-ARCHITECTURE-PROPOSAL.md** (35KB, 45 min read)
   - Comprehensive specification
   - All 12 packages detailed
   - Layer architecture
   - Dependency graphs

4. **V6-IMPLEMENTATION-QUICKSTART.md** (15KB, 30 min read)
   - Concrete implementation guide
   - Week-by-week migration plan
   - Code examples and quality gates

**Total reading time**: ~2 hours to fully understand the v6 architecture

---

## Document Catalog

### Core Architecture Documents

#### 1. V6-ARCHITECTURE-SUMMARY.md
**Size**: 4KB | **Reading time**: 15 minutes
**Purpose**: Executive summary and quick reference

**Contains**:
- Problem statement (57 packages, unclear boundaries)
- Solution overview (12 packages, 3 layers)
- Key consolidations table
- Success metrics
- Migration timeline

**When to read**: First thing, before diving into details

---

#### 2. V6-ARCHITECTURE-PROPOSAL.md
**Size**: 35KB | **Reading time**: 45 minutes
**Purpose**: Comprehensive technical specification

**Contains**:
- Detailed description of all 12 packages
- Layer architecture with dependency rules
- Mermaid dependency graphs
- Kill list (45 packages eliminated)
- Merge candidates with rationale
- New packages and their responsibilities
- Migration strategy (phased, 7 weeks)
- Success metrics and quality gates
- Architecture Decision Records (ADRs)
- Package comparison tables

**When to read**: After summary, before implementation

**Sections**:
1. Proposed Package Count (12 packages)
2. Layer Architecture (3 layers)
3. Dependency Graph
4. Kill List (45 packages)
5. Merge Candidates
6. New Packages
7. Migration Strategy
8. Success Metrics
9. Risks and Mitigation
10. ADRs
11. File Structure
12. Conclusion
13. Appendix: Package Comparison

---

#### 3. V6-KILL-LIST-EVIDENCE.md
**Size**: 14KB | **Reading time**: 25 minutes
**Purpose**: Evidence-based justification for every elimination

**Contains**:
- Code-level evidence for duplications
- LOC analysis for thin packages
- Dependency analysis showing circular deps
- Specific examples of duplicate implementations
- Category-by-category breakdown
- Summary statistics

**When to read**: When you need to defend architectural decisions

**Categories**:
1. Duplicate Cryptographic Provenance (5 → 1)
2. YAWL Integration Sprawl (9 → 1)
3. CLI Fragmentation (3 → 1)
4. Runtime Distribution (4 → 1)
5. AI/ML Fragmentation (5 → 1)
6. UI Fragmentation (4 → 1)
7. Tool Fragmentation (5 → 1)
8. Hooks/Rules Overlap (2 → 1)
9. Core Split (2 → 2 different)
10. KILLED ENTIRELY (17 packages)

**Key Evidence Examples**:
```javascript
// BEFORE: Duplicate Merkle trees in 3 packages
kgc-4d/src/freeze.mjs: verifyReceipt(receipt) { ... }
receipts/src/batch-receipt-generator.mjs: verifyBatchReceipt(receipt) { ... }
blockchain/src/merkle/merkle-proof-generator.mjs: class MerkleProofGenerator { ... }

// AFTER: Single implementation in @unrdf/governance
governance/src/receipts/index.mjs: verifyReceipt(receipt) { ... }
```

---

#### 4. V6-IMPLEMENTATION-QUICKSTART.md
**Size**: 15KB | **Reading time**: 30 minutes
**Purpose**: Step-by-step implementation guide

**Contains**:
- Phase 1: Foundation setup (Week 1-2)
  - Create @unrdf/store (with code examples)
  - Create @unrdf/rdf (with code examples)
  - Create @unrdf/governance (with code examples)
- Phase 2: Runtime layer (Week 3-4)
- Phase 3: Applications (Week 5-6)
- Phase 4: Validation (Week 7)
- Testing strategy
- Migration checklist
- Quality gates
- Quick commands reference
- Success metrics dashboard

**When to read**: When ready to start implementation

**Includes**:
- Actual package.json files
- Actual source code for initial implementations
- Test examples
- Build commands
- Quality gate criteria

---

#### 5. V6-TRANSFORMATION-VISUAL.md
**Size**: 25KB | **Reading time**: 20 minutes
**Purpose**: Visual before/after comparison

**Contains**:
- ASCII art diagrams of v5 vs v6 architecture
- Package count by category table
- LOC distribution bar charts
- Dependency graph transformation
- Import pattern transformation (confusing → intuitive)
- Migration path visualization
- Key metrics transformation table
- One-sentence summary

**When to read**: To quickly understand the transformation

**Visual Highlights**:
- Before: 57 packages in chaotic layers
- After: 12 packages in 3 clean layers
- Side-by-side comparison tables
- Import pattern examples showing improvement

---

### Supporting Documents

#### 6. Current Codebase Analysis
**Generated during this session**:
- Total packages: 57 actual packages found
- Total LOC: 417,144 lines of code
- Total files: 1,468 .mjs files
- Largest packages: yawl (39K), kgc-claude (23K), core (23K)

**Analysis outputs**:
- Package sizes and dependencies extracted from package.json
- Code duplication identified (Merkle trees, receipt verification)
- Circular dependencies found (federation ↔ consensus)

---

## Quick Reference: The 12 V6 Packages

### Layer 1: Foundation (3 packages)
1. **@unrdf/store** - Oxigraph + SPARQL execution (8K LOC)
2. **@unrdf/rdf** - Data model + parsers + validation (6K LOC)
3. **@unrdf/governance** - Provenance + receipts + time-travel (12K LOC)

### Layer 2: Runtime (4 packages)
4. **@unrdf/workflows** - YAWL engine + durable execution (35K LOC)
5. **@unrdf/runtime** - Streaming + federation + consensus (10K LOC)
6. **@unrdf/hooks** - Policies + rules + inference (12K LOC)
7. **@unrdf/observability** - Metrics + tracing + logging (5K LOC)

### Layer 3: Applications (5 packages)
8. **@unrdf/cli** - Command-line tools (8K LOC)
9. **@unrdf/integrations** - Kafka/REST/GraphQL adapters (6K LOC)
10. **@unrdf/ai** - ML inference + semantic search (5K LOC)
11. **@unrdf/ui** - React/Vue components + visualization (4K LOC)
12. **@unrdf/tools** - Testing + docs + benchmarks (8K LOC)

---

## Key Decisions (ADRs)

### ADR-001: Why 12 packages?
- 5 too few (forces unrelated concerns together)
- 20 too many (back to fragmentation)
- 12 is sweet spot (each has clear responsibility)

### ADR-002: Why merge kgc-4d, receipts, blockchain?
- All do cryptographic provenance
- Duplicate Merkle tree implementations (3x)
- Single API eliminates confusion

### ADR-003: Why NOT merge YAWL integrations into workflows?
- Keeps core pure
- Integrations change frequently
- Plugin model more flexible

### ADR-004: Why split core into store + rdf?
- Separate execution from data model
- RDF parsing doesn't need SPARQL engine
- Smaller dependency graphs

### ADR-005: Why kill @unrdf/kgc-claude?
- 23K LOC with different release cycle
- Not core RDF concern
- Move to separate monorepo

### ADR-006: Why observability is cross-cutting?
- Metrics/tracing needed everywhere
- Doesn't fit clean layer model
- Special case acceptable

---

## Migration Timeline

```
Week 1-2: Foundation
  ├─ @unrdf/store
  ├─ @unrdf/rdf
  └─ @unrdf/governance
       Gate: Tests pass, benchmarks meet targets

Week 3-4: Runtime
  ├─ @unrdf/workflows
  ├─ @unrdf/runtime
  ├─ @unrdf/hooks
  └─ @unrdf/observability
       Gate: Workflows functional, no circular deps

Week 5-6: Applications
  ├─ @unrdf/cli
  ├─ @unrdf/integrations
  ├─ @unrdf/ai
  ├─ @unrdf/ui
  └─ @unrdf/tools
       Gate: All packages created, tests pass

Week 7: Validation
  └─ Quality gates
       Gate: 80% coverage, security audit, docs complete
```

---

## Success Metrics

| Metric | v5 | v6 | Target |
|--------|----|----|--------|
| Packages | 57 | 12 | 79% reduction |
| LOC | 417K | 120K | 71% reduction |
| Layers | 5+ | 3 | Clear boundaries |
| Circular deps | 2+ | 0 | Zero tolerance |
| Build time | 60s | <20s | 67% faster |
| Onboarding | 2-3 days | 4 hours | 83% faster |

---

## FAQ

### Q: Why such aggressive consolidation?
**A**: 57 packages is organizational debt. Every package adds cognitive load, build complexity, and maintenance burden. The goal is maximum simplicity while preserving all functionality.

### Q: What if I need a feature from a killed package?
**A**: No features were killed, only reorganized. Check the merge table to find where functionality moved. E.g., kgc-4d → @unrdf/governance.

### Q: Can I still use Kafka?
**A**: Yes! Import from `@unrdf/integrations/kafka`. Plugin model makes it cleaner.

### Q: What about backwards compatibility?
**A**: v6 is a clean break. v5 will have 12-month LTS support. Migration tool provided.

### Q: How long will migration take?
**A**: 7 weeks for core team. Includes testing, benchmarks, documentation.

### Q: What if we find issues during migration?
**A**: Quality gates at end of each phase. If gate fails, fix before continuing.

---

## Navigation Guide

**Goal** → **Read This**

| Goal | Document |
|------|----------|
| Understand the proposal in 15 min | V6-ARCHITECTURE-SUMMARY.md |
| See visual before/after | V6-TRANSFORMATION-VISUAL.md |
| Get comprehensive spec | V6-ARCHITECTURE-PROPOSAL.md |
| Start implementation | V6-IMPLEMENTATION-QUICKSTART.md |
| Defend architectural decisions | V6-KILL-LIST-EVIDENCE.md |
| Quick package reference | This index (V6-ARCHITECTURE-INDEX.md) |

---

## Document Status

All documents created: 2025-12-28

**Status**: PROPOSAL - Awaiting review and approval

**Next Steps**:
1. Review with team
2. Address concerns and questions
3. Get approval to proceed
4. Start Phase 1 implementation

---

## Document Sizes

```
  4K  V6-ARCHITECTURE-SUMMARY.md       ← Start here
 35K  V6-ARCHITECTURE-PROPOSAL.md      ← Comprehensive spec
 14K  V6-KILL-LIST-EVIDENCE.md         ← Evidence for decisions
 15K  V6-IMPLEMENTATION-QUICKSTART.md  ← Implementation guide
 25K  V6-TRANSFORMATION-VISUAL.md      ← Visual comparison
 11K  V6-ARCHITECTURE-INDEX.md         ← This document
────
104K  Total documentation
```

---

## Contact

For questions about this proposal:
- Architecture decisions → See ADRs in V6-ARCHITECTURE-PROPOSAL.md
- Implementation details → See V6-IMPLEMENTATION-QUICKSTART.md
- Evidence for eliminations → See V6-KILL-LIST-EVIDENCE.md
- General questions → Start with V6-ARCHITECTURE-SUMMARY.md

---

**Remember**: This is a complete rewrite from first principles. No backwards compatibility constraints. The goal is the ideal architecture for an RDF knowledge graph platform.

**The Litmus Test**: Can a new developer understand the entire architecture in 1 hour?

**v5 Answer**: NO (57 packages, unclear boundaries)
**v6 Answer**: YES (12 packages, 3 layers, this index)
