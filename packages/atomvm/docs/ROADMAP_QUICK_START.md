# Improvement Roadmap - Quick Start Guide

**Generated**: 2025-12-21
**For**: Immediate action on UNRDF monorepo improvements

---

## 🚀 Start Here (Next 1-2 Weeks)

### Phase 1: CRITICAL ISSUES - 26-34 hours

**Must complete before any other work. Blocks production deployment.**

---

### Week 1: Maturity Matrix Foundation (16-20 hours)

**BLOCKING** - No other work can proceed until this is complete.

**Tasks (can run in parallel)**:
```bash
# 1. Create structure (1h)
mkdir -p packages/cli/src/lib/maturity/{formatters,synergies}

# 2. Create RDF ontology (2h)
# File: packages/core/src/ontologies/maturity.ttl
# See: specs/006-maturity-matrix/data-model.md

# 3. Create Zod schemas (2h)
# File: packages/cli/src/lib/maturity/schemas.mjs
# Validate: MaturityLevel, CoverageMetrics, etc.

# 4. Implement coverage collector (4h)
# File: packages/cli/src/lib/maturity/collector.mjs
# Parse: packages/*/coverage/coverage-summary.json

# 5. Implement weighted scorer (3h)
# File: packages/cli/src/lib/maturity/scorer.mjs
# Criteria: Coverage 25%, API 20%, Docs 15%, Tests 15%, etc.

# 6. Create base CLI command (2h)
# File: packages/cli/src/commands/maturity.mjs
# Using: citty CLI framework

# 7. OTEL instrumentation (2h)
# File: packages/validation/src/maturity/otel-collector.mjs
# Emit spans for assessment operations
```

**Success Check**:
```bash
# Should work:
unrdf maturity assess core

# Should show:
# - Maturity level (L1-L5)
# - Weighted score (0-100)
# - Coverage metrics
# - Assessment timestamp
```

---

### Week 1-2: N3 Rule Reasoning (6-8 hours)

**HIGH IMPACT** - Enables knowledge graph inference

**Tasks (sequential)**:
```bash
# 1. Research eye.js integration (2h)
# Read: eye.js documentation
# Analyze: packages/core/src/rdf/minimal-n3-integration.mjs:108

# 2. Implement N3 rule execution (3h)
# Add dependency: eye.js
# Implement: Rule parsing and execution
# Location: packages/core/src/rdf/minimal-n3-integration.mjs

# 3. Add tests (2h)
# Test: Rule parsing, execution, inference correctness
# Coverage: 80%+ for new code

# 4. Documentation (1h)
# JSDoc: All public functions
# Examples: Rule sets in docs/
```

**Success Check**:
```bash
# Should work:
# N3 rules execute on RDF graphs
# Inference results validated

# Example rule:
# { ?s a :Person } => { ?s a :Human }
```

---

### Week 2: Sidecar Graph Listing (4-6 hours)

**MODERATE IMPACT** - Enables federated graph discovery

**Tasks (sequential)**:
```bash
# 1. Design RPC protocol (2h)
# Define: Request/response schema
# Plan: Error handling, timeout, retry

# 2. Implement sidecar discovery (2h)
# File: packages/cli/src/commands/graph/list.mjs (lines 100-156)
# Query: Remote sidecars via RPC
# Aggregate: Local + remote results

# 3. Add tests (2h)
# Mock: Sidecar responses
# Test: Timeout, partial failure scenarios
# Coverage: 80%+
```

**Success Check**:
```bash
# Should work:
unrdf graph list

# Should show:
# - Local graphs
# - Remote graphs from all sidecars
# - Handle sidecar failures gracefully
# - Performance: <2s for 10 sidecars
```

---

## 📋 Phase 1 Completion Checklist

Before starting Phase 2, verify:

- [ ] `unrdf maturity assess core` works and shows maturity level
- [ ] Coverage data collected from Vitest reports
- [ ] Weighted scores calculated correctly (7 criteria)
- [ ] OTEL spans validate ≥80/100
- [ ] N3 rules execute correctly on RDF graphs
- [ ] Inference results validated
- [ ] `unrdf graph list` shows local + remote graphs
- [ ] All existing tests still pass (231/231)
- [ ] 0 critical Andon signals
- [ ] All code reviewed and approved

**If ANY item fails, do NOT proceed to Phase 2.**

---

## 🎯 What Happens Next (Phase 2+)

### Phase 2: HIGH PRIORITY (2-6 weeks, 72-100 hours)

After Phase 1 complete:

1. **Complete Maturity Assessment** (24-32h)
   - Assess all 21 packages
   - Output formats: table, JSON, TTL
   - All 7 criteria automated

2. **Synergy Documentation** (12-16h)
   - 11 synergy categories (A-K)
   - Package-to-synergy mapping
   - CLI: `unrdf maturity synergy`

3. **Fix Pre-existing Test Failures** (16-24h)
   - CLI: 18 failures → 0
   - Streaming: Failures → 0
   - All 249+ tests passing

4. **Reduce Linting Warnings** (12-16h)
   - 153 warnings → <80
   - New baseline documented
   - Pre-commit hook added

5. **API Stability Tracking** (8-12h)
   - Detect breaking changes
   - Assign stability levels
   - Integrate into maturity

### Phase 3: MEDIUM PRIORITY (6-10 weeks, 74-102 hours)

After Phase 2 complete:

1. Report generation (all 21 packages)
2. Package comparison
3. OTEL observability (score ≥80/100)
4. Comprehensive documentation
5. Security scanning automation
6. Performance benchmarking suite

### Phase 4: LOW PRIORITY (Future, 56-82 hours)

Polish and enhancements:
- Streaming validation schemas
- Zero linting warnings
- Enhanced error testing
- Documentation improvements

---

## 👥 Team Recommendations

**Minimum for Phase 1**:
- 1 Backend Developer (Node.js, RDF, CLI)
- Can complete in 1-2 weeks full-time

**Optimal for Phase 1**:
- 1 Senior Backend Developer (lead, complex N3 work)
- 1 Junior Backend Developer (CLI, testing)
- Can complete in 1 week

**Skills Needed**:
- ✓✓✓ Node.js / JavaScript (ES modules, MJS)
- ✓✓✓ RDF / SPARQL / Turtle
- ✓✓✓ CLI development (citty framework)
- ✓✓ Testing (Vitest)
- ✓✓ OTEL / Observability
- ✓ eye.js / N3 reasoning

---

## 🚨 Critical Constraints

**DO NOT**:
- ❌ Skip Phase 1 foundation work
- ❌ Start Phase 2 before Phase 1 complete
- ❌ Commit code with failing tests
- ❌ Introduce new critical Andon signals
- ❌ Reduce test coverage

**DO**:
- ✅ Complete Phase 1 foundation FIRST
- ✅ Keep all tests passing (231/231 minimum)
- ✅ Maintain 0 critical Andon signals
- ✅ Run `pnpm check:andon` before commits
- ✅ Get code review for all changes
- ✅ Document all decisions

---

## 📊 Success Metrics (Phase 1)

| Metric                  | Baseline | Target | Status |
| ----------------------- | -------- | ------ | ------ |
| Maturity CLI            | None     | Working | ⏳     |
| N3 Reasoning            | Stubbed  | Working | ⏳     |
| Sidecar Discovery       | Incomplete | Working | ⏳     |
| Core Tests Passing      | 231/231  | 231/231 | ✅     |
| Critical Andon Signals  | 0        | 0      | ✅     |
| Linting Errors          | 0        | 0      | ✅     |
| OTEL Validation         | N/A      | ≥80/100 | ⏳     |

---

## 📚 References

**Full Documentation**:
- [COMPREHENSIVE_IMPROVEMENT_ROADMAP.md](./COMPREHENSIVE_IMPROVEMENT_ROADMAP.md) - Complete roadmap (1,074 lines)
- [ROADMAP_SUMMARY.md](./ROADMAP_SUMMARY.md) - Executive summary (343 lines)
- [specs/006-maturity-matrix/spec.md](/Users/sac/unrdf/specs/006-maturity-matrix/spec.md) - Feature specification
- [specs/006-maturity-matrix/tasks.md](/Users/sac/unrdf/specs/006-maturity-matrix/tasks.md) - All 50 tasks
- [specs/006-maturity-matrix/data-model.md](/Users/sac/unrdf/specs/006-maturity-matrix/data-model.md) - RDF ontology

**Quality Controls**:
- [docs/ANDON_SIGNALS.md](/Users/sac/unrdf/docs/ANDON_SIGNALS.md) - Andon signal management
- [docs/COMPLETION_REPORT_2025_12_20.md](/Users/sac/unrdf/docs/COMPLETION_REPORT_2025_12_20.md) - Recent completion

**Commands**:
```bash
# Check quality
pnpm check:andon

# Run tests
pnpm test:core

# Run linting
pnpm lint

# Check structure
pnpm check:structure
```

---

## 🎬 Getting Started NOW

**Right now, you should**:

1. **Review** this quick start guide (5 min)
2. **Read** full roadmap for context (15 min)
3. **Allocate** 1 developer for Week 1 (16-20h)
4. **Create** tracking board (GitHub Projects, 10 min)
5. **Begin** Phase 1, Task 1: Create maturity structure (1h)

**Today's goal**:
- [ ] Roadmap reviewed and approved
- [ ] Developer allocated
- [ ] Tracking board created
- [ ] First task started (maturity structure)

**This week's goal**:
- [ ] Maturity CLI foundation complete
- [ ] `unrdf maturity assess core` working
- [ ] OTEL validation ≥80/100

**Next week's goal**:
- [ ] N3 reasoning enabled
- [ ] Sidecar discovery working
- [ ] Phase 1 COMPLETE, Phase 2 can start

---

**Questions?** See full roadmap or contact project lead.

**Status**: Ready to start Phase 1 immediately
**Timeline**: 1-2 weeks to complete Phase 1
**Next**: Begin Task 1 (maturity structure)
