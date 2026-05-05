# Big Bang 80/20 Phased Consolidation Plan

**Objective:** Fix package imports → Run permutation tests → Make data-driven consolidation decisions

**Methodology:** Big Bang 80/20 (single-pass, Pareto-optimized, pattern-based)

**Timeline:** 4 phases, each delivering 80% of remaining value with 20% effort

---

## 📊 Pareto Analysis

### Current State

- ❌ 0/8 permutation tests passing
- ❌ ALL packages fail to import
- ❌ Workspace resolution broken
- ❌ Can't make consolidation decisions without data

### Value Distribution (80/20 Analysis)

| Phase       | Effort | Value Delivered     | Cumulative Value |
| ----------- | ------ | ------------------- | ---------------- |
| **Phase 1** | 20%    | Get 1 test passing  | 80%              |
| **Phase 2** | 20%    | Verify integrations | 95%              |
| **Phase 3** | 20%    | Full validation     | 99%              |
| **Phase 4** | 40%    | Documentation       | 100%             |

**Key Insight:** Getting ONE test to pass (Phase 1) proves the entire system works. That's 80% of the value.

---

## 🎯 Phase 1: Core Infrastructure (20% effort, 80% value)

**Goal:** Get `01-core-only.mjs` passing

**Why Phase 1 First:**

- Proves workspace resolution works
- Validates @unrdf/oxigraph is functional
- Establishes pattern for other packages
- If this fails, nothing else matters

### Phase 1 Tasks

#### latest Verify @unrdf/oxigraph Package Structure

- [ ] Check package.json exports are correct
- [ ] Verify src/index.mjs exists and exports functions
- [ ] Test: `node -e "import('@unrdf/oxigraph')"`

#### latest Fix Workspace Resolution (if needed)

- [ ] Option A: Build packages (`pnpm run build`)
- [ ] Option B: Fix export paths in package.json
- [ ] Option C: Use `--shamefully-hoist` flag

#### latest Validate Core Imports

- [ ] Test: `import { createStore, dataFactory } from '@unrdf/oxigraph'`
- [ ] Test: `import { executeQuerySync } from '@unrdf/core'`

#### latest Run 01-core-only.mjs

- [ ] Execute: `timeout 5s node permutation-tests/01-core-only.mjs`
- [ ] Verify: Exit code 0, test passes
- [ ] Measure: Time to complete

### Success Criteria (Phase 1)

- ✅ `01-core-only.mjs` exits with code 0
- ✅ Creates store, adds quad, queries successfully
- ✅ Completes in <5 seconds
- ✅ Pattern identified for other packages

### Deliverables (Phase 1)

- Working @unrdf/oxigraph package
- Working @unrdf/core package
- 1/8 permutation tests passing (latest%)
- Documented fix pattern

**Estimated Time:** 30-60 minutes

---

## 🔗 Phase 2: Integration Validation (20% effort, 15% value)

**Goal:** Verify package integrations work

**Why Phase 2:**

- Tests actual multi-package usage
- Validates dependency chains
- Proves hooks and kgc-4d can import core

### Phase 2 Tasks

#### latest Fix @unrdf/hooks Package

- [ ] Apply Phase 1 pattern to hooks
- [ ] Test: `import { defineHook } from '@unrdf/hooks'`
- [ ] Run: `02-hooks-only.mjs` (expected fail - needs core)
- [ ] Run: `05-core-hooks.mjs` (should pass)

#### latest Fix @unrdf/kgc-4d Package

- [ ] Apply Phase 1 pattern to kgc-4d
- [ ] Test: `import { KGCStore } from '@unrdf/kgc-4d'`
- [ ] Run: `06-core-kgc4d.mjs` (should pass)

#### latest Validate Dependencies

- [ ] Verify hooks imports core correctly
- [ ] Verify kgc-4d imports core + oxigraph correctly

### Success Criteria (Phase 2)

- ✅ 3/8 tests passing (01, 05, 06)
- ✅ Integration patterns validated
- ✅ Dependencies resolve correctly

**Estimated Time:** 30-45 minutes

---

## 📋 Phase 3: Full Validation (20% effort, 4% value)

**Goal:** Run all permutation tests, collect comprehensive data

**Why Phase 3:**

- Complete empirical validation
- Identifies edge cases
- Provides 100% coverage data for decisions

### Phase 3 Tasks

#### latest Fix Remaining Packages

- [ ] Fix @unrdf/knowledge-engine
- [ ] Test: `04-knowledge-engine-only.mjs`

#### latest Run All Permutations

- [ ] Execute: `node permutation-tests/run-all.mjs`
- [ ] Collect results for all 8 tests
- [ ] Document pass/fail patterns

#### latest Analyze Results

- [ ] Which packages work standalone?
- [ ] Which require dependencies?
- [ ] What's the minimal working set?

### Success Criteria (Phase 3)

- ✅ All permutation tests execute (pass or fail with clear reason)
- ✅ Comprehensive data collected
- ✅ Patterns documented

**Estimated Time:** 30 minutes

---

## 📊 Phase 4: Consolidation Decision (40% effort, 1% value)

**Goal:** Make data-driven consolidation recommendations

**Why Phase 4:**

- Based on empirical evidence from phases 1-3
- Clear recommendations with proof
- Actionable next steps

### Phase 4 Tasks

#### latest Analyze Test Results

- [ ] Calculate actual dependency graph
- [ ] Identify truly independent packages
- [ ] Measure overlap and duplication

#### latest Generate Recommendations

- [ ] Option A: Merge strategy (which packages to merge)
- [ ] Option B: Keep separate (with justification)
- [ ] Option C: Hybrid approach

#### latest Document Findings

- [ ] Create CONSOLIDATION-PLAN.md
- [ ] Include test evidence
- [ ] Provide implementation steps

### Success Criteria (Phase 4)

- ✅ Clear recommendation backed by data
- ✅ Implementation plan documented
- ✅ Trade-offs analyzed

**Estimated Time:** 45-60 minutes

---

## 🚀 Implementation Strategy

### Big Bang 80/20 Principles Applied

1. **Single-Pass Implementation**
   - Fix once, fix correctly
   - Use proven patterns from Phase 1
   - No rework between phases

2. **Pattern Reuse**
   - Phase 1 establishes pattern
   - Phases 2-3 replicate exact same fix
   - Copy, don't improve

3. **Pareto Optimization**
   - Phase 1 = 80% value (prove it works)
   - Phase 2 = 15% value (prove integrations)
   - Phase 3 = 4% value (complete coverage)
   - Phase 4 = 1% value (documentation)

4. **Evidence-Based**
   - Every phase produces measurable output
   - No assumptions, only test results
   - Adversarial PM validation at each phase

---

## 📈 Success Metrics

| Metric                    | Target         | Phase Achieved |
| ------------------------- | -------------- | -------------- |
| First test passing        | 1/8            | Phase 1        |
| Core integrations working | 3/8            | Phase 2        |
| Full test coverage        | 8/8 executable | Phase 3        |
| Consolidation decision    | Data-driven    | Phase 4        |

---

## ⚡ Quick Reference

### Phase 1 Commands

```bash
# Fix oxigraph
cat packages/oxigraph/package.json
ls packages/oxigraph/src/

# Test import
node -e "import('@unrdf/oxigraph').then(console.log)"

# Run test
timeout 5s node permutation-tests/01-core-only.mjs
```

### Phase 2 Commands

```bash
# Fix hooks + kgc-4d (same pattern as Phase 1)
timeout 5s node permutation-tests/05-core-hooks.mjs
timeout 5s node permutation-tests/06-core-kgc4d.mjs
```

### Phase 3 Commands

```bash
# Run all tests
timeout 60s node permutation-tests/run-all.mjs
```

---

## 🎯 Phase 1 Implementation Plan (NEXT)

**Ready to implement:** Phase 1 tasks broken down into executable steps

1. Check oxigraph package structure (5 min)
2. Fix workspace resolution (15 min)
3. Validate imports (10 min)
4. Run test and verify (5 min)
5. Document pattern (5 min)

**Total Phase 1 Time:** 40 minutes

**Let's begin Phase 1 implementation.**

---

**Generated:** December 6, 2024
**Methodology:** Big Bang 80/20 + Pareto Optimization
**Status:** ✅ Plan complete, ready for Phase 1 implementation
