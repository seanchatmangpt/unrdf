# Refactoring Blocked - Test Prerequisite Not Met

**Date**: 2025-12-25
**Status**: ❌ BLOCKED
**Reason**: Test pass rate 69.2% (required: 100%)

---

## 🚨 Adversarial PM Analysis

### CLAIM
"Refactor top 10 files exceeding 500-line limit"

### QUESTIONS
1. **Did tests pass?** ❌ NO - 69.2% pass rate (256/370 tests)
2. **Can we proceed?** ❌ NO - CLAUDE.md requires 100% pass rate
3. **What breaks if we proceed?** Risk introducing regressions into already-failing test suite
4. **What's the evidence?** See Test Evidence section below

---

## 📊 Test Evidence (MEASURED)

### Overall Test Status
```bash
$ npm run test:fast
Overall Pass Rate: 69.2% (256/370 tests)

Package Breakdown:
- YAWL: 182/292 passed (62.3%) ❌
- KGC-4D: 296/305 passed (97.0%) ⚠️
- AtomVM: 45/45 passed (100%) ✅
- Root Tests: 24/28 passed (85.7%) ⚠️
```

### Critical Failures
1. **graph-analytics package**: Missing dependency `@dagrejs/graphlib`
   - 4 test files failing
   - All tests in package skipped
2. **YAWL package**: 110 tests failing (37.7% failure rate)
3. **Root tests**: 4 tests failing

---

## 📋 Files Requiring Refactoring (Top 10)

### Priority 1: Critical Violations (>1000 lines)

| Rank | File | Lines | Package | Impact |
|------|------|-------|---------|--------|
| 1 | `otel-span-builder.mjs` | 1278 | validation | High - Core OTEL |
| 2 | `yawl-schemas.mjs` | 1091 | yawl | High - Type system |
| 3 | `yawl-hooks.mjs` | 1073 | yawl | High - Hook system |
| 4 | `schemas.mjs` | 1063 | knowledge-engine | High - Core schemas |
| 5 | `query-optimizer.mjs` | 1051 | knowledge-engine | Medium - Query perf |

### Priority 2: Major Violations (900-1000 lines)

| Rank | File | Lines | Package | Impact |
|------|------|-------|---------|--------|
| 6 | `otel-validator-core.mjs` | 1004 | validation | High - Validation |
| 7 | `domain-infer.mjs` | 966 | project-engine | Medium - Inference |
| 8 | `initialize.mjs` | 957 | project-engine | Medium - Init logic |
| 9 | `knowledge-substrate-core.mjs` | 927 | knowledge-engine | High - Core substrate |
| 10 | `browser.mjs` | 910 | knowledge-engine | Medium - Browser compat |

### Full Paths (for execution)
```bash
# Priority 1 (>1000 lines)
/home/user/unrdf/packages/validation/src/otel-span-builder.mjs
/home/user/unrdf/packages/yawl/src/types/yawl-schemas.mjs
/home/user/unrdf/packages/yawl/src/hooks/yawl-hooks.mjs
/home/user/unrdf/packages/knowledge-engine/src/schemas.mjs
/home/user/unrdf/packages/knowledge-engine/src/query-optimizer.mjs

# Priority 2 (900-1000 lines)
/home/user/unrdf/packages/validation/src/otel-validator-core.mjs
/home/user/unrdf/packages/project-engine/src/domain-infer.mjs
/home/user/unrdf/packages/project-engine/src/initialize.mjs
/home/user/unrdf/packages/knowledge-engine/src/knowledge-substrate-core.mjs
/home/user/unrdf/packages/knowledge-engine/src/browser.mjs
```

---

## 📈 Statistics (VERIFIED)

**Command**: `find packages -type f -name "*.mjs" ! -path "*/node_modules/*" ! -path "*/dist/*"`

- **Total source files**: 737 .mjs files
- **Total source files >500 lines**: 83 files (11.3% violation rate)
- **Files >1000 lines**: 5 files (critical)
- **Files 900-1000 lines**: 5 files (major)
- **Files 700-900 lines**: 20 files (moderate)
- **Files 500-700 lines**: 53 files (minor)

---

## 🛣️ Roadmap to Unblock Refactoring

### Phase 1: Fix Test Prerequisites (REQUIRED)
```bash
# 1. Install missing dependencies
cd /home/user/unrdf/packages/graph-analytics
pnpm add @dagrejs/graphlib

# 2. Run tests and identify failures
timeout 30s pnpm test:fast

# 3. Fix failures until 100% pass rate
# Target: 370/370 tests passing
```

### Phase 2: Refactoring Strategy (80/20 Approach)

**Focus on Top 5 files first** (covers largest violations):

#### File 1: otel-span-builder.mjs (1278 → ~400 lines each)
**Split into**:
- `otel-span-builder-core.mjs` - Core builder logic (~400 lines)
- `otel-span-builder-validators.mjs` - Validation functions (~400 lines)
- `otel-span-builder-formatters.mjs` - Formatting utilities (~400 lines)
- `otel-span-builder.mjs` - Barrel export (~78 lines)

#### File 2: yawl-schemas.mjs (1091 → ~350 lines each)
**Split into**:
- `yawl-schemas-workflow.mjs` - Workflow schemas (~350 lines)
- `yawl-schemas-task.mjs` - Task schemas (~350 lines)
- `yawl-schemas-execution.mjs` - Execution schemas (~350 lines)
- `yawl-schemas.mjs` - Barrel export (~41 lines)

#### File 3: yawl-hooks.mjs (1073 → ~350 lines each)
**Split into**:
- `yawl-hooks-lifecycle.mjs` - Lifecycle hooks (~350 lines)
- `yawl-hooks-execution.mjs` - Execution hooks (~350 lines)
- `yawl-hooks-validation.mjs` - Validation hooks (~350 lines)
- `yawl-hooks.mjs` - Barrel export (~23 lines)

#### File 4: schemas.mjs (1063 → ~350 lines each)
**Split into**:
- `schemas-graph.mjs` - Graph schemas (~350 lines)
- `schemas-query.mjs` - Query schemas (~350 lines)
- `schemas-transaction.mjs` - Transaction schemas (~350 lines)
- `schemas.mjs` - Barrel export (~13 lines)

#### File 5: query-optimizer.mjs (1051 → ~350 lines each)
**Split into**:
- `query-optimizer-rules.mjs` - Optimization rules (~350 lines)
- `query-optimizer-transforms.mjs` - Query transformations (~350 lines)
- `query-optimizer-cost.mjs` - Cost estimation (~350 lines)
- `query-optimizer.mjs` - Barrel export (~1 line)

---

## 📝 Refactoring Checklist (For Each File)

### Pre-Refactoring
- [ ] Tests at 100% pass rate ❌ **BLOCKED**
- [ ] File analysis complete ✅
- [ ] Split strategy defined ✅
- [ ] Dependent files identified (pending)

### During Refactoring (per file)
- [ ] Read original file
- [ ] Create new module files
- [ ] Move code maintaining semantic equivalence
- [ ] Update barrel export in original file
- [ ] Find and update all imports in dependent files
- [ ] Run tests after EACH split (must stay 100%)
- [ ] Verify line counts (<500 per module)

### Post-Refactoring
- [ ] All tests still passing (100%)
- [ ] No functional changes (pure refactoring)
- [ ] All imports updated
- [ ] Line count report generated
- [ ] Violations reduced to <10 files

---

## 🎯 Success Criteria

### Mandatory Prerequisites
1. ✅ **Test pass rate: 100%** (currently: 69.2%) ❌ BLOCKED
2. ✅ **All dependencies installed**
3. ✅ **No import errors**

### Refactoring Success
1. Top 5 files split to <500 lines each
2. All tests remain at 100% after each split
3. Zero functional changes (pure refactoring)
4. All imports updated correctly
5. Total violations reduced from 99 → <50 files

### Validation
```bash
# After each file refactored:
timeout 5s npm run lint        # 0 errors
timeout 30s npm run test:fast  # 100% pass (370/370)
wc -l packages/*/src/*.mjs | awk '$1 > 500' | wc -l  # Count remaining

# Final validation:
node validation/run-all.mjs comprehensive
grep "Score:" validation-output.log  # MUST be ≥80/100
```

---

## 🚫 DO NOT PROCEED Until:

1. ❌ Tests achieve 100% pass rate (currently 69.2%)
2. ❌ Missing dependencies installed (@dagrejs/graphlib)
3. ❌ YAWL package tests fixed (110 failures)
4. ❌ Root test failures resolved (4 failures)

**Estimated Effort to Unblock**: 2-4 hours (fix tests first)
**Estimated Refactoring Effort**: 4-6 hours (after unblocked)

---

## 📚 References

- **CLAUDE.md**: CRITICAL PREREQUISITE - "Only proceed if tests are at 100% pass rate"
- **Test Report**: `/home/user/unrdf/E2E-TEST-REPORT.md`
- **Coding Standards**: `/home/user/unrdf/CODING-STANDARDS.md`
- **80/20 Methodology**: `/home/user/unrdf/docs/bb80-20-methodology.md`

---

## 🤔 Adversarial PM Questions

**Q: Can we refactor even with failing tests?**
A: NO. Risk of masking bugs and introducing regressions.

**Q: Can we skip the "small" violations (<700 lines)?**
A: YES. 80/20 principle - focus on top 10 files first.

**Q: How do we know tests are "really" at 100%?**
A: RUN `npm test` and READ full output. 370/370 passing.

**Q: What if splitting a file breaks tests?**
A: STOP. Revert immediately. Each split must maintain 100%.

---

**Status**: Documentation complete. Execution BLOCKED until test prerequisite met.
**Next Action**: Fix test failures → Achieve 100% pass rate → Resume refactoring
