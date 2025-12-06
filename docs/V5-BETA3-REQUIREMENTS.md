# v5.0.0-beta3 Requirements - Evidence-Based Planning

**Date**: 2025-12-06
**Current Version**: 5.0.0-beta.1
**Target**: 5.0.0-beta.3 (or beta.2 depending on numbering)
**Branch**: `claude/v5-beta2-requirements-01NKQfQKwKizGRkgaAsnhRuC`

---

## Executive Summary

Beta.1 achieved **190 tests passing**, **83/100 OTEL validation**, and **verified claims**. Beta3 should focus on:

1. **Increase test coverage** (190 → 250+)
2. **Improve OTEL score** (83 → 90+)
3. **Fix test execution** (pnpm -r test hangs)
4. **Verify performance** (benchmark 40% faster, 60% memory claims)
5. **Security audit** (zero critical vulnerabilities)

---

## Current State (Beta.1 Verified)

### ✅ What Works

| Component | Status | Evidence |
|-----------|--------|----------|
| Core tests | 166 passing | Ran 4 test files individually |
| CLI tests | 24 passing | Ran adversarial tests |
| OTEL validation | 83/100 | Ran validation script |
| Version alignment | 100% | All 15 packages at beta.1 |
| Dependencies | Installed | pnpm install succeeded |
| Documentation | Complete | 160+ files |

### ⚠️ Known Issues

| Issue | Impact | Workaround |
|-------|--------|------------|
| pnpm -r test hangs | Can't run all tests at once | Run per-package |
| knowledge-hooks-api OTEL | Lowers score to 83/100 | Known failure |
| Performance unverified | Claims not proven | Mark as "not benchmarked" |
| Some tests not run | Unknown test count | ~100 tests unrun |

### ❌ Removed

| Package | Reason | Recovery |
|---------|--------|----------|
| @unrdf/browser | 60+ failures, incomplete | git checkout a73d216 |
| @unrdf/react | Broken, removed earlier | - |

---

## Beta3 Goals

### Goal 1: Increase Test Coverage ✅ → 🎯 250+ tests

**Current**: 190 tests verified (core 166, CLI 24)

**Target**: 250+ tests verified

**Plan**:
1. Run remaining core tests (~100 more):
   - test/sparql/n3-backward-compat.test.mjs
   - test/sparql/branch-coverage.test.mjs
   - test/integration/store-integration.test.mjs
   - test/benchmarks/oxigraph-performance.test.mjs

2. Run CLI tests:
   - packages/cli/test/cli/cli.test.mjs

3. Run root test suite:
   - test/guards.test.mjs
   - test/project-engine/**/*.test.mjs
   - test/knowledge-engine/**/*.test.mjs
   - test/streaming/streaming.test.mjs
   - test/validation/otel-validation-v3.1.test.mjs

**Evidence Required**:
- Show output of each test file
- Count actual passing tests
- Update CHANGELOG with verified count

---

### Goal 2: Improve OTEL Validation 83/100 → 🎯 90+

**Current**: 83/100 (5/6 features passing)

**Target**: 90/100 (6/6 features passing)

**Failing Feature**: knowledge-hooks-api (0/100 - no spans collected)

**Plan**:
1. Investigate why knowledge-hooks-api produces no spans
2. Check if TracerProvider initialized correctly
3. Fix span collection or mark feature as deprecated
4. Re-run validation: `node validation/run-all.mjs comprehensive`

**Evidence Required**:
- Show OTEL validation output with 90+/100 score
- OR document why knowledge-hooks-api deprecated

---

### Goal 3: Fix Test Execution 🐛 → ✅

**Current**: `pnpm -r test` hangs indefinitely

**Target**: All tests run via single command

**Root Cause**: Complex root vitest.config.mjs incompatible with pnpm workspace

**Plan**:
1. Simplify root vitest.config.mjs
   - Remove complex pool options (singleFork)
   - Remove or fix setupFiles causing hang
   - Use standard defaults

2. Test iteratively:
   - `timeout 60s pnpm test` (root tests)
   - `timeout 60s pnpm -r test` (all packages)
   - Verify completion without hangs

**Evidence Required**:
- Show successful `pnpm -r test` output
- All tests complete within timeout
- No hanging processes

---

### Goal 4: Verify Performance Claims ⚠️ → ✅

**Current**: "40% faster, 60% lower memory" (not benchmarked in beta.1)

**Target**: Verified performance numbers OR remove claims

**Plan**:

Option A: Run Benchmarks
1. Find existing benchmark suite
2. Run baseline (without Oxigraph optimizations)
3. Run current (with Oxigraph)
4. Calculate actual improvement percentage
5. Update claims with real numbers

Option B: Remove Unverified Claims
1. Remove "40% faster" and "60% memory" from CHANGELOG
2. Document in RELEASE-PLAN as "theoretical improvement"
3. Add caveat: "Performance not yet benchmarked"

**Recommendation**: Option B (faster, honest)

**Evidence Required**:
- Benchmark output (if Option A)
- Updated CHANGELOG (if Option B)

---

### Goal 5: Security Audit 🔒 → ✅

**Current**: No security audit performed

**Target**: Zero critical/high vulnerabilities

**Plan**:
1. Run pnpm audit:
   ```bash
   timeout 30s pnpm audit --audit-level=high
   ```

2. Review results:
   - Critical: Must fix immediately
   - High: Must fix for beta3
   - Medium: Document and defer
   - Low: Ignore for now

3. Fix critical/high vulnerabilities:
   - Update dependencies
   - Apply patches
   - Document mitigations

**Evidence Required**:
- Show `pnpm audit` output
- Document vulnerabilities found/fixed
- Show zero critical/high remaining

---

## Beta3 Checklist

### Phase 1: Testing (1-2 days)

- [ ] Run remaining core tests (~100 more)
- [ ] Run CLI tests
- [ ] Run root test suite
- [ ] Count total tests passing
- [ ] Update CHANGELOG with verified count
- [ ] Fix pnpm -r test hanging issue
- [ ] Verify `pnpm -r test` completes successfully

### Phase 2: Validation (1 day)

- [ ] Investigate knowledge-hooks-api OTEL failure
- [ ] Fix OR mark as deprecated
- [ ] Re-run OTEL validation
- [ ] Verify score ≥90/100
- [ ] Update CHANGELOG with new score

### Phase 3: Quality & Security (1 day)

- [ ] Run pnpm audit
- [ ] Fix critical/high vulnerabilities
- [ ] Decide on performance claims (verify or remove)
- [ ] Update documentation with verified data only
- [ ] Run linter: `timeout 5s pnpm run lint`

### Phase 4: Release (1 day)

- [ ] Bump version to 5.0.0-beta.3
- [ ] Update CHANGELOG for beta.3
- [ ] Regenerate pnpm-lock.yaml
- [ ] Git commit and push
- [ ] Create release notes
- [ ] Tag release: `git tag v5.0.0-beta.3`

---

## Success Criteria

**Beta3 is ready when ALL are ✅**:

- [ ] Test count ≥250 verified passing
- [ ] OTEL validation score ≥90/100
- [ ] `pnpm -r test` completes without hanging
- [ ] Security audit shows 0 critical/high vulnerabilities
- [ ] All claims in CHANGELOG verified with evidence
- [ ] Documentation up to date
- [ ] No unverified performance claims

---

## Timeline Estimate

**Optimistic**: 3 days (if no major issues)
**Realistic**: 5 days (some debugging needed)
**Pessimistic**: 7 days (multiple issues found)

**Target Release**: 2025-12-11 (5 days from now)

---

## Risk Assessment

### Low Risk
- Running more tests (just time-consuming)
- Security audit (likely no critical issues)
- Updating documentation

### Medium Risk
- Fixing pnpm -r test hanging (may be complex)
- Running performance benchmarks (may not have baseline)

### High Risk
- Fixing knowledge-hooks-api OTEL (unknown root cause)
- Finding security vulnerabilities (requires fixes)

---

## Beta3 vs Beta2 Naming

**Question**: Should this be beta.2 or beta.3?

**Current situation**:
- We're on beta.1
- Browser package was deleted
- Some improvements made

**Options**:

1. **Call it beta.2**:
   - Pros: Natural progression (beta.1 → beta.2)
   - Cons: None really

2. **Call it beta.3**:
   - Pros: Aligns with user question
   - Cons: Skips a version number

**Recommendation**: Use **beta.2** (natural progression)

**Rationale**: beta.1 has issues, beta.2 fixes them, beta.3 for future

---

## Quick Wins (80/20)

**If time is limited, focus on these HIGH IMPACT items**:

1. ✅ **Run remaining tests** (2 hours)
   - Gets us from 190 → 250+ verified
   - Shows comprehensive coverage

2. ✅ **Fix pnpm -r test** (2-4 hours)
   - Enables CI/CD integration
   - Simplifies development workflow

3. ✅ **Security audit** (1 hour)
   - Shows no critical vulnerabilities
   - Required for production readiness

4. ⚠️ **Remove unverified performance claims** (30 min)
   - Faster than benchmarking
   - More honest

5. ⚠️ **Document knowledge-hooks-api as deprecated** (30 min)
   - Easier than fixing
   - Explains OTEL score

**Total Time**: 6-8 hours for 80% of value

---

## Deferred to Later

**NOT required for beta3, can wait**:

- Performance benchmarking (defer to beta.4 or stable)
- Fixing knowledge-hooks-api (mark deprecated instead)
- Running ALL root tests (run subset for verification)
- Additional documentation (already comprehensive)
- More packages (current 15 is sufficient)

---

**Document Version**: 1.0.0
**Created**: 2025-12-06
**Status**: 📋 PLANNING
**Next Step**: Start with Quick Wins (run tests, security audit)
