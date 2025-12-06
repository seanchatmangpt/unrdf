# v5.0.0-beta.3 Requirements - Final Plan

**Date**: 2025-12-06
**Previous Version**: 5.0.0-beta.2
**Status**: PLANNING
**Branch**: TBD (new branch for beta.3 work)

---

## Executive Summary

Beta.2 achieved significant quality improvements (276 tests, 83/100 OTEL, 0 critical vulnerabilities). Beta.3 should focus on **production readiness** and **final polish**:

### Critical Goals (Must Have)

1. ✅ **Fix pnpm -r test** - Enable CI/CD integration
2. ✅ **Regenerate lockfile** - Clean dependency resolution
3. ✅ **Verify all tests pass** - Full test suite validation
4. ✅ **Create beta.3 release** - Tagged and documented

### Optional Goals (Nice to Have)

5. ⚠️ **Performance benchmarking** - Verify "40% faster, 60% memory" claims
6. ⚠️ **Additional test coverage** - Root test suite execution
7. ⚠️ **Fix knowledge-hooks-api** - Bring OTEL to 100/100

---

## What Beta.2 Accomplished

### ✅ Completed

- **Testing**: 190 → 276 tests (+86 tests, 45% increase)
- **Security**: 0 critical/high vulnerabilities
- **Documentation**: Added DEPRECATED.md, V5-BETA2-RELEASE-NOTES.md
- **Infrastructure**: Simplified vitest.config.mjs (344 → 74 lines)
- **OTEL**: Maintained 83/100 (documented deprecated features)
- **Branch**: Reconciled with main (merged Nextra/Docusaurus changes)

### ⚠️ Partially Fixed

- **pnpm -r test**: No longer hangs on root, but core package still hangs
- **Test infrastructure**: Simplified config, fixed packages without tests

### ❌ Not Done (Deferred to Beta.3)

- Performance benchmarking (claims unverified)
- Full test execution via single command
- Root test suite execution
- Knowledge hooks OTEL fix (marked deprecated instead)

---

## Beta.3 Critical Path

### Phase 1: Fix Test Execution (Priority: CRITICAL)

**Goal**: Enable `pnpm -r test` to complete successfully

**Root Cause**: packages/core has test interference when running all 252 tests together

**Action Plan**:

1. **Investigate core package hanging** (2-4 hours)
   ```bash
   # Try different approaches:
   cd packages/core

   # Option A: Disable coverage
   npx vitest run --no-coverage --reporter=verbose

   # Option B: Sequential execution
   npx vitest run --no-coverage --pool=forks --poolOptions.forks.singleFork=true

   # Option C: Increase timeout
   npx vitest run --no-coverage --testTimeout=60000

   # Option D: Run in batches
   npx vitest run --no-coverage test/adversarial.test.mjs test/core.test.mjs
   npx vitest run --no-coverage test/rdf/*.test.mjs
   npx vitest run --no-coverage test/sparql/*.test.mjs
   npx vitest run --no-coverage test/integration/*.test.mjs
   ```

2. **Update core package.json** (30 min)
   - Change test script based on what works
   - Document approach in TESTING.md

3. **Verify pnpm -r test completes** (30 min)
   ```bash
   timeout 180s pnpm -r test
   # Should complete without hanging
   ```

**Success Criteria**:
- `pnpm -r test` completes within 3 minutes
- All packages report results (pass or skip)
- No hanging processes

**Evidence Required**:
- Full pnpm -r test output showing completion
- Test count matches individual execution

---

### Phase 2: Dependency Cleanup (Priority: HIGH)

**Goal**: Clean lockfile and resolve dependency conflicts

**Action Plan**:

1. **Remove pnpm-lock.yaml** (5 min)
   ```bash
   rm pnpm-lock.yaml
   ```

2. **Regenerate with fresh install** (5-10 min)
   ```bash
   pnpm install
   # Verify no errors
   ```

3. **Run security audit** (5 min)
   ```bash
   pnpm audit --audit-level=high
   # Should still show 0 critical/high
   ```

4. **Test all packages** (10 min)
   ```bash
   # Try the fixed pnpm -r test
   pnpm -r test
   ```

**Success Criteria**:
- Clean lockfile generation
- No dependency conflicts
- Security audit still passes

---

### Phase 3: Documentation & Release (Priority: MEDIUM)

**Goal**: Prepare beta.3 release with complete documentation

**Action Plan**:

1. **Update CHANGELOG.md** (30 min)
   - Add beta.3 section
   - Document what's new vs beta.2
   - Update test counts if changed
   - Note pnpm -r test fix

2. **Create V5-BETA3-RELEASE-NOTES.md** (1 hour)
   - Summary of changes
   - Migration guide (if needed)
   - Known issues
   - Success criteria met

3. **Update TESTING.md** (30 min)
   - Remove "hangs" warnings if fixed
   - Update quick start guide
   - Add pnpm -r test as recommended approach

4. **Update package versions** (30 min)
   ```bash
   # Update all package.json files
   # version: "5.0.0-beta.2" → "5.0.0-beta.3"
   ```

5. **Commit and tag release** (15 min)
   ```bash
   git add -A
   git commit -m "feat: v5.0.0-beta.3 - production-ready test infrastructure"
   git push
   git tag v5.0.0-beta.3
   git push --tags
   ```

**Success Criteria**:
- All documentation updated
- Version bumped to beta.3
- Git tag created

---

## Optional Enhancements (80/20)

### Enhancement 1: Performance Benchmarking

**Status**: OPTIONAL (defer if time-constrained)

**Why**: Claims "40% faster, 60% lower memory" are currently unverified

**Action Plan**:

1. **Find or create baseline benchmark** (1-2 hours)
   - Check if oxigraph-performance.test.mjs has baseline
   - Or create simple SPARQL query benchmark

2. **Run with N3.js baseline** (30 min)
   - Temporarily switch back to N3
   - Run benchmark suite
   - Record results

3. **Run with Oxigraph** (30 min)
   - Switch to current Oxigraph implementation
   - Run same benchmark suite
   - Record results

4. **Calculate actual improvement** (15 min)
   - Compare query speed
   - Compare memory usage
   - Update claims with real numbers

**Alternative**: Remove unverified claims from CHANGELOG (15 min)
- Change "40% faster" to "Optimized with Oxigraph Rust backend"
- Change "60% lower memory" to "Reduced memory footprint"
- Add note: "Performance improvements not yet benchmarked"

**Recommendation**: Use alternative (faster, more honest)

---

### Enhancement 2: Root Test Suite

**Status**: OPTIONAL (low priority)

**Why**: Root test/ directory has ~15+ test files not yet run

**Action Plan**:

1. **Audit root tests** (30 min)
   ```bash
   find test -name "*.test.mjs" -type f | grep -v setup
   # List all root test files
   ```

2. **Run individually** (1-2 hours)
   - Try each test file
   - Count passing tests
   - Document failures

3. **Update test count** (15 min)
   - Add to total verified count
   - Update CHANGELOG and TESTING.md

**Alternative**: Document as "not included in beta.3 scope" (5 min)

**Recommendation**: Defer to later release

---

### Enhancement 3: Knowledge Hooks OTEL Fix

**Status**: OPTIONAL (low priority - already deprecated)

**Why**: Would bring OTEL score from 83/100 to 100/100

**Action Plan**:

1. **Investigate span generation** (1-2 hours)
   - Check why hooks don't generate OTEL spans
   - Identify where TracerProvider should be initialized

2. **Add OTEL instrumentation** (2-3 hours)
   - Update hooks implementation
   - Add span generation to hook lifecycle

3. **Test validation** (30 min)
   ```bash
   node validation/run-all.mjs comprehensive
   # Should now show 100/100
   ```

**Alternative**: Keep as deprecated (0 min)

**Recommendation**: Keep deprecated, fix in future release if hooks become critical

---

## Timeline Estimate

### Optimistic (1 day)
- Fix pnpm -r test: 3 hours
- Regenerate lockfile: 30 min
- Documentation: 2 hours
- Release: 30 min
- **Total**: 6 hours

### Realistic (2 days)
- Fix pnpm -r test: 4 hours (includes debugging)
- Regenerate lockfile: 1 hour (includes testing)
- Documentation: 3 hours (thorough)
- Release: 1 hour (includes verification)
- **Total**: 9 hours (1.5 days)

### Pessimistic (3 days)
- Fix pnpm -r test: 6 hours (complex debugging)
- Regenerate lockfile: 2 hours (conflicts)
- Documentation: 4 hours (comprehensive)
- Release: 2 hours (issues found)
- **Total**: 14 hours (2 days)

**Target Release**: 2025-12-09 (3 days from now)

---

## Success Criteria

**Beta.3 is ready when ALL are ✅**:

### Critical
- [ ] `pnpm -r test` completes without hanging
- [ ] All 276+ tests passing
- [ ] pnpm-lock.yaml regenerated cleanly
- [ ] Security audit: 0 critical/high vulnerabilities
- [ ] All documentation updated (CHANGELOG, TESTING, release notes)
- [ ] Version bumped to 5.0.0-beta.3
- [ ] Git tag created: v5.0.0-beta.3

### Optional
- [ ] Performance benchmarks run (or claims removed)
- [ ] Root test suite executed
- [ ] Knowledge hooks OTEL fixed (or kept deprecated)

---

## Risk Assessment

### Low Risk ✅
- Regenerating lockfile (standard operation)
- Documentation updates (no code changes)
- Version bumping (automated)

### Medium Risk ⚠️
- Fixing pnpm -r test (may require trial and error)
- Merge conflicts (if main has new changes)

### High Risk ❌
- None identified

---

## Pre-Flight Checklist

**Before starting beta.3 work**:

- [ ] Verify beta.2 branch merged to main (or ready to merge)
- [ ] Create new branch for beta.3: `claude/v5-beta3-final-production-ready`
- [ ] Ensure working directory is clean
- [ ] Verify all beta.2 commits pushed
- [ ] Review beta.2 feedback (if any)

**During beta.3 work**:

- [ ] Follow Adversarial PM methodology (prove all claims)
- [ ] Run commands, capture output, verify results
- [ ] Document workarounds if full fix not possible
- [ ] Commit incrementally with clear messages
- [ ] Test after each major change

**Before releasing beta.3**:

- [ ] All tests pass individually
- [ ] pnpm -r test completes (or documented workaround)
- [ ] Security audit clean
- [ ] Documentation comprehensive
- [ ] Version numbers correct
- [ ] CHANGELOG accurate

---

## Deferred to Future Releases

**Not required for beta.3**:

1. **Performance Benchmarking** → defer to v5.0.0-rc.1
2. **Root Test Suite** → defer to v5.0.0-rc.1
3. **Knowledge Hooks OTEL** → defer to v5.1.0 (or remove)
4. **Browser Package** → removed, recoverable from git if needed
5. **Additional SPARQL tests** → defer to v5.0.0-rc.1

**Rationale**: Beta.3 focuses on production readiness (test infrastructure, clean dependencies, documentation). Feature additions and optimizations can wait for RC or stable release.

---

## Beta.3 vs Beta.2 Comparison

| Metric | Beta.2 | Beta.3 (Goal) | Change |
|--------|--------|---------------|--------|
| Tests Verified | 276 | 276+ | Maintain or increase |
| OTEL Score | 83/100 | 83/100 | Maintain (deprecated documented) |
| pnpm -r test | Partial (hangs on core) | ✅ Fixed | Critical improvement |
| Security | 0 critical/high | 0 critical/high | Maintain |
| Lockfile | Conflicts from merge | Clean regeneration | Quality improvement |
| Documentation | Comprehensive | Production-ready | Polish |
| Release Status | Beta testing | Production-ready | Final beta |

---

## Post-Beta.3 Path

**If beta.3 succeeds** → Release Candidate (v5.0.0-rc.1)
- Focus on performance verification
- Run full test suite including root tests
- Comprehensive benchmarking
- External beta testing

**If beta.3 has issues** → Beta.4 (bug fixes)
- Address any critical bugs found
- Iterate on test infrastructure
- Maintain quality standards

---

**Document Version**: 1.0.0
**Created**: 2025-12-06
**Author**: Claude (Evidence-Based Planning)
**Status**: 📋 READY TO EXECUTE

**Next Step**: Create branch `claude/v5-beta3-final-production-ready` and start Phase 1 (Fix Test Execution)
