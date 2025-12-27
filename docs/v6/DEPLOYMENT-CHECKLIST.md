# V6 Deployment Checklist

**Version:** 1.0.0
**Last Updated:** 2025-12-27

Use this checklist for every v6 deployment to ensure safety and rollback readiness.

---

## Pre-Deployment (Day Before)

### 1. Test Rollback System

- [ ] **Test rollback system:**
  ```bash
  node scripts/test-rollback-system.mjs
  ```
  - **Expected:** All tests pass (6/6)
  - **If fails:** Fix issues before proceeding

- [ ] **Verify snapshot directory:**
  ```bash
  ls -la .rollback-snapshots/
  mkdir -p .rollback-snapshots/  # Create if missing
  ```

### 2. Run Pre-Deployment Validation

- [ ] **All tests pass:**
  ```bash
  timeout 120s pnpm test
  ```
  - **Expected:** 100% pass rate
  - **If fails:** Fix before deploying

- [ ] **Linting clean:**
  ```bash
  timeout 15s pnpm lint
  ```
  - **Expected:** 0 errors, 0 warnings
  - **If fails:** Fix all errors

- [ ] **Build successful:**
  ```bash
  timeout 30s pnpm build
  ```
  - **Expected:** All packages build
  - **If fails:** Fix build errors

- [ ] **Performance baseline:**
  ```bash
  timeout 60s pnpm benchmark:baseline
  ```
  - **Expected:** Baseline saved
  - **Use for:** Post-deploy comparison

### 3. Code Review

- [ ] **Migration guide reviewed**
  - File: `docs/v6/MIGRATION_GUIDE.md`
  - All breaking changes documented

- [ ] **Breaking changes documented**
  - File: `docs/v6/BREAKING-CHANGES.md`
  - All API changes listed

- [ ] **Dependencies reviewed**
  ```bash
  pnpm ls --depth=0 | grep -E "zod|rdf-canonize|n3"
  ```
  - **Expected:** zod 4.x, rdf-canonize 5.x, n3 1.26.x

---

## Deployment Day - Pre-Deploy

### 1. Create Snapshot (CRITICAL)

- [ ] **Create pre-deployment snapshot:**
  ```bash
  node scripts/v6-snapshot.mjs pre-v6-production-$(date +%Y%m%d-%H%M)
  ```
  - **Expected:** Snapshot created successfully
  - **Duration:** ~10 seconds
  - **NEVER deploy without this**

- [ ] **Verify snapshot:**
  ```bash
  ls -lah .rollback-snapshots/ | tail -1
  cat .rollback-snapshots/*/manifest.json | jq '.name, .timestamp'
  ```
  - **Expected:** Latest snapshot visible with correct timestamp

### 2. Test Rollback (Dry-Run)

- [ ] **Test rollback procedure:**
  ```bash
  node scripts/v6-rollback.mjs --dry-run
  ```
  - **Expected:** Shows what would be rolled back
  - **Duration:** ~30 seconds
  - **If fails:** Investigate before deploying

### 3. Communication

- [ ] **Notify team:**
  - Deployment window: _____________
  - Expected duration: ~30 minutes
  - Rollback plan: Automated, <5 min

- [ ] **Incident channel ready:**
  - Create: `#incident-v6-deploy-YYYY-MM-DD`
  - Invite: Oncall, backup, manager

---

## Deployment Execution

### 1. Deploy V6

- [ ] **Pull latest code:**
  ```bash
  git pull origin main
  git checkout v6.0.0  # Or appropriate tag
  ```

- [ ] **Install dependencies:**
  ```bash
  pnpm install --frozen-lockfile
  ```
  - **Expected:** Clean install, no errors
  - **Duration:** ~30-60 seconds

- [ ] **Run migration (if needed):**
  ```bash
  node scripts/migrate-to-v6.mjs
  ```
  - **Expected:** Migration successful
  - **If fails:** ROLLBACK IMMEDIATELY

- [ ] **Build packages:**
  ```bash
  timeout 30s pnpm build
  ```
  - **Expected:** All packages build
  - **If fails:** ROLLBACK IMMEDIATELY

### 2. Initial Validation

- [ ] **Quick test:**
  ```bash
  timeout 30s pnpm test:fast
  ```
  - **Expected:** All tests pass
  - **If >50% fail:** ROLLBACK IMMEDIATELY
  - **If 25-50% fail:** Assess, likely rollback
  - **If <25% fail:** Investigate, fix forward possible

- [ ] **Import check:**
  ```bash
  node -e "import('@unrdf/core').then(() => console.log('✓ Imports work'))"
  ```
  - **Expected:** "✓ Imports work"
  - **If fails:** ROLLBACK IMMEDIATELY

---

## Post-Deployment Validation

### 1. Comprehensive Testing

- [ ] **Full test suite:**
  ```bash
  timeout 120s pnpm test
  ```
  - **Expected:** 100% pass rate
  - **Threshold:** >95% pass = proceed, <95% = rollback

- [ ] **Linting:**
  ```bash
  timeout 15s pnpm lint
  ```
  - **Expected:** 0 errors
  - **Threshold:** <10 errors = proceed, >10 = rollback

- [ ] **Type checking:**
  ```bash
  timeout 30s pnpm lint  # Includes type checking
  ```
  - **Expected:** 0 type errors
  - **Threshold:** 0 errors required

### 2. Performance Validation

- [ ] **Run benchmarks:**
  ```bash
  timeout 60s pnpm benchmark:compare
  ```
  - **Expected:** Performance within 20% of baseline
  - **Threshold:** <20% regression = proceed, >50% = rollback

- [ ] **Memory check:**
  ```bash
  timeout 30s pnpm benchmark:memory
  ```
  - **Expected:** No memory leaks detected
  - **Threshold:** Any leak = investigate immediately

### 3. Integration Testing

- [ ] **SPARQL queries work:**
  ```bash
  node -e "
    import('@unrdf/core').then(async ({ createStore }) => {
      const store = createStore();
      console.log('✓ SPARQL integration works');
    });
  "
  ```

- [ ] **RDF parsing works:**
  ```bash
  node packages/core/examples/rdf-parsing/index.mjs
  ```
  - **Expected:** No errors, RDF parsed correctly

- [ ] **Oxigraph integration works:**
  ```bash
  node -e "
    import('@unrdf/oxigraph').then(({ createStore }) => {
      const store = createStore();
      console.log('✓ Oxigraph works');
    });
  "
  ```

---

## Monitoring (First 24 Hours)

### Hour 1: Intensive Monitoring

- [ ] **T+15min: Quick check:**
  ```bash
  timeout 30s pnpm test:fast
  ```

- [ ] **T+30min: Full check:**
  ```bash
  timeout 120s pnpm test
  timeout 60s pnpm benchmark:core
  ```

- [ ] **T+60min: Stability check:**
  - All tests still passing?
  - No new errors in logs?
  - Performance stable?

### Hour 2-4: Regular Monitoring

- [ ] **Every hour:**
  ```bash
  timeout 30s pnpm test:fast
  # Check for regressions
  ```

### Hour 4-24: Periodic Monitoring

- [ ] **Every 4 hours:**
  ```bash
  timeout 120s pnpm test
  timeout 60s pnpm benchmark:compare
  # Document any issues
  ```

---

## Rollback Triggers

**ROLLBACK IMMEDIATELY if:**

- ❌ **Test failures >50%**
- ❌ **Import errors** (Cannot find module)
- ❌ **Build failures**
- ❌ **Critical runtime errors**
- ❌ **Performance degradation >50%**

**CONSIDER ROLLBACK if:**

- ⚠️ **Test failures 25-50%**
- ⚠️ **Performance degradation 20-50%**
- ⚠️ **Error rate >10%**
- ⚠️ **Memory usage increase >2x**

**FIX FORWARD if:**

- ✓ **Test failures <25%** (and non-critical)
- ✓ **Performance degradation <20%**
- ✓ **Minor bugs** (workarounds available)

---

## Rollback Execution

### If Rollback Needed

1. **Execute rollback:**
   ```bash
   # If uncommitted changes exist
   node scripts/v6-rollback.mjs --force

   # If no uncommitted changes
   node scripts/v6-rollback.mjs
   ```

2. **Validate rollback:**
   ```bash
   # Check version
   cat package.json | grep version
   # Expected: "version": "5.x.x"

   # Check dependencies
   pnpm ls --depth=0 | grep -E "zod|rdf-canonize"
   # Expected: zod 3.x, rdf-canonize 2.x

   # Test suite
   timeout 120s pnpm test
   # Expected: 100% pass
   ```

3. **Notify team:**
   - Post in incident channel
   - Explain what happened
   - Plan next steps

4. **Post-mortem:**
   - Schedule within 48 hours
   - Document root cause
   - Create action items
   - Update deployment process

---

## Post-Deployment Tasks

### Immediate (Day 1)

- [ ] **Update documentation:**
  - Mark v6 as deployed
  - Document any issues encountered
  - Update troubleshooting guide

- [ ] **Monitor metrics:**
  - Save performance baselines
  - Document any anomalies
  - Track error rates

- [ ] **Team notification:**
  - Deployment complete
  - Any known issues
  - Next steps

### Week 1

- [ ] **Performance analysis:**
  ```bash
  pnpm benchmark:report
  ```
  - Compare to pre-deployment baseline
  - Document improvements/regressions

- [ ] **Gather feedback:**
  - Team members using v6?
  - Any issues reported?
  - Documentation clear?

- [ ] **Post-mortem (if rollback occurred):**
  - What went wrong?
  - How to prevent?
  - Update deployment process

### Week 2

- [ ] **Cleanup old snapshots:**
  ```bash
  # Keep last 10 snapshots
  cd .rollback-snapshots/
  ls -t | tail -n +11 | xargs rm -rf
  ```

- [ ] **Update runbooks:**
  - Add lessons learned
  - Update thresholds if needed
  - Improve automation

---

## Checklist Summary

**Pre-Deployment:**
- ✅ Rollback system tested
- ✅ All tests passing
- ✅ Snapshot created
- ✅ Team notified

**Deployment:**
- ✅ Code deployed
- ✅ Dependencies installed
- ✅ Initial validation passed

**Post-Deployment:**
- ✅ Full tests passing
- ✅ Performance acceptable
- ✅ Integration tests passed
- ✅ Monitoring active

**If Rollback:**
- ✅ Rollback executed
- ✅ Rollback validated
- ✅ Team notified
- ✅ Post-mortem scheduled

---

## Sign-Off

**Deployed by:** _____________
**Date:** _____________
**Time:** _____________
**Snapshot:** _____________
**Rollback tested:** YES / NO
**All tests passed:** YES / NO
**Performance validated:** YES / NO

**Notes:**
_____________________________________________________________
_____________________________________________________________
_____________________________________________________________

---

**Document Version:** 1.0.0
**Last Updated:** 2025-12-27
