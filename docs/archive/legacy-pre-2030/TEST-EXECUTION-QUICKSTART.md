# YAWL + KGC-4D Integration - Test Execution Quickstart

## 🚨 Current Status: TESTS NOT RUN

**Blocker**: Dependencies not installed (`pnpm install` timeouts)

---

## ⚡ Quick Commands (Run These)

### 1. Install Dependencies
```bash
timeout 20s pnpm install
```
**Expected**: Success, no timeout
**Actual**: ❌ Timeouts after 10-15 seconds

### 2. Run Existing Tests
```bash
cd /home/user/unrdf/packages/yawl
timeout 5s pnpm test
```
**Expected**: 91 tests pass
**Actual**: ❓ Not run (dependencies missing)

### 3. Run New Adversarial Tests
```bash
cd /home/user/unrdf/packages/yawl
timeout 5s pnpm test integration-kgc4d.test.mjs
```
**Expected**: 17 tests pass
**Actual**: ❓ Not run (dependencies missing)

### 4. Generate Coverage Report
```bash
cd /home/user/unrdf/packages/yawl
timeout 10s pnpm test:coverage
```
**Expected**: ≥80% coverage
**Actual**: ❓ Not run (dependencies missing)

---

## 📊 Test Files

### Existing Tests (91 tests)

| File | Tests | Focus |
|------|-------|-------|
| `test/yawl-events.test.mjs` | 19 | KGC-4D event sourcing, time-travel |
| `test/receipt.test.mjs` | 34 | Cryptographic receipts, BLAKE3 |
| `test/yawl-hooks.test.mjs` | 38 | Hook execution, policy packs |
| Other files | - | Cancellation, patterns, resources, API |

### New Adversarial Tests (17 tests)

| File | Tests | Focus |
|------|-------|-------|
| `test/integration-kgc4d.test.mjs` | 17 | Round-trip, failures, performance |

**Suites**:
1. YAWL → KGC-4D → YAWL Round-Trip (4 tests)
2. KGC-4D Offline Failure Scenarios (4 tests)
3. Hook Execution Verification (3 tests)
4. Concurrent Case Execution (2 tests)
5. Performance Under Load (2 tests)

---

## ✅ What to Verify

### When Tests Run Successfully

#### 1. Integration Proof
- [ ] Events appear in KGC-4D store (`store.getEventCount()` increases)
- [ ] Time-travel reconstruction returns correct states
- [ ] Receipts have valid BLAKE3 hashes (64 chars)
- [ ] Audit trails are complete and ordered

#### 2. Hook Execution Proof
- [ ] Hook evaluator is called (spy verification)
- [ ] Receipts include justification
- [ ] SPARQL queries are captured
- [ ] Failures are handled gracefully

#### 3. Performance Proof
- [ ] Event append: < 10ms per event
- [ ] Reconstruction: < 100ms for 50 events
- [ ] Concurrent cases: 10+ simultaneous cases work

#### 4. Failure Handling Proof
- [ ] KGC-4D offline doesn't crash system
- [ ] Invalid events are rejected
- [ ] Missing fields caught by Zod
- [ ] Hook errors don't propagate

---

## 🔍 Adversarial Checklist

Before declaring "DONE", answer these:

### Did You RUN It?
- [ ] `pnpm test` executed successfully
- [ ] ALL 108 tests (91 + 17) passed
- [ ] Coverage report generated
- [ ] Performance benchmarks measured

### Can You PROVE It?
- [ ] Event count increased in KGC-4D store
- [ ] State reconstruction returned correct values
- [ ] Hashes are 64-char BLAKE3
- [ ] Hooks were executed (not just defined)

### What BREAKS?
- [ ] Tested KGC-4D unavailable
- [ ] Tested invalid event types
- [ ] Tested missing required fields
- [ ] Tested hook execution failures

### Where's the EVIDENCE?
- [ ] Full test output captured
- [ ] Coverage percentages documented
- [ ] Performance timings recorded
- [ ] Failure scenarios tested

---

## 📈 Success Criteria

### Minimum (Merge Blocker)
- ✅ 100% test pass rate (108/108)
- ✅ ≥80% statement coverage
- ✅ ≥75% branch coverage
- ✅ No unhandled errors in output

### Recommended
- ✅ Event append < 10ms average
- ✅ Reconstruction < 100ms (50 events)
- ✅ Concurrent cases (10+) work
- ✅ All failure modes tested

### Ideal
- ✅ ≥90% code coverage
- ✅ GitBackbone integration tested
- ✅ 1000+ concurrent cases tested
- ✅ Chaos engineering scenarios pass

---

## 🚀 Next Steps

### Immediate (Do Now)
1. Fix `pnpm install` timeout
2. Run ALL tests
3. Capture FULL output (not summary)
4. Verify 100% pass rate

### Short-term (This Week)
1. Generate coverage report
2. Document uncovered lines
3. Add missing tests for gaps
4. Run performance benchmarks

### Long-term (Next Sprint)
1. Add GitBackbone integration tests
2. Add distributed scenario tests
3. Add chaos engineering tests
4. Set up CI/CD pipeline

---

## 📝 Command Reference

### Test Commands
```bash
# All tests
pnpm test

# Specific file
pnpm test yawl-events.test.mjs

# Coverage
pnpm test:coverage

# Watch mode
pnpm test:watch

# Verbose
pnpm test -- --reporter=verbose
```

### Debug Commands
```bash
# Check vitest is installed
which vitest

# Check dependencies
pnpm list @unrdf/kgc-4d
pnpm list @unrdf/hooks

# Verify imports
node --eval "import('@unrdf/kgc-4d').then(console.log)"
```

---

## 🤔 Adversarial PM Questions

**Q: Are tests passing?**
**A**: ❓ Don't know - they haven't run

**Q: Is KGC-4D integration working?**
**A**: ❓ Looks good in code, but ZERO execution proof

**Q: Do hooks execute?**
**A**: ❓ Hooks are defined, but never saw them run

**Q: Is performance acceptable?**
**A**: ❓ No timing data exists

**Q: Can we ship this?**
**A**: ❌ NO - Not without running tests

---

**Last Updated**: 2025-12-25
**Status**: 🔴 BLOCKED - Dependencies not installed
**Action Required**: Fix `pnpm install`, run tests, capture output
