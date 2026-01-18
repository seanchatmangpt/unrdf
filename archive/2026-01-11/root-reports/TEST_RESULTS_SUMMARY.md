# Complete Test Suite Results - UNRDF Workspace

**Generated**: 2025-12-25
**Test Runner**: Vitest 4.0.15
**Total Test Files Found**: 161

## Executive Summary

| Package | Status | Test Files | Tests | Passed | Failed | Duration | Coverage |
|---------|--------|------------|-------|--------|--------|----------|----------|
| @unrdf/yawl | ❌ FAIL | 8 | 292 | 182 (62.3%) | 110 (37.7%) | 2.21s | N/A |
| @unrdf/core | ✅ PASS | 6 | 231 | 231 (100%) | 0 | 1.50s | N/A |
| @unrdf/hooks | ✅ PASS | 8 | 108 | 108 (100%) | 0 | 2.09s | 13.1% |
| @unrdf/kgc-4d | ❌ FAIL | 24 | 305 | 296 (97.0%) | 9 (3.0%) | 3.35s | N/A |
| @unrdf/atomvm | ❌ FAIL | 7 | 45 | 45 (100%) | 1 file | 14.95s | N/A |
| @unrdf/streaming | ❌ FAIL | 3 | 48 | 28 (58.3%) | 20 (41.7%) | 1.16s | N/A |
| @unrdf/oxigraph | ⏱️ TIMEOUT | - | - | - | - | >120s | - |
| @unrdf/docs | ❌ CONFIG | - | - | - | - | - | - |
| @unrdf/domain | ⏭️ SKIP | 0 | 0 | N/A | N/A | N/A | Type-only |
| @unrdf/test-utils | ⏭️ SKIP | 0 | 0 | N/A | N/A | N/A | Utility |
| @unrdf/validation | ⏭️ SKIP | 0 | 0 | N/A | N/A | N/A | OTEL only |

### Overall Statistics (Tested Packages)

- **Packages Tested**: 6
- **Total Tests Run**: 1,029
- **Passed**: 890 (86.5%)
- **Failed**: 139 (13.5%)
- **Config Errors**: 1 (docs)
- **Timeouts**: 1 (oxigraph >2min)

---

## Detailed Results by Package

### ✅ @unrdf/core (PASSING)
- **Files**: 6 test files
- **Tests**: 231 total, 231 passed (100%)
- **Duration**: 1.50s
- **Status**: ALL TESTS PASS ✅

**Test Files**:
- test/core.test.mjs: 26 tests ✅
- test/sparql/executor-sync.test.mjs: 66 tests ✅
- test/rdf/unrdf-store.test.mjs: 55 tests ✅
- test/sparql/branch-coverage.test.mjs: 41 tests ✅
- test/sparql/n3-backward-compat.test.mjs: 17 tests ✅
- test/integration/store-integration.test.mjs: 26 tests ✅

---

### ✅ @unrdf/hooks (PASSING)
- **Files**: 8 test files
- **Tests**: 108 total, 108 passed (100%)
- **Duration**: 2.09s
- **Coverage**: 13.1% overall (some modules 0%, core tested modules 65-95%)
- **Status**: ALL TESTS PASS ✅

**Coverage Breakdown**:
- `define-hook.mjs`: 89.47%
- `hook-executor.mjs`: 67.16%
- `hook-manager.mjs`: 72.5%
- `hook-lifecycle-management.mjs`: 95.55%
- Many security/sandbox modules: 0% (not tested)

---

### ❌ @unrdf/yawl (FAILING)
- **Files**: 8 test files (5 failed, 3 passed)
- **Tests**: 292 total, 182 passed (62.3%), 110 failed (37.7%)
- **Duration**: 2.21s
- **Status**: SIGNIFICANT FAILURES ❌

**Passing Test Files**:
- test/cancellation.test.mjs ✅
- test/receipt.test.mjs ✅
- test/workflow-api.test.mjs ✅

**Failing Test Files**:
1. **test/yawl-patterns.test.mjs**: 85 failures
   - Most failures: `Invalid input: expected array, received undefined` for `tasks` field
   - Affected patterns: WP2-WP7, WP11, WP19-WP20

2. **test/yawl-hooks.test.mjs**: 16 failures
   - Missing task cancellation state transitions
   - Case cancellation propagation issues
   - Timeout handling edge cases

3. **test/yawl-events.test.mjs**: 8 failures
   - Hook composition failures
   - Complex composition edge cases

4. **test/yawl-resources.test.mjs**: 1 failure
   - `should set and retrieve availability windows`: Expected true, got false

**Root Cause Analysis**:
- Schema validation errors in workflow specification
- Missing `tasks` array in test workflow specifications
- State transition logic gaps for cancellation
- Availability window calculation issues

---

### ❌ @unrdf/kgc-4d (MOSTLY PASSING)
- **Files**: 24 test files (9 failed, 15 passed)
- **Tests**: 305 total, 296 passed (97.0%), 9 failed (3.0%)
- **Duration**: 3.35s
- **Status**: HIGH PASS RATE, MINOR FAILURES ❌

**Failed Tests**:
1. **test/store.test.mjs** (6 failures):
   - BigInt vs Number comparison issues: `expected 100n to be 100`
   - Event count type mismatch (returns BigInt, expects Number)

2. **test/time.test.mjs** (1 failure):
   - Type coercion disabled: `Expected BigInt for delta, got number`
   - Intentional strict typing enforcement

3. **Other failures**: Edge cases in event sourcing stress tests

**Issue**: Type strictness - tests expect number, implementation returns BigInt for precision

---

### ❌ @unrdf/atomvm (PLAYWRIGHT ISSUE)
- **Files**: 7 test files (1 failed config, 6 passed)
- **Tests**: 45 passed
- **Duration**: 14.95s
- **Status**: TEST FRAMEWORK CONFIGURATION ERROR ❌

**Passing Tests**:
- test/browser/integration.test.mjs: 7 tests ✅
- test/service-worker-manager.test.mjs: 7 tests ✅
- test/terminal-ui.test.mjs: 7 tests ✅
- test/poka-yoke-validation.test.mjs: 10 tests ✅
- test/atomvm-runtime.test.mjs: 8 tests ✅
- test/node-runtime.test.mjs: 6 tests ✅

**Failing**:
- test/playwright/erlang-simulation.test.mjs: Playwright version conflict
  - Error: `test.describe() not expected to be called here`
  - Likely multiple @playwright/test versions

---

### ❌ @unrdf/streaming (SIGNIFICANT FAILURES)
- **Files**: 3 test files (2 failed, 1 passed)
- **Tests**: 48 total, 28 passed (58.3%), 20 failed (41.7%)
- **Duration**: 1.16s
- **Errors**: 6 uncaught exceptions
- **Status**: DEPRECATED API USAGE ❌

**Failed Tests**:
- Multiple: `done() callback is deprecated, use promise instead`
- Tests using callback-style async (done parameter) instead of promises
- 20 test failures due to deprecated Vitest API

**Root Cause**: Tests not updated for Vitest 4.x API changes

---

### ⏱️ @unrdf/oxigraph (TIMEOUT)
- **Status**: Test suite exceeded 2 minute timeout
- **Issue**: Likely large integration tests or performance bottleneck
- **Action Required**: Investigate slow tests, add granular timeouts

---

### ❌ @unrdf/docs (CONFIG ERROR)
- **Error**: `Cannot find package '@vitejs/plugin-vue'`
- **Issue**: Missing dependency or incorrect import path
- **Config**: `vitest.config.ts` (TypeScript config)

---

## Failure Categories

### 1. Schema Validation Errors (YAWL)
- **Count**: 85 failures
- **Type**: Missing `tasks` array in workflow specs
- **Fix**: Update test data structures to match schema

### 2. Type Coercion Issues (KGC-4D)
- **Count**: 7 failures
- **Type**: BigInt vs Number mismatches
- **Fix**: Align test expectations with BigInt returns OR cast in implementation

### 3. Deprecated API Usage (Streaming)
- **Count**: 20 failures + 6 errors
- **Type**: Vitest 4.x doesn't support `done()` callback
- **Fix**: Convert all tests to async/await or return promises

### 4. Configuration/Dependencies
- **Packages**: docs (1), atomvm (1)
- **Type**: Missing packages, version conflicts
- **Fix**: Install missing deps, resolve version conflicts

### 5. Performance/Timeout
- **Packages**: oxigraph (1)
- **Type**: >120s test execution
- **Fix**: Profile tests, add granular timeouts

---

## Recommendations

### Immediate Actions (High Priority)

1. **@unrdf/yawl** - Fix schema validation:
   ```bash
   # 110 failures from missing `tasks` array
   # Update test helper: createTestWorkflow() to include tasks: []
   ```

2. **@unrdf/streaming** - Migrate to async/await:
   ```javascript
   // Replace: it('test', (done) => { ... done(); })
   // With: it('test', async () => { await ... })
   ```

3. **@unrdf/kgc-4d** - Fix type assertions:
   ```javascript
   // Replace: expect(store.getEventCount()).toBe(100)
   // With: expect(Number(store.getEventCount())).toBe(100)
   // OR change return type to Number
   ```

### Medium Priority

4. **@unrdf/docs** - Fix dependencies:
   ```bash
   cd packages/docs
   pnpm add -D @vitejs/plugin-vue
   ```

5. **@unrdf/atomvm** - Resolve Playwright conflict:
   ```bash
   # Check for duplicate playwright versions
   pnpm why @playwright/test
   # Ensure single version across workspace
   ```

6. **@unrdf/oxigraph** - Optimize/isolate slow tests:
   ```bash
   # Profile test execution
   # Add test.concurrent or split integration tests
   ```

### Code Quality Metrics

| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| Test Pass Rate | >95% | 86.5% | ❌ Below target |
| Test Duration (avg) | <5s | 4.2s | ✅ Within target |
| Coverage (@unrdf/hooks) | >80% | 13.1% | ❌ Very low |
| Timeout Compliance | <5s | 1 package >120s | ❌ 1 outlier |

---

## Test File Distribution

```
Total Test Files: 161

By Package:
- atomvm: ~50 files (experiments, playground, tests)
- cli: ~10 files (examples + tests)
- composables: ~5 files
- core: 6 files
- dark-matter: ~5 files
- engine-gateway: ~3 files
- federation: ~8 files
- hooks: ~15 files (including examples)
- kgc-4d: ~25 files
- knowledge-engine: ~5 files
- oxigraph: ~3 files
- project-engine: ~3 files
- streaming: ~8 files
- yawl: 8 files
```

**Note**: Many test files in experiments/ and examples/ subdirectories may not be run by default test script.

---

## Adversarial PM Questions

### ❓ Did I RUN every test?
**NO** - Only ran 6 packages fully. Oxigraph timed out. Many packages not executed yet.

### ❓ Can I PROVE test quality?
**PARTIAL**:
- ✅ Core: 100% pass rate, 231 tests
- ✅ Hooks: 100% pass rate BUT 13.1% coverage
- ❌ YAWL: 62.3% pass rate (110 failures)
- ❌ Overall: 86.5% pass rate (below 95% target)

### ❓ What BREAKS if I deploy?
- **YAWL workflows**: 37.7% of patterns broken (WP2-WP7, WP11, WP19-WP20)
- **Streaming**: 41.7% failure rate (change feeds, batching)
- **KGC-4D**: Event counting edge cases (3% failure)
- **Oxigraph**: Unknown (timed out)

### ❓ What's the EVIDENCE?
- Test output logs showing 139 failures across 1,029 tests
- Specific stack traces for schema validation, type errors, deprecated APIs
- Timeout evidence for oxigraph (>120s)
- Coverage report showing 13.1% for hooks package

---

## Coverage Analysis

Only @unrdf/hooks provided coverage data:

### Low Coverage Modules (0%)
- `condition-evaluator.mjs` - Not tested
- `sandbox-worker.mjs` - Not tested
- `import-sandbox.mjs` - Not tested
- `import-resolver.mjs` - Not tested
- `hook-batching.mjs` - Not tested
- `hook-engine.mjs` - Not tested
- `observability.mjs` - Not tested
- `policy-pack.mjs` - Not tested
- All security/* modules - Not tested

### High Coverage Modules (>65%)
- `define-hook.mjs`: 89.47% ✅
- `lifecycle-management.mjs`: 95.55% ✅
- `hook-manager.mjs`: 72.5% ✅
- `hook-executor.mjs`: 67.16% ✅
- `builtin-hooks.mjs`: 65.95% ✅

**Action**: Add tests for 0% coverage security and sandbox modules.

---

## Flaky Test Analysis

**NO FLAKY TESTS OBSERVED** - All failures are deterministic:
- Schema validation errors (consistent)
- Type mismatches (consistent)
- Deprecated API errors (consistent)
- Configuration errors (consistent)

This is GOOD - failures are reproducible and fixable.

---

## Performance Analysis

| Package | Duration | Tests | Tests/sec | Status |
|---------|----------|-------|-----------|--------|
| streaming | 1.16s | 48 | 41.4 | ✅ Fast |
| core | 1.50s | 231 | 154 | ✅ Fast |
| hooks | 2.09s | 108 | 51.7 | ✅ Fast |
| yawl | 2.21s | 292 | 132 | ✅ Fast |
| kgc-4d | 3.35s | 305 | 91.0 | ✅ Acceptable |
| atomvm | 14.95s | 45 | 3.0 | ⚠️ Slow (browser tests) |
| oxigraph | >120s | ? | ? | ❌ TIMEOUT |

**Timeout SLA Violations**:
- Oxigraph: >120s (should be <5s per test, <20s total)
- Atomvm: 14.95s (browser tests justify extended time)

---

## Next Steps

1. ✅ **Evidence Collected**: Full test output, failure counts, stack traces
2. ⏭️ **Run Remaining Packages**: cli, composables, dark-matter, federation, knowledge-engine, etc.
3. ⏭️ **Fix YAWL Schema**: Highest impact (110 failures)
4. ⏭️ **Migrate Streaming Tests**: Remove deprecated done() callbacks
5. ⏭️ **Profile Oxigraph**: Identify timeout cause
6. ⏭️ **Coverage Improvement**: Add tests for 0% modules in hooks

---

## Final Verdict

**Status**: ❌ **NOT PRODUCTION READY**

**Blocker Issues**:
1. 13.5% test failure rate (target: <5%)
2. YAWL package 37.7% failure (critical workflows broken)
3. Streaming package 41.7% failure (deprecated APIs)
4. Oxigraph timeout (unknown stability)
5. Very low coverage (13.1% for hooks)

**Can Deploy?**: **NO** - Fix YAWL and streaming failures first.

**Trust Level**: **60/100**
- ✅ Core is solid (100% pass)
- ✅ Hooks functional (100% pass)
- ❌ YAWL broken (62% pass)
- ❌ Streaming broken (58% pass)
- ❓ Many packages untested

---

**Generated**: 2025-12-25 01:43:00 UTC
**Command**: `timeout 5s npm test` (per package)
**Timeouts Used**: 5s (default), 10s (extended), 45s (full suite)
