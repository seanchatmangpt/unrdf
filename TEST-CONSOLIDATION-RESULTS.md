# Test Consolidation Results

**Report Generated**: 2026-01-11
**Branch**: claude/consolidate-tests-Fssji
**Configuration**: vitest.config.fast.mjs (80/20 fast suite)

---

## Executive Summary

**SLA Status**: CRITICAL FAILURE

The consolidated 80/20 fast test suite does NOT meet the 5-second SLA requirement.

| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| **Total Execution Time** | <5s | 10.75s | **FAIL** |
| **Test Execution Time** | <5s | 18.59s | **FAIL** |
| **Test Files** | 17 passed | 6 passed, 11 failed | **FAIL** |
| **Individual Tests** | 35 passed | 26 passed, 9 failed | **FAIL** |
| **Pass Rate** | 100% | 74.3% | **FAIL** |

---

## Execution Stats

### Time Breakdown (10.75s total)
```
Transform    : 7.38s  (68.7% of runtime)
Setup        : 1.69s  (15.7% of runtime)
Import       : 3.23s  (30.0% of runtime)
Tests        : 18.59s (172.9% of total - with retries)
Environment  : 2ms    (negligible)
───────────────────────
TOTAL        : 10.75s (2.15x SLA - OVER BUDGET)
```

### Test Results Summary
- **Test Files Configured**: 17
- **Test Files Failed**: 11 (64.7%)
- **Test Files Passed**: 6 (35.3%)
- **Individual Tests**: 35 total
- **Tests Passed**: 26 (74.3%)
- **Tests Failed**: 9 (25.7%)
- **Retry Count**: 10+ (due to failures)

---

## Failed Test Files (11/17 = 64.7%)

### 1. Module Not Found Errors (5 files)

#### `test/diff.test.mjs`
**Error**: Cannot find module '../packages/diff.mjs'
**Status**: BLOCKED - Module missing
**Action**: Diff engine module needs to be created or path corrected

#### `test/project-engine.test.mjs`
**Error**: Cannot find module '../packages/project-engine/index.mjs'
**Status**: BLOCKED - Module missing
**Impact**: Domain inference tests cannot run
**Action**: Create project-engine package or stub module

#### `test/hook-executor-deps.test.mjs`
**Error**: Cannot find module '../packages/knowledge-engine/hook-executor.mjs'
**Status**: BLOCKED - Module missing
**Impact**: Hook dependency validation tests blocked
**Action**: Verify knowledge-engine module structure

#### `test/knowledge-engine/utils/circuit-breaker.test.mjs`
**Error**: Cannot find module '../../../packages/knowledge-engine/utils/circuit-breaker.mjs'
**Status**: BLOCKED - Module missing
**Action**: Ensure circuit-breaker utility exists

#### `test/knowledge-engine/utils/ring-buffer.test.mjs`
**Error**: Cannot find module '../../../packages/knowledge-engine/utils/ring-buffer.mjs'
**Status**: BLOCKED - Module missing
**Action**: Ensure ring-buffer utility exists

### 2. Timeout Failures (2 files, 3 tests)

#### `test/knowledge-engine/parse-contract.test.mjs`
**Test**: "should parse valid TTL and return store"
**Error**: Test timed out in 2000ms (Vitest default)
**Timeout**: 8044ms (exceeded limit by 4.04x)
**Issue**: TTL parsing initialization too slow
**Root Cause**: Possible missing dependency injection or slow initialization
**Action**:
- Increase timeout to 5000ms OR
- Optimize initialization logic
- Check for synchronous I/O blocking

#### `test/knowledge-engine/query-contract.test.mjs`
**Tests**: 2 failures
1. "should execute SELECT query and return results array"
   - Error: Test timed out in 2000ms
   - Timeout: 8041ms (exceeded by 4.02x)

2. "should return different results for specific queries"
   - Error: Parse failure - "await isn't allowed in non-async function"
   - Location: `/src/knowledge-engine/knowledge-substrate-core.mjs:87:17`
   - **CRITICAL**: Constructor using await (not async)

**Root Cause**: Syntax error in knowledge-substrate-core.mjs (await in sync function)

### 3. Assertion Failures (3 files, 4 tests)

#### `test/lockchain-merkle-verification.test.mjs`
**Tests**: 2 failures (both retried once)

1. "should calculate 64-char hex hash"
   - Expected: `/^[0-9a-f]{64}$/`
   - Received: `"a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6"` (63 chars)
   - Issue: Hash calculation returns 63 chars instead of 64

2. "should validate hash format"
   - Expected: `true`
   - Received: `false`
   - Issue: Hash validation rejects valid hashes

#### `test/security-error-sanitizer.test.mjs`
**Tests**: 2 failures (both retried once)

1. "redacts API keys from error messages"
   - Expected: Pattern NOT to match `/sk_live_/`
   - Received: `"Error: Invalid API_KEY=sk_live_51234567890abcdefghij"`
   - Issue: API key not being sanitized

2. "redacts home directory paths"
   - Expected: To contain `"[PATH]"`
   - Received: `"Error in /[HOME]/project/src/app.js:4…"`
   - Issue: Path redaction not working correctly

#### `packages/v6-compat/test/batch-1-validation.test.mjs`
**Tests**: 2 failures (both retried once)

1. "validates createQuad params (4 required params)"
   - Expected: `false`
   - Received: `true`
   - Issue: Schema validation accepting invalid params

2. "validates schema-generator schemas"
   - Expected: `false`
   - Received: `true`
   - Issue: Schema validation acceptance too permissive

---

## Passed Tests (6 files, 26 tests)

### Quick Wins (Tests that work)

#### `test/cli.test.mjs` ✓
- 6 tests PASSED
- Total time: 6.043s (suite, with 6 sub-tests)
- Individual test times:
  - CLI validate: 2.43ms
  - CLI propose: 0.44ms
  - CLI admit: 0.44ms
  - CLI project: 0.61ms
  - CLI run: 0.49ms
  - CLI unknown command: 0.35ms
- **Status**: RELIABLE - All tests pass consistently

#### `test/dark-matter-80-20.test.mjs` ✓
- 3 tests PASSED
- Times: 4ms, 1ms, 1ms
- **Status**: FAST - Excellent performance

#### `test/e2e-integration.test.mjs` ✓
- 1 test PASSED (2ms)
- **Status**: FAST - Smoke test working

#### `packages/v6-compat/test/integration.test.mjs` ✓
- 13 tests PASSED
- Suite time: 14.55ms (fast)
- **Status**: RELIABLE - Migration compatibility verified

#### `packages/v6-compat/test/integration-node.test.mjs` ✓
- 6 tests PASSED
- **Status**: RELIABLE - Node.js compatibility confirmed

#### `packages/v6-compat/test/batch-1-validation.test.mjs` (partial)
- 3 tests PASSED
- 2 tests FAILED (see failures section)
- **Status**: MIXED - Schema validation issues

#### `test/receipts.test.mjs` ✓
- 7 tests PASSED (TAP format)
- Times range: 0.3ms to 13.95ms
- **Status**: RELIABLE - Receipt operations working

---

## Coverage Analysis

### Current Test Structure

**Configured Test Files**: 17
- TIER 1 (Essential 20%): 7 files
- TIER 2 (Important 30%): 10 files

**Test Organization**:
- `test/` directory: 14 test files (multiple failures)
- `packages/v6-compat/test/`: 2 test files (partial failures)
- `packages/hooks/test/`: Not included (missing)
- `packages/yawl/test/`: 1 file (missing module reference)
- Other packages: Not configured

### Coverage Metrics (Cannot measure due to failures)

| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| Lines | 80% | N/A | NOT MEASURED |
| Functions | 80% | N/A | NOT MEASURED |
| Branches | 80% | N/A | NOT MEASURED |
| Statements | 80% | N/A | NOT MEASURED |

**Reason**: Coverage reporting disabled due to 64.7% test file failure rate

---

## Critical Issues (Blocking SLA)

### Issue #1: Module Not Found Errors (5 tests blocked)
**Severity**: CRITICAL
**Impact**: 29.4% of configured tests cannot run
**Causes**:
- `packages/diff.mjs` does not exist
- `packages/project-engine/` package structure broken
- `packages/knowledge-engine/hook-executor.mjs` missing
- `packages/knowledge-engine/utils/` missing utilities

**Evidence**:
```
Error: Cannot find module '../packages/diff.mjs'
Error: Cannot find module '../packages/project-engine/index.mjs'
Error: Cannot find module '../packages/knowledge-engine/hook-executor.mjs'
```

### Issue #2: Timeout SLA Violations (2 test files)
**Severity**: CRITICAL
**Impact**: Parse and query contract tests timing out
**Current**: 8000+ ms (4x timeout)
**Configured**: 2000ms
**Root Cause**:
- TTL parsing initialization too slow
- Possible missing module imports
- Synchronous I/O blocking

**Evidence**:
```
Test timed out in 2000ms
Duration: 8041ms for parse-contract.test.mjs
Duration: 8044ms for query-contract.test.mjs
```

### Issue #3: Syntax Error in Core Module
**Severity**: CRITICAL
**Location**: `/src/knowledge-engine/knowledge-substrate-core.mjs:87:17`
**Error**: `await` in non-async function (constructor)
**Impact**: Blocks query contract test compilation
**Code**:
```javascript
constructor() {
  // ...
  this.store = await createStore();  // ERROR: await in sync constructor
}
```

**Fix Required**: Make constructor async or refactor initialization

### Issue #4: Test Execution Time Exceeds SLA by 2.15x
**Severity**: CRITICAL
**Current**: 10.75s
**Target**: 5.00s
**Over Budget**: 5.75s (115% overage)
**Breakdown**:
- Transform (68.7%): 7.38s
- Setup (15.7%): 1.69s
- Import (30.0%): 3.23s
- Tests (172.9%): 18.59s (with retries)

**Recommendation**:
1. Fix module not found errors (will reduce test retries)
2. Fix timeout issues (will reduce test execution time)
3. Consider splitting suite into sub-tiers

---

## Per-Test Timing Details

### Fast Tests (< 5ms) - 23 tests
```
dark-matter-80-20.test.mjs:
  - System Initialization: 4ms
  - High Value Delivery: 1ms
  - System Cleanup: 1ms

CLI Tests (filtered):
  - propose: 0.44ms
  - admit: 0.44ms
  - project: 0.61ms
  - run: 0.49ms
  - unknown command: 0.35ms

Receipts:
  - Delta Validation (Receipt Schema): 0.36ms
  - Delta Validation (Context Schema): 0.31ms
  - Contract Basics (Valid Format): 0.37ms
  - Schema export matching: 0.00ms
```

### Medium Tests (5-100ms) - 10 tests
```
v6-compat batch-1-validation.test.mjs:
  - imports adapters.schema: 439ms
  - imports types.schema: 115ms
  - validates createNamedNode: 2ms
  - validates createLiteral: 1ms

receipts.test.mjs:
  - Receipt Creation (Basic): 13.95ms
  - Receipt Creation (Chain): 1.08ms
  - Contract Basics (Invalid Hash): 1.04ms
  - Smoke Test (E2E): 1.79ms

v6-compat integration-node.test.mjs:
  - createStore adapter: 9.60ms
  - MigrationTracker (directory scan): 231ms
```

### Slow Tests (>1000ms) - 2 tests FAILED
```
parse-contract.test.mjs: 8041ms (TIMEOUT - 4x limit)
query-contract.test.mjs: 8044ms (TIMEOUT - 4x limit)
```

---

## Recommendations

### Priority 1: CRITICAL FIXES (Required for SLA)

1. **Fix Module References**
   - [ ] Create `/home/user/unrdf/packages/diff.mjs` or update import paths
   - [ ] Create `/home/user/unrdf/packages/project-engine/index.mjs`
   - [ ] Create `/home/user/unrdf/packages/knowledge-engine/hook-executor.mjs`
   - [ ] Create `/home/user/unrdf/packages/knowledge-engine/utils/circuit-breaker.mjs`
   - [ ] Create `/home/user/unrdf/packages/knowledge-engine/utils/ring-buffer.mjs`
   - **Impact**: Will unlock 5 blocked tests (29% of suite)

2. **Fix Syntax Error in knowledge-substrate-core.mjs**
   - [ ] Make constructor async OR refactor initialization
   - [ ] Validate TTL parsing no longer has await in sync function
   - [ ] Re-test query-contract.test.mjs
   - **Impact**: Unblocks query contract test compilation

3. **Optimize Timeout Violations**
   - [ ] Increase testTimeout from 2000ms to 5000ms (vitest.config.fast.mjs:23)
   - [ ] OR refactor TTL parsing for faster initialization
   - [ ] Validate parse-contract and query-contract tests complete <5s
   - **Impact**: Tests can complete instead of timing out

4. **Fix Assertion Failures**
   - [ ] Fix hash calculation (63 chars vs 64 required) in lockchain-merkle
   - [ ] Fix error sanitization for API keys and paths
   - [ ] Fix schema validation permissiveness in v6-compat
   - **Impact**: 4 test assertions will pass

### Priority 2: PERFORMANCE OPTIMIZATION (Optional)

1. **Reduce Transform Time (7.38s = 68.7%)**
   - Profile Vite transform pipeline
   - Consider lazy-loading heavy dependencies
   - Target: <3s

2. **Reduce Setup Time (1.69s = 15.7%)**
   - Optimize cleanup hooks (test/setup/cleanup-hooks.mjs)
   - Target: <0.5s

3. **Reduce Import Time (3.23s = 30%)**
   - Profile import chain
   - Consider dynamic imports for heavy modules
   - Target: <1s

### Priority 3: STRUCTURAL IMPROVEMENTS (Post-SLA)

1. **Split Test Suite Further**
   - Tier 1 (Critical): 5-7 tests, <2s
   - Tier 2 (Important): 15-20 tests, <5s
   - Tier 3 (Nice): Everything else
   - Run Tier 1+2 in pre-push hook

2. **Remove Slow Schema Imports**
   - v6-compat batch-1-validation takes 439ms for one schema load
   - Consider lazy-loading or stub schemas in fast tier
   - Move to separate test suite

3. **Consolidate Test Organization**
   - Currently 17 different test files with poor isolation
   - Consider merging quick tests (CLI, dark-matter, e2e)
   - Keep failed module tests separate pending fixes

---

## Test Status Summary Table

| File | Tests | Passed | Failed | Time | Status |
|------|-------|--------|--------|------|--------|
| test/cli.test.mjs | 6 | 6 | 0 | 6.04s | ✓ PASS |
| test/dark-matter-80-20.test.mjs | 3 | 3 | 0 | 6ms | ✓ PASS |
| test/e2e-integration.test.mjs | 1 | 1 | 0 | 2ms | ✓ PASS |
| test/receipts.test.mjs | 7 | 7 | 0 | 18.6ms | ✓ PASS |
| packages/v6-compat/integration.test.mjs | 13 | 13 | 0 | 14.6ms | ✓ PASS |
| packages/v6-compat/integration-node.test.mjs | 6 | 6 | 0 | 14.6ms | ✓ PASS |
| test/diff.test.mjs | - | - | BLOCKED | - | ✗ MODULE NOT FOUND |
| test/project-engine.test.mjs | - | - | BLOCKED | - | ✗ MODULE NOT FOUND |
| test/hook-executor-deps.test.mjs | - | - | BLOCKED | - | ✗ MODULE NOT FOUND |
| test/knowledge-engine/circuit-breaker.test.mjs | - | - | BLOCKED | - | ✗ MODULE NOT FOUND |
| test/knowledge-engine/ring-buffer.test.mjs | - | - | BLOCKED | - | ✗ MODULE NOT FOUND |
| test/lockchain-merkle-verification.test.mjs | 2 | 0 | 2 | - | ✗ ASSERTION FAIL |
| test/security-error-sanitizer.test.mjs | 2 | 1 | 2 | - | ✗ ASSERTION FAIL |
| test/knowledge-engine/parse-contract.test.mjs | 1 | 0 | 1 | 8041ms | ✗ TIMEOUT |
| test/knowledge-engine/query-contract.test.mjs | 2 | 0 | 2 | 8044ms | ✗ TIMEOUT + SYNTAX |
| packages/v6-compat/batch-1-validation.test.mjs | 5 | 3 | 2 | - | ✗ MIXED |
| **TOTAL** | **35** | **26** | **9** | **10.75s** | **FAIL** |

---

## SLA Verdict

**Overall Status**: FAIL

The 80/20 fast test consolidation does NOT meet the 5-second SLA:

- **Actual**: 10.75s execution (over 30s overall test suite)
- **Target**: 5.00s
- **Variance**: +5.75s (+115%)

**Pass Rate**: 74.3% (26/35 tests)
- Target: 100%
- **Variance**: -25.7%

**Blocking Issues**:
1. 5 tests cannot run due to missing modules (29%)
2. 2 tests timeout due to slow initialization (6%)
3. 2 tests have syntax errors preventing compilation (6%)
4. 4 tests have failing assertions (11%)

**Recommendation**: Address Priority 1 fixes before declaring consolidation complete.

---

## Next Steps

1. **Immediate** (Day 1):
   - [ ] Create missing module files (diff.mjs, project-engine/, hook-executor.mjs, circuit-breaker.mjs, ring-buffer.mjs)
   - [ ] Fix async/await syntax error in knowledge-substrate-core.mjs
   - [ ] Re-run: `timeout 30s pnpm test:fast`

2. **Short Term** (Day 2):
   - [ ] Fix timeout by increasing testTimeout to 5000ms
   - [ ] Fix hash calculation (64 chars) in lockchain-merkle
   - [ ] Fix error sanitization in security-error-sanitizer
   - [ ] Fix schema validation in v6-compat
   - [ ] Re-run and verify <5s execution

3. **Medium Term** (Week 1):
   - [ ] Profile and optimize transform/import times
   - [ ] Consolidate quick tests into unified suite
   - [ ] Move slow schema tests to separate suite
   - [ ] Validate end-to-end SLA

---

**Report Generated**: 2026-01-11
**Branch**: claude/consolidate-tests-Fssji
**Verifier**: QA Agent
**Next Review**: After Priority 1 fixes applied
