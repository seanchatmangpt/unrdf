# Verify Tests Before Completion - Multi-Step Workflow

## Purpose

This command guides agents through the complete workflow of running tests, identifying failures, fixing issues, and ensuring all tests pass before marking work as complete. It breaks down the complex process into clear, sequential steps with validation checkpoints.

## Workflow Overview

```
Step 1: Run Tests (with Measurement) → Step 2: Analyze Results → Step 3: Fix Failures → Step 4: Re-Run Tests → Step 5: Verify Completion (with Measurement & Control)
```

## Step-by-Step Instructions

### Step 1: Run Test Suite

**Action**: Run all tests to identify any failures.

```bash
pnpm test
```

**What this does**:

- Runs all unit tests
- Runs all integration tests
- Runs all example tests
- Runs property-based tests (if feature enabled)
- Runs mutation tests (if feature enabled)

**Expected Result**: All tests pass (exit code 0)

**If this step fails**: Proceed to Step 2 (Analyze Results)

**If this step succeeds**: Skip to Step 5 (Verify Completion)

**Note**: Always use `pnpm test` for running tests. See [Build System Practices](../rules/build-system-practices.mdc).

#### 1.1: Collect Baseline Data (DMAIC Measurement)

**Action**: Measure current test state to establish baseline.

**Data to collect**:

- **Test count**: How many tests exist?
- **Failure count**: How many tests fail?
- **Failure rate**: What percentage of tests fail?
- **Failure types**: What types of failures (syntax/type, test, assertion, timeout)?

**Action**: Collect baseline data

```bash
# Count total tests
pnpm test 2>&1 | grep -c "test.*\.\.\."
# Output: 150 tests total

# Count failures
pnpm test 2>&1 | grep -c "FAILED"
# Output: 5 failures

# Calculate failure rate
# 5 failures / 150 tests = 3.3% failure rate

# Categorize failures
# Syntax/Type errors: 2
# Test failures: 2
# Panics: 1
# Timeouts: 0
```

**Example baseline data**:

```markdown
## Baseline Data

**Total Tests**: 150
**Failures**: 5
**Failure Rate**: 3.3% (5/150)

**By Type**:

- Syntax/Type errors: 2 (40%)
- Test failures: 2 (40%)
- Assertion errors: 1 (20%)
- Timeouts: 0 (0%)
```

---

### Step 2: Analyze Test Results

**Action**: Parse test output to identify all failures and categorize them.

#### 2.1: Extract Failure Information

**Look for these patterns in output**:

**Syntax/Type Errors**:

```
Error:...]: <description>
  -=> src/file.mjs:line:column
```

**Test Failures**:

```
test('test_name'  FAILED
```

**Panics**:

```
thread 'test_name' panicked at '<message>', src/file.mjs:line:column
```

**Timeouts**:

```
test('test_name' ... timeout
```

#### 2.2: Categorize Failures

**Create failure list**:

```markdown
## Test Failures

### Compilation Errors

- [ ] `src/fixture.mjs:123` - Error: `expected type, found ...`

### Test Failures

- [ ] `test_fixture_creation` - Error: `Fixture creation failed`
- [ ] `test_builder_pattern` - Error: `assertion failed: expected 10, got 5`

### Panics

- [ ] `test_async_operation` - Panic: `called Result. on an Err value`

### Timeouts

- [ ] `test_slow_operation` - Timeout after 60s
```

#### 2.3: Prioritize Fixes

**Priority Order**:

1. **Syntax/Type errors** - Must fix first (blocks everything)
2. **Test failures** - Fix by test importance (critical path first)
3. **Assertion errors** - Fix immediately (indicates bugs)
4. **Timeouts** - Fix or optimize (may indicate performance issues)

---

### Step 3: Fix Test Failures

**Action**: Systematically fix each failure category.

#### 3.1: Fix Syntax/Type Errors

**For each syntax/type error**:

**Step 3.1.1**: Read error message carefully

- Understand what the error is complaining about
- Identify the root cause

**Step 3.1.2**: Fix the error

- Update code to resolve syntax/type issue
- Ensure proper JSDoc types
- Fix import statements if needed

**Step 3.1.3**: Verify fix

```bash
pnpm lint
```

**Step 3.1.4**: Repeat until all syntax/type errors fixed

**Common Fixes**:

- Missing imports: Add `import` statements
- Type mismatches: Fix JSDoc type annotations or Zod schemas
- Missing dependencies: Install packages via `pnpm add`
- Syntax errors: Fix syntax issues

**Reference**: See [User Guide - Troubleshooting](../../docs/USER_GUIDE.md#troubleshooting)

#### 3.2: Fix Test Failures

**For each test failure**:

**Step 3.2.1**: Read test failure message

- Understand what the test expected vs. what it got
- Identify the root cause

**Step 3.2.2**: Determine if test or implementation is wrong

- Review test logic
- Review implementation logic
- Check if test needs updating or implementation needs fixing

**Step 3.2.3**: Fix the issue

- Update test if test is wrong
- Update implementation if implementation is wrong
- Ensure test follows AAA pattern (see [Chicago TDD Standards](../rules/chicago-tdd-standards.mdc))

**Step 3.2.4**: Verify fix

```bash
pnpm test('test_name'
```

**Step 3.2.5**: Repeat for each failing test

**Common Fixes**:

- Wrong expected values: Update assertions
- Missing setup: Add Arrange phase
- Async issues: Ensure proper async handling
- Dependencies: Install required packages

**Reference**: See [User Guide - Best Practices](../../docs/USER_GUIDE.md#best-practices)

#### 3.3: Fix Panics

**For each panic**:

**Step 3.3.1**: Identify panic source

- Read stack trace
- Find the exact line causing panic

**Step 3.3.2**: Fix panic source

- Replace unsafe operations with proper error handling
- Add null checks
- Fix index out of bounds
- Handle edge cases

**Step 3.3.3**: Verify fix

```bash
pnpm test('test_name'
```

**Step 3.3.4**: Repeat for each panic

**Common Fixes**:

- ``on`null`: Use `match`or`?` operator
- Index out of bounds: Add bounds checking
- Division by zero: Add zero checks
- Null pointer: Add null checks

**Reference**: See [Expert Testing Patterns](./expert-testing-patterns.md) for panic safety testing

#### 3.4: Fix Timeouts

**For each timeout**:

**Step 3.4.1**: Identify slow operation

- Review test code
- Find the operation taking too long

**Step 3.4.2**: Optimize or mock

- Optimize slow code
- Mock external dependencies
- Increase timeout if legitimate
- Use test fixtures for setup

**Step 3.4.3**: Verify fix

```bash
pnpm test('test_name'
```

**Step 3.4.4**: Repeat for each timeout

**Common Fixes**:

- Mock external APIs
- Use test fixtures (see [User Guide - Test Fixtures](../../docs/USER_GUIDE.md#test-fixtures))
- Optimize algorithms
- Increase timeout for legitimate slow operations

---

### Step 4: Re-Run Tests

**Action**: Run tests again to verify all fixes worked.

```bash
pnpm test
```

**Expected Result**: All tests pass (exit code 0)

**If this step fails**:

- Return to Step 2
- Identify remaining failures
- Fix them in Step 3
- Repeat until all tests pass

**If this step succeeds**: Proceed to Step 5

**CRITICAL**: Do not mark work as complete until Step 4 passes completely.

---

### Step 5: Verify Completion

**Action**: Final verification that work is complete.

#### 5.1: Verify All Tests Pass

```bash
pnpm test
```

**Expected**: Exit code 0, all tests pass

#### 5.2: Verify Syntax/Type Check

```bash
pnpm lint
```

**Expected**: Exit code 0, no syntax/type errors

#### 5.3: Verify No Pending Test Fixes

**Check**: Review todo list for any pending test fixes

**Action**: Remove completed test fixes from todo list

**Expected**: No pending test fixes remain

#### 5.4: Measure Improvement (DMAIC Measurement)

**Action**: Measure improvement against baseline data.

**Measurement**:

- Re-count failures after fixes
- Compare to baseline
- Calculate improvement percentage
- Verify success criteria met

**Action**: Measure improvement

```bash
# Re-count failures after fixes
pnpm test 2>&1 | grep -c "FAILED"
# Output: 0 failures (down from 5)

# Calculate improvement
# Baseline: 5 failures (3.3% failure rate)
# After fixes: 0 failures (0% failure rate)
# Improvement: 100% (5/5 failures fixed)
```

**Example improvement measurement**:

```markdown
## Improvement Measurement

**Baseline**: 5 failures (3.3% failure rate)
**After Fixes**: 0 failures (0% failure rate)
**Improvement**: 100% (5/5 failures fixed)

**By Type**:

- Syntax/Type errors: 2 → 0 (100% improvement)
- Test failures: 2 → 0 (100% improvement)
- Assertion errors: 1 → 0 (100% improvement)

**Success Criteria Met**: ✅

- All tests pass: 150/150 (100%) ✅
- No syntax/type errors ✅
- No test failures ✅
```

#### 5.5: Mark Work Complete

**Only when**:

- ✅ All tests pass (`pnpm test` exits with code 0)
- ✅ No syntax/type errors (`pnpm lint` succeeds)
- ✅ No test failures
- ✅ No pending test fixes in todo list
- ✅ Improvement measured and verified

**Then**: Mark work as complete

#### 5.6: Establish Controls (DMAIC Control)

**Action**: Set up controls to prevent test failures from returning.

**Controls**:

- **CI/CD**: Run tests automatically on every commit
- **Pre-commit hooks**: Run tests before commits
- **Monitoring**: Track test failure rate over time
- **Alerts**: Set up alerts if failure rate increases

**Action**: Create todo list for controls (10+ items)

```markdown
## Test Verification Control Todos (10+ items)

**CI/CD Controls**:

- [ ] Add CI check: Run all tests on every commit
- [ ] Configure CI to fail if tests fail
- [ ] Add test failure rate tracking to CI
- [ ] Verify CI checks work correctly

**Pre-commit Controls**:

- [ ] Add pre-commit hook: Run tests before commit
- [ ] Configure hook to prevent commit if tests fail
- [ ] Verify pre-commit hooks work correctly
- [ ] Document hook usage

**Monitoring Controls**:

- [ ] Set up test failure rate tracking dashboard
- [ ] Configure alerts if failure rate > 1%
- [ ] Review test failure trends weekly
- [ ] Document failure patterns

**Standards Controls**:

- [ ] Add standard: All tests must pass before commit
- [ ] Add standard: Test failure rate must be < 1%
- [ ] Update team documentation with standards
- [ ] Verify standards are followed
```

**Execution**:

1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement controls)
3. Mark todos as completed as controls are implemented
4. Verify each control works before moving to next
5. Continue until all controls implemented

**Principle**: Implement controls to prevent test failures, don't just document them. Todos track progress, controls prevent recurrence.

#### 5.7: Monitor (DMAIC Control)

**Action**: Monitor to ensure test failures don't return.

**Monitoring**:

- Track test failure rate over time
- Set up alerts for regression
- Review trends periodically
- Adjust controls if needed

**Action**: Set up monitoring

```bash
# Monitor test failure rate
# Run daily: pnpm test 2>&1 | grep -c "FAILED"
# Alert if failure rate > 1%

# Track trends
# Week 1: 5 failures (3.3% failure rate - baseline)
# Week 2: 0 failures (0% failure rate - after fixes)
# Week 3: 0 failures (0% failure rate - controls working)
# Week 4: 0 failures (0% failure rate - sustained)
```

---

## Advanced: Running Specific Test Suites

### Run Unit Tests Only

```bash
pnpm test-unit
```

**Use when**: Quick feedback during development

### Run Example Tests

```bash
pnpm test-examples
```

**Use when**: Verifying example code works

**Reference**: See [Getting Started Guide](../../docs/getting-started.md) for examples

### Run Property-Based Tests

```bash
pnpm test-property
```

**Use when**: Testing with `property-testing` feature enabled

**Reference**: See [User Guide - Property-Based Testing](../../docs/USER_GUIDE.md#property-based-testing)

### Run Mutation Tests

```bash
pnpm test-mutation
```

**Use when**: Testing with `mutation-testing` feature enabled

**Reference**: See [User Guide - Mutation Testing](../../docs/USER_GUIDE.md#mutation-testing)

### Run Single-Threaded Tests

```bash
pnpm test-single-threaded
```

**Use when**:

- Tests are flaky
- Need deterministic execution
- Debugging race conditions

### Run Verbose Tests

```bash
pnpm test-verbose
```

**Use when**: Need detailed output for debugging

---

## Failure Pattern Reference

### Compilation Errors

**Pattern**: `Error:...]: <description>`

**Example**:

```
Error:0425]: cannot find function `test_function` in this scope
  -=> src/test.mjs:10:5
   |
10 |     test_function();
   |     ^^^^^^^^^^^^ not found in this scope
```

**Fix**: Add missing function or import

### Test Failures

**Pattern**: `test('test_name'  FAILED`

**Example**:

```
test test_fixture_creation  FAILED

---- test_fixture_creation stdout ----
thread 'test_fixture_creation' panicked at 'assertion failed: `(left == right)`
  left: `0`,
 right: `1`', src/fixture.mjs:123:5
```

**Fix**: Review assertion, fix test or implementation

### Panics

**Pattern**: `thread 'test_name' panicked`

**Example**:

```
thread 'test_builder' panicked at 'called `Result.` on an `Err` value: "error"', src/builders.mjs:45:23
```

**Fix**: Replace `` with proper error handling

### Timeouts

**Pattern**: `test('test_name' ... timeout`

**Example**:

```
test test_slow_operation ... timeout
```

**Fix**: Optimize code or increase timeout

---

## Complete Workflow Example

```bash
# Step 1: Run Tests
pnpm test
# Output: 2 tests failed

# Step 2: Analyze Results
# Found:
# - test_fixture_creation: FAILED - assertion failed
# - test_builder: FAILED - panic on

# Step 3: Fix Failures
# Fix test_fixture_creation: Update expected value
# Fix test_builder: Replace  with proper handling

# Step 4: Re-Run Tests
pnpm test
# All tests pass ✅

# Step 5: Verify Completion
pnpm lint  # Syntax/Type check OK
pnpm test   # All tests pass
# No pending test fixes in todo list
# Mark work complete ✅
```

## Error Handling

### If Tests Fail Repeatedly

**After 3 attempts**:

- Document issue in todo list
- Create detailed failure report
- Consider if test is correct
- Ask for help if stuck

### If Tests Are Flaky

**Action**:

- Add to todo list: "Fix flaky test: `test_name`"
- Use `pnpm test-single-threaded` for deterministic execution
- Review test isolation
- Check for race conditions

**Reference**: See [User Guide - Troubleshooting](../../docs/USER_GUIDE.md#troubleshooting)

### If Syntax/Type Errors Persist

**Action**:

- Review error messages carefully
- Check package dependencies
- Verify imports and exports
- Consider if architecture change needed

**Reference**: See [Build System Practices](../rules/build-system-practices.mdc)

## Best Practices

1. **Run tests frequently** - Don't wait until the end
2. **Fix immediately** - Address failures as they occur
3. **One fix at a time** - Fix and verify each issue separately
4. **Verify after fixes** - Always re-run tests after fixes
5. **Document failures** - Add to todo list if not immediately fixable
6. **Never skip validation** - All tests must pass before completion

## Documentation References

- **[Build System Practices](../rules/build-system-practices.mdc)** - Build commands
- **[Chicago TDD Standards](../rules/chicago-tdd-standards.mdc)** - Testing standards
- **[Expert Testing Patterns](./expert-testing-patterns.md)** - Expert patterns
- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use DMAIC measurement and control steps integrated into this workflow
- **[Getting Started Guide](../../docs/getting-started.md)** - Quick start
- **[User Guide](../../docs/USER_GUIDE.md)** - Complete guide
- **[API Reference](../../docs/api/)** - API documentation

## Quick Reference

```bash
# Full workflow
pnpm test                    # Step 1: Run tests
# Analyze failures                 # Step 2: Analyze
# Fix failures                     # Step 3: Fix
pnpm test                    # Step 4: Re-run
pnpm lint                   # Step 5: Verify syntax/types
# Mark complete                    # Step 5: Verify completion
```

End Command ---
