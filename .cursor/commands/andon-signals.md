# Andon Signals (Visual Problem Indicators) - Multi-Step Workflow

## Purpose

This command guides agents to treat compiler errors, test failures, and warnings as Andon signals - visual indicators that something is wrong and work should stop. Andon means "lantern" or "sign" - a visual signal that alerts to problems. Experts stop and fix problems immediately when signals appear.

## Workflow Overview

```
Step 1: Monitor Andon Signals (with Measurement) → Step 2: Stop When Signal Appears → Step 3: Investigate Root Cause → Step 4: Fix Root Cause → Step 5: Verify Signal Cleared (with Measurement & Control)
```

## Step-by-Step Instructions

### Step 1: Monitor Andon Signals

**Action**: Watch for visual signals that indicate problems.

**Andon signal types**:

1. **Syntax/Type errors** - Red signals, must stop
   - Pattern: `SyntaxError:`, `TypeError:`, `ReferenceError:`, or ESLint errors
   - Severity: **CRITICAL** - Cannot proceed

2. **Linting errors** - Yellow/red signals, should stop
   - Pattern: ESLint errors/warnings
   - Severity: **HIGH** - Should fix before proceeding

3. **Test failures** - Red signals, must stop
   - Pattern: `FAIL` or `FAILED` in Vitest output
   - Severity: **CRITICAL** - Cannot proceed

4. **Formatting errors** - Yellow signals, should stop
   - Pattern: Prettier formatting issues
   - Severity: **HIGH** - Should fix before proceeding

5. **Performance regressions** - Yellow signals, investigate
   - Pattern: Tests taking longer than expected
   - Severity: **MEDIUM** - Investigate if significant

**Action**: Set up signal monitoring

```bash
# Monitor syntax/type signals
node --check src/**/*.mjs
pnpm lint
# Look for: SyntaxError, TypeError, or ESLint errors

# Monitor test signals
pnpm test
# Look for: FAIL or FAILED patterns in Vitest output

# Monitor linting signals
pnpm lint
# Look for: ESLint errors or warnings

# Monitor formatting signals
pnpm format:check
# Look for: Prettier formatting issues
```

**Principle**: "Andon signals are visual management" - Make problems immediately visible, don't hide them.

#### 1.1: Collect Baseline Data (DMAIC Measurement)

**Action**: Measure current signal frequency to establish baseline.

**Data to collect**:

- **Signal count**: How many signals exist?
- **Signal frequency**: How often do signals appear?
- **Signal types**: What types of signals (errors, warnings, failures)?
- **Signal severity**: What is the severity distribution?

**Action**: Collect baseline data

```bash
# Count syntax/type errors
pnpm lint 2>&1 | grep -c "error"
# Output: 3 errors

# Count linting warnings
pnpm lint 2>&1 | grep -c "warning"
# Output: 5 warnings

# Count test failures
pnpm test 2>&1 | grep -c "FAIL\|FAILED"
# Output: 2 failures

# Count formatting issues
pnpm format:check 2>&1 | grep -c "Code style issues"
# Output: 1 formatting issue
```

**Example baseline data**:

```markdown
## Baseline Data

**Total Signals**: 11
**By Type**:

- Compiler errors: 3 (27%)
- Compiler warnings: 5 (45%)
- Test failures: 2 (18%)
- Linting errors: 1 (9%)

**By Severity**:

- CRITICAL: 5 (errors + failures)
- HIGH: 6 (warnings + linting)
- MEDIUM: 0
```

---

### Step 2: Stop When Signal Appears

**Action**: Immediately stop work when an Andon signal appears.

#### 2.1: Recognize Signal Severity

**Action**: Determine signal severity and response.

**Signal severity levels**:

- **CRITICAL (Red)** - Must stop immediately
  - Compiler errors
  - Test failures
  - **Response**: Stop all work, fix immediately

- **HIGH (Yellow)** - Should stop
  - Compiler warnings
  - Linting errors
  - **Response**: Stop current work, fix before proceeding

- **MEDIUM (Yellow)** - Investigate
  - Performance warnings
  - Code quality warnings
  - **Response**: Investigate, fix if significant

**Action**: Classify signal

```markdown
## Andon Signal Classification

### Critical Signals (Stop Immediately)

- [ ] Syntax error: `SyntaxError: Unexpected token`
- [ ] Type error: `TypeError: Cannot read property 'x' of undefined`
- [ ] Test failure: `FAIL test-name.test.mjs`

### High Signals (Stop and Fix)

- [ ] Linting error: `ESLint: 'variable' is assigned a value but never used`
- [ ] Formatting issue: `Prettier: Code style issues found`

### Medium Signals (Investigate)

- [ ] Performance Warning: Test taking longer than expected
- [ ] Code quality Warning: Complexity too high
```

#### 2.2: Stop the Line

**Action**: Stop current work when signal appears.

**Stop the line principles**:

- **Don't ignore** - Never ignore Andon signals
- **Don't proceed** - Don't continue work with signals present
- **Don't hide** - Don't suppress warnings/errors
- **Fix immediately** - Address signal before continuing

**Example response**:

```bash
# Signal appeared: Syntax error
pnpm lint
# Output: SyntaxError: Unexpected token in src/test.mjs:10

# STOP: Do not proceed with other work
# ACTION: Fix syntax error immediately
```

---

### Step 3: Investigate Root Cause

**Action**: Understand why the signal appeared.

#### 3.1: Read Signal Message

**Action**: Carefully read the signal message.

**What to look for**:

- **Error message** - What went wrong?
- **Location** - Where did it occur?
- **Context** - What was happening when it occurred?

**Example**:

```
SyntaxError: Unexpected token '{'
  at src/test.mjs:10:5
    8 |   const result = testFunction();
    9 |   return result;
> 10 | }
     |  ^
```

**Analysis**:

- **What**: Syntax error - unexpected token
- **Where**: `src/test.mjs:10:5`
- **Why**: Missing closing brace or syntax issue

#### 3.2: Trace Root Cause

**Action**: Use root cause analysis to find why signal appeared.

**Questions to ask**:

- Why did this signal appear?
- What changed that caused it?
- Is this a symptom of a deeper problem?

**Example root cause analysis**:

```markdown
## Root Cause Analysis

**Signal**: Syntax error - unexpected token
**Why #1**: Missing closing brace in function
**Why #2**: Code was refactored and brace was accidentally removed
**Why #3**: Linting wasn't run before commit
**Root Cause**: Missing linting step in pre-commit workflow
```

**Reference**: See [Root Cause Analysis](./root-cause-analysis.md) for detailed 5 Whys process

#### 3.3: Verify Root Cause

**Action**: Confirm root cause hypothesis.

**Verification**:

- Does fixing root cause clear the signal?
- Does data support root cause hypothesis?
- Are there other contributing factors?

---

### Step 4: Fix Root Cause

**Action**: Address the underlying cause, not just the symptom.

#### 4.1: Fix the Problem

**Action**: Implement fix that addresses root cause.

**Fix principles**:

- **Fix root cause** - Not just symptom
- **Fix completely** - Don't leave partial fixes
- **Fix safely** - Don't introduce new problems

**Example fix**:

```javascript
// Root cause: Missing closing brace
// Fix: Add missing brace or fix syntax

// Option 1: Add missing closing brace
export function testFunction() {
  const result = processData();
  return result;
} // Added missing brace

// Option 2: Fix syntax error
// Before: export function testFunction() {
// After: export function testFunction() {
```

#### 4.2: Verify Fix

**Action**: Ensure fix resolves the signal.

**Verification steps**:

1. Fix the problem
2. Re-run signal check
3. Verify signal cleared

**Example**:

```bash
# Fix applied
# Re-check signal
pnpm lint
# Expected: No errors, signal cleared ✅
```

---

### Step 5: Verify Signal Cleared

**Action**: Confirm signal is resolved and won't return.

#### 5.1: Verify Signal Cleared

**Action**: Run checks to confirm signal gone.

**Verification**:

- ✅ Syntax/Type errors cleared: `pnpm lint`
- ✅ Test failures cleared: `pnpm test`
- ✅ Linting warnings cleared: `pnpm lint`
- ✅ Formatting issues cleared: `pnpm format:check`
- ✅ No new signals appeared

**Example**:

```bash
# Verify all signals cleared
pnpm lint          # No errors ✅
pnpm test          # All tests pass ✅
pnpm format:check  # No formatting issues ✅
```

#### 5.2: Prevent Signal Return

**Action**: Add controls to prevent signal from returning.

**Prevention methods**:

- **Tests** - Add tests to catch regression
- **Linting** - Enable linting rules in CI
- **Documentation** - Document why fix was needed
- **Code review** - Review to prevent similar issues

**Example**:

```javascript
// Add test to prevent regression
import { describe, it, expect } from "vitest";
import { testFunction } from "./test.mjs";

describe("testFunction", () => {
  it("should exist and be callable", () => {
    // Test that would fail if function removed again
    expect(() => testFunction()).not.toThrow();
  });
});
```

#### 5.3: Measure Improvement (DMAIC Measurement)

**Action**: Measure improvement against baseline data.

**Measurement**:

- Re-count signals after fixes
- Compare to baseline
- Calculate improvement percentage
- Verify success criteria met

**Action**: Measure improvement

```bash
# Re-count signals after fixes
pnpm lint 2>&1 | grep -c "error"
# Output: 0 errors (down from 3)

pnpm test 2>&1 | grep -c "FAIL\|FAILED"
# Output: 0 failures (down from 2)

# Calculate improvement
# Baseline: 11 signals total
# After fixes: 0 signals
# Improvement: 100% (11/11 signals cleared)
```

**Example improvement measurement**:

```markdown
## Improvement Measurement

**Baseline**: 11 signals total
**After Fixes**: 0 signals
**Improvement**: 100% (11/11 signals cleared)

**By Type**:

- Syntax/Type errors: 3 → 0 (100% improvement)
- Linting warnings: 5 → 0 (100% improvement)
- Test failures: 2 → 0 (100% improvement)
- Formatting issues: 1 → 0 (100% improvement)

**Success Criteria Met**: ✅

- All signals cleared ✅
- No new signals appeared ✅
```

#### 5.4: Establish Controls (DMAIC Control)

**Action**: Set up controls to prevent signals from returning.

**Controls**:

- **Automated checks**: Run checks automatically in CI
- **Pre-commit hooks**: Run checks before commits
- **Monitoring**: Track signal frequency over time
- **Alerts**: Set up alerts if signals appear

**Action**: Create todo list for controls (10+ items)

```markdown
## Andon Signal Control Todos (10+ items)

**Automated Checks**:

- [ ] Add CI check: Run `pnpm lint` on every commit
- [ ] Add CI check: Run `pnpm test` on every commit
- [ ] Add CI check: Run `pnpm format:check` on every commit
- [ ] Configure CI to fail if signals appear

**Pre-commit Controls**:

- [ ] Add pre-commit hook: Run checks before commit
- [ ] Configure hook to prevent commit if signals appear
- [ ] Verify pre-commit hooks work correctly
- [ ] Document hook usage

**Monitoring Controls**:

- [ ] Set up signal frequency tracking dashboard
- [ ] Configure alerts if signal count > 0
- [ ] Review signal trends weekly
- [ ] Document signal patterns

**Standards Controls**:

- [ ] Add standard: No signals allowed before commit
- [ ] Add standard: Fix signals immediately when they appear
- [ ] Update team documentation with standards
- [ ] Verify standards are followed
```

**Execution**:

1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement controls)
3. Mark todos as completed as controls are implemented
4. Verify each control works before moving to next
5. Continue until all controls implemented

**Principle**: Implement controls to prevent signals, don't just document them. Todos track progress, controls prevent recurrence.

#### 5.5: Monitor for New Signals (DMAIC Control)

**Action**: Continue monitoring for new signals with systematic tracking.

**Monitoring**:

- Run checks regularly
- Don't ignore warnings
- Fix signals immediately
- Track signal frequency over time
- Set up alerts for regression

**Action**: Set up monitoring

```bash
# Monitor signal frequency
# Run daily:
#   pnpm lint 2>&1 | grep -c "error"
#   pnpm test 2>&1 | grep -c "FAIL\|FAILED"
# Alert if signal count > 0
```

# Track trends

# Week 1: 11 signals (baseline)

# Week 2: 0 signals (after fixes)

# Week 3: 0 signals (controls working)

# Week 4: 0 signals (sustained)

```

---

## Complete Workflow Example

```bash
# Step 1: Monitor Andon Signals
pnpm lint
# Signal appeared: SyntaxError: Unexpected token

# Step 2: Stop When Signal Appears
# STOP: Do not proceed with other work
# ACTION: Fix syntax error immediately

# Step 3: Investigate Root Cause
# Root cause: Missing closing brace during refactoring

# Step 4: Fix Root Cause
# Fix: Add missing brace or fix syntax
# Applied fix

# Step 5: Verify Signal Cleared
pnpm lint   # No errors ✅
pnpm test   # All tests pass ✅
# Signal cleared, work can proceed
```

## Andon Signal Response Matrix

| Signal Type         | Severity | Response                                 | Example                                    |
| ------------------- | -------- | ---------------------------------------- | ------------------------------------------ |
| Syntax/Type error   | CRITICAL | Stop immediately, fix now                | `SyntaxError: Unexpected token`            |
| Test failure        | CRITICAL | Stop immediately, fix now                | `FAIL test-name.test.mjs`                  |
| Linting error       | HIGH     | Stop current work, fix before proceeding | `ESLint: 'var' is assigned but never used` |
| Formatting issue    | HIGH     | Stop current work, fix before proceeding | `Prettier: Code style issues found`        |
| Performance warning | MEDIUM   | Investigate, fix if significant          | Test timeout                               |

## Integration with Other Commands

- **[Root Cause Analysis](./root-cause-analysis.md)** - Use 5 Whys in Step 3 to find root cause
- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use DMAIC measurement and control steps integrated into this workflow
- **[Gemba Walk](./gemba-walk.md)** - Go to source in Step 3 to investigate
- **[Poka-Yoke Design](./poka-yoke-design.md)** - Use Zod validation and JSDoc types in Step 4 to prevent signals

## Expert Insights

**Why this matters**: Ignoring signals leads to accumulating problems. Experts treat every signal as important and fix them immediately.

**Key principle**: "Stop the line" - When an Andon signal appears, stop work and fix the problem immediately. Don't proceed with problems present.

**Remember**: Andon signals are visual management. They make problems immediately visible. Don't hide them, don't ignore them, fix them.

**Andon culture**: In Lean manufacturing, any worker can stop the production line if they see a problem. In coding, any developer should stop and fix problems when signals appear. This prevents defects from propagating.

**DfLSS alignment**: Andon signals help prevent both defects (quality) and waste (efficiency) - stopping problems early prevents rework (waste) and defects from propagating (quality). This aligns with DfLSS (Design for Lean Six Sigma) principles. Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.
