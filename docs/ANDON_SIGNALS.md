# Andon Signals - Visual Problem Management

**Last Updated**: 2025-12-20
**Baseline**: 153 warnings, 0 errors
**Current**: 153 warnings, 0 errors
**Status**: ✅ All critical signals cleared

## Overview

This document tracks Andon signals (visual problem indicators) in the UNRDF codebase. When signals appear, work stops immediately and problems are fixed before proceeding.

## Signal Types & Severity

### 🔴 CRITICAL - Must Stop Immediately

- **Syntax/Type Errors** - Prevent code execution
  - Pattern: `SyntaxError:`, `TypeError:`, `ReferenceError:`
  - Check: `node --check packages/*/src/**/*.mjs`
  - Response: Fix immediately, commit cannot proceed

- **Test Failures** - Prevent code quality verification
  - Pattern: `FAIL` or `FAILED` in Vitest output
  - Check: `pnpm test:core` (primary suite)
  - Response: Fix immediately, cannot deploy with failures

### 🟡 HIGH - Should Stop

- **Linting Errors** - Code quality violations
  - Pattern: ESLint `error` level (not warnings)
  - Check: `pnpm lint | grep "error"`
  - Response: Fix before next commit
  - Current: **0 errors** ✅

- **Formatting Issues** - Code style violations
  - Pattern: Prettier formatting mismatch
  - Check: `pnpm format:check`
  - Response: Run `pnpm format:fix` and commit

## Signal Baseline (2025-12-20)

```
Total Signals: 153
├── Linting Warnings: 153 (100%)
├── Linting Errors: 0 ✅
├── Test Failures: 0 ✅
├── Syntax Errors: 0 ✅
└── Formatting Issues: 0 ✅
```

### Pre-existing Warnings (Not Regressions)

All 153 warnings are pre-existing and not caused by recent changes:
- Unused variables in KGN, streaming, validation packages
- No new warnings introduced in capability completion work

**Files with warnings**:
- `packages/kgn/src/**/*.js` - 40+ warnings (pre-existing)
- `packages/streaming/src/**/*.mjs` - 2 warnings (pre-existing)
- `packages/validation/src/**/*.mjs` - 1 warning (pre-existing)

## Completed Capability Fixes (2025-12-20)

✅ **CLI Graph Metadata Validation** (`packages/cli/src/commands/graph/update.mjs`)
- Added Zod schema for type safety
- Prevents JSON parsing crashes
- No new signals introduced

✅ **Format Conversion Validation** (`packages/core/src/utils/transform-utils.mjs`)
- Added format support validation
- Upfront error checking
- Core tests: 231/231 PASS

✅ **Streaming Package JSDoc** (`packages/streaming/src/index.mjs`)
- Complete documentation for public APIs
- No signals affected

## Signal Monitoring Checklist

### Daily Checks (Before Commit)

```bash
# Check for critical signals
node --check packages/*/src/**/*.mjs          # Syntax errors?
pnpm lint 2>&1 | grep -c "error"              # Errors? (should be 0)
pnpm test:core                                # Core tests pass?
```

### Pre-Commit Hooks

```bash
#!/bin/bash
# Check syntax/type errors
node --check packages/*/src/**/*.mjs || exit 1

# Check for linting errors (not warnings)
pnpm lint 2>&1 | grep -q "error" && exit 1

# Core tests must pass
pnpm test:core || exit 1

echo "✅ All signals cleared, commit allowed"
```

### CI Pipeline Requirements

1. **Syntax Check**: `node --check` on all MJS files
2. **Linting Check**: `pnpm lint` must show 0 errors (warnings OK)
3. **Core Tests**: `pnpm test:core` must pass 100%
4. **Fail Fast**: Stop on first signal

## Root Cause Analysis (5 Whys)

### Why Do 153 Warnings Exist?

1. **Why**: Unused variables in legacy code (KGN, streaming)
2. **Why**: Pre-existing patterns from multi-year development
3. **Why**: Low priority compared to critical signals
4. **Why**: Fixing requires understanding legacy behavior
5. **Root Cause**: Technical debt from prior development cycles

### Why Should We NOT Ignore Warnings?

1. **Hygiene**: Clean signals = easier to spot regressions
2. **Culture**: Ignore small signals → ignore big signals
3. **Efficiency**: Warning creep = harder to maintain baseline

### Action: Establish Baseline

- Current warnings: 153 (acceptable baseline)
- Target new PRs: 0 new signals introduced
- Monitor: Flag if warning count increases
- Plan: Address technical debt incrementally

## Prevention Controls

### 1. Pre-Commit Hooks (Automated)

**File**: `.husky/pre-commit`

```bash
#!/bin/bash
# Andon signal checks before commit

echo "🚨 Running Andon signal checks..."

# 1. Syntax/Type check
echo "  Checking syntax..."
node --check packages/*/src/**/*.mjs || exit 1
echo "  ✅ No syntax errors"

# 2. Linting check (errors only)
echo "  Checking linting..."
pnpm lint 2>&1 | grep -q "error" && echo "  ❌ Linting errors found" && exit 1
echo "  ✅ No linting errors"

# 3. Core tests
echo "  Running core tests..."
pnpm test:core || exit 1
echo "  ✅ Core tests pass"

echo "✅ All Andon signals cleared, commit allowed"
```

### 2. CI Pipeline (GitHub Actions)

**File**: `.github/workflows/quality.yml`

```yaml
name: Quality Checks

on: [push, pull_request]

jobs:
  andon-signals:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: pnpm/action-setup@v2
      - uses: actions/setup-node@v3
        with:
          node-version: '18'
          cache: 'pnpm'

      - name: Check Syntax
        run: node --check packages/*/src/**/*.mjs

      - name: Lint (Fail on Errors)
        run: pnpm lint 2>&1 | grep -v "warning" | grep "error" && exit 1 || exit 0

      - name: Core Tests
        run: pnpm test:core

      - name: Signal Status
        if: always()
        run: echo "✅ Andon signals check complete"
```

### 3. Monitoring Dashboard (Weekly Review)

**Metrics to Track**:
- Signal count (target: 0 new signals)
- Signal types (errors vs warnings)
- Time to resolution (when signals appear)
- Baseline drift (warnings should stay ≤153)

**Weekly Report Template**:

```markdown
## Andon Signal Report - Week of 2025-12-20

### Summary
- Total signals: 153 (baseline maintained)
- New signals this week: 0 ✅
- Signals fixed this week: 0
- Baseline drift: 0% ✅

### By Type
- Syntax errors: 0 (target: 0)
- Test failures: 0 (target: 0)
- Linting errors: 0 (target: 0)
- Warnings: 153 (baseline: 153)

### Actions Taken
- Monitored signals daily
- No regressions detected
- Baseline maintained

### Next Week
- Continue daily monitoring
- Maintain 0-signal critical threshold
- Plan technical debt cleanup for warnings
```

## Standards & Procedures

### When You See a Signal

1. **STOP** - Do not proceed with other work
2. **ALERT** - Notify team immediately
3. **INVESTIGATE** - Understand root cause (5 Whys)
4. **FIX** - Address underlying problem
5. **VERIFY** - Confirm signal cleared
6. **DOCUMENT** - Update this file and lessons learned

### Commit Policy

**NO COMMITS WITH CRITICAL SIGNALS**

```
Critical Signal → Cannot Commit
├── Syntax errors
├── Test failures
└── Linting ERRORS (warnings OK)

Non-Critical Signal → Should Fix, Can Commit
├── Linting warnings (accumulating)
├── Documentation gaps
└── Performance notes
```

## Escalation Path

**Signal Detected** → **Developer** → **Stop Work** → **Investigate** → **Fix** → **Verify**

If blocked (cannot fix):
1. Document the blocker
2. Notify tech lead
3. Create high-priority issue
4. Track as impediment

## FAQ

**Q: Can I suppress warnings?**
A: Only with documented justification. Use `// eslint-disable-next-line` with reason.

**Q: What if I inherit warnings?**
A: Don't add more. Fix what you touch. Establish baseline before adding features.

**Q: How often should I check signals?**
A: Before every commit (pre-commit hook). Daily review of trends.

**Q: What's the success criteria?**
A: Zero CRITICAL signals (errors, test failures). Maintain baseline on warnings.

## Related Commands

- `/andon-signals` - This workflow
- `/80-20-fill-gaps` - Capability completion
- `/root-cause-analysis` - 5 Whys framework
- `/dmaic-problem-solving` - DMAIC measurement & control

## References

- **Andon**: Japanese term for "sign" or "lantern" - visual management of problems
- **Stop the Line**: Lean manufacturing principle - stop work when problems detected
- **Poka-Yoke**: Error-proofing design - prevent signals through design
