# Add, Commit, Push (ACP) - Multi-Step Workflow

## Purpose

This command guides agents through the complete workflow of staging changes, validating code quality, committing, and pushing. It breaks down the complex process into clear, sequential steps with validation checkpoints.

**CRITICAL POLICY: WARNINGS ARE TREATED AS ERRORS**

All warnings from ESLint, Prettier, tests, or any validation tool MUST be fixed before committing. Warnings are not acceptable and will block the commit process. Treat every warning as a blocking error that must be resolved.

## Workflow Overview

```
Step 1: Pre-Validation → Step 2: Fix Issues → Step 3: Re-Validation → Step 4: Stage → Step 5: Generate Commit Message → Step 6: Commit → Step 7: Push
```

## Step-by-Step Instructions

### Step 1: Pre-Validation Checkpoint

**Action**: Run pre-commit validation to identify all issues before proceeding.

```bash
pnpm precommit
```

**What this does**:

- Lints code (`pnpm lint`)
- Runs tests (`pnpm test`)
- Checks for syntax errors and ESLint violations

**Expected Result**: All checks pass (exit code 0) with ZERO warnings

**CRITICAL**: Pre-commit checks MUST pass before committing. CI/CD will block commits
with syntax errors, test failures, OR warnings. Always run `pnpm precommit` before committing.
**FIX ALL ISSUES - DO NOT SKIP OR IGNORE ANY FAILURES OR WARNINGS.**

**WARNINGS ARE ERRORS**: Any warning from ESLint, Prettier, tests, or validation tools is treated as a blocking error. All warnings must be fixed before proceeding.

**Root Cause Prevention**: This step prevents syntax errors and test failures from being committed.
If this step fails, DO NOT commit - fix errors first. **FIX ALL ISSUES NO MATTER WHAT.**

**If this step fails**: Proceed to Step 2 (Fix Issues)

**If this step succeeds**: Skip Step 2, proceed to Step 4 (Stage Changes)

---

### Step 2: Fix Issues (Only if Step 1 Failed)

**Action**: Systematically fix ALL issues found in Step 1. Fix issues automatically when possible.

**CRITICAL**: This step MUST attempt to fix all issues. Do not skip fixes - address every issue found.

#### 2.1: Fix Syntax/Type Errors

**Check**: Look for `SyntaxError:`, `TypeError:`, or ESLint error patterns in output

**Action**:

```bash
# First, try auto-fix
pnpm lint:fix

# Then check remaining issues
pnpm lint
```

**Fix**:

- **AUTO-FIX FIRST**: Always run `pnpm lint:fix` to automatically fix issues
- Read error messages carefully for remaining issues
- Fix syntax/type errors in source files directly
- For each error: identify the file and line, fix the issue, verify
- Re-run `pnpm lint` until it passes OR all fixable issues are addressed

**Validation**: `pnpm lint` exits with code 0 OR all auto-fixable issues resolved

**If errors persist after auto-fix**: Fix manually, one error at a time, until all resolved

#### 2.2: Fix Formatting Issues

**Check**: Look for Prettier formatting warnings

**CRITICAL**: Prettier warnings are treated as errors and MUST be fixed.

**Action**:

```bash
# Auto-fix all formatting issues
pnpm format

# Verify formatting is correct (must have zero warnings)
pnpm format:check
```

**Fix**:

- **ALWAYS RUN**: `pnpm format` automatically fixes all formatting issues
- Re-run `pnpm format:check` to verify all issues resolved with ZERO warnings
- If issues persist, run `pnpm format` again
- All formatting warnings must be resolved before proceeding

**Validation**: `pnpm format:check` completes without errors or warnings. All formatting issues must be auto-fixed.

#### 2.3: Fix Linting Errors and Warnings

**Check**: Look for ESLint warnings/errors

**CRITICAL**: ESLint warnings are treated as errors and MUST be fixed.

**Action**:

```bash
# Auto-fix all auto-fixable linting issues
pnpm lint:fix

# Check remaining issues (including warnings)
pnpm lint
```

**Fix**:

- **AUTO-FIX FIRST**: Always run `pnpm lint:fix` to automatically fix issues
- **FIX ALL WARNINGS**: Warnings are treated as errors - fix every warning, not just errors
- Read ESLint suggestions for remaining issues
- Apply fixes manually for issues that can't be auto-fixed
- Add `// eslint-disable-next-line` comments ONLY when fix is not possible and issue is acceptable
- Re-run `pnpm lint` until it passes with ZERO warnings OR all fixable issues addressed

**Validation**: `pnpm lint` exits with code 0 with ZERO warnings. All warnings must be resolved.

**If errors or warnings persist**: Fix manually, addressing each error and warning systematically. Do not proceed until all warnings are resolved.

#### 2.4: Fix Test Failures and Warnings

**Check**: Look for `FAIL`, `FAILED`, or warning patterns in Vitest output

**CRITICAL**: Test warnings are treated as errors and MUST be fixed.

**Action**:

```bash
# Run tests to identify failures and warnings
pnpm test 2>&1 | tee test-failures.log

# Fix each failure and warning systematically
# (See fix strategies below)
```

**Fix**:

- **IDENTIFY ALL FAILURES AND WARNINGS**: Extract all failing test names, error messages, and warnings
- **FIX SYSTEMATICALLY**: Address each failure and warning one by one
- **For each failure or warning**:
  1. Read the test failure/warning message carefully
  2. Identify if it's a test issue or implementation issue
  3. Fix the root cause (test or implementation)
  4. Re-run the specific test: `pnpm test <test-name>`
  5. Verify fix before moving to next failure/warning
- **If test is flaky**: Increase timeout or fix race condition
- **If test is incorrect**: Update test expectations
- **If implementation is wrong**: Fix the implementation
- Re-run `pnpm test` after each fix to verify progress

**Validation**: `pnpm test` exits with code 0 with ZERO warnings. All test failures and warnings must be fixed.

**Reference**: See [verify-tests.md](./verify-tests.md) for detailed test failure handling

**CRITICAL**: Do not skip test fixes. Fix ALL test failures and warnings before proceeding. Warnings are treated as errors.

#### 2.5: Fix Git Hook Issues

**Check**: Look for hook validation failures

**Common Issues**:

- Missing JSDoc comments on public functions
- `console.log()` statements in production code
- TODO/FIXME comments (on main branch)
- Missing Zod validation schemas

**Fix**:

- Add JSDoc comments to all public functions
- Remove or replace `console.log()` with proper logging
- Remove or document TODOs appropriately
- Add Zod validation schemas where needed
- Install hooks if missing: Check `package.json` husky configuration

**Validation**: Git hooks pass

---

### Step 3: Re-Validation Checkpoint

**Action**: Re-run pre-commit validation to ensure all issues are fixed.

```bash
pnpm precommit
```

**Expected Result**: All checks pass (exit code 0) with ZERO warnings

**If this step fails**:

- **DO NOT GIVE UP**: Return to Step 2
- Identify remaining issues systematically (including all warnings)
- Fix each remaining issue and warning
- Re-run validation after each fix
- Continue until ALL issues and warnings resolved OR maximum attempts reached (see Error Handling)

**If this step succeeds**: Proceed to Step 4

**CRITICAL**: Do not proceed to Step 4 until Step 3 passes completely with ZERO warnings. Fix ALL issues and warnings no matter what. Warnings are treated as errors.

---

### Step 4: Stage Changes

**Action**: Stage all changes for commit.

```bash
git add -A
```

**Verify**: Check what will be committed

```bash
git status
```

**Expected Result**: All intended changes are staged

**If unexpected files are staged**: Review and unstage if needed (`git reset <file>`)

---

### Step 5: Generate Commit Message

**Action**: Generate commit message based on changes.

**Pattern**: Analyze staged changes and generate appropriate message:

**Documentation changes** (`README.md`, `docs/**/*.md`):

```
docs: update documentation
```

**JavaScript code changes** (`src/**/*.mjs`, `test/**/*.test.mjs`):

- New features: `feat: <description>`
- Bug fixes: `fix: <description>`
- Refactoring: `refactor: <description>`
- Tests: `test: <description>`

**Dependencies** (`package.json`, `pnpm-lock.yaml`):

```
chore: update dependencies
```

**Configuration** (`package.json`, `.cursor/**`):

```
chore: update configuration
```

**Multiple types**: Use most significant change type, or combine:

```
feat: add new feature and update docs
```

**Default**: If unclear, use descriptive message:

```
chore: update project files
```

**Format**: Use conventional commit format: `<type>: <description>`

---

### Step 6: Commit Changes

**Action**: Commit staged changes with generated message.

```bash
git commit -m "<generated-message>"
```

**Expected Result**: Commit succeeds

**If commit fails**:

- Check for uncommitted changes: `git status`
- Verify message format
- Retry commit

---

### Step 7: Push to Remote

**Action**: Push committed changes to remote repository.

```bash
git push
```

**Expected Result**: Push succeeds

**If push fails**:

- Check remote connection: `git remote -v`
- Check branch tracking: `git branch -vv`
- Resolve conflicts if any
- Retry push

---

## Complete Workflow Example

```bash
# Step 1: Pre-Validation
pnpm precommit
# Output: Some tests failed

# Step 2: Fix Issues
pnpm test
# Identify failing tests
# Fix test code
pnpm test
# All tests pass

# Step 3: Re-Validation
pnpm precommit
# All checks pass ✅

# Step 4: Stage
git add -A
git status  # Verify

# Step 5: Generate Message
# Analyze: Changed docs/getting-started.md
# Message: "docs: update getting started guide"

# Step 6: Commit
git commit -m "docs: update getting started guide"

# Step 7: Push
git push
```

## Error Handling

### If Pre-Validation Fails Multiple Times

**After 5 attempts**:

- **CONTINUE FIXING**: Do not give up easily
- Review all remaining issues systematically
- Fix issues one by one, verifying after each fix
- If issues are truly unfixable (e.g., external dependency issues):
  - Document remaining issues in todo list
  - Commit with `WIP:` prefix ONLY if absolutely necessary
  - Create issue for remaining problems
  - Do not commit broken code to main branch unless it's a critical hotfix

**CRITICAL**: Make at least 5 serious attempts to fix all issues before considering WIP commit.

### If Tests Are Flaky

**Action**:

- **FIX THE FLAKINESS**: Do not accept flaky tests
- Identify the root cause (race condition, timing, async issues)
- Fix the test to be deterministic:
  - Add proper waits/timeouts
  - Fix race conditions
  - Use proper test isolation
  - Mock external dependencies
- Re-run test multiple times to verify it's no longer flaky
- If truly unfixable after multiple attempts:
  - Add flaky test to todo list
  - Document in commit message: `test: fix flaky test (WIP)`
  - Create follow-up task to investigate
  - Consider skipping the test temporarily with proper documentation

### If Git Push Fails

**Common Causes**:

- Remote branch diverged
- No upstream branch set
- Authentication issues

**Fix**:

```bash
# Check remote
git remote -v

# Set upstream if needed
git push --set-upstream origin <branch>

# Pull and merge if diverged
git pull
git merge <branch>  # This will merge the remote branch into the local branch
# Resolve conflicts if any
# Commit the changes
git commit -m "Merge remote-tracking branch 'origin/<branch>' into <branch>"
# Push the changes
git push
```

## Best Practices

1. **Always run Step 1 first** - Never skip validation
2. **Warnings are errors** - Treat all warnings as blocking errors that must be fixed
3. **Fix issues immediately** - Don't accumulate technical debt
4. **Use auto-fix first** - Always try `pnpm lint:fix` and `pnpm format` before manual fixes
5. **Fix systematically** - Address issues one by one, verify after each fix
6. **Don't give up easily** - Make multiple attempts to fix issues before considering WIP
7. **Verify after fixes** - Always run Step 3 before committing
8. **Meaningful commit messages** - Use conventional commit format
9. **Small, focused commits** - One logical change per commit
10. **Never skip validation** - Broken code breaks the build
11. **Fix ALL issues and warnings** - Do not commit with known failures or warnings unless absolutely critical

## Documentation References

- **[Build System Practices](../rules/build-system-practices.mdc)** - Build commands and workflows
- **[Verify Tests Command](./verify-tests.md)** - Detailed test failure handling
- **[Chicago TDD Standards](../rules/chicago-tdd-standards.mdc)** - Testing standards
- **[Getting Started Guide](../../docs/getting-started.md)** - Quick start guide
- **[User Guide](../../docs/USER_GUIDE.md)** - Complete usage guide

## Quick Reference

```bash
# Full workflow (run sequentially)
pnpm precommit                 # Step 1: Validate
# Fix issues if needed          # Step 2: Fix
pnpm precommit                 # Step 3: Re-validate
git add -A                      # Step 4: Stage
# Generate commit message       # Step 5: Generate Commit Message
git commit -m "<message>"       # Step 6: Commit
git push                        # Step 7: Push
```

NEVER REBASE BECAUSE IT WILL BREAK THE BUILD.
