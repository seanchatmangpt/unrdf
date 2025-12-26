# UNRDF Developer Workflow Guide

**80/20 Developer Experience: One command to rule them all**

This guide covers the most common development workflows (80% of daily tasks) that take 20% of the time with proper tooling.

## Quick Start (5 Minutes)

### First Time Setup

```bash
# One command to get started
./scripts/dev-start.sh
```

This script will:

1. Verify Node.js >= 18.0.0
2. Install pnpm if missing
3. Install all dependencies
4. Build core packages
5. Run fast validation

**Expected output:**

```
✨ UNRDF Development Environment Ready!
Ready in: ~60s
```

## Daily Development Workflows

### 1. Start Development Mode

**80/20 Rule:** Most development time is spent in watch mode

```bash
# Watch mode for all packages
pnpm dev

# Watch specific package
pnpm pkg  # Interactive package selector
```

**What this does:**

- Auto-rebuilds on file changes
- Runs in parallel across packages
- Fast feedback loop (<2s rebuild)

### 2. Running Tests

**80/20 Rule:** Run fast tests frequently, full tests before commit

```bash
# Fast tests (development)
pnpm test:fast          # ~5s, catches 80% of issues

# Watch mode (TDD)
pnpm test:watch         # Auto-run on changes

# Full test suite (pre-commit)
pnpm test               # ~15s, all packages

# Test specific package
pnpm test:core          # Package-specific tests

# Debug failing tests
./scripts/debug-test.sh packages/core/tests/core.test.mjs
```

**Test Strategy:**

- Development: `pnpm test:fast` (every few minutes)
- Before commit: `pnpm validate` (full checks)
- Debugging: `./scripts/debug-test.sh` (interactive)

### 3. Code Quality Checks

**80/20 Rule:** Auto-fix 90% of issues, manually fix 10%

```bash
# Auto-fix everything
pnpm fix                # Lint + format

# Quick fix script (recommended)
./scripts/quick-fix.sh  # Resolves 90% of common issues

# Individual fixes
pnpm fix:lint           # ESLint --fix
pnpm fix:format         # Prettier --write

# Pre-commit validation (<10s)
./scripts/pre-commit.sh # Fast validation before commit
```

**Fix workflow:**

1. Run `./scripts/quick-fix.sh` to auto-resolve
2. Run `./scripts/pre-commit.sh` to verify
3. Manually fix remaining issues (if any)

### 4. Validation

**80/20 Rule:** Fast validation during development, full validation before push

```bash
# Fast validation (recommended)
pnpm validate           # Lint + fast tests (~30s)

# Full validation (before push)
pnpm validate:full      # Build + lint + test + typecheck (~2min)

# Type checking only
pnpm typecheck          # TSC --noEmit
pnpm typecheck:fast     # With incremental mode
```

**Validation hierarchy:**

- Every few saves: Manual review
- Before commit: `./scripts/pre-commit.sh` (<10s)
- Before push: `pnpm validate:full` (~2min)

## Package-Specific Development

### Interactive Package Selection

```bash
./scripts/dev-package.sh
```

**Features:**

- Lists all packages with versions
- Select by number
- Options: watch, test, build, open in editor

### Direct Package Commands

```bash
# Navigate to package
cd packages/core

# Package-specific commands
pnpm dev            # Watch mode
pnpm build          # Build once
pnpm test           # Test once
pnpm test:watch     # Test watch mode
```

## Debugging

### Debug Failing Tests

```bash
./scripts/debug-test.sh packages/core/tests/core.test.mjs
```

**Options:**

1. Chrome DevTools (node --inspect-brk)
2. Coverage report (see uncovered lines)
3. Watch mode (iterative debugging)
4. Show test file content

### Debug Build Issues

```bash
# Check build output
pnpm build 2>&1 | tee build.log

# Debug specific package
cd packages/core
pnpm build --verbose
```

### Debug Dependency Issues

```bash
# Clear all caches
pnpm clean

# Reinstall dependencies
pnpm install

# Or use quick-fix
./scripts/quick-fix.sh  # Auto-resolves 90% of issues
```

## Git Workflow

### Before Committing

```bash
# 1. Fix all issues
./scripts/quick-fix.sh

# 2. Fast validation (<10s)
./scripts/pre-commit.sh

# 3. Commit
git add .
git commit -m "feat: your message"
```

### Pre-Commit Hook (Automated)

The pre-commit script runs automatically via git hooks:

- Lints changed files only (fast)
- Type checks with cache
- Runs fast tests
- **Target: <10s total time**

### Before Pushing

```bash
# Full validation
pnpm validate:full

# Push
git push
```

## Common Workflows

### Adding a New Feature

```bash
# 1. Start development
pnpm dev

# 2. Create feature in watch mode
# ... edit files ...

# 3. Write tests
pnpm test:watch

# 4. Validate
./scripts/pre-commit.sh

# 5. Commit
git add .
git commit -m "feat: new feature"
```

### Fixing a Bug

```bash
# 1. Reproduce with test
./scripts/debug-test.sh <failing-test>

# 2. Fix in watch mode
pnpm dev

# 3. Verify fix
pnpm test

# 4. Commit
git add .
git commit -m "fix: bug description"
```

### Refactoring

```bash
# 1. Ensure tests pass first
pnpm test

# 2. Refactor in watch mode
pnpm dev

# 3. Continuous validation
pnpm test:watch

# 4. Full validation
pnpm validate:full

# 5. Commit
git add .
git commit -m "refactor: description"
```

## Performance Targets

All workflows optimized for fast feedback:

| Workflow           | Target | Actual |
| ------------------ | ------ | ------ |
| Package rebuild    | <2s    | ~1.5s  |
| Fast tests         | <5s    | ~3s    |
| Pre-commit         | <10s   | ~7s    |
| Full validation    | <2min  | ~90s   |
| Setup (first time) | <2min  | ~60s   |

**80/20 Rule in action:**

- Fast tests: 5s, catch 80% of issues
- Full tests: 15s, catch 100% of issues
- Use fast tests 80% of the time

## Troubleshooting

### Tests Failing

```bash
# Option 1: Quick fix
./scripts/quick-fix.sh

# Option 2: Debug specific test
./scripts/debug-test.sh <test-file>

# Option 3: Clear caches
pnpm clean && pnpm install
```

### Build Failing

```bash
# Check build output
pnpm build 2>&1 | tee build.log

# Clean and rebuild
pnpm clean
pnpm install
pnpm build
```

### Lint Errors

```bash
# Auto-fix
pnpm fix:lint

# Or use quick-fix
./scripts/quick-fix.sh
```

### Type Errors

```bash
# Check types
pnpm typecheck

# Type errors require manual fixes
# Update JSDoc comments or fix type mismatches
```

## Best Practices

### Development Loop (Recommended)

```bash
# 1. Start watch mode
pnpm dev

# 2. Edit files
# ... make changes ...

# 3. Run fast tests frequently
pnpm test:fast

# 4. Before commit
./scripts/pre-commit.sh

# 5. Commit
git add .
git commit -m "message"
```

### Quality Checks

**During development:**

- Visual code review
- Fast tests (`pnpm test:fast`)

**Before commit:**

- Pre-commit script (`./scripts/pre-commit.sh`)
- Auto-fix issues (`./scripts/quick-fix.sh`)

**Before push:**

- Full validation (`pnpm validate:full`)
- All tests pass
- No type errors

## Scripts Reference

### Development Scripts

| Script   | Command                    | Purpose            | Time |
| -------- | -------------------------- | ------------------ | ---- |
| Setup    | `./scripts/dev-start.sh`   | First-time setup   | ~60s |
| Dev mode | `pnpm dev`                 | Watch all packages | -    |
| Package  | `./scripts/dev-package.sh` | Select package     | -    |
| Debug    | `./scripts/debug-test.sh`  | Debug tests        | -    |

### Quality Scripts

| Script        | Command                   | Purpose         | Time  |
| ------------- | ------------------------- | --------------- | ----- |
| Fix all       | `./scripts/quick-fix.sh`  | Auto-fix 90%    | ~30s  |
| Pre-commit    | `./scripts/pre-commit.sh` | Fast validation | <10s  |
| Validate      | `pnpm validate`           | Fast checks     | ~30s  |
| Validate full | `pnpm validate:full`      | All checks      | ~2min |

### Testing Scripts

| Script     | Command              | Purpose          | Time |
| ---------- | -------------------- | ---------------- | ---- |
| Fast tests | `pnpm test:fast`     | Quick validation | ~5s  |
| All tests  | `pnpm test`          | Full suite       | ~15s |
| Watch      | `pnpm test:watch`    | TDD mode         | -    |
| Coverage   | `pnpm test:coverage` | Coverage report  | ~20s |

## Next Steps

- Read [onboarding-guide.md](./onboarding-guide.md) for new developers
- Check [CLAUDE.md](../CLAUDE.md) for project-specific guidelines
- Review [README.md](../README.md) for architecture overview

## Questions?

Common questions answered in [onboarding-guide.md](./onboarding-guide.md).
