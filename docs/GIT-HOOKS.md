# Git Hooks Configuration

## Pre-Push Hook

The pre-push hook validates code quality before pushing to the remote repository.

### What It Checks

**Gate 1: Format Check (5s timeout)**

- Validates code formatting with Prettier
- Run manually: `pnpm format:check`
- Auto-fix: `pnpm format`

**Gate 2: Lint Check (10s timeout)**

- Validates code quality with ESLint
- Run manually: `pnpm lint`
- Auto-fix: `pnpm lint:fix`

**Gate 3: Build Check (15s timeout)**

- Validates build succeeds
- Run manually: `pnpm build`

### What It Doesn't Check (and Why)

**Tests are NOT run in pre-push hook**

Rationale:

- Full test suite is comprehensive but slow (~2-5 minutes)
- Tests run in CI/CD pipeline before merge anyway
- 30-second timeout is too aggressive for full test suite
- V8/Node.js crashes are infrastructure issues, not code issues
- Developers can run `pnpm test` locally before pushing if desired

**Result:** You can push valid code without workarounds. Full test suite validates in CI.

### Bypassing the Hook

For emergency situations only (use sparingly):

```bash
SKIP_PRE_PUSH=1 git push
```

### To Permanently Remove

```bash
rm .git/hooks/pre-push
```

### Local Testing

Run all pre-push validation manually:

```bash
pnpm format:check && pnpm lint && pnpm build
```

Run the full validation including tests:

```bash
pnpm format:check && pnpm lint && pnpm build && pnpm test
```

## Pre-Commit Hook

The pre-commit hook validates code before each commit.

### What It Does

- Skips validation if no relevant files are staged
- Used for fast sanity checks
- Does not block commits for most cases

## Design Philosophy

These hooks follow **Lean Six Sigma** principles:

- Only check what matters
- Be practical about timeouts
- Fail fast on preventable issues
- Let CI/CD handle comprehensive validation
- Avoid false negatives that require `--no-verify` workarounds
