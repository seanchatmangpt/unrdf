# Git Hooks

UNRDF uses git hooks to ensure code quality before commits and pushes. The hooks are installed via a script and run automatically.

## Installation

After cloning the repository, install the hooks:

```bash
pnpm install-hooks
# or
./scripts/install-hooks.sh
```

This installs:
- **pre-commit**: Fast validation (<5s) - format and lint checks
- **pre-push**: Full validation (<60s) - format, lint, build, and tests

## Pre-Commit Hook (Fast Tier)

Runs before each commit. Target: <5 seconds.

**Gates:**
1. **Format Check** (2s timeout, auto-fixable)
   - Checks if code is formatted with Prettier
   - Auto-fixes if possible, then requires re-commit

2. **Lint Check** (3s timeout)
   - Runs ESLint on staged files
   - Blocks commit if linting errors found

**To fix issues:**
```bash
pnpm format        # Auto-fix formatting
pnpm lint:fix      # Auto-fix linting issues
```

## Pre-Push Hook (Full Tier)

Runs before each push. Target: <60 seconds.

**Gates:**
1. **Format Check** (5s timeout)
   - Ensures all code is formatted

2. **Lint Check** (10s timeout)
   - Ensures no linting errors

3. **Build Check** (15s timeout)
   - Ensures project builds successfully

4. **Unit Tests** (30s timeout)
   - Ensures all tests pass

**To fix issues:**
```bash
pnpm format        # Fix formatting
pnpm lint:fix      # Fix linting
pnpm build         # Check build
pnpm test          # Run tests
```

## Bypassing Hooks (DISCOURAGED)

Hooks can be bypassed in emergency situations, but this is **strongly discouraged**:

```bash
# Bypass pre-commit
SKIP_PRE_COMMIT=1 git commit -m "message"

# Bypass pre-push
SKIP_PRE_PUSH=1 git push
```

**Why bypassing is discouraged:**
- Hooks catch defects early (DfLSS principle)
- Pushing broken code affects the entire team
- Fixing issues locally is faster than fixing CI failures

## Uninstalling Hooks

To remove hooks:

```bash
./scripts/install-hooks.sh --uninstall
# or manually
rm .git/hooks/pre-commit .git/hooks/pre-push
```

## Troubleshooting

### Hook not running

1. Check if hook is installed:
   ```bash
   ls -la .git/hooks/pre-commit
   ```

2. Check if hook is executable:
   ```bash
   chmod +x .git/hooks/pre-commit
   ```

3. Reinstall hooks:
   ```bash
   pnpm install-hooks
   ```

### Timeout errors

If you see timeout errors, the checks are taking too long. Options:

1. Fix the underlying issue (slow tests, large codebase)
2. Increase timeout in hook script (not recommended)
3. Use bypass (not recommended)

### Hook conflicts with husky

If you have husky installed, it may conflict. The direct git hooks take precedence. To use husky instead:

1. Uninstall direct hooks: `./scripts/install-hooks.sh --uninstall`
2. Install husky: `pnpm husky install`
3. Note: husky configuration exists in `package.json` but is not actively used

## Design Principles

These hooks follow DfLSS (Design for Lean Six Sigma) principles:

- **Fast feedback**: Pre-commit is fast (<5s) to catch 80% of issues
- **Comprehensive validation**: Pre-push catches remaining 20% of issues
- **Defect prevention**: Catch issues before they reach CI/CD
- **80/20 rule**: Fast checks catch most issues, comprehensive checks catch edge cases

## Comparison with Gold Standards

These hooks are based on patterns from:
- `chicago-tdd-tools`: Direct git hooks with install script
- `ggen`: Sophisticated hook system with timeouts and gates

Adapted for JavaScript/Node.js:
- Uses `pnpm` instead of `cargo make`
- Uses `prettier` and `eslint` instead of Rust formatters
- Uses `vitest` instead of Rust test framework

