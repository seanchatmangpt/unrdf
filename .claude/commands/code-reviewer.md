---
description: Review code changes for UNRDF three-tier architecture compliance (Commands → Ops → Runtime) and quality standards
---

# Code Reviewer

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Purpose

Systematically review code changes against UNRDF quality standards, three-tier architecture boundaries, and production readiness criteria.

## Three-Tier Architecture

UNRDF follows a strict layer separation:

```
┌─────────────────────────────────────────┐
│ Commands Layer (packages/cli/src/commands/) │
│ - User-facing CLI commands              │
│ - Input validation (Zod schemas)        │
│ - Output formatting                     │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│ Ops Layer (packages/*/src/lib/)         │
│ - Business logic orchestration          │
│ - Cross-package coordination            │
│ - Error handling and recovery           │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│ Runtime Layer (packages/core/src/)      │
│ - RDF store operations (@unrdf/oxigraph)│
│ - Pure functions, no side effects       │
│ - SPARQL execution                      │
└─────────────────────────────────────────┘
```

## Execution Steps

### 1. Gather Changes

```bash
# Get changed files
git diff --name-only HEAD~1 HEAD 2>/dev/null || git diff --name-only --staged

# Get detailed diff
git diff HEAD~1 HEAD --stat
```

### 2. Layer Boundary Check

For each changed file, verify layer compliance:

| Layer    | Allowed Imports      | Forbidden Imports |
| -------- | -------------------- | ----------------- |
| Commands | Ops, Runtime, Zod    | Direct N3 imports |
| Ops      | Runtime, other Ops   | Commands layer    |
| Runtime  | Only @unrdf/oxigraph | Ops, Commands     |

```bash
# Check for N3 imports outside justified modules
grep -r "from 'n3'" packages/*/src/ --include="*.mjs" | grep -v "n3-justified-only.mjs" || echo "✅ No unauthorized N3 imports"

# Check for Store constructor (should use createStore)
grep -r "new Store()" packages/*/src/ --include="*.mjs" || echo "✅ No direct Store constructor"
```

### 3. Quality Standards Check

```bash
# Run linting
timeout 30s pnpm lint 2>&1 | tail -20

# Run type check
timeout 30s pnpm typecheck 2>&1 | tail -20

# Run tests for changed packages
CHANGED_PKGS=$(git diff --name-only HEAD~1 HEAD | grep "^packages/" | cut -d'/' -f2 | sort -u)
for pkg in $CHANGED_PKGS; do
  echo "Testing @unrdf/$pkg..."
  timeout 60s pnpm --filter "@unrdf/$pkg" test 2>&1 | tail -10
done
```

### 4. JSDoc Coverage

Verify all public functions have JSDoc:

```bash
# Check for exported functions without JSDoc
grep -B1 "^export" packages/*/src/**/*.mjs | grep -v "@" | grep "function" || echo "✅ All exports have JSDoc"
```

### 5. Security Review

```bash
# Check for hardcoded secrets
grep -rE "(password|secret|api_key|token)\s*[:=]\s*['\"]" packages/*/src/ --include="*.mjs" || echo "✅ No hardcoded secrets"

# Check for eval usage
grep -r "eval(" packages/*/src/ --include="*.mjs" || echo "✅ No eval usage"
```

## Output Format

```markdown
## Code Review Report

**Scope**: [files reviewed]
**Date**: [timestamp]

### Layer Compliance

- Commands Layer: ✅/❌
- Ops Layer: ✅/❌
- Runtime Layer: ✅/❌

### Quality Gates

| Check    | Status | Details         |
| -------- | ------ | --------------- |
| Lint     | ✅/❌  | [error count]   |
| Types    | ✅/❌  | [coverage %]    |
| Tests    | ✅/❌  | [pass/fail]     |
| JSDoc    | ✅/❌  | [missing count] |
| Security | ✅/❌  | [findings]      |

### Issues Found

1. [CRITICAL/HIGH/MEDIUM/LOW] Description
   - Location: file:line
   - Fix: Suggested resolution

### Recommendations

- [actionable items]
```

## Review Checklist

- [ ] No `from 'n3'` imports (use @unrdf/oxigraph)
- [ ] No `new Store()` (use createStore())
- [ ] Layer boundaries respected
- [ ] All exports have JSDoc
- [ ] Zod schemas for CLI inputs
- [ ] Error handling with proper messages
- [ ] No console.log in production code
- [ ] Tests cover new functionality
- [ ] No hardcoded configuration

End Command ---
