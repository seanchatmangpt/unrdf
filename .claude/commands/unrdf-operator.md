---
description: Operate UNRDF monorepo workflows including builds, tests, releases, and package management across 21 packages
---

# UNRDF Operator

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Purpose

Execute common UNRDF monorepo operations, manage package builds, run tests, and coordinate releases across all 21 packages.

## Quick Commands

```bash
# Build all packages
pnpm build

# Test all packages
pnpm test

# Lint all packages
pnpm lint

# Type check
pnpm typecheck

# Format code
pnpm format
```

## Package Overview

### 21 UNRDF Packages

| Package                 | Purpose            | Dependencies    |
| ----------------------- | ------------------ | --------------- |
| @unrdf/core             | RDF primitives     | oxigraph        |
| @unrdf/oxigraph         | Store wrapper      | -               |
| @unrdf/validation       | SHACL + OTEL       | core            |
| @unrdf/hooks            | Event hooks        | core            |
| @unrdf/streaming        | Real-time          | core            |
| @unrdf/federation       | Distributed        | core, streaming |
| @unrdf/cli              | CLI interface      | all             |
| @unrdf/composables      | Vue composables    | core            |
| @unrdf/domain           | Business logic     | core            |
| @unrdf/engine-gateway   | Query routing      | core            |
| @unrdf/knowledge-engine | Knowledge ops      | core            |
| @unrdf/kgn              | Knowledge network  | core            |
| @unrdf/kgc-4d           | 4D knowledge       | core            |
| @unrdf/atomvm           | Atom VM            | core            |
| @unrdf/test-utils       | Test helpers       | -               |
| @unrdf/browser          | Browser bundle     | core            |
| @unrdf/nextra           | Nextra integration | core            |
| @unrdf/project-engine   | Project management | core            |
| @unrdf/docs             | Documentation      | -               |

## Execution Steps

### 1. Build Operations

```bash
# Build all packages (respecting dependency order)
pnpm build

# Build specific package
pnpm --filter "@unrdf/core" build

# Build with dependencies
pnpm --filter "@unrdf/cli..." build

# Watch mode for development
pnpm --filter "@unrdf/core" build --watch
```

### 2. Test Operations

```bash
# Run all tests
pnpm test

# Test specific package
pnpm --filter "@unrdf/core" test

# Test with coverage
pnpm test:coverage

# Run specific test file
pnpm vitest run packages/core/test/store.test.mjs

# Watch mode
pnpm vitest watch
```

### 3. Lint Operations

```bash
# Lint all packages
pnpm lint

# Lint with auto-fix
pnpm lint:fix

# Lint specific package
pnpm --filter "@unrdf/core" lint

# Check formatting
pnpm format:check

# Apply formatting
pnpm format
```

### 4. Package Management

```bash
# Add dependency to specific package
pnpm --filter "@unrdf/cli" add citty

# Add dev dependency
pnpm --filter "@unrdf/core" add -D vitest

# Add internal dependency
pnpm --filter "@unrdf/cli" add @unrdf/core@workspace:*

# Update all dependencies
pnpm update

# Check outdated
pnpm outdated
```

### 5. Release Operations

```bash
# Check for changes
pnpm changeset status

# Create changeset
pnpm changeset

# Version packages
pnpm changeset version

# Publish packages
pnpm changeset publish

# Dry run publish
pnpm changeset publish --dry-run
```

## Workspace Commands

### Filter Syntax

```bash
# Single package
pnpm --filter "@unrdf/core" <command>

# Package and dependencies
pnpm --filter "@unrdf/cli..." <command>

# Package and dependents
pnpm --filter "...@unrdf/core" <command>

# Multiple packages
pnpm --filter "@unrdf/core" --filter "@unrdf/streaming" <command>

# Pattern matching
pnpm --filter "@unrdf/*" <command>
```

### Parallel Execution

```bash
# Run in parallel (default)
pnpm -r test

# Run sequentially
pnpm -r --sequential test

# Limit concurrency
pnpm -r --workspace-concurrency=4 build
```

## Validation Pipeline

```bash
# Full validation (run before commit)
pnpm precommit

# This runs:
# 1. pnpm lint
# 2. pnpm typecheck
# 3. pnpm test

# CI/CD pipeline
pnpm ci
```

## Troubleshooting

### Build Failures

```bash
# Clean build artifacts
pnpm clean

# Rebuild from scratch
rm -rf node_modules packages/*/node_modules
pnpm install
pnpm build
```

### Dependency Issues

```bash
# Check for issues
pnpm why @unrdf/core

# Dedupe dependencies
pnpm dedupe

# Verify integrity
pnpm install --frozen-lockfile
```

### Test Failures

```bash
# Run single test for debugging
pnpm vitest run path/to/test.test.mjs --reporter=verbose

# Run with debug output
DEBUG=* pnpm test

# Check test isolation
pnpm vitest run --no-threads
```

## Output Format

```markdown
## UNRDF Operation Report

**Operation**: [build/test/lint/release]
**Date**: [timestamp]
**Duration**: [time]

### Summary

| Package     | Status | Duration | Notes |
| ----------- | ------ | -------- | ----- |
| @unrdf/core | ✅     | Xs       | -     |
| @unrdf/cli  | ✅     | Xs       | -     |

### Issues Found

1. [package]: [issue description]

### Metrics

- Packages processed: X/21
- Success rate: X%
- Total duration: Xs

### Next Steps

- [recommendations]
```

## Development Workflow

### 1. Start Development

```bash
# Clone and setup
git clone <repo>
cd unrdf
pnpm install
pnpm build
```

### 2. Make Changes

```bash
# Create feature branch
git checkout -b feat/my-feature

# Watch mode for package being developed
pnpm --filter "@unrdf/core" build --watch

# Run tests in watch mode
pnpm vitest watch
```

### 3. Validate Changes

```bash
# Run full validation
pnpm precommit

# Check specific packages
pnpm --filter "@unrdf/core" test
pnpm --filter "@unrdf/core" lint
```

### 4. Commit and Push

```bash
# Stage changes
git add -A

# Commit with conventional format
git commit -m "feat(core): add new RDF feature"

# Push
git push -u origin feat/my-feature
```

## Environment Variables

```bash
# Debug mode
DEBUG=unrdf:* pnpm test

# CI mode
CI=true pnpm test

# Skip certain checks
SKIP_LINT=true pnpm precommit
```

End Command ---
