# Quick Start: Package Unification

**Feature**: 005-unify-packages
**Phase**: Implementation guide for developers
**Date**: 2025-12-20

## Overview

This guide walks through the package unification process for UNRDF monorepo. Covers structure migration, tooling setup, and validation.

## Prerequisites

- Node.js 18+ installed
- pnpm 8.0+ installed
- Fresh checkout of UNRDF monorepo from `005-unify-packages` branch

## Step 1: Standardize Package Structure (User Story 1)

### Objective
All 17 packages follow: `packages/{name}/src/index.mjs` structure with co-located tests.

### Migration Process

For each package:

1. **Create src/ directory**:
   ```bash
   mkdir -p packages/{name}/src
   ```

2. **Move source files**:
   ```bash
   # Move from lib/ or root to src/
   mv packages/{name}/lib/* packages/{name}/src/
   # OR
   mv packages/{name}/*.mjs packages/{name}/src/
   ```

3. **Consolidate exports**:
   - Create `packages/{name}/src/index.mjs`
   - Use named exports only:
   ```javascript
   // src/index.mjs
   export { api1 } from './module1.mjs';
   export { api2 } from './module2.mjs';
   // NO default exports
   ```

4. **Move test files**:
   ```bash
   # Move tests next to source
   mv packages/{name}/test/*.test.mjs packages/{name}/src/
   # Rename to *.test.mjs pattern
   ```

5. **Update package.json**:
   ```json
   {
     "main": "dist/index.mjs",
     "exports": {
       ".": {
         "import": "./dist/index.mjs",
         "require": "./dist/index.cjs"
       }
     }
   }
   ```

6. **Validate**:
   ```bash
   pnpm run lint  # Should pass with 0 violations
   pnpm test      # Should pass with ≥80% coverage
   ```

### Completion Checklist
- [ ] All 17 packages have `src/` directory
- [ ] `src/index.mjs` exists in all 17 packages
- [ ] Test files moved to `src/*.test.mjs`
- [ ] package.json exports fields updated
- [ ] All tests pass

---

## Step 2: Unify Build & Tooling (User Story 2)

### Objective
Single esbuild config, Ruff linting, Vitest testing for all packages.

### Build Configuration

1. **Create `/esbuild.config.mjs`** at monorepo root:
   ```javascript
   import { build } from 'esbuild';
   import dts from 'esbuild-plugin-dts';

   export default {
     entryPoints: ['packages/*/src/index.mjs'],
     outdir: 'dist',
     format: ['esm', 'cjs'],
     minify: true,
     sourcemap: true,
     target: 'es2020',
     plugins: [dts.default()],
   };
   ```

2. **Create `/ruffrc.toml`** at monorepo root:
   ```toml
   line-length = 100
   target-version = "es2020"

   [tool.ruff]
   select = [
     "E", "F", "W",  # pycodestyle, pyflakes, whitespace
     "I",            # isort (imports)
     "S",            # bandit (security)
     "C",            # McCabe complexity
   ]
   ignore = ["E501"]  # Line too long (covered by line-length)
   ```

3. **Create `/vitest.config.mjs`** at monorepo root:
   ```javascript
   import { defineConfig } from 'vitest/config';

   export default defineConfig({
     test: {
       environment: 'node',
       coverage: {
         provider: 'v8',
         reporter: ['text', 'json', 'html'],
         lines: 80,
         branches: 80,
         functions: 80,
         statements: 80,
       },
     },
   });
   ```

4. **Update root `/package.json`** scripts:
   ```json
   {
     "scripts": {
       "build": "esbuild $(ls -d packages/*)",
       "lint": "ruff check .",
       "test": "vitest run",
       "coverage": "vitest run --coverage"
     }
   }
   ```

5. **Remove per-package build scripts**:
   - Delete individual esbuild.config.* files
   - Delete individual .eslintrc* files
   - Delete individual vitest.config.* files
   - Delete custom build scripts from package.json files

### Linting Validation

```bash
# Lint all packages
pnpm run lint

# Expected: 0 violations across all packages
# If violations found: fix them (NO suppression comments allowed)
```

### Testing Validation

```bash
# Run all tests with coverage
pnpm run coverage

# Expected: All tests pass, ≥80% coverage in all packages
# Coverage report in coverage/index.html
```

### Completion Checklist
- [ ] `/esbuild.config.mjs` created and working
- [ ] `/ruffrc.toml` created with 400+ rules
- [ ] `/vitest.config.mjs` created and configured
- [ ] Root `package.json` scripts updated
- [ ] Per-package build scripts removed
- [ ] `pnpm run build` builds all 17 packages
- [ ] `pnpm run lint` passes with 0 violations
- [ ] `pnpm test` runs all tests with ≥80% coverage

---

## Step 3: Consolidate Dependencies (User Story 3)

### Objective
All shared dependencies at single version; no unused dependencies.

### Audit Process

1. **Run dependency audit**:
   ```bash
   # Install depcheck
   npm install -g depcheck

   # Check each package
   for pkg in packages/*/; do
     echo "=== $(basename $pkg) ==="
     depcheck "$pkg"
   done
   ```

2. **Find version conflicts**:
   ```bash
   # Show all versions of shared dependencies
   pnpm list oxigraph zod n3
   # Should show single version per package
   ```

3. **Resolve conflicts**:
   - If `oxigraph@1.2.0` and `oxigraph@1.2.1` exist:
     - Use highest compatible version (1.2.1)
     - Run tests to verify compatibility
     - Update all packages to 1.2.1

4. **Remove unused dependencies**:
   ```bash
   # For each package, remove unused deps
   cd packages/{name}
   npm prune
   ```

5. **Rebuild lock file**:
   ```bash
   pnpm install --frozen-lockfile=false
   # OR
   rm pnpm-lock.yaml
   pnpm install
   ```

### Validation

```bash
# Verify single version per shared dependency
pnpm list | grep "^  ├─"  # Should be all same version

# Verify clean install gives identical lock file
rm pnpm-lock.yaml
pnpm install
git diff pnpm-lock.yaml  # Should be no changes (deterministic)
```

### Completion Checklist
- [ ] No unused dependencies in any package
- [ ] All shared dependencies at single version
- [ ] pnpm-lock.yaml deterministic (same on fresh install)
- [ ] All tests still pass after version alignment

---

## Step 4: Align Exports & Metadata (User Stories 4 & 5)

### Objective
Named exports only, TypeScript definitions, complete package metadata.

### Export Alignment

1. **Check current exports**:
   ```bash
   for pkg in packages/*/src/index.mjs; do
     echo "=== $(dirname $pkg | xargs basename) ==="
     grep "^export" "$pkg" | head -5
   done
   ```

2. **Convert to named exports**:
   ```javascript
   // ❌ WRONG (default export)
   export default { api1, api2 };

   // ✅ CORRECT (named exports)
   export { api1, api2 };
   ```

3. **Generate TypeScript definitions**:
   ```bash
   # Using esbuild-plugin-dts
   pnpm build  # Creates dist/*.d.ts files

   # Validate no type errors
   npx tsc --noEmit --allowJs dist/**/*.d.ts
   ```

### Metadata Standardization

1. **Update each package.json**:
   ```json
   {
     "name": "@unrdf/package-name",
     "version": "0.0.1",
     "description": "Clear, one-line description of what this package does",
     "license": "MIT",
     "repository": {
       "type": "git",
       "url": "https://github.com/seanchatmangpt/unrdf",
       "directory": "packages/package-name"
     },
     "keywords": ["rdf", "knowledge-graph", "semantic-web"]
   }
   ```

2. **Create README.md** for each package:
   ```markdown
   # @unrdf/package-name

   One-line description.

   ## Installation

   pnpm add @unrdf/package-name

   ## Usage

   import { api1 } from '@unrdf/package-name';

   ## API

   - `api1(...)`: Description
   - `api2(...)`: Description
   ```

3. **Verify LICENSE** in each package:
   ```bash
   for pkg in packages/*/; do
     if [ ! -f "$pkg/LICENSE" ]; then
       cp LICENSE "$pkg/LICENSE"
     fi
   done
   ```

### Validation

```bash
# Verify metadata
for pkg in packages/*/package.json; do
  jq '.description, .license, .keywords' "$pkg"
done

# Verify README and LICENSE exist
for pkg in packages/*/; do
  [ -f "$pkg/README.md" ] && [ -f "$pkg/LICENSE" ] || echo "Missing: $(basename $pkg)"
done
```

### Completion Checklist
- [ ] All exports are named exports (no defaults)
- [ ] TypeScript definitions generated for all packages
- [ ] All package.json have description, license, keywords
- [ ] All packages have README.md and LICENSE
- [ ] Repository field points to correct monorepo location

---

## Step 5: Validate Complete Unification

### Final Validation Checklist

```bash
# Structure: All packages have src/index.mjs
for pkg in packages/*/; do
  [ -f "$pkg/src/index.mjs" ] || echo "Missing: $(basename $pkg)/src/index.mjs"
done

# Build: All packages build successfully
pnpm run build
# Check: All dist/ directories exist
for pkg in packages/*/; do
  [ -d "$pkg/dist" ] || echo "Missing dist: $(basename $pkg)"
done

# Lint: 0 violations
pnpm run lint
# Check exit code is 0

# Tests: All pass with ≥80% coverage
pnpm run coverage
# Check: All packages have ≥80% coverage
# Check exit code is 0

# Dependencies: Single version per shared dep
pnpm list
# Manual verification: no version conflicts

# Metadata: All packages complete
for pkg in packages/*/package.json; do
  jq -e '.description and .license and .keywords' "$pkg" > /dev/null || echo "Missing metadata: $(dirname $pkg)"
done
```

### Expected Results

- ✅ **Structure**: 17/17 packages have `src/index.mjs`
- ✅ **Build**: `pnpm run build` succeeds in <30s
- ✅ **Linting**: `pnpm run lint` produces 0 violations
- ✅ **Tests**: All tests pass, ≥80% coverage in all packages
- ✅ **Dependencies**: No version conflicts, no unused dependencies
- ✅ **Exports**: All named exports, TypeScript definitions generated
- ✅ **Metadata**: All packages have description, license, README, keywords

## Troubleshooting

### Build fails with "Cannot resolve..."
- Check that all imports in `src/` reference actual files
- Verify no circular dependencies: `pnpm madge --circular packages/*/src/index.mjs`

### Tests fail after structure migration
- Ensure test file paths updated to `src/*.test.mjs`
- Check test imports reference new structure paths

### Linting produces violations
- Fix all violations (no suppression comments allowed)
- Common: trailing whitespace, unused variables, unquoted strings

### Dependency version conflicts
- Resolve to highest compatible version
- Run full test suite after resolution
- Update all packages to same version

## Next Steps

After successful validation:
1. Create git commit with all changes
2. Run `/speckit.tasks` to generate detailed task list
3. Begin implementation with Task story priorities (P1, P2)
