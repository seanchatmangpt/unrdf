# UNRDF Monorepo Development Guide

**Last Updated**: 2025-12-20
**Version**: 5.0.1+ (Post-Unification)

## Overview

This is a **unified monorepo** using **pnpm workspaces** with 21 packages organized by capability. After unification (Feature 005), all packages follow a consistent structure, tooling, and quality gates.

### Key Principles

✅ **Single dev workflow**: One set of commands for all packages
✅ **Consistent structure**: All packages follow `src/` → `index.mjs` → `dist/` pattern
✅ **Unified tooling**: Single esbuild, Ruff, Vitest config serving all packages
✅ **Zero-defect quality**: 400+ linting rules, ≥80% test coverage, 0 suppression comments
✅ **Deterministic builds**: <30s build time, identical lock files on clean install

---

## Quick Start

### Installation

```bash
# Clone repository
git clone https://github.com/seanchatmangpt/unrdf.git
cd unrdf

# Install dependencies (pnpm required)
pnpm install

# Verify structure (all 21 packages)
pnpm run check:structure
```

### Essential Commands

```bash
# Build all 21 packages (single config, <30s)
pnpm run build

# Run linting across all packages (0 violations required)
pnpm run lint

# Fix linting violations automatically
pnpm run lint:fix

# Run all tests with coverage (≥80% required)
pnpm run test:coverage

# Run tests in watch mode (development)
pnpm run test:watch

# Check for circular dependencies
pnpm run check:deps

# Audit dependency versions
pnpm run deps:audit

# Clean all builds and node_modules
pnpm run clean
```

---

## Project Structure

### 21 Core Packages

```
packages/
├── atomvm/              # Atomic value machine
├── browser/             # Browser runtime
├── cli/                 # Command-line interface
├── composables/         # Composable functions
├── core/                # Core RDF engine
├── dark-matter/         # Dark matter (schema-less RDF)
├── docs/                # Documentation package
├── domain/              # Domain models
├── engine-gateway/      # Engine gateway
├── federation/          # Federation support
├── hooks/               # Knowledge hooks system
├── kgc-4d/              # Knowledge graph compiler (4D)
├── kgn/                 # Knowledge graph notation
├── knowledge-engine/    # Knowledge engine
├── nextra/              # Nextra documentation
├── oxigraph/            # RDF store bindings
├── project-engine/      # Project engine
├── react/               # React integration
├── streaming/           # Streaming support
├── test-utils/          # Testing utilities
└── validation/          # Validation system
```

### Unified Package Structure

Every package follows this pattern:

```
packages/{name}/
├── src/                              # Source code
│   ├── index.mjs                     # Main entry point (named exports only)
│   ├── module1.mjs                   # Feature modules
│   ├── module2.mjs
│   └── {name}.test.mjs               # Tests co-located with source
│
├── dist/                             # Built output (generated)
│   ├── index.mjs                     # ESM distribution
│   ├── index.cjs                     # CommonJS distribution (if needed)
│   ├── index.d.ts                    # TypeScript definitions (auto-generated)
│   └── index.js.map                  # Source maps
│
├── package.json                      # Standard metadata
├── README.md                         # Package documentation
└── LICENSE                           # Copy of monorepo MIT license
```

---

## Development Workflow

### 1. Create New Feature

```bash
# Branch from main
git checkout -b feat/my-feature

# Make changes in any package
vim packages/{name}/src/module.mjs

# Create or update tests
vim packages/{name}/src/module.test.mjs
```

### 2. Verify Quality Gates

```bash
# Run linting (must be 0 violations)
pnpm run lint

# If violations, fix automatically
pnpm run lint:fix

# Run tests with coverage (must be ≥80%)
pnpm run test:coverage

# Check for circular dependencies
pnpm run check:deps
```

### 3. Build & Test

```bash
# Build all packages
pnpm run build

# Run full test suite
pnpm test

# Or watch mode for development
pnpm run test:watch
```

### 4. Commit & Push

```bash
# Stage changes
git add .

# Commit (pre-commit hooks run lint + test:fast)
git commit -m "feat: description of feature"

# Push
git push origin feat/my-feature
```

---

## Unified Tooling

### Build: esbuild

**Config**: `/esbuild.config.mjs`

Single configuration builds all 21 packages:
- Input: `packages/*/src/index.mjs`
- Output: `packages/*/dist/index.{mjs,cjs,d.ts}`
- Performance: <30 seconds for all packages
- Features: Dual output (ESM + CJS), source maps, TypeScript definitions

### Linting: Ruff

**Config**: `/pyproject.toml`

400+ linting rules with zero suppression comments allowed:
- Target: JavaScript/TypeScript files in `packages/*/src/`
- Rules: E, F, W, I, S, C, etc. (400+ total)
- Line length: 100 characters max
- No `# noqa` or suppression comments

```bash
# Check all packages
pnpm run lint

# Fix violations automatically
pnpm run lint:fix
```

### Testing: Vitest

**Config**: `/vitest.config.unified.mjs`

Modern test runner with coverage reporting:
- Provider: v8 (built-in, no extra dependencies)
- Test pattern: `packages/*/src/**/*.test.mjs`
- Coverage thresholds: 80% minimum
- Reporters: text, JSON, HTML

```bash
# Run all tests with coverage
pnpm run test:coverage

# Watch mode (development)
pnpm run test:watch

# Specific package
pnpm -C packages/{name} test
```

---

## Adding a New Package

### Step 1: Create Package Structure

```bash
mkdir -p packages/my-package/src
cd packages/my-package
```

### Step 2: Create package.json

```json
{
  "name": "@unrdf/my-package",
  "version": "0.0.1",
  "description": "Brief description of package purpose",
  "type": "module",
  "main": "dist/index.mjs",
  "exports": {
    ".": {
      "types": "./dist/index.d.ts",
      "import": "./dist/index.mjs",
      "require": "./dist/index.cjs"
    }
  },
  "files": ["dist", "README.md", "LICENSE"],
  "scripts": {},
  "keywords": ["rdf", "knowledge-graph", "your-keyword"],
  "license": "MIT",
  "repository": {
    "type": "git",
    "url": "https://github.com/seanchatmangpt/unrdf",
    "directory": "packages/my-package"
  }
}
```

### Step 3: Create Entry Point

```bash
cat > src/index.mjs << 'EOF'
/**
 * @unrdf/my-package - Package description
 * @module
 */

/**
 * Example function
 * @param {string} input - Input parameter
 * @returns {string} Output
 */
export function myFunction(input) {
  return `Result: ${input}`;
}
EOF
```

### Step 4: Create Tests

```bash
cat > src/index.test.mjs << 'EOF'
import { describe, it, expect } from 'vitest';
import { myFunction } from './index.mjs';

describe('@unrdf/my-package', () => {
  it('myFunction returns formatted result', () => {
    expect(myFunction('test')).toBe('Result: test');
  });
});
EOF
```

### Step 5: Create README

```bash
cat > README.md << 'EOF'
# @unrdf/my-package

Description of what this package does.

## Installation

```bash
pnpm add @unrdf/my-package
```

## Usage

```javascript
import { myFunction } from '@unrdf/my-package';

console.log(myFunction('input'));
```

## API

- `myFunction(input)`: Brief description
EOF
```

### Step 6: Copy License

```bash
cp ../LICENSE LICENSE
```

### Step 7: Verify

```bash
# From monorepo root
pnpm run check:structure
pnpm run build
pnpm run test
```

---

## Common Tasks

### Run Tests for Single Package

```bash
pnpm -C packages/core test
pnpm -C packages/hooks test
```

### Debug with Node Inspector

```bash
# Watch mode with debugging
pnpm run test:watch -- --inspect-brk packages/core

# Then connect debugger to chrome://inspect
```

### Profile Build Performance

```bash
# Measure build time
time pnpm run build

# Profile with Node
node --prof esbuild.config.mjs
node --prof-process isolate-*.log > prof.txt
```

### Update All Dependencies

```bash
# Check for updates
pnpm update --interactive

# Update and install
pnpm install
```

### Analyze Dependency Graph

```bash
# Show dependency statistics
pnpm run deps:stats

# Generate visual graph
pnpm run deps:graph
```

---

## Quality Gates (Mandatory)

All of these must pass before code can be merged:

### ✅ Linting: 0 Violations

```bash
pnpm run lint
# Exit code must be 0
# No suppression comments allowed
```

### ✅ Testing: 100% Pass Rate

```bash
pnpm run test
# All tests must pass
# No skipped or flaky tests
```

### ✅ Coverage: ≥80% Per Package

```bash
pnpm run test:coverage
# Check coverage/index.html for detailed report
# All packages must meet 80% threshold
```

### ✅ Build: <30 Seconds

```bash
time pnpm run build
# Must complete in < 30 seconds on single machine
```

### ✅ Structure: All Packages Consistent

```bash
pnpm run check:structure
# Must pass with 21/21 packages verified
```

### ✅ Circular Dependencies: 0 Found

```bash
pnpm run check:deps
# Must report no circular dependencies
```

---

## Troubleshooting

### Issue: Build fails with "Cannot resolve..."

**Solution**: Check that all imports reference actual files in `src/`:
```bash
# Verify file exists
ls packages/{name}/src/{imported-file}.mjs

# Check for circular dependencies
pnpm run check:deps
```

### Issue: Tests fail after structure migration

**Solution**: Verify test paths reference new `src/` location:
```bash
# Tests should be in src/{name}.test.mjs or src/__tests__/
find packages -name "*.test.mjs" | head -10
```

### Issue: Linting produces violations

**Solution**: Fix violations automatically:
```bash
# Auto-fix
pnpm run lint:fix

# Check what was fixed
git diff

# Commit
git add . && git commit -m "fix: linting violations"
```

### Issue: "pnpm not found"

**Solution**: Install pnpm:
```bash
npm install -g pnpm@latest
pnpm --version  # Should be 8.0+
```

### Issue: Dependencies not installing correctly

**Solution**: Rebuild lock file:
```bash
# Remove lock file
rm pnpm-lock.yaml

# Clean install
pnpm install

# Verify determinism
pnpm install --frozen-lockfile
```

---

## Contributing

1. **Create feature branch** from `main`
2. **Make changes** in any package under `packages/*/src/`
3. **Write tests** co-located with source (`.test.mjs`)
4. **Run quality gates**:
   ```bash
   pnpm run lint
   pnpm run test
   pnpm run build
   ```
5. **Commit** with descriptive messages
6. **Create PR** with summary of changes
7. **Wait for CI** to pass all checks
8. **Merge** when approved

---

## Resources

- **RDF Specs**: https://www.w3.org/TR/rdf11-concepts/
- **SPARQL**: https://www.w3.org/TR/sparql11-query/
- **esbuild**: https://esbuild.github.io/
- **Ruff**: https://docs.astral.sh/ruff/
- **Vitest**: https://vitest.dev/
- **pnpm**: https://pnpm.io/

---

**Generated**: 2025-12-20
**Status**: ✅ Ready for unified development
