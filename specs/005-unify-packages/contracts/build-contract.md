# Build System Contract

**Feature**: 005-unify-packages
**Component**: esbuild unified build system
**Status**: Design specification

## Purpose

Single esbuild configuration serving all 17 packages in UNRDF monorepo. Replaces per-package build scripts with unified, declarative build process.

## Interface

### Input

**Command**:
```bash
pnpm run build
```

**Environment**:
- Node.js 18+
- pnpm 8.0+
- /esbuild.config.mjs configured
- All package.json files valid

**Preconditions**:
- All packages have `src/index.mjs` entry point
- No circular dependencies
- All dependencies resolved in pnpm-lock.yaml

### Process

1. **Read Configuration**: Load `/esbuild.config.mjs`
2. **Discover Packages**: Identify all 17 packages in `packages/*/`
3. **Validate Structure**: Verify each has `src/index.mjs`
4. **Build ESM**: Generate `dist/index.mjs` for each package
5. **Build CJS**: Generate `dist/index.cjs` for each package (optional)
6. **Generate Types**: Create `dist/index.d.ts` (via dts plugin or tsc)
7. **Sourcemaps**: Create `.map` files for debugging
8. **Output Statistics**: Report build time, file sizes

### Output

**Success** (exit code 0):
```
✓ Build successful
  17 packages built
  Total size: XXX KB
  Build time: XXX ms
```

**Files Created**:
- `packages/{name}/dist/index.mjs` (ESM distribution)
- `packages/{name}/dist/index.cjs` (CommonJS distribution, optional)
- `packages/{name}/dist/index.d.ts` (TypeScript definitions)
- `packages/{name}/dist/index.js.map` (source maps)

**Failure** (exit code 1):
- Syntax errors in source
- Broken imports
- Circular dependencies detected
- Build takes >30 seconds (performance regression)

## Contracts

### Build Configuration Schema

```javascript
// /esbuild.config.mjs
export default {
  entryPoints: Array<string>,     // e.g., ['packages/core/src/index.mjs', ...]
  outdir: string,                 // Output directory for each package
  bundle: false,                  // Libraries don't bundle dependencies
  minify: boolean,                // true for prod, false for dev
  sourcemap: true,                // Always generate sourcemaps
  target: 'es2020',               // ES module target
  platform: 'node',               // Node.js platform
  format: ['esm', 'cjs'],         // Output both formats
  external: ['node_modules'],     // Don't bundle node_modules
  plugins: [
    dtsPlugin(),                  // Generate .d.ts files
  ],
};
```

### Package Output Contract

Each package must have after build:

```
packages/{name}/dist/
├── index.mjs              # ESM distribution
├── index.mjs.map          # ESM source map
├── index.cjs              # CommonJS distribution (optional)
├── index.cjs.map          # CJS source map (optional)
└── index.d.ts             # TypeScript definitions
```

### Entry Point Contract

All packages export from single entry point:

```javascript
// src/index.mjs (source)
export { api1, api2, api3 } from './modules.mjs';

// dist/index.mjs (after build)
// Same named exports
```

### Dependency Resolution Contract

```
package.json:
  dependencies: {
    "oxigraph": "1.2.0",
    "zod": "3.20.0"
  }

Expects:
  node_modules/oxigraph/package.json version: 1.2.0
  node_modules/zod/package.json version: 3.20.0
```

## Performance Contract

- Build all 17 packages in <30 seconds (single machine)
- No per-package custom build scripts
- Parallelization via esbuild's internal workers
- No significant memory growth (baseline ~500MB)

## Compatibility Contract

- Output ESM is ES2020 compatible
- Output CommonJS is CommonJS compatible
- TypeScript definitions are type-correct (0 type errors)
- Source maps map correctly to original source

## Test Cases

### Test Case 1: Successful build of all packages
```bash
$ pnpm run build
# All 17 packages build successfully
# Exit code: 0
```

### Test Case 2: Build fails on syntax error
```bash
# Introduce syntax error in one package src/
$ pnpm run build
# Error: Syntax error in packages/core/src/index.mjs
# Exit code: 1
```

### Test Case 3: Build fails on broken import
```bash
# src/ imports non-existent module
$ pnpm run build
# Error: Cannot resolve './nonexistent.mjs'
# Exit code: 1
```

### Test Case 4: TypeScript definitions generated
```bash
$ pnpm run build
# Check: packages/*/dist/index.d.ts files exist
# Check: TypeScript definitions compile (0 errors)
```

## Validation

- ✅ `pnpm run build` builds all 17 packages
- ✅ Build time <30 seconds
- ✅ All dist/ files created correctly
- ✅ Source maps functional
- ✅ TypeScript definitions accurate
- ✅ No per-package build scripts
