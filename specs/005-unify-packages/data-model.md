# Data Model: Package Unification

**Feature**: 005-unify-packages
**Phase**: Phase 1 Design
**Date**: 2025-12-20

## Core Entities

### Package

**What it represents**: A self-contained, versioned unit of functionality in the monorepo

**Key Attributes**:
- `name` (string): npm package name (e.g., @unrdf/core)
- `version` (semver): Current version of the package
- `description` (string): Purpose and what the package does
- `license` (string): License identifier (inherited from monorepo, typically MIT)
- `repository` (object): Points to monorepo with directory path
- `main` (string): Entry point for CommonJS (dist/index.cjs)
- `exports` (object): Conditional exports for ESM/CommonJS
- `keywords` (array<string>): For npm discoverability
- `path` (string): Absolute path in monorepo (packages/{name}/)

**Public API Fields**:
- `src/index.mjs`: Source entry point (exported at build time)
- `dist/index.mjs`: Built ESM distribution
- `dist/index.cjs`: Built CommonJS distribution (optional)
- `dist/index.d.ts`: TypeScript definitions (generated)

**Private Fields** (not exported):
- `src/internal/*`: Internal modules
- `src/utils/*`: Helper utilities (if not publicly exposed)
- Test files (`*.test.mjs`)
- Build configs

**Relationships**:
- `depends_on` [Package]: Internal dependencies via @unrdf/* packages
- `exports` [PublicAPI]: Public API boundary
- `contains` [SourceModule]: Module files in src/

---

### SourceModule

**What it represents**: A JavaScript module file in src/

**Key Attributes**:
- `filename` (string): Module filename (e.g., core-engine.mjs)
- `path` (string): Relative path in src/ (e.g., src/core-engine.mjs)
- `exports` (array<string>): Named exports from this module
- `imports` (array<string>): Internal and external imports
- `lines_of_code` (number): For size tracking
- `test_coverage` (number %): Line coverage percentage

**Validation Rules**:
- Must be named export (not default)
- Must have JSDoc type annotations on all exports (100% type coverage)
- Must have corresponding test file (*.test.mjs) or documented exception
- Must not expose internal utilities
- Must not re-export transitive dependencies

**Relationships**:
- `part_of` [Package]
- `imports_from` [SourceModule]: Internal module imports
- `imports_external` [NPMPackage]: External dependencies

---

### PackageManifest (package.json)

**What it represents**: Configuration and metadata for a package

**Key Attributes**:
- `name` (string): npm package name
- `version` (semver): Current package version
- `type` (string): "module" (required for ES modules)
- `main` (string): Entry point for CommonJS consumers
- `exports` (object): Conditional exports with exact field structure:
  ```json
  {
    ".": {
      "types": "./dist/index.d.ts",
      "import": "./dist/index.mjs",
      "require": "./dist/index.cjs"
    }
  }
  ```
- `files` (array): Array of files/dirs to include in npm publish
- `scripts` (object): npm scripts (build, test, lint inherited from root)
- `dependencies` (object): Production dependencies
- `devDependencies` (object): Development dependencies
- `description` (string): One-line package description
- `keywords` (array<string>): Min 3, max 10 relevant keywords
- `license` (string): "MIT" (matching monorepo)
- `repository` (object):
  ```json
  {
    "type": "git",
    "url": "https://github.com/seanchatmangpt/unrdf",
    "directory": "packages/{package-name}"
  }
  ```

**Validation Rules**:
- Must have `name`, `version`, `description`, `license`
- Must have `type: "module"`
- Must have `main`, `exports`, `files` fields
- `keywords` must be 3+ and 10 or fewer
- All `dependencies` must be actually used in code
- No `devDependencies` in production dependencies
- `scripts` uses shared monorepo commands (no package-specific overrides except exceptions)

---

### BuildConfig (esbuild.config.mjs)

**What it represents**: Unified build configuration at monorepo root

**Key Attributes**:
- `entryPoints` (array): All packages as entry points
- `outdir` (string): dist/ directory per package
- `bundle` (boolean): false (we're building libraries, not bundles)
- `minify` (boolean): true for production builds
- `sourcemap` (boolean): true for debugging
- `target` (string): "es2020" (ES modules support)
- `platform` (string): "node"
- `format` (array): ["esm", "cjs"] (dual output)
- `external` (array): All node_modules are external (not bundled)

**Global Configuration**:
- Single esbuild config at root: `/esbuild.config.mjs`
- All 17 packages build with identical settings (no overrides)
- Output structure: `packages/{name}/dist/index.{mjs,cjs,d.ts}`

---

### LintConfig (ruffrc.toml or pyproject.toml)

**What it represents**: Unified linting rules for all packages

**Key Attributes**:
- `rules` (array): 400+ rules enabled (per constitution)
- `ignore` (array): Patterns to ignore (dist/, node_modules/)
- `line_length` (number): 100 characters max
- `target_version` (string): es2020
- `suppress` (array): Empty array (NO suppression comments allowed)

**Validation Rules**:
- All 17 packages linted with exact same config
- NO per-package .eslintrc or overrides
- Build fails if any violations found (0 suppression comments allowed)
- CI/CD gate: `pnpm run lint` must pass with 0 violations

---

### TestConfig (vitest.config.mjs)

**What it represents**: Unified test runner configuration

**Key Attributes**:
- `test` (object):
  - `environment` (string): "node"
  - `coverage` (object):
    - `provider` (string): "v8"
    - `reporter` (array): ["text", "json", "html"]
    - `lines` (number): 80 (minimum coverage threshold)
    - `branches` (number): 80
    - `functions` (number): 80
    - `statements` (number): 80
  - `globals` (boolean): true (for describe, it, expect)

**Validation Rules**:
- All tests run with Vitest
- Test files: `*.test.mjs` pattern only
- Coverage minimum: ≥80% per package
- Build fails if coverage below threshold
- All 17 packages report coverage

---

### DependencyConstraint

**What it represents**: Version constraint for a shared dependency

**Key Attributes**:
- `package_name` (string): npm package name (e.g., oxigraph, zod)
- `version` (string): Exact version (e.g., 1.2.0)
- `used_by` (array<Package>): Which packages declare this dependency
- `type` (enum): "production" | "development"
- `is_unused` (boolean): True if declared but not actually used

**Validation Rules**:
- Single version per shared dependency across all 17 packages
- All declared dependencies must be actually imported
- No unused dependencies in any package
- Transitive dependencies not re-exported

**Conflict Resolution**:
- If packages declare different versions: resolve to highest compatible version
- Test suite must pass with resolved version
- Document in breaking changes if incompatibility found

---

### ExportBoundary

**What it represents**: Public API surface of a package

**Key Attributes**:
- `package` (Package): Which package this boundary belongs to
- `public_exports` (array<string>): Intentionally exposed APIs (named exports from index.mjs)
- `internal_modules` (array<string>): NOT exported (src/internal/*, private utilities)
- `type_definitions` (string): Path to generated .d.ts file

**Validation Rules**:
- Only items in `public_exports` are available to consumers
- Internal modules cannot be imported from external code
- All public exports must have TypeScript definitions
- No default exports (only named exports)
- No transitive dependency re-exports

---

## Relationships & Constraints

### Dependency Graph

```
Package A
  ├── depends_on → Package B
  ├── depends_on → Package C
  │   └── depends_on → Package D
  └── uses_external → NPM:zod
```

**Constraint**: No circular dependencies allowed
- Package A → B → C → A is INVALID
- Detected automatically during build

### Module Import Validation

```
src/index.mjs (public)
  ├── exports: [api1, api2, api3]
  ├── imports_from: src/engine.mjs
  ├── imports_from: src/utils.mjs
  └── imports_external: zod, oxigraph

src/engine.mjs (private to index.mjs)
  └── imports_from: src/utils.mjs

src/utils.mjs (private)
  └── imports_external: zod
```

**Constraint**: Only index.mjs exports to consumers; internal modules are private

### Version Alignment Constraint

```
Shared dependency: oxigraph
  ├── @unrdf/core: 1.2.0
  ├── @unrdf/streaming: 1.2.0 ✓
  ├── @unrdf/federation: 1.2.0 ✓
  └── All other packages: 1.2.0 ✓
```

**Constraint**: All packages using same shared dep must have identical version

---

## State Transitions

### Package Lifecycle

```
Created
  ↓ (structure migration)
Standardized
  ↓ (build/test/lint config applied)
Tested (≥80% coverage)
  ↓ (exports aligned)
Published (to npm)
  ↓ (updates only)
Updated
```

---

## Success Validation

### Structure Audit
- ✅ All 17 packages have `src/` directory
- ✅ All 17 packages have `src/index.mjs` as single entry point
- ✅ All 17 packages have JSDoc 100% type coverage
- ✅ All test files follow `*.test.mjs` pattern

### Quality Audit
- ✅ All 17 packages lint with 0 violations
- ✅ All 17 packages have ≥80% test coverage
- ✅ All 17 packages build successfully
- ✅ No circular dependencies detected

### Metadata Audit
- ✅ All 17 packages have description, license, keywords
- ✅ All 17 packages have README.md and LICENSE
- ✅ All 17 packages have correct repository field
- ✅ All shared dependencies at single version

### Export Audit
- ✅ All 17 packages use named exports only
- ✅ TypeScript definitions generated for all public packages
- ✅ No transitive dependency re-exports
- ✅ Public API boundary clearly documented
