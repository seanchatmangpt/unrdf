# UNRDF Workspace Structure

This document describes the standardized file layout for packages in the UNRDF monorepo.

## Root Directory Structure

```
/home/user/unrdf/
├── packages/                      # 17 monorepo packages
│   ├── core/                      # @unrdf/core
│   ├── oxigraph/                  # @unrdf/oxigraph
│   ├── hooks/                     # @unrdf/hooks
│   ├── streaming/                 # @unrdf/streaming
│   ├── federation/                # @unrdf/federation
│   ├── knowledge-engine/          # @unrdf/knowledge-engine
│   ├── browser/                   # @unrdf/browser
│   ├── cli/                       # @unrdf/cli
│   ├── react/                     # @unrdf/react
│   ├── composables/               # @unrdf/composables
│   ├── dark-matter/               # @unrdf/dark-matter
│   ├── project-engine/            # @unrdf/project-engine
│   ├── engine-gateway/            # @unrdf/engine-gateway
│   ├── test-utils/                # @unrdf/test-utils (private)
│   ├── validation/                # @unrdf/validation (private)
│   └── domain/                    # @unrdf/domain (private)
│
├── docs/                          # Documentation (100+ files)
│   ├── START-HERE.md              # Quick orientation
│   ├── MONOREPO-QUICK-REFERENCE.md  # Package matrix
│   ├── LOCAL-DEVELOPMENT.md       # Dev environment setup
│   ├── WORKSPACE-STRUCTURE.md     # This file
│   ├── PACKAGE-DEVELOPMENT.md     # Add new packages
│   ├── TESTING-STRATEGY.md        # Testing guide
│   ├── ARCHITECTURE.md            # System design
│   ├── PACKAGES.md                # Package details
│   ├── GETTING-STARTED/           # Tutorials
│   ├── guides/                    # How-to guides
│   ├── explanation/               # Conceptual docs
│   └── audit/                     # Compliance & benchmarks
│
├── examples/                      # Example projects
├── src/                           # Legacy source (being migrated)
├── test/                          # Shared test utilities
├── scripts/                       # Build & automation scripts
│
├── root package.json              # Monorepo config & scripts
├── pnpm-workspace.yaml            # pnpm monorepo definition
├── tsconfig.json                  # TypeScript config (if needed)
├── .eslintrc.js                   # ESLint config
├── .prettierrc.js                 # Prettier config
├── CLAUDE.md                      # Development guidelines
├── CONTRIBUTING.md                # Contributing guide
├── README.md                      # Main documentation
└── LICENSE                        # MIT License
```

---

## Standard Package Structure

Every package in `/packages/*` follows this structure:

```
packages/core/
├── src/                           # Source files (JavaScript with JSDoc)
│   ├── index.mjs                  # Main entry point
│   ├── api/                       # Public API
│   │   ├── knowledge-substrate.mjs
│   │   ├── store.mjs
│   │   └── query.mjs
│   ├── internal/                  # Internal implementation (not exported)
│   │   ├── parser.mjs
│   │   ├── executor.mjs
│   │   └── validator.mjs
│   ├── types/                     # Type definitions & Zod schemas
│   │   ├── store.mjs
│   │   ├── query.mjs
│   │   └── index.mjs
│   └── utils/                     # Utilities
│       ├── logger.mjs
│       ├── validator.mjs
│       └── index.mjs
│
├── test/                          # Test files (Vitest)
│   ├── api/                       # Tests for public API
│   │   ├── knowledge-substrate.test.mjs
│   │   ├── store.test.mjs
│   │   └── query.test.mjs
│   ├── internal/                  # Tests for internal modules
│   │   ├── parser.test.mjs
│   │   ├── executor.test.mjs
│   │   └── validator.test.mjs
│   ├── integration/               # Integration tests
│   │   ├── sparql.test.mjs
│   │   ├── shacl.test.mjs
│   │   └── transactions.test.mjs
│   ├── fixtures/                  # Test data
│   │   ├── sample-graph.ttl
│   │   ├── shapes.ttl
│   │   └── queries.sparql
│   └── setup.mjs                  # Test configuration
│
├── dist/                          # Built output (generated)
│   ├── index.mjs
│   ├── api/
│   └── [minified & transpiled]
│
├── docs/                          # Package-specific documentation
│   ├── README.md                  # Package overview
│   ├── ARCHITECTURE.md            # Package internals
│   ├── API.md                     # API reference
│   └── EXAMPLES.md                # Usage examples
│
├── package.json                   # Package configuration
├── tsconfig.json                  # TypeScript config (if needed)
├── vitest.config.mjs              # Test configuration
├── .eslintrc.js                   # ESLint config (if custom)
└── README.md                      # Quick readme
```

---

## Naming Conventions

### File Naming

| Type | Pattern | Example |
|------|---------|---------|
| **Source files** | camelCase | `queryExecutor.mjs` |
| **Test files** | `*.test.mjs` | `queryExecutor.test.mjs` |
| **Constants** | UPPER_SNAKE_CASE | `MAX_GRAPH_SIZE.mjs` |
| **Directories** | kebab-case | `internal/`, `type-defs/`, `test-utils/` |

### Export Naming

```javascript
// Good - named exports for clarity
export { createStore, parseRdf, query };

// Good - default export for main API
export { createKnowledgeSubstrateCore as default };

// Internal - not exported, stays internal
// function internalHelper() { ... }
```

### Module Naming

```
packages/core/
├── src/
│   ├── index.mjs                  # Main export
│   ├── api/knowledge-substrate.mjs  # Public API module
│   ├── internal/parser.mjs        # Internal (not exported)
│   └── types/store.mjs            # Type schemas
```

---

## Monorepo Conventions

### Package Naming

**Format:** `@unrdf/<package-name>`

**Examples:**
```
@unrdf/core           (essential)
@unrdf/oxigraph       (backend)
@unrdf/hooks          (behaviors)
@unrdf/test-utils     (internal)
```

### Version Numbering

- **Root:** 5.0.0-alpha.0 (monorepo version)
- **Packages:** Independent semver (e.g., core might be v1.2.3 while hooks is v2.0.1)
- **Pre-release:** `-alpha`, `-beta`, `-rc` for development versions

### Dependency Management

**Cross-package dependencies:**
```json
{
  "dependencies": {
    "@unrdf/core": "workspace:*",
    "@unrdf/oxigraph": "workspace:*"
  }
}
```

The `workspace:*` prefix means "always use the local version in this monorepo."

---

## Source Code Organization (src/)

### Entry Point Structure

Each package has a single entry point (`src/index.mjs`):

```javascript
// src/index.mjs
// Export all public APIs

export { createKnowledgeSubstrateCore } from './api/knowledge-substrate.mjs';
export { createStore } from './api/store.mjs';
export { query } from './api/query.mjs';

// Type exports (for TypeScript)
export { /* types */ };
```

### API vs Internal Split

**Public API** (`src/api/`):
- Exported from `src/index.mjs`
- Stable contract with backward compatibility
- Well-documented
- Examples: `knowledge-substrate.mjs`, `store.mjs`, `query.mjs`

**Internal** (`src/internal/`):
- NOT exported, not part of public API
- Can change without notice
- No backward compatibility guarantees
- Examples: `parser.mjs`, `executor.mjs`, `validator.mjs`

### Type Definitions (`src/types/`)

Zod schemas for input validation:

```javascript
// src/types/store.mjs
import { z } from 'zod';

export const StoreConfigSchema = z.object({
  backend: z.enum(['memory', 'oxigraph']),
  persistence: z.boolean().optional(),
  maxTriples: z.number().optional()
});

export const { /* types */ } = StoreConfigSchema;
```

### Utilities (`src/utils/`)

Shared helpers:

```javascript
// src/utils/logger.mjs
export function debug(msg, ...args) {
  if (process.env.DEBUG) {
    console.debug(`[DEBUG] ${msg}`, ...args);
  }
}

export function warn(msg, ...args) {
  console.warn(`[WARN] ${msg}`, ...args);
}
```

---

## Test Organization (test/)

### Test File Structure

```
test/
├── api/                     # Public API tests
│   └── knowledge-substrate.test.mjs
├── internal/                # Internal implementation tests
│   └── parser.test.mjs
├── integration/             # Cross-module integration tests
│   └── sparql.test.mjs
├── fixtures/                # Test data
│   ├── sample-graph.ttl
│   └── queries.sparql
└── setup.mjs                # Vitest configuration
```

### Test File Naming

- **Mirrors source:** `src/api/store.mjs` → `test/api/store.test.mjs`
- **Suffixed:** Always ends with `.test.mjs`
- **Pattern:** `<module>.test.mjs`

### Test Setup

```javascript
// test/setup.mjs
import { beforeAll, afterEach } from 'vitest';

beforeAll(() => {
  // Global setup for all tests
});

afterEach(() => {
  // Clean up after each test
});
```

### Fixtures

Test data lives in `test/fixtures/`:

```
test/fixtures/
├── sample-graph.ttl         # RDF test data
├── shapes.ttl               # SHACL shapes
└── queries.sparql           # SPARQL queries
```

---

## Package Configuration (package.json)

### Required Fields

```json
{
  "name": "@unrdf/core",
  "version": "1.0.0",
  "type": "module",
  "description": "RDF storage with SPARQL & SHACL",
  "license": "MIT",

  "main": "./dist/index.mjs",
  "exports": {
    ".": "./dist/index.mjs",
    "./types": "./dist/types/index.mjs"
  },

  "files": ["dist/", "README.md"],

  "scripts": {
    "build": "...",
    "test": "vitest run",
    "lint": "ruff check src test"
  },

  "dependencies": {
    "@unrdf/oxigraph": "workspace:*"
  },

  "devDependencies": {
    "vitest": "^1.0.0"
  }
}
```

### Script Conventions

```json
{
  "scripts": {
    "build": "tsc || true",           // Build transpilation
    "test": "vitest run",              // Run tests once
    "test:watch": "vitest",            // Watch mode
    "lint": "ruff check src test",     // Lint code
    "lint:fix": "ruff check --fix src test"  // Fix issues
  }
}
```

---

## Documentation in Packages

### Package README.md

Located at `packages/*/README.md`:

```markdown
# @unrdf/core

Brief description of package.

## Installation

```bash
npm install @unrdf/core
```

## Quick Start

Minimal working example.

## API Reference

Link to full API docs.

## Examples

Links to example usage.

## See Also

Related packages.
```

### Package Docs/ Directory

More detailed docs in `packages/*/docs/`:

```
packages/core/docs/
├── README.md           # Package overview
├── ARCHITECTURE.md     # Internal design
├── API.md              # Detailed API reference
└── EXAMPLES.md         # Usage examples
```

---

## Build Output (dist/)

Generated during `pnpm run build`:

```
packages/core/dist/
├── index.mjs           # Main export
├── api/                # Public APIs (transpiled)
├── types/              # Type definitions
└── internal/           # Internal code (if exposed for debugging)
```

**Important:** Never edit files in `dist/`. Edit source in `src/` and rebuild.

---

## Sharing Code Across Packages

### Option 1: Use Existing Package

If code is useful across packages, put it in a shared package:

```javascript
// In any package
import { logger } from '@unrdf/core';  // Reuse from core
```

### Option 2: Create Utility Package

For shared utilities:

```bash
# Create a new package
mkdir packages/shared-utils
# ... follow standard structure
# Export from packages/shared-utils/src/index.mjs
```

### Option 3: Keep Internal

If code is specific to one package, keep it in `src/internal/`:

```javascript
// Only used within this package
// src/internal/parser.mjs
function parseTriples(data) { ... }  // Not exported
```

---

## Adding Files to a Package

### Adding a New API

1. **Create file in `src/api/`:**
   ```bash
   touch packages/core/src/api/federation.mjs
   ```

2. **Implement the API:**
   ```javascript
   // src/api/federation.mjs
   export function federatedQuery(stores, sparql) {
     // Implementation
   }
   ```

3. **Export from `src/index.mjs`:**
   ```javascript
   export { federatedQuery } from './api/federation.mjs';
   ```

4. **Add tests in `test/api/`:**
   ```bash
   touch packages/core/test/api/federation.test.mjs
   ```

5. **Update package README** if it's a major feature

### Adding Internal Code

No export needed - it's automatically internal:

```bash
touch packages/core/src/internal/new-module.mjs
```

```javascript
// src/internal/new-module.mjs
function internalHelper() { ... }  // Not exported
```

---

## Legacy Code (src/)

The `/src` directory at the root contains legacy code being gradually migrated.

**Migration strategy:**
1. Extract working code to appropriate packages
2. Move to `packages/*/src/`
3. Follow standard package structure
4. Eventually delete from `/src`

---

## Examples Directory

```
examples/
├── basic/                   # Minimal SPARQL example
├── react-app/              # React integration
├── streaming/              # Large graph processing
├── federation/             # Distributed queries
└── [more examples]
```

Each example should:
- Have its own `package.json`
- Include a `README.md` explaining the example
- Be runnable with `npm install && npm start`
- Demonstrate a specific feature or pattern

---

## Monorepo Git Workflow

### Commit Structure

Organize commits by package or feature:

```bash
# Feature across multiple packages
git commit -m "feat: add federation support

- Implements distributed query execution
- Updates core, hooks, federation packages
- Adds 15 new tests across packages"

# Single package feature
git commit -m "feat(core): optimize SPARQL execution"

# Fix in multiple packages
git commit -m "fix: memory leak in hook execution

Affects: hooks, core, streaming"
```

---

## Troubleshooting Package Layout

### Issue: "Cannot find module @unrdf/core"

**Solution:** Ensure cross-package dependency uses `workspace:*`:

```json
{
  "dependencies": {
    "@unrdf/core": "workspace:*"
  }
}
```

### Issue: Changes not reflected

**Solution:** Rebuild both packages:

```bash
pnpm --filter @unrdf/core run build
pnpm --filter @unrdf/hooks run build
```

### Issue: Tests can't import from src/

**Solution:** Use the relative path import:

```javascript
// Good - relative
import { parser } from '../src/internal/parser.mjs';

// Wrong - assumes built dist/
import { parser } from '@unrdf/core/internal/parser.mjs';
```

---

## Summary Checklist

When creating or modifying a package:

- [ ] **Structure:** Follows `src/`, `test/`, `docs/` layout
- [ ] **Exports:** Public APIs in `src/api/`, internal in `src/internal/`
- [ ] **Naming:** camelCase files, UPPER_SNAKE_CASE for constants
- [ ] **Testing:** Every `.mjs` in `src/` has matching `.test.mjs`
- [ ] **Documentation:** `README.md` and `docs/` explain the package
- [ ] **Dependencies:** Cross-package deps use `workspace:*`
- [ ] **Build:** `pnpm run build` succeeds without errors
- [ ] **Tests:** `pnpm test` passes for the package

---

**Next:** → [PACKAGE-DEVELOPMENT.md](PACKAGE-DEVELOPMENT.md) to create a new package, or [LOCAL-DEVELOPMENT.md](LOCAL-DEVELOPMENT.md) for setup.
