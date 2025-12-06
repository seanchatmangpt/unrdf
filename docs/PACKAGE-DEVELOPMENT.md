# UNRDF Package Development Guide

This guide explains how to create new packages, modify existing packages, and manage dependencies in the UNRDF monorepo.

## Before You Start

- [ ] Have completed [LOCAL-DEVELOPMENT.md](LOCAL-DEVELOPMENT.md) setup
- [ ] Understand the [WORKSPACE-STRUCTURE.md](WORKSPACE-STRUCTURE.md) layout
- [ ] Know why you need a new package (don't create unless it's a logical boundary)

---

## When to Create a New Package

### ✅ DO Create a Package If:

- **Different dependency trees:** Package needs different dependencies than core
- **Optional feature:** Users might not need this functionality
- **Runtime choice:** Users choose between implementations (e.g., memory vs oxigraph backend)
- **Language boundary:** Integration with another language/system (e.g., browser, React, Vue)
- **Distribution:** Package is versioned/released independently

### ❌ DON'T Create a Package If:

- **Pure implementation detail:** It's internal logic (put in `src/internal/`)
- **Type definitions:** Use `src/types/` instead
- **Small utility:** Fits in `src/utils/`
- **Tightly coupled:** Can't be used independently
- **Adds bulk:** 99% of users would need this anyway

---

## Creating a New Package

### Step 1: Plan the Package

```javascript
// Decide on:
// - Name: @unrdf/<name>
// - Purpose: One sentence description
// - Dependencies: What does it depend on?
// - Exports: What's the public API?
// - Private: Public or internal to monorepo?

// Example: @unrdf/cache (hypothetical)
// Purpose: Caching layer for SPARQL queries
// Depends on: @unrdf/core
// Exports: { createQueryCache, invalidateCache }
// Public: yes (extends core)
```

### Step 2: Create Package Directory

```bash
# Create the package directory
mkdir -p packages/cache

# Create subdirectories
cd packages/cache
mkdir -p src test docs
```

### Step 3: Create package.json

```bash
touch packages/cache/package.json
```

```json
{
  "name": "@unrdf/cache",
  "version": "0.1.0",
  "description": "Query result caching for UNRDF",
  "type": "module",
  "license": "MIT",

  "main": "./dist/index.mjs",
  "exports": {
    ".": "./dist/index.mjs"
  },

  "files": [
    "dist/",
    "README.md"
  ],

  "scripts": {
    "build": "tsc || true",
    "test": "vitest run",
    "test:watch": "vitest",
    "lint": "ruff check src test"
  },

  "dependencies": {
    "@unrdf/core": "workspace:*"
  },

  "devDependencies": {
    "vitest": "^1.0.0"
  }
}
```

**Key points:**
- `"name"`: Always `@unrdf/<package-name>`
- `"version"`: Start at 0.1.0 or 1.0.0
- `"type": "module"`: Required for ESM
- Cross-package deps: Use `"workspace:*"`

### Step 4: Create Entry Point (src/index.mjs)

```bash
touch packages/cache/src/index.mjs
```

```javascript
// src/index.mjs
// Export all public APIs

export { createQueryCache } from './api/query-cache.mjs';
export { invalidateCache } from './api/cache-control.mjs';
```

### Step 5: Create API Modules (src/api/)

```bash
mkdir -p packages/cache/src/api
touch packages/cache/src/api/query-cache.mjs
touch packages/cache/src/api/cache-control.mjs
```

```javascript
// src/api/query-cache.mjs
import { z } from 'zod';

export const QueryCacheConfigSchema = z.object({
  maxSize: z.number().default(1000),
  ttl: z.number().default(5 * 60 * 1000)
});

export function createQueryCache(config) {
  const validated = QueryCacheConfigSchema.parse(config || {});
  // Implementation...
}
```

### Step 6: Create Tests (test/)

```bash
mkdir -p packages/cache/test/api
touch packages/cache/test/api/query-cache.test.mjs
touch packages/cache/test/setup.mjs
```

```javascript
// test/api/query-cache.test.mjs
import { describe, it, expect } from 'vitest';
import { createQueryCache } from '../../src/api/query-cache.mjs';

describe('QueryCache', () => {
  it('caches query results', () => {
    const cache = createQueryCache({ maxSize: 100 });
    // Test implementation...
  });
});
```

### Step 7: Create vitest.config.mjs

```bash
touch packages/cache/vitest.config.mjs
```

```javascript
import { defineConfig } from 'vitest/config';
import path from 'path';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node'
  },
  resolve: {
    alias: {
      '@': path.resolve(__dirname, './src')
    }
  }
});
```

### Step 8: Create Documentation

```bash
touch packages/cache/README.md
mkdir -p packages/cache/docs
touch packages/cache/docs/ARCHITECTURE.md
```

```markdown
# @unrdf/cache

Query result caching for UNRDF.

## Installation

```bash
npm install @unrdf/cache
```

## Quick Start

```javascript
import { createQueryCache } from '@unrdf/cache';

const cache = createQueryCache({ maxSize: 1000 });
// ... use cache
```

## API Reference

See [docs/API.md](docs/API.md).
```

### Step 9: Update Root package.json

No action needed! pnpm workspace automatically picks up new packages in `packages/`.

### Step 10: Build and Test

```bash
# Install dependencies
pnpm install

# Test your new package
pnpm --filter @unrdf/cache test

# Build it
pnpm --filter @unrdf/cache run build

# Verify it builds without errors
ls packages/cache/dist/
```

### Step 11: Add to MONOREPO-QUICK-REFERENCE.md

Update [MONOREPO-QUICK-REFERENCE.md](MONOREPO-QUICK-REFERENCE.md) to list your new package:

```markdown
| **@unrdf/cache** | Query result caching | `npm install @unrdf/cache` | 🟢 Beta | You want to cache SPARQL results |
```

---

## Modifying Existing Packages

### Adding a New Export

```bash
# 1. Create new module in src/api/
touch packages/core/src/api/new-feature.mjs

# 2. Implement the feature
# ... code ...

# 3. Export from src/index.mjs
# Edit packages/core/src/index.mjs and add:
# export { newFeature } from './api/new-feature.mjs';

# 4. Add tests
touch packages/core/test/api/new-feature.test.mjs

# 5. Build and test
pnpm --filter @unrdf/core test
pnpm --filter @unrdf/core run build
```

### Adding a Dependency

```bash
# Add to a specific package
pnpm --filter @unrdf/core add lodash

# Add to all packages
pnpm add -w typescript  # -w = workspace
```

### Removing a Dependency

```bash
# Remove from a package
pnpm --filter @unrdf/core remove lodash

# Remove from workspace
pnpm remove -w typescript
```

### Updating Version

Edit `packages/<name>/package.json`:

```json
{
  "version": "1.2.0"  // Update semantic version
}
```

Then:

```bash
# Rebuild with new version
pnpm --filter @unrdf/<name> run build

# Commit version bump
git add packages/<name>/package.json
git commit -m "chore: bump @unrdf/<name> to 1.2.0"
```

---

## Cross-Package Dependencies

### Using Workspace Packages

When a package depends on another UNRDF package:

```json
{
  "dependencies": {
    "@unrdf/core": "workspace:*",
    "@unrdf/oxigraph": "workspace:*"
  }
}
```

**`workspace:*` means:** "Use the local version from this monorepo, always."

### Direct File Imports (For Development)

Test code can import directly from source during development:

```javascript
// ✅ OK in tests
import { internalHelper } from '@unrdf/core/src/internal/parser.mjs';

// ❌ Not OK in production code (breaks when package is published)
import { internalHelper } from '@unrdf/core/src/internal/parser.mjs';
```

### Published Package Imports (For Users)

Users always import from the public API:

```javascript
// ✅ Correct
import { query } from '@unrdf/core';

// ❌ Wrong (not exported)
import { internalParser } from '@unrdf/core/src/internal/parser.mjs';
```

---

## Building and Publishing

### Local Build

```bash
# Build all packages
pnpm run build

# Build one package
pnpm --filter @unrdf/cache run build

# Clean build artifacts
pnpm run clean
```

### Before Publishing

```bash
# 1. Run all tests
pnpm test

# 2. Run linting
pnpm run lint

# 3. Build without errors
pnpm run build

# 4. Update CHANGELOG
# Edit CHANGELOG.md with new version info

# 5. Commit
git add .
git commit -m "chore: prepare v1.2.0 release"

# 6. Tag (for releases)
git tag @unrdf/cache@1.2.0
git push --tags
```

### Publishing to npm

```bash
# Publish the package
pnpm publish --filter @unrdf/cache

# Or publish all packages
pnpm publish
```

---

## Package Templates

### Minimal Template

```
packages/new-feature/
├── src/
│   ├── index.mjs
│   └── api/new-feature.mjs
├── test/
│   ├── api/new-feature.test.mjs
│   └── setup.mjs
├── package.json
├── vitest.config.mjs
└── README.md
```

### Full Template

```
packages/new-feature/
├── src/
│   ├── index.mjs
│   ├── api/
│   │   ├── public-api.mjs
│   │   └── exports.mjs
│   ├── internal/
│   │   ├── implementation.mjs
│   │   └── utils.mjs
│   └── types/
│       └── schemas.mjs
├── test/
│   ├── api/
│   │   └── public-api.test.mjs
│   ├── internal/
│   │   └── implementation.test.mjs
│   ├── integration/
│   │   └── end-to-end.test.mjs
│   ├── fixtures/
│   │   └── sample-data.mjs
│   └── setup.mjs
├── docs/
│   ├── ARCHITECTURE.md
│   ├── API.md
│   └── EXAMPLES.md
├── dist/                  (generated)
├── package.json
├── vitest.config.mjs
└── README.md
```

---

## Troubleshooting Package Development

### Issue: "Cannot find module @unrdf/new-package"

**Solution:** Ensure new package is in `packages/` directory:

```bash
ls -la packages/ | grep new-package
```

### Issue: Changes not reflected

**Solution:** Rebuild the package:

```bash
pnpm --filter @unrdf/new-package run build
```

### Issue: Tests can't import from src

**Solution:** Use relative imports in tests:

```javascript
// ✅ Good
import { helper } from '../src/api/helper.mjs';

// ❌ Wrong
import { helper } from '@unrdf/new-package/src/api/helper.mjs';
```

### Issue: Circular dependency between packages

**Solution:** Refactor to move shared code to a utility package:

```
Before: A ↔ B (circular)
After:  A → C ← B (shared in C)
```

---

## Checklist for New Packages

- [ ] Directory created in `packages/`
- [ ] `package.json` with proper metadata
- [ ] `src/index.mjs` entry point
- [ ] Public APIs in `src/api/`
- [ ] `test/` directory with tests for each module
- [ ] `vitest.config.mjs` for test configuration
- [ ] `README.md` with quick start
- [ ] `docs/` directory with detailed docs
- [ ] Cross-package deps use `workspace:*`
- [ ] `pnpm install` succeeds
- [ ] `pnpm --filter @unrdf/new-package test` passes
- [ ] `pnpm --filter @unrdf/new-package run build` succeeds
- [ ] Updated MONOREPO-QUICK-REFERENCE.md

---

## Next Steps

- **Deploy:** See publishing docs
- **Integrate:** Update [LOCAL-DEVELOPMENT.md](LOCAL-DEVELOPMENT.md) if you added new scripts
- **Document:** Add examples to [EXAMPLES.md](EXAMPLES.md) if applicable
- **Test:** See [TESTING-STRATEGY.md](TESTING-STRATEGY.md) for comprehensive testing

---

**Ready?** Create your package and follow the checklist above!
