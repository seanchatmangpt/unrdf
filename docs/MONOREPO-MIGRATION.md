# UNRDF Monorepo Migration Guide (v4.x → v5.0)

## Overview

UNRDF v5 moves from a single-package model to a monorepo with clear separation of concerns. This guide helps you migrate your code.

## Key Changes

### Package Structure
```
v4.x:
  import { ... } from 'unrdf'
  import { ... } from 'unrdf/knowledge-engine'

v5.0+:
  import { ... } from '@unrdf/core'
  import { ... } from '@unrdf/knowledge-engine'
  import { ... } from '@unrdf/hooks'
  import { ... } from '@unrdf/federation'
  // etc.
```

### What Moved

**@unrdf/core** (always needed):
- RDF operations (createStore, addQuad, etc.)
- SPARQL execution (executeQuery)
- Types and constants

**@unrdf/hooks** (core substrate):
- Knowledge Hooks (defineHook, executeHook)
- Hook validation

**@unrdf/federation** (core substrate):
- Peer discovery (discoverPeers)
- Remote queries (remoteQuery)

**@unrdf/streaming** (core substrate):
- Change feeds (subscribeToChanges)
- Stream processors

**@unrdf/browser** (for web apps):
- IndexedDB storage (IndexedDBStore)
- Browser shims
- Comunica adapter

**@unrdf/cli** (for CLI use):
- Graph commands
- Context management
- Hook evaluation

**@unrdf/knowledge-engine** (optional):
- Rule engine (inference)
- Query optimization was moved to dark-matter
- AI semantic integration patterns

**@unrdf/dark-matter** (optional):
- Query optimization (80/20)
- Critical path analysis
- Performance analysis

**@unrdf/composables** (optional, web only):
- Vue 3 composables (useGraph, useDelta, etc.)

## Migration Steps

### Step 1: Update Package Installation

```bash
# Old way (single package)
pnpm add unrdf

# New way (substrate only)
pnpm add @unrdf/core @unrdf/hooks @unrdf/federation @unrdf/streaming

# Add optional extensions as needed
pnpm add @unrdf/knowledge-engine  # If using rules/inference
pnpm add @unrdf/dark-matter       # If optimizing queries
pnpm add @unrdf/composables       # If building Vue apps
```

### Step 2: Update Imports

**Before (v4.x)**:
```javascript
import {
  createStore,
  executeQuery,
  defineHook,
  subscribeToChanges,
  useGraph,
  optimizeQuery,
  inferPatterns
} from 'unrdf'
```

**After (v5.0)**:
```javascript
// Core substrate
import { createStore, executeQuery } from '@unrdf/core'
import { defineHook, executeHook } from '@unrdf/hooks'
import { subscribeToChanges } from '@unrdf/streaming'
import { connectToPeer, remoteQuery } from '@unrdf/federation'

// Optional extensions
import { useGraph } from '@unrdf/composables'         // Web apps only
import { optimizeQuery } from '@unrdf/dark-matter'   // Optional
import { inferPatterns } from '@unrdf/knowledge-engine' // Optional
```

### Step 3: Update CLI References

**Before (v4.x)**:
```bash
unrdf graph create --name my-graph
unrdf hook eval --hook my-hook.mjs
```

**After (v5.0)**:
```bash
# Same command (CLI package exported globally)
unrdf graph create --name my-graph
unrdf hook eval --hook my-hook.mjs

# Or use directly:
pnpm exec unrdf graph create --name my-graph
```

### Step 4: Update Browser App Imports

**Before (v4.x)**:
```javascript
import { IndexedDBStore, useGraph } from 'unrdf'
```

**After (v5.0)**:
```javascript
import { IndexedDBStore } from '@unrdf/browser'
import { useGraph } from '@unrdf/composables'
```

## Breaking Changes by Scenario

### Using RDF Operations
- ❌ Old: `import { createStore } from 'unrdf'`
- ✅ New: `import { createStore } from '@unrdf/core'`
- **No functionality change**

### Using Knowledge Hooks
- ❌ Old: `import { defineHook } from 'unrdf'`
- ✅ New: `import { defineHook } from '@unrdf/hooks'`
- **No functionality change**

### Using Knowledge Engine Rules
- ❌ Old: `import { inferPatterns } from 'unrdf/knowledge-engine'`
- ✅ New: `import { inferPatterns } from '@unrdf/knowledge-engine'`
- ⚠️ Knowledge Engine now in separate package
- ✅ **Optional**: Only install if you use it

### Using Dark Matter Optimization
- ❌ Old: `import { optimizeQuery } from 'unrdf/knowledge-engine/dark-matter'`
- ✅ New: `import { optimizeQuery } from '@unrdf/dark-matter'`
- ✅ **Separate package**: Only install if needed

### Using Composables (Vue)
- ❌ Old: `import { useGraph } from 'unrdf/composables'`
- ✅ New: `import { useGraph } from '@unrdf/composables'`
- **No functionality change, just package name**

## Migration Checklist

- [ ] Update `package.json` dependencies
- [ ] Replace `import 'unrdf'` with appropriate `@unrdf/*` packages
- [ ] Test build and imports
- [ ] Update TypeScript imports if using types
- [ ] Run tests to verify functionality
- [ ] Update documentation/README
- [ ] Commit changes

## Dependency Resolution

Each package declares its substrate dependencies:

```json
{
  "dependencies": {
    "@unrdf/core": "^5.0.0",
    "@unrdf/hooks": "^5.0.0"
  }
}
```

This means you can install a high-level package and get substrate dependencies automatically:

```bash
# Install just composables
pnpm add @unrdf/composables

# This automatically installs:
# - @unrdf/core
# - @unrdf/browser
# - @unrdf/streaming
```

## Troubleshooting

### "Cannot find module '@unrdf/core'"
```bash
# Ensure you installed the package
pnpm add @unrdf/core

# Or use monorepo workspace (local development)
pnpm install
```

### "Multiple versions of @unrdf/core"
```bash
# Check installed versions
pnpm list '@unrdf/core'

# Ensure all packages use same version
pnpm add @unrdf/core@^5.0.0
```

### "Missing optional dependencies"
You only need to install optional packages if you use them:

```bash
# These are optional:
pnpm add @unrdf/knowledge-engine  # Only if using rules
pnpm add @unrdf/dark-matter       # Only if optimizing queries
pnpm add @unrdf/composables       # Only if building Vue apps
```

### TypeScript compilation errors
Ensure `tsconfig.json` includes:

```json
{
  "compilerOptions": {
    "skipLibCheck": true,
    "moduleResolution": "node"
  }
}
```

## Version Management

Each package in the monorepo can have independent versions:

- `@unrdf/core`: 5.0.0+ (substrate, rarely changes)
- `@unrdf/hooks`: 5.0.0+ (core, stable)
- `@unrdf/knowledge-engine`: 2.0.0+ (optional, may change faster)
- `@unrdf/dark-matter`: 1.0.0+ (optional, may change faster)
- `@unrdf/composables`: 1.0.0+ (optional, updates with Vue)

This means you can upgrade features without waiting for core stability.

## Migration Complete!

After updating imports, your code should work identically. The benefits you get:

✅ **Smaller bundle size** - Only install what you need
✅ **Clearer dependencies** - Know exactly what each package provides
✅ **Faster updates** - Core stable, extensions innovate faster
✅ **Better for edge/IoT** - Install just `@unrdf/core` if that's all you need
✅ **Community extensions** - Easy to build `@unrdf/my-plugin`

## Questions?

See the [MONOREPO-STRUCTURE.md](./MONOREPO-STRUCTURE.md) for architecture details, or the [v5-voc-to-implementation.md](./v5-voc-to-implementation.md) for why we made these changes.
