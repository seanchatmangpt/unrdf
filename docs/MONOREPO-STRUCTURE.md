# UNRDF Monorepo Structure

## Overview
UNRDF v5 is organized as a pnpm monorepo with clear package boundaries aligned to the substrate VOC analysis.

```
unrdf/
├── packages/
│   ├── core/                     # @unrdf/core - RDF substrate
│   ├── browser/                  # @unrdf/browser - Browser SDK
│   ├── cli/                      # @unrdf/cli - Command-line tools
│   ├── hooks/                    # @unrdf/hooks - Knowledge Hooks
│   ├── federation/               # @unrdf/federation - Federation system
│   ├── streaming/                # @unrdf/streaming - Change feeds & subscriptions
│   ├── knowledge-engine/         # @unrdf/knowledge-engine - Rule engine (optional)
│   ├── dark-matter/              # @unrdf/dark-matter - Query optimization (optional)
│   ├── composables/              # @unrdf/composables - Vue 3 composables (optional)
│   └── project-engine/           # @unrdf/project-engine - Self-hosting tools (optional)
├── examples/                     # Examples and demos
├── docs/                         # Documentation
├── test/                         # Shared test utilities
└── pnpm-workspace.yaml           # Workspace configuration
```

## Package Descriptions

### Core Substrate Packages (Required)

#### `packages/core` - @unrdf/core
**Purpose**: RDF graph operations, SPARQL execution, foundational types

**Exports**:
```javascript
// RDF operations
export { Store, addQuad, removeQuad, getQuads, iterateQuads }
export { canonicalize, toNTriples }

// SPARQL
export { executeQuery, prepareQuery }

// Types
export { Quad, Term, NamedNode, Literal }
export { RDFObject }

// Constants
export { RDF, RDFS, OWL, XSD, FOAF }
```

**Dependencies**: @rdfjs/*, @comunica/*, n3, rdf-canonize
**Size**: ~150KB uncompressed

#### `packages/hooks` - @unrdf/hooks
**Purpose**: Knowledge Hooks - policy definition and execution

**Exports**:
```javascript
export { defineHook, executeHook }
export { Hook, HookRegistry, HookContext }
export { ValidationHook, TransformHook, EnforcementHook }
```

**Dependencies**: @unrdf/core, zod
**Size**: ~50KB uncompressed

#### `packages/federation` - @unrdf/federation
**Purpose**: Peer discovery, federated queries, remote execution

**Exports**:
```javascript
export { connectToPeer, discoverPeers }
export { remoteQuery, federatedQuery }
export { FederationCoordinator }
```

**Dependencies**: @unrdf/core, @unrdf/hooks
**Size**: ~80KB uncompressed

#### `packages/streaming` - @unrdf/streaming
**Purpose**: Change feeds, subscriptions, real-time synchronization

**Exports**:
```javascript
export { subscribeToChanges, unsubscribe }
export { getChangesSince, applyDelta }
export { StreamProcessor, ChangeEvent }
```

**Dependencies**: @unrdf/core, @unrdf/hooks
**Size**: ~60KB uncompressed

#### `packages/browser` - @unrdf/browser
**Purpose**: Browser SDK with IndexedDB storage and polyfills

**Exports**:
```javascript
export { IndexedDBStore, BrowserStore }
export { BrowserShim, FileSystemAdapter }
export { ComunicaBrowserAdapter }
```

**Dependencies**: @unrdf/core, @unrdf/streaming
**Size**: ~40KB uncompressed

#### `packages/cli` - @unrdf/cli
**Purpose**: Command-line interface for graph operations and context management

**Exports**:
```javascript
export { cli, commands }
export { graphCommand, contextCommand, hookCommand }
```

**Dependencies**: @unrdf/core, @unrdf/hooks, @unrdf/federation, citty
**Size**: ~70KB uncompressed

### Optional Extension Packages

#### `packages/knowledge-engine` - @unrdf/knowledge-engine
**Purpose**: Rule engine, inference, pattern matching (optional add-on)

**Status**: Separate package, can be added to projects that need it
**Dependencies**: @unrdf/core, @unrdf/streaming, eyereasoner
**Size**: ~250KB uncompressed

#### `packages/dark-matter` - @unrdf/dark-matter
**Purpose**: Query optimization, critical path analysis (optional add-on)

**Status**: Separate package for performance-critical applications
**Dependencies**: @unrdf/core, typhonjs-escomplex
**Size**: ~100KB uncompressed

#### `packages/composables` - @unrdf/composables
**Purpose**: Vue 3 composables for reactive RDF state

**Status**: Web applications only
**Dependencies**: @unrdf/core, @unrdf/browser, @unrdf/streaming, vue 3+
**Size**: ~30KB uncompressed

#### `packages/project-engine` - @unrdf/project-engine
**Purpose**: UNRDF self-hosting tools and infrastructure

**Status**: For contributing to UNRDF itself
**Dependencies**: @unrdf/core, various dev tools
**Size**: ~200KB uncompressed

## Package Dependencies

```
@unrdf/core (no UNRDF deps)
    ↑
    ├── @unrdf/hooks
    ├── @unrdf/federation
    ├── @unrdf/streaming
    ├── @unrdf/browser
    └── @unrdf/cli

@unrdf/knowledge-engine (optional, depends on core)
    ↑
    ├── @unrdf/core
    └── @unrdf/streaming

@unrdf/dark-matter (optional, depends on core)
    ↑
    └── @unrdf/core

@unrdf/composables (web only, depends on core + browser)
    ↑
    ├── @unrdf/core
    ├── @unrdf/browser
    └── @unrdf/streaming

@unrdf/project-engine (contributing only)
    ↑
    └── @unrdf/core
```

## File Structure Per Package

```
packages/core/
├── package.json
├── src/
│   ├── index.mjs
│   ├── rdf/
│   │   ├── store.mjs
│   │   ├── quads.mjs
│   │   ├── canonicalize.mjs
│   │   └── namespaces.mjs
│   ├── sparql/
│   │   ├── executor.mjs
│   │   ├── parser.mjs
│   │   └── formatter.mjs
│   ├── types.mjs
│   ├── constants.mjs
│   └── validation.mjs
├── test/
│   ├── rdf/
│   ├── sparql/
│   └── types.test.mjs
└── README.md

packages/hooks/
├── package.json
├── src/
│   ├── index.mjs
│   ├── define.mjs
│   ├── executor.mjs
│   ├── registry.mjs
│   └── schemas.mjs
├── test/
└── README.md

# Same pattern for other packages...
```

## Workspace Configuration

### pnpm-workspace.yaml
```yaml
packages:
  - 'packages/*'
  - 'examples/*'
```

### Root package.json (monorepo)
```json
{
  "name": "unrdf-workspace",
  "version": "5.0.0",
  "private": true,
  "workspaces": ["packages/*"],
  "scripts": {
    "test": "pnpm -r test",
    "test:fast": "pnpm -r test:fast",
    "lint": "pnpm -r lint",
    "build": "pnpm -r build",
    "dev": "pnpm -r --parallel dev",
    "clean": "pnpm -r clean && rm -rf node_modules"
  },
  "devDependencies": {
    "vitest": "workspace:*",
    "eslint": "workspace:*"
  }
}
```

## Installation & Development

### Install dependencies
```bash
pnpm install
```

### Run tests across all packages
```bash
pnpm test
```

### Run tests for specific package
```bash
pnpm -C packages/core test
```

### Develop a specific package
```bash
cd packages/core
pnpm dev
```

### Build all packages
```bash
pnpm build
```

## Importing Between Packages

### From another package
```javascript
// package.json declares dependency
{
  "dependencies": {
    "@unrdf/core": "workspace:*"
  }
}

// Import like normal npm package
import { executeQuery } from '@unrdf/core'
```

### Local development (auto-linked)
No special setup needed. pnpm handles workspace linking automatically.

## Publishing

### Single package
```bash
cd packages/core
npm publish
```

### All packages (ordered)
```bash
# Core first
cd packages/core && npm publish

# Then core-dependent packages in parallel
cd packages/hooks && npm publish &
cd packages/federation && npm publish &
cd packages/streaming && npm publish &
# etc
```

## Version Management

Option 1: Unified versioning (all packages same version)
```bash
pnpm version patch  # Updates all packages
```

Option 2: Independent versioning (each package owns version)
```bash
cd packages/core
npm version patch
```

**Recommended**: Independent versioning (option 2) so core stability doesn't block feature releases in extensions.

## Adding New Packages

```bash
mkdir packages/my-feature
cd packages/my-feature
npm init -y

# Update package.json
# Add @unrdf/core dependency if needed
# Create src/, test/, README.md

# Add to pnpm-workspace.yaml (auto-detected)
# Test imports: pnpm test
```

## CI/CD Integration

### GitHub Actions
```yaml
name: Test All Packages

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: pnpm/action-setup@v2
      - uses: actions/setup-node@v3
      with:
          node-version: 18
          cache: 'pnpm'
      - run: pnpm install
      - run: pnpm lint
      - run: pnpm test
      - run: pnpm build
```

## Troubleshooting

### Package not found
```bash
# Ensure package.json has dependency
pnpm add @unrdf/core --workspace

# Reinstall workspace
pnpm install
```

### Version mismatch
```bash
# Ensure workspace:* in package.json
pnpm list @unrdf/core
```

### Build issues
```bash
# Clean and rebuild
pnpm clean
pnpm install
pnpm build
```
