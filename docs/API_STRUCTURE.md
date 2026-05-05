# UNRDF API Architecture

> **Version**: latest
> **Last Updated**: 2025-11-21

---

## Table of Contents

1. [Overview](#overview)
2. [Module Organization](#module-organization)
3. [Export Structure](#export-structure)
4. [Internal Dependencies](#internal-dependencies)
5. [Design Patterns](#design-patterns)
6. [Extensibility Points](#extensibility-points)
7. [Build Configuration](#build-configuration)

---

## Overview

UNRDF follows a modular architecture with clear separation of concerns:

```
unrdf/
├── src/
│   ├── index.mjs                    # Main entry point
│   ├── knowledge-engine.mjs         # Engine factory
│   ├── knowledge-engine/            # Core engine modules
│   ├── composables/                 # Vue-style composables
│   ├── react-hooks/                 # React hooks (80/20 tiered)
│   ├── utils/                       # Utility functions
│   ├── cli/                         # CLI implementation
│   ├── context/                     # unctx store context
│   ├── engines/                     # RDF engine implementations
│   ├── validation/                  # Validation infrastructure
│   ├── profiling/                   # Performance profiling
│   ├── security/                    # Sandbox and security
│   └── browser/                     # Browser-specific builds
└── package.json                     # Package exports
```

---

## Module Organization

### Entry Points

| Export Path | Module | Description |
|-------------|--------|-------------|
| `unrdf` | `src/index.mjs` | Main entry, re-exports all core |
| `unrdf/react-hooks` | `src/react-hooks/index.mjs` | React hooks (tiered) |
| `unrdf/react-hooks/core` | `src/react-hooks/core/index.mjs` | Core React hooks |
| `unrdf/react-hooks/streaming` | `src/react-hooks/streaming/index.mjs` | Streaming hooks |
| `unrdf/react-hooks/federation` | `src/react-hooks/federation/index.mjs` | Federation hooks |
| `unrdf/react-hooks/dark-matter` | `src/react-hooks/dark-matter/index.mjs` | Optimization hooks |
| `unrdf/react-hooks/ai-semantic` | `src/react-hooks/ai-semantic/index.mjs` | AI/Semantic hooks |
| `unrdf/react-hooks/advanced-utility` | `src/react-hooks/advanced-utility/index.mjs` | Advanced utility hooks |
| `unrdf/react-hooks/policy-security` | `src/react-hooks/policy-security/index.mjs` | Policy/Security hooks |
| `unrdf/react-hooks/error-recovery` | `src/react-hooks/error-recovery/index.mjs` | Error handling hooks |
| `unrdf/react-hooks/form-ui` | `src/react-hooks/form-ui/index.mjs` | Form/UI hooks |
| `unrdf/react-hooks/composition` | `src/react-hooks/composition/index.mjs` | Composition hooks |
| `unrdf/knowledge-engine` | `src/knowledge-engine/index.mjs` | Full knowledge engine |
| `unrdf/knowledge-engine/lite` | `src/knowledge-engine/lite.mjs` | Lightweight engine |
| `unrdf/composables/*` | `src/composables/*.mjs` | Individual composables |
| `unrdf/cli` | `src/cli/index.mjs` | CLI application |

### Core Modules

#### `src/index.mjs` - Main Entry

```javascript
// Store context
export * from "./context/index.mjs";

// Core composables
export { useGraph } from "./composables/use-graph.mjs";
export { useTurtle } from "./composables/use-turtle.mjs";
export { useTerms } from "./composables/use-terms.mjs";
export { useReasoner } from "./composables/use-reasoner.mjs";
export { useCanon } from "./composables/use-canon.mjs";
export { useZod } from "./composables/use-zod.mjs";
export { useDelta } from "./composables/use-delta.mjs";

// Engines
export { RdfEngine } from "./engines/rdf-engine.mjs";

// Knowledge Engine System
export * from "./knowledge-engine/index.mjs";

// Utilities
export * from "./utils/index.mjs";
```

#### `src/knowledge-engine/index.mjs` - Knowledge Engine Hub

```javascript
// Core Engine Components
export { KnowledgeHookManager } from "./knowledge-hook-manager.mjs";
export { TransactionManager } from "./transaction.mjs";

// Hook System
export { defineHook } from "./define-hook.mjs";
export { createHookExecutor } from "./hook-executor.mjs";
export { createConditionEvaluator } from "./condition-evaluator.mjs";
export { registerHook, deregisterHook, evaluateHook, getRegisteredHooks, resetGlobalHookManager } from "./hook-management.mjs";

// Knowledge Substrate Core (80/20 Framework)
export { KnowledgeSubstrateCore, createKnowledgeSubstrateCore, KnowledgeSubstrateFactory } from "./knowledge-substrate-core.mjs";

// Storage & Persistence
export { LockchainWriter, createLockchainWriter } from "./lockchain-writer.mjs";
export { ResolutionLayer } from "./resolution-layer.mjs";

// Query & Optimization
export { QueryOptimizer } from "./query-optimizer.mjs";
export { query } from "./query.mjs";

// Parsing & Serialization
export { parseTurtle, toTurtle, toNQuads, parseJsonLd, toJsonLd } from "./parse.mjs";

// Validation
export { validateShacl, validateShaclMultiple, formatValidationReport, hasValidationErrors, getValidationErrors, getValidationWarnings } from "./validate.mjs";

// Canonicalization
export { canonicalize, isIsomorphic, getCanonicalHash, groupByIsomorphism, findDuplicates, getCanonicalizationStats, createCanonicalizationSession } from "./canonicalize.mjs";

// Reasoning
export { reason, reasonMultiple, extractInferred, getReasoningStats, validateRules, createReasoningSession } from "./reason.mjs";

// File Resolution
export { resolveFileUri, calculateFileHash, loadFileWithHash, loadSparqlFile } from "./file-resolver.mjs";

// Security & Sandbox
export { EffectSandbox } from "./effect-sandbox.mjs";

// Policy Management
export { PolicyPackManager, PolicyPack } from "./policy-pack.mjs";

// Observability System
export { ObservabilityManager, createObservabilityManager, defaultObservabilityManager } from "./observability.mjs";

// N3 Re-exports
export { Store, Parser, Writer, DataFactory } from "n3";

// Schemas
export * from "./schemas.mjs";
```

---

## Export Structure

### Package.json Exports

```json
{
  "exports": {
    ".": "./src/index.mjs",
    "./react-hooks": "./src/react-hooks/index.mjs",
    "./react-hooks/core": "./src/react-hooks/core/index.mjs",
    "./react-hooks/streaming": "./src/react-hooks/streaming/index.mjs",
    "./react-hooks/federation": "./src/react-hooks/federation/index.mjs",
    "./react-hooks/dark-matter": "./src/react-hooks/dark-matter/index.mjs",
    "./react-hooks/ai-semantic": "./src/react-hooks/ai-semantic/index.mjs",
    "./react-hooks/advanced-utility": "./src/react-hooks/advanced-utility/index.mjs",
    "./react-hooks/policy-security": "./src/react-hooks/policy-security/index.mjs",
    "./react-hooks/error-recovery": "./src/react-hooks/error-recovery/index.mjs",
    "./react-hooks/form-ui": "./src/react-hooks/form-ui/index.mjs",
    "./react-hooks/composition": "./src/react-hooks/composition/index.mjs",
    "./knowledge-engine": "./src/knowledge-engine/index.mjs",
    "./knowledge-engine/lite": "./src/knowledge-engine/lite.mjs",
    "./composables/*": "./src/composables/*.mjs",
    "./cli": "./src/cli/index.mjs"
  }
}
```

### React Hooks 80/20 Export Tiers

```javascript
// Tier 1: Essential (60% usage)
export { useKnowledgeEngine, useKnowledgeEngineContext, useTransaction, useKnowledgeHook } from './core/index.mjs';
export { useChangeFeed } from './streaming/use-change-feed.mjs';
export { useDarkMatterCore } from './dark-matter/use-dark-matter-core.mjs';
export { useQueryAnalyzer } from './dark-matter/use-query-analyzer.mjs';
export { useErrorBoundary } from './error-recovery/use-error-boundary.mjs';

// Tier 2: Important (20% usage)
export { useGraphDiff } from './advanced-utility/use-graph-diff.mjs';
export { useSPARQLEditor } from './form-ui/use-sparql-editor.mjs';

// Tier 3: Standard (15% usage)
export { useFederatedSystem } from './federation/use-federated-system.mjs';
export { useStreamProcessor } from './streaming/use-stream-processor.mjs';
export { useOptimizer } from './dark-matter/use-optimizer.mjs';
export { useSemanticAnalyzer } from './ai-semantic/use-semantic-analyzer.mjs';
export { useGraphMerge } from './advanced-utility/use-graph-merge.mjs';
export { usePolicyPack } from './policy-security/use-policy-pack.mjs';
export { useRecovery } from './error-recovery/use-recovery.mjs';
export { useGraphVisualizer, useResultsPaginator } from './form-ui/index.mjs';

// Tier 4: Advanced (5% usage) - Category exports
export * as Federation from './federation/index.mjs';
export * as Streaming from './streaming/index.mjs';
export * as DarkMatter from './dark-matter/index.mjs';
export * as AISemantic from './ai-semantic/index.mjs';
export * as AdvancedUtility from './advanced-utility/index.mjs';
export * as PolicySecurity from './policy-security/index.mjs';
export * as ErrorRecovery from './error-recovery/index.mjs';
export * as FormUI from './form-ui/index.mjs';
export * as Composition from './composition/index.mjs';
```

---

## Internal Dependencies

### Dependency Graph

```
src/index.mjs
├── context/index.mjs
│   └── unctx (external)
├── composables/
│   ├── use-graph.mjs
│   │   ├── n3 (external)
│   │   └── context/index.mjs
│   ├── use-turtle.mjs
│   │   ├── node:fs
│   │   ├── node:path
│   │   └── context/index.mjs
│   ├── use-zod.mjs
│   │   ├── zod (external)
│   │   └── context/index.mjs
│   └── ...
├── engines/rdf-engine.mjs
│   ├── n3 (external)
│   ├── @comunica/query-sparql (external)
│   └── rdf-validate-shacl (external)
└── knowledge-engine/index.mjs
    ├── parse.mjs
    │   ├── n3 (external)
    │   └── @opentelemetry/api (external)
    ├── query.mjs
    │   ├── n3 (external)
    │   ├── query-cache.mjs
    │   └── @opentelemetry/api (external)
    ├── validate.mjs
    │   ├── n3 (external)
    │   ├── rdf-ext (external)
    │   ├── rdf-validate-shacl (external)
    │   └── @opentelemetry/api (external)
    ├── knowledge-hook-manager.mjs
    │   ├── transaction.mjs
    │   ├── hook-executor.mjs
    │   ├── condition-evaluator.mjs
    │   ├── policy-pack.mjs
    │   ├── security-validator.mjs
    │   ├── n3 (external)
    │   └── schemas.mjs
    ├── schemas.mjs
    │   └── zod (external)
    └── ...
```

### External Dependencies

| Dependency | Version | Purpose |
|------------|---------|---------|
| `n3` | ^latest | N3.js RDF library (Store, Parser, Writer) |
| `@comunica/query-sparql` | ^latest | SPARQL query engine |
| `rdf-ext` | ^latest | RDF/JS Dataset implementation |
| `rdf-validate-shacl` | ^latest | SHACL validation |
| `rdf-canonize` | ^latest | RDF canonicalization |
| `eyereasoner` | ^latest | N3 reasoning engine |
| `jsonld` | ^latest | JSON-LD processing |
| `zod` | ^latest | Runtime validation |
| `unctx` | ^latest | Composition context |
| `citty` | ^latest | CLI framework |
| `@opentelemetry/*` | Various | Observability |
| `@noble/hashes` | ^latest | Cryptographic hashing |
| `lru-cache` | ^latest | Caching |
| `isolated-vm` | ^latest | Secure sandboxing |
| `vm2` | ^latest | VM sandboxing |
| `ws` | ^latest | WebSocket support |

### Internal Module Dependencies

```
knowledge-engine/
├── parse.mjs ──────────────────────> n3, @opentelemetry/api
├── query.mjs ──────────────────────> query-cache.mjs, n3, @opentelemetry/api
├── query-cache.mjs ────────────────> @comunica/query-sparql, lru-cache
├── validate.mjs ───────────────────> n3, rdf-ext, rdf-validate-shacl, @opentelemetry/api
├── reason.mjs ─────────────────────> eyereasoner, n3
├── canonicalize.mjs ───────────────> rdf-canonize, n3, @noble/hashes
├── transaction.mjs ────────────────> n3, @noble/hashes, schemas.mjs
├── knowledge-hook-manager.mjs ─────> transaction.mjs, hook-executor.mjs, condition-evaluator.mjs,
│                                     policy-pack.mjs, security-validator.mjs, schemas.mjs
├── hook-executor.mjs ──────────────> condition-evaluator.mjs, effect-sandbox.mjs
├── condition-evaluator.mjs ────────> query.mjs, file-resolver.mjs
├── effect-sandbox.mjs ─────────────> isolated-vm, vm2, worker_threads
├── policy-pack.mjs ────────────────> file-resolver.mjs, schemas.mjs
├── lockchain-writer.mjs ───────────> @noble/hashes, node:child_process (git)
├── resolution-layer.mjs ───────────> schemas.mjs
├── query-optimizer.mjs ────────────> lru-cache, query.mjs
├── file-resolver.mjs ──────────────> node:fs, node:path, node:crypto
├── observability.mjs ──────────────> @opentelemetry/*
├── schemas.mjs ────────────────────> zod
└── index.mjs ──────────────────────> All above
```

---

## Design Patterns

### 1. Composable Pattern

Vue-style composables for reactive state management:

```javascript
// Pattern: useX() returns object with methods and reactive state
export function useGraph() {
  const context = useStoreContext();

  return {
    async query(sparql, options) { /* ... */ },
    async select(sparql) { /* ... */ },
    validate(shapes) { /* ... */ },
    get size() { return context.store.size; }
  };
}
```

### 2. Factory Pattern

Factories for complex object creation:

```javascript
// Pattern: createX() returns configured instance
export function createKnowledgeEngine(options = {}) {
  const { baseIRI, strictMode, maxHooks } = options;

  return {
    async parseTurtle(ttl) { /* ... */ },
    async query(store, sparql) { /* ... */ },
    // ... all methods bound to options
  };
}
```

### 3. Singleton Pattern

Singleton for expensive resources:

```javascript
// Pattern: Cached singleton with lazy initialization
let queryEngine = null;

export function getQueryEngine() {
  if (!queryEngine) {
    queryEngine = new QueryEngine();
  }
  return queryEngine;
}
```

### 4. Strategy Pattern

Pluggable strategies for resolution:

```javascript
// Pattern: Strategy objects with common interface
const resolutionStrategies = {
  voting: { resolve(proposals) { /* voting logic */ } },
  merging: { resolve(proposals) { /* merge logic */ } },
  crdt: { resolve(proposals) { /* CRDT logic */ } },
  consensus: { resolve(proposals) { /* consensus logic */ } }
};

export function createResolutionLayer(strategy) {
  return {
    resolve(proposals) {
      return resolutionStrategies[strategy].resolve(proposals);
    }
  };
}
```

### 5. Builder Pattern

Fluent builders for complex construction:

```javascript
// Pattern: Chainable methods returning this
export function createSPARQLBuilder() {
  const state = { prefixes: [], select: [], where: [], limit: null };

  return {
    prefix(name, iri) { state.prefixes.push({ name, iri }); return this; },
    select(...vars) { state.select.push(...vars); return this; },
    where(s, p, o) { state.where.push({ s, p, o }); return this; },
    limit(n) { state.limit = n; return this; },
    build() { /* construct SPARQL string */ }
  };
}
```

### 6. Observer Pattern

Event-based notifications:

```javascript
// Pattern: Subscribe/unsubscribe with callbacks
export class TransactionManager {
  #listeners = new Set();

  subscribe(callback) {
    this.#listeners.add(callback);
    return () => this.#listeners.delete(callback);
  }

  #notify(event) {
    this.#listeners.forEach(cb => cb(event));
  }
}
```

### 7. Decorator Pattern (OpenTelemetry)

Observability wrapping:

```javascript
// Pattern: Wrap operations with tracing
export async function parseTurtle(ttl, baseIRI) {
  return tracer.startActiveSpan('parse.turtle', async (span) => {
    try {
      span.setAttributes({
        'parse.format': 'turtle',
        'parse.base_iri': baseIRI
      });

      const result = await actualParseTurtle(ttl, baseIRI);

      span.setStatus({ code: SpanStatusCode.OK });
      return result;
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  });
}
```

### 8. Validation Pattern (Zod)

Runtime validation with schemas:

```javascript
// Pattern: Schema + validator function + creator function
export const HookMetaSchema = z.object({
  name: z.string().min(1).max(100),
  description: z.string().optional(),
  version: z.string().regex(/^\d+\.\d+\.\d+$/).optional()
});

export function validateHookMeta(meta) {
  try {
    const validated = HookMetaSchema.parse(meta);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    return { success: false, data: null, errors: error.errors };
  }
}

export function createHookMeta(meta) {
  const result = validateHookMeta(meta);
  if (!result.success) {
    throw new TypeError(`Invalid hook meta: ${result.errors.join(', ')}`);
  }
  return Object.freeze(result.data);
}
```

---

## Extensibility Points

### 1. Custom Hook Conditions

Extend the condition evaluator:

```javascript
// Register custom condition type
import { createConditionEvaluator } from 'unrdf/knowledge-engine';

const evaluator = createConditionEvaluator({
  customConditions: {
    'custom-metric': async (condition, store, options) => {
      // Custom evaluation logic
      const metric = await calculateCustomMetric(store);
      return metric > condition.spec.threshold;
    }
  }
});
```

### 2. Custom Resolution Strategies

Add resolution strategies:

```javascript
import { ResolutionLayer } from 'unrdf/knowledge-engine';

class CustomResolutionLayer extends ResolutionLayer {
  registerStrategy(name, strategy) {
    this.strategies.set(name, strategy);
  }
}

const layer = new CustomResolutionLayer();
layer.registerStrategy('ai-consensus', {
  resolve: async (proposals) => {
    // AI-based resolution
  }
});
```

### 3. Custom Sandbox Executors

Add sandbox implementations:

```javascript
import { EffectSandbox } from 'unrdf/knowledge-engine';

class CustomSandbox extends EffectSandbox {
  async execute(code, context) {
    // Custom sandboxed execution
  }
}
```

### 4. Custom Observability Exporters

Add telemetry exporters:

```javascript
import { createObservabilityManager } from 'unrdf/knowledge-engine';

const manager = createObservabilityManager({
  exporters: [
    customJaegerExporter,
    customPrometheusExporter,
    customDatadogExporter
  ]
});
```

### 5. Policy Pack Plugins

Create policy pack plugins:

```javascript
// policy-packs/compliance/manifest.json
{
  "meta": {
    "name": "compliance-pack",
    "version": "latest"
  },
  "hooks": [
    {
      "file": "./hooks/gdpr.mjs",
      "condition": {
        "kind": "sparql-ask",
        "ref": { "uri": "./queries/gdpr-check.sparql" }
      }
    }
  ]
}
```

### 6. Custom React Hooks

Compose custom hooks from primitives:

```javascript
import {
  useKnowledgeEngine,
  useChangeFeed,
  useErrorBoundary
} from 'unrdf/react-hooks';

export function useCustomDashboard(options) {
  const engine = useKnowledgeEngine(options);
  const feed = useChangeFeed();
  const errorBoundary = useErrorBoundary();

  return {
    ...engine,
    changes: feed.changes,
    errors: errorBoundary.errors,
    // Custom aggregated functionality
    getDashboardStats: async () => {
      // Custom logic combining multiple hooks
    }
  };
}
```

### 7. CLI Plugins

Extend CLI with plugins:

```javascript
// plugins/my-plugin.mjs
export const myCommand = defineCommand({
  meta: {
    name: 'my-command',
    description: 'Custom command'
  },
  args: {
    input: { type: 'string', required: true }
  },
  run: async ({ args }) => {
    // Custom command logic
  }
});
```

### 8. Utility Extensions

Add utility functions:

```javascript
// Extend transform utilities
import { transformStore } from 'unrdf/utils';

export function transformWithProvenance(store, transformer, provenanceIRI) {
  const transformed = transformStore(store, transformer);
  // Add provenance triples
  // ...
  return transformed;
}
```

---

## Build Configuration

### `build.config.mjs`

```javascript
export default {
  entries: [
    { input: 'src/index.mjs', name: 'index' },
    { input: 'src/knowledge-engine/index.mjs', name: 'knowledge-engine' },
    { input: 'src/react-hooks/index.mjs', name: 'react-hooks' },
    { input: 'src/cli/index.mjs', name: 'cli' }
  ],
  externals: [
    'n3',
    '@comunica/query-sparql',
    'rdf-ext',
    'rdf-validate-shacl',
    'rdf-canonize',
    'eyereasoner',
    'jsonld',
    'zod',
    'unctx',
    'citty',
    'react',
    'react-dom'
  ],
  declaration: false, // No TypeScript
  clean: true
};
```

### ESLint Configuration

```javascript
// eslint.config.mjs
export default [
  {
    files: ['src/**/*.mjs', 'test/**/*.mjs'],
    plugins: ['jsdoc'],
    rules: {
      'jsdoc/require-jsdoc': 'error',
      'jsdoc/require-param': 'error',
      'jsdoc/require-returns': 'error',
      'jsdoc/check-param-names': 'error',
      'jsdoc/check-types': 'error'
    }
  }
];
```

### Vitest Configuration

```javascript
// vitest.config.mjs
export default {
  test: {
    include: ['test/**/*.test.mjs'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'lcov', 'html'],
      include: ['src/**/*.mjs']
    }
  }
};
```

---

## Module Boundaries

### Public API Surface

| Module | Public | Internal |
|--------|--------|----------|
| `index.mjs` | All exports | None |
| `knowledge-engine/index.mjs` | All exports | None |
| `knowledge-engine/*.mjs` | Named exports | Private functions |
| `composables/*.mjs` | `use*` functions | Helper functions |
| `react-hooks/*.mjs` | `use*` hooks | Internal state |
| `utils/index.mjs` | All exports | None |
| `cli/index.mjs` | `main` command | Subcommands |

### Private/Internal Conventions

- Functions starting with `_` are private
- Files in `internal/` directories are not exported
- `#` prefix for private class fields

```javascript
class KnowledgeHookManager {
  #privateField = new Map();  // Private field

  _internalMethod() {         // Internal method (convention)
    // ...
  }

  publicMethod() {            // Public API
    // ...
  }
}
```

---

## Related Documentation

- [API Reference](/docs/REFERENCE/api-reference.md)
- [Contributing Guide](/docs/CONTRIBUTING.md)
- [Getting Started](/docs/getting-started.md)
- [Architecture Summary](/docs/internal/architecture/ARCHITECTURE-SUMMARY.md)

---

> **Document Version**: latest
> **Generated**: 2025-11-21
> **Maintainer**: UNRDF Team
