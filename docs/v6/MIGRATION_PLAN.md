# UNRDF V6 Migration Plan

**Version**: 6.0.0-alpha.1
**Target Release**: Q1 2025
**Status**: Planning Phase

## Executive Summary

UNRDF v6 represents a major architectural evolution focused on:
- **Receipt-driven determinism**: All operations produce verifiable receipts (KGC-4D integration)
- **Pure ESM**: Remove all CommonJS compatibility layers
- **Zod-first validation**: Enforce runtime schema validation across all packages
- **Maturity ladder enforcement**: Graduated rollout from L1 → L5
- **Breaking API consolidation**: Standardize patterns across 47 packages

## Breaking Changes Summary

### 1. Store Initialization (CRITICAL)

**v5 (Deprecated)**:
```javascript
import { Store } from 'n3';
const store = new Store();
```

**v6 (Required)**:
```javascript
import { createStore } from '@unrdf/oxigraph';
const store = await createStore();
```

**Migration Path**: Use `@unrdf/v6-compat` adapter for gradual migration.

**Rationale**: Oxigraph provides 10x faster SPARQL execution and native WASM support.

---

### 2. Receipt-Driven Operations

**v5 (No receipts)**:
```javascript
await workflow.execute(task);
// No proof of execution
```

**v6 (Receipt required)**:
```javascript
const receipt = await workflow.execute(task);
// receipt = { hash: "sha256:...", timestamp: 1704..., proof: {...} }
await kgc4d.freeze(receipt); // Snapshot to Git
```

**Migration Path**: Wrap v5 operations in receipt adapters (see `@unrdf/v6-compat/adapters`).

**Rationale**: Deterministic execution, replay guarantees, adversarial validation.

---

### 3. Zod Schema Validation (Mandatory)

**v5 (Optional validation)**:
```javascript
function processData(data) {
  // Manual validation or none
}
```

**v6 (Zod enforcement)**:
```javascript
import { z } from 'zod';

const DataSchema = z.object({
  id: z.string().uuid(),
  content: z.string().min(1)
});

function processData(data) {
  const validated = DataSchema.parse(data); // Throws on invalid
  return validated;
}
```

**Migration Path**: Generate schemas from existing TypeScript types using `@unrdf/v6-compat/schema-generator`.

**Rationale**: Runtime safety, self-documenting APIs, deterministic validation.

---

### 4. Pure ESM (No CJS)

**v5 (Dual mode)**:
```json
{
  "main": "dist/index.js",
  "module": "dist/index.mjs",
  "exports": {
    "require": "./dist/index.js",
    "import": "./dist/index.mjs"
  }
}
```

**v6 (ESM only)**:
```json
{
  "type": "module",
  "main": "src/index.mjs",
  "exports": {
    ".": "./src/index.mjs"
  }
}
```

**Migration Path**: Use native ESM in Node 18+. No fallback.

**Rationale**: Simplify build, reduce bundle size, align with ecosystem.

---

### 5. Hook Lifecycle Changes

**v5 (Implicit execution)**:
```javascript
const hook = defineHook({
  name: 'validate',
  handler: async (ctx) => { /* ... */ }
});
```

**v6 (Explicit activation + receipts)**:
```javascript
const hook = defineHook({
  name: 'validate',
  schema: ValidationSchema, // Zod required
  handler: async (ctx) => { /* ... */ },
  receipt: true // Generate KGC-4D receipt
});

const receipt = await activateHook(hook, context);
```

**Migration Path**: Add `schema` and `receipt` fields to all hooks.

**Rationale**: Type safety, execution proof, replay guarantees.

---

### 6. Federation Query API

**v5 (String-based queries)**:
```javascript
const results = await federation.query('SELECT * WHERE { ?s ?p ?o }');
```

**v6 (Typed query builders)**:
```javascript
import { sparql } from '@unrdf/federation';

const results = await federation.query(
  sparql`SELECT * WHERE { ?s ?p ?o }`
    .timeout(5000)
    .receipt(true)
);
```

**Migration Path**: Use `@unrdf/v6-compat/sparql-adapter` for string queries.

**Rationale**: Prevent injection, enforce timeouts, generate receipts.

---

### 7. Streaming API Changes

**v5 (EventEmitter)**:
```javascript
stream.on('data', (quad) => { /* ... */ });
stream.on('error', (err) => { /* ... */ });
```

**v6 (AsyncIterator + Receipts)**:
```javascript
for await (const quad of stream) {
  // Process quad
}
// stream.receipt() available after completion
```

**Migration Path**: Use `@unrdf/v6-compat/stream-adapter`.

**Rationale**: Modern async patterns, backpressure handling, deterministic replay.

---

## Package-by-Package Migration Checklist

### Tier 1: Core Foundation (Required for all others)
- [ ] `@unrdf/oxigraph` - Pure Oxigraph adapter (L5)
- [ ] `@unrdf/core` - RDF operations + SPARQL (L5)
- [ ] `@unrdf/kgc-4d` - Receipt engine (L5)
- [ ] `@unrdf/v6-compat` - Migration bridge (L3)

### Tier 2: Essential Infrastructure
- [ ] `@unrdf/hooks` - Policy execution framework
- [ ] `@unrdf/streaming` - Change feeds
- [ ] `@unrdf/federation` - Distributed queries
- [ ] `@unrdf/cli` - Command-line tools

### Tier 3: Workflow & Orchestration
- [ ] `@unrdf/yawl` - Workflow engine
- [ ] `@unrdf/yawl-api` - HTTP API
- [ ] `@unrdf/yawl-observability` - OTEL integration
- [ ] `@unrdf/yawl-durable` - Persistence layer

### Tier 4: Advanced Features
- [ ] `@unrdf/knowledge-engine` - Reasoning
- [ ] `@unrdf/graph-analytics` - Graph algorithms
- [ ] `@unrdf/ml-inference` - ML integration
- [ ] `@unrdf/semantic-search` - Vector search

### Tier 5: Domain-Specific
- [ ] `@unrdf/blockchain` - Blockchain integration
- [ ] `@unrdf/serverless` - FaaS adapters
- [ ] `@unrdf/react` - React bindings
- [ ] `@unrdf/rdf-graphql` - GraphQL bridge

### Tier 6: Tooling & Extensions (27 remaining packages)
All KGC-CLI extensions, YAWL extensions, domain packages.

---

## Deprecation Timeline

### Phase 1: Alpha (Jan 2025)
- **v6.0.0-alpha.1**: Core packages (Tier 1-2) released
- **v5.0.x**: Full support, no deprecation warnings
- **Action**: Early adopters test core migration

### Phase 2: Beta (Feb 2025)
- **v6.0.0-beta.1**: All packages migrated
- **v5.0.x**: Deprecation warnings in logs (not breaking)
- **Action**: Production testing with compatibility layer

### Phase 3: RC (Mar 2025)
- **v6.0.0-rc.1**: Feature freeze, bug fixes only
- **v5.0.x**: Deprecation warnings escalate (console.warn)
- **Action**: Migration deadline announced (Q2 2025)

### Phase 4: Stable (Apr 2025)
- **v6.0.0**: Official release
- **v5.0.x**: Security fixes only (6-month EOL warning)
- **Action**: All new projects use v6

### Phase 5: End-of-Life (Oct 2025)
- **v5.x**: No updates, archive status
- **v6.x**: Standard support
- **Action**: All users must migrate or maintain v5 fork

---

## API Mapping Rules

### Store Creation
| v5 API | v6 API | Compat Layer |
|--------|--------|--------------|
| `new Store()` | `createStore()` | `v6Compat.createStore()` |
| `new N3.Parser()` | `@unrdf/core/rdf/n3-justified-only` | Built-in |
| Direct N3 imports | Centralized adapters | ESLint rule enforced |

### Workflow Execution
| v5 API | v6 API | Compat Layer |
|--------|--------|--------------|
| `workflow.run(task)` | `workflow.execute(task)` + receipt | `v6Compat.wrapWorkflow()` |
| No receipts | KGC-4D freeze | Auto-wrap adapter |

### Query Execution
| v5 API | v6 API | Compat Layer |
|--------|--------|--------------|
| `query(string)` | `query(sparql\`...\`)` | `v6Compat.querySparql()` |
| No timeout | 5s default | Config override |

### Streaming
| v5 API | v6 API | Compat Layer |
|--------|--------|--------------|
| `stream.on('data')` | `for await (const x of stream)` | `v6Compat.streamToAsync()` |

---

## Migration Verification

### Step 1: Dependency Audit
```bash
# Check for direct N3 imports (should be 0)
timeout 5s grep -r "from 'n3'" packages/*/src --include="*.mjs" | wc -l

# Check for v5 Store usage (should be 0)
timeout 5s grep -r "new Store()" packages/*/src --include="*.mjs" | wc -l
```

### Step 2: Test Coverage
```bash
# All packages must have 100% passing tests
timeout 10s pnpm test

# Check coverage report
timeout 5s pnpm test:coverage | grep "All files"
```

### Step 3: OTEL Validation
```bash
# Validate receipts (score ≥80/100)
node validation/run-all.mjs comprehensive
grep "Score:" validation-output.log
```

### Step 4: Type Safety
```bash
# No TypeScript errors
timeout 10s pnpm -r run typecheck
```

---

## Rollback Plan

If critical issues arise during migration:

1. **Keep v5 tags**: All v5.x releases tagged in Git
2. **Compatibility layer**: `@unrdf/v6-compat` supports both APIs
3. **Gradual migration**: Tier-by-tier rollout allows partial adoption
4. **Support period**: 6 months overlap (v5 + v6 both supported)

**Rollback Command**:
```bash
# Restore entire workspace to v5
git checkout v5.0.1
pnpm install

# Or per-package
pnpm add @unrdf/core@5.0.1
```

---

## Success Criteria

- [ ] All 47 packages at v6.0.0
- [ ] 100% test pass rate (no regressions)
- [ ] OTEL validation ≥80/100 for all packages
- [ ] Zero direct N3 imports outside justified modules
- [ ] All operations produce receipts (where applicable)
- [ ] Documentation updated with v6 examples
- [ ] Migration guide tested by 3+ external users

---

## References

- **KGC-4D Receipts**: `/packages/kgc-4d/docs/RECEIPTS.md`
- **Maturity Ladder**: `/docs/v6/MATURITY_LADDER.md`
- **Capsule Backlog**: `/docs/v6/CAPSULE_BACKLOG.md`
- **BB80/20 Methodology**: `/docs/bb80-20-methodology.md`
