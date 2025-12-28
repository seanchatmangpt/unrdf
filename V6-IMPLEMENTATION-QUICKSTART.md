# V6 Implementation Quick-Start Guide

**Prerequisites**: Read V6-ARCHITECTURE-SUMMARY.md first (15 min read)

This guide shows exactly how to start implementing the v6 architecture.

---

## Phase 1: Foundation Setup (Week 1)

### Step 1.1: Create @unrdf/store

```bash
# Create package structure
mkdir -p packages/store/{src,test}
cd packages/store

# Create package.json
cat > package.json << 'EOF'
{
  "name": "@unrdf/store",
  "version": "6.0.0-alpha.1",
  "description": "Graph storage and SPARQL execution using Oxigraph",
  "type": "module",
  "main": "./src/index.mjs",
  "exports": {
    ".": "./src/index.mjs"
  },
  "dependencies": {
    "oxigraph": "^0.5.2"
  },
  "engines": {
    "node": ">=18.0.0"
  }
}
EOF

# Create initial implementation
cat > src/index.mjs << 'EOF'
/**
 * @unrdf/store - Graph storage and SPARQL execution
 * @module @unrdf/store
 */

import oxigraph from 'oxigraph';

/**
 * Create a new in-memory RDF store
 * @returns {Object} Store instance with execute method
 */
export function createStore() {
  const store = new oxigraph.Store();

  return {
    /**
     * Execute SPARQL query
     * @param {string} query - SPARQL query string
     * @returns {Promise<Array>} Query results
     */
    async execute(query) {
      return Array.from(store.query(query));
    },

    /**
     * Load quads into store
     * @param {Array} quads - RDF quads to load
     */
    async load(quads) {
      for (const quad of quads) {
        store.add(quad);
      }
    },

    /**
     * Get underlying Oxigraph store
     * @private
     */
    _native: store
  };
}

export { oxigraph };
EOF

# Create first test
cat > test/store.test.mjs << 'EOF'
import { describe, it, expect } from 'vitest';
import { createStore } from '../src/index.mjs';

describe('@unrdf/store', () => {
  it('creates store and executes query', async () => {
    const store = createStore();
    const results = await store.execute('SELECT * WHERE { ?s ?p ?o } LIMIT 1');
    expect(results).toBeInstanceOf(Array);
  });
});
EOF

# Run tests
pnpm test
```

**Migration source**: Extract from `packages/oxigraph/src/*` + `packages/core/src/sparql/execute.mjs`

**Success criteria**:
- ✅ Tests pass
- ✅ Zero dependencies on other @unrdf packages
- ✅ Benchmark: SPARQL query <10ms P95

---

### Step 1.2: Create @unrdf/rdf

```bash
mkdir -p packages/rdf/{src/{parsers,serializers,validation},test}
cd packages/rdf

cat > package.json << 'EOF'
{
  "name": "@unrdf/rdf",
  "version": "6.0.0-alpha.1",
  "description": "RDF data model, parsing, serialization, validation",
  "type": "module",
  "main": "./src/index.mjs",
  "exports": {
    ".": "./src/index.mjs",
    "./parsers": "./src/parsers/index.mjs",
    "./validation": "./src/validation/index.mjs"
  },
  "dependencies": {
    "@rdfjs/data-model": "^2.1.1",
    "n3": "^1.26.0",
    "rdf-validate-shacl": "^0.6.5",
    "zod": "^4.1.13"
  }
}
EOF

cat > src/index.mjs << 'EOF'
/**
 * @unrdf/rdf - RDF data model
 * @module @unrdf/rdf
 */

import * as rdf from '@rdfjs/data-model';

export const { quad, namedNode, literal, blankNode, defaultGraph } = rdf;

// Re-export parsers and validation as submodules
export { parseTurtle, parseJsonLd } from './parsers/index.mjs';
export { validateShacl } from './validation/index.mjs';
EOF

cat > src/parsers/index.mjs << 'EOF'
import { Parser } from 'n3';

export async function parseTurtle(text) {
  const parser = new Parser({ format: 'text/turtle' });
  return parser.parse(text);
}

export async function parseJsonLd(text) {
  // Implementation from @unrdf/core
}
EOF
```

**Migration source**: Extract from `packages/core/src/rdf/*`

**Success criteria**:
- ✅ Zero dependencies on other @unrdf packages
- ✅ All parsers work (Turtle, JSON-LD, N-Triples)
- ✅ SHACL validation functional

---

### Step 1.3: Create @unrdf/governance

```bash
mkdir -p packages/governance/{src/{receipts,time-travel,blockchain},test}
cd packages/governance

cat > package.json << 'EOF'
{
  "name": "@unrdf/governance",
  "version": "6.0.0-alpha.1",
  "description": "Provenance, receipts, time-travel, deterministic event sourcing",
  "type": "module",
  "main": "./src/index.mjs",
  "exports": {
    ".": "./src/index.mjs",
    "./time-travel": "./src/time-travel/index.mjs",
    "./receipts": "./src/receipts/index.mjs",
    "./blockchain": "./src/blockchain/index.mjs"
  },
  "dependencies": {
    "@unrdf/store": "workspace:*",
    "@unrdf/rdf": "workspace:*",
    "hash-wasm": "^4.12.0",
    "isomorphic-git": "^1.35.1"
  }
}
EOF

cat > src/index.mjs << 'EOF'
/**
 * @unrdf/governance - Cryptographic provenance
 * @module @unrdf/governance
 */

export { freezeUniverse, travelToTimestamp } from './time-travel/index.mjs';
export { generateReceipt, verifyReceipt } from './receipts/index.mjs';
export { anchorToBlockchain } from './blockchain/index.mjs';
EOF
```

**Migration source**:
- Core logic from `packages/kgc-4d/src/{freeze,time,store}.mjs`
- Merkle trees from `packages/receipts/src/merkle-batcher.mjs`
- Blockchain from `packages/blockchain/src/anchoring/*.mjs`
- Dedup Merkle implementations (choose best one)

**Success criteria**:
- ✅ Single Merkle tree implementation (not 3)
- ✅ Time-travel works (reconstruct state at timestamp T)
- ✅ Receipt generation <1ms P95
- ✅ Blockchain anchoring optional (feature flag)

---

## Phase 2: Runtime Layer (Week 2)

### Step 2.1: Create @unrdf/workflows

```bash
mkdir -p packages/workflows/{src/{engine,patterns,durable},test}
cd packages/workflows

cat > package.json << 'EOF'
{
  "name": "@unrdf/workflows",
  "version": "6.0.0-alpha.1",
  "description": "YAWL workflow engine with deterministic execution",
  "type": "module",
  "main": "./src/index.mjs",
  "dependencies": {
    "@unrdf/store": "workspace:*",
    "@unrdf/rdf": "workspace:*",
    "@unrdf/governance": "workspace:*"
  }
}
EOF
```

**Migration source**:
- Core engine from `packages/yawl/src/engine-*.mjs`
- Patterns from `packages/yawl/src/patterns/*.mjs`
- Durable execution from `packages/yawl-durable/src/*.mjs`

**Success criteria**:
- ✅ All 23 Van der Aalst patterns implemented
- ✅ Case lifecycle works (create, execute, complete)
- ✅ Saga pattern functional
- ✅ Deterministic execution (same inputs → same outputs)

---

### Step 2.2: Create @unrdf/runtime

```bash
mkdir -p packages/runtime/{src/{streaming,federation,consensus,collab},test}

cat > package.json << 'EOF'
{
  "name": "@unrdf/runtime",
  "version": "6.0.0-alpha.1",
  "description": "Streaming, federation, consensus, real-time sync",
  "dependencies": {
    "@unrdf/store": "workspace:*",
    "@unrdf/rdf": "workspace:*",
    "@unrdf/governance": "workspace:*"
  }
}
EOF
```

**Migration source**:
- Streaming from `packages/streaming/src/*.mjs`
- Federation from `packages/federation/src/*.mjs`
- Consensus from `packages/consensus/src/*.mjs`
- CRDTs from `packages/collab/src/*.mjs`

**Key consolidation**: Eliminate circular dependency between federation ↔ consensus

---

## Phase 3: Applications (Week 3)

### Step 3.1: Create @unrdf/integrations

```bash
mkdir -p packages/integrations/src/{rest,graphql,kafka,queue,serverless}

cat > src/index.mjs << 'EOF'
/**
 * @unrdf/integrations - External service adapters
 * Plugin model: import from subpaths
 */

// No default export - use subpaths:
// import { createRestServer } from '@unrdf/integrations/rest';
// import { KafkaProducer } from '@unrdf/integrations/kafka';
EOF

cat > src/rest/index.mjs << 'EOF'
// Migrate from packages/yawl-api/src/server.mjs
export function createRestServer(workflowEngine, options = {}) {
  // Express server setup
}
EOF

cat > src/kafka/index.mjs << 'EOF'
// Migrate from packages/yawl-kafka/src/*.mjs
export class KafkaProducer { }
export class KafkaConsumer { }
EOF
```

**Migration source**: Consolidate 6 adapter packages into single package with subpaths

---

## Testing Strategy

### Unit Tests (each package)
```bash
# Run package tests
pnpm -C packages/store test
pnpm -C packages/rdf test
pnpm -C packages/governance test

# Coverage requirement: 80%
pnpm -C packages/store test:coverage
```

### Integration Tests (monorepo /test)
```bash
# Create integration test suite
mkdir -p test/integration

cat > test/integration/e2e-workflow.test.mjs << 'EOF'
import { describe, it } from 'vitest';
import { createStore } from '@unrdf/store';
import { quad, namedNode, literal } from '@unrdf/rdf';
import { WorkflowEngine } from '@unrdf/workflows';
import { freezeUniverse } from '@unrdf/governance';

describe('E2E: Workflow with provenance', () => {
  it('executes workflow and generates receipt', async () => {
    const store = createStore();
    const engine = new WorkflowEngine(store);

    // Create workflow case
    const caseId = await engine.createCase({
      spec: 'purchase-order',
      data: { orderId: '123' }
    });

    // Execute tasks
    await engine.executeTask(caseId, 'approve-order');

    // Freeze universe and get receipt
    const receipt = await freezeUniverse(store);

    expect(receipt).toHaveProperty('merkleRoot');
    expect(receipt).toHaveProperty('timestamp');
  });
});
EOF

# Run integration tests
pnpm test test/integration/
```

---

## Migration Checklist

### Before Starting
- [ ] Read V6-ARCHITECTURE-PROPOSAL.md (comprehensive spec)
- [ ] Read V6-ARCHITECTURE-SUMMARY.md (15 min overview)
- [ ] Read V6-KILL-LIST-EVIDENCE.md (justifications)
- [ ] Set up v6 branch: `git checkout -b v6-rewrite`

### Phase 1: Foundation (Weeks 1-2)
- [ ] Create @unrdf/store package
  - [ ] Migrate Oxigraph bindings
  - [ ] Implement SPARQL execution
  - [ ] Write unit tests (80% coverage)
  - [ ] Run benchmarks (P95 <10ms)
- [ ] Create @unrdf/rdf package
  - [ ] Migrate data model
  - [ ] Migrate parsers (Turtle, JSON-LD)
  - [ ] Migrate SHACL validation
  - [ ] Write unit tests
- [ ] Create @unrdf/governance package
  - [ ] Consolidate kgc-4d, receipts, blockchain
  - [ ] Dedup Merkle tree implementations
  - [ ] Implement time-travel
  - [ ] Write unit tests
  - [ ] Benchmark receipt generation (<1ms)

### Phase 2: Runtime (Weeks 3-4)
- [ ] Create @unrdf/workflows
  - [ ] Migrate YAWL core engine (39K LOC)
  - [ ] Merge yawl-durable saga pattern
  - [ ] Test all 23 workflow patterns
  - [ ] Verify deterministic execution
- [ ] Create @unrdf/runtime
  - [ ] Consolidate streaming, federation, consensus, collab
  - [ ] Eliminate circular dependencies
  - [ ] Test distributed coordination
- [ ] Create @unrdf/hooks
  - [ ] Merge knowledge-engine rules
  - [ ] Unified policy framework
- [ ] Create @unrdf/observability
  - [ ] Consolidate OTEL instrumentation
  - [ ] Single metrics layer

### Phase 3: Applications (Weeks 5-6)
- [ ] Create @unrdf/integrations
  - [ ] Migrate 6 adapter packages
  - [ ] Test REST API, Kafka, GraphQL, etc.
- [ ] Create @unrdf/ai
  - [ ] Consolidate ML packages
  - [ ] Shared vector infrastructure
- [ ] Create @unrdf/ui
  - [ ] React hooks
  - [ ] Vue composables
  - [ ] D3 visualization
- [ ] Create @unrdf/cli
  - [ ] Consolidate 3 CLI packages
  - [ ] Plugin system
- [ ] Create @unrdf/tools
  - [ ] Testing utilities
  - [ ] Documentation generation

### Phase 4: Validation (Week 7)
- [ ] Run full test suite
  - [ ] Unit tests: 80% coverage minimum
  - [ ] Integration tests: All workflows pass
  - [ ] E2E tests: CLI commands work
- [ ] Performance benchmarks
  - [ ] Match or exceed v5 performance
  - [ ] Receipt generation <1ms P95
  - [ ] SPARQL queries <10ms P95
- [ ] Security audit
  - [ ] Credential detection scan
  - [ ] Injection vulnerability check
- [ ] Documentation review
  - [ ] All packages have README
  - [ ] API docs complete
  - [ ] Migration guide written

---

## Quality Gates

### Gate 1: Foundation Complete (End of Week 2)
**Criteria**:
- ✅ @unrdf/store, @unrdf/rdf, @unrdf/governance packages created
- ✅ All tests passing (≥80% coverage)
- ✅ Zero dependencies between foundation packages
- ✅ Benchmarks meet targets (SPARQL <10ms, receipts <1ms)

**Decision**: PASS → Continue to Phase 2 | FAIL → Fix foundation before continuing

---

### Gate 2: Runtime Complete (End of Week 4)
**Criteria**:
- ✅ @unrdf/workflows, @unrdf/runtime, @unrdf/hooks, @unrdf/observability created
- ✅ Workflow engine functional (23 patterns working)
- ✅ Federation/consensus operational
- ✅ No circular dependencies

**Decision**: PASS → Continue to Phase 3 | FAIL → Refactor runtime

---

### Gate 3: Applications Complete (End of Week 6)
**Criteria**:
- ✅ All 12 packages created
- ✅ Integration tests passing
- ✅ CLI functional
- ✅ Documentation complete

**Decision**: PASS → Final validation | FAIL → Complete applications

---

### Gate 4: Production Ready (End of Week 7)
**Criteria**:
- ✅ Test coverage ≥80% across all packages
- ✅ Performance benchmarks meet or exceed v5
- ✅ Security audit clean (0 critical issues)
- ✅ Documentation complete (README, API docs, migration guide)
- ✅ Quality score ≥70/100

**Decision**: PASS → Release v6.0.0 | FAIL → Address issues before release

---

## Quick Commands Reference

```bash
# Create new package
mkdir -p packages/<name>/{src,test}
cd packages/<name>
pnpm init

# Install dependencies
pnpm add <dependency>
pnpm add -D vitest

# Run tests
pnpm test                    # Single package
pnpm -r test                 # All packages
pnpm test:coverage           # With coverage

# Benchmarks
pnpm benchmark
pnpm benchmark:regression    # Compare to v5

# Lint and format
pnpm lint
pnpm format

# Build
pnpm build                   # All packages
pnpm -C packages/<name> build  # Single package
```

---

## Success Metrics Dashboard

Track these metrics weekly:

| Metric | Week 1 | Week 2 | Week 3 | Week 4 | Week 5 | Week 6 | Week 7 | Target |
|--------|--------|--------|--------|--------|--------|--------|--------|--------|
| Packages created | 3 | 3 | 7 | 10 | 12 | 12 | 12 | 12 |
| Test coverage | 60% | 80% | 70% | 75% | 80% | 85% | 90% | ≥80% |
| Build time | 45s | 40s | 35s | 30s | 25s | 20s | <20s | <20s |
| Circular deps | - | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| LOC migrated | 30K | 60K | 90K | 110K | 120K | 120K | 120K | 120K |

---

## Getting Help

- **Architecture questions**: Read V6-ARCHITECTURE-PROPOSAL.md
- **Merge decisions**: Read V6-KILL-LIST-EVIDENCE.md
- **Package boundaries unclear**: Check layer architecture diagram
- **Circular dependency**: Move shared code to lower layer
- **Performance regression**: Check benchmarks/baselines.json

---

## Next Steps

1. ✅ Read this entire guide (you just did!)
2. Create v6-rewrite branch
3. Start Phase 1, Step 1.1 (create @unrdf/store)
4. Write first test, make it pass
5. Iterate package by package
6. Pass quality gates
7. Ship v6.0.0

**Remember**: Ruthless simplicity. 12 packages, 3 layers, zero duplication.
