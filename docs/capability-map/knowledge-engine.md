# @unrdf/knowledge-engine Capability Map

**Version**: 5.0.1
**Status**: Production Ready
**Runtime**: Node.js ≥18.0.0
**Last Updated**: 2025-12-28

---

## Overview

Rule Engine, Inference, and Pattern Matching for UNRDF. Provides SPARQL query execution, SHACL validation, RDF canonicalization, N3 reasoning, transaction management, and AI-enhanced semantic search.

**Key Capabilities**:
- **Query Execution**: SPARQL SELECT/ASK/CONSTRUCT/DESCRIBE/UPDATE with optimization
- **Validation**: SHACL shape validation with detailed error reporting
- **Canonicalization**: RDF graph normalization with isomorphism detection
- **Reasoning**: N3 rule-based inference via eyereasoner
- **Transactions**: ACID-compliant RDF transactions with rollback
- **AI Search**: Semantic search with embedding-based similarity

**Package Exports**:
```javascript
import {
  query,
  validateShacl,
  canonicalize,
  reason,
  TransactionManager,
  KnowledgeSubstrateCore
} from '@unrdf/knowledge-engine';
```

**Dependencies**:
- Required: `@unrdf/core` (workspace), `@unrdf/oxigraph` (workspace), `@unrdf/streaming` (workspace), `eyereasoner` (^18.23.0)
- Optional: `@xenova/transformers` (^2.17.2) for AI search

**Evidence**:
- Test Coverage: Not specified
- Test Files: vitest-based test suite
- OTEL Validation: Not specified
- Example Files: Hook system examples

---

## Capability Atoms

### Core Capabilities (Tier 1)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `query()` | Function | Node | [src/query.mjs](file:///home/user/unrdf/packages/knowledge-engine/src/query.mjs) | C1 |
| `validateShacl()` | Function | Node | [src/validate.mjs](file:///home/user/unrdf/packages/knowledge-engine/src/validate.mjs) | C2 |
| `canonicalize()` | Function | Node | [src/canonicalize.mjs](file:///home/user/unrdf/packages/knowledge-engine/src/canonicalize.mjs) | C3 |
| `reason()` | Function | Node | [src/reason.mjs](file:///home/user/unrdf/packages/knowledge-engine/src/reason.mjs) | C4 |
| `TransactionManager` | Class | Node | [src/transaction.mjs](file:///home/user/unrdf/packages/knowledge-engine/src/transaction.mjs) | C5 |
| `KnowledgeHookManager` | Class | Node | [src/knowledge-hook-manager.mjs](file:///home/user/unrdf/packages/knowledge-engine/src/knowledge-hook-manager.mjs) | C6 |

**Verification**:
```bash
timeout 5s pnpm --filter @unrdf/knowledge-engine test
```

### Advanced Capabilities (Tier 2)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `KnowledgeSubstrateCore` | Class | Node | [src/knowledge-substrate-core.mjs](file:///home/user/unrdf/packages/knowledge-engine/src/knowledge-substrate-core.mjs) | C7 |
| `QueryOptimizer` | Class | Node | [src/query-optimizer.mjs](file:///home/user/unrdf/packages/knowledge-engine/src/query-optimizer.mjs) | C8 |
| `PolicyPackManager` | Class | Node | [src/policy-pack.mjs](file:///home/user/unrdf/packages/knowledge-engine/src/policy-pack.mjs) | C9 |
| `parseTurtle()` | Function | Node | [src/parse.mjs](file:///home/user/unrdf/packages/knowledge-engine/src/parse.mjs) | C10 |

### Experimental Capabilities (Tier 3)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| AI Search (via imports) | Module | Node | [src/index.mjs](file:///home/user/unrdf/packages/knowledge-engine/src/index.mjs) | C11 |
| `EffectSandbox` | Class | Node | [src/effect-sandbox.mjs](file:///home/user/unrdf/packages/knowledge-engine/src/effect-sandbox.mjs) | C12 |

---

## Composition Patterns

**C1**: **SPARQL Query** - Execute query → Return bindings
```javascript
import { query } from '@unrdf/knowledge-engine';

const results = await query(store, 'SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10');
// results = [{ s, p, o }, ...]
```

**C2**: **SHACL Validation** - Validate graph → Get violations
```javascript
import { validateShacl, hasValidationErrors } from '@unrdf/knowledge-engine';

const report = await validateShacl(dataStore, shapesStore);
if (hasValidationErrors(report)) {
  const errors = getValidationErrors(report);
  console.error('Validation failed:', errors);
}
```

**C3**: **Graph Canonicalization** - Normalize → Isomorphism check
```javascript
import { canonicalize, isIsomorphic } from '@unrdf/knowledge-engine';

const canonical1 = canonicalize(graph1);
const canonical2 = canonicalize(graph2);
const areEqual = isIsomorphic(canonical1, canonical2);
```

**C4**: **N3 Reasoning** - Apply rules → Infer new triples
```javascript
import { reason } from '@unrdf/knowledge-engine';

const inferred = await reason(dataStore, rulesStore);
// inferred = new triples derived from rules
```

**C5**: **Transactions** - Begin → Execute → Commit/Rollback
```javascript
import { TransactionManager } from '@unrdf/knowledge-engine';

const txManager = new TransactionManager(store);
const tx = await txManager.begin();

try {
  await tx.insert(quad1);
  await tx.delete(quad2);
  await tx.commit();
} catch (error) {
  await tx.rollback();
}
```

**C6**: **Hook Management** - Register hooks → Execute on events
```javascript
import { KnowledgeHookManager } from '@unrdf/knowledge-engine';

const hookManager = new KnowledgeHookManager();
hookManager.register({
  trigger: 'before-insert',
  condition: ({ quad }) => quad.predicate.value === 'http://example.org/property'
});
```

**C7**: **Knowledge Substrate** - Unified RDF substrate with all features
```javascript
import { createKnowledgeSubstrateCore } from '@unrdf/knowledge-engine';

const substrate = createKnowledgeSubstrateCore({
  enableReasoning: true,
  enableValidation: true,
  enableTransactions: true
});

await substrate.insert(quads);
const results = await substrate.query('SELECT ...');
```

**C8**: **Query Optimization** - Optimize SPARQL → Rewrite patterns
```javascript
import { QueryOptimizer } from '@unrdf/knowledge-engine';

const optimizer = new QueryOptimizer();
const optimized = optimizer.optimize('SELECT ?s WHERE { ?s ?p ?o . ?s ?p2 ?o2 }');
// Reordered patterns for efficiency
```

**C9**: **Policy Pack Management** - Load policy packs → Govern operations
```javascript
import { PolicyPackManager } from '@unrdf/knowledge-engine';

const policyManager = new PolicyPackManager();
await policyManager.load('/path/to/policy-pack');
```

**C10**: **RDF Parsing** - Parse Turtle/N-Triples/JSON-LD → Quads
```javascript
import { parseTurtle, toTurtle } from '@unrdf/knowledge-engine';

const quads = await parseTurtle(turtleString);
const serialized = toTurtle(quads);
```

**C11**: **AI-Enhanced Search** - Semantic search with embeddings
```javascript
// AI search capabilities available but require @xenova/transformers
// Example usage would involve embedding-based similarity search
```

**C12**: **Effect Sandbox** - Execute effects in isolated environment
```javascript
import { EffectSandbox } from '@unrdf/knowledge-engine';

const sandbox = new EffectSandbox();
const result = await sandbox.execute(effectFn, { context });
```

---

## Performance Model

**Theoretical Performance**:

Based on SPARQL query engine and reasoning:
- Time Complexity: O(n·m) for SPARQL (n=triples, m=pattern variables)
- Space Complexity: O(k) for result bindings (k=result count)
- Scalability: Memory-bound (entire graph in RAM)

**Empirical Benchmarks**:

Not available in performance-analysis.md. Package is optional extension.

**Performance Characteristics**:
- SPARQL queries depend on Oxigraph backend performance
- SHACL validation is O(n·s) where n=data size, s=shape complexity
- Reasoning is expensive (depends on rule complexity)
- Canonicalization is O(n log n) for sorting

**Optimization Strategies**:
1. **Query Optimizer**: Rewrite SPARQL for better execution plans
2. **Caching**: Cache query results and validation reports
3. **Incremental Reasoning**: Only re-infer affected triples

---

## Runtime Compatibility Matrix

| Capability | Node.js | Browser | BEAM/WASM | Notes |
|------------|---------|---------|-----------|-------|
| SPARQL Query | ✅ ≥18.0 | ✅ ES2020+ | ⏳ Planned | Via Oxigraph |
| SHACL Validation | ✅ ≥18.0 | ✅ ES2020+ | ⏳ Planned | Via Oxigraph |
| Canonicalization | ✅ ≥18.0 | ✅ ES2020+ | ✅ Supported | Universal |
| N3 Reasoning | ✅ ≥18.0 | ❌ Not supported | ❌ Not supported | Node-only |
| Transactions | ✅ ≥18.0 | ✅ ES2020+ | ⏳ Planned | Via Oxigraph |
| AI Search | ✅ ≥18.0 | ⚠️ Partial | ❌ Not supported | Requires transformers |

**Legend**:
- ✅ Fully supported
- ⏳ Planned/In progress
- ❌ Not supported
- ⚠️ Partial support

---

## Evidence & Verification

### Source Code References

All capability atoms are traceable to source:
- Query: [src/query.mjs](file:///home/user/unrdf/packages/knowledge-engine/src/query.mjs)
- Validation: [src/validate.mjs](file:///home/user/unrdf/packages/knowledge-engine/src/validate.mjs)
- Canonicalization: [src/canonicalize.mjs](file:///home/user/unrdf/packages/knowledge-engine/src/canonicalize.mjs)

### Verification Commands

**Quick Verification** (< 5 seconds):
```bash
timeout 5s pnpm --filter @unrdf/knowledge-engine test
```

---

## Cross-References

### Related Packages
- **@unrdf/oxigraph**: SPARQL backend
- **@unrdf/core**: Shared utilities
- **@unrdf/streaming**: Real-time validation

---

**Document Metadata**:
- **Template Version**: 1.0.0
- **Generated**: 2025-12-28
- **Maintainer**: @unrdf/core-team
- **Last Review**: 2025-12-28
- **Next Review**: 2026-03-28
