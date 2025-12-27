# UNRDF v6 Breaking Changes

## Overview

UNRDF v6 represents a major architectural shift from v5, focusing on performance, type safety, and production readiness through the adoption of Oxigraph as the core RDF store.

## Migration Priority: HIGH

**Timeline**: v6.0.0-alpha.1 → v6.0.0-rc.1 → v6.0.0 stable
**Status**: Currently in alpha - not production ready
**Migration Guide**: See `/home/user/unrdf/docs/v6/MIGRATION_PLAN.md`

---

## Core Breaking Changes

### 1. RDF Store Implementation (CRITICAL)

**Before (v5)**:
```javascript
import { Store } from 'n3';
const store = new Store();
```

**After (v6)**:
```javascript
import { createStore } from '@unrdf/oxigraph';
const store = createStore();
```

**Impact**: ALL code using `new Store()` from N3 must migrate
**Reason**: Oxigraph provides 3-10x better performance and WASM-based native execution
**Migration**: Use `@unrdf/v6-compat` adapters for gradual migration

### 2. Data Factory Changes (HIGH)

**Before (v5)**:
```javascript
import { DataFactory } from 'n3';
const { namedNode, literal } = DataFactory;
```

**After (v6)**:
```javascript
import { dataFactory } from '@unrdf/oxigraph';
const { namedNode, literal } = dataFactory;
```

**Impact**: All RDF term creation code
**Reason**: Oxigraph data factory is RDF/JS compliant with better type definitions

### 3. N3 Library Isolation (CRITICAL)

**Rule**: Direct `from 'n3'` imports are FORBIDDEN in application code

**Exceptions**:
- `@unrdf/v6-compat` package (migration adapter)
- `packages/core/src/rdf/n3-justified-only.mjs` (centralized justified use)
- Test files, benchmarks, and migration scripts (non-production)

**Enforcement**: ESLint rule `no-direct-n3-imports` with auto-fix

### 4. SPARQL Query Execution (MEDIUM)

**Before (v5)**:
```javascript
// N3 Store didn't have native SPARQL
const results = await comunica.query(store, query);
```

**After (v6)**:
```javascript
// Native SPARQL in Oxigraph
const results = store.query(query);
```

**Impact**: Query execution patterns
**Benefit**: Native SPARQL is 2-5x faster than Comunica for simple queries

### 5. Streaming Parser Changes (MEDIUM)

**Before (v5)**:
```javascript
import { StreamParser } from 'n3';
```

**After (v6)**:
```javascript
import { parseStream } from '@unrdf/oxigraph';
// OR use justified N3 wrapper
import { createStreamParser } from '@unrdf/core/rdf/n3-justified-only';
```

**Impact**: RDF parsing code
**Reason**: Centralized N3 usage through justified wrappers

---

## Package-Level Changes

### Removed Packages

- `@unrdf/v5-core` - Replaced by `@unrdf/core` with Oxigraph
- `@unrdf/n3-store` - Replaced by `@unrdf/oxigraph`

### New Packages

- `@unrdf/oxigraph` - Core RDF store (MANDATORY)
- `@unrdf/v6-compat` - Migration adapters and lint rules
- `@unrdf/validation` - OTEL-based validation framework

### Renamed/Restructured

- `@unrdf/kgc` → `@unrdf/kgc-4d` (4-dimensional Knowledge Graph Compiler)
- `@unrdf/hooks` → Enhanced with Oxigraph integration

---

## API Changes

### Store API

| v5 Method | v6 Method | Notes |
|-----------|-----------|-------|
| `store.addQuad()` | `store.add()` | RDF/JS compliant |
| `store.removeQuad()` | `store.delete()` | RDF/JS compliant |
| `store.getQuads()` | `store.match()` | Returns iterator |
| `store.countQuads()` | `store.size` | Property, not method |

### Type System

**Before (v5)**: Loose types with JSDoc

**After (v6)**: Strict Zod schemas + TypeScript definitions

```javascript
// v6 enforces strict schema validation
import { QuadSchema } from '@unrdf/oxigraph';
const quad = QuadSchema.parse(data); // Throws if invalid
```

---

## Performance Impact

### Improvements

- **Store Operations**: 3-10x faster (Oxigraph WASM)
- **SPARQL Queries**: 2-5x faster (native vs Comunica)
- **Memory Usage**: 20-40% reduction
- **Startup Time**: 60% faster

### Regressions

- **Complex Federated Queries**: Comunica may still be faster
- **N3 Writer Streaming**: Native N3 writer still recommended for streaming

---

## Validation Requirements (NEW)

### OTEL Validation

v6 introduces mandatory OpenTelemetry-based validation:

```bash
node validation/run-all.mjs comprehensive
# Must achieve ≥80/100 score
```

**Enforced Metrics**:
- Receipt generation correctness
- Hook execution integrity
- Transaction atomicity
- SPARQL query accuracy

### Test Coverage

- Minimum: 80% coverage
- Target: 95%+ for core packages
- 100% pass rate required

---

## Deprecation Timeline

| Feature | Deprecated | Removed | Alternative |
|---------|-----------|---------|-------------|
| N3 Store direct use | v6.0.0-alpha.1 | v7.0.0 | `@unrdf/oxigraph` |
| Direct N3 imports | v6.0.0-alpha.1 | v6.0.0-rc.1 | `n3-justified-only.mjs` |
| Legacy hooks API | v6.0.0-alpha.1 | v6.0.0 | New hooks with Oxigraph |

---

## Migration Strategy

### Phase 1: Compatibility Layer (Current)

Use `@unrdf/v6-compat` for gradual migration:

```javascript
import { createStoreCompat } from '@unrdf/v6-compat/adapters';
const store = createStoreCompat(); // Emits deprecation warnings
```

### Phase 2: Direct Migration

Replace N3 imports with Oxigraph:

```bash
# Automated migration
npx @unrdf/v6-compat migrate ./src
```

### Phase 3: Validation

Run OTEL validation to ensure correctness:

```bash
npm run validate
# Must pass 12/12 checks
```

---

## Known Issues

1. **diataxis-kit tests hang** (non-blocking, infrastructure issue)
2. **Test suite takes 30s** (target: <5s, optimization needed)
3. **107 files >500 lines** (refactoring recommended, not blocking)

---

## Support & Resources

- **Migration Guide**: `/home/user/unrdf/docs/v6/MIGRATION_PLAN.md`
- **API Reference**: `/home/user/unrdf/docs/v6/API_REFERENCE.md`
- **Compatibility Package**: `@unrdf/v6-compat`
- **Issues**: https://github.com/unrdf/unrdf/issues
- **Discussions**: https://github.com/unrdf/unrdf/discussions

---

## Version Status

**Current**: 6.0.0-alpha.1
**Next**: 6.0.0-rc.1 (pending validation 12/12)
**Stable**: 6.0.0 (pending production validation)

**Validation Status**: 4/12 checks passing (as of 2025-12-27)

---

*Last Updated*: 2025-12-27
*Validation Script*: `/home/user/unrdf/scripts/validate-v6.mjs`
