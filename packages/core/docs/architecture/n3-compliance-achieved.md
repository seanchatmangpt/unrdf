# ✅ N3 Compliance: 100% Isolation Achieved

## Executive Summary

**Status**: ✅ **100% COMPLIANT**

All N3 imports have been isolated to 2 justified modules. All other code uses Oxigraph.

## Compliance Metrics

| Metric | Value |
|--------|-------|
| **Total Files with N3 Imports** | 2 |
| **Justified N3 Modules** | 2 |
| **Compliance Rate** | 100% |
| **Core Code Files** | 0 N3 imports |
| **Test Files** | 0 direct N3 imports |

## Justified N3 Modules

### 1. `/src/rdf/n3-justified-only.mjs`

**Purpose**: Streaming RDF parsing/writing (ONLY justified use case)

**Exports**:
- `streamingParse()` - SAX-like streaming parser
- `streamingWrite()` - Streaming RDF serialization
- `createStreamParser()` - For large file streaming
- `createStreamWriter()` - For large file writing
- `UnrdfDataFactory` - DataFactory facade (backward compat)

**Why Justified**:
- N3's streaming parser uses SAX-like API (no DOM buffering)
- Oxigraph doesn't support backpressure-aware streaming yet
- Required for processing large RDF files without memory exhaustion

### 2. `/src/rdf/n3-migration.mjs`

**Purpose**: N3.Store backward compatibility ONLY

**Exports**:
- `isN3Store()` - Detect N3 Store instances
- `convertN3ToOxigraph()` - Convert N3 stores to Oxigraph
- `ensureOxigraphStore()` - Unified store handler
- `createN3Store()` - For backward compat tests ONLY

**Why Justified**:
- External code may pass N3.Store instances to our APIs
- Provides 100% backward compatibility without breaking changes
- Internal code NEVER creates N3 stores

## Architecture

```
┌─────────────────────────────────────────┐
│      Application Code                   │
│  (100% Oxigraph, 0% N3)                 │
└───────────────┬─────────────────────────┘
                │
                ├─────────────────────────────┐
                │                             │
                ▼                             ▼
    ┌──────────────────────┐    ┌──────────────────────┐
    │ n3-justified-only     │    │ n3-migration          │
    │ (Streaming ONLY)      │    │ (Backward Compat)     │
    │                       │    │                       │
    │ • streamingParse()    │    │ • isN3Store()         │
    │ • streamingWrite()    │    │ • convertN3ToOxigraph │
    │ • createStreamParser  │    │ • ensureOxigraphStore │
    └──────────────────────┘    └──────────────────────┘
                │                             │
                └─────────────┬───────────────┘
                              ▼
                        ┌──────────┐
                        │    N3     │
                        │ (Isolated)│
                        └──────────┘
```

## Refactoring Changes

### Core Modules

1. **`src/types.mjs`**
   - ❌ Before: `import { DataFactory } from 'n3'`
   - ✅ After: `import { dataFactory } from '@unrdf/oxigraph'`

2. **`src/rdf/store.mjs`**
   - ❌ Before: `import { Store, DataFactory } from 'n3'`
   - ✅ After: `import { createStore, dataFactory } from '@unrdf/oxigraph'`
   - Changed: `addQuad()` → `add()`, `removeQuad()` → `delete()`, `getQuads()` → `match()`

3. **`examples/rdf-parsing/src/index.mjs`**
   - ❌ Before: `import { Parser, Store } from 'n3'`
   - ✅ After: `import { createStore } from '@unrdf/oxigraph'`
   - Changed: All parsing uses `store.load()` instead of `N3.Parser`

### Tests

4. **`test/sparql/n3-backward-compat.test.mjs`**
   - ❌ Before: `import { Store } from 'n3'`
   - ✅ After: `import { createN3Store } from '../../src/rdf/n3-migration.mjs'`
   - Purpose: Test backward compatibility with N3.Store instances

## Benefits

### 1. **Maintainability**
- Clear separation of concerns
- Easy to identify N3 usage (only 2 files)
- Simple migration path when Oxigraph adds streaming

### 2. **Performance**
- Oxigraph is 10-100x faster than N3 for most operations
- Only use N3 where justified (streaming)
- Reduced memory footprint

### 3. **Type Safety**
- Consistent API surface (all Oxigraph)
- Fewer imports to maintain
- Single source of truth for RDF operations

### 4. **Future-Proof**
- When Oxigraph adds streaming, easy to remove N3 entirely
- Migration module can be deprecated without breaking changes
- Clear upgrade path

## Migration Guide

### For New Code

**❌ DON'T:**
```javascript
import { DataFactory } from 'n3';
const { namedNode } = DataFactory;
```

**✅ DO:**
```javascript
import { dataFactory } from '@unrdf/oxigraph';
const { namedNode } = dataFactory;
```

### For Streaming

**✅ USE n3-justified-only:**
```javascript
import { streamingParse } from '@unrdf/core/rdf/n3-justified-only';

const quads = await streamingParse(largeRdfFile, {
  format: 'text/turtle',
  baseIRI: 'http://example.org/'
});
```

### For N3 Store Compatibility

**✅ USE n3-migration:**
```javascript
import { ensureOxigraphStore } from '@unrdf/core/rdf/n3-migration';

export function processStore(store) {
  // Convert N3 stores to Oxigraph automatically
  const oxiStore = ensureOxigraphStore(store);
  return oxiStore.query('SELECT * WHERE { ?s ?p ?o }');
}
```

## Verification

```bash
# Count N3 imports (should be 2)
find . -type f -name "*.mjs" -exec grep -l "from 'n3'" {} \; | grep -v node_modules | wc -l

# List N3 files (should show only justified modules)
find . -type f -name "*.mjs" -exec grep -l "from 'n3'" {} \; | grep -v node_modules
```

**Expected Output:**
```
2
./src/rdf/n3-justified-only.mjs
./src/rdf/n3-migration.mjs
```

## Success Criteria

✅ Only 2 files import N3 directly
✅ Both imports are justified (streaming + backward compat)
✅ All core code uses Oxigraph
✅ All tests pass
✅ Backward compatibility maintained
✅ No breaking changes

## Next Steps

### Phase 4 (Future): Remove N3 Entirely

When Oxigraph adds streaming support:

1. Replace `streamingParse()` with Oxigraph streaming
2. Replace `streamingWrite()` with Oxigraph serialization
3. Deprecate `n3-justified-only.mjs`
4. Keep `n3-migration.mjs` for legacy support only
5. Remove N3 dependency entirely (except peer dependency)

### Monitoring

- [ ] Track N3 usage with linting rules
- [ ] Monitor Oxigraph streaming feature requests
- [ ] Benchmark Oxigraph vs N3 streaming performance
- [ ] Create migration timeline when Oxigraph streaming lands

## Conclusion

**100% N3 compliance achieved.** All N3 usage isolated to 2 justified modules with clear architectural boundaries and migration paths.

---

**Date**: 2025-12-04
**Status**: ✅ Complete
**Compliance**: 100%
