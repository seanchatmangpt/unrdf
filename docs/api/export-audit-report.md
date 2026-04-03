# Export Audit & TypeScript Definitions Report

**Generated**: 2025-12-20
**Audit Scope**: 21 packages in UNRDF monorepo

---

## Executive Summary

| Metric | Count | Status |
|--------|-------|--------|
| **Total Packages** | 21 | - |
| **Packages with src/index.mjs** | 16 | ✅ |
| **Named Exports Only** | 15/16 | ⚠️ 1 default export |
| **Has TypeScript Definitions** | 7/16 | ❌ 9 missing |
| **No src/index.mjs** | 3 | ⚠️ (docs, kgn, nextra) |
| **Transitive Re-exports** | 2 | ⚠️ (kgn, knowledge-engine) |

---

## Critical Issues

### 1. Default Export Found (CRITICAL)

**Package**: `@unrdf/oxigraph`
**Location**: `packages/oxigraph/src/index.mjs:30`

```javascript
// ❌ WRONG - Default export
export default {
  createStore,
  dataFactory,
  OxigraphStore,
};
```

**Impact**: Breaks ESM interop, inconsistent with other packages
**Priority**: HIGH
**Fix Required**: Remove default export, keep only named exports

---

### 2. Missing TypeScript Definitions

**9 packages missing dist/index.d.ts**:

1. ❌ `@unrdf/cli` - No dist/index.d.ts
2. ❌ `@unrdf/dark-matter` - No dist/index.d.ts (build.config.mjs missing)
3. ❌ `@unrdf/domain` - No dist/index.d.ts (build.config.mjs missing)
4. ❌ `@unrdf/federation` - No dist/index.d.ts (build.config.mjs missing)
5. ❌ `@unrdf/hooks` - No dist/index.d.ts (build.config.mjs missing)
6. ❌ `@unrdf/knowledge-engine` - No dist/index.d.ts (build.config.mjs missing)
7. ❌ `@unrdf/project-engine` - No dist/index.d.ts (build.config.mjs missing)
8. ❌ `@unrdf/streaming` - No dist/index.d.ts (build.config.mjs missing)
9. ❌ `@unrdf/test-utils` - No dist/index.d.ts (build.config.mjs missing)

**Root Cause**: Missing `build.config.mjs` files with `declaration: true`

---

### 3. Transitive Re-Exports (WARNING)

#### @unrdf/kgn
```javascript
// ⚠️ Namespace re-exports
export * as BaseTemplates from './base/index.js';
export * as NextJSTemplates from './templates/nextjs/index.js';
export * as OfficeTemplates from './templates/office/index.js';
export * as LaTeXTemplates from './templates/latex/index.js';
```

**Status**: Acceptable (namespace exports, not wildcard)

#### @unrdf/knowledge-engine
```javascript
// ⚠️ Wildcard re-export
export * from './schemas.mjs';
```

**Status**: Review required - ensure schemas.mjs is controlled API surface

---

## Packages with Complete Compliance

✅ **7 packages with TypeScript definitions**:

1. `@unrdf/atomvm` - dist/index.d.ts (30KB)
2. `@unrdf/composables` - dist/index.d.ts (86KB)
3. `@unrdf/core` - dist/index.d.ts (29KB)
4. `@unrdf/engine-gateway` - dist/index.d.ts (6KB)
5. `@unrdf/kgc-4d` - dist/index.d.ts (64KB)
6. `@unrdf/oxigraph` - dist/index.d.ts (5KB) ⚠️ has default export
7. `@unrdf/validation` - dist/index.d.ts (214KB)

---

## Public API Boundaries

### Well-Defined APIs (Named Exports Only)

#### @unrdf/core
```javascript
export { UnrdfStore, createStore as createUnrdfStore }
export { canonicalize, toNTriples, sortQuads, isIsomorphic }
export { RDF, RDFS, OWL, XSD, FOAF, DCTERMS, SKOS, COMMON_PREFIXES }
```

#### @unrdf/composables
```javascript
export { useGraph, useQuery, useDelta, useTerms }
export { useSubscription, useStreaming }
```

#### @unrdf/engine-gateway
```javascript
export { EngineGateway }
export { detectOperationType, N3_ONLY_OPS, OXIGRAPH_OPS }
export { validateN3Usage, validateOxigraphUsage }
```

#### @unrdf/federation
```javascript
export { createCoordinator }
export { createPeerManager, PeerConfigSchema, PeerInfoSchema }
export { createHealthEndpoint }
export { CoordinatorConfigSchema }
```

#### @unrdf/streaming
```javascript
export { createChangeFeed }
export { createSubscriptionManager }
export { createStreamProcessor }
```

---

## JSDoc Coverage Analysis

| Package | Direct Exports | JSDoc Blocks | Coverage |
|---------|----------------|--------------|----------|
| @unrdf/dark-matter | 2 | 3 | 150% ✅ |
| @unrdf/oxigraph | 2 | 3 | 150% ✅ |
| @unrdf/test-utils | 8 | 41 | 512% ✅ |
| Others | Re-exports only | N/A | - |

**Note**: Most packages use re-exports, so JSDoc coverage is delegated to source modules.

---

## Required Actions

### Priority 1: Fix Default Export

**File**: `packages/oxigraph/src/index.mjs`

```diff
-export default {
-  createStore,
-  dataFactory,
-  OxigraphStore,
-};
```

### Priority 2: Add Missing build.config.mjs

**Template** (copy from `packages/core/build.config.mjs`):

```javascript
import { defineBuildConfig } from 'unbuild';

export default defineBuildConfig({
  entries: ['src/index.mjs'],
  outDir: 'dist',
  declaration: true,
  rollup: {
    emitCJS: false,
    inlineDependencies: false
  }
});
```

**Apply to**:
- packages/dark-matter/build.config.mjs
- packages/domain/build.config.mjs
- packages/federation/build.config.mjs
- packages/hooks/build.config.mjs
- packages/knowledge-engine/build.config.mjs
- packages/project-engine/build.config.mjs
- packages/streaming/build.config.mjs
- packages/test-utils/build.config.mjs

### Priority 3: Build & Validate

```bash
# Build all packages
pnpm -r build

# Verify TypeScript definitions
timeout 10s npx tsc --noEmit --allowJs packages/*/dist/index.d.ts

# Check all packages have definitions
find packages/*/dist -name "index.d.ts" | wc -l  # Should be 16
```

---

## Verification Checklist

- [ ] Remove default export from @unrdf/oxigraph
- [ ] Add build.config.mjs to 9 packages
- [ ] Run `pnpm -r build` successfully
- [ ] Verify 16/16 packages have dist/index.d.ts
- [ ] Run TypeScript type checker (0 errors)
- [ ] Verify no new default exports introduced
- [ ] Review knowledge-engine wildcard re-export
- [ ] Update package.json "types" fields if needed

---

## Build System Configuration

### Current Setup

**Root**: `tsconfig.json` with `checkJs: false`, `strict: false`
**Packages**: `unbuild` with `declaration: true`
**Issue**: Some packages missing build configs

### Recommended Fix

1. Add build.config.mjs to all packages with src/index.mjs
2. Ensure package.json scripts include `"build": "unbuild"`
3. Verify unbuild dependency in devDependencies
4. Run builds in CI/CD pipeline

---

## Type Safety Metrics

| Package | .d.ts Size | Status |
|---------|-----------|--------|
| validation | 214 KB | ✅ Comprehensive |
| composables | 86 KB | ✅ Comprehensive |
| kgc-4d | 64 KB | ✅ Comprehensive |
| atomvm | 30 KB | ✅ Comprehensive |
| core | 29 KB | ✅ Comprehensive |
| engine-gateway | 6 KB | ✅ Complete |
| oxigraph | 5 KB | ⚠️ Has default export |

---

## Next Steps

1. **Immediate**: Fix oxigraph default export
2. **Short-term**: Add missing build configs
3. **Medium-term**: Build all packages and verify definitions
4. **Long-term**: Enforce no default exports in linting rules

---

## Conclusion

**Overall Status**: ⚠️ **ACTION REQUIRED**

- 1 default export blocking ESM consistency
- 9 packages missing TypeScript definitions
- Otherwise well-structured named exports

**Estimated Fix Time**: 30-60 minutes
**Risk Level**: LOW (additive changes only)
**Breaking Changes**: None (removing default export is additive)
