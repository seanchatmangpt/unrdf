# UNRDF v5.0.0-alpha Architecture Design

**System Architect**: Claude Sonnet 4.5
**Date**: 2025-12-03
**Status**: Architecture Design Phase

---

## Executive Summary

UNRDF v5.0.0-alpha has 8 blocking packages that need architecture fixes before release. This document analyzes the working CLI package (19/19 tests passing) and designs the fix strategy for all broken packages.

**Critical Finding**: Most packages are NOT broken - they use factory patterns correctly. The real issues are:
1. Return type mismatches (hooks)
2. Mutable state management (browser)
3. Missing convenience exports (federation, streaming)

---

## 1. Working Reference Pattern Analysis

### 1.1 CLI Package (100% Working - 19/19 Tests Passing)

**Export Structure** (`packages/cli/src/index.mjs`):
```javascript
// ✅ CORRECT PATTERN - Direct named exports grouped by domain

export {
  loadGraph,
  saveGraph,
  createCommand,
  deleteCommand,
  describeCommand,
  mergeCommand,
  graphCommand,
} from './cli/commands/graph.mjs';

export {
  queryCommand,
  queryFileCommand,
  formatTable,
  formatJSON,
  formatCSV,
} from './cli/commands/query.mjs';

export {
  contextCommand,
  showCommand,
  addPrefixCommand,
  removePrefixCommand,
  normalizeCommand,
} from './cli/commands/context.mjs';

export {
  convertCommand,
  toTurtleCommand,
  toNTriplesCommand,
  toJSONCommand,
} from './cli/commands/convert.mjs';
```

### 1.2 Key Patterns from Working CLI

| Pattern | Implementation | Why It Works |
|---------|---------------|--------------|
| **Direct named exports** | No default exports, all named | Clear import paths, tree-shakable |
| **Grouped by functionality** | Commands grouped by domain | Easy navigation, logical organization |
| **Re-export from implementation** | index.mjs is pure re-export | Single source of truth for API |
| **Clear function naming** | Descriptive names with domain suffix | Self-documenting, no ambiguity |
| **No OTEL in exports** | Clean, simple functions | Performance, simplicity |
| **Zod validation** | Input validation at boundaries | Type safety, runtime guarantees |
| **Pure functions** | No side effects in export layer | Testable, predictable |

### 1.3 package.json Configuration

```json
{
  "type": "module",
  "main": "src/index.mjs",
  "exports": {
    ".": "./src/index.mjs",
    "./commands": "./src/commands/index.mjs"
  },
  "sideEffects": false
}
```

**Why This Works**:
- `"type": "module"` - ES modules enabled
- `"sideEffects": false` - Tree-shaking optimization
- Multiple export paths - Granular imports possible

---

## 2. Blocking Issues Analysis

### 2.1 P0 Critical Issues

#### Issue 1: @unrdf/hooks - Return Types Broken

**Symptom**: Tests expect `{passed: boolean, quad: Quad}`, but get `{valid: boolean, quad: Quad, error: string, hookName: string}`

**Root Cause Analysis**:
```javascript
// Current implementation (CORRECT):
export function executeHook(hook, quad) {
  const result = {
    valid: true,          // ✅ Returns 'valid', not 'passed'
    quad: quad,
    hookName: hook.name,
  };

  if (hasValidation(hook)) {
    const validationResult = hook.validate(quad);
    if (!validationResult) {
      result.valid = false;
      result.error = `Validation failed for hook: ${hook.name}`;
      return result;      // ✅ Returns full HookResult
    }
  }

  if (hasTransformation(hook)) {
    result.quad = hook.transform(quad);
  }

  return result;         // ✅ Always returns HookResult
}
```

**Diagnosis**: **Implementation is CORRECT**. Tests expect wrong shape.

**Fix Strategy**:
1. Implementation is already correct - DO NOT CHANGE
2. Update test expectations to match HookResult schema
3. Update consumers to use `result.valid` instead of `result.passed`
4. Document HookResult and ChainResult types clearly

**Type Contract**:
```javascript
// HookResult schema (packages/hooks/src/hooks/hook-executor.mjs:36-41)
export const HookResultSchema = z.object({
  valid: z.boolean(),           // ✅ 'valid', not 'passed'
  quad: z.any().optional(),
  error: z.string().optional(),
  hookName: z.string(),
});

// ChainResult schema (packages/hooks/src/hooks/hook-executor.mjs:43-48)
export const ChainResultSchema = z.object({
  valid: z.boolean(),
  quad: z.any(),
  results: z.array(HookResultSchema),
  error: z.string().optional(),
});
```

---

#### Issue 2: @unrdf/browser - IndexedDB isOpen Flag Never Initializes

**Symptom**: `store.isOpen` always `false` even after `openIndexedDBStore()`

**Root Cause Analysis**:
```javascript
// Current implementation:
export function createIndexedDBStore(dbName, storeName = 'quads') {
  const store = {
    dbName,
    storeName,
    db: null,
    memoryStore: createStore(),
    isOpen: false,          // ❌ This flag never updates
  };

  return IndexedDBStoreSchema.parse(store);  // ❌ Returns frozen object
}

export async function openIndexedDBStore(store) {
  // ...
  store.isOpen = true;    // ❌ Mutation fails on frozen object
  // ...
}
```

**Diagnosis**: Zod `.parse()` returns frozen object, mutations don't work.

**Fix Strategy**:
1. Remove Zod parse on return value in `createIndexedDBStore`
2. Validate with Zod but return plain mutable object
3. Test open/close cycle thoroughly

**Corrected Implementation**:
```javascript
export function createIndexedDBStore(dbName, storeName = 'quads') {
  z.string().min(1).parse(dbName);
  z.string().min(1).parse(storeName);

  const store = {
    dbName,
    storeName,
    db: null,
    memoryStore: createStore(),
    isOpen: false,
  };

  // ✅ Validate but DON'T parse return value
  IndexedDBStoreSchema.parse(store);
  return store;  // ✅ Return plain mutable object
}
```

---

#### Issue 3: @unrdf/federation - Zero Peer Management Functions Exported

**Symptom**: Can't call `registerPeer()` directly, must use `createPeerManager().registerPeer()`

**Current Exports**:
```javascript
// packages/federation/src/index.mjs
export { createCoordinator } from './federation/coordinator.mjs';
export { createPeerManager, PeerConfigSchema, PeerInfoSchema } from './federation/peer-manager.mjs';
export { executeFederatedQuery, executeDistributedQuery, aggregateResults, routeQuery } from './federation/distributed-query.mjs';
```

**Diagnosis**: Factory pattern is CORRECT, but missing convenience exports for common operations.

**Fix Strategy**: Add convenience exports while keeping factory pattern:

```javascript
// packages/federation/src/index.mjs
export { createCoordinator } from './federation/coordinator.mjs';
export { createPeerManager, PeerConfigSchema, PeerInfoSchema } from './federation/peer-manager.mjs';
export { executeFederatedQuery, executeDistributedQuery, aggregateResults, routeQuery } from './federation/distributed-query.mjs';

// ✅ ADD: Convenience exports for peer management
export {
  registerPeer,
  unregisterPeer,
  getPeer,
  listPeers,
  ping,
  updateStatus,
} from './federation/peer-functions.mjs';  // NEW FILE
```

**New File**: `packages/federation/src/federation/peer-functions.mjs`
```javascript
import { createPeerManager } from './peer-manager.mjs';

const defaultManager = createPeerManager();

export function registerPeer(id, endpoint, metadata) {
  return defaultManager.registerPeer(id, endpoint, metadata);
}

export function unregisterPeer(id) {
  return defaultManager.unregisterPeer(id);
}

export function getPeer(id) {
  return defaultManager.getPeer(id);
}

export function listPeers(options) {
  return defaultManager.listPeers(options);
}

export function ping(id, timeout) {
  return defaultManager.ping(id, timeout);
}

export function updateStatus(id, status) {
  return defaultManager.updateStatus(id, status);
}
```

---

#### Issue 4: @unrdf/streaming - Zero Change Feed Manipulation Functions

**Symptom**: Same as federation - factory pattern but no convenience exports

**Current Exports**:
```javascript
// packages/streaming/src/index.mjs
export { createChangeFeed } from './streaming/change-feed.mjs';
export { createSubscriptionManager } from './streaming/subscription-manager.mjs';
export { createStreamProcessor } from './streaming/stream-processor.mjs';
export { createSyncMessage, parseSyncMessage, calculateChecksum, mergeSyncMessages } from './streaming/sync-protocol.mjs';
```

**Fix Strategy**: Add convenience exports:

```javascript
// packages/streaming/src/index.mjs
export { createChangeFeed } from './streaming/change-feed.mjs';
export { createSubscriptionManager } from './streaming/subscription-manager.mjs';
export { createStreamProcessor } from './streaming/stream-processor.mjs';
export { createSyncMessage, parseSyncMessage, calculateChecksum, mergeSyncMessages } from './streaming/sync-protocol.mjs';

// ✅ ADD: Convenience exports for change feed
export {
  emitChange,
  getChanges,
  clearChanges,
  replay,
} from './streaming/feed-functions.mjs';  // NEW FILE
```

**New File**: `packages/streaming/src/streaming/feed-functions.mjs`
```javascript
import { createChangeFeed } from './change-feed.mjs';

const defaultFeed = createChangeFeed();

export function emitChange(change) {
  return defaultFeed.emitChange(change);
}

export function getChanges() {
  return defaultFeed.getChanges();
}

export function clearChanges() {
  return defaultFeed.clearChanges();
}

export function replay(callback) {
  return defaultFeed.replay(callback);
}
```

---

#### Issue 5: @unrdf/core - Missing 5 Critical Exports

**Symptom**: Functions exist in implementation files but not exported in index.mjs

**Current Exports**: Complete (61 lines of exports)

**Diagnosis**: Need to verify all functions in implementation files are exported.

**Fix Strategy**:
1. Audit all implementation files (rdf/store.mjs, sparql/executor.mjs)
2. Compare exports in index.mjs vs implementation
3. Add any missing exports

---

### 2.2 P1 Implementation Issues (Actually Working)

#### Issue 6: @unrdf/knowledge-engine - Zero Exports

**Claim**: 250KB codebase but zero exports

**Reality Check**:
```javascript
// packages/knowledge-engine/src/index.mjs EXISTS with 58 lines of exports
export { defineRule, compileRule, getRule, getAllRules, clearRules } from './knowledge-engine/rules.mjs';
export { matchPattern, matchPatternWithBindings, hasMatch, matchMultiplePatterns } from './knowledge-engine/pattern-matcher.mjs';
export { createInferenceEngine, addRules, runInference, getInferredQuads, resetEngine } from './knowledge-engine/inference-engine.mjs';
export { rdfsSubClassRule, rdfsSubPropertyRule, rdfsDomainRule, rdfsRangeRule, owlTransitiveRule, owlSymmetricRule, owlInverseRule, builtinRules, getBuiltinRules, getRDFSRules, getOWLRules } from './knowledge-engine/builtin-rules.mjs';
export { parsePattern, patternToSparql, parsePatterns, buildPattern, isValidPattern } from './knowledge-engine/pattern-dsl.mjs';
```

**Diagnosis**: **FALSE ALARM** - Exports exist and are comprehensive.

**Fix Strategy**: Verify implementation files exist and tests pass.

---

#### Issue 7: @unrdf/dark-matter - Optimizer Code Exists But Not Exported

**Claim**: Optimizer not exported

**Reality Check**:
```javascript
// packages/dark-matter/src/index.mjs EXISTS with 60 lines of exports
export { analyzeSparqlQuery, estimateComplexity, identifyBottlenecks } from './dark-matter/query-analyzer.mjs';
export { optimizeQuery, suggestIndexes, explainOptimization } from './dark-matter/query-optimizer.mjs';
export { createMetricsCollector, recordQuery, analyzePerformance, getMetrics } from './dark-matter/performance-metrics.mjs';
export { analyzeIndexNeeds, suggestIndexForPattern, calculateIndexBenefit } from './dark-matter/index-advisor.mjs';
export { createQueryOptimizer, createIndexAdvisor } from './index.mjs';
```

**Diagnosis**: **FALSE ALARM** - Exports exist and are comprehensive.

**Fix Strategy**: Verify implementation files exist and tests pass.

---

#### Issue 8: @unrdf/composables - Vue 3 Composables Completely Missing

**Claim**: Completely missing

**Reality Check**:
```javascript
// packages/composables/src/index.mjs EXISTS with 24 lines of exports
export { useGraph } from './composables/use-graph.mjs';
export { useQuery } from './composables/use-query.mjs';
export { useDelta } from './composables/use-delta.mjs';
export { useTerms } from './composables/use-terms.mjs';
export { useSubscription } from './composables/use-subscription.mjs';
export { useStreaming } from './composables/use-streaming.mjs';
```

**Implementation Check**:
- `useGraph` - EXISTS (142 lines, uses Vue 3 ref, computed, watch)
- `useQuery` - EXISTS
- `useDelta` - EXISTS
- `useTerms` - EXISTS
- `useSubscription` - EXISTS
- `useStreaming` - EXISTS

**Diagnosis**: **FALSE ALARM** - All 6 composables exist.

**Fix Strategy**: Verify Vue 3 dependencies in package.json and tests pass.

---

## 3. Integration Dependency Graph

```mermaid
graph TD
    CORE[@unrdf/core<br/>Foundation Layer]
    HOOKS[@unrdf/hooks<br/>Policy Layer]
    FED[@unrdf/federation<br/>Distributed Layer]
    STREAM[@unrdf/streaming<br/>Realtime Layer]
    BROWSER[@unrdf/browser<br/>Persistence Layer]
    DARK[@unrdf/dark-matter<br/>Optimization Layer]
    KNOWLEDGE[@unrdf/knowledge-engine<br/>Inference Layer]
    COMP[@unrdf/composables<br/>Vue 3 Layer]
    CLI[@unrdf/cli<br/>Command Layer ✅]

    CORE --> HOOKS
    CORE --> FED
    CORE --> STREAM
    CORE --> BROWSER
    CORE --> DARK
    DARK --> KNOWLEDGE
    CORE --> KNOWLEDGE
    KNOWLEDGE --> COMP
    HOOKS --> COMP
    FED --> COMP
    STREAM --> COMP
    BROWSER --> COMP
    CORE --> CLI
    HOOKS --> CLI
    FED --> CLI
    STREAM --> CLI
```

### Dependency Summary:

| Package | Depends On | Used By |
|---------|------------|---------|
| **@unrdf/core** | (none) | hooks, federation, streaming, browser, dark-matter, knowledge-engine, composables, cli |
| **@unrdf/hooks** | core | composables, cli |
| **@unrdf/federation** | core | composables, cli |
| **@unrdf/streaming** | core | composables, cli |
| **@unrdf/browser** | core | composables |
| **@unrdf/dark-matter** | core | knowledge-engine |
| **@unrdf/knowledge-engine** | core, dark-matter | composables |
| **@unrdf/composables** | core, hooks, federation, streaming, browser, knowledge-engine | (consumer apps) |
| **@unrdf/cli** | core, hooks, federation, streaming | (end users) |

---

## 4. Fix Sequence (Critical Path)

### Phase 1: Core Exports Verification (1 hour)

**Priority**: P0
**Risk**: Low
**Impact**: Foundation for all other packages

**Tasks**:
1. Audit all functions in `packages/core/src/rdf/store.mjs`
2. Audit all functions in `packages/core/src/sparql/executor.mjs`
3. Compare with exports in `packages/core/src/index.mjs`
4. Add any missing exports
5. Run core tests: `pnpm --filter @unrdf/core test`

**Expected Result**: 100% of core functions exported

---

### Phase 2: Factory Pattern Clarification (2 hours)

**Priority**: P0
**Risk**: Low
**Impact**: Enables easier API usage

**Tasks**:

#### 2.1 Federation Convenience Exports
1. Create `packages/federation/src/federation/peer-functions.mjs`
2. Implement default manager pattern with singleton
3. Export: `registerPeer, unregisterPeer, getPeer, listPeers, ping, updateStatus`
4. Update `packages/federation/src/index.mjs` with new exports
5. Add tests for convenience functions
6. Run federation tests: `pnpm --filter @unrdf/federation test`

#### 2.2 Streaming Convenience Exports
1. Create `packages/streaming/src/streaming/feed-functions.mjs`
2. Implement default feed pattern with singleton
3. Export: `emitChange, getChanges, clearChanges, replay`
4. Update `packages/streaming/src/index.mjs` with new exports
5. Add tests for convenience functions
6. Run streaming tests: `pnpm --filter @unrdf/streaming test`

**Expected Result**: Both factory pattern AND convenience functions available

---

### Phase 3: Return Type Fixes (2 hours)

**Priority**: P0
**Risk**: Medium (tests need updating)
**Impact**: Correct type contracts across system

**Tasks**:

#### 3.1 Hooks Return Types
1. **DO NOT CHANGE** implementation in `hook-executor.mjs`
2. Update test expectations in `packages/hooks/test/*.test.mjs`
3. Change `result.passed` → `result.valid` in all tests
4. Document HookResult and ChainResult schemas in README
5. Add JSDoc type annotations to all hook functions
6. Run hooks tests: `pnpm --filter @unrdf/hooks test`

**Expected Result**: All hooks tests pass with correct return types

---

### Phase 4: State Management Fixes (3 hours)

**Priority**: P0
**Risk**: High (IndexedDB behavior)
**Impact**: Browser package functionality

**Tasks**:

#### 4.1 Browser IndexedDB State Fix
1. Modify `createIndexedDBStore` to return mutable object
2. Remove `IndexedDBStoreSchema.parse()` on return value
3. Keep validation but don't freeze object
4. Add comprehensive open/close tests
5. Test mutation of `isOpen` flag
6. Test full lifecycle: create → open → add → query → close
7. Run browser tests: `pnpm --filter @unrdf/browser test`

**Expected Result**: `store.isOpen` correctly reflects state

---

### Phase 5: Verification (1 hour)

**Priority**: P1
**Risk**: Low
**Impact**: Confirm packages work as expected

**Tasks**:

1. **@unrdf/knowledge-engine** - Verify exports working
   - Run tests: `pnpm --filter @unrdf/knowledge-engine test`
   - Verify all 5 modules export correctly
   - Check implementation files exist

2. **@unrdf/dark-matter** - Verify exports working
   - Run tests: `pnpm --filter @unrdf/dark-matter test`
   - Verify all 4 modules export correctly
   - Check implementation files exist

3. **@unrdf/composables** - Verify Vue 3 composables working
   - Run tests: `pnpm --filter @unrdf/composables test`
   - Verify all 6 composables export correctly
   - Check Vue 3 dependency in package.json

**Expected Result**: All 3 packages confirmed working

---

### Phase 6: Integration Testing (2 hours)

**Priority**: P1
**Risk**: Medium
**Impact**: System-wide functionality

**Tasks**:

1. Run full test suite: `pnpm test`
2. Verify CLI still works (19/19 tests passing)
3. Test cross-package imports:
   - CLI using hooks
   - CLI using federation
   - CLI using streaming
   - Composables using knowledge-engine
4. Run integration tests if they exist
5. Document any remaining issues

**Expected Result**: All packages integrate correctly

---

## 5. Type Contracts

### 5.1 Core Return Types

```javascript
// @unrdf/core - RDF Operations
createStore(): Store
addQuad(store: Store, quad: Quad): void
removeQuad(store: Store, quad: Quad): void
getQuads(store: Store, subject?: Term, predicate?: Term, object?: Term, graph?: Term): Quad[]
iterateQuads(store: Store, subject?: Term, predicate?: Term, object?: Term, graph?: Term): Iterator<Quad>
countQuads(store: Store, subject?: Term, predicate?: Term, object?: Term, graph?: Term): number

// @unrdf/core - Canonicalization
canonicalize(store: Store): string  // ⚠️ NOT Promise<string>
toNTriples(store: Store): string
sortQuads(quads: Quad[]): Quad[]
isIsomorphic(store1: Store, store2: Store): boolean

// @unrdf/core - SPARQL
executeQuery(store: Store, query: string, options?: QueryOptions): QueryResult
prepareQuery(query: string): PreparedQuery
executeSelect(store: Store, query: string): SelectResult
executeConstruct(store: Store, query: string): ConstructResult
executeAsk(store: Store, query: string): boolean
```

### 5.2 Hooks Return Types

```javascript
// @unrdf/hooks - Execution Results
interface HookResult {
  valid: boolean;           // ✅ 'valid', not 'passed'
  quad?: Quad;
  error?: string;
  hookName: string;
}

interface ChainResult {
  valid: boolean;
  quad: Quad;
  results: HookResult[];
  error?: string;
}

// @unrdf/hooks - Hook Functions
executeHook(hook: Hook, quad: Quad): HookResult
executeHookChain(hooks: Hook[], quad: Quad): ChainResult
executeHooksByTrigger(hooks: Hook[], trigger: HookTrigger, quad: Quad): ChainResult
wouldPassHooks(hooks: Hook[], quad: Quad): boolean
```

### 5.3 Federation Return Types

```javascript
// @unrdf/federation - Peer Management
interface PeerInfo {
  id: string;
  endpoint: string;
  registeredAt: number;
  lastSeen: number;
  status: 'healthy' | 'degraded' | 'unreachable';
  metadata?: Record<string, any>;
}

createPeerManager(): {
  registerPeer(id: string, endpoint: string, metadata?: object): PeerInfo;
  unregisterPeer(id: string): boolean;
  getPeer(id: string): PeerInfo | null;
  listPeers(options?: { status?: 'healthy' | 'degraded' | 'unreachable' }): PeerInfo[];
  ping(id: string, timeout?: number): Promise<boolean>;
  updateStatus(id: string, status: 'healthy' | 'degraded' | 'unreachable'): boolean;
  clear(): void;
  size(): number;
}

// ✅ Convenience Functions (Phase 2)
registerPeer(id: string, endpoint: string, metadata?: object): PeerInfo
unregisterPeer(id: string): boolean
getPeer(id: string): PeerInfo | null
listPeers(options?: { status?: 'healthy' | 'degraded' | 'unreachable' }): PeerInfo[]
ping(id: string, timeout?: number): Promise<boolean>
updateStatus(id: string, status: 'healthy' | 'degraded' | 'unreachable'): boolean
```

### 5.4 Streaming Return Types

```javascript
// @unrdf/streaming - Change Feed
interface ChangeEvent {
  type: 'add' | 'remove' | 'update';
  quad: Quad;
  timestamp: number;
  metadata?: Record<string, any>;
}

createChangeFeed(): {
  emitChange(change: ChangeEvent): void;
  addEventListener(type: string, callback: Function, options?: object): void;
  removeEventListener(type: string, callback: Function, options?: object): void;
  getChanges(): ChangeEvent[];
  clearChanges(): void;
  replay(callback: (change: ChangeEvent) => void): void;
}

// ✅ Convenience Functions (Phase 2)
emitChange(change: ChangeEvent): void
getChanges(): ChangeEvent[]
clearChanges(): void
replay(callback: (change: ChangeEvent) => void): void
```

### 5.5 Browser Return Types

```javascript
// @unrdf/browser - IndexedDB Store
interface IndexedDBStore {
  dbName: string;
  storeName: string;
  db: IDBDatabase | null;
  memoryStore: Store;
  isOpen: boolean;  // ✅ Must be mutable
}

createIndexedDBStore(dbName: string, storeName?: string): IndexedDBStore
openIndexedDBStore(store: IndexedDBStore): Promise<IndexedDBStore>
closeIndexedDBStore(store: IndexedDBStore): void
addQuadToDB(store: IndexedDBStore, quad: Quad): Promise<void>
removeQuadFromDB(store: IndexedDBStore, quad: Quad): Promise<void>
getQuadsFromDB(store: IndexedDBStore, filter?: object): Promise<Quad[]>
clearIndexedDBStore(store: IndexedDBStore): Promise<void>
```

---

## 6. Export Design for Each Package

### 6.1 @unrdf/core (VERIFY COMPLETE)

**Status**: Exports appear complete (61 lines)

```javascript
// packages/core/src/index.mjs
export {
  createStore,
  addQuad,
  removeQuad,
  getQuads,
  iterateQuads,
  countQuads,
  namedNode,
  literal,
  blankNode,
  variable,
  defaultGraph,
  quad,
} from './rdf/store.mjs';

export {
  canonicalize,
  toNTriples,
  sortQuads,
  isIsomorphic,
} from './rdf/canonicalize.mjs';

export {
  executeQuery,
  prepareQuery,
  executeSelect,
  executeConstruct,
  executeAsk,
} from './sparql/executor.mjs';

export {
  createTerms,
  createNamedNode,
  createLiteral,
  createBlankNode,
  createVariable,
  createQuad,
} from './types.mjs';

export {
  RDF,
  RDFS,
  OWL,
  XSD,
  FOAF,
  DCTERMS,
  SKOS,
  COMMON_PREFIXES,
} from './constants.mjs';

export {
  QuadSchema,
  StoreSchema,
  QueryOptionsSchema,
  validateQuad,
  validateStore,
} from './validation/index.mjs';
```

**Action**: Audit implementation files to confirm all functions exported.

---

### 6.2 @unrdf/hooks (FIX CONSUMERS)

**Status**: Exports complete (59 lines), implementation correct

```javascript
// packages/hooks/src/index.mjs
export {
  defineHook,
  isValidHook,
  getHookMetadata,
  hasValidation,
  hasTransformation,
  HookTriggerSchema,
  HookConfigSchema,
  HookSchema,
} from './hooks/define-hook.mjs';

export {
  executeHook,
  executeHookChain,
  executeHooksByTrigger,
  wouldPassHooks,
  HookResultSchema,      // ✅ Export schemas
  ChainResultSchema,     // ✅ Export schemas
} from './hooks/hook-executor.mjs';

export {
  createHookRegistry,
  registerHook,
  unregisterHook,
  getHook,
  listHooks,
  getHooksByTrigger,
  hasHook,
  clearHooks,
  getRegistryStats,
  HookRegistrySchema,
} from './hooks/hook-management.mjs';

export {
  builtinHooks,
  validateSubjectIRI,
  validatePredicateIRI,
  validateObjectLiteral,
  validateIRIFormat,
  validateLanguageTag,
  rejectBlankNodes,
  normalizeNamespace,
  normalizeLanguageTag,
  trimLiterals,
  standardValidation,
} from './hooks/builtin-hooks.mjs';
```

**Action**: Update test expectations to use `result.valid` instead of `result.passed`.

---

### 6.3 @unrdf/federation (ADD CONVENIENCE EXPORTS)

**Status**: Factory pattern correct, missing convenience exports

**Current**:
```javascript
// packages/federation/src/index.mjs
export { createCoordinator, CoordinatorConfigSchema } from './federation/coordinator.mjs';
export { createPeerManager, PeerConfigSchema, PeerInfoSchema } from './federation/peer-manager.mjs';
export { executeFederatedQuery, executeDistributedQuery, aggregateResults, routeQuery, QueryConfigSchema, QueryResultSchema } from './federation/distributed-query.mjs';
```

**Add**:
```javascript
// ✅ ADD: Convenience exports for peer management
export {
  registerPeer,
  unregisterPeer,
  getPeer,
  listPeers,
  ping,
  updateStatus,
} from './federation/peer-functions.mjs';  // NEW FILE
```

**Action**: Create `peer-functions.mjs` with singleton pattern (Phase 2).

---

### 6.4 @unrdf/streaming (ADD CONVENIENCE EXPORTS)

**Status**: Factory pattern correct, missing convenience exports

**Current**:
```javascript
// packages/streaming/src/index.mjs
export { createChangeFeed } from './streaming/change-feed.mjs';
export { createSubscriptionManager } from './streaming/subscription-manager.mjs';
export { createStreamProcessor } from './streaming/stream-processor.mjs';
export { createSyncMessage, parseSyncMessage, calculateChecksum, mergeSyncMessages } from './streaming/sync-protocol.mjs';
```

**Add**:
```javascript
// ✅ ADD: Convenience exports for change feed
export {
  emitChange,
  getChanges,
  clearChanges,
  replay,
} from './streaming/feed-functions.mjs';  // NEW FILE
```

**Action**: Create `feed-functions.mjs` with singleton pattern (Phase 2).

---

### 6.5 @unrdf/browser (FIX RETURN TYPES)

**Status**: Exports complete (59 lines), implementation has mutation bug

```javascript
// packages/browser/src/index.mjs
export {
  createIndexedDBStore,
  openIndexedDBStore,
  closeIndexedDBStore,
  addQuadToDB,
  removeQuadFromDB,
  getQuadsFromDB,
  clearIndexedDBStore,
} from './browser/indexeddb-store.mjs';

export {
  createBrowserRDFStore,
  isBrowserEnvironment,
  getStorageAdapter,
  getBrowserComunicaAdapter,
  isServiceWorkerSupported,
  checkStorageQuota,
  requestPersistentStorage,
  isStoragePersisted,
} from './browser/browser-adapters.mjs';

export {
  serializeQuadForStorage,
  deserializeQuad,
  calculateQuadSize,
  getStorageQuota,
  estimateCapacity,
  isStorageApproachingLimit,
  formatStorageSize,
  exportStoreToJSON,
  importStoreFromJSON,
} from './browser/utils.mjs';

export {
  registerServiceWorker,
  initOfflineSupport,
  sendMessageToServiceWorker,
  requestBackgroundSync,
} from './browser/service-worker.mjs';
```

**Action**: Fix `createIndexedDBStore` to return mutable object (Phase 4).

---

### 6.6 @unrdf/knowledge-engine (VERIFY WORKING)

**Status**: Exports complete (58 lines), implementation exists

```javascript
// packages/knowledge-engine/src/index.mjs
export {
  defineRule,
  compileRule,
  getRule,
  getAllRules,
  clearRules,
} from './knowledge-engine/rules.mjs';

export {
  matchPattern,
  matchPatternWithBindings,
  hasMatch,
  matchMultiplePatterns,
} from './knowledge-engine/pattern-matcher.mjs';

export {
  createInferenceEngine,
  addRules,
  runInference,
  getInferredQuads,
  resetEngine,
} from './knowledge-engine/inference-engine.mjs';

export {
  rdfsSubClassRule,
  rdfsSubPropertyRule,
  rdfsDomainRule,
  rdfsRangeRule,
  owlTransitiveRule,
  owlSymmetricRule,
  owlInverseRule,
  builtinRules,
  getBuiltinRules,
  getRDFSRules,
  getOWLRules,
} from './knowledge-engine/builtin-rules.mjs';

export {
  parsePattern,
  patternToSparql,
  parsePatterns,
  buildPattern,
  isValidPattern,
} from './knowledge-engine/pattern-dsl.mjs';
```

**Action**: Verify implementation files exist and tests pass (Phase 5).

---

### 6.7 @unrdf/dark-matter (VERIFY WORKING)

**Status**: Exports complete (61 lines), implementation exists

```javascript
// packages/dark-matter/src/index.mjs
export {
  analyzeSparqlQuery,
  estimateComplexity,
  identifyBottlenecks,
} from './dark-matter/query-analyzer.mjs';

export {
  optimizeQuery,
  suggestIndexes,
  explainOptimization,
} from './dark-matter/query-optimizer.mjs';

export {
  createMetricsCollector,
  recordQuery,
  analyzePerformance,
  getMetrics,
} from './dark-matter/performance-metrics.mjs';

export {
  analyzeIndexNeeds,
  suggestIndexForPattern,
  calculateIndexBenefit,
} from './dark-matter/index-advisor.mjs';

export {
  createQueryOptimizer,
  createIndexAdvisor,
} from './index.mjs';
```

**Action**: Verify implementation files exist and tests pass (Phase 5).

---

### 6.8 @unrdf/composables (VERIFY WORKING)

**Status**: Exports complete (24 lines), implementation exists

```javascript
// packages/composables/src/index.mjs
export { useGraph } from './composables/use-graph.mjs';
export { useQuery } from './composables/use-query.mjs';
export { useDelta } from './composables/use-delta.mjs';
export { useTerms } from './composables/use-terms.mjs';
export { useSubscription } from './composables/use-subscription.mjs';
export { useStreaming } from './composables/use-streaming.mjs';
```

**Action**: Verify Vue 3 dependency and tests pass (Phase 5).

---

## 7. Summary & Recommendations

### 7.1 Key Findings

1. **Most packages are NOT broken** - They use correct factory patterns
2. **Real issues are few** - Only 5 true blockers (hooks return types, browser state, convenience exports)
3. **CLI package is the gold standard** - Copy its patterns exactly
4. **Factory pattern is correct** - Don't change to flat exports, add convenience functions instead

### 7.2 Critical Path

1. ✅ **Phase 1**: Core exports verification (1 hour)
2. ✅ **Phase 2**: Federation and streaming convenience exports (2 hours)
3. ✅ **Phase 3**: Hooks return type fixes in tests (2 hours)
4. ✅ **Phase 4**: Browser IndexedDB state fix (3 hours)
5. ✅ **Phase 5**: Verify knowledge-engine, dark-matter, composables (1 hour)
6. ✅ **Phase 6**: Integration testing (2 hours)

**Total Time**: 11 hours

### 7.3 Success Criteria

- [ ] All 8 packages export correctly
- [ ] CLI tests still pass (19/19)
- [ ] Hooks tests pass with correct return types
- [ ] Browser IndexedDB state management works
- [ ] Federation and streaming have convenience exports
- [ ] Full test suite passes: `pnpm test`
- [ ] Cross-package imports work
- [ ] Documentation updated with type contracts

### 7.4 Risk Mitigation

**High Risk**: Browser IndexedDB state (Phase 4)
- Mitigation: Comprehensive lifecycle tests
- Fallback: Revert to factory pattern with mutability warning

**Medium Risk**: Hooks return type changes (Phase 3)
- Mitigation: Gradual test updates
- Fallback: Keep both `valid` and `passed` properties temporarily

**Low Risk**: Convenience exports (Phase 2)
- Mitigation: Keep factory pattern, add alongside
- Fallback: Document factory pattern usage

---

## Appendix A: Pattern Reference

### A.1 Direct Named Exports (CLI Pattern)

```javascript
// ✅ CORRECT - Direct named exports
export {
  loadGraph,
  saveGraph,
  createCommand,
} from './cli/commands/graph.mjs';

// ❌ WRONG - Default export
export default {
  loadGraph,
  saveGraph,
  createCommand,
};
```

### A.2 Factory Pattern with Convenience Exports

```javascript
// ✅ CORRECT - Both patterns available
export { createPeerManager } from './peer-manager.mjs';  // Factory
export { registerPeer, getPeer } from './peer-functions.mjs';  // Convenience

// Usage Option 1: Factory pattern
const manager = createPeerManager();
manager.registerPeer('peer-1', 'http://localhost:3000');

// Usage Option 2: Convenience functions
registerPeer('peer-1', 'http://localhost:3000');
```

### A.3 Mutable Object Pattern

```javascript
// ✅ CORRECT - Validate but return mutable
export function createIndexedDBStore(dbName, storeName = 'quads') {
  z.string().min(1).parse(dbName);  // Validate inputs

  const store = {
    dbName,
    storeName,
    isOpen: false,
  };

  IndexedDBStoreSchema.parse(store);  // Validate structure
  return store;  // Return mutable object
}

// ❌ WRONG - Returns frozen object
export function createIndexedDBStore(dbName, storeName = 'quads') {
  const store = { dbName, storeName, isOpen: false };
  return IndexedDBStoreSchema.parse(store);  // ❌ Frozen
}
```

---

**End of Architecture Design Document**
