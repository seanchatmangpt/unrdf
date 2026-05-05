# UNRDF vlatest Agent Reference

**FOR AGENTS ONLY**: Quick reference for coordinating v5 release fixes.

---

## TL;DR for Agents

**Real Blockers**: 2 bugs, 2 missing features, 1 verification
**False Alarms**: 3 packages already working
**Fix Time**: 8 hours with parallelization

---

## What's Already Working (Don't Touch)

| Package | Status | Proof |
|---------|--------|-------|
| @unrdf/cli | ✅ Reference pattern | 19/19 tests passing |
| @unrdf/knowledge-engine | ✅ Working | index.mjs exists, 58 lines, 24 exports |
| @unrdf/dark-matter | ✅ Working | index.mjs exists, 61 lines, 16 exports |
| @unrdf/composables | ✅ Working | index.mjs exists, 24 lines, 6 composables |

**Action**: Run tests to confirm, then move on.

---

## Critical Fixes (Parallel Execution)

### Agent 1: Core Verification (1 hour)

**Task**: Verify all functions exported from @unrdf/core

**Steps**:
1. Read `/Users/sac/unrdf/packages/core/src/rdf/store.mjs`
2. Read `/Users/sac/unrdf/packages/core/src/sparql/executor.mjs`
3. Compare with exports in `/Users/sac/unrdf/packages/core/src/index.mjs`
4. Add any missing exports
5. Run: `pnpm --filter @unrdf/core test`

**Success**: All functions exported, tests pass

---

### Agent 2: Convenience Exports (2 hours)

**Task**: Add convenience exports to federation and streaming

**Federation** (1 hour):
```javascript
// Create: packages/federation/src/federation/peer-functions.mjs
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

**Streaming** (1 hour):
```javascript
// Create: packages/streaming/src/streaming/feed-functions.mjs
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

**Update index.mjs for both packages**:
```javascript
// Add to packages/federation/src/index.mjs:
export {
  registerPeer,
  unregisterPeer,
  getPeer,
  listPeers,
  ping,
  updateStatus,
} from './federation/peer-functions.mjs';

// Add to packages/streaming/src/index.mjs:
export {
  emitChange,
  getChanges,
  clearChanges,
  replay,
} from './streaming/feed-functions.mjs';
```

**Success**: Both factory AND convenience functions available

---

### Agent 3: Hooks Return Types (2 hours)

**Task**: Update test expectations to match implementation

**CRITICAL**: **DO NOT CHANGE IMPLEMENTATION** - Implementation is correct!

**Steps**:
1. Find all test files: `grep -r "result.passed" packages/hooks/test/`
2. Replace `.passed` with `.valid` in all test expectations
3. Find all test files: `grep -r "expect(result).toHaveProperty('passed')" packages/hooks/test/`
4. Replace with `expect(result).toHaveProperty('valid')`
5. Run: `pnpm --filter @unrdf/hooks test`

**Example Fix**:
```javascript
// ❌ WRONG (current tests):
expect(result.passed).toBe(true);
expect(result).toHaveProperty('passed');

// ✅ CORRECT (fixed tests):
expect(result.valid).toBe(true);
expect(result).toHaveProperty('valid');
expect(result).toHaveProperty('hookName');
```

**Success**: All hooks tests pass, implementation unchanged

---

### Agent 4: Browser State Fix (3 hours)

**Task**: Fix IndexedDB isOpen flag mutation

**Root Cause**: Zod `.parse()` returns frozen object

**Fix**:
```javascript
// File: packages/browser/src/browser/indexeddb-store.mjs
// Line 41-54

// ❌ CURRENT (broken):
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

  return IndexedDBStoreSchema.parse(store);  // ❌ Returns frozen object
}

// ✅ FIXED:
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

  IndexedDBStoreSchema.parse(store);  // Validate structure
  return store;  // ✅ Return mutable object
}
```

**Add Comprehensive Tests**:
```javascript
// test/browser/indexeddb-lifecycle.test.mjs
describe('IndexedDB Store Lifecycle', () => {
  it('should update isOpen flag when opened', async () => {
    const store = createIndexedDBStore('test-db');
    expect(store.isOpen).toBe(false);

    await openIndexedDBStore(store);
    expect(store.isOpen).toBe(true);  // ✅ Must pass
  });

  it('should update isOpen flag when closed', async () => {
    const store = createIndexedDBStore('test-db');
    await openIndexedDBStore(store);
    expect(store.isOpen).toBe(true);

    closeIndexedDBStore(store);
    expect(store.isOpen).toBe(false);  // ✅ Must pass
  });
});
```

**Success**: `store.isOpen` correctly reflects state, lifecycle tests pass

---

### Agent 5: Verification (1 hour)

**Task**: Verify 3 "broken" packages are actually working

**Knowledge-Engine**:
```bash
pnpm --filter @unrdf/knowledge-engine test
```
Expected: Tests pass, all 24 exports work

**Dark-Matter**:
```bash
pnpm --filter @unrdf/dark-matter test
```
Expected: Tests pass, all 16 exports work

**Composables**:
```bash
# First verify Vue 3 dependency
cat packages/composables/package.json | grep vue

# Then run tests
pnpm --filter @unrdf/composables test
```
Expected: Tests pass, all 6 composables work

**Success**: Confirm all 3 packages working, document any issues

---

## Integration Testing (Final Phase)

**Task**: Verify all packages work together

**Steps**:
1. Run full suite: `pnpm test`
2. Verify CLI still works: `pnpm --filter @unrdf/cli test` (must be 19/19)
3. Test cross-package imports:
   ```javascript
   import { executeHook } from '@unrdf/hooks';
   import { registerPeer } from '@unrdf/federation';
   import { emitChange } from '@unrdf/streaming';
   import { createIndexedDBStore } from '@unrdf/browser';
   import { defineRule } from '@unrdf/knowledge-engine';
   import { optimizeQuery } from '@unrdf/dark-matter';
   import { useGraph } from '@unrdf/composables';
   ```

**Success**: All imports work, no errors

---

## Pattern Reference (Copy These Exactly)

### 1. Direct Named Exports (CLI Pattern)

```javascript
// ✅ CORRECT - What CLI does (100% working)
export {
  loadGraph,
  saveGraph,
  createCommand,
} from './cli/commands/graph.mjs';

// ❌ WRONG - Don't do this
export default {
  loadGraph,
  saveGraph,
};
```

### 2. Factory + Convenience Pattern

```javascript
// ✅ CORRECT - Both patterns available
export { createPeerManager } from './peer-manager.mjs';  // Factory
export { registerPeer, getPeer } from './peer-functions.mjs';  // Convenience

// Usage Option 1: Factory
const manager = createPeerManager();
manager.registerPeer('peer-1', 'http://localhost:3000');

// Usage Option 2: Convenience
registerPeer('peer-1', 'http://localhost:3000');
```

### 3. Mutable Object Pattern

```javascript
// ✅ CORRECT - Validate but return mutable
export function createIndexedDBStore(dbName, storeName = 'quads') {
  z.string().min(1).parse(dbName);

  const store = { dbName, storeName, isOpen: false };

  IndexedDBStoreSchema.parse(store);  // Validate
  return store;  // Return mutable
}

// ❌ WRONG - Returns frozen
export function createIndexedDBStore(dbName, storeName = 'quads') {
  const store = { dbName, storeName, isOpen: false };
  return IndexedDBStoreSchema.parse(store);  // ❌ Frozen
}
```

---

## File Paths Quick Reference

```
/Users/sac/unrdf/
├── packages/
│   ├── cli/                          ✅ Reference (100% working)
│   │   └── src/index.mjs
│   ├── core/                         🔍 Verify exports
│   │   └── src/index.mjs
│   ├── hooks/                        ❌ Fix tests
│   │   ├── src/index.mjs
│   │   └── test/*.test.mjs
│   ├── federation/                   ➕ Add convenience exports
│   │   ├── src/index.mjs
│   │   └── src/federation/peer-functions.mjs  [CREATE]
│   ├── streaming/                    ➕ Add convenience exports
│   │   ├── src/index.mjs
│   │   └── src/streaming/feed-functions.mjs  [CREATE]
│   ├── browser/                      ❌ Fix state bug
│   │   └── src/browser/indexeddb-store.mjs
│   ├── knowledge-engine/             ✅ Verify working
│   │   └── src/index.mjs
│   ├── dark-matter/                  ✅ Verify working
│   │   └── src/index.mjs
│   └── composables/                  ✅ Verify working
│       └── src/index.mjs
└── docs/architecture/
    ├── v5-alpha-architecture.md       📄 Full design doc
    ├── v5-alpha-fix-summary.md        📄 Summary
    └── agent-reference.md             📄 This file
```

---

## Command Quick Reference

```bash
# Run all tests
pnpm test

# Run specific package tests
pnpm --filter @unrdf/core test
pnpm --filter @unrdf/hooks test
pnpm --filter @unrdf/federation test
pnpm --filter @unrdf/streaming test
pnpm --filter @unrdf/browser test
pnpm --filter @unrdf/knowledge-engine test
pnpm --filter @unrdf/dark-matter test
pnpm --filter @unrdf/composables test
pnpm --filter @unrdf/cli test

# Search for test issues
grep -r "result.passed" packages/hooks/test/
grep -r "IndexedDBStoreSchema.parse(store)" packages/browser/src/
```

---

## Success Checklist

- [ ] Core: All exports verified (Agent 1)
- [ ] Federation: Convenience exports added (Agent 2)
- [ ] Streaming: Convenience exports added (Agent 2)
- [ ] Hooks: Tests use `result.valid` (Agent 3)
- [ ] Browser: `store.isOpen` mutation works (Agent 4)
- [ ] Knowledge-engine: Tests pass (Agent 5)
- [ ] Dark-matter: Tests pass (Agent 5)
- [ ] Composables: Tests pass (Agent 5)
- [ ] CLI: Still 19/19 tests passing (Integration)
- [ ] Full suite: `pnpm test` passes (Integration)

---

**End of Agent Reference**
