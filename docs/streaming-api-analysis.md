# Streaming API Mismatch Analysis

## Executive Summary

**CRITICAL API MISMATCH**: The `@unrdf/streaming` package has a fundamental disconnect between:
1. **Implementation**: Low-level EventTarget-based primitives that don't integrate with N3.Store
2. **Example Usage**: High-level APIs that expect automatic Store change detection

**Impact**: 2 example packages (~20 tests) cannot run because the expected API doesn't exist.

---

## 1. Current Implementation

### 1.1 `createChangeFeed()` - Current Signature

**Location**: `packages/streaming/src/streaming/change-feed.mjs`

**Current API**:
```javascript
const feed = createChangeFeed();
// Returns: {
//   emitChange(change),
//   addEventListener(type, callback, options),
//   removeEventListener(type, callback, options),
//   getChanges(),
//   clearChanges(),
//   replay(callback)
// }
```

**What it does**:
- Creates a bare EventTarget wrapper
- Requires MANUAL `emitChange()` calls
- Does NOT monitor any Store
- Does NOT auto-detect changes

**Missing from current implementation**:
- ❌ No Store parameter
- ❌ No automatic change detection
- ❌ No `subscribe()` method (uses `addEventListener` instead)
- ❌ No `getHistory()` method (uses `getChanges` instead)
- ❌ No Store monitoring/hooking

### 1.2 `createSubscriptionManager()` - Current Signature

**Location**: `packages/streaming/src/streaming/subscription-manager.mjs`

**Current API**:
```javascript
const manager = createSubscriptionManager(feed);
// Parameters: (feed) - Takes a change feed as input
// Returns: {
//   subscribe(callback, filter),
//   unsubscribe(subscriptionId),
//   listSubscriptions(),
//   clearSubscriptions()
// }
```

**What it does**:
- Wraps an existing change feed
- Adds filtering by subject/predicate/object
- Returns numeric subscription IDs

**Issues**:
- ❌ Parameter is `feed`, not `store`
- ❌ Requires pre-existing feed with manual `emitChange` calls
- ❌ Does NOT monitor Store changes

---

## 2. Expected API (From Examples)

### 2.1 `createChangeFeed()` - Expected Signature

**Source**:
- `packages/streaming/examples/change-feeds/src/index.mjs` (lines 19, 59, 108, 148)
- `packages/streaming/examples/real-time-sync/src/index.mjs` (lines 164, 165, 166)

**Expected API**:
```javascript
const store = new Store();
const feed = createChangeFeed(store);  // ← Takes Store as parameter!

// Returns: {
//   subscribe(callback),           // ← Not addEventListener!
//   getHistory(options),           // ← Not getChanges!
//   replay(callback),              // ✓ Exists
//   on(event, callback),           // ← Missing
//   off(event, callback)           // ← Missing
// }
```

**Expected behavior**:
- ✅ Accepts N3.Store as parameter
- ✅ Automatically detects Store.addQuad() calls
- ✅ Automatically detects Store.removeQuad() calls
- ✅ Emits change events WITHOUT manual calls
- ✅ `subscribe()` returns unsubscribe function
- ✅ `getHistory()` supports time-based queries with `{ since: timestamp }`

**Example usage**:
```javascript
// From change-feeds/src/index.mjs:19
const store = new Store();
const feed = createChangeFeed(store);  // ← Auto-monitors store!

const unsubscribe = feed.subscribe((change) => {
  console.log(`Change detected: ${change.type}`);
});

store.addQuad(quad(alice, name, literal('Alice')));
// ↑ Should automatically trigger feed.subscribe() callback!

const history = feed.getHistory();  // Get all changes
const recent = feed.getHistory({ since: timestamp });  // Time-based query
```

### 2.2 `createSubscriptionManager()` - Expected Signature

**Source**: `packages/streaming/examples/real-time-sync/src/index.mjs` (line 19)

**Expected API**:
```javascript
const store = new Store();
const manager = createSubscriptionManager(store);  // ← Takes Store, not feed!

// Returns: {
//   subscribe(filter, callback),   // ← Parameter order SWAPPED!
//   unsubscribe(subscriptionId)    // ✓ Exists
// }
```

**Expected behavior**:
- ✅ Accepts N3.Store as parameter (not feed)
- ✅ Automatically monitors Store changes
- ✅ `subscribe(filter, callback)` - filter first, callback second
- ✅ Callback receives array of quads matching filter
- ✅ Returns string subscription ID (not number)

**Example usage**:
```javascript
// From real-time-sync/src/index.mjs:19-30
const store = new Store();
const manager = createSubscriptionManager(store);  // ← Direct Store!

const subscriptionId = manager.subscribe({
  subject: alice,
  predicate: name
}, (quads) => {  // ← Callback is SECOND parameter
  console.log(`Name updated for Alice: ${quads[0].object.value}`);
});

store.addQuad(quad(alice, name, literal('Alice')));
// ↑ Should automatically trigger callback with matching quads!
```

---

## 3. Root Cause Analysis

### 3.1 Missing Store Integration Layer

**The Problem**: There is NO code that hooks into N3.Store to detect changes.

**Current architecture**:
```
N3.Store (manual addQuad/removeQuad)
    ↓
[MISSING INTEGRATION LAYER]
    ↓
createChangeFeed() (expects manual emitChange calls)
    ↓
createSubscriptionManager()
```

**Expected architecture**:
```
N3.Store (addQuad/removeQuad)
    ↓
StoreMonitor/Proxy (auto-detects changes)  ← MISSING!
    ↓
createChangeFeed() (auto-emits changes)
    ↓
createSubscriptionManager()
```

### 3.2 API Signature Mismatch

| Function | Current | Expected | Mismatch |
|----------|---------|----------|----------|
| `createChangeFeed()` | `createChangeFeed()` | `createChangeFeed(store)` | ❌ Missing store parameter |
| `feed.subscribe()` | Does not exist | `feed.subscribe(callback)` | ❌ Missing method |
| `feed.getHistory()` | `feed.getChanges()` | `feed.getHistory(options)` | ❌ Wrong method name |
| `createSubscriptionManager()` | `createSubscriptionManager(feed)` | `createSubscriptionManager(store)` | ❌ Wrong parameter type |
| `manager.subscribe()` | `subscribe(callback, filter)` | `subscribe(filter, callback)` | ❌ Swapped parameters |

### 3.3 Test Failures

**change-feeds example** (line 19):
```javascript
const feed = createChangeFeed(store);  // ← TypeError: createChangeFeed expects 0 args, got 1
```

**real-time-sync example** (line 19):
```javascript
const manager = createSubscriptionManager(store);  // ← Manager expects feed, got store
```

**Expected test count**: ~20 tests across 2 examples
**Actual passing tests**: 0 (all fail on API mismatch)

---

## 4. Breaking Changes Analysis

### 4.1 High-Level API Changes Required

**createChangeFeed()**:
```javascript
// BEFORE (current implementation)
const feed = createChangeFeed();
feed.emitChange({ type: 'add', quad });

// AFTER (expected by examples)
const feed = createChangeFeed(store);  // Store monitors changes automatically
store.addQuad(quad);  // Triggers feed automatically
```

**createSubscriptionManager()**:
```javascript
// BEFORE (current implementation)
const feed = createChangeFeed();
const manager = createSubscriptionManager(feed);
manager.subscribe((change) => {}, { subject: alice });

// AFTER (expected by examples)
const manager = createSubscriptionManager(store);
manager.subscribe({ subject: alice }, (quads) => {});  // Swapped params
```

### 4.2 New Components Required

**1. Store Monitoring Proxy** (NEW FILE NEEDED)
```javascript
// packages/streaming/src/streaming/store-monitor.mjs
export function createStoreMonitor(store) {
  return new Proxy(store, {
    get(target, prop) {
      if (prop === 'addQuad') {
        return (quad) => {
          const result = target.addQuad(quad);
          emitChange({ type: 'add', quad, timestamp: Date.now() });
          return result;
        };
      }
      if (prop === 'removeQuad') {
        return (quad) => {
          const result = target.removeQuad(quad);
          emitChange({ type: 'remove', quad, timestamp: Date.now() });
          return result;
        };
      }
      return target[prop];
    }
  });
}
```

**2. Enhanced createChangeFeed()** (BREAKING CHANGE)
```javascript
export function createChangeFeed(store, options = {}) {
  // Create base feed
  const feed = createBaseFeed();

  // Monitor store if provided
  if (store) {
    const monitor = createStoreMonitor(store);
    monitor.on('change', (change) => feed.emitChange(change));
  }

  return {
    // New API methods
    subscribe(callback) {
      feed.addEventListener('change', (event) => callback(event.detail));
      return () => feed.removeEventListener('change', callback);
    },
    getHistory(options = {}) {
      const changes = feed.getChanges();
      if (options.since) {
        return changes.filter(c => c.timestamp >= options.since);
      }
      return changes;
    },
    replay: feed.replay,
    on: feed.addEventListener,
    off: feed.removeEventListener
  };
}
```

**3. Fixed createSubscriptionManager()** (BREAKING CHANGE)
```javascript
export function createSubscriptionManager(storeOrFeed) {
  // Auto-create feed if given a store
  const feed = storeOrFeed.addQuad
    ? createChangeFeed(storeOrFeed)
    : storeOrFeed;

  return {
    subscribe(filter, callback) {  // ← Swapped parameter order!
      // Subscribe and return string ID
      const id = String(generateId());

      feed.subscribe((change) => {
        if (matchesFilter(change.quad, filter)) {
          // Get all matching quads from store
          const quads = getMatchingQuads(store, filter);
          callback(quads);  // ← Callback receives quads, not change
        }
      });

      return id;
    },
    unsubscribe(subscriptionId) {
      // Implementation
    }
  };
}
```

### 4.3 Backward Compatibility Impact

**Current users (if any)**:
- ❌ Will break immediately on upgrade
- ❌ Must migrate to new API signatures
- ❌ Must change parameter order in subscribe()
- ❌ Must change method names (getChanges → getHistory)

**Migration path**:
1. Deprecate old API in v1.x
2. Add new API alongside old API
3. Remove old API in v2.0
4. Update all examples and tests

**Estimated migration effort**:
- Simple projects: 1-2 hours
- Complex projects: 4-8 hours
- No automatic migration tool possible

---

## 5. Implementation Roadmap

### Phase 1: Core Store Integration (REQUIRED)
- [ ] Create `store-monitor.mjs` with Proxy-based change detection
- [ ] Add Store parameter to `createChangeFeed(store, options)`
- [ ] Add automatic change emission on Store.addQuad/removeQuad
- [ ] Add `subscribe()` method to feed API
- [ ] Rename `getChanges()` to `getHistory()`
- [ ] Add time-based filtering to `getHistory({ since })`

### Phase 2: Subscription Manager Fix (REQUIRED)
- [ ] Change `createSubscriptionManager(feed)` to accept Store
- [ ] Swap parameter order: `subscribe(callback, filter)` → `subscribe(filter, callback)`
- [ ] Change callback signature: receives `change` → receives `quads[]`
- [ ] Change subscription ID: number → string
- [ ] Add Store.getQuads() calls to get matching quads

### Phase 3: Testing & Migration (REQUIRED)
- [ ] Fix all unit tests in `packages/streaming/test/streaming.test.mjs`
- [ ] Fix change-feeds example tests (~10 tests)
- [ ] Fix real-time-sync example tests (~10 tests)
- [ ] Add migration guide to README
- [ ] Add deprecation warnings to old API

### Phase 4: Documentation (REQUIRED)
- [ ] Update API documentation with new signatures
- [ ] Add Store integration examples
- [ ] Document breaking changes
- [ ] Add migration examples

---

## 6. Detailed Issue List

### 6.1 createChangeFeed Issues

| Issue | Current | Expected | Priority |
|-------|---------|----------|----------|
| Store parameter | None | `createChangeFeed(store)` | CRITICAL |
| subscribe() method | Missing | `subscribe(callback) → unsubscribe` | CRITICAL |
| getHistory() method | Named `getChanges()` | `getHistory(options)` | HIGH |
| Time-based filtering | Missing | `getHistory({ since: timestamp })` | HIGH |
| Auto change detection | Missing | Proxy Store.addQuad/removeQuad | CRITICAL |
| on/off methods | Missing | `on(event, callback)`, `off(...)` | MEDIUM |

### 6.2 createSubscriptionManager Issues

| Issue | Current | Expected | Priority |
|-------|---------|----------|----------|
| Parameter type | `feed` | `store` | CRITICAL |
| subscribe() params | `(callback, filter)` | `(filter, callback)` | CRITICAL |
| Callback signature | Receives `change` | Receives `quads[]` | HIGH |
| Subscription ID type | `number` | `string` | MEDIUM |
| Store queries | Missing | Call Store.getQuads() | HIGH |

### 6.3 Store Integration Issues

| Issue | Status | Impact |
|-------|--------|--------|
| No Store monitoring | MISSING | CRITICAL - Examples cannot work |
| No Proxy wrapper | MISSING | CRITICAL - No change detection |
| No auto-emit | MISSING | CRITICAL - Manual emitChange required |
| No Store.addQuad hook | MISSING | CRITICAL - Changes not detected |
| No Store.removeQuad hook | MISSING | CRITICAL - Deletions not detected |

---

## 7. Test Failure Summary

### 7.1 Expected Test Failures

**change-feeds example** (`examples/change-feeds/test/example.test.mjs`):
```
❌ Line 19: createChangeFeed(store) - TypeError: expected 0 arguments, got 1
❌ Line 22: feed.subscribe() - TypeError: feed.subscribe is not a function
❌ Line 51: feed.getHistory() - TypeError: feed.getHistory is not a function
❌ Line 74: feed.getHistory() - TypeError: feed.getHistory is not a function
❌ Line 118: feed.getHistory({ since }) - TypeError: feed.getHistory is not a function
```

**real-time-sync example** (`examples/real-time-sync/test/example.test.mjs`):
```
❌ Line 19: createSubscriptionManager(store) - Manager expects feed, got store
❌ Line 25: manager.subscribe({ filter }, callback) - Wrong parameter order
❌ Line 20: subscription ID type - Expected string, got number
❌ All tests: No automatic change detection - Callbacks never fire
```

### 7.2 Test Count

- **change-feeds**: 9 tests (all failing)
- **real-time-sync**: 11 tests (all failing)
- **Total blocked tests**: 20 tests

---

## 8. Recommendations

### 8.1 Immediate Actions (CRITICAL)

1. **Create Store monitoring layer**
   - Implement Proxy-based Store wrapper
   - Hook addQuad/removeQuad methods
   - Auto-emit change events

2. **Fix createChangeFeed() signature**
   - Add Store parameter
   - Add subscribe() method
   - Rename getChanges() → getHistory()
   - Add time-based filtering

3. **Fix createSubscriptionManager() signature**
   - Accept Store instead of feed
   - Swap parameter order in subscribe()
   - Change callback to receive quads[]
   - Return string subscription IDs

### 8.2 Migration Strategy

**Option A: Breaking change in v2.0** (RECOMMENDED)
- Mark v1.x as deprecated
- Release v2.0 with correct API
- Provide migration guide
- Update all examples

**Option B: Dual API in v1.1**
- Keep old API (deprecated)
- Add new API alongside
- Remove old API in v2.0
- Confusing for users

**Option C: Fix-in-place**
- Fix API immediately
- Document breaking changes
- No backward compatibility
- Fastest to implement

### 8.3 Implementation Estimate

- **Store monitoring layer**: 4-6 hours
- **createChangeFeed() fixes**: 3-4 hours
- **createSubscriptionManager() fixes**: 2-3 hours
- **Test updates**: 2-3 hours
- **Documentation**: 2-3 hours
- **Total**: 13-19 hours

---

## 9. Conclusion

**Status**: ❌ **BLOCKING - Cannot ship examples**

**Root cause**: Complete disconnect between low-level EventTarget primitives and high-level Store integration expected by examples.

**Solution**: Implement Store monitoring layer + API fixes to match example expectations.

**Priority**: CRITICAL - Required before examples can run.

**Next steps**:
1. Implement store-monitor.mjs
2. Fix createChangeFeed() API
3. Fix createSubscriptionManager() API
4. Update all tests
5. Verify examples pass

---

## Appendix: Code Samples

### A.1 Current vs Expected - createChangeFeed

**Current (BROKEN)**:
```javascript
// Implementation
const feed = createChangeFeed();
feed.emitChange({ type: 'add', quad });

// Tests expect
const feed = createChangeFeed(store);  // ← TypeError!
const unsub = feed.subscribe(() => {});  // ← TypeError!
const history = feed.getHistory();  // ← TypeError!
```

**Expected (WORKING)**:
```javascript
// Implementation
const feed = createChangeFeed(store);  // Auto-monitors store
store.addQuad(quad);  // Triggers feed automatically

// Tests use
const unsub = feed.subscribe(() => {});  // Works
const history = feed.getHistory();  // Works
const recent = feed.getHistory({ since: Date.now() });  // Works
```

### A.2 Current vs Expected - createSubscriptionManager

**Current (BROKEN)**:
```javascript
// Implementation
const feed = createChangeFeed();
const manager = createSubscriptionManager(feed);
const id = manager.subscribe((change) => {}, filter);  // callback first!

// Tests expect
const manager = createSubscriptionManager(store);  // ← Wrong param type!
const id = manager.subscribe(filter, (quads) => {});  // ← Swapped params!
```

**Expected (WORKING)**:
```javascript
// Implementation
const manager = createSubscriptionManager(store);
const id = manager.subscribe(
  { subject: alice },  // Filter first
  (quads) => {}  // Callback second
);

// Auto-triggers on store changes
store.addQuad(quad);  // Callback fires automatically
```

---

**End of Analysis**
