# Phase 3B P1 Detailed Blockers Analysis

**Generated**: 2025-12-04
**Scope**: 8 failing examples from 21 total
**Pass Rate**: 61.9% (target: 67%)

---

## Quick Reference

```
PASS RATE PROGRESS
==================
Phase 1:  ████████████░░░░░░░░░░ 57.1% (12/21)
Phase 3A: ████████████░░░░░░░░░░ 57.1% (12/21)
Phase 3B: █████████████░░░░░░░░░ 61.9% (13/21) ← Current
Target:   ██████████████░░░░░░░░ 67.0% (14/21) ← Need 1 more
Goal:     ████████████████████████ 100% (21/21)

GAP ANALYSIS
============
Current to Target:  -1 example  (5.1% gap)
Current to Goal:    -8 examples (38.1% gap)
```

---

## P0 - Critical Blockers (Must Fix)

### 1. full-stack/server - Hook Trigger Format Error
**Impact**: 🔴 BLOCKER - Server cannot start
**Tests Failing**: Suite failure (34 tests)
**Severity**: Critical - Prevents all server functionality

**Error**:
```javascript
ZodError: Invalid enum value.
Expected 'before-add' | 'after-add' | 'before-query' | 'after-query' | 'before-remove' | 'after-remove'
Received 'before:add'
```

**Root Cause**: Hook trigger format mismatch
- Server code uses: `before:add` (colon separator)
- Zod schema expects: `before-add` (dash separator)

**Fix Strategy**:
```javascript
// In: playground/full-stack-example/apps/server/src/index.mjs
// BEFORE
hookManager.define({
  name: 'validate-person',
  trigger: 'before:add',  // ❌ Wrong format
  validate: (quad) => { ... }
});

// AFTER
hookManager.define({
  name: 'validate-person',
  trigger: 'before-add',  // ✅ Correct format
  validate: (quad) => { ... }
});
```

**Files to Edit**:
- `playground/full-stack-example/apps/server/src/index.mjs` (line 63)
- Search for all instances of `before:` and `after:` patterns
- Replace `:` with `-` in trigger values

**Estimated Time**: 30 minutes
**Testing**: `cd apps/server && pnpm test`
**Success Criteria**: Server starts and all 34 tests pass

---

### 2. full-stack/web - Complete Web App Failure
**Impact**: 🔴 BLOCKER - Web app cannot load or function
**Tests Failing**: 29/31 tests (93.5% failure rate)
**Severity**: Critical - Prevents all web functionality

**Error Categories**:

**2a. Server Connection Failure**
```javascript
TypeError: Failed to parse URL from /api/quads
```
- Cause: Relative URL `/api/quads` requires base URL
- Fix: Use `http://localhost:3000/api/quads` or configure base URL

**2b. Missing Vue Methods**
```javascript
TypeError: wrapper.vm.clearError is not a function
```
- Cause: Vue component missing error clearing method
- Fix: Add `clearError()` method to component

**2c. DOM Wrapper Issues**
```javascript
Error: Cannot call trigger on an empty DOMWrapper
Error: Cannot call setValue on an empty DOMWrapper
```
- Cause: Vue components not rendering in test environment
- Fix: Fix component mounting and selector queries

**Fix Strategy**:

1. **Fix Server URL** (apps/web/src/App.vue):
```javascript
// BEFORE
const response = await fetch('/api/quads');  // ❌ Relative URL fails

// AFTER
const baseUrl = import.meta.env.VITE_API_URL || 'http://localhost:3000';
const response = await fetch(`${baseUrl}/api/quads`);  // ✅ Full URL
```

2. **Add Missing Methods** (apps/web/src/App.vue):
```javascript
const clearError = () => {
  error.value = null;
};

// Export in setup return
return {
  // ...existing
  clearError,
};
```

3. **Fix DOM Selectors** (apps/web/test/integration.test.mjs):
```javascript
// BEFORE
const form = wrapper.find('form');  // ❌ Returns empty wrapper
await form.trigger('submit');

// AFTER
const form = wrapper.find('[data-testid="quad-form"]');  // ✅ Specific selector
expect(form.exists()).toBe(true);
await form.trigger('submit');
```

**Files to Edit**:
- `playground/full-stack-example/apps/web/src/App.vue`
- `playground/full-stack-example/apps/web/test/integration.test.mjs`
- `playground/full-stack-example/apps/web/.env.test` (add VITE_API_URL)

**Estimated Time**: 4-6 hours
**Testing**: `cd apps/web && pnpm test`
**Success Criteria**: 29+ tests passing, web app loads and functions

---

### 3. streaming/change-feeds - Missing Core Methods
**Impact**: 🔴 BLOCKER - Streaming functionality completely broken
**Tests Failing**: 9/9 tests (100% failure rate)
**Severity**: Critical - Core feature missing

**Error**:
```javascript
TypeError: feed.subscribe is not a function
TypeError: feed.getHistory is not a function
```

**Root Cause**: ChangeFeed class missing required methods
- `subscribe(callback)` - Event subscription
- `getHistory(options)` - Historical changes query

**Fix Strategy**:

```javascript
// In: packages/streaming/src/streaming/change-feed.mjs

export class ChangeFeed {
  #subscribers = new Map();
  #history = [];
  #nextId = 0;

  /**
   * Subscribe to change events
   * @param {Function} callback - Called on each change
   * @returns {Function} unsubscribe function
   */
  subscribe(callback) {
    const id = this.#nextId++;
    this.#subscribers.set(id, callback);

    return () => {
      this.#subscribers.delete(id);
    };
  }

  /**
   * Get change history
   * @param {Object} options - Filter options
   * @param {number} options.since - Timestamp to filter from
   * @returns {Array} Historical changes
   */
  getHistory(options = {}) {
    if (options.since) {
      return this.#history.filter(change => change.timestamp >= options.since);
    }
    return [...this.#history];
  }

  /**
   * Emit change to subscribers and store in history
   * @private
   */
  #emit(change) {
    const changeWithTimestamp = {
      ...change,
      timestamp: Date.now()
    };

    this.#history.push(changeWithTimestamp);

    for (const callback of this.#subscribers.values()) {
      callback(changeWithTimestamp);
    }
  }
}
```

**Files to Edit**:
- `packages/streaming/src/streaming/change-feed.mjs`
- Wire up `#emit()` calls in `addQuad()` and `removeQuad()` methods

**Estimated Time**: 2-3 hours
**Testing**: `cd packages/streaming/examples/change-feeds && pnpm test`
**Success Criteria**: All 9 tests passing

---

### 4. streaming/real-time-sync - Zod Validation Error
**Impact**: 🔴 BLOCKER - Real-time subscriptions completely broken
**Tests Failing**: 11/11 tests (100% failure rate)
**Severity**: Critical - Core feature unusable

**Error**:
```javascript
ZodError: [
  {
    "code": "invalid_type",
    "expected": "object",
    "received": "function",
    "path": [],
    "message": "Expected object, received function"
  }
]
```

**Root Cause**: Parameter order mismatch in `subscribe()` calls
- Test code: `subscribe(callback, filter)`
- Implementation: `subscribe(filter, callback)` or vice versa

**Fix Strategy**:

Check the implementation signature:
```javascript
// In: packages/streaming/src/streaming/subscription-manager.mjs
subscribe(callback, filter) {
  const validated = FilterSchema.parse(filter);  // ❌ Expects filter as 2nd param
  // ...
}
```

The issue is clear: implementation signature is `subscribe(callback, filter)` but tests call it with callback first. The Zod validation fails because it receives the callback function where it expects the filter object.

**Two Options**:

**Option A - Fix Implementation** (Recommended):
```javascript
// Change signature to match common pattern
subscribe(filter, callback) {
  const validated = FilterSchema.parse(filter);
  // ...
}
```

**Option B - Fix All Test Calls**:
```javascript
// Change all test calls to match implementation
// BEFORE
manager.subscribe((change) => { ... }, { subject: 'Alice' });

// AFTER
manager.subscribe({ subject: 'Alice' }, (change) => { ... });
```

**Recommendation**: Option A (fix implementation) because:
1. Callback-first is the standard pattern (Array.filter, EventEmitter, etc.)
2. Less code to change (1 file vs many test files)
3. More intuitive API

**Files to Edit**:
- `packages/streaming/src/streaming/subscription-manager.mjs` (swap parameter order)

**Estimated Time**: 1-2 hours
**Testing**: `cd packages/streaming/examples/real-time-sync && pnpm test`
**Success Criteria**: All 11 tests passing

---

## P1 - Major Issues (Should Fix)

### 5. browser/indexed-db - Persistence Partially Broken
**Impact**: 🟡 MAJOR - Browser persistence unreliable
**Tests Failing**: 2/16 tests (12.5% failure rate)
**Severity**: Major - Core feature degraded

**Error 1 - Missing Database Property**:
```javascript
expect(store.db).toBeDefined()  // ❌ Gets undefined
```
- Cause: IndexedDBStore not exposing db property
- Fix: Add `get db()` getter to class

**Error 2 - N3 Term Validation**:
```javascript
Error: Unexpected termType: undefined
```
- Cause: Quad serialization missing termType metadata
- Fix: Ensure quad objects have proper N3 term structure

**Fix Strategy**:

1. **Add DB Property** (packages/browser/src/browser/indexeddb-store.mjs):
```javascript
export class IndexedDBStore {
  #db = null;

  async open() {
    this.#db = await openDB(this.#dbName, ...);
    // ...
  }

  get db() {
    return this.#db;  // ✅ Expose database
  }
}
```

2. **Fix Quad Serialization** (same file):
```javascript
function addQuadToDB(store, quadData) {
  // Ensure proper N3 term structure
  const quad = {
    subject: ensureTerm(quadData.subject),
    predicate: ensureTerm(quadData.predicate),
    object: ensureTerm(quadData.object),
    graph: ensureTerm(quadData.graph || DataFactory.defaultGraph())
  };

  store.addQuad(quad);
}

function ensureTerm(term) {
  if (!term.termType) {
    // Convert plain objects to N3 terms
    return DataFactory.namedNode(term.value || term);
  }
  return term;
}
```

**Files to Edit**:
- `packages/browser/src/browser/indexeddb-store.mjs`

**Estimated Time**: 2-3 hours
**Testing**: `cd packages/browser/examples/indexed-db && pnpm test`
**Success Criteria**: All 16 tests passing

---

### 6. knowledge-engine/sparql-rules - Inference Not Working
**Impact**: 🟡 MAJOR - Knowledge inference broken
**Tests Failing**: 2/6 tests (33% failure rate)
**Severity**: Major - Core feature not functioning

**Error**:
```javascript
// Test expects derived facts, gets nothing
expect(personTypes.length).toBeGreaterThan(0)  // ❌ Gets 0
expect(dianaTypes).toHaveLength(1)             // ❌ Gets []
```

**Root Cause**: SPARQL CONSTRUCT rules not executing or not materializing results

**Failing Tests**:
1. "should handle transitive relationship chains"
   - Input: Alice -> Bob -> Charlie (parent relationships)
   - Expected: Transitive closure derived (Alice ancestor of Charlie)
   - Actual: No derivations

2. "should handle multiple derivation paths without duplicates"
   - Input: Diana has multiple paths to Person type
   - Expected: Single Person type derived
   - Actual: No derivations

**Fix Strategy**:

Debug the SPARQL rule execution:

1. **Check Rule Execution** (packages/knowledge-engine/examples/sparql-rules/src/index.mjs):
```javascript
export async function executeRule(store, rule) {
  // Add debug logging
  console.log('Executing rule:', rule.name);

  const results = await store.query(rule.construct);
  console.log('Rule results:', results.length, 'new facts');

  // Verify results are being added to store
  for (const quad of results) {
    console.log('Adding derived fact:', quad);
    store.add(quad);
  }

  return results;
}
```

2. **Check CONSTRUCT Query** (verify SPARQL syntax):
```sparql
# Transitive closure rule
CONSTRUCT {
  ?person a <http://example.org/Person> .
}
WHERE {
  ?person <http://xmlns.com/foaf/0.1/knows> ?someone .
  ?someone a <http://example.org/Person> .
}
```

3. **Verify Store State** (check if facts are persisting):
```javascript
console.log('Store size before:', store.size);
await executeRule(store, rule);
console.log('Store size after:', store.size);
```

**Files to Edit**:
- `packages/knowledge-engine/examples/sparql-rules/src/index.mjs`
- Add debug logging to identify where derivation breaks

**Estimated Time**: 3-4 hours (requires debugging)
**Testing**: `cd packages/knowledge-engine/examples/sparql-rules && pnpm test`
**Success Criteria**: All 6 tests passing, derivations working

---

## P2 - Minor Issues (Nice to Have)

### 7. browser/offline-support - Sync Error Handling
**Impact**: 🟢 MINOR - Edge case in error handling
**Tests Failing**: 1/18 tests (5.6% failure rate)
**Severity**: Minor - Sync failure recovery only

**Error**:
```javascript
TypeError: Cannot read properties of undefined (reading 'value')
```

**Root Cause**: Defensive check missing in sync error handler

**Fix Strategy**:
```javascript
// In: packages/browser/examples/offline-support/src/index.mjs
async syncQueuedOperations() {
  for (const op of this.#queue) {
    try {
      await this.#remoteStore[op.type](op.quad);
      synced++;
    } catch (err) {
      // BEFORE
      console.error(`Failed to sync ${op.type} operation:`, err);  // ❌ err might be undefined

      // AFTER
      console.error(`Failed to sync ${op.type} operation:`, err?.message || err);  // ✅ Safe access
      failed++;
    }
  }
}
```

**Files to Edit**:
- `packages/browser/examples/offline-support/src/index.mjs` (line 186)

**Estimated Time**: 30 minutes
**Testing**: `cd packages/browser/examples/offline-support && pnpm test`
**Success Criteria**: All 18 tests passing

---

### 8. composables/query-integration - Result Clearing Edge Case
**Impact**: 🟢 MINOR - Edge case in result clearing
**Tests Failing**: 1/24 tests (4.2% failure rate)
**Severity**: Minor - Empty result handling only

**Error**:
```javascript
// Test expects empty array after clearing
expect(query.results.value).toEqual([])
// But gets old results
// Received: [{ s: '...', p: '...', o: '...' }, ...]
```

**Root Cause**: Reactive ref not clearing properly

**Fix Strategy**:
```javascript
// In: packages/composables/examples/query-integration/src/index.mjs
export function useQuery(store, query) {
  const results = ref([]);
  const isLoading = ref(false);
  const error = ref(null);

  const execute = async () => {
    isLoading.value = true;
    error.value = null;
    results.value = [];  // ✅ Clear before query

    try {
      const newResults = await store.query(query);
      results.value = newResults;
    } catch (err) {
      error.value = err;
      results.value = [];  // ✅ Clear on error too
    } finally {
      isLoading.value = false;
    }
  };

  return {
    results,
    execute,
    // ...
  };
}
```

**Files to Edit**:
- `packages/composables/examples/query-integration/src/index.mjs`

**Estimated Time**: 1 hour
**Testing**: `cd packages/composables/examples/query-integration && pnpm test`
**Success Criteria**: All 24 tests passing

---

## Summary Table

| # | Example | Priority | Tests Failing | Fix Time | Complexity |
|---|---------|----------|---------------|----------|------------|
| 1 | full-stack/server | P0 | Suite | 30 min | Low |
| 2 | full-stack/web | P0 | 29/31 | 4-6 hrs | High |
| 3 | streaming/change-feeds | P0 | 9/9 | 2-3 hrs | Medium |
| 4 | streaming/real-time-sync | P0 | 11/11 | 1-2 hrs | Low |
| 5 | browser/indexed-db | P1 | 2/16 | 2-3 hrs | Medium |
| 6 | knowledge-engine/sparql-rules | P1 | 2/6 | 3-4 hrs | High |
| 7 | browser/offline-support | P2 | 1/18 | 30 min | Low |
| 8 | composables/query-integration | P2 | 1/24 | 1 hr | Low |

**Total Estimated Time**: 14.5-20.5 hours (2-3 work days)

---

## Recommended Fix Order

### Phase 1: Quick Wins (1.5 hours → 67% pass rate)
1. Fix server hook trigger (30 min)
2. Fix offline-support error handling (30 min)
3. Fix query-integration result clearing (30 min)

**Result**: 16/21 = 76.2% pass rate ✅ Exceeds 67% target

### Phase 2: Critical Features (8-11 hours → 85% pass rate)
4. Fix change-feeds methods (2-3 hrs)
5. Fix real-time-sync validation (1-2 hrs)
6. Fix web app integration (4-6 hrs)

**Result**: 19/21 = 90.5% pass rate

### Phase 3: Polish (5-7 hours → 100% pass rate)
7. Fix indexed-db persistence (2-3 hrs)
8. Fix sparql-rules inference (3-4 hrs)

**Result**: 21/21 = 100% pass rate ✅ Zero defects

---

## Decision Matrix

| Option | Time | Pass Rate | User Impact | Recommendation |
|--------|------|-----------|-------------|----------------|
| **Phase 1 Only** | 1.5 hrs | 76.2% | Medium | ⭐ Good for quick release |
| **Phase 1+2** | 9.5-12.5 hrs | 90.5% | High | ⭐⭐ Recommended |
| **All Phases** | 14.5-20.5 hrs | 100% | Highest | ⭐⭐⭐ Professional release |

**Recommendation**: Execute **Phase 1+2** to reach 90.5% with all user-facing features working.
