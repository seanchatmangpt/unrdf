# Browser Package Removal Migration Guide

**Date:** 2025-12-05
**Affected Package:** @unrdf/browser (removed)
**Migration Target:** unrdf-react (use-offline-store)

---

## What Changed?

The `@unrdf/browser` package has been **completely removed** from the UNRDF monorepo.

---

## Why Was It Removed?

The browser package was non-functional and duplicated:

1. **Non-functional** - Package couldn't run tests, had missing dependencies, no build artifacts
2. **Duplicated** - React package already had superior implementation (`useOfflineStore`)
3. **Zero users** - Only 1 broken import found (never worked in published form)
4. **40% dead code** - Half the package was orphaned from incomplete refactoring
5. **Broken integration** - Import paths were broken, would fail in published package

**See:** `packages/browser/AUDIT-REPORT.md` and `packages/browser/REMOVAL-PLAN.md` (in git history) for complete analysis.

---

## Who Is Affected?

### ❌ If you were using `useIndexedDBStore` from React

**This hook never worked.** The import was broken and would have failed immediately.

```javascript
// THIS NEVER WORKED (broken import):
import { useIndexedDBStore } from 'unrdf-react/hooks';
const { store, loading } = useIndexedDBStore();
```

**Error you would have seen:**
```
Error: Cannot find module '../../../browser/indexeddb-store.mjs'
```

### ❌ If you imported from `@unrdf/browser` directly

**This package was never published** and existed only in the monorepo workspace.

---

## Migration Path

### Use React's `useOfflineStore` (Recommended)

React already has a **superior** offline-first IndexedDB implementation:

```javascript
// AFTER (working, production-ready):
import { useOfflineStore } from 'unrdf-react';

const {
  quads,           // Current quads in store
  insert,          // Insert quads
  delete: del,     // Delete quads
  sync,            // Manual sync
  pendingCount,    // Number of pending sync operations
  isOnline,        // Online/offline status
  lastSynced,      // Last sync timestamp
  isSyncing,       // Sync in progress
  error            // Error state
} = useOfflineStore({
  dbName: 'myapp-rdf',
  storeName: 'quads',
  autoSync: true,              // Auto-sync when online
  syncInterval: 30000,         // Sync every 30s
  onSync: () => {              // Sync complete callback
    console.log('Synced!');
  },
  onConflict: async (item, serverData) => {
    // Conflict resolution
    return 'server'; // or 'local'
  }
});

// Insert quads (works offline)
await insert([
  { subject, predicate, object, graph }
]);

// Delete quads
await del([quad]);

// Manual sync
await sync();
```

---

## Feature Comparison

| Feature | @unrdf/browser | useOfflineStore |
|---------|----------------|-----------------|
| **Status** | ❌ Broken | ✅ Working |
| **IndexedDB Persistence** | Basic | ✅ Advanced |
| **Offline Support** | None | ✅ Offline-first |
| **Sync Queue** | None | ✅ Built-in |
| **Auto Sync** | None | ✅ Configurable |
| **Conflict Resolution** | None | ✅ Callbacks |
| **Online/Offline Detection** | None | ✅ Built-in |
| **Retry Logic** | None | ✅ Exponential backoff |
| **Error Handling** | Basic | ✅ Comprehensive |
| **Testing** | ❌ Can't run | ✅ Tested |
| **Dependencies** | ❌ Missing | ✅ Complete |

**Conclusion:** `useOfflineStore` is superior in every aspect.

---

## API Mapping

### Basic Operations

```javascript
// @unrdf/browser (never worked):
const store = new IndexedDBStore({ name: 'mydb' });
await store.open();
await store.add(quad);
const quads = await store.match(null, predicate, null);
await store.clear();

// useOfflineStore (working):
const { quads, insert, delete: del, sync } = useOfflineStore({
  dbName: 'mydb'
});
await insert([quad]);
// Quads available immediately in state
await del([quad]);
```

### Storage Quota

```javascript
// @unrdf/browser (unused utilities):
import { checkStorageQuota, formatStorageSize } from '@unrdf/browser';
const quota = await checkStorageQuota();
console.log(formatStorageSize(quota.usage));

// useOfflineStore (implement yourself if needed):
const getQuota = async () => {
  if (navigator.storage?.estimate) {
    const estimate = await navigator.storage.estimate();
    return {
      usage: estimate.usage,
      quota: estimate.quota,
      percentUsed: (estimate.usage / estimate.quota) * 100
    };
  }
  return null;
};

const formatSize = (bytes) => {
  const units = ['B', 'KB', 'MB', 'GB'];
  let size = bytes;
  let i = 0;
  while (size >= 1024 && i < units.length - 1) {
    size /= 1024;
    i++;
  }
  return `${size.toFixed(1)} ${units[i]}`;
};
```

### Service Workers

```javascript
// @unrdf/browser (unused):
import { registerServiceWorker, initOfflineSupport } from '@unrdf/browser';
await registerServiceWorker('/sw.js');

// useOfflineStore (built-in offline support):
const { isOnline, sync } = useOfflineStore({
  autoSync: true  // Automatically syncs when back online
});

// Or implement service worker registration yourself:
if ('serviceWorker' in navigator) {
  navigator.serviceWorker.register('/sw.js');
}
```

---

## Non-React Users

If you're not using React, you can extract the core IndexedDB logic:

### Option 1: Copy Implementation

The `useOfflineStore` implementation is pure JavaScript with React hooks wrapper. You can:

1. Copy `packages/react/src/composition/use-offline-store.mjs` (in git history)
2. Remove React-specific hooks (`useState`, `useEffect`, `useCallback`)
3. Convert to class-based or functional implementation

### Option 2: Use IndexedDB Directly

```javascript
// Simple IndexedDB quad store (no dependencies):
class QuadStore {
  constructor(dbName = 'unrdf', storeName = 'quads') {
    this.dbName = dbName;
    this.storeName = storeName;
    this.db = null;
  }

  async open() {
    return new Promise((resolve, reject) => {
      const request = indexedDB.open(this.dbName, 1);

      request.onerror = () => reject(request.error);
      request.onsuccess = () => {
        this.db = request.result;
        resolve(this);
      };

      request.onupgradeneeded = (event) => {
        const db = event.target.result;
        if (!db.objectStoreNames.contains(this.storeName)) {
          const store = db.createObjectStore(this.storeName, {
            keyPath: 'id',
            autoIncrement: true
          });
          store.createIndex('subject', 'subject', { unique: false });
          store.createIndex('predicate', 'predicate', { unique: false });
          store.createIndex('object', 'object', { unique: false });
        }
      };
    });
  }

  async add(quad) {
    const tx = this.db.transaction([this.storeName], 'readwrite');
    const store = tx.objectStore(this.storeName);
    const data = {
      subject: quad.subject.value,
      predicate: quad.predicate.value,
      object: quad.object.value,
      graph: quad.graph?.value || ''
    };
    return new Promise((resolve, reject) => {
      const request = store.add(data);
      request.onsuccess = () => resolve();
      request.onerror = () => reject(request.error);
    });
  }

  async getAll() {
    const tx = this.db.transaction([this.storeName], 'readonly');
    const store = tx.objectStore(this.storeName);
    return new Promise((resolve, reject) => {
      const request = store.getAll();
      request.onsuccess = () => resolve(request.result);
      request.onerror = () => reject(request.error);
    });
  }
}

// Usage:
const store = new QuadStore('myapp');
await store.open();
await store.add(quad);
const quads = await store.getAll();
```

---

## Breaking Changes

### Package Removed
- `@unrdf/browser` no longer exists
- Cannot be installed or imported

### React Hook Removed
- `useIndexedDBStore` removed from `unrdf-react/hooks`
- Never worked, so no migration needed for actual users

### Dependencies Updated
- `@unrdf/composables` no longer depends on `@unrdf/browser`
- No impact (dependency was never used)

### Validation Removed
- `validation/browser-validation.mjs` deleted
- No user impact (internal tooling only)

---

## Timeline

- **Removed:** 2025-12-05
- **Reason:** Non-functional package with duplicate functionality
- **Alternative:** `useOfflineStore` from `unrdf-react` (superior implementation)

---

## Questions?

**Q: I was using `@unrdf/browser` in production. What should I do?**
A: This is impossible. The package never worked (broken imports, missing dependencies). Check your actual imports - you're likely using something else.

**Q: I need the exact same API as `IndexedDBStore`.**
A: That API never worked. Use `useOfflineStore` which has a better API and actually works. See API mapping above.

**Q: Can I still access the old code?**
A: Yes, it's in git history:
```bash
git checkout HEAD~1 packages/browser/
```

**Q: Will this break my application?**
A: No. The package never worked and had zero actual users. If your app runs now, it will continue to run.

**Q: Why wasn't this package fixed instead of removed?**
A: React's `useOfflineStore` already provides superior functionality. Fixing would duplicate effort for inferior result.

---

## Additional Resources

- **Audit Report:** See git history for `packages/browser/AUDIT-REPORT.md`
- **Removal Plan:** See git history for `packages/browser/REMOVAL-PLAN.md`
- **React Hook Docs:** `packages/react/src/composition/use-offline-store.mjs` (extensive JSDoc)

---

## Summary

✅ **Action Required:** Use `useOfflineStore` from `unrdf-react` instead
✅ **Breaking Changes:** Only affects broken code that never worked
✅ **Migration Complexity:** Low (most users unaffected)
✅ **Benefits:** Cleaner codebase, better functionality, actually works

**Migration complete when:**
- All imports from `@unrdf/browser` removed
- All uses of `useIndexedDBStore` replaced with `useOfflineStore`
- Tests pass
