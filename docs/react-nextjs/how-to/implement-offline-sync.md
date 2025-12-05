# How-to: Implement Offline Sync

**Goal:** Build offline-capable applications that sync when reconnected.

**Time:** 20-25 minutes

---

## Enable Persistent Storage

Use the `useStorage` hook to persist your knowledge graph to IndexedDB:

```jsx
import { useKnowledgeEngine, useStorage } from 'unrdf-react';
import { useEffect } from 'react';

function OfflineApp() {
  const { engine, ready } = useKnowledgeEngine();

  // Enable automatic persistence
  const { save, load, isStored } = useStorage({
    storeName: 'my-app-knowledge-graph',
    autoSave: true,
    saveInterval: 5000, // Save every 5 seconds
  });

  // Load persisted data on mount
  useEffect(() => {
    if (ready) {
      load();
    }
  }, [ready, load]);

  return <div>Offline storage enabled!</div>;
}
```

**Result:** Knowledge graph automatically persists to IndexedDB.

---

## Detect Online/Offline Status

Track network connectivity:

```jsx
import { useOnlineStatus } from 'unrdf-react';

function NetworkStatus() {
  const { isOnline, wasOffline } = useOnlineStatus();

  return (
    <div className={`status ${isOnline ? 'online' : 'offline'}`}>
      {isOnline ? (
        <>
          ✅ Online
          {wasOffline && <span className="reconnected">Reconnected!</span>}
        </>
      ) : (
        <>⚠️ Offline - Changes will sync when reconnected</>
      )}
    </div>
  );
}
```

**Result:** Shows current connectivity status with reconnection notifications.

---

## Queue Changes While Offline

Store mutations in a queue when offline:

```jsx
import { useKnowledgeEngine, useOnlineStatus, useOfflineQueue } from 'unrdf-react';
import { useState } from 'react';

function OfflineTodoApp() {
  const { engine, ready } = useKnowledgeEngine();
  const { isOnline } = useOnlineStatus();
  const [newTodo, setNewTodo] = useState('');

  // Queue for offline operations
  const { enqueue, pendingCount, sync } = useOfflineQueue({
    autoSync: true, // Automatically sync when coming back online
    onSyncComplete: () => {
      console.log('All offline changes synced!');
    },
  });

  const addTodo = async () => {
    if (!ready || !newTodo.trim()) return;

    const todoId = `http://example.org/todos/${Date.now()}`;
    const triples = [
      {
        subject: todoId,
        predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        object: 'http://example.org/Todo'
      },
      {
        subject: todoId,
        predicate: 'http://example.org/text',
        object: newTodo
      }
    ];

    if (isOnline) {
      // Direct execution when online
      await engine.addTriples(triples);
    } else {
      // Queue for later when offline
      enqueue({
        type: 'add',
        triples,
      });
    }

    setNewTodo('');
  };

  return (
    <div>
      <NetworkStatusBanner isOnline={isOnline} pendingCount={pendingCount} />

      <input
        value={newTodo}
        onChange={(e) => setNewTodo(e.target.value)}
        placeholder="Add todo (works offline!)"
      />
      <button onClick={addTodo}>Add</button>

      {!isOnline && pendingCount > 0 && (
        <div className="pending-notice">
          {pendingCount} changes waiting to sync
        </div>
      )}
    </div>
  );
}

function NetworkStatusBanner({ isOnline, pendingCount }) {
  return (
    <div className={`banner ${isOnline ? 'online' : 'offline'}`}>
      {isOnline ? (
        <>
          ✅ Online {pendingCount > 0 && `- Syncing ${pendingCount} changes...`}
        </>
      ) : (
        <>⚠️ Working Offline</>
      )}
    </div>
  );
}
```

**Result:** Todos can be added offline and automatically sync when reconnected.

---

## Handle Sync Conflicts

Resolve conflicts when offline changes conflict with server state:

```jsx
import { useOfflineQueue, useConflictResolver } from 'unrdf-react';

function ConflictAwareApp() {
  const { sync, conflicts } = useOfflineQueue();

  const { resolveConflict } = useConflictResolver({
    strategy: 'last-write-wins', // Options: last-write-wins, first-write-wins, manual

    onConflict: (conflict) => {
      console.log('Conflict detected:', conflict);
      // conflict = { local: Triple[], remote: Triple[], subject: string }
    },

    onResolved: (resolution) => {
      console.log('Conflict resolved:', resolution);
    },
  });

  // Manual conflict resolution UI
  return (
    <div>
      {conflicts?.map((conflict, idx) => (
        <div key={idx} className="conflict-card">
          <h3>Conflict on {conflict.subject}</h3>

          <div className="options">
            <div>
              <h4>Your Changes (Local)</h4>
              <pre>{JSON.stringify(conflict.local, null, 2)}</pre>
              <button onClick={() => resolveConflict(conflict.id, 'local')}>
                Keep Local
              </button>
            </div>

            <div>
              <h4>Server Changes (Remote)</h4>
              <pre>{JSON.stringify(conflict.remote, null, 2)}</pre>
              <button onClick={() => resolveConflict(conflict.id, 'remote')}>
                Keep Remote
              </button>
            </div>
          </div>

          <button onClick={() => resolveConflict(conflict.id, 'merge')}>
            Merge Both
          </button>
        </div>
      ))}
    </div>
  );
}
```

**Result:** User can manually resolve conflicts or use automatic strategies.

---

## Progressive Sync

Sync large graphs incrementally to avoid blocking:

```jsx
import { useProgressiveSync } from 'unrdf-react';

function ProgressiveSyncApp() {
  const {
    sync,
    progress,
    issyncing,
    error
  } = useProgressiveSync({
    batchSize: 100, // Sync 100 triples at a time
    delayBetweenBatches: 100, // 100ms delay between batches

    onBatchComplete: (batchNumber, totalBatches) => {
      console.log(`Synced batch ${batchNumber}/${totalBatches}`);
    },
  });

  return (
    <div>
      <button onClick={sync} disabled={isSyncing}>
        {isSyncing ? 'Syncing...' : 'Sync Now'}
      </button>

      {isSyncing && (
        <div className="progress-bar">
          <div
            className="progress-fill"
            style={{ width: `${progress}%` }}
          />
          <span>{Math.round(progress)}%</span>
        </div>
      )}

      {error && <div className="error">Sync failed: {error.message}</div>}
    </div>
  );
}
```

**Result:** Large graphs sync in the background without freezing the UI.

---

## Service Worker Integration

For full offline support, register a service worker:

Create `public/service-worker.js`:

```javascript
const CACHE_NAME = 'unrdf-app-v1';
const urlsToCache = [
  '/',
  '/app.js',
  '/styles.css',
];

// Install service worker
self.addEventListener('install', (event) => {
  event.waitUntil(
    caches.open(CACHE_NAME)
      .then((cache) => cache.addAll(urlsToCache))
  );
});

// Serve from cache when offline
self.addEventListener('fetch', (event) => {
  event.respondWith(
    caches.match(event.request)
      .then((response) => {
        // Return cached version or fetch from network
        return response || fetch(event.request);
      })
  );
});

// Clean up old caches
self.addEventListener('activate', (event) => {
  event.waitUntil(
    caches.keys().then((cacheNames) => {
      return Promise.all(
        cacheNames.map((cacheName) => {
          if (cacheName !== CACHE_NAME) {
            return caches.delete(cacheName);
          }
        })
      );
    })
  );
});
```

Register in your app:

```jsx
// app/layout.jsx
'use client';

import { useEffect } from 'react';

export default function RootLayout({ children }) {
  useEffect(() => {
    // Register service worker
    if ('serviceWorker' in navigator) {
      navigator.serviceWorker
        .register('/service-worker.js')
        .then((registration) => {
          console.log('Service Worker registered:', registration);
        })
        .catch((error) => {
          console.error('Service Worker registration failed:', error);
        });
    }
  }, []);

  return (
    <html>
      <body>{children}</body>
    </html>
  );
}
```

**Result:** App works completely offline, even after refresh.

---

## Background Sync

Use Background Sync API for reliable sync:

```jsx
import { useBackgroundSync } from 'unrdf-react';

function BackgroundSyncApp() {
  const { register, unregister } = useBackgroundSync({
    tag: 'unrdf-sync',

    onSync: async () => {
      // This runs when browser detects connectivity
      console.log('Background sync triggered!');
      // Perform your sync logic here
    },
  });

  // Register sync on component mount
  useEffect(() => {
    register();
    return () => unregister();
  }, [register, unregister]);

  return <div>Background sync enabled</div>;
}
```

**Result:** Changes sync automatically in the background, even when app is closed.

---

## Complete Offline-First Example

Here's a full offline-capable app:

```jsx
'use client';

import {
  useKnowledgeEngine,
  useStorage,
  useOnlineStatus,
  useOfflineQueue
} from 'unrdf-react';
import { useState, useEffect } from 'react';

export default function OfflineFirstApp() {
  const { engine, ready } = useKnowledgeEngine();
  const { isOnline, wasOffline } = useOnlineStatus();
  const { load } = useStorage({
    storeName: 'offline-app',
    autoSave: true,
    saveInterval: 3000,
  });
  const { enqueue, pendingCount, sync } = useOfflineQueue({
    autoSync: true,
  });

  const [notes, setNotes] = useState([]);
  const [newNote, setNewNote] = useState('');

  // Load persisted data
  useEffect(() => {
    if (ready) {
      load();
    }
  }, [ready, load]);

  // Add note (works offline)
  const addNote = async () => {
    if (!ready || !newNote.trim()) return;

    const noteId = `http://example.org/notes/${Date.now()}`;
    const triples = [
      {
        subject: noteId,
        predicate: 'http://example.org/text',
        object: newNote
      },
      {
        subject: noteId,
        predicate: 'http://example.org/createdAt',
        object: new Date().toISOString()
      }
    ];

    if (isOnline) {
      await engine.addTriples(triples);
    } else {
      enqueue({ type: 'add', triples });
      // Optimistically add to UI
      setNotes([...notes, { id: noteId, text: newNote, pending: true }]);
    }

    setNewNote('');
  };

  return (
    <div className="max-w-2xl mx-auto p-6">
      {/* Status Banner */}
      <div className={`mb-4 p-3 rounded ${isOnline ? 'bg-green-100' : 'bg-yellow-100'}`}>
        {isOnline ? (
          <>
            ✅ Online
            {wasOffline && <span className="ml-2 text-green-700">Reconnected & synced!</span>}
            {pendingCount > 0 && <span className="ml-2">Syncing {pendingCount} changes...</span>}
          </>
        ) : (
          <>⚠️ Offline Mode - Changes saved locally</>
        )}
      </div>

      {/* Add Note */}
      <div className="mb-6 flex gap-2">
        <input
          type="text"
          value={newNote}
          onChange={(e) => setNewNote(e.target.value)}
          onKeyPress={(e) => e.key === 'Enter' && addNote()}
          placeholder="Add a note (works offline!)"
          className="flex-1 px-4 py-2 border rounded"
        />
        <button
          onClick={addNote}
          className="px-6 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
        >
          Add
        </button>
      </div>

      {/* Notes List */}
      <div className="space-y-2">
        {notes.map((note) => (
          <div
            key={note.id}
            className={`p-4 border rounded ${note.pending ? 'bg-blue-50 border-blue-200' : 'bg-white'}`}
          >
            {note.text}
            {note.pending && <span className="ml-2 text-xs text-blue-600">(pending sync)</span>}
          </div>
        ))}
      </div>

      {/* Manual Sync */}
      {!isOnline && pendingCount > 0 && (
        <button
          onClick={sync}
          className="mt-4 px-4 py-2 bg-gray-500 text-white rounded"
          disabled={true}
        >
          Will sync when online ({pendingCount} pending)
        </button>
      )}
    </div>
  );
}
```

---

## Testing Offline Behavior

Test your offline implementation:

1. **Chrome DevTools:**
   - Open DevTools (F12)
   - Go to Network tab
   - Select "Offline" from throttling dropdown

2. **Simulated Offline:**
   ```jsx
   // For testing only
   const { setOnline } = useOnlineStatus();

   <button onClick={() => setOnline(false)}>Simulate Offline</button>
   ```

3. **IndexedDB Inspection:**
   - Chrome DevTools > Application tab > IndexedDB
   - Verify data persistence

---

## Related

- [Reference: Storage Hooks API](../reference/storage-hooks.md)
- [Reference: Offline Queue API](../reference/offline-hooks.md)
- [How-to: Handle Conflicts](./handle-conflicts.md)
- [Explanation: Offline-First Architecture](../explanation/offline-architecture.md)
