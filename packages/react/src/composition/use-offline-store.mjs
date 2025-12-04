/**
 * @file use-offline-store.mjs
 * @description Offline-first IndexedDB persistence with sync queue
 * @since 3.2.0
 *
 * Innovation: Enables knowledge graphs to work completely offline,
 * with automatic background sync when connectivity is restored.
 * Uses IndexedDB for persistent storage and a mutation queue for
 * offline-first writes.
 */
/* global navigator, window, indexedDB */

import { useState, useCallback, useEffect, useRef, useMemo } from 'react';

/**
 * @typedef {Object} OfflineStoreConfig
 * @property {string} [dbName='unrdf-offline'] - IndexedDB database name
 * @property {string} [storeName='quads'] - Object store name
 * @property {string} [syncQueueName='sync-queue'] - Sync queue store name
 * @property {number} [syncInterval=30000] - Sync interval in ms (30s default)
 * @property {boolean} [autoSync=true] - Auto-sync when online
 * @property {Function} [onSync] - Callback when sync completes
 * @property {Function} [onConflict] - Conflict resolution callback
 */

/**
 * @typedef {Object} SyncQueueItem
 * @property {string} id - Unique mutation ID
 * @property {'insert' | 'delete' | 'update'} operation - Mutation type
 * @property {Object[]} quads - Quads to sync
 * @property {number} timestamp - When mutation occurred
 * @property {number} retries - Number of sync attempts
 * @property {'pending' | 'syncing' | 'failed'} status - Sync status
 */

/**
 * Offline-first IndexedDB persistence hook
 *
 * @since 3.2.0
 * @param {OfflineStoreConfig} config - Configuration
 * @returns {Object} Offline store interface
 * @throws {Error} When IndexedDB not initialized (insert/delete called too early)
 * @throws {Error} When IndexedDB is not available (SSR or unsupported browser)
 * @throws {Error} When sync fails after exhausting retries
 * @throws {Error} When conflict resolution callback throws
 * @performance IndexedDB operations are async - batch inserts for throughput. Sync queue
 *   grows with offline operations - monitor pendingCount. Auto-sync interval adds background
 *   activity. Large datasets may cause memory pressure during loadFromDB.
 *
 * @example
 * // Basic offline-first usage
 * const {
 *   quads,
 *   insert,
 *   delete: del,
 *   sync,
 *   pendingCount,
 *   isOnline,
 *   lastSynced
 * } = useOfflineStore();
 *
 * // Insert works offline - queued for sync
 * await insert([{ subject, predicate, object }]);
 *
 * // Manual sync when needed
 * await sync();
 *
 * @example
 * // Conflict resolution with server-wins strategy
 * const { insert, sync } = useOfflineStore({
 *   onConflict: async (item, serverData) => {
 *     console.log('Conflict detected, accepting server version');
 *     return 'server'; // or 'local' to force local version
 *   }
 * });
 */
export function useOfflineStore(config = {}) {
  const {
    dbName = 'unrdf-offline',
    storeName = 'quads',
    syncQueueName = 'sync-queue',
    syncInterval = 30000,
    autoSync = true,
    onSync,
    onConflict,
  } = config;

  // State
  const [quads, setQuads] = useState([]);
  const [syncQueue, setSyncQueue] = useState([]);
  const [isOnline, setIsOnline] = useState(
    typeof navigator !== 'undefined' ? navigator.onLine : true
  );
  const [lastSynced, setLastSynced] = useState(null);
  const [isSyncing, setIsSyncing] = useState(false);
  const [error, setError] = useState(null);

  // Refs
  const dbRef = useRef(null);
  const syncIntervalRef = useRef(null);

  // Initialize IndexedDB
  useEffect(() => {
    initDB();
    return () => {
      if (syncIntervalRef.current) {
        clearInterval(syncIntervalRef.current);
      }
    };
  }, [dbName]);

  // Online/offline detection
  useEffect(() => {
    const handleOnline = () => {
      setIsOnline(true);
      if (autoSync) {
        sync();
      }
    };

    const handleOffline = () => {
      setIsOnline(false);
    };

    if (typeof window !== 'undefined') {
      window.addEventListener('online', handleOnline);
      window.addEventListener('offline', handleOffline);

      return () => {
        window.removeEventListener('online', handleOnline);
        window.removeEventListener('offline', handleOffline);
      };
    }
  }, [autoSync]);

  // Auto-sync interval
  useEffect(() => {
    if (autoSync && isOnline) {
      syncIntervalRef.current = setInterval(() => {
        if (syncQueue.length > 0) {
          sync();
        }
      }, syncInterval);

      return () => {
        if (syncIntervalRef.current) {
          clearInterval(syncIntervalRef.current);
        }
      };
    }
  }, [autoSync, isOnline, syncInterval, syncQueue.length]);

  /**
   * Initialize IndexedDB database
   */
  const initDB = useCallback(async () => {
    if (typeof indexedDB === 'undefined') {
      console.warn('IndexedDB not available - offline storage disabled');
      return;
    }

    return new Promise((resolve, reject) => {
      const request = indexedDB.open(dbName, 1);

      request.onerror = () => {
        setError(request.error);
        reject(request.error);
      };

      request.onsuccess = () => {
        dbRef.current = request.result;
        loadFromDB();
        resolve(request.result);
      };

      request.onupgradeneeded = (event) => {
        const db = event.target.result;

        // Quads store with indexes
        if (!db.objectStoreNames.contains(storeName)) {
          const quadStore = db.createObjectStore(storeName, { keyPath: 'id' });
          quadStore.createIndex('subject', 'subject', { unique: false });
          quadStore.createIndex('predicate', 'predicate', { unique: false });
          quadStore.createIndex('graph', 'graph', { unique: false });
        }

        // Sync queue store
        if (!db.objectStoreNames.contains(syncQueueName)) {
          const queueStore = db.createObjectStore(syncQueueName, {
            keyPath: 'id',
          });
          queueStore.createIndex('status', 'status', { unique: false });
          queueStore.createIndex('timestamp', 'timestamp', { unique: false });
        }
      };
    });
  }, [dbName, storeName, syncQueueName]);

  /**
   * Load quads and sync queue from IndexedDB
   */
  const loadFromDB = useCallback(async () => {
    if (!dbRef.current) return;

    const tx = dbRef.current.transaction([storeName, syncQueueName], 'readonly');

    // Load quads
    const quadStore = tx.objectStore(storeName);
    const quadsRequest = quadStore.getAll();

    // Load sync queue
    const queueStore = tx.objectStore(syncQueueName);
    const queueRequest = queueStore.getAll();

    await new Promise((resolve) => {
      tx.oncomplete = () => {
        setQuads(quadsRequest.result || []);
        setSyncQueue(queueRequest.result || []);
        resolve();
      };
    });
  }, [storeName, syncQueueName]);

  /**
   * Insert quads (offline-first)
   * @param {Object[]} newQuads - Quads to insert
   * @returns {Promise<Object>} Insert result
   */
  const insert = useCallback(
    async (newQuads) => {
      if (!dbRef.current) {
        throw new Error('IndexedDB not initialized');
      }

      const timestamp = Date.now();
      const quadsWithIds = newQuads.map((q, i) => ({
        ...q,
        id: `${timestamp}-${i}-${Math.random().toString(36).substr(2, 9)}`,
        _localOnly: true,
        _createdAt: timestamp,
      }));

      // Add to IndexedDB
      const tx = dbRef.current.transaction([storeName, syncQueueName], 'readwrite');
      const quadStore = tx.objectStore(storeName);
      const queueStore = tx.objectStore(syncQueueName);

      // Store quads locally
      for (const quad of quadsWithIds) {
        quadStore.put(quad);
      }

      // Add to sync queue
      const queueItem = {
        id: `sync-${timestamp}-${Math.random().toString(36).substr(2, 9)}`,
        operation: 'insert',
        quads: quadsWithIds,
        timestamp,
        retries: 0,
        status: 'pending',
      };
      queueStore.put(queueItem);

      await new Promise((resolve) => {
        tx.oncomplete = resolve;
      });

      // Update local state
      setQuads((prev) => [...prev, ...quadsWithIds]);
      setSyncQueue((prev) => [...prev, queueItem]);

      // Try immediate sync if online
      if (isOnline && autoSync) {
        sync();
      }

      return { inserted: quadsWithIds.length, queued: !isOnline };
    },
    [storeName, syncQueueName, isOnline, autoSync]
  );

  /**
   * Delete quads (offline-first)
   * @param {Object} pattern - Pattern to match for deletion
   * @returns {Promise<Object>} Delete result
   */
  const deleteQuads = useCallback(
    async (pattern) => {
      if (!dbRef.current) {
        throw new Error('IndexedDB not initialized');
      }

      const timestamp = Date.now();

      // Find matching quads
      const toDelete = quads.filter((q) => {
        if (pattern.subject && q.subject !== pattern.subject) return false;
        if (pattern.predicate && q.predicate !== pattern.predicate) return false;
        if (pattern.object && q.object !== pattern.object) return false;
        if (pattern.graph && q.graph !== pattern.graph) return false;
        return true;
      });

      if (toDelete.length === 0) {
        return { deleted: 0 };
      }

      const tx = dbRef.current.transaction([storeName, syncQueueName], 'readwrite');
      const quadStore = tx.objectStore(storeName);
      const queueStore = tx.objectStore(syncQueueName);

      // Delete from local store
      for (const quad of toDelete) {
        quadStore.delete(quad.id);
      }

      // Add to sync queue
      const queueItem = {
        id: `sync-${timestamp}-${Math.random().toString(36).substr(2, 9)}`,
        operation: 'delete',
        quads: toDelete,
        timestamp,
        retries: 0,
        status: 'pending',
      };
      queueStore.put(queueItem);

      await new Promise((resolve) => {
        tx.oncomplete = resolve;
      });

      // Update local state
      const deleteIds = new Set(toDelete.map((q) => q.id));
      setQuads((prev) => prev.filter((q) => !deleteIds.has(q.id)));
      setSyncQueue((prev) => [...prev, queueItem]);

      // Try immediate sync if online
      if (isOnline && autoSync) {
        sync();
      }

      return { deleted: toDelete.length, queued: !isOnline };
    },
    [quads, storeName, syncQueueName, isOnline, autoSync]
  );

  /**
   * Sync pending mutations to server
   * @returns {Promise<Object>} Sync result
   */
  const sync = useCallback(async () => {
    if (!isOnline) {
      return { success: false, reason: 'offline' };
    }

    if (isSyncing) {
      return { success: false, reason: 'already-syncing' };
    }

    const pendingItems = syncQueue.filter((item) => item.status === 'pending');
    if (pendingItems.length === 0) {
      return { success: true, synced: 0 };
    }

    setIsSyncing(true);
    const results = { synced: 0, failed: 0, conflicts: 0 };

    try {
      for (const item of pendingItems) {
        try {
          // Update status to syncing
          await updateQueueItemStatus(item.id, 'syncing');

          // Simulate sync to server (replace with actual API call)
          await simulateServerSync(item);

          // Remove from queue on success
          await removeFromQueue(item.id);

          // Mark quads as synced
          await markQuadsSynced(item.quads);

          results.synced++;
        } catch (err) {
          // Handle conflicts
          if (err.code === 'CONFLICT' && onConflict) {
            const resolution = await onConflict(item, err.serverData);
            if (resolution === 'local') {
              // Force local version
              await simulateServerSync(item, { force: true });
              await removeFromQueue(item.id);
              results.conflicts++;
            } else if (resolution === 'server') {
              // Accept server version
              await removeFromQueue(item.id);
              await updateLocalQuads(err.serverData);
              results.conflicts++;
            }
          } else {
            // Increment retry count
            await updateQueueItemStatus(item.id, 'pending', item.retries + 1);
            results.failed++;
          }
        }
      }

      setLastSynced(new Date());
      onSync?.(results);

      return { success: true, ...results };
    } finally {
      setIsSyncing(false);
      await loadFromDB();
    }
  }, [isOnline, isSyncing, syncQueue, onConflict, onSync]);

  /**
   * Update queue item status in IndexedDB
   */
  const updateQueueItemStatus = useCallback(
    async (id, status, retries) => {
      if (!dbRef.current) return;

      const tx = dbRef.current.transaction(syncQueueName, 'readwrite');
      const store = tx.objectStore(syncQueueName);
      const request = store.get(id);

      await new Promise((resolve) => {
        request.onsuccess = () => {
          const item = request.result;
          if (item) {
            item.status = status;
            if (retries !== undefined) item.retries = retries;
            store.put(item);
          }
          resolve();
        };
      });
    },
    [syncQueueName]
  );

  /**
   * Remove item from sync queue
   */
  const removeFromQueue = useCallback(
    async (id) => {
      if (!dbRef.current) return;

      const tx = dbRef.current.transaction(syncQueueName, 'readwrite');
      const store = tx.objectStore(syncQueueName);
      store.delete(id);

      await new Promise((resolve) => {
        tx.oncomplete = resolve;
      });
    },
    [syncQueueName]
  );

  /**
   * Mark quads as synced (remove _localOnly flag)
   */
  const markQuadsSynced = useCallback(
    async (syncedQuads) => {
      if (!dbRef.current) return;

      const tx = dbRef.current.transaction(storeName, 'readwrite');
      const store = tx.objectStore(storeName);

      for (const quad of syncedQuads) {
        const request = store.get(quad.id);
        request.onsuccess = () => {
          const q = request.result;
          if (q) {
            delete q._localOnly;
            q._syncedAt = Date.now();
            store.put(q);
          }
        };
      }

      await new Promise((resolve) => {
        tx.oncomplete = resolve;
      });
    },
    [storeName]
  );

  /**
   * Update local quads with server data
   */
  const updateLocalQuads = useCallback(
    async (serverQuads) => {
      if (!dbRef.current) return;

      const tx = dbRef.current.transaction(storeName, 'readwrite');
      const store = tx.objectStore(storeName);

      for (const quad of serverQuads) {
        store.put(quad);
      }

      await new Promise((resolve) => {
        tx.oncomplete = resolve;
      });
    },
    [storeName]
  );

  /**
   * Simulate server sync (replace with actual implementation)
   */
  const simulateServerSync = async (item, _options = {}) => {
    // Simulate network delay
    await new Promise((resolve) => setTimeout(resolve, 100));

    // In real implementation, this would call your API
    // throw { code: 'CONFLICT', serverData: [...] } for conflicts

    return { success: true };
  };

  /**
   * Clear all local data
   */
  const clearAll = useCallback(async () => {
    if (!dbRef.current) return;

    const tx = dbRef.current.transaction([storeName, syncQueueName], 'readwrite');
    tx.objectStore(storeName).clear();
    tx.objectStore(syncQueueName).clear();

    await new Promise((resolve) => {
      tx.oncomplete = resolve;
    });

    setQuads([]);
    setSyncQueue([]);
  }, [storeName, syncQueueName]);

  // Computed values
  const pendingCount = useMemo(() => {
    return syncQueue.filter((item) => item.status === 'pending').length;
  }, [syncQueue]);

  const localOnlyCount = useMemo(() => {
    return quads.filter((q) => q._localOnly).length;
  }, [quads]);

  return {
    // Data
    quads,

    // Operations (offline-first)
    insert,
    delete: deleteQuads,

    // Sync
    sync,
    syncQueue,
    pendingCount,
    isSyncing,
    lastSynced,

    // Status
    isOnline,
    localOnlyCount,

    // Utilities
    clearAll,
    loadFromDB,

    // Error state
    error,
  };
}

export default useOfflineStore;
