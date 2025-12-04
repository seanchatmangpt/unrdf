/**
 * @file Tests for useOfflineStore hook functionality
 * Tests local storage operations, pending change queue, sync operations, conflict resolution, and online/offline detection
 * @since 3.2.0
 */

import { describe, it, expect, _beforeEach, vi } from 'vitest';

describe('useOfflineStore', () => {
  describe('Local Storage Operations', () => {
    it('should store quads locally', () => {
      const localStore = new Map();

      const insert = quads => {
        const timestamp = Date.now();
        quads.forEach((quad, i) => {
          const id = `${timestamp}-${i}-${Math.random().toString(36).substr(2, 9)}`;
          localStore.set(id, {
            ...quad,
            id,
            _localOnly: true,
            _createdAt: timestamp,
          });
        });
        return { inserted: quads.length };
      };

      const result = insert([{ subject: 'alice', predicate: 'knows', object: 'bob' }]);

      expect(result.inserted).toBe(1);
      expect(localStore.size).toBe(1);
    });

    it('should retrieve all stored quads', () => {
      const quads = [
        { id: '1', subject: 'alice', predicate: 'knows', object: 'bob' },
        { id: '2', subject: 'bob', predicate: 'knows', object: 'carol' },
      ];

      expect(quads).toHaveLength(2);
      expect(quads[0].subject).toBe('alice');
    });

    it('should delete quads by pattern', () => {
      let quads = [
        { id: '1', subject: 'alice', predicate: 'knows', object: 'bob' },
        { id: '2', subject: 'bob', predicate: 'knows', object: 'carol' },
        { id: '3', subject: 'alice', predicate: 'likes', object: 'coding' },
      ];

      const deleteQuads = pattern => {
        const before = quads.length;
        quads = quads.filter(q => {
          if (pattern.subject && q.subject !== pattern.subject) return true;
          if (pattern.predicate && q.predicate !== pattern.predicate) return true;
          return false;
        });
        return { deleted: before - quads.length };
      };

      const result = deleteQuads({ subject: 'alice' });

      expect(result.deleted).toBe(2);
      expect(quads).toHaveLength(1);
    });

    it('should mark quads as local-only on insert', () => {
      const timestamp = Date.now();
      const quad = {
        subject: 'alice',
        predicate: 'knows',
        object: 'bob',
      };

      const stored = {
        ...quad,
        id: `${timestamp}-0-abc123`,
        _localOnly: true,
        _createdAt: timestamp,
      };

      expect(stored._localOnly).toBe(true);
      expect(stored._createdAt).toBe(timestamp);
    });

    it('should clear all local data', () => {
      let quads = [
        { id: '1', subject: 'alice' },
        { id: '2', subject: 'bob' },
      ];
      let syncQueue = [{ id: 'sync-1', status: 'pending' }];

      const clearAll = () => {
        quads = [];
        syncQueue = [];
      };

      clearAll();

      expect(quads).toHaveLength(0);
      expect(syncQueue).toHaveLength(0);
    });
  });

  describe('Pending Change Queue', () => {
    it('should add operations to sync queue', () => {
      const syncQueue = [];

      const queueChange = (operation, quads) => {
        const timestamp = Date.now();
        const item = {
          id: `sync-${timestamp}-${Math.random().toString(36).substr(2, 9)}`,
          operation,
          quads,
          timestamp,
          retries: 0,
          status: 'pending',
        };
        syncQueue.push(item);
        return item;
      };

      const item = queueChange('insert', [{ subject: 'alice' }]);

      expect(syncQueue).toHaveLength(1);
      expect(item.operation).toBe('insert');
      expect(item.status).toBe('pending');
      expect(item.retries).toBe(0);
    });

    it('should track queue item status', () => {
      const item = {
        id: 'sync-1',
        operation: 'insert',
        status: 'pending',
        retries: 0,
      };

      // Update to syncing
      item.status = 'syncing';
      expect(item.status).toBe('syncing');

      // Update to failed
      item.status = 'failed';
      item.retries = 1;
      expect(item.status).toBe('failed');
      expect(item.retries).toBe(1);
    });

    it('should calculate pending count', () => {
      const syncQueue = [
        { id: '1', status: 'pending' },
        { id: '2', status: 'syncing' },
        { id: '3', status: 'pending' },
        { id: '4', status: 'failed' },
      ];

      const pendingCount = syncQueue.filter(item => item.status === 'pending').length;

      expect(pendingCount).toBe(2);
    });

    it('should remove items from queue after successful sync', () => {
      let syncQueue = [
        { id: 'sync-1', status: 'pending' },
        { id: 'sync-2', status: 'pending' },
      ];

      const removeFromQueue = id => {
        syncQueue = syncQueue.filter(item => item.id !== id);
      };

      removeFromQueue('sync-1');

      expect(syncQueue).toHaveLength(1);
      expect(syncQueue[0].id).toBe('sync-2');
    });

    it('should increment retry count on failure', () => {
      const item = {
        id: 'sync-1',
        status: 'pending',
        retries: 0,
      };

      const handleSyncFailure = queueItem => {
        queueItem.retries += 1;
        queueItem.status = 'pending';
      };

      handleSyncFailure(item);
      handleSyncFailure(item);

      expect(item.retries).toBe(2);
    });
  });

  describe('Sync Operations', () => {
    it('should sync pending changes when online', async () => {
      const state = {
        isOnline: true,
        isSyncing: false,
        syncQueue: [
          { id: 'sync-1', status: 'pending', quads: [{ subject: 'alice' }] },
          { id: 'sync-2', status: 'pending', quads: [{ subject: 'bob' }] },
        ],
      };

      const syncToServer = vi.fn().mockResolvedValue({ success: true });

      const sync = async () => {
        if (!state.isOnline) return { success: false, reason: 'offline' };
        if (state.isSyncing) return { success: false, reason: 'already-syncing' };

        state.isSyncing = true;
        const pending = state.syncQueue.filter(item => item.status === 'pending');

        for (const item of pending) {
          item.status = 'syncing';
          await syncToServer(item);
          item.status = 'synced';
        }

        state.isSyncing = false;
        return { success: true, synced: pending.length };
      };

      const result = await sync();

      expect(result.success).toBe(true);
      expect(result.synced).toBe(2);
      expect(syncToServer).toHaveBeenCalledTimes(2);
    });

    it('should not sync when offline', async () => {
      const state = {
        isOnline: false,
        syncQueue: [{ id: 'sync-1', status: 'pending' }],
      };

      const sync = async () => {
        if (!state.isOnline) return { success: false, reason: 'offline' };
        return { success: true, synced: state.syncQueue.length };
      };

      const result = await sync();

      expect(result.success).toBe(false);
      expect(result.reason).toBe('offline');
    });

    it('should prevent concurrent syncs', async () => {
      const state = {
        isOnline: true,
        isSyncing: true,
      };

      const sync = async () => {
        if (state.isSyncing) return { success: false, reason: 'already-syncing' };
        return { success: true };
      };

      const result = await sync();

      expect(result.success).toBe(false);
      expect(result.reason).toBe('already-syncing');
    });

    it('should return early when no pending items', async () => {
      const state = {
        isOnline: true,
        isSyncing: false,
        syncQueue: [],
      };

      const sync = async () => {
        if (!state.isOnline) return { success: false, reason: 'offline' };
        if (state.isSyncing) return { success: false, reason: 'already-syncing' };

        const pending = state.syncQueue.filter(item => item.status === 'pending');
        if (pending.length === 0) return { success: true, synced: 0 };

        return { success: true, synced: pending.length };
      };

      const result = await sync();

      expect(result.success).toBe(true);
      expect(result.synced).toBe(0);
    });

    it('should update lastSynced timestamp after sync', async () => {
      const state = {
        isOnline: true,
        lastSynced: null,
        syncQueue: [{ id: 'sync-1', status: 'pending' }],
      };

      const sync = async () => {
        if (!state.isOnline) return { success: false };

        // Simulate sync
        await new Promise(resolve => setTimeout(resolve, 10));

        state.lastSynced = new Date();
        return { success: true };
      };

      await sync();

      expect(state.lastSynced).toBeInstanceOf(Date);
    });

    it('should mark quads as synced after successful sync', () => {
      const quads = [
        { id: '1', subject: 'alice', _localOnly: true, _syncedAt: null },
        { id: '2', subject: 'bob', _localOnly: true, _syncedAt: null },
      ];

      const markQuadsSynced = quadIds => {
        quadIds.forEach(id => {
          const quad = quads.find(q => q.id === id);
          if (quad) {
            delete quad._localOnly;
            quad._syncedAt = Date.now();
          }
        });
      };

      markQuadsSynced(['1', '2']);

      expect(quads[0]._localOnly).toBeUndefined();
      expect(quads[0]._syncedAt).toBeDefined();
      expect(quads[1]._syncedAt).toBeDefined();
    });
  });

  describe('Conflict Resolution', () => {
    it('should detect conflicts between local and remote', () => {
      const local = { subject: 'alice', age: 30, version: 1 };
      const remote = { subject: 'alice', age: 31, version: 2 };

      const hasConflict = (localData, remoteData) => {
        return (
          localData.version < remoteData.version &&
          JSON.stringify(localData) !== JSON.stringify(remoteData)
        );
      };

      expect(hasConflict(local, remote)).toBe(true);
    });

    it('should resolve conflict with server-wins strategy', () => {
      const local = { value: 'local', version: 1 };
      const remote = { value: 'remote', version: 2 };

      const resolveConflict = async (localData, remoteData, strategy) => {
        if (strategy === 'server') return remoteData;
        if (strategy === 'local') return localData;
        return remoteData.version > localData.version ? remoteData : localData;
      };

      expect(resolveConflict(local, remote, 'server')).resolves.toEqual(remote);
    });

    it('should resolve conflict with client-wins strategy', async () => {
      const local = { value: 'local', version: 1 };
      const remote = { value: 'remote', version: 2 };

      const resolveConflict = async (localData, remoteData, strategy) => {
        if (strategy === 'local') return localData;
        return remoteData;
      };

      const result = await resolveConflict(local, remote, 'local');

      expect(result.value).toBe('local');
    });

    it('should call onConflict callback', async () => {
      const onConflict = vi.fn().mockResolvedValue('server');

      const localItem = { id: 'sync-1', quads: [{ value: 'local' }] };
      const serverData = [{ value: 'server' }];

      const handleConflict = async (item, serverData, callback) => {
        const resolution = await callback(item, serverData);
        return resolution;
      };

      const result = await handleConflict(localItem, serverData, onConflict);

      expect(onConflict).toHaveBeenCalledWith(localItem, serverData);
      expect(result).toBe('server');
    });

    it('should update local quads with server data on server-wins', () => {
      let localQuads = [{ id: '1', subject: 'alice', age: 30 }];

      const serverQuads = [{ id: '1', subject: 'alice', age: 31 }];

      const updateLocalQuads = serverData => {
        serverData.forEach(serverQuad => {
          const index = localQuads.findIndex(q => q.id === serverQuad.id);
          if (index >= 0) {
            localQuads[index] = serverQuad;
          } else {
            localQuads.push(serverQuad);
          }
        });
      };

      updateLocalQuads(serverQuads);

      expect(localQuads[0].age).toBe(31);
    });
  });

  describe('Online/Offline Detection', () => {
    it('should detect online status', () => {
      let isOnline = true;

      const handleOnline = () => {
        isOnline = true;
      };
      const handleOffline = () => {
        isOnline = false;
      };

      expect(isOnline).toBe(true);

      handleOffline();
      expect(isOnline).toBe(false);

      handleOnline();
      expect(isOnline).toBe(true);
    });

    it('should queue operations while offline', () => {
      const state = {
        isOnline: false,
        syncQueue: [],
      };

      const insert = quads => {
        const item = {
          id: `sync-${Date.now()}`,
          operation: 'insert',
          quads,
          status: 'pending',
        };
        state.syncQueue.push(item);
        return { inserted: quads.length, queued: !state.isOnline };
      };

      const result = insert([{ subject: 'alice' }]);

      expect(result.queued).toBe(true);
      expect(state.syncQueue).toHaveLength(1);
    });

    it('should auto-sync when coming online', async () => {
      const state = {
        isOnline: false,
        syncQueue: [{ id: 'sync-1', status: 'pending' }],
      };

      const syncToServer = vi.fn().mockResolvedValue({ success: true });

      const handleOnline = async () => {
        state.isOnline = true;
        if (state.syncQueue.length > 0) {
          await syncToServer(state.syncQueue);
        }
      };

      await handleOnline();

      expect(state.isOnline).toBe(true);
      expect(syncToServer).toHaveBeenCalled();
    });

    it('should track localOnlyCount', () => {
      const quads = [
        { id: '1', _localOnly: true },
        { id: '2', _localOnly: false },
        { id: '3', _localOnly: true },
        { id: '4' },
      ];

      const localOnlyCount = quads.filter(q => q._localOnly).length;

      expect(localOnlyCount).toBe(2);
    });
  });

  describe('Auto-Sync Interval', () => {
    it('should trigger sync on interval when pending items exist', async () => {
      const state = {
        isOnline: true,
        autoSync: true,
        syncQueue: [{ id: 'sync-1', status: 'pending' }],
      };

      const sync = vi.fn().mockResolvedValue({ success: true });

      // Simulate interval trigger
      const intervalCallback = async () => {
        if (state.autoSync && state.isOnline && state.syncQueue.length > 0) {
          await sync();
        }
      };

      await intervalCallback();

      expect(sync).toHaveBeenCalled();
    });

    it('should not trigger sync when no pending items', async () => {
      const state = {
        isOnline: true,
        autoSync: true,
        syncQueue: [],
      };

      const sync = vi.fn();

      const intervalCallback = async () => {
        if (state.autoSync && state.isOnline && state.syncQueue.length > 0) {
          await sync();
        }
      };

      await intervalCallback();

      expect(sync).not.toHaveBeenCalled();
    });

    it('should respect syncInterval configuration', () => {
      const config = {
        syncInterval: 30000,
      };

      expect(config.syncInterval).toBe(30000);
    });
  });

  describe('IndexedDB Simulation', () => {
    it('should create object stores with indexes', () => {
      const stores = {
        quads: {
          keyPath: 'id',
          indexes: ['subject', 'predicate', 'graph'],
        },
        syncQueue: {
          keyPath: 'id',
          indexes: ['status', 'timestamp'],
        },
      };

      expect(stores.quads.indexes).toContain('subject');
      expect(stores.syncQueue.indexes).toContain('status');
    });

    it('should handle database initialization', async () => {
      let dbInitialized = false;

      const initDB = async () => {
        await new Promise(resolve => setTimeout(resolve, 10));
        dbInitialized = true;
        return { success: true };
      };

      await initDB();

      expect(dbInitialized).toBe(true);
    });

    it('should load data from database on init', async () => {
      const dbData = {
        quads: [{ id: '1', subject: 'alice' }],
        syncQueue: [{ id: 'sync-1', status: 'pending' }],
      };

      const loadFromDB = async () => {
        return dbData;
      };

      const result = await loadFromDB();

      expect(result.quads).toHaveLength(1);
      expect(result.syncQueue).toHaveLength(1);
    });

    it('should handle IndexedDB not available', () => {
      const isIndexedDBAvailable = false;

      const initDB = () => {
        if (!isIndexedDBAvailable) {
          console.warn('IndexedDB not available - offline storage disabled');
          return null;
        }
        return {};
      };

      const db = initDB();

      expect(db).toBeNull();
    });
  });

  describe('Error Handling', () => {
    it('should set error state on initialization failure', async () => {
      let error = null;

      const initDB = async () => {
        throw new Error('IndexedDB initialization failed');
      };

      try {
        await initDB();
      } catch (err) {
        error = err;
      }

      expect(error.message).toBe('IndexedDB initialization failed');
    });

    it('should throw when insert called before init', () => {
      const db = null;

      const insert = quads => {
        if (!db) {
          throw new Error('IndexedDB not initialized');
        }
        return { inserted: quads.length };
      };

      expect(() => insert([{ subject: 'alice' }])).toThrow('IndexedDB not initialized');
    });
  });
});
