/**
 * @vitest-environment jsdom
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { indexedDB, IDBKeyRange } from 'fake-indexeddb';
import {
  createIndexedDBStore,
  addSampleData,
  queryQuads,
  verifyPersistence,
  checkStorageQuota,
  clearAllData
} from '../src/index.mjs';
import { namedNode } from '@unrdf/core';

// Mock IndexedDB and navigator in jsdom environment
global.indexedDB = indexedDB;
global.IDBKeyRange = IDBKeyRange;

describe('IndexedDB Persistence Example', () => {
  let store;

  beforeEach(async () => {
    store = await createIndexedDBStore();
  });

  afterEach(async () => {
    if (store) {
      await clearAllData(store);
      await store.close();
    }
  });

  describe('Store Creation', () => {
    it('should create IndexedDB store', () => {
      expect(store).toBeDefined();
      expect(store.constructor.name).toBe('IndexedDBStore');
    });

    it('should open database connection', async () => {
      expect(store.db).toBeDefined();
    });
  });

  describe('Quad Insertion', () => {
    it('should add sample data to store', async () => {
      await addSampleData(store);
      const size = await store.size();
      expect(size).toBe(3);
    });

    it('should handle duplicate quad insertion', async () => {
      await addSampleData(store);
      await addSampleData(store);
      const size = await store.size();
      expect(size).toBe(3);
    });
  });

  describe('Quad Queries', () => {
    it('should query all quads', async () => {
      await addSampleData(store);
      const allQuads = await queryQuads(store);
      expect(allQuads).toBeInstanceOf(Array);
      expect(allQuads.length).toBe(3);
    });

    it('should query quads by predicate pattern', async () => {
      await addSampleData(store);
      const nameQuads = await queryQuads(store, {
        predicate: namedNode('http://xmlns.com/foaf/0.1/name')
      });
      expect(nameQuads).toBeInstanceOf(Array);
      expect(nameQuads.length).toBe(2);
    });

    it('should query quads by subject pattern', async () => {
      await addSampleData(store);
      const aliceQuads = await queryQuads(store, {
        subject: namedNode('http://example.org/user/1')
      });
      expect(aliceQuads.length).toBe(2);
    });

    it('should handle empty queries', async () => {
      const quads = await queryQuads(store);
      expect(quads).toEqual([]);
    });
  });

  describe('Quad Removal', () => {
    it('should remove specific quad', async () => {
      await addSampleData(store);
      const allQuads = await queryQuads(store);
      await store.remove(allQuads[0]);
      const remaining = await store.size();
      expect(remaining).toBe(2);
    });
  });

  describe('Session Persistence', () => {
    it('should persist data across sessions', async () => {
      await addSampleData(store);
      const persisted = await verifyPersistence(store);
      expect(persisted).toBe(true);
    });

    it('should maintain data after close and reopen', async () => {
      await addSampleData(store);
      const sizeBefore = await store.size();
      await store.close();
      await store.open();
      const sizeAfter = await store.size();
      expect(sizeBefore).toBe(sizeAfter);
    });
  });

  describe('Storage Quota', () => {
    it('should check storage quota when API available', async () => {
      global.navigator = {
        storage: {
          estimate: vi.fn().mockResolvedValue({
            usage: 1024 * 1024,
            quota: 100 * 1024 * 1024
          })
        }
      };

      const quota = await checkStorageQuota();
      expect(quota.supported).toBe(true);
      expect(quota.usageInMB).toBeDefined();
      expect(quota.quotaInMB).toBeDefined();
      expect(quota.percentUsed).toBeDefined();
    });

    it('should handle missing Storage API', async () => {
      global.navigator = {};
      const quota = await checkStorageQuota();
      expect(quota.supported).toBe(false);
    });
  });

  describe('Concurrent Operations', () => {
    it('should handle concurrent reads', async () => {
      await addSampleData(store);
      const promises = Array(5).fill(0).map(() => queryQuads(store));
      const results = await Promise.all(promises);
      results.forEach(result => {
        expect(result.length).toBe(3);
      });
    });

    it('should handle concurrent writes', async () => {
      const writes = Array(3).fill(0).map((_, i) =>
        store.add({
          subject: namedNode(`http://example.org/user/${i}`),
          predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
          object: { value: `User${i}` },
          graph: { value: '' }
        })
      );
      await Promise.all(writes);
      const size = await store.size();
      expect(size).toBe(3);
    });
  });

  describe('Data Cleanup', () => {
    it('should clear all data', async () => {
      await addSampleData(store);
      await clearAllData(store);
      const size = await store.size();
      expect(size).toBe(0);
    });
  });
});
