/**
 * @vitest-environment jsdom
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { indexedDB, IDBKeyRange } from 'fake-indexeddb';
import {
  OfflineRDFStore,
  setupOfflineSupport,
  cacheRDFData,
  handleNetworkDetection
} from '../src/index.mjs';
import { quad, namedNode, literal } from '@unrdf/core';

// Mock IndexedDB and browser APIs
global.indexedDB = indexedDB;
global.IDBKeyRange = IDBKeyRange;
global.navigator = { onLine: true };
global.window = {
  addEventListener: vi.fn()
};

describe('Offline Support Example', () => {
  let store;

  beforeEach(async () => {
    store = await setupOfflineSupport();
  });

  afterEach(async () => {
    if (store) {
      await store.clearCache();
      await store.close();
    }
  });

  describe('Offline Store Creation', () => {
    it('should create offline RDF store', () => {
      expect(store).toBeInstanceOf(OfflineRDFStore);
    });

    it('should initialize with online status', () => {
      expect(store.getNetworkStatus()).toBe(true);
    });

    it('should setup network listeners', () => {
      expect(window.addEventListener).toHaveBeenCalledWith('online', expect.any(Function));
      expect(window.addEventListener).toHaveBeenCalledWith('offline', expect.any(Function));
    });
  });

  describe('Online Operations', () => {
    it('should add quad when online', async () => {
      const testQuad = quad(
        namedNode('http://example.org/user/1'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      );

      await store.add(testQuad);
      const results = await store.match({ subject: testQuad.subject });
      expect(results.length).toBe(1);
    });

    it('should remove quad when online', async () => {
      const testQuad = quad(
        namedNode('http://example.org/user/1'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      );

      await store.add(testQuad);
      await store.remove(testQuad);
      const results = await store.match({ subject: testQuad.subject });
      expect(results.length).toBe(0);
    });
  });

  describe('Operation Queuing While Offline', () => {
    beforeEach(() => {
      store.isOnline = false;
    });

    it('should queue add operation when offline', async () => {
      const testQuad = quad(
        namedNode('http://example.org/user/1'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      );

      await store.add(testQuad);
      expect(store.getQueueSize()).toBe(1);
    });

    it('should queue remove operation when offline', async () => {
      const testQuad = quad(
        namedNode('http://example.org/user/1'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      );

      await store.remove(testQuad);
      expect(store.getQueueSize()).toBe(1);
    });

    it('should queue multiple operations', async () => {
      await store.add(quad(
        namedNode('http://example.org/user/1'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      ));
      await store.add(quad(
        namedNode('http://example.org/user/2'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Bob')
      ));

      expect(store.getQueueSize()).toBe(2);
    });
  });

  describe('Automatic Sync When Online', () => {
    it('should sync queued operations when coming online', async () => {
      store.isOnline = false;

      await store.add(quad(
        namedNode('http://example.org/user/1'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      ));

      expect(store.getQueueSize()).toBe(1);

      store.isOnline = true;
      const syncResult = await store.syncQueuedOperations();

      expect(syncResult.synced).toBe(1);
      expect(syncResult.failed).toBe(0);
      expect(store.getQueueSize()).toBe(0);
    });

    it('should handle sync failures gracefully', async () => {
      store.isOnline = false;
      const invalidQuad = { invalid: 'data' };
      await store.add(invalidQuad);

      store.isOnline = true;
      const syncResult = await store.syncQueuedOperations();

      expect(syncResult.failed).toBeGreaterThan(0);
    });
  });

  describe('Network Detection', () => {
    it('should detect online status', () => {
      store.isOnline = true;
      const status = handleNetworkDetection(store);
      expect(status.online).toBe(true);
    });

    it('should detect offline status', () => {
      store.isOnline = false;
      const status = handleNetworkDetection(store);
      expect(status.online).toBe(false);
    });

    it('should report queue size in status', async () => {
      store.isOnline = false;
      await store.add(quad(
        namedNode('http://example.org/user/1'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      ));

      const status = handleNetworkDetection(store);
      expect(status.queueSize).toBe(1);
    });
  });

  describe('Query While Offline', () => {
    it('should query cached data while offline', async () => {
      store.isOnline = true;
      await store.add(quad(
        namedNode('http://example.org/user/1'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      ));

      store.isOnline = false;
      const results = await store.match({
        subject: namedNode('http://example.org/user/1')
      });

      expect(results.length).toBe(1);
    });
  });

  describe('Conflict Handling', () => {
    it('should handle sync with existing data', async () => {
      const testQuad = quad(
        namedNode('http://example.org/user/1'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      );

      await store.add(testQuad);

      store.isOnline = false;
      await store.add(testQuad);

      store.isOnline = true;
      const syncResult = await store.syncQueuedOperations();

      expect(syncResult.synced).toBeGreaterThanOrEqual(1);
    });
  });

  describe('Queue Persistence', () => {
    it('should preserve queue across operations', async () => {
      store.isOnline = false;
      await store.add(quad(
        namedNode('http://example.org/user/1'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      ));

      const queueBefore = store.getQueueSize();
      await store.match({});
      const queueAfter = store.getQueueSize();

      expect(queueBefore).toBe(queueAfter);
    });
  });

  describe('Cache Management', () => {
    it('should cache RDF data', async () => {
      const quads = [
        quad(
          namedNode('http://example.org/user/1'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice')
        ),
        quad(
          namedNode('http://example.org/user/2'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Bob')
        )
      ];

      await cacheRDFData(store, quads);
      const results = await store.match({});
      expect(results.length).toBe(2);
    });

    it('should clear cache', async () => {
      await store.add(quad(
        namedNode('http://example.org/user/1'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      ));

      await store.clearCache();
      const results = await store.match({});
      expect(results.length).toBe(0);
      expect(store.getQueueSize()).toBe(0);
    });
  });
});
