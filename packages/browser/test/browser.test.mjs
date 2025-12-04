/**
 * @vitest-environment jsdom
 */

import { describe, it, expect } from 'vitest';
import {
  createIndexedDBStore,
  openIndexedDBStore,
  closeIndexedDBStore,
  addQuadToDB,
  removeQuadFromDB,
  getQuadsFromDB,
} from '../src/browser/indexeddb-store.mjs';
import {
  createBrowserRDFStore,
  isBrowserEnvironment,
  getStorageAdapter,
  getBrowserComunicaAdapter,
  isServiceWorkerSupported,
  checkStorageQuota,
  requestPersistentStorage,
  isStoragePersisted,
} from '../src/browser/browser-adapters.mjs';
import {
  serializeQuadForStorage,
  deserializeQuad,
  calculateQuadSize,
  getStorageQuota,
  estimateCapacity,
  isStorageApproachingLimit,
  formatStorageSize,
} from '../src/browser/utils.mjs';
import {
  registerServiceWorker,
  initOfflineSupport,
  sendMessageToServiceWorker,
  requestBackgroundSync,
} from '../src/browser/service-worker.mjs';

describe('@unrdf/browser - IndexedDB Store', () => {
  it('should create IndexedDB store with default store name', () => {
    const store = createIndexedDBStore('test-db');
    expect(store.dbName).toBe('test-db');
    expect(store.storeName).toBe('quads');
    expect(store.isOpen).toBe(false);
  });

  it('should create IndexedDB store with custom store name', () => {
    const store = createIndexedDBStore('test-db', 'custom-store');
    expect(store.dbName).toBe('test-db');
    expect(store.storeName).toBe('custom-store');
  });

  it('should validate dbName is non-empty', () => {
    expect(() => createIndexedDBStore('')).toThrow();
  });

  it('should validate storeName is non-empty', () => {
    expect(() => createIndexedDBStore('test-db', '')).toThrow();
  });

  it('should have memoryStore property', () => {
    const store = createIndexedDBStore('test-db');
    expect(store.memoryStore).toBeDefined();
  });

  it('should reject opening store with invalid structure', async () => {
    const invalidStore = { dbName: 'test', invalid: true };
    await expect(openIndexedDBStore(invalidStore)).rejects.toThrow();
  });

  it('should handle close operation on unopened store', () => {
    const store = createIndexedDBStore('test-db');
    expect(() => closeIndexedDBStore(store)).not.toThrow();
  });

  it('should reject quad operations on closed store', async () => {
    const store = createIndexedDBStore('test-db');
    const quad = {
      subject: { value: 'http://example.org/alice', termType: 'NamedNode' },
      predicate: { value: 'http://xmlns.com/foaf/0.1/name', termType: 'NamedNode' },
      object: { value: 'Alice', termType: 'Literal' },
      graph: { value: '', termType: 'DefaultGraph' },
    };
    await expect(addQuadToDB(store, quad)).rejects.toThrow('Store is not open');
  });
});

describe('@unrdf/browser - Browser Adapters', () => {
  it('should detect browser environment in jsdom', () => {
    const result = isBrowserEnvironment();
    expect(typeof result).toBe('boolean');
  });

  it('should get storage adapter type', () => {
    const adapter = getStorageAdapter();
    expect(['indexeddb', 'memory']).toContain(adapter);
  });

  it('should create browser RDF store with default options', async () => {
    const store = await createBrowserRDFStore();
    expect(store).toBeDefined();
  });

  it('should create browser RDF store with custom options', async () => {
    const store = await createBrowserRDFStore({
      dbName: 'custom-db',
      storeName: 'custom-store',
      persistent: false,
    });
    expect(store).toBeDefined();
  });

  it('should get browser Comunica adapter config', () => {
    const config = getBrowserComunicaAdapter();
    expect(config).toHaveProperty('sources');
    expect(config).toHaveProperty('lenient');
  });

  it('should check service worker support', () => {
    const supported = isServiceWorkerSupported();
    expect(typeof supported).toBe('boolean');
  });

  it('should check storage quota', async () => {
    const quota = await checkStorageQuota();
    expect(quota).toHaveProperty('usage');
    expect(quota).toHaveProperty('quota');
    expect(quota).toHaveProperty('percentUsed');
  });

  it('should handle persistent storage request', async () => {
    const result = await requestPersistentStorage();
    expect(typeof result).toBe('boolean');
  });

  it('should check if storage is persisted', async () => {
    const result = await isStoragePersisted();
    expect(typeof result).toBe('boolean');
  });
});

describe('@unrdf/browser - Utilities', () => {
  const sampleQuad = {
    subject: { value: 'http://example.org/alice', termType: 'NamedNode' },
    predicate: { value: 'http://xmlns.com/foaf/0.1/name', termType: 'NamedNode' },
    object: {
      value: 'Alice',
      termType: 'Literal',
      language: 'en',
      datatype: { value: 'http://www.w3.org/2001/XMLSchema#string' },
    },
    graph: { value: '', termType: 'DefaultGraph' },
  };

  it('should serialize quad for storage', () => {
    const serialized = serializeQuadForStorage(sampleQuad);
    expect(serialized).toHaveProperty('subject', 'http://example.org/alice');
    expect(serialized).toHaveProperty('predicate', 'http://xmlns.com/foaf/0.1/name');
    expect(serialized).toHaveProperty('object', 'Alice');
    expect(serialized).toHaveProperty('objectLanguage', 'en');
  });

  it('should deserialize quad from storage', () => {
    const serialized = serializeQuadForStorage(sampleQuad);
    const deserialized = deserializeQuad(serialized);
    expect(deserialized.subject.value).toBe(sampleQuad.subject.value);
    expect(deserialized.predicate.value).toBe(sampleQuad.predicate.value);
    expect(deserialized.object.value).toBe(sampleQuad.object.value);
  });

  it('should calculate quad size', () => {
    const size = calculateQuadSize(sampleQuad);
    expect(size).toBeGreaterThan(0);
  });

  it('should get storage quota', async () => {
    const quota = await getStorageQuota();
    expect(quota).toHaveProperty('available');
    expect(quota).toHaveProperty('used');
    expect(quota).toHaveProperty('quota');
  });

  it('should estimate capacity', async () => {
    const capacity = await estimateCapacity(sampleQuad);
    expect(capacity).toBeGreaterThanOrEqual(0);
  });

  it('should check if storage approaching limit', async () => {
    const approaching = await isStorageApproachingLimit(0.8);
    expect(typeof approaching).toBe('boolean');
  });

  it('should validate threshold range', async () => {
    await expect(isStorageApproachingLimit(-0.1)).rejects.toThrow();
    await expect(isStorageApproachingLimit(1.1)).rejects.toThrow();
  });

  it('should format storage size in bytes', () => {
    expect(formatStorageSize(0)).toBe('0.0 B');
    expect(formatStorageSize(500)).toBe('500.0 B');
  });

  it('should format storage size in KB', () => {
    expect(formatStorageSize(1024)).toBe('1.0 KB');
    expect(formatStorageSize(2048)).toBe('2.0 KB');
  });

  it('should format storage size in MB', () => {
    expect(formatStorageSize(1048576)).toBe('1.0 MB');
  });

  it('should format storage size in GB', () => {
    expect(formatStorageSize(1073741824)).toBe('1.0 GB');
  });

  it('should reject negative bytes', () => {
    expect(() => formatStorageSize(-1)).toThrow();
  });
});

describe('@unrdf/browser - Service Worker', () => {
  it('should handle service worker registration in non-SW environment', async () => {
    const result = await registerServiceWorker();
    expect(result).toBeNull();
  });

  it('should validate service worker URL', async () => {
    await expect(registerServiceWorker('')).rejects.toThrow();
  });

  it('should initialize offline support', async () => {
    const offline = await initOfflineSupport();
    expect(offline).toHaveProperty('enabled');
  });

  it('should initialize offline support with custom options', async () => {
    const offline = await initOfflineSupport({
      swURL: '/custom-sw.js',
      autoSync: false,
    });
    expect(offline).toBeDefined();
  });

  it('should reject sending message without active SW', async () => {
    await expect(sendMessageToServiceWorker({ type: 'TEST' })).rejects.toThrow(
      'No active service worker'
    );
  });

  it('should reject background sync without SW', async () => {
    await expect(requestBackgroundSync()).rejects.toThrow('Service worker not ready');
  });

  it('should validate sync tag', async () => {
    await expect(requestBackgroundSync('')).rejects.toThrow();
  });
});

describe('@unrdf/browser - Integration', () => {
  it('should serialize and deserialize quad round-trip', () => {
    const originalQuad = {
      subject: { value: 'http://example.org/alice', termType: 'NamedNode' },
      predicate: { value: 'http://xmlns.com/foaf/0.1/name', termType: 'NamedNode' },
      object: { value: 'Alice', termType: 'Literal' },
      graph: { value: '', termType: 'DefaultGraph' },
    };

    const serialized = serializeQuadForStorage(originalQuad);
    const deserialized = deserializeQuad(serialized);

    expect(deserialized.subject.value).toBe(originalQuad.subject.value);
    expect(deserialized.predicate.value).toBe(originalQuad.predicate.value);
    expect(deserialized.object.value).toBe(originalQuad.object.value);
  });

  it('should create browser store and check environment', async () => {
    const store = await createBrowserRDFStore();
    const isBrowser = isBrowserEnvironment();
    const adapter = getStorageAdapter();

    expect(store).toBeDefined();
    expect(typeof isBrowser).toBe('boolean');
    expect(['indexeddb', 'memory']).toContain(adapter);
  });

  it('should handle storage quota operations', async () => {
    const quota = await getStorageQuota();
    const formatted = formatStorageSize(quota.used);

    expect(quota).toBeDefined();
    expect(formatted).toMatch(/\d+\.\d+ [KMGT]?B/);
  });
});

describe('@unrdf/browser - Error Handling', () => {
  it('should handle invalid store configuration', () => {
    expect(() => createIndexedDBStore('')).toThrow();
  });

  it('should handle operations on closed store', async () => {
    const store = createIndexedDBStore('test-db');
    const quad = {
      subject: { value: 'http://example.org/s', termType: 'NamedNode' },
      predicate: { value: 'http://example.org/p', termType: 'NamedNode' },
      object: { value: 'o', termType: 'Literal' },
      graph: { value: '', termType: 'DefaultGraph' },
    };

    await expect(addQuadToDB(store, quad)).rejects.toThrow('Store is not open');
    await expect(removeQuadFromDB(store, quad)).rejects.toThrow('Store is not open');
    await expect(getQuadsFromDB(store)).rejects.toThrow('Store is not open');
  });

  it('should validate threshold bounds', async () => {
    await expect(isStorageApproachingLimit(1.5)).rejects.toThrow();
  });

  it('should reject empty service worker URL', async () => {
    await expect(registerServiceWorker('')).rejects.toThrow();
  });
});
