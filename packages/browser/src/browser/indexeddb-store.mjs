/**
 * @unrdf/browser - IndexedDB Store
 *
 * Provides persistent RDF storage in the browser using IndexedDB.
 * Stores quads with indexed subject, predicate, and object for efficient queries.
 *
 * @module @unrdf/browser/indexeddb-store
 */

import { z } from 'zod';
import { createStore, addQuad, removeQuad, getQuads } from '@unrdf/core';

/**
 * @typedef {Object} IndexedDBStore
 * @property {string} dbName - Database name
 * @property {string} storeName - Object store name
 * @property {IDBDatabase|null} db - IndexedDB database instance
 * @property {Object} memoryStore - In-memory N3 store
 * @property {boolean} isOpen - Connection status
 */

const IndexedDBStoreSchema = z.object({
  dbName: z.string().min(1),
  storeName: z.string().min(1),
  db: z.any().nullable(),
  memoryStore: z.any(),
  isOpen: z.boolean(),
});

/**
 * Create an IndexedDB-backed RDF store
 *
 * @param {string} dbName - Database name
 * @param {string} [storeName='quads'] - Object store name
 * @returns {IndexedDBStore} IndexedDB store instance (not yet opened)
 *
 * @example
 * const store = createIndexedDBStore('myapp-rdf', 'quads');
 * await openIndexedDBStore(store);
 */
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

  return IndexedDBStoreSchema.parse(store);
}

/**
 * Open IndexedDB connection and load persisted quads
 *
 * @param {IndexedDBStore} store - Store to open
 * @returns {Promise<IndexedDBStore>} Opened store
 *
 * @example
 * const store = createIndexedDBStore('myapp-rdf');
 * await openIndexedDBStore(store);
 */
export async function openIndexedDBStore(store) {
  IndexedDBStoreSchema.parse(store);

  if (store.isOpen) {
    return store;
  }

  return new Promise((resolve, reject) => {
    const request = indexedDB.open(store.dbName, 1);

    request.onerror = () => reject(new Error(`Failed to open IndexedDB: ${request.error}`));

    request.onsuccess = async () => {
      store.db = request.result;
      store.isOpen = true;

      // Load all persisted quads into memory
      const quads = await getQuadsFromDB(store);
      for (const quad of quads) {
        addQuad(store.memoryStore, quad);
      }

      resolve(store);
    };

    request.onupgradeneeded = event => {
      const db = event.target.result;

      if (!db.objectStoreNames.contains(store.storeName)) {
        const objectStore = db.createObjectStore(store.storeName, {
          keyPath: 'id',
          autoIncrement: true,
        });

        // Create indexes for efficient querying
        objectStore.createIndex('subject', 'subject', { unique: false });
        objectStore.createIndex('predicate', 'predicate', { unique: false });
        objectStore.createIndex('object', 'object', { unique: false });
        objectStore.createIndex('graph', 'graph', { unique: false });
      }
    };
  });
}

/**
 * Close IndexedDB connection
 *
 * @param {IndexedDBStore} store - Store to close
 *
 * @example
 * await closeIndexedDBStore(store);
 */
export function closeIndexedDBStore(store) {
  IndexedDBStoreSchema.parse(store);

  if (store.db) {
    store.db.close();
    store.db = null;
    store.isOpen = false;
  }
}

/**
 * Add quad to IndexedDB and memory store
 *
 * @param {IndexedDBStore} store - Target store
 * @param {Object} quad - Quad to add
 * @returns {Promise<void>}
 *
 * @example
 * await addQuadToDB(store, quad(':alice', 'foaf:name', '"Alice"'));
 */
export async function addQuadToDB(store, quad) {
  IndexedDBStoreSchema.parse(store);

  if (!store.isOpen) {
    throw new Error('Store is not open');
  }

  // Add to memory store
  addQuad(store.memoryStore, quad);

  // Persist to IndexedDB
  const transaction = store.db.transaction([store.storeName], 'readwrite');
  const objectStore = transaction.objectStore(store.storeName);

  const quadData = {
    subject: quad.subject?.value || '',
    predicate: quad.predicate?.value || '',
    object: quad.object?.value || '',
    graph: quad.graph?.value || '',
    subjectType: quad.subject?.termType || 'NamedNode',
    objectType: quad.object?.termType || 'Literal',
    objectLanguage: quad.object?.language || null,
    objectDatatype: quad.object?.datatype?.value || null,
  };

  return new Promise((resolve, reject) => {
    const request = objectStore.add(quadData);
    request.onsuccess = () => resolve();
    request.onerror = () => reject(new Error(`Failed to add quad: ${request.error}`));
  });
}

/**
 * Remove quad from IndexedDB and memory store
 *
 * @param {IndexedDBStore} store - Target store
 * @param {Object} quad - Quad to remove
 * @returns {Promise<void>}
 *
 * @example
 * await removeQuadFromDB(store, quad);
 */
export async function removeQuadFromDB(store, quad) {
  IndexedDBStoreSchema.parse(store);

  if (!store.isOpen) {
    throw new Error('Store is not open');
  }

  // Remove from memory store
  removeQuad(store.memoryStore, quad);

  // Remove from IndexedDB
  const transaction = store.db.transaction([store.storeName], 'readwrite');
  const objectStore = transaction.objectStore(store.storeName);

  // Find and delete matching quads
  const index = objectStore.index('subject');
  const subjectValue = quad.subject?.value || '';
  const predicateValue = quad.predicate?.value || '';
  const objectValue = quad.object?.value || '';
  const graphValue = quad.graph?.value || '';
  const request = index.openCursor(IDBKeyRange.only(subjectValue));

  return new Promise((resolve, reject) => {
    request.onsuccess = event => {
      const cursor = event.target.result;
      if (cursor) {
        const record = cursor.value;
        if (
          record.predicate === predicateValue &&
          record.object === objectValue &&
          record.graph === graphValue
        ) {
          cursor.delete();
          resolve();
        } else {
          cursor.continue();
        }
      } else {
        resolve();
      }
    };
    request.onerror = () => reject(new Error(`Failed to remove quad: ${request.error}`));
  });
}

/**
 * Query quads from IndexedDB
 *
 * @param {IndexedDBStore} store - Source store
 * @param {Object} [filter] - Query filter
 * @returns {Promise<Array<Object>>} Matching quads
 *
 * @example
 * const quads = await getQuadsFromDB(store, { subject: ':alice' });
 */
export async function getQuadsFromDB(store, filter = {}) {
  IndexedDBStoreSchema.parse(store);

  if (!store.isOpen) {
    throw new Error('Store is not open');
  }

  const transaction = store.db.transaction([store.storeName], 'readonly');
  const objectStore = transaction.objectStore(store.storeName);

  // Use index if available
  let request;
  if (filter.subject) {
    const index = objectStore.index('subject');
    request = index.openCursor(IDBKeyRange.only(filter.subject));
  } else if (filter.predicate) {
    const index = objectStore.index('predicate');
    request = index.openCursor(IDBKeyRange.only(filter.predicate));
  } else if (filter.object) {
    const index = objectStore.index('object');
    request = index.openCursor(IDBKeyRange.only(filter.object));
  } else {
    request = objectStore.openCursor();
  }

  return new Promise((resolve, reject) => {
    const quads = [];
    request.onsuccess = event => {
      const cursor = event.target.result;
      if (cursor) {
        const record = cursor.value;

        // Apply remaining filters
        let matches = true;
        if (filter.subject && record.subject !== filter.subject) matches = false;
        if (filter.predicate && record.predicate !== filter.predicate) matches = false;
        if (filter.object && record.object !== filter.object) matches = false;
        if (filter.graph && record.graph !== filter.graph) matches = false;

        if (matches) {
          // Reconstruct quad (simplified - full reconstruction would use createQuad)
          quads.push({
            subject: { value: record.subject, termType: record.subjectType },
            predicate: { value: record.predicate, termType: 'NamedNode' },
            object: {
              value: record.object,
              termType: record.objectType,
              language: record.objectLanguage,
              datatype: record.objectDatatype ? { value: record.objectDatatype } : null,
            },
            graph: { value: record.graph, termType: 'DefaultGraph' },
          });
        }

        cursor.continue();
      } else {
        resolve(quads);
      }
    };
    request.onerror = () => reject(new Error(`Failed to query quads: ${request.error}`));
  });
}

/**
 * Clear all quads from IndexedDB and memory
 *
 * @param {IndexedDBStore} store - Store to clear
 * @returns {Promise<void>}
 *
 * @example
 * await clearIndexedDBStore(store);
 */
export async function clearIndexedDBStore(store) {
  IndexedDBStoreSchema.parse(store);

  if (!store.isOpen) {
    throw new Error('Store is not open');
  }

  // Clear memory store
  store.memoryStore = createStore();

  // Clear IndexedDB
  const transaction = store.db.transaction([store.storeName], 'readwrite');
  const objectStore = transaction.objectStore(store.storeName);

  return new Promise((resolve, reject) => {
    const request = objectStore.clear();
    request.onsuccess = () => resolve();
    request.onerror = () => reject(new Error(`Failed to clear store: ${request.error}`));
  });
}
