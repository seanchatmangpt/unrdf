/**
 * Browser Receipt Store Tests
 *
 * Tests IndexedDB-backed receipt storage.
 */

import { test } from 'node:test';
import assert from 'node:assert';
import { BrowserReceiptStore } from '../../src/browser/receipt-store.mjs';
import { createReceipt } from '../../src/receipts/index.mjs';

// Mock IndexedDB for Node.js testing
// In real browser tests, this would use actual IndexedDB
let mockDB = {
  receipts: new Map(),
  merkleTrees: new Map(),
};

// Mock indexedDB API
if (typeof indexedDB === 'undefined') {
  global.indexedDB = {
    open: (dbName, version) => {
      const db = {
        objectStoreNames: {
          contains: (name) => true,
        },
        transaction: (stores, mode) => {
          const storeArray = Array.isArray(stores) ? stores : [stores];
          const getStore = (storeName) => {
            const mapKey = storeName === 'receipts' ? 'receipts' : 'merkleTrees';

            return {
              get: (key) => {
                const request = {
                  result: mockDB[mapKey].get(key),
                  error: null,
                  onerror: null,
                  onsuccess: null,
                };
                queueMicrotask(() => {
                  if (request.onsuccess) request.onsuccess();
                });
                return request;
              },
              put: (obj) => {
                const request = {
                  result: undefined,
                  error: null,
                  onerror: null,
                  onsuccess: null,
                };
                mockDB[mapKey].set(obj.id, obj);
                queueMicrotask(() => {
                  if (request.onsuccess) request.onsuccess();
                });
                return request;
              },
              getAll: (range) => {
                const request = {
                  result: Array.from(mockDB[mapKey].values()),
                  error: null,
                  onerror: null,
                  onsuccess: null,
                };
                queueMicrotask(() => {
                  if (request.onsuccess) request.onsuccess();
                });
                return request;
              },
              clear: () => {
                const request = {
                  error: null,
                  onerror: null,
                  onsuccess: null,
                };
                mockDB[mapKey].clear();
                queueMicrotask(() => {
                  if (request.onsuccess) request.onsuccess();
                });
                return request;
              },
              index: (name) => ({
                get: (val) => {
                  const request = {
                    result: Array.from(mockDB.receipts.values()).find(r => r[name] === val),
                    error: null,
                    onerror: null,
                    onsuccess: null,
                  };
                  queueMicrotask(() => {
                    if (request.onsuccess) request.onsuccess();
                  });
                  return request;
                },
                getAll: (range) => {
                  const request = {
                    result: Array.from(mockDB.receipts.values()),
                    error: null,
                    onerror: null,
                    onsuccess: null,
                  };
                  queueMicrotask(() => {
                    if (request.onsuccess) request.onsuccess();
                  });
                  return request;
                },
              }),
            };
          };

          const tx = {
            objectStore: getStore,
            error: null,
            onerror: null,
            oncomplete: null,
          };

          // Trigger oncomplete after microtask queue
          queueMicrotask(() => {
            if (tx.oncomplete) tx.oncomplete();
          });

          return tx;
        },
        close: () => {},
      };

      const request = {
        result: db,
        error: null,
        onerror: null,
        onsuccess: null,
        onupgradeneeded: null,
      };

      // Trigger onsuccess asynchronously
      queueMicrotask(() => {
        if (request.onsuccess) request.onsuccess();
      });

      return request;
    },
  };

  // Mock IDBKeyRange
  global.IDBKeyRange = {
    only: (value) => ({ only: value }),
    lowerBound: (lower, open) => ({ lower, lowerOpen: open }),
    upperBound: (upper, open) => ({ upper, upperOpen: open }),
    bound: (lower, upper, lowerOpen, upperOpen) => ({ lower, upper, lowerOpen, upperOpen }),
  };
}

test('BrowserReceiptStore - create and init', async (t) => {
  const store = new BrowserReceiptStore({ dbName: 'test-receipts-1' });
  assert.ok(store, 'Store created');
  assert.strictEqual(store.dbName, 'test-receipts-1', 'DB name set correctly');

  // Note: In real browser environment, this would actually open IndexedDB
  // For Node.js tests, we're using mocks
  await store.init();
  // Wait for microtasks to complete
  await new Promise(resolve => setImmediate(resolve));
  assert.ok(store.db, 'DB initialized');
});

test('BrowserReceiptStore - save and retrieve receipt', async (t) => {
  const store = new BrowserReceiptStore({ dbName: 'test-receipts-2' });
  await store.init();
  await new Promise(resolve => setImmediate(resolve));

  const receipt = await createReceipt('execution', {
    eventType: 'TASK_COMPLETED',
    caseId: 'case-123',
    taskId: 'task-456',
    payload: { result: 'success', data: { value: 42 } },
  });

  // Save receipt
  await store.saveReceipt(receipt);
  await new Promise(resolve => setImmediate(resolve));
  assert.ok(true, 'Receipt saved successfully');

  // Retrieve receipt
  const retrieved = await store.getReceipt(receipt.id);
  await new Promise(resolve => setImmediate(resolve));
  assert.ok(retrieved, 'Receipt retrieved');
  assert.strictEqual(retrieved.id, receipt.id, 'Receipt ID matches');
  assert.strictEqual(retrieved.receiptType, 'execution', 'Receipt type matches');
});

test('BrowserReceiptStore - handles errors gracefully', async (t) => {
  const store = new BrowserReceiptStore();

  // Should throw error when not initialized
  await assert.rejects(
    async () => {
      await store.saveReceipt({});
      await new Promise(resolve => setImmediate(resolve));
    },
    /Store not initialized/,
    'Throws error when not initialized'
  );
});

console.log('✅ Browser receipt store tests defined');
console.log('ℹ️  Note: These tests use mocks in Node.js');
console.log('ℹ️  Run in real browser environment for full IndexedDB testing');
