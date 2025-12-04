/**
 * @file Service worker for offline RDF data support
 * @module @unrdf/example-offline-support/service-worker
 */

const CACHE_NAME = 'unrdf-offline-v1';
const CACHE_URLS = [
  '/',
  '/index.html',
  '/src/index.mjs'
];

/**
 * Install event - cache initial resources
 */
self.addEventListener('install', (event) => {
  console.log('Service Worker: Installing...');

  event.waitUntil(
    caches.open(CACHE_NAME).then((cache) => {
      console.log('Service Worker: Caching app shell');
      return cache.addAll(CACHE_URLS);
    })
  );

  self.skipWaiting();
});

/**
 * Activate event - cleanup old caches
 */
self.addEventListener('activate', (event) => {
  console.log('Service Worker: Activating...');

  event.waitUntil(
    caches.keys().then((cacheNames) => {
      return Promise.all(
        cacheNames.map((cacheName) => {
          if (cacheName !== CACHE_NAME) {
            console.log('Service Worker: Deleting old cache:', cacheName);
            return caches.delete(cacheName);
          }
        })
      );
    })
  );

  self.clients.claim();
});

/**
 * Fetch event - serve from cache with network fallback
 */
self.addEventListener('fetch', (event) => {
  event.respondWith(
    caches.match(event.request).then((response) => {
      // Return cached response if found
      if (response) {
        console.log('Service Worker: Serving from cache:', event.request.url);
        return response;
      }

      // Otherwise fetch from network
      console.log('Service Worker: Fetching from network:', event.request.url);
      return fetch(event.request).then((response) => {
        // Don't cache non-successful responses
        if (!response || response.status !== 200 || response.type !== 'basic') {
          return response;
        }

        // Clone response to cache
        const responseToCache = response.clone();
        caches.open(CACHE_NAME).then((cache) => {
          cache.put(event.request, responseToCache);
        });

        return response;
      }).catch((error) => {
        console.error('Service Worker: Fetch failed:', error);

        // Return offline page if available
        return caches.match('/offline.html');
      });
    })
  );
});

/**
 * Message event - handle commands from main thread
 */
self.addEventListener('message', (event) => {
  console.log('Service Worker: Received message:', event.data);

  if (event.data && event.data.type === 'SKIP_WAITING') {
    self.skipWaiting();
  }

  if (event.data && event.data.type === 'CLEAR_CACHE') {
    caches.delete(CACHE_NAME).then(() => {
      event.ports[0].postMessage({ success: true });
    });
  }

  if (event.data && event.data.type === 'GET_CACHE_SIZE') {
    caches.open(CACHE_NAME).then((cache) => {
      cache.keys().then((keys) => {
        event.ports[0].postMessage({ size: keys.length });
      });
    });
  }
});

/**
 * Background sync event - sync queued data when online
 */
self.addEventListener('sync', (event) => {
  console.log('Service Worker: Background sync:', event.tag);

  if (event.tag === 'sync-rdf-data') {
    event.waitUntil(syncRDFData());
  }
});

/**
 * Sync RDF data with server
 *
 * @returns {Promise<void>}
 */
async function syncRDFData() {
  console.log('Service Worker: Syncing RDF data...');

  try {
    // Get queued operations from IndexedDB
    const db = await openIndexedDB();
    const operations = await getQueuedOperations(db);

    // Sync each operation
    for (const op of operations) {
      await syncOperation(op);
    }

    // Clear queue after successful sync
    await clearQueue(db);

    console.log('Service Worker: Sync complete');
  } catch (error) {
    console.error('Service Worker: Sync failed:', error);
    throw error;
  }
}

/**
 * Open IndexedDB connection
 *
 * @returns {Promise<IDBDatabase>}
 */
function openIndexedDB() {
  return new Promise((resolve, reject) => {
    const request = indexedDB.open('unrdf-offline', 1);
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => reject(request.error);
  });
}

/**
 * Get queued operations from IndexedDB
 *
 * @param {IDBDatabase} db - Database connection
 * @returns {Promise<Array>}
 */
function getQueuedOperations(db) {
  return new Promise((resolve, reject) => {
    const transaction = db.transaction(['queue'], 'readonly');
    const store = transaction.objectStore('queue');
    const request = store.getAll();

    request.onsuccess = () => resolve(request.result);
    request.onerror = () => reject(request.error);
  });
}

/**
 * Sync single operation
 *
 * @param {object} operation - Operation to sync
 * @returns {Promise<Response>}
 */
async function syncOperation(operation) {
  const response = await fetch('/api/rdf/sync', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(operation)
  });

  if (!response.ok) {
    throw new Error(`Sync failed: ${response.statusText}`);
  }

  return response;
}

/**
 * Clear operation queue
 *
 * @param {IDBDatabase} db - Database connection
 * @returns {Promise<void>}
 */
function clearQueue(db) {
  return new Promise((resolve, reject) => {
    const transaction = db.transaction(['queue'], 'readwrite');
    const store = transaction.objectStore('queue');
    const request = store.clear();

    request.onsuccess = () => resolve();
    request.onerror = () => reject(request.error);
  });
}
