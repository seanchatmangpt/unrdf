/**
 * @unrdf/browser
 *
 * Browser SDK - Client-side RDF with IndexedDB Storage
 *
 * Provides:
 * - Persistent RDF storage via IndexedDB
 * - Browser-optimized RDF operations
 * - Offline support with service workers
 * - Storage quota management
 * - Import/export utilities
 *
 * @module @unrdf/browser
 */

// Export IndexedDB store operations
export {
  createIndexedDBStore,
  openIndexedDBStore,
  closeIndexedDBStore,
  addQuadToDB,
  removeQuadFromDB,
  getQuadsFromDB,
  clearIndexedDBStore,
} from './browser/indexeddb-store.mjs';

// Export browser adapters
export {
  createBrowserRDFStore,
  isBrowserEnvironment,
  getStorageAdapter,
  getBrowserComunicaAdapter,
  isServiceWorkerSupported,
  checkStorageQuota,
  requestPersistentStorage,
  isStoragePersisted,
} from './browser/browser-adapters.mjs';

// Export utilities
export {
  serializeQuadForStorage,
  deserializeQuad,
  calculateQuadSize,
  getStorageQuota,
  estimateCapacity,
  isStorageApproachingLimit,
  formatStorageSize,
  exportStoreToJSON,
  importStoreFromJSON,
} from './browser/utils.mjs';

// Export service worker support
export {
  registerServiceWorker,
  initOfflineSupport,
  sendMessageToServiceWorker,
  requestBackgroundSync,
} from './browser/service-worker.mjs';
