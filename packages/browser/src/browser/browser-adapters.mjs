/* eslint-disable no-undef */
/**
 * @fileoverview Browser Adapters
 *
 * Provides browser-specific adaptations for RDF operations.
 * Handles environment detection, storage selection, and SPARQL execution.
 *
 * @module @unrdf/browser/browser-adapters
 */

import { z } from 'zod';
import { createStore } from '@unrdf/core';
import { createIndexedDBStore, openIndexedDBStore } from './indexeddb-store.mjs';

/**
 * Check if running in browser environment
 *
 * @returns {boolean} True if in browser
 *
 * @example
 * if (isBrowserEnvironment()) {
 *   // Use browser-specific features
 * }
 */
export function isBrowserEnvironment() {
  return (
    typeof window !== 'undefined' &&
    typeof window.document !== 'undefined'
  );
}

/**
 * Get appropriate storage adapter for current environment
 *
 * @returns {'indexeddb'|'memory'} Storage type
 *
 * @example
 * const storageType = getStorageAdapter();
 * console.log(`Using ${storageType} storage`);
 */
export function getStorageAdapter() {
  if (!isBrowserEnvironment()) {
    return 'memory';
  }

  // Check if IndexedDB is available
  if (typeof indexedDB !== 'undefined') {
    return 'indexeddb';
  }

  // Fallback to memory
  return 'memory';
}

/**
 * Create browser-optimized RDF store
 * Uses IndexedDB if available, falls back to memory store
 *
 * @param {Object} [options] - Store options
 * @param {string} [options.dbName='unrdf'] - Database name
 * @param {string} [options.storeName='quads'] - Object store name
 * @param {boolean} [options.persistent=true] - Enable persistence
 * @returns {Promise<Object>} Store instance
 *
 * @example
 * const store = await createBrowserRDFStore({ dbName: 'myapp-rdf' });
 */
export async function createBrowserRDFStore(options = {}) {
  const opts = z
    .object({
      dbName: z.string().default('unrdf'),
      storeName: z.string().default('quads'),
      persistent: z.boolean().default(true),
    })
    .parse(options);

  const storageType = getStorageAdapter();

  if (storageType === 'indexeddb' && opts.persistent) {
    const store = createIndexedDBStore(opts.dbName, opts.storeName);
    await openIndexedDBStore(store);
    return store;
  }

  // Fallback to memory store
  return createStore();
}

/**
 * Get browser-compatible Comunica SPARQL adapter
 * Returns configuration for running SPARQL queries in browser
 *
 * @returns {Object} Comunica configuration
 *
 * @example
 * const comunica = getBrowserComunicaAdapter();
 * // Use with @comunica/query-sparql
 */
export function getBrowserComunicaAdapter() {
  return {
    // Browser-safe Comunica configuration
    sources: [],
    lenient: true,
    // Disable Node.js-specific features
    httpProxyHandler: null,
    fetch: typeof fetch !== 'undefined' ? fetch : null,
  };
}

/**
 * Check if service worker is supported
 *
 * @returns {boolean} True if service workers are supported
 *
 * @example
 * if (isServiceWorkerSupported()) {
 *   await registerServiceWorker();
 * }
 */
export function isServiceWorkerSupported() {
  return (
    typeof navigator !== 'undefined' &&
    'serviceWorker' in navigator &&
    typeof ServiceWorkerRegistration !== 'undefined'
  );
}

/**
 * Check browser storage quota and usage
 *
 * @returns {Promise<Object>} Storage quota information
 * @property {number} usage - Current storage usage in bytes
 * @property {number} quota - Available storage quota in bytes
 * @property {number} percentUsed - Percentage of quota used
 *
 * @example
 * const quota = await checkStorageQuota();
 * console.log(`Using ${quota.percentUsed}% of available storage`);
 */
export async function checkStorageQuota() {
  if (!isBrowserEnvironment()) {
    return { usage: 0, quota: 0, percentUsed: 0 };
  }

  if (navigator.storage && navigator.storage.estimate) {
    const estimate = await navigator.storage.estimate();
    return {
      usage: estimate.usage || 0,
      quota: estimate.quota || 0,
      percentUsed: estimate.quota ? ((estimate.usage || 0) / estimate.quota) * 100 : 0,
    };
  }

  return { usage: 0, quota: 0, percentUsed: 0 };
}

/**
 * Request persistent storage permission
 *
 * @returns {Promise<boolean>} True if persistent storage is granted
 *
 * @example
 * const persistent = await requestPersistentStorage();
 * if (persistent) {
 *   console.log('Data will not be evicted');
 * }
 */
export async function requestPersistentStorage() {
  if (!isBrowserEnvironment()) {
    return false;
  }

  if (navigator.storage && navigator.storage.persist) {
    const isPersistent = await navigator.storage.persist();
    return isPersistent;
  }

  return false;
}

/**
 * Check if storage is persisted
 *
 * @returns {Promise<boolean>} True if storage is persisted
 *
 * @example
 * const persisted = await isStoragePersisted();
 */
export async function isStoragePersisted() {
  if (!isBrowserEnvironment()) {
    return false;
  }

  if (navigator.storage && navigator.storage.persisted) {
    return await navigator.storage.persisted();
  }

  return false;
}
