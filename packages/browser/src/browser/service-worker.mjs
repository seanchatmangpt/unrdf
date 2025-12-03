/**
 * @unrdf/browser - Service Worker Support
 *
 * Provides offline RDF persistence and background sync via service workers.
 *
 * @module @unrdf/browser/service-worker
 */

import { z } from 'zod';

/**
 * Register service worker for offline RDF support
 *
 * @param {string} [scriptURL='/sw.js'] - Service worker script URL
 * @returns {Promise<ServiceWorkerRegistration|null>} Registration or null
 *
 * @example
 * const registration = await registerServiceWorker('/rdf-sw.js');
 */
export async function registerServiceWorker(scriptURL = '/sw.js') {
  const validatedURL = z.string().min(1).parse(scriptURL);

  if (typeof navigator === 'undefined' || !('serviceWorker' in navigator)) {
    console.warn('Service workers not supported');
    return null;
  }

  const registration = await navigator.serviceWorker.register(validatedURL);
  return registration;
}

/**
 * Initialize offline support for RDF store
 * Sets up service worker and IndexedDB sync
 *
 * @param {Object} [options] - Offline options
 * @param {string} [options.swURL='/sw.js'] - Service worker URL
 * @param {boolean} [options.autoSync=true] - Enable background sync
 * @returns {Promise<Object>} Offline configuration
 *
 * @example
 * const offline = await initOfflineSupport({ autoSync: true });
 */
export async function initOfflineSupport(options = {}) {
  const opts = z
    .object({
      swURL: z.string().default('/sw.js'),
      autoSync: z.boolean().default(true),
    })
    .parse(options);

  const registration = await registerServiceWorker(opts.swURL);

  if (!registration) {
    return { enabled: false };
  }

  // Setup message listener for SW communication
  if (opts.autoSync) {
    navigator.serviceWorker.addEventListener('message', event => {
      if (event.data.type === 'SYNC_RDF') {
        // Handle sync events
        console.log('RDF sync triggered from service worker');
      }
    });
  }

  return {
    enabled: true,
    registration,
    autoSync: opts.autoSync,
  };
}

/**
 * Send message to service worker
 *
 * @param {Object} message - Message to send
 * @returns {Promise<void>}
 *
 * @example
 * await sendMessageToServiceWorker({ type: 'CACHE_QUADS', quads: [...] });
 */
export async function sendMessageToServiceWorker(message) {
  if (
    typeof navigator === 'undefined' ||
    !navigator.serviceWorker ||
    !navigator.serviceWorker.controller
  ) {
    throw new Error('No active service worker');
  }

  navigator.serviceWorker.controller.postMessage(message);
}

/**
 * Request background sync for RDF data
 *
 * @param {string} [tag='rdf-sync'] - Sync tag
 * @returns {Promise<void>}
 *
 * @example
 * await requestBackgroundSync('rdf-sync');
 */
export async function requestBackgroundSync(tag = 'rdf-sync') {
  z.string().min(1).parse(tag);

  if (typeof navigator === 'undefined' || !navigator.serviceWorker) {
    throw new Error('Service worker not ready');
  }

  const registration = await navigator.serviceWorker.ready;

  if ('sync' in registration) {
    await registration.sync.register(tag);
  }
}
