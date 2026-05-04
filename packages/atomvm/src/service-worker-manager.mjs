/**
 * Service Worker Manager
 *
 * Handles registration of coi-serviceworker to enable Cross-Origin-Isolation.
 * This is required for SharedArrayBuffer support in modern browsers.
 *
 * @module service-worker-manager
 */

/** @constant {number} Wait time (ms) for service worker auto-registration */
const SW_REGISTRATION_WAIT_DELAY_MS = 200;

/** @constant {number} Maximum time (ms) to wait for service worker installation */
const SW_INSTALLATION_TIMEOUT_MS = 5000;

/** @constant {number} Default timeout (ms) for waiting for Cross-Origin-Isolation */
const COI_WAIT_DEFAULT_TIMEOUT_MS = 5000;

/** @constant {number} Delay (ms) before reloading page to activate service worker */
const COI_RELOAD_DELAY_MS = 500;

/** @constant {number} Interval (ms) for polling Cross-Origin-Isolation status */
const COI_POLL_INTERVAL_MS = 100;

/** @constant {number} Size (bytes) of SharedArrayBuffer for availability test */
const SHARED_ARRAY_BUFFER_TEST_SIZE = 1;

/**
 * Register the coi-serviceworker
 *
 * @returns {Promise<boolean>} True if registration successful
 */
export async function registerServiceWorker() {
  if (!('serviceWorker' in navigator)) {
    console.warn('Service Workers not supported in this browser');
    return false;
  }

  try {
    // Load and register coi-serviceworker
    // The coi-serviceworker auto-registers when imported
    await import('coi-serviceworker');
    console.log('coi-serviceworker loaded');

    // Wait for service worker to be available
    // coi-serviceworker registers automatically, but we need to wait
    let registration = await navigator.serviceWorker.getRegistration();
    
    // If not registered yet, wait a bit for auto-registration
    if (!registration) {
      await new Promise(resolve => setTimeout(resolve, SW_REGISTRATION_WAIT_DELAY_MS));
      registration = await navigator.serviceWorker.getRegistration();
    }

    if (registration) {
      console.log('Service Worker registered:', registration.scope);

      // Wait for activation if installing or waiting
      if (registration.installing) {
        await new Promise((resolve, reject) => {
          const timeout = setTimeout(() => {
            reject(new Error('Service worker installation timeout'));
          }, SW_INSTALLATION_TIMEOUT_MS);

          registration.installing.addEventListener('statechange', (e) => {
            const state = e.target.state;
            if (state === 'activated') {
              clearTimeout(timeout);
              resolve();
            } else if (state === 'redundant') {
              clearTimeout(timeout);
              reject(new Error('Service worker installation failed'));
            }
          });
        });
      } else if (registration.waiting) {
        // If waiting, try to activate it
        registration.waiting.postMessage({ type: 'skipWaiting' });
        await new Promise((resolve) => {
          registration.waiting.addEventListener('statechange', (e) => {
            if (e.target.state === 'activated') {
              resolve();
            }
          });
        });
      }

      return true;
    }

    console.warn('Service worker registration not found after import');
    return false;

  } catch (error) {
    console.error('Service Worker registration error:', error);
    return false;
  }
}

/**
 * Check if Cross-Origin-Isolation is enabled
 *
 * @returns {boolean} True if COI is enabled
 */
export function checkCrossOriginIsolation() {
  // Check for crossOriginIsolated property
  if (typeof crossOriginIsolated !== 'undefined') {
    return crossOriginIsolated;
  }

  // Fallback: check SharedArrayBuffer availability
  if (typeof SharedArrayBuffer !== 'undefined') {
    try {
      new SharedArrayBuffer(SHARED_ARRAY_BUFFER_TEST_SIZE);
      return true;
    } catch (e) {
      return false;
    }
  }

  return false;
}

/**
 * Get current Cross-Origin-Isolation status details
 *
 * @returns {Object} Status object with details
 */
export function getCOIStatus() {
  return {
    crossOriginIsolated: typeof crossOriginIsolated !== 'undefined' ? crossOriginIsolated : false,
    sharedArrayBufferAvailable: typeof SharedArrayBuffer !== 'undefined',
    serviceWorkerSupported: 'serviceWorker' in navigator,
    headers: {
      coep: 'require-corp (via service worker)',
      coop: 'same-origin (via service worker)'
    }
  };
}

/**
 * Wait for Cross-Origin-Isolation to be enabled
 * Will trigger page reload if needed
 *
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<boolean>} True if COI is enabled
 */
export async function waitForCOI(timeout = COI_WAIT_DEFAULT_TIMEOUT_MS) {
  const startTime = Date.now();

  while (Date.now() - startTime < timeout) {
    if (checkCrossOriginIsolation()) {
      return true;
    }

    // If service worker is controlling but COI not yet active, reload
    if (navigator.serviceWorker.controller && !checkCrossOriginIsolation()) {
      console.log('Service worker active but COI not enabled, reloading...');
      // Small delay before reload to ensure service worker is ready
      await new Promise(resolve => setTimeout(resolve, COI_RELOAD_DELAY_MS));
      window.location.reload();
      return false;
    }

    await new Promise(resolve => setTimeout(resolve, COI_POLL_INTERVAL_MS));
  }

  const isIsolated = checkCrossOriginIsolation();
  if (!isIsolated && navigator.serviceWorker.controller) {
    // Last attempt: reload if service worker is controlling
    console.log('COI timeout, reloading to activate service worker...');
    window.location.reload();
  }

  return isIsolated;
}
