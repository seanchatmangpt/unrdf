/**
 * Service Worker Manager
 *
 * Handles registration of coi-serviceworker to enable Cross-Origin-Isolation.
 * This is required for SharedArrayBuffer support in modern browsers.
 *
 * @module service-worker-manager
 */

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
    const coiScript = await import('coi-serviceworker');

    // The coi-serviceworker auto-registers when imported
    // Just need to ensure it's loaded
    console.log('coi-serviceworker loaded');

    // Check if already registered
    const registration = await navigator.serviceWorker.getRegistration();

    if (registration) {
      console.log('Service Worker already registered:', registration);

      // Wait for activation if installing
      if (registration.installing) {
        await new Promise((resolve) => {
          registration.installing.addEventListener('statechange', (e) => {
            if (e.target.state === 'activated') {
              resolve();
            }
          });
        });
      }

      return true;
    }

    // If not registered, the coi-serviceworker script should handle it
    // Wait a bit for registration to complete
    await new Promise(resolve => setTimeout(resolve, 100));

    const newRegistration = await navigator.serviceWorker.getRegistration();
    return !!newRegistration;

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
      new SharedArrayBuffer(1);
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
export async function waitForCOI(timeout = 5000) {
  const startTime = Date.now();

  while (Date.now() - startTime < timeout) {
    if (checkCrossOriginIsolation()) {
      return true;
    }

    // If service worker is controlling but COI not yet active, reload
    if (navigator.serviceWorker.controller && !checkCrossOriginIsolation()) {
      console.log('Service worker active but COI not enabled, reloading...');
      window.location.reload();
      return false;
    }

    await new Promise(resolve => setTimeout(resolve, 100));
  }

  return checkCrossOriginIsolation();
}
