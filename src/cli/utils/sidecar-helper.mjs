/**
 * @file Sidecar Helper Utilities
 * @module cli/utils/sidecar-helper
 *
 * @description
 * Shared utilities for integrating with KGC sidecar client across CLI commands.
 */

import { SidecarClient } from '../../sidecar/client.mjs';

/**
 * Global sidecar client instance (singleton pattern)
 */
let sidecarClient = null;

/**
 * Get or create sidecar client
 * @param {Object} [options] - Client options
 * @returns {Promise<SidecarClient>} Sidecar client instance
 */
export async function getSidecarClient(options = {}) {
  if (!sidecarClient || !sidecarClient.connected) {
    sidecarClient = new SidecarClient(options);

    // Add timeout to prevent hanging indefinitely (default 3 seconds)
    const timeout = options.timeout || 3000;
    const connectPromise = sidecarClient.connect(options.address);
    const timeoutPromise = new Promise((_, reject) =>
      setTimeout(() => {
        const error = new Error('Connection timeout');
        error.code = 4; // DEADLINE_EXCEEDED
        reject(error);
      }, timeout)
    );

    await Promise.race([connectPromise, timeoutPromise]);
  }
  return sidecarClient;
}

/**
 * Close sidecar client
 * @returns {Promise<void>}
 */
export async function closeSidecarClient() {
  if (sidecarClient && sidecarClient.connected) {
    await sidecarClient.disconnect();
    sidecarClient = null;
  }
}

/**
 * Execute with sidecar client
 * @param {Function} fn - Function to execute with client
 * @param {Object} [options] - Client options
 * @returns {Promise<any>} Function result
 */
export async function withSidecar(fn, options = {}) {
  try {
    const client = await getSidecarClient(options);
    return await fn(client);
  } catch (error) {
    // Preserve gRPC error codes for graceful degradation
    const wrappedError = new Error(`Sidecar operation failed: ${error.message}`);
    wrappedError.code = error.code; // Preserve error code (14=UNAVAILABLE, 4=DEADLINE_EXCEEDED)
    wrappedError.originalError = error;
    throw wrappedError;
  }
}

/**
 * Check if sidecar is available
 * @param {Object} [options] - Client options
 * @returns {Promise<boolean>} True if sidecar is available
 */
export async function isSidecarAvailable(options = {}) {
  try {
    const client = await getSidecarClient(options);
    const response = await client.healthCheck();
    return response.status === 'SERVING';
  } catch (error) {
    return false;
  }
}

/**
 * Format sidecar error for CLI display
 * @param {Error} error - Error object
 * @returns {string} Formatted error message
 */
export function formatSidecarError(error) {
  // gRPC error code 14 = UNAVAILABLE (connection failed)
  if (error.code === 14) {
    return 'Sidecar unavailable. Ensure sidecar is running: unrdf sidecar start';
  }

  // gRPC error code 4 = DEADLINE_EXCEEDED (timeout)
  if (error.code === 4) {
    return 'Sidecar unavailable (timeout). Ensure sidecar is running: unrdf sidecar start';
  }

  // gRPC error code 12 = UNIMPLEMENTED (method not supported)
  if (error.code === 12) {
    return `Sidecar error: Method not implemented - ${error.message}`;
  }

  // gRPC error code 16 = UNAUTHENTICATED
  if (error.code === 16) {
    return `Sidecar error: Authentication required - ${error.message}`;
  }

  // Generic gRPC error
  if (error.code) {
    return `Sidecar error (code ${error.code}): ${error.message}`;
  }

  // Non-gRPC error
  return error.message;
}
