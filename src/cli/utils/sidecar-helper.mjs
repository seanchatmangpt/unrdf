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
    await sidecarClient.connect(options.address);
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
    throw new Error(`Sidecar operation failed: ${error.message}`);
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
  if (error.code === 14) {
    return 'Sidecar unavailable. Ensure sidecar is running: unrdf sidecar start';
  }
  if (error.code === 4) {
    return `Sidecar error: ${error.message}`;
  }
  return error.message;
}
