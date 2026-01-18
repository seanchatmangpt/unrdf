/**
 * @file v6 DeltaGate Helpers
 * @module @unrdf/daemon/integrations/v6-deltagate-helpers
 * @description Helper utility functions for delta operations
 */

import { createHash, randomUUID } from 'node:crypto';

/**
 * Generate SHA256 hash of data
 * @param {*} data - Data to hash
 * @returns {string} 64-character hex hash
 */
export function hashData(data) {
  let str;
  if (typeof data === 'string') {
    str = data;
  } else {
    // Handle BigInt serialization
    str = JSON.stringify(data, (key, value) =>
      typeof value === 'bigint' ? value.toString() : value
    );
  }
  return createHash('sha256').update(str).digest('hex');
}

/**
 * Generate UUID v4
 * @returns {string} Valid UUID v4
 */
export function generateUUID() {
  return randomUUID();
}

/**
 * Get nanosecond timestamp
 * @returns {bigint} Nanosecond timestamp
 */
export function getNs() {
  return BigInt(Date.now()) * 1_000_000n;
}

/**
 * Get ISO timestamp
 * @returns {string} ISO 8601 timestamp
 */
export function getISOTimestamp() {
  return new Date().toISOString();
}
