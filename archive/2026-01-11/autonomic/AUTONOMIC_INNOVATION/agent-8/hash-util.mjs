/**
 * Hash Utility - Deterministic hashing using Node.js crypto
 * Fallback for hash-wasm when not available
 */

import { createHash } from 'node:crypto';

/**
 * Hash string using SHA-256 (deterministic)
 * @param {string} data - Data to hash
 * @returns {Promise<string>} Hex-encoded hash
 */
export async function blake3(data) {
  // Use SHA-256 as fallback (blake3 not available without hash-wasm)
  const hash = createHash('sha256');
  hash.update(data);
  return hash.digest('hex');
}
