/**
 * @fileoverview Cryptographic hashing utilities using Node.js built-in crypto.
 * Provides SHA-256 hashing with deterministic outputs for proof chains.
 */

import { createHash } from 'node:crypto';

/**
 * Compute SHA-256 hash of data.
 *
 * @param {string | Buffer} data - Data to hash
 * @returns {Buffer} Hash as Buffer
 * @throws {Error} If data is invalid
 */
export function sha256(data) {
  if (data === null || data === undefined) {
    throw new Error('Cannot hash null or undefined data');
  }

  const hash = createHash('sha256');
  hash.update(data);
  return hash.digest();
}

/**
 * Compute SHA-256 hash and return as hex string.
 *
 * @param {string | Buffer} data - Data to hash
 * @returns {string} Hash as hex string (64 chars)
 * @throws {Error} If data is invalid
 */
export function sha256Hex(data) {
  if (data === null || data === undefined) {
    throw new Error('Cannot hash null or undefined data');
  }

  const hash = createHash('sha256');
  hash.update(data);
  return hash.digest('hex');
}

/**
 * Compute sequential hash chain over array of items.
 * Each hash includes the previous hash, creating a tamper-evident chain.
 *
 * @param {Array<string | Buffer>} items - Items to chain hash
 * @returns {Array<string>} Array of hex hash strings
 * @throws {Error} If items is not an array or is empty
 */
export function hashChain(items) {
  if (!Array.isArray(items)) {
    throw new Error('hashChain requires an array of items');
  }

  if (items.length === 0) {
    return [];
  }

  const hashes = [];
  let previousHash = '';

  for (const item of items) {
    if (item === null || item === undefined) {
      throw new Error('Cannot hash null or undefined item in chain');
    }

    // Combine previous hash with current item for chaining
    const combined = previousHash + (Buffer.isBuffer(item) ? item.toString('utf8') : item);
    const hash = sha256Hex(combined);
    hashes.push(hash);
    previousHash = hash;
  }

  return hashes;
}

/**
 * Compute hash with SHA-256 prefix for identification.
 *
 * @param {string | Buffer} data - Data to hash
 * @returns {string} Hash with 'sha256:' prefix
 */
export function sha256Prefixed(data) {
  return `sha256:${sha256Hex(data)}`;
}

/**
 * Verify if a hash matches the expected format.
 *
 * @param {string} hash - Hash to verify
 * @returns {boolean} True if valid SHA-256 hex format
 */
export function isValidSha256Hex(hash) {
  if (typeof hash !== 'string') {
    return false;
  }

  // SHA-256 hex should be exactly 64 characters
  return /^[a-f0-9]{64}$/i.test(hash);
}

/**
 * Verify if a prefixed hash matches the expected format.
 *
 * @param {string} hash - Hash to verify
 * @returns {boolean} True if valid 'sha256:' prefixed format
 */
export function isValidSha256Prefixed(hash) {
  if (typeof hash !== 'string') {
    return false;
  }

  return /^sha256:[a-f0-9]{64}$/i.test(hash);
}
