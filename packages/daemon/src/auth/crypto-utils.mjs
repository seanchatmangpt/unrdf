/**
 * @file Cryptographic Utilities for API Key Management
 * @module @unrdf/daemon/auth/crypto-utils
 * @description BLAKE3 hashing and secure random generation for API key operations.
 * Uses hash-wasm for high-performance BLAKE3 implementation.
 */

import { blake3 } from 'hash-wasm';
import crypto from 'crypto';

/**
 * Generate a secure random API key
 * @param {number} [length=32] - Byte length of the random key (default 32 bytes = 64 hex chars)
 * @returns {string} Hex-encoded random API key
 * @throws {RangeError} If length is not between 16 and 64 bytes
 * @example
 * const apiKey = generateSecureApiKey();
 * // Returns: "a1b2c3d4e5f6..." (64 hex characters)
 */
export function generateSecureApiKey(length = 32) {
  if (length < 16 || length > 64) {
    throw new RangeError('API key length must be between 16 and 64 bytes');
  }
  return crypto.randomBytes(length).toString('hex');
}

/**
 * Hash API key using BLAKE3
 * @param {string} apiKey - API key to hash
 * @returns {Promise<string>} BLAKE3 hash of the API key
 * @throws {TypeError} If apiKey is not a string
 * @example
 * const hash = await hashApiKey('my-secret-key');
 * // Returns: "c4d2e3f4a5b6..." (BLAKE3 hash)
 */
export async function hashApiKey(apiKey) {
  if (typeof apiKey !== 'string') {
    throw new TypeError('API key must be a string');
  }
  if (apiKey.length === 0) {
    throw new Error('API key cannot be empty');
  }
  return await blake3(apiKey);
}

/**
 * Verify API key against stored hash using constant-time comparison
 * @param {string} providedKey - API key provided by user
 * @param {string} storedHash - Stored BLAKE3 hash
 * @returns {Promise<boolean>} True if key matches hash
 * @throws {TypeError} If arguments are not strings
 * @example
 * const isValid = await verifyApiKey('my-key', storedHash);
 * if (isValid) {
 *   console.log('Authentication successful');
 * }
 */
export async function verifyApiKey(providedKey, storedHash) {
  if (typeof providedKey !== 'string' || typeof storedHash !== 'string') {
    throw new TypeError('API key and hash must be strings');
  }

  const providedHash = await hashApiKey(providedKey);

  // Constant-time comparison to prevent timing attacks
  return crypto.timingSafeEqual(
    Buffer.from(providedHash),
    Buffer.from(storedHash)
  );
}

/**
 * Generate API key and its hash together
 * @param {number} [length=32] - Byte length of the random key
 * @returns {Promise<{key: string, hash: string}>} Object with key and hash
 * @example
 * const { key, hash } = await generateApiKeyPair();
 * // Store hash in database
 * // Give key to user (only shown once)
 */
export async function generateApiKeyPair(length = 32) {
  const key = generateSecureApiKey(length);
  const hash = await hashApiKey(key);
  return { key, hash };
}
