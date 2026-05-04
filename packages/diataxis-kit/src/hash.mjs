/**
 * @file Hashing utilities using stable JSON stringification
 * @module hash
 */

import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { stableStringify } from './stable-json.mjs';

/**
 * Hash an object deterministically using SHA256
 * @param {any} obj - Object to hash
 * @returns {string} Hex-encoded SHA256 hash
 */
export function hashObject(obj) {
  const json = stableStringify(obj, { indent: 0 });
  return hashString(json);
}

/**
 * Hash a string using SHA256
 * @param {string} str - String to hash
 * @returns {string} Hex-encoded SHA256 hash
 */
export function hashString(str) {
  return createHash('sha256').update(str, 'utf8').digest('hex');
}

/**
 * Hash a file's contents using SHA256
 * @param {string} filePath - Path to file
 * @returns {Promise<string>} Hex-encoded SHA256 hash
 */
export async function hashFile(filePath) {
  const content = await readFile(filePath, 'utf8');
  return hashString(content);
}
