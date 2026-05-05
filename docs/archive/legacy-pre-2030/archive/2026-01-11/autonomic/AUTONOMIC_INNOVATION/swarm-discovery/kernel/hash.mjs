/**
 * @file hash.mjs - Deterministic hashing for receipts and provenance
 * @description SHA256 hashing with canonical JSON support
 * @invariant Q1: hash(data) is deterministic and reproducible
 * @invariant Q2: evidenceHash links all atoms to source code
 */

import crypto from 'crypto'

/**
 * Compute deterministic SHA256 hash of any JSON-serializable object
 * @param {any} data - Data to hash
 * @returns {string} - Hex-encoded SHA256 digest
 */
export function hashData(data) {
  const json = JSON.stringify(data, Object.keys(data).sort())
  return crypto.createHash('sha256').update(json).digest('hex')
}

/**
 * Compute hash of file content
 * @param {string} content - File content
 * @returns {string} - Hex-encoded SHA256 digest
 */
export function hashContent(content) {
  return crypto.createHash('sha256').update(content).digest('hex')
}

/**
 * Compute combined hash from multiple inputs
 * @param {string[]} hashes - Array of hashes
 * @returns {string} - Combined hash
 */
export function combineHashes(hashes) {
  const combined = hashes.sort().join('')
  return crypto.createHash('sha256').update(combined).digest('hex')
}

/**
 * Create evidence hash linking to source
 * @param {string} filePath - Source file path
 * @param {number} lineNumber - Line number
 * @param {string} contentHash - Hash of content at that line
 * @returns {string} - Evidence hash
 */
export function evidenceHash(filePath, lineNumber, contentHash) {
  const evidence = `${filePath}:${lineNumber}#${contentHash}`
  return hashData(evidence)
}
