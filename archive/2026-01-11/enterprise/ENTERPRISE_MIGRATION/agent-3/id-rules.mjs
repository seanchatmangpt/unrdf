/**
 * @fileoverview ID Generation and Validation Rules
 * Deterministic ID generation using hash-based approach.
 * Format: {type}_{timestamp}_{hash8}
 */

import { createHash } from 'crypto';

/**
 * @typedef {'receipt' | 'capsule' | 'domain' | 'lens' | 'migration'} IDType
 */

/**
 * ID type patterns
 * @type {Record<IDType, RegExp>}
 */
export const ID_PATTERNS = {
  receipt: /^receipt_\d{10}_[a-f0-9]{8}$/,
  capsule: /^capsule_\d{10}_[a-f0-9]{8}$/,
  domain: /^domain_\d{10}_[a-f0-9]{8}$/,
  lens: /^lens_\d{10}_[a-f0-9]{8}$/,
  migration: /^migration_\d{10}_[a-f0-9]{8}$/,
};

/**
 * Generate deterministic 8-character hash from input
 * @param {string} input - Input string to hash
 * @returns {string} 8-character hex hash
 */
function generateHash8(input) {
  const hash = createHash('sha256').update(input).digest('hex');
  return hash.substring(0, 8);
}

/**
 * Generate deterministic ID for given type and inputs
 * @param {IDType} type - Type of ID
 * @param {object} inputs - Input data for deterministic generation
 * @param {number} [inputs.timestamp] - Unix timestamp (defaults to current time)
 * @param {string} inputs.data - Data to hash for uniqueness
 * @returns {string} Generated ID in format: {type}_{timestamp}_{hash8}
 */
export function generateId(type, inputs) {
  if (!ID_PATTERNS[type]) {
    throw new Error(`Invalid ID type: ${type}`);
  }

  const timestamp = inputs.timestamp || Math.floor(Date.now() / 1000);
  const hashInput = `${type}:${timestamp}:${inputs.data}`;
  const hash = generateHash8(hashInput);

  return `${type}_${timestamp}_${hash}`;
}

/**
 * Generate receipt ID
 * @param {object} params - Receipt parameters
 * @param {string} params.capsuleId - Capsule ID
 * @param {string} params.domainId - Domain ID
 * @param {string} params.operation - Operation type
 * @param {number} [params.timestamp] - Optional timestamp
 * @returns {string} Receipt ID
 */
export function generateReceiptId(params) {
  const data = `${params.capsuleId}:${params.domainId}:${params.operation}`;
  return generateId('receipt', { data, timestamp: params.timestamp });
}

/**
 * Generate capsule ID
 * @param {object} params - Capsule parameters
 * @param {string} params.content - Capsule content hash
 * @param {string} params.schemaVersion - Schema version
 * @param {number} [params.timestamp] - Optional timestamp
 * @returns {string} Capsule ID
 */
export function generateCapsuleId(params) {
  const data = `${params.content}:${params.schemaVersion}`;
  return generateId('capsule', { data, timestamp: params.timestamp });
}

/**
 * Generate domain ID
 * @param {object} params - Domain parameters
 * @param {string} params.name - Domain name
 * @param {string} params.namespace - Namespace
 * @param {number} [params.timestamp] - Optional timestamp
 * @returns {string} Domain ID
 */
export function generateDomainId(params) {
  const data = `${params.namespace}:${params.name}`;
  return generateId('domain', { data, timestamp: params.timestamp });
}

/**
 * Generate lens ID
 * @param {object} params - Lens parameters
 * @param {string} params.sourceDomain - Source domain ID
 * @param {string} params.targetDomain - Target domain ID
 * @param {string} params.transformType - Transform type
 * @param {number} [params.timestamp] - Optional timestamp
 * @returns {string} Lens ID
 */
export function generateLensId(params) {
  const data = `${params.sourceDomain}:${params.targetDomain}:${params.transformType}`;
  return generateId('lens', { data, timestamp: params.timestamp });
}

/**
 * Generate migration ID
 * @param {object} params - Migration parameters
 * @param {string} params.sourceSystem - Source system identifier
 * @param {string} params.targetSystem - Target system identifier
 * @param {string} params.batchId - Batch identifier
 * @param {number} [params.timestamp] - Optional timestamp
 * @returns {string} Migration ID
 */
export function generateMigrationId(params) {
  const data = `${params.sourceSystem}:${params.targetSystem}:${params.batchId}`;
  return generateId('migration', { data, timestamp: params.timestamp });
}

/**
 * Validate ID format
 * @param {string} id - ID to validate
 * @param {IDType} expectedType - Expected ID type
 * @returns {boolean} True if valid
 */
export function validateId(id, expectedType) {
  if (typeof id !== 'string') {
    return false;
  }

  const pattern = ID_PATTERNS[expectedType];
  if (!pattern) {
    throw new Error(`Invalid ID type: ${expectedType}`);
  }

  return pattern.test(id);
}

/**
 * Parse ID into components
 * @param {string} id - ID to parse
 * @returns {{type: string, timestamp: number, hash: string} | null} Parsed components or null if invalid
 */
export function parseId(id) {
  if (typeof id !== 'string') {
    return null;
  }

  const parts = id.split('_');
  if (parts.length !== 3) {
    return null;
  }

  const [type, timestampStr, hash] = parts;
  const timestamp = parseInt(timestampStr, 10);

  if (isNaN(timestamp) || !/^[a-f0-9]{8}$/.test(hash)) {
    return null;
  }

  return { type, timestamp, hash };
}

/**
 * Extract timestamp from ID
 * @param {string} id - ID to extract from
 * @returns {number | null} Unix timestamp or null if invalid
 */
export function extractTimestamp(id) {
  const parsed = parseId(id);
  return parsed ? parsed.timestamp : null;
}

/**
 * Verify ID matches expected data
 * @param {string} id - ID to verify
 * @param {IDType} type - Expected type
 * @param {string} data - Expected data
 * @returns {boolean} True if ID matches expected generation
 */
export function verifyId(id, type, data) {
  const parsed = parseId(id);
  if (!parsed || parsed.type !== type) {
    return false;
  }

  const hashInput = `${type}:${parsed.timestamp}:${data}`;
  const expectedHash = generateHash8(hashInput);

  return parsed.hash === expectedHash;
}
