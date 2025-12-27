/**
 * @file Mismatch detection and reporting with deterministic hashing
 * @description Generate structured mismatch reports with SHA-256 hashing for deduplication
 */

import { createHash } from 'crypto';

/**
 * @typedef {Object} MismatchReport
 * @property {string} mismatchHash - SHA-256 hash of the mismatch
 * @property {bigint} timestamp - Timestamp in nanoseconds
 * @property {string[]} path - JSON path where difference occurs
 * @property {*} legacyValue - Value from legacy system
 * @property {*} facadeValue - Value from facade system
 * @property {'critical'|'warning'|'info'} severity - Severity level
 * @property {string} recommendation - Suggested fix
 * @property {Object} [context] - Optional context information
 */

/**
 * Generate a structured mismatch report from two differing results
 *
 * @param {*} legacyResult - Result from legacy system
 * @param {*} facadeResult - Result from facade system
 * @param {Object} [context] - Optional context (requestId, endpoint, etc.)
 * @returns {MismatchReport} Structured mismatch report
 */
export function mismatchReport(legacyResult, facadeResult, context = {}) {
  // Find first difference path
  const diffPath = findDifferencePath(legacyResult, facadeResult);

  // Get values at difference path
  const legacyValue = getValueAtPath(legacyResult, diffPath);
  const facadeValue = getValueAtPath(facadeResult, diffPath);

  // Classify severity
  const severity = classifySeverity(legacyValue, facadeValue, diffPath);

  // Generate recommendation
  const recommendation = generateRecommendation(legacyValue, facadeValue, diffPath, severity);

  // Create base report
  const report = {
    timestamp: process.hrtime.bigint(),
    path: diffPath,
    legacyValue,
    facadeValue,
    severity,
    recommendation,
    ...(Object.keys(context).length > 0 && { context })
  };

  // Compute deterministic hash
  const hash = hashMismatchReport(report);

  return {
    mismatchHash: hash,
    ...report
  };
}

/**
 * Canonicalize mismatch report for deterministic hashing
 * Sorts all keys recursively and removes timestamp
 *
 * @param {MismatchReport} report - Mismatch report
 * @returns {string} Canonical JSON representation
 */
export function canonicalizeMismatchReport(report) {
  // Remove timestamp for deterministic hashing (same mismatch = same hash regardless of when)
  const { timestamp, mismatchHash, ...canonical } = report;

  return JSON.stringify(canonical, Object.keys(canonical).sort(), 0);
}

/**
 * Compute SHA-256 hash of mismatch report
 *
 * @param {MismatchReport} report - Mismatch report
 * @returns {string} SHA-256 hash as hex string
 */
export function hashMismatchReport(report) {
  const canonical = canonicalizeMismatchReport(report);
  return createHash('sha256').update(canonical).digest('hex');
}

/**
 * Find the path to the first difference between two values
 *
 * @param {*} a - First value
 * @param {*} b - Second value
 * @param {string[]} currentPath - Current path (for recursion)
 * @returns {string[]} Path to first difference
 */
function findDifferencePath(a, b, currentPath = []) {
  // If values are equal, no difference
  if (deepEqual(a, b)) return [];

  // If types differ, difference is at current path
  if (typeof a !== typeof b || (a == null) !== (b == null)) {
    return currentPath;
  }

  // If both are arrays
  if (Array.isArray(a) && Array.isArray(b)) {
    // Different lengths
    if (a.length !== b.length) {
      return [...currentPath, 'length'];
    }

    // Find first differing element
    for (let i = 0; i < a.length; i++) {
      if (!deepEqual(a[i], b[i])) {
        return findDifferencePath(a[i], b[i], [...currentPath, String(i)]);
      }
    }
  }

  // If both are objects
  if (typeof a === 'object' && a !== null && typeof b === 'object' && b !== null) {
    const keysA = Object.keys(a).sort();
    const keysB = Object.keys(b).sort();

    // Different keys
    if (keysA.length !== keysB.length || !deepEqual(keysA, keysB)) {
      const missingInB = keysA.find(k => !keysB.includes(k));
      const missingInA = keysB.find(k => !keysA.includes(k));

      if (missingInB) return [...currentPath, missingInB];
      if (missingInA) return [...currentPath, missingInA];
    }

    // Find first differing value
    for (const key of keysA) {
      if (!deepEqual(a[key], b[key])) {
        return findDifferencePath(a[key], b[key], [...currentPath, key]);
      }
    }
  }

  // Primitive difference at current path
  return currentPath;
}

/**
 * Get value at a given path in an object/array
 *
 * @param {*} obj - Object or array
 * @param {string[]} path - Path to value
 * @returns {*} Value at path, or undefined if not found
 */
function getValueAtPath(obj, path) {
  let current = obj;

  for (const segment of path) {
    if (current == null) return undefined;
    current = current[segment];
  }

  return current;
}

/**
 * Classify mismatch severity based on values and path
 *
 * @param {*} legacyValue - Legacy value
 * @param {*} facadeValue - Facade value
 * @param {string[]} path - Difference path
 * @returns {'critical'|'warning'|'info'} Severity level
 */
function classifySeverity(legacyValue, facadeValue, path) {
  // Null/undefined mismatches are critical
  if ((legacyValue == null) !== (facadeValue == null)) {
    return 'critical';
  }

  // Type mismatches are critical
  if (typeof legacyValue !== typeof facadeValue) {
    return 'critical';
  }

  // Missing required fields (id, type, etc.) are critical
  const criticalFields = ['id', 'type', 'status', 'error', 'success'];
  if (path.some(segment => criticalFields.includes(segment.toLowerCase()))) {
    return 'critical';
  }

  // String case differences are warnings
  if (typeof legacyValue === 'string' && typeof facadeValue === 'string') {
    if (legacyValue.toLowerCase() === facadeValue.toLowerCase()) {
      return 'info';
    }
  }

  // Number precision differences are warnings
  if (typeof legacyValue === 'number' && typeof facadeValue === 'number') {
    const diff = Math.abs(legacyValue - facadeValue);
    if (diff < 0.0001) return 'info';
  }

  // Date/timestamp fields
  if (path.some(segment => segment.toLowerCase().includes('date') ||
                           segment.toLowerCase().includes('time'))) {
    return 'warning';
  }

  // Metadata differences are info
  const metadataFields = ['metadata', 'meta', 'timestamp', 'created', 'updated'];
  if (path.some(segment => metadataFields.includes(segment.toLowerCase()))) {
    return 'info';
  }

  // Default to warning
  return 'warning';
}

/**
 * Generate recommendation for fixing mismatch
 *
 * @param {*} legacyValue - Legacy value
 * @param {*} facadeValue - Facade value
 * @param {string[]} path - Difference path
 * @param {string} severity - Severity level
 * @returns {string} Recommendation text
 */
function generateRecommendation(legacyValue, facadeValue, path, severity) {
  const pathStr = path.join('.');

  // Null/undefined
  if ((legacyValue == null) !== (facadeValue == null)) {
    return `Ensure ${pathStr} is properly initialized in facade`;
  }

  // Type mismatch
  if (typeof legacyValue !== typeof facadeValue) {
    return `Convert ${pathStr} to ${typeof legacyValue} in facade (currently ${typeof facadeValue})`;
  }

  // String case
  if (typeof legacyValue === 'string' && typeof facadeValue === 'string') {
    if (legacyValue.toLowerCase() === facadeValue.toLowerCase()) {
      return `Normalize case for ${pathStr} in facade`;
    }
    return `Verify string transformation logic for ${pathStr}`;
  }

  // Number precision
  if (typeof legacyValue === 'number' && typeof facadeValue === 'number') {
    return `Check numeric precision/rounding for ${pathStr}`;
  }

  // Date/timestamp
  if (path.some(s => s.toLowerCase().includes('date') || s.toLowerCase().includes('time'))) {
    return `Verify date format/timezone handling for ${pathStr}`;
  }

  // Array length
  if (path[path.length - 1] === 'length') {
    return `Verify array population logic for ${path.slice(0, -1).join('.')}`;
  }

  // Generic
  return `Review data transformation logic for ${pathStr}`;
}

/**
 * Deep equality check (same as shadow.mjs)
 *
 * @param {*} a - First value
 * @param {*} b - Second value
 * @returns {boolean} True if deeply equal
 */
function deepEqual(a, b) {
  if (a === b) return true;
  if (a == null || b == null) return false;
  if (typeof a !== typeof b) return false;

  if (a instanceof Date && b instanceof Date) {
    return a.getTime() === b.getTime();
  }

  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) {
      if (!deepEqual(a[i], b[i])) return false;
    }
    return true;
  }

  if (typeof a === 'object' && typeof b === 'object') {
    const keysA = Object.keys(a).sort();
    const keysB = Object.keys(b).sort();

    if (keysA.length !== keysB.length) return false;
    if (!deepEqual(keysA, keysB)) return false;

    for (const key of keysA) {
      if (!deepEqual(a[key], b[key])) return false;
    }
    return true;
  }

  return false;
}
