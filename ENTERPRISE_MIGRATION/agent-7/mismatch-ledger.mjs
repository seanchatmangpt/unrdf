/**
 * Mismatch Ledger - Track and analyze mismatches between legacy and substrate
 * @module agent-7/mismatch-ledger
 */

import { createHash } from 'crypto';

/**
 * In-memory ledger of mismatches
 * @type {Array<Object>}
 */
const ledger = [];

/**
 * Ledger metadata
 * @type {Object}
 */
const metadata = {
  createdAt: Date.now(),
  lastUpdated: Date.now(),
  version: '1.0.0',
};

/**
 * Generate unique mismatch ID
 * @param {number} timestamp - Timestamp
 * @param {string} operation - Operation identifier
 * @returns {string} Unique mismatch ID
 */
function generateMismatchId(timestamp, operation) {
  const random = Math.random().toString(36).substring(2, 10);
  const hash = createHash('md5')
    .update(`${timestamp}_${operation}_${random}`)
    .digest('hex')
    .substring(0, 8);

  return `mismatch_${timestamp}_${hash}`;
}

/**
 * Record a mismatch between legacy and substrate results
 * @param {string} operation - Operation identifier
 * @param {*} legacyResult - Legacy system result
 * @param {*} substrateResult - Substrate system result
 * @param {Object} diff - Difference object from comparison
 * @returns {Object} Recorded mismatch entry
 */
export function recordMismatch(operation, legacyResult, substrateResult, diff) {
  if (!operation || typeof operation !== 'string') {
    throw new Error('Operation must be a non-empty string');
  }

  if (!diff || typeof diff !== 'object') {
    throw new Error('Diff must be an object');
  }

  const timestamp = Date.now();

  const entry = {
    id: generateMismatchId(timestamp, operation),
    timestamp,
    operation,
    legacyResult,
    substrateResult,
    diff,
  };

  ledger.push(entry);
  metadata.lastUpdated = timestamp;

  return entry;
}

/**
 * Get mismatches with optional filtering
 * @param {Object} filter - Filter criteria
 * @param {string} [filter.operation] - Filter by operation
 * @param {string} [filter.diffType] - Filter by diff type
 * @param {number} [filter.since] - Filter by timestamp (ms since epoch)
 * @param {number} [filter.limit] - Limit number of results
 * @returns {Array<Object>} Filtered mismatch entries
 */
export function getMismatches(filter = {}) {
  let results = [...ledger];

  // Filter by operation
  if (filter.operation) {
    results = results.filter((entry) => entry.operation === filter.operation);
  }

  // Filter by diff type
  if (filter.diffType) {
    results = results.filter((entry) => entry.diff.type === filter.diffType);
  }

  // Filter by timestamp
  if (filter.since) {
    results = results.filter((entry) => entry.timestamp >= filter.since);
  }

  // Sort by timestamp (newest first)
  results.sort((a, b) => b.timestamp - a.timestamp);

  // Apply limit
  if (filter.limit && filter.limit > 0) {
    results = results.slice(0, filter.limit);
  }

  return results;
}

/**
 * Export ledger as stable JSON with hash
 * @returns {Object} Ledger export with metadata and hash
 */
export function exportLedger() {
  // Sort ledger by timestamp for deterministic output
  const sortedLedger = [...ledger].sort((a, b) => a.timestamp - b.timestamp);

  // Create stable export
  const exportData = {
    metadata: {
      ...metadata,
      exportedAt: Date.now(),
      entryCount: sortedLedger.length,
    },
    entries: sortedLedger,
  };

  // Calculate deterministic hash
  const jsonStr = JSON.stringify(exportData, null, 2);
  const hash = createHash('sha256').update(jsonStr).digest('hex');

  return {
    ...exportData,
    hash,
  };
}

/**
 * Clear all entries from ledger
 * @returns {Object} Result object
 */
export function clearLedger() {
  const count = ledger.length;
  ledger.length = 0;

  metadata.lastUpdated = Date.now();

  return {
    success: true,
    clearedCount: count,
    timestamp: Date.now(),
  };
}

/**
 * Get ledger statistics
 * @returns {Object} Statistics about ledger
 */
export function getLedgerStats() {
  const stats = {
    totalEntries: ledger.length,
    byOperation: {},
    byDiffType: {},
    timeRange: {
      oldest: null,
      newest: null,
    },
  };

  if (ledger.length === 0) {
    return stats;
  }

  // Count by operation and diff type
  for (const entry of ledger) {
    // By operation
    stats.byOperation[entry.operation] = (stats.byOperation[entry.operation] || 0) + 1;

    // By diff type
    const diffType = entry.diff.type || 'UNKNOWN';
    stats.byDiffType[diffType] = (stats.byDiffType[diffType] || 0) + 1;
  }

  // Time range
  const timestamps = ledger.map((e) => e.timestamp);
  stats.timeRange.oldest = Math.min(...timestamps);
  stats.timeRange.newest = Math.max(...timestamps);

  return stats;
}

/**
 * Get recent mismatches for an operation
 * @param {string} operation - Operation identifier
 * @param {number} count - Number of recent entries to return
 * @returns {Array<Object>} Recent mismatch entries
 */
export function getRecentMismatches(operation, count = 10) {
  return getMismatches({ operation, limit: count });
}

/**
 * Import ledger from export
 * @param {Object} exportData - Previously exported ledger data
 * @returns {Object} Result object
 */
export function importLedger(exportData) {
  if (!exportData || typeof exportData !== 'object') {
    throw new Error('Export data must be an object');
  }

  if (!Array.isArray(exportData.entries)) {
    throw new Error('Export data must contain entries array');
  }

  // Verify hash if present
  if (exportData.hash) {
    const { hash, ...dataWithoutHash } = exportData;
    const jsonStr = JSON.stringify(dataWithoutHash, null, 2);
    const calculatedHash = createHash('sha256').update(jsonStr).digest('hex');

    if (calculatedHash !== hash) {
      throw new Error('Hash mismatch - data may be corrupted');
    }
  }

  // Clear existing ledger
  ledger.length = 0;

  // Import entries
  let imported = 0;
  const errors = [];

  for (const entry of exportData.entries) {
    try {
      // Validate entry structure
      if (!entry.id || !entry.operation || !entry.diff) {
        throw new Error('Invalid entry structure');
      }

      ledger.push(entry);
      imported++;
    } catch (error) {
      errors.push({
        entryId: entry.id || 'unknown',
        error: error.message,
      });
    }
  }

  metadata.lastUpdated = Date.now();

  return {
    success: errors.length === 0,
    imported,
    errors,
    timestamp: Date.now(),
  };
}

/**
 * Get mismatch by ID
 * @param {string} id - Mismatch ID
 * @returns {Object|null} Mismatch entry or null if not found
 */
export function getMismatchById(id) {
  return ledger.find((entry) => entry.id === id) || null;
}

/**
 * Delete mismatch by ID
 * @param {string} id - Mismatch ID
 * @returns {Object} Result object
 */
export function deleteMismatch(id) {
  const index = ledger.findIndex((entry) => entry.id === id);

  if (index === -1) {
    return {
      success: false,
      error: 'Mismatch not found',
      id,
    };
  }

  ledger.splice(index, 1);
  metadata.lastUpdated = Date.now();

  return {
    success: true,
    id,
    timestamp: Date.now(),
  };
}

/**
 * Get metadata about ledger
 * @returns {Object} Ledger metadata
 */
export function getMetadata() {
  return {
    ...metadata,
    currentSize: ledger.length,
  };
}
