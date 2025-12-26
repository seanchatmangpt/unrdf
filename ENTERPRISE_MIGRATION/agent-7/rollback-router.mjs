/**
 * Rollback Router - Manage rollback routing for safe migration
 * @module agent-7/rollback-router
 */

import { setMode, getMode, ROUTING_MODES } from './routing-modes.mjs';
import { detectDrift } from './drift-detector.mjs';

/**
 * Rollback registry
 * @type {Map<string, Object>}
 */
const rollbackRegistry = new Map();

/**
 * Rollback history
 * @type {Array<Object>}
 */
const rollbackHistory = [];

/**
 * Mark operation for rollback
 * @param {string} operation - Operation identifier
 * @param {Object} options - Rollback options
 * @param {string} [options.reason] - Reason for rollback
 * @param {number} [options.driftScore] - Drift score that triggered rollback
 * @param {Object} [options.metadata] - Additional metadata
 * @returns {Object} Rollback marker
 */
export function markForRollback(operation, options = {}) {
  if (!operation || typeof operation !== 'string') {
    throw new Error('Operation must be a non-empty string');
  }

  const timestamp = Date.now();
  const currentMode = getMode(operation);

  const marker = {
    operation,
    markedAt: timestamp,
    currentMode,
    reason: options.reason || 'Unspecified',
    driftScore: options.driftScore || null,
    metadata: options.metadata || {},
    executed: false,
  };

  rollbackRegistry.set(operation, marker);

  return marker;
}

/**
 * Execute rollback for an operation
 * @param {string} operation - Operation identifier
 * @param {Object} options - Execution options
 * @param {boolean} [options.force] - Force rollback even if not marked
 * @returns {Object} Rollback result
 */
export function executeRollback(operation, options = {}) {
  if (!operation || typeof operation !== 'string') {
    throw new Error('Operation must be a non-empty string');
  }

  const marker = rollbackRegistry.get(operation);
  const currentMode = getMode(operation);

  // Check if operation is marked for rollback
  if (!marker && !options.force) {
    throw new Error(
      `Operation ${operation} is not marked for rollback. Use force option to override.`
    );
  }

  // Can't rollback if already in legacy-only mode
  if (currentMode === ROUTING_MODES.LEGACY_ONLY) {
    return {
      success: false,
      operation,
      reason: 'Already in LEGACY_ONLY mode',
      currentMode,
      timestamp: Date.now(),
    };
  }

  const previousMode = currentMode;
  const timestamp = Date.now();

  // Set mode to legacy-only
  setMode(operation, ROUTING_MODES.LEGACY_ONLY);

  // Update marker if exists
  if (marker) {
    marker.executed = true;
    marker.executedAt = timestamp;
    marker.previousMode = previousMode;
  }

  // Record in history
  const historyEntry = {
    operation,
    timestamp,
    previousMode,
    newMode: ROUTING_MODES.LEGACY_ONLY,
    marker: marker || null,
    forced: options.force || false,
  };

  rollbackHistory.push(historyEntry);

  return {
    success: true,
    operation,
    previousMode,
    newMode: ROUTING_MODES.LEGACY_ONLY,
    marker: marker || null,
    timestamp,
  };
}

/**
 * Get rollback status for all operations
 * @returns {Object} Rollback status summary
 */
export function getRollbackStatus() {
  const marked = [];
  const executed = [];

  for (const [operation, marker] of rollbackRegistry.entries()) {
    if (marker.executed) {
      executed.push(marker);
    } else {
      marked.push(marker);
    }
  }

  return {
    markedCount: marked.length,
    executedCount: executed.length,
    marked,
    executed,
    timestamp: Date.now(),
  };
}

/**
 * Get rollback status for specific operation
 * @param {string} operation - Operation identifier
 * @returns {Object|null} Rollback marker or null if not marked
 */
export function getOperationRollbackStatus(operation) {
  const marker = rollbackRegistry.get(operation);

  if (!marker) {
    return null;
  }

  return {
    ...marker,
    currentMode: getMode(operation),
  };
}

/**
 * Clear rollback marker for an operation
 * @param {string} operation - Operation identifier
 * @returns {Object} Result object
 */
export function clearRollbackMarker(operation) {
  if (!operation || typeof operation !== 'string') {
    throw new Error('Operation must be a non-empty string');
  }

  const marker = rollbackRegistry.get(operation);

  if (!marker) {
    return {
      success: false,
      operation,
      reason: 'Operation not marked for rollback',
    };
  }

  rollbackRegistry.delete(operation);

  return {
    success: true,
    operation,
    clearedMarker: marker,
    timestamp: Date.now(),
  };
}

/**
 * Get rollback history
 * @param {Object} options - Query options
 * @param {string} [options.operation] - Filter by operation
 * @param {number} [options.since] - Filter by timestamp
 * @param {number} [options.limit] - Limit results
 * @returns {Array<Object>} Rollback history entries
 */
export function getRollbackHistory(options = {}) {
  let results = [...rollbackHistory];

  // Filter by operation
  if (options.operation) {
    results = results.filter((entry) => entry.operation === options.operation);
  }

  // Filter by timestamp
  if (options.since) {
    results = results.filter((entry) => entry.timestamp >= options.since);
  }

  // Sort by timestamp (newest first)
  results.sort((a, b) => b.timestamp - a.timestamp);

  // Apply limit
  if (options.limit && options.limit > 0) {
    results = results.slice(0, options.limit);
  }

  return results;
}

/**
 * Automatic rollback based on drift detection
 * @param {string} operation - Operation identifier
 * @param {Object} options - Detection options
 * @param {number} [options.threshold] - Drift score threshold (0-100)
 * @param {boolean} [options.autoExecute] - Auto-execute rollback if threshold exceeded
 * @returns {Object} Auto-rollback result
 */
export function autoRollback(operation, options = {}) {
  const threshold = options.threshold || 75; // Default: CRITICAL threshold
  const autoExecute = options.autoExecute !== false; // Default: true

  // Detect drift
  const driftResult = detectDrift(operation);

  if (!driftResult.hasDrift || driftResult.score <= threshold) {
    return {
      triggered: false,
      operation,
      driftScore: driftResult.score,
      threshold,
      reason: 'Drift score below threshold',
      timestamp: Date.now(),
    };
  }

  // Mark for rollback
  const marker = markForRollback(operation, {
    reason: `Automatic rollback: drift score ${driftResult.score} exceeds threshold ${threshold}`,
    driftScore: driftResult.score,
    metadata: {
      driftSeverity: driftResult.severity,
      autoTriggered: true,
    },
  });

  let rollbackResult = null;

  // Execute rollback if auto-execute enabled
  if (autoExecute) {
    rollbackResult = executeRollback(operation);
  }

  return {
    triggered: true,
    operation,
    driftScore: driftResult.score,
    threshold,
    marker,
    executed: autoExecute,
    rollbackResult,
    timestamp: Date.now(),
  };
}

/**
 * Batch rollback for multiple operations
 * @param {Array<string>} operations - Array of operation identifiers
 * @param {Object} options - Rollback options
 * @returns {Object} Batch rollback result
 */
export function batchRollback(operations, options = {}) {
  if (!Array.isArray(operations)) {
    throw new Error('Operations must be an array');
  }

  const results = {
    success: [],
    failed: [],
    timestamp: Date.now(),
  };

  for (const operation of operations) {
    try {
      const result = executeRollback(operation, options);
      results.success.push(result);
    } catch (error) {
      results.failed.push({
        operation,
        error: error.message,
      });
    }
  }

  return {
    total: operations.length,
    successCount: results.success.length,
    failedCount: results.failed.length,
    results,
    timestamp: Date.now(),
  };
}

/**
 * Clear all rollback markers
 * @returns {Object} Result object
 */
export function clearAllRollbackMarkers() {
  const count = rollbackRegistry.size;
  rollbackRegistry.clear();

  return {
    success: true,
    clearedCount: count,
    timestamp: Date.now(),
  };
}

/**
 * Get rollback statistics
 * @returns {Object} Rollback statistics
 */
export function getRollbackStats() {
  const marked = [];
  const executed = [];

  for (const marker of rollbackRegistry.values()) {
    if (marker.executed) {
      executed.push(marker);
    } else {
      marked.push(marker);
    }
  }

  return {
    totalMarked: rollbackRegistry.size,
    pendingRollbacks: marked.length,
    executedRollbacks: executed.length,
    totalHistoryEntries: rollbackHistory.length,
    byReason: countByReason(Array.from(rollbackRegistry.values())),
    timestamp: Date.now(),
  };
}

/**
 * Count rollbacks by reason
 * @param {Array<Object>} markers - Array of rollback markers
 * @returns {Object} Count by reason
 */
function countByReason(markers) {
  const counts = {};

  for (const marker of markers) {
    const reason = marker.reason || 'Unspecified';
    counts[reason] = (counts[reason] || 0) + 1;
  }

  return counts;
}

/**
 * Export rollback state
 * @returns {Object} Exportable rollback state
 */
export function exportRollbackState() {
  return {
    version: '1.0.0',
    exportedAt: Date.now(),
    registry: Array.from(rollbackRegistry.entries()).map(([operation, marker]) => ({
      operation,
      ...marker,
    })),
    history: rollbackHistory,
  };
}

/**
 * Import rollback state
 * @param {Object} state - Previously exported state
 * @returns {Object} Result object
 */
export function importRollbackState(state) {
  if (!state || typeof state !== 'object') {
    throw new Error('State must be an object');
  }

  if (!Array.isArray(state.registry)) {
    throw new Error('State must contain registry array');
  }

  if (!Array.isArray(state.history)) {
    throw new Error('State must contain history array');
  }

  // Clear existing state
  rollbackRegistry.clear();
  rollbackHistory.length = 0;

  let imported = 0;
  const errors = [];

  // Import registry
  for (const entry of state.registry) {
    try {
      const { operation, ...marker } = entry;
      rollbackRegistry.set(operation, marker);
      imported++;
    } catch (error) {
      errors.push({
        operation: entry.operation || 'unknown',
        error: error.message,
      });
    }
  }

  // Import history
  rollbackHistory.push(...state.history);

  return {
    success: errors.length === 0,
    imported,
    errors,
    timestamp: Date.now(),
  };
}
