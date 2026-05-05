/**
 * Agent 7 - Routing + Shadow Modes
 * Complete routing and shadow execution system for safe migration
 * @module agent-7
 */

// Routing Modes
export {
  ROUTING_MODES,
  setMode,
  getMode,
  listModes,
  resetModes,
  getModeStats,
  exportConfig,
  importConfig,
} from './routing-modes.mjs';

// Import for local use in helper functions
import {
  getModeStats as _getModeStats,
  listModes as _listModes,
  exportConfig as _exportConfig,
  importConfig as _importConfig,
} from './routing-modes.mjs';

import {
  getLedgerStats as _getLedgerStats,
  exportLedger as _exportLedger,
  importLedger as _importLedger,
} from './mismatch-ledger.mjs';

import {
  getDriftReport as _getDriftReport,
} from './drift-detector.mjs';

import {
  getRollbackStats as _getRollbackStats,
  exportRollbackState as _exportRollbackState,
  importRollbackState as _importRollbackState,
} from './rollback-router.mjs';

// Shadow Executor
export {
  shadowWrite,
  shadowRead,
  partialServe,
  execute,
} from './shadow-executor.mjs';

// Mismatch Ledger
export {
  recordMismatch,
  getMismatches,
  exportLedger,
  clearLedger,
  getLedgerStats,
  getRecentMismatches,
  importLedger,
  getMismatchById,
  deleteMismatch,
  getMetadata,
} from './mismatch-ledger.mjs';

// Drift Detector
export {
  detectDrift,
  calculateDriftScore,
  isAcceptableDrift,
  getDriftReport,
  getThresholds,
  getSeverityWeights,
  analyzeDiffType,
} from './drift-detector.mjs';

// Rollback Router
export {
  markForRollback,
  executeRollback,
  getRollbackStatus,
  getOperationRollbackStatus,
  clearRollbackMarker,
  getRollbackHistory,
  autoRollback,
  batchRollback,
  clearAllRollbackMarkers,
  getRollbackStats,
  exportRollbackState,
  importRollbackState,
} from './rollback-router.mjs';

/**
 * Quick start helper - Initialize routing for an operation
 * @param {string} operation - Operation identifier
 * @param {string} mode - Initial routing mode
 * @returns {Object} Initialization result
 */
export function initializeRouting(operation, mode) {
  return setMode(operation, mode || ROUTING_MODES.LEGACY_ONLY);
}

/**
 * Quick start helper - Run shadow mode with automatic drift detection
 * @param {string} operation - Operation identifier
 * @param {*} payload - Request payload
 * @param {Function} legacyFn - Legacy system function
 * @param {Function} substrateFn - Substrate system function
 * @param {Object} options - Options
 * @returns {Promise<Object>} Execution result with drift analysis
 */
export async function runShadowWithDrift(
  operation,
  payload,
  legacyFn,
  substrateFn,
  options = {}
) {
  // Execute in shadow mode
  const result = await execute(operation, payload, legacyFn, substrateFn, options);

  // Detect drift
  const drift = detectDrift(operation, options.driftOptions);

  // Auto-rollback if configured
  let rollback = null;
  if (options.autoRollback && drift.hasDrift) {
    rollback = autoRollback(operation, {
      threshold: options.driftThreshold || 75,
      autoExecute: true,
    });
  }

  return {
    ...result,
    drift,
    rollback,
  };
}

/**
 * Get comprehensive system status
 * @returns {Object} Complete system status
 */
export function getSystemStatus() {
  return {
    modes: {
      stats: _getModeStats(),
      operations: _listModes(),
    },
    mismatches: _getLedgerStats(),
    drift: _getDriftReport(),
    rollbacks: _getRollbackStats(),
    timestamp: Date.now(),
  };
}

/**
 * Export complete system state
 * @returns {Object} Exportable system state
 */
export function exportSystemState() {
  return {
    version: '1.0.0',
    exportedAt: Date.now(),
    modes: _exportConfig(),
    ledger: _exportLedger(),
    rollbacks: _exportRollbackState(),
  };
}

/**
 * Import complete system state
 * @param {Object} state - Previously exported state
 * @returns {Object} Import results
 */
export function importSystemState(state) {
  const results = {
    modes: null,
    ledger: null,
    rollbacks: null,
    success: false,
  };

  try {
    if (state.modes) {
      results.modes = _importConfig(state.modes);
    }

    if (state.ledger) {
      results.ledger = _importLedger(state.ledger);
    }

    if (state.rollbacks) {
      results.rollbacks = _importRollbackState(state.rollbacks);
    }

    results.success =
      results.modes?.success !== false &&
      results.ledger?.success !== false &&
      results.rollbacks?.success !== false;
  } catch (error) {
    results.error = error.message;
    results.success = false;
  }

  return results;
}
