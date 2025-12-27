/**
 * @fileoverview Lockfile Manager - Re-exports from cache modules.
 *
 * This module provides backward-compatible exports for the enhanced
 * lockfile system implemented in cache/.
 *
 * For new code, prefer importing directly from cache/ modules.
 *
 * @module lib/latex/lockfile
 */

// Import for local use and re-export
import {
  createLockfile,
  loadLockfile,
  saveLockfile,
  addEntry,
  verifyEntry,
  verifyAllEntries,
  getEntry,
  removeEntry,
  listEntries,
  mergeLockfiles,
  pruneLockfile,
  createLockEntry,
  LockEntrySchema,
  LockfileSchema
} from './cache/lockfile.mjs';

// Re-export everything
export {
  createLockfile,
  loadLockfile,
  saveLockfile,
  addEntry,
  verifyEntry,
  verifyAllEntries,
  getEntry,
  removeEntry,
  listEntries,
  mergeLockfiles,
  pruneLockfile,
  createLockEntry,
  LockEntrySchema,
  LockfileSchema
};

export {
  getCached,
  setCached,
  listCached,
  getCacheStats,
  clearCache,
  verifyCached
} from './cache/store.mjs';

export {
  exportBundle,
  importBundle,
  verifyBundle,
  listBundle
} from './cache/bundle.mjs';

/**
 * Legacy compatibility functions for existing code.
 */

/**
 * Update lockfile with newly resolved inputs (legacy).
 *
 * @deprecated Use addEntry() directly for each resolved file
 * @param {Object} lockfile - Lockfile object
 * @param {Map<string, Uint8Array>} resolvedInputs - Resolved files
 */
export function updateLockfileWithResolved(lockfile, resolvedInputs) {
  for (const [path, content] of resolvedInputs) {
    const name = path.split('/').pop();
    const entry = createLockEntry({
      name,
      content,
      cachedPath: path
    });
    addEntry(lockfile, entry);
  }
}

/**
 * Update lockfile with successful compilation info (legacy).
 *
 * @deprecated Store compilation metadata separately
 * @param {Object} lockfile - Lockfile object
 * @param {Object} compileInfo - Compilation metadata
 */
export function updateLockfileWithSuccess(lockfile, compileInfo) {
  // Store as custom field for backward compatibility
  lockfile.lastCompilation = {
    ...compileInfo,
    success: true,
    timestamp: Date.now()
  };
}
