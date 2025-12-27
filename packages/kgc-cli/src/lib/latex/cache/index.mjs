/**
 * @fileoverview Cache management for LaTeX packages.
 *
 * Unified exports for lockfile, store, and bundle operations.
 *
 * @module lib/latex/cache
 */

// Lockfile operations
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
} from './lockfile.mjs';

// Cache store operations
export {
  getCached,
  setCached,
  listCached,
  getCacheStats,
  clearCache,
  verifyCached
} from './store.mjs';

// Bundle operations
export {
  exportBundle,
  importBundle,
  verifyBundle,
  listBundle
} from './bundle.mjs';

// CTAN package resolution (Agent 4)
export {
  resolveMissingInputs,
  augmentVfsWithResolvedPackages
} from './resolve.mjs';

// CTAN package mapping
export {
  buildCtanUrls,
  buildVfsPath,
  extractPackageName,
  getExtension,
  isLocalFixture,
  getPackageMetadata,
  CTAN_PATH_TEMPLATES,
  VFS_PATH_TEMPLATES,
  PACKAGE_NAME_EXCEPTIONS,
  DEFAULT_CTAN_MIRROR
} from './ctan-map.mjs';
