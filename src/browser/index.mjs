/**
 * @fileoverview Browser support module for UNRDF
 *
 * Exports all browser-compatible utilities:
 * - Browser shims (fs, path, crypto, Worker)
 * - IndexedDB file system
 * - IndexedDB quad store
 * - File system adapter
 * - Comunica browser adapter
 * - Browser lockchain writer
 *
 * @module browser
 */

// Browser shims
export {
  fs,
  path,
  crypto,
  BrowserWorker,
  isBrowser,
  getEnvironmentShims,
} from './browser-shim.mjs';

// IndexedDB file system
export { IndexedDBFileSystem } from './indexeddb-fs.mjs';

// IndexedDB quad store
export { IndexedDBQuadStore } from './indexeddb-store.mjs';

// File system adapter
export {
  FileSystemAdapter,
  createFsAdapter,
  getPath,
} from './fs-adapter.mjs';

// Comunica browser adapter
export {
  BrowserQueryExecutor,
  createBrowserQueryExecutor,
} from './comunica-browser-adapter.mjs';

// Browser lockchain writer
export {
  BrowserLockchainWriter,
  createBrowserLockchainWriter,
} from './browser-lockchain-writer.mjs';
