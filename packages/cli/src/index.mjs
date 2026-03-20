/**
 * @file UNRDF CLI - Main Entry Point
 * @module @unrdf/cli
 * @description
 * Re-export aggregator for the CLI package.
 * Provides store operations, receipt utilities, IO helpers,
 * and the main CLI entry point.
 */

// Store operations
export { backupStore } from './store-backup.mjs';
export { importStore } from './store-import.mjs';
export { restoreStore } from './store-restore.mjs';

// CLI receipt utilities
export {
  CommandSchema,
  CommandResultSchema,
  executeCommand,
  testCLIDeterminism,
} from './cli-receipts.mjs';

// IO utilities
export {
  fileExists,
  ensureDir,
  detectRDFFormat,
} from './utils/io-utils.mjs';
