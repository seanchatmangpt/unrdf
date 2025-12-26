/**
 * @file Substrate Adapter Layer - Main exports
 * @module agent-9
 *
 * Provides adapters to use existing UNRDF packages as the substrate.
 * The substrate adapter provides:
 * - Atomic apply (batch operations)
 * - Query interface
 * - Verify/freeze capabilities
 * - Transaction support
 * - Integration with @unrdf/oxigraph and @unrdf/hooks
 */

// Operation types
export {
  OperationType,
  createOperation,
  validateOperation,
  validateOperations,
  isOperation
} from './operation-types.mjs';

// Substrate store
export {
  createSubstrate,
  apply,
  query,
  snapshot,
  getStats
} from './substrate-store.mjs';

// Transaction support
export {
  beginTransaction,
  commit,
  rollback,
  isInTransaction,
  getCurrentTransaction,
  addOperation,
  getTransactionStats,
  withTransaction
} from './transaction.mjs';

// Oxigraph adapter
export {
  createOxigraphSubstrate,
  toOxigraphOps,
  fromOxigraphResult,
  applyToOxigraph,
  queryOxigraph,
  snapshotOxigraph,
  getOxigraphStats
} from './oxigraph-adapter.mjs';

// Hooks adapter
export {
  createHooksSubstrate,
  registerHookAdapter,
  executeHook,
  executeHooksByTrigger,
  unregisterHook,
  enableHook,
  disableHook,
  getHooks,
  getHook,
  getHookHistory,
  clearHookHistory,
  getHooksStats
} from './hooks-adapter.mjs';
