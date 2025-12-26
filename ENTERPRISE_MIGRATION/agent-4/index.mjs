/**
 * @fileoverview Lens Kernel - Translation layer for contract-preserving migration
 * @module lens-kernel
 *
 * The Lens Kernel provides bidirectional transformations between legacy requests
 * and substrate operations, enabling contract-preserving migration.
 *
 * Architecture:
 * - Registry: Manage lens definitions
 * - Compiler: Validate and optimize lenses
 * - Executor: Execute transformations with timing/logging
 * - Utils: Normalization and comparison utilities
 *
 * @example
 * import { defineLens, compileLens, executeToSubstrate } from './agent-4/index.mjs';
 *
 * // Define lens
 * const lens = defineLens({
 *   name: 'user-profile',
 *   version: '1.0.0',
 *   toSubstrate: (req) => ({ type: 'UPSERT_USER', payload: req.body }),
 *   fromSubstrate: (data) => ({ id: data.id, name: data.name }),
 * });
 *
 * // Compile lens
 * const compiled = compileLens(lens);
 *
 * // Execute transformation
 * const result = executeToSubstrate(compiled, { body: { name: 'Alice' } });
 */

// Registry exports
export {
  defineLens,
  getLens,
  listLenses,
  hasLens,
  unregisterLens,
  clearRegistry,
  getRegistryStats,
} from './lens-registry.mjs';

// Compiler exports
export {
  compileLens,
  isCompiledLens,
  decompileLens,
  lensesEquivalent,
} from './lens-compiler.mjs';

// Executor exports
export {
  executeToSubstrate,
  executeFromSubstrate,
  executeBatch,
  testBidirectionality,
  getExecutionStats,
  clearExecutionStats,
} from './lens-executor.mjs';

// Utility exports
export {
  normalizePath,
  normalizeId,
  deepEqual,
  hashObject,
  validateFunction,
  validateString,
  deepClone,
} from './lens-utils.mjs';
