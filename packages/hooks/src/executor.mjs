/**
 * @fileoverview Hook Execution API - Entry point for @unrdf/hooks/executor
 *
 * Provides the primary execution interface:
 * - HookExecutor (KnowledgeHookEngine) for high-performance hook execution
 * - executeHook() for single hook execution
 * - batchExecute() for bulk operations
 * - Result schemas and types
 *
 * @module @unrdf/hooks/executor
 */

// Main execution engine (aliased as HookExecutor for public API)
export { KnowledgeHookEngine as HookExecutor } from './hooks/knowledge-hook-engine.mjs';

// Single hook execution
export { executeHook } from './hooks/hook-executor.mjs';

// Batch execution (aliased as batchExecute for public API)
export { executeBatch as batchExecute } from './hooks/hook-executor.mjs';

// Result types / schemas
export { HookResultSchema, ChainResultSchema } from './hooks/hook-executor.mjs';
