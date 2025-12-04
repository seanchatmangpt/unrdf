/**
 * @unrdf/hooks
 *
 * Knowledge Hooks - Policy Definition and Execution Framework
 *
 * @module @unrdf/hooks
 */

// Hook definition
export {
  defineHook,
  isValidHook,
  getHookMetadata,
  hasValidation,
  hasTransformation,
  HookTriggerSchema,
  HookConfigSchema,
  HookSchema,
} from './hooks/define-hook.mjs';

// Hook execution
export {
  executeHook,
  executeHookChain,
  executeHooksByTrigger,
  wouldPassHooks,
  HookResultSchema,
  ChainResultSchema,
  // Validation-only execution
  validateOnly,
  // Cache management
  clearHookCaches,
  prewarmHookCache,
  // Batch API (high-performance bulk operations)
  executeBatch,
  validateBatch,
  transformBatch,
} from './hooks/hook-executor.mjs';

// Hook chain compiler (JIT optimization)
export {
  compileHookChain,
  compileValidationOnlyChain,
  clearCompiledChainCache,
  getCompilerStats,
  isJitAvailable,
  getChainKey,
} from './hooks/hook-chain-compiler.mjs';

// Quad pool (object pooling optimization)
export {
  QuadPool,
  quadPool,
  createPooledTransform,
  isPooledQuad,
} from './hooks/quad-pool.mjs';

// Hook management
export {
  createHookRegistry,
  registerHook,
  unregisterHook,
  getHook,
  listHooks,
  getHooksByTrigger,
  hasHook,
  clearHooks,
  getRegistryStats,
  HookRegistrySchema,
} from './hooks/hook-management.mjs';

// Built-in hooks
export {
  builtinHooks,
  validateSubjectIRI,
  validatePredicateIRI,
  validateObjectLiteral,
  validateIRIFormat,
  validateLanguageTag,
  rejectBlankNodes,
  normalizeNamespace,
  normalizeLanguageTag,
  trimLiterals,
  // Pooled variants (zero-allocation transforms)
  normalizeLanguageTagPooled,
  trimLiteralsPooled,
  standardValidation,
} from './hooks/builtin-hooks.mjs';

// Hook manager (class-based interface)
export { KnowledgeHookManager } from './hooks/knowledge-hook-manager.mjs';

// Hook scheduler (cron/interval triggers)
export {
  HookScheduler,
  createHookScheduler,
  ScheduleConfigSchema,
} from './hooks/hook-scheduler.mjs';

// Quality metrics (Lean Six Sigma hooks)
export {
  QualityMetricsCollector,
  createQualityHooks,
  QualityGateSchema,
  SPCDataPointSchema,
} from './hooks/quality-metrics.mjs';
