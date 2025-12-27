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
export { QuadPool, quadPool, createPooledTransform, isPooledQuad } from './hooks/quad-pool.mjs';

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

// Policy Packs (Versioned governance units)
export {
  PolicyPack,
  PolicyPackManager,
  createPolicyPackFromDirectory,
  createPolicyPackManifest,
} from './hooks/policy-pack.mjs';

// Knowledge Hook Engine (High-performance executor)
export { KnowledgeHookEngine } from './hooks/knowledge-hook-engine.mjs';

// Condition Evaluator (SPARQL/SHACL evaluation)
export {
  evaluateCondition,
  createConditionEvaluator,
  validateCondition,
} from './hooks/condition-evaluator.mjs';

// File Resolver (Content-addressed file loading)
export {
  createFileResolver,
  resolveFileUri,
  loadFileWithHash,
  calculateFileHash,
} from './hooks/file-resolver.mjs';

// Schemas (Hook validation schemas)
export {
  KnowledgeHookSchema,
  HookMetaSchema,
  HookConditionSchema,
  HookEffectSchema,
  createKnowledgeHook,
  validateKnowledgeHook,
  ObservabilityConfigSchema,
  PerformanceMetricsSchema,
} from './hooks/schemas.mjs';

// Store Cache (Oxigraph store caching)
export { StoreCache } from './hooks/store-cache.mjs';

// Condition Cache (Condition result caching)
export { ConditionCache } from './hooks/condition-cache.mjs';

// Telemetry (Batched OTEL telemetry)
export { BatchedTelemetry } from './hooks/telemetry.mjs';

// Query utilities (SPARQL execution)
export { ask, select, construct } from './hooks/query.mjs';

// Validation utilities (SHACL validation)
export {
  validateShacl,
  validateNode,
  isConforming,
  getViolations,
  getWarnings,
} from './hooks/validate.mjs';

// Query Optimizer
export { createQueryOptimizer } from './hooks/query-optimizer.mjs';
