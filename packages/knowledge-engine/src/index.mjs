/**
 * @file Central Knowledge Engine Index
 * @module knowledge-engine
 *
 * @description
 * Centralized export hub for the entire knowledge engine system.
 * This provides a single import point for all functionality.
 */

// Core Engine Components
// NOTE: KnowledgeHookManager temporarily disabled - missing dependencies
// export { KnowledgeHookManager } from './knowledge-hook-manager.mjs';
export { TransactionManager } from './transaction.mjs';

// Hook System
// NOTE: Hook system files not yet implemented - exports temporarily disabled
// export { defineHook } from './define-hook.mjs';
// export { createHookExecutor } from './hook-executor.mjs';
// export { createConditionEvaluator } from './condition-evaluator.mjs';
// export {
//   registerHook,
//   deregisterHook,
//   evaluateHook,
//   getRegisteredHooks,
//   resetGlobalHookManager,
// } from './hook-management.mjs';

// Knowledge Substrate Core (80/20 Framework)
// NOTE: knowledge-substrate-core.mjs not yet implemented - exports temporarily disabled
// export {
//   KnowledgeSubstrateCore,
//   createKnowledgeSubstrateCore,
//   KnowledgeSubstrateFactory,
//   // Legacy compatibility
//   DarkMatterCore,
//   createDarkMatterCore,
//   DarkMatterFactory,
// } from './knowledge-substrate-core.mjs';

// Storage & Persistence
export { LockchainWriter, createLockchainWriter } from '@unrdf/core/utils/lockchain-writer';
export { ResolutionLayer } from './resolution-layer.mjs';

// Query & Optimization
export { QueryOptimizer } from './query-optimizer.mjs';
// NOTE: query.mjs not yet implemented - exports temporarily disabled
// export { query, select, ask, construct, describe, update, getQueryStats } from './query.mjs';

// Utilities
// NOTE: Utility files not yet implemented - exports temporarily disabled
// export { parseTurtle, toTurtle, toNQuads, parseJsonLd, toJsonLd } from './parse.mjs';
// export {
//   validateShacl,
//   validateShaclMultiple,
//   formatValidationReport,
//   hasValidationErrors,
//   getValidationErrors,
//   getValidationWarnings,
// } from './validate.mjs';
// export {
//   canonicalize,
//   isIsomorphic,
//   getCanonicalHash,
//   groupByIsomorphism,
//   findDuplicates,
//   getCanonicalizationStats,
//   createCanonicalizationSession,
// } from './canonicalize.mjs';
// export {
//   reason,
//   reasonMultiple,
//   extractInferred,
//   getReasoningStats,
//   validateRules,
//   createReasoningSession,
// } from './reason.mjs';
// export {
//   resolveFileUri,
//   calculateFileHash,
//   loadFileWithHash,
//   loadSparqlFile,
// } from './file-resolver.mjs';

// Security & Sandbox
// NOTE: effect-sandbox.mjs not yet implemented - export temporarily disabled
// export { EffectSandbox } from './effect-sandbox.mjs';

// Policy Management
// NOTE: policy-pack.mjs not yet implemented - export temporarily disabled
// export { PolicyPackManager, PolicyPack } from './policy-pack.mjs';

// Observability System
export {
  ObservabilityManager,
  createObservabilityManager,
  defaultObservabilityManager,
} from './observability.mjs';

// Consolidated Schemas (single source of truth)
export * from './schemas.mjs';
