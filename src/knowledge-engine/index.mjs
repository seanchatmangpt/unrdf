/**
 * @file Central Knowledge Engine Index
 * @module knowledge-engine
 *
 * @description
 * Centralized export hub for the entire knowledge engine system.
 * This provides a single import point for all functionality.
 */

// Core Engine Components
export { KnowledgeHookManager } from './knowledge-hook-manager.mjs';
export { TransactionManager } from './transaction.mjs';

// Hook System
export { defineHook } from './define-hook.mjs';
export { createHookExecutor } from './hook-executor.mjs';
export { createConditionEvaluator } from './condition-evaluator.mjs';
export {
  registerHook,
  deregisterHook,
  evaluateHook,
  getRegisteredHooks,
  resetGlobalHookManager,
} from './hook-management.mjs';

// Knowledge Substrate Core (80/20 Framework)
export {
  KnowledgeSubstrateCore,
  createKnowledgeSubstrateCore,
  KnowledgeSubstrateFactory,
  // Legacy compatibility
  DarkMatterCore,
  createDarkMatterCore,
  DarkMatterFactory,
} from './knowledge-substrate-core.mjs';

// Storage & Persistence
export { LockchainWriter, createLockchainWriter } from './lockchain-writer.mjs';
export { ResolutionLayer } from './resolution-layer.mjs';

// Query & Optimization
export { QueryOptimizer } from './query-optimizer.mjs';
export { query } from './query.mjs';

// Utilities
export { parseTurtle, toTurtle, toNQuads, parseJsonLd, toJsonLd } from './parse.mjs';
export {
  validateShacl,
  validateShaclMultiple,
  formatValidationReport,
  hasValidationErrors,
  getValidationErrors,
  getValidationWarnings,
} from './validate.mjs';
export {
  canonicalize,
  isIsomorphic,
  getCanonicalHash,
  groupByIsomorphism,
  findDuplicates,
  getCanonicalizationStats,
  createCanonicalizationSession,
} from './canonicalize.mjs';
export {
  reason,
  reasonMultiple,
  extractInferred,
  getReasoningStats,
  validateRules,
  createReasoningSession,
} from './reason.mjs';
export {
  resolveFileUri,
  calculateFileHash,
  loadFileWithHash,
  loadSparqlFile,
} from './file-resolver.mjs';

// Security & Sandbox
export { EffectSandbox } from './effect-sandbox.mjs';

// Policy Management
export { PolicyPackManager, PolicyPack } from './policy-pack.mjs';

// Observability System
export {
  ObservabilityManager,
  createObservabilityManager,
  defaultObservabilityManager,
} from './observability.mjs';

// N3 Re-exports (as documented in README)
export { Store, Parser, Writer, DataFactory } from '@unrdf/core/rdf/n3-justified-only';

// Consolidated Schemas (single source of truth)
export * from './schemas.mjs';
