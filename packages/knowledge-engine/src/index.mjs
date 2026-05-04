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
export { LockchainWriter, createLockchainWriter } from '@unrdf/core/utils/lockchain-writer';
export { ResolutionLayer } from './resolution-layer.mjs';

// Query & Optimization
export { QueryOptimizer } from './query-optimizer.mjs';
export { query, select, ask, construct, describe, update, getQueryStats } from './query.mjs';

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

// Chatman Equation Integration
export { ChatmanOperator, createChatmanOperator } from './chatman-operator.mjs';
export { ArtifactGenerator, createArtifactGenerator } from './artifact-generator.mjs';
export { DarkFieldDetector, createDarkFieldDetector } from './dark-field-detector.mjs';
export { FormationTheorems, createFormationTheorems } from './formation-theorems.mjs';
export {
  ChatmanConfigLoader,
  createChatmanConfigLoader,
  loadChatmanConfig,
} from './chatman-config-loader.mjs';
export { ChatmanEngine, createChatmanEngine } from './chatman-engine.mjs';

// Chatman Dynamics Rules
export * from './chatman/market-dynamics.mjs';
export * from './chatman/organizational-dynamics.mjs';
export * from './chatman/strategic-dynamics.mjs';
export * from './chatman/disruption-arithmetic.mjs';

// Consolidated Schemas (single source of truth)
export * from './schemas.mjs';
