/**
 * @file Central Knowledge Engine Index
 * @module knowledge-engine
 * 
 * @description
 * Centralized export hub for the entire knowledge engine system.
 * This provides a single import point for all functionality with
 * automatic browser/Node.js compatibility detection.
 */

/**
 * Detect if we're running in a browser environment
 */
const isBrowser = typeof window !== 'undefined' && typeof window.document !== 'undefined';

// Conditionally export browser-compatible versions
if (isBrowser) {
  export {
    BrowserKnowledgeHookManager as KnowledgeHookManager,
    BrowserLockchainWriter as LockchainWriter,
    EffectSandbox,
    QueryEngine,
    createBrowserLockchainWriter as createLockchainWriter,
    createBrowserKnowledgeHookManager as createKnowledgeHookManager,
    createEffectSandbox,
    BrowserHookExecutor,
    BrowserConditionEvaluator,
    BrowserPolicyPackManager,
    BrowserFileResolver,
    BrowserResolutionLayer
  } from './browser.mjs';
} else {
  // Core Engine Components (Node.js version)
  export { KnowledgeHookManager } from './knowledge-hook-manager.mjs';
  export { TransactionManager } from './transaction.mjs';
}

// Hook System
export { defineHook } from './define-hook.mjs';
export { createHookExecutor } from './hook-executor.mjs';
export { createConditionEvaluator } from './condition-evaluator.mjs';

// Storage & Persistence
export { createLockchainWriter } from './lockchain-writer.mjs';
export { ResolutionLayer } from './resolution-layer.mjs';

// Query & Optimization
export { QueryOptimizer } from './query-optimizer.mjs';
export { query } from './query.mjs';

// Utilities
export { parseTurtle, toTurtle, toNQuads, parseJsonLd, toJsonLd } from './parse.mjs';
export { validateShacl, validateShaclMultiple, formatValidationReport, hasValidationErrors, getValidationErrors, getValidationWarnings } from './validate.mjs';
export { canonicalize, isIsomorphic, getCanonicalHash, groupByIsomorphism, findDuplicates, getCanonicalizationStats, createCanonicalizationSession } from './canonicalize.mjs';
export { reason, reasonMultiple, extractInferred, getReasoningStats, validateRules, createReasoningSession } from './reason.mjs';
export { resolveFileUri, calculateFileHash, loadFileWithHash, loadSparqlFile } from './file-resolver.mjs';

// Security & Sandbox
export { EffectSandbox } from './effect-sandbox.mjs';

// Policy Management
export { PolicyPackManager, PolicyPack } from './policy-pack.mjs';

// Consolidated Schemas (single source of truth)
export * from './schemas.mjs';
