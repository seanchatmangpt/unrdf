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
} from './hooks/hook-executor.mjs';

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
  standardValidation,
} from './hooks/builtin-hooks.mjs';

// Hook manager (class-based interface)
export { KnowledgeHookManager } from './hooks/knowledge-hook-manager.mjs';
