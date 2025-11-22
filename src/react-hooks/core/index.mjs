/**
 * @file react-hooks/core/index.mjs
 * @description Core React hooks for UNRDF knowledge engine integration
 *
 * IMPORTANT: This module is designed for use with bundlers (Vite, webpack, etc.)
 * that support JSX transformation. These hooks use React context providers which
 * contain JSX syntax.
 *
 * This module exports the essential hooks that cover 60% of typical usage:
 * - useKnowledgeEngine: Primary hook for knowledge engine access
 * - useStore: Direct store manipulation
 * - useTriples: Triple pattern queries
 * - useTerms: RDF term utilities
 * - useGraphs: Named graph operations
 * - useTransaction: ACID-compliant quad operations
 * - useKnowledgeHook: Hook execution and management
 *
 * @example
 * // Use with bundler (Vite, webpack, etc.)
 * import { useKnowledgeEngine, useTriples } from 'unrdf/react-hooks/core';
 *
 * function MyComponent() {
 *   const { engine, store, query } = useKnowledgeEngine();
 *   const { triples } = useTriples({ subject: null });
 *   // ...
 * }
 *
 * @module react-hooks/core
 */

// Primary engine hook (standalone, no context dependency)
export { useKnowledgeEngine } from './useKnowledgeEngine.mjs';

// Store operations (standalone)
export { useStore } from './useStore.mjs';

// Hooks with context dependencies (require bundler for JSX)
export { useTriples } from './useTriples.mjs';
export { useTerms } from './useTerms.mjs';
export { useGraphs } from './useGraphs.mjs';

// Transaction hook (context dependent)
export { useTransaction } from '../storage/useTransaction.mjs';

// Knowledge hook integration (context dependent)
export { useKnowledgeHook } from '../knowledge-hooks/useKnowledgeHook.mjs';
export { useHookManager } from '../knowledge-hooks/useHookManager.mjs';
export { useHookExecution } from '../knowledge-hooks/useHookExecution.mjs';
export { useHookRegistry } from '../knowledge-hooks/useHookRegistry.mjs';

// Context provider exports (JSX - requires bundler)
export { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';
export { useConfigContext } from '../context/useConfigContext.mjs';

// Module preloading (TRIZ #24 - Intermediary pattern)
export {
  useModulePreloader,
  preloadModule,
  preloadModules,
  getModule,
  isModuleLoaded,
  isModuleLoading,
  clearModuleCache,
  getCacheStats,
  PRELOAD_MODULES,
  PRELOAD_PRIORITY,
  PRELOAD_GROUPS,
  areEssentialModulesLoaded,
  preloadEssentialModules,
} from './use-module-preloader.mjs';
