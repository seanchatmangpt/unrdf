/**
 * @fileoverview Main entry point for UNRDF React Hooks
 * @module react-hooks
 *
 * @description
 * Comprehensive React hooks framework for UNRDF Knowledge Engine.
 * Provides reactive RDF data management, SPARQL queries, knowledge hooks,
 * and advanced caching/optimization features.
 */

// Core Hooks
export { useKnowledgeEngine } from './core/useKnowledgeEngine.mjs';
export { useStore } from './core/useStore.mjs';
export { useTriples } from './core/useTriples.mjs';
export { useGraphs } from './core/useGraphs.mjs';
export { useTerms } from './core/useTerms.mjs';

// Query Hooks
export { useSPARQLQuery } from './query/useSPARQLQuery.mjs';
export { useQueryAsync } from './query/useQueryAsync.mjs';
export { useShapeValidation } from './query/useShapeValidation.mjs';
export { useReasoning } from './query/useReasoning.mjs';
export { useDeltaQuery } from './query/useDeltaQuery.mjs';

// Knowledge Hooks
export { useKnowledgeHook } from './knowledge-hooks/useKnowledgeHook.mjs';
export { useHookManager } from './knowledge-hooks/useHookManager.mjs';
export { useHookRegistry } from './knowledge-hooks/useHookRegistry.mjs';
export { useHookExecution } from './knowledge-hooks/useHookExecution.mjs';

// Storage Hooks
export { useIndexedDBStore } from './storage/useIndexedDBStore.mjs';
export { useQuadStore } from './storage/useQuadStore.mjs';
export { useTransaction } from './storage/useTransaction.mjs';
export { useAuditTrail } from './storage/useAuditTrail.mjs';

// Caching Hooks
export { useQueryCache } from './cache/useQueryCache.mjs';
export { useMemoizedQuery } from './cache/useMemoizedQuery.mjs';
export { useCacheStats } from './cache/useCacheStats.mjs';

// Effect Hooks
export { useKnowledgeEffect } from './effects/useKnowledgeEffect.mjs';
export { useDeltaTracking } from './effects/useDeltaTracking.mjs';
export { useGraphListener } from './effects/useGraphListener.mjs';

// Utility Hooks
export { useNamespaces } from './utils/useNamespaces.mjs';
export { useValidation } from './utils/useValidation.mjs';
export { useDebug } from './utils/useDebug.mjs';
export { usePerformanceTracking } from './utils/usePerformanceTracking.mjs';

// Context & Providers
export {
  KnowledgeEngineProvider,
  KnowledgeEngineContext
} from './context/KnowledgeEngineProvider.mjs';
export { useKnowledgeEngineContext } from './context/useKnowledgeEngineContext.mjs';
export {
  useConfigContext,
  ConfigProvider,
  ConfigContext
} from './context/useConfigContext.mjs';

// Batch Operations
export { useBatchOperations } from './batch/useBatchOperations.mjs';
export { useOptimizedBatch } from './batch/useOptimizedBatch.mjs';
