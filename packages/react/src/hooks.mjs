/**
 * @fileoverview All hooks export (for tree-shaking)
 * @module react-hooks/hooks
 */

// Core
export { useKnowledgeEngine } from './core/useKnowledgeEngine.mjs';
export { useStore } from './core/useStore.mjs';
export { useTriples } from './core/useTriples.mjs';
export { useGraphs } from './core/useGraphs.mjs';
export { useTerms } from './core/useTerms.mjs';

// Query
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

// Storage
// useIndexedDBStore removed - use useOfflineStore from './composition/use-offline-store.mjs' instead
export { useQuadStore } from './storage/useQuadStore.mjs';
export { useTransaction } from './storage/useTransaction.mjs';
export { useAuditTrail } from './storage/useAuditTrail.mjs';

// Cache
export { useQueryCache } from './cache/useQueryCache.mjs';
export { useMemoizedQuery } from './cache/useMemoizedQuery.mjs';
export { useCacheStats } from './cache/useCacheStats.mjs';

// Effects
export { useKnowledgeEffect } from './effects/useKnowledgeEffect.mjs';
export { useDeltaTracking } from './effects/useDeltaTracking.mjs';
export { useGraphListener } from './effects/useGraphListener.mjs';

// Utils
export { useNamespaces } from './utils/useNamespaces.mjs';
export { useValidation } from './utils/useValidation.mjs';
export { useDebug } from './utils/useDebug.mjs';
export { usePerformanceTracking } from './utils/usePerformanceTracking.mjs';

// Batch
export { useBatchOperations } from './batch/useBatchOperations.mjs';
export { useOptimizedBatch } from './batch/useOptimizedBatch.mjs';
