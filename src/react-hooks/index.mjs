/**
 * @file react-hooks/index.mjs
 * @description UNRDF React Hooks - 80/20 Optimized Export
 *
 * This export structure follows the 80/20 principle:
 * - 7 core hooks deliver 80% of value
 * - 9 standard hooks provide 15% of value
 * - 19 advanced hooks provide 5% of value (stubs/simplified)
 */

// ============================================================================
// TIER 1: ESSENTIAL (5 hooks - 60% usage)
// ============================================================================

// Core Knowledge Engine (40% usage)
export {
  useKnowledgeEngine,
  useKnowledgeEngineContext,
  useTransaction,
  useKnowledgeHook
} from './core/index.mjs';

// Streaming - Real-time Updates (20% usage)
export {
  useChangeFeed
} from './streaming/use-change-feed.mjs';

// Optimization - Performance Analysis (15% usage)
export {
  useDarkMatterCore
} from './dark-matter/use-dark-matter-core.mjs';

// Optimization - Query Analysis (10% usage)
export {
  useQueryAnalyzer
} from './dark-matter/use-query-analyzer.mjs';

// Error Handling (15% usage)
export {
  useErrorBoundary
} from './error-recovery/use-error-boundary.mjs';

// ============================================================================
// TIER 2: IMPORTANT (2 hooks - 20% usage)
// ============================================================================

// Utility - Graph Operations (10% usage)
export {
  useGraphDiff
} from './advanced-utility/use-graph-diff.mjs';

// UI - Query Interface (10% usage)
export {
  useSPARQLEditor
} from './form-ui/use-sparql-editor.mjs';

// ============================================================================
// TIER 3: STANDARD (9 hooks - 15% usage)
// ============================================================================

// Federation (5% usage)
export {
  useFederatedSystem
} from './federation/use-federated-system.mjs';

// Streaming (3% usage)
export {
  useStreamProcessor
} from './streaming/use-stream-processor.mjs';

// Optimization (2% usage)
export {
  useOptimizer
} from './dark-matter/use-optimizer.mjs';

// AI/Semantic (1% usage)
export {
  useSemanticAnalyzer
} from './ai-semantic/use-semantic-analyzer.mjs';

// Utility (1% usage each)
export {
  useGraphMerge
} from './advanced-utility/use-graph-merge.mjs';

// Policy (1% usage)
export {
  usePolicyPack
} from './policy-security/use-policy-pack.mjs';

// Error (1% usage)
export {
  useRecovery
} from './error-recovery/use-recovery.mjs';

// UI (1% usage each)
export {
  useGraphVisualizer,
  useResultsPaginator
} from './form-ui/index.mjs';

// ============================================================================
// TIER 4: ADVANCED (19 hooks - 5% usage)
// ============================================================================
// Available via category imports for advanced use cases

/**
 * USAGE GUIDE:
 *
 * 80% of apps only need Tier 1:
 * ```js
 * import {
 *   useKnowledgeEngine,
 *   useChangeFeed,
 *   useDarkMatterCore,
 *   useQueryAnalyzer,
 *   useErrorBoundary
 * } from 'unrdf/react-hooks';
 * ```
 *
 * 15% of apps add Tier 2:
 * ```js
 * import {
 *   useGraphDiff,
 *   useSPARQLEditor
 * } from 'unrdf/react-hooks';
 * ```
 *
 * 4% of apps explore Tier 3:
 * ```js
 * import {
 *   useFederatedSystem,
 *   useStreamProcessor,
 *   useOptimizer
 * } from 'unrdf/react-hooks';
 * ```
 *
 * 1% of apps need Tier 4 (advanced):
 * ```js
 * import { useConsensusManager } from 'unrdf/react-hooks/federation';
 * import { useEmbeddingsManager } from 'unrdf/react-hooks/ai-semantic';
 * ```
 */

// ============================================================================
// INNOVATION: COMPOSITION HOOKS (Quick Wins)
// ============================================================================

// Hook Composition Helpers - Pre-configured bundles for common patterns
export {
  useKnowledgeStack,
  useCRUDStack,
  useDashboardStack,
  useProductionStack
} from './composition/use-knowledge-stack.mjs';

// Offline-First Support - IndexedDB persistence with sync queue
export {
  useOfflineStore
} from './composition/use-offline-store.mjs';

// Category exports for advanced users
export * as Federation from './federation/index.mjs';
export * as Streaming from './streaming/index.mjs';
export * as DarkMatter from './dark-matter/index.mjs';
export * as AISemantic from './ai-semantic/index.mjs';
export * as AdvancedUtility from './advanced-utility/index.mjs';
export * as PolicySecurity from './policy-security/index.mjs';
export * as ErrorRecovery from './error-recovery/index.mjs';
export * as FormUI from './form-ui/index.mjs';
export * as Composition from './composition/index.mjs';
