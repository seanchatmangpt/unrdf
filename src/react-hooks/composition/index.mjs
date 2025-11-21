/**
 * @file composition/index.mjs
 * @description Hook composition utilities - combining multiple hooks for common patterns
 */

export {
  useKnowledgeStack,
  useCRUDStack,
  useDashboardStack,
  useProductionStack
} from './use-knowledge-stack.mjs';

export {
  useOfflineStore
} from './use-offline-store.mjs';
