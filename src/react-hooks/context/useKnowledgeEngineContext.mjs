/**
 * @fileoverview useKnowledgeEngineContext - Hook to access Knowledge Engine context
 * @module react-hooks/context/useKnowledgeEngineContext
 *
 * @description
 * Hook for accessing the Knowledge Engine instance from context.
 * Must be used within a KnowledgeEngineProvider.
 *
 * @example
 * ```jsx
 * import { useKnowledgeEngineContext } from 'unrdf/react-hooks';
 *
 * function MyComponent() {
 *   const { engine, store, query } = useKnowledgeEngineContext();
 *
 *   return <div>Store size: {store.size}</div>;
 * }
 * ```
 */

import { useContext } from 'react';
import { KnowledgeEngineContext } from './KnowledgeEngineProvider.mjs';

/**
 * Hook to access Knowledge Engine context
 *
 * @returns {Object} Knowledge Engine context value
 * @throws {Error} If used outside KnowledgeEngineProvider
 */
export function useKnowledgeEngineContext() {
  const context = useContext(KnowledgeEngineContext);

  if (!context) {
    throw new Error(
      '[useKnowledgeEngineContext] Must be used within KnowledgeEngineProvider'
    );
  }

  return context;
}
