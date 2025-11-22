/**
 * @fileoverview KnowledgeEngineProvider - React context provider for UNRDF
 * @module react-hooks/context/KnowledgeEngineProvider
 *
 * @description
 * Context provider component that makes the Knowledge Engine available to all child components.
 * Must wrap your React application to enable UNRDF hooks.
 *
 * @example
 * ```jsx
 * import { KnowledgeEngineProvider } from 'unrdf/react-hooks';
 *
 * function App() {
 *   return (
 *     <KnowledgeEngineProvider
 *       config={{
 *         enableKnowledgeHooks: true,
 *         enableObservability: true
 *       }}
 *     >
 *       <MyComponents />
 *     </KnowledgeEngineProvider>
 *   );
 * }
 * ```
 */

import _React, { createContext, useMemo } from 'react';
import { useKnowledgeEngine } from '../core/useKnowledgeEngine.mjs';

/**
 * Knowledge Engine React Context
 */
export const KnowledgeEngineContext = createContext(null);

/**
 * Knowledge Engine Provider Props
 * @typedef {Object} KnowledgeEngineProviderProps
 * @property {Object} [config] - Engine configuration
 * @property {React.ReactNode} children - Child components
 */

/**
 * Provider component for Knowledge Engine context
 *
 * @param {KnowledgeEngineProviderProps} props - Component props
 * @returns {React.ReactElement} Provider component
 */
export function KnowledgeEngineProvider({ config = {}, children }) {
  const engineState = useKnowledgeEngine(config);

  const contextValue = useMemo(
    () => ({
      ...engineState,
    }),
    [engineState]
  );

  return (
    <KnowledgeEngineContext.Provider value={contextValue}>
      {children}
    </KnowledgeEngineContext.Provider>
  );
}
