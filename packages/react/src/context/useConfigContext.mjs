/**
 * @fileoverview useConfigContext - Hook for accessing configuration
 * @module react-hooks/context/useConfigContext
 *
 * @description
 * Hook for accessing and managing UNRDF configuration within React context.
 *
 * @example
 * ```jsx
 * import { useConfigContext } from 'unrdf/react-hooks';
 *
 * function ConfigPanel() {
 *   const { config, updateConfig } = useConfigContext();
 *
 *   return (
 *     <div>
 *       <h3>Config</h3>
 *       <p>Knowledge Hooks: {config.enableKnowledgeHooks ? 'ON' : 'OFF'}</p>
 *     </div>
 *   );
 * }
 * ```
 */

import { useState, useCallback, useMemo, createContext, useContext } from 'react';

/**
 * Configuration Context
 */
export const ConfigContext = createContext(null);

/**
 * Hook to access configuration context
 *
 * @returns {Object} Configuration state and operations
 */
export function useConfigContext() {
  const context = useContext(ConfigContext);

  if (!context) {
    // Return default config if not within provider
    return {
      config: {
        enableKnowledgeHooks: true,
        enableObservability: true,
        strictMode: false,
      },
      updateConfig: () => {
        console.warn('[useConfigContext] Not within ConfigProvider, config cannot be updated');
      },
      resetConfig: () => {
        console.warn('[useConfigContext] Not within ConfigProvider, config cannot be reset');
      },
    };
  }

  return context;
}

/**
 * Configuration Provider Component
 *
 * @param {Object} props - Component props
 * @param {Object} [props.initialConfig] - Initial configuration
 * @param {React.ReactNode} props.children - Child components
 * @returns {React.ReactElement} Provider component
 */
export function ConfigProvider({ initialConfig = {}, children }) {
  const defaultConfig = useMemo(
    () => ({
      enableKnowledgeHooks: true,
      enableObservability: true,
      strictMode: false,
      ...initialConfig,
    }),
    [initialConfig]
  );

  const [config, setConfig] = useState(defaultConfig);

  /**
   * Update configuration
   */
  const updateConfig = useCallback((updates) => {
    setConfig((prev) => ({ ...prev, ...updates }));
  }, []);

  /**
   * Reset configuration to defaults
   */
  const resetConfig = useCallback(() => {
    setConfig(defaultConfig);
  }, [defaultConfig]);

  const contextValue = useMemo(
    () => ({
      config,
      updateConfig,
      resetConfig,
    }),
    [config, updateConfig, resetConfig]
  );

  return <ConfigContext.Provider value={contextValue}>{children}</ConfigContext.Provider>;
}
