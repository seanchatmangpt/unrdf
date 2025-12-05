/**
 * @fileoverview useKnowledgeHook - Hook for registering and managing a single knowledge hook
 * @module react-hooks/knowledge-hooks/useKnowledgeHook
 *
 * @description
 * Hook for registering and managing a single knowledge hook with lifecycle management.
 *
 * @example
 * ```jsx
 * import { useKnowledgeHook } from 'unrdf/react-hooks';
 *
 * function ValidationHook() {
 *   const { register, unregister, status } = useKnowledgeHook({
 *     meta: { name: 'validator', version: '1.0.0' },
 *     when: { event: 'pre-transaction' },
 *     run: async (event) => {
 *       // Validation logic
 *       return { success: true };
 *     }
 *   });
 *
 *   return <div>Hook status: {status}</div>;
 * }
 * ```
 */

import { useState, useEffect, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

/**
 * Hook for managing a single knowledge hook
 *
 * @param {Object} hookDefinition - Knowledge hook definition
 * @param {Object} [options] - Hook options
 * @param {boolean} [options.autoRegister=true] - Auto-register on mount
 * @param {boolean} [options.autoUnregister=true] - Auto-unregister on unmount
 * @returns {Object} Hook management operations
 */
export function useKnowledgeHook(hookDefinition, options = {}) {
  const { autoRegister = true, autoUnregister = true } = options;
  const { addKnowledgeHook, removeKnowledgeHook, engine } = useKnowledgeEngineContext();

  const [isRegistered, setIsRegistered] = useState(false);
  const [error, setError] = useState(null);
  const [lastExecution, setLastExecution] = useState(null);

  /**
   * Register the knowledge hook
   */
  const register = useCallback(() => {
    if (!engine || !hookDefinition) {
      return false;
    }

    try {
      addKnowledgeHook(hookDefinition);
      setIsRegistered(true);
      setError(null);
      return true;
    } catch (err) {
      console.error('[useKnowledgeHook] Registration failed:', err);
      setError(err);
      return false;
    }
  }, [engine, hookDefinition, addKnowledgeHook]);

  /**
   * Unregister the knowledge hook
   */
  const unregister = useCallback(() => {
    if (!engine || !hookDefinition?.meta?.name) {
      return false;
    }

    try {
      removeKnowledgeHook(hookDefinition.meta.name);
      setIsRegistered(false);
      return true;
    } catch (err) {
      console.error('[useKnowledgeHook] Unregistration failed:', err);
      setError(err);
      return false;
    }
  }, [engine, hookDefinition, removeKnowledgeHook]);

  /**
   * Manually execute the hook
   */
  const execute = useCallback(
    async (event) => {
      if (!hookDefinition?.run) {
        throw new Error('[useKnowledgeHook] Hook has no run function');
      }

      try {
        const result = await hookDefinition.run(event);
        setLastExecution({
          timestamp: Date.now(),
          result,
          success: true,
        });
        return result;
      } catch (err) {
        setLastExecution({
          timestamp: Date.now(),
          error: err,
          success: false,
        });
        throw err;
      }
    },
    [hookDefinition]
  );

  // Auto-register and auto-unregister
  useEffect(() => {
    if (autoRegister) {
      register();
    }

    return () => {
      if (autoUnregister && isRegistered) {
        unregister();
      }
    };
  }, [autoRegister, autoUnregister]);

  return {
    register,
    unregister,
    execute,
    isRegistered,
    error,
    lastExecution,
    status: isRegistered ? 'active' : 'inactive',
  };
}
