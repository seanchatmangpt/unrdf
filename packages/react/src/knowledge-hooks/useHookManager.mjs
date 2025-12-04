/**
 * @fileoverview useHookManager - Hook for managing multiple knowledge hooks
 * @module react-hooks/knowledge-hooks/useHookManager
 *
 * @description
 * Hook for managing multiple knowledge hooks with batch operations.
 *
 * @example
 * ```jsx
 * import { useHookManager } from 'unrdf/react-hooks';
 *
 * function HookManager() {
 *   const { hooks, registerHooks, unregisterAll, stats } = useHookManager();
 *
 *   const handleRegisterAll = () => {
 *     registerHooks([hook1, hook2, hook3]);
 *   };
 *
 *   return (
 *     <div>
 *       <h3>Hooks: {stats.total}</h3>
 *       <button onClick={handleRegisterAll}>Register All</button>
 *     </div>
 *   );
 * }
 * ```
 */

import { useState, useCallback, useMemo } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

/**
 * Hook for managing multiple knowledge hooks
 *
 * @returns {Object} Hook manager operations
 */
export function useHookManager() {
  const { _engine, addKnowledgeHook, removeKnowledgeHook } = useKnowledgeEngineContext();

  const [registeredHooks, setRegisteredHooks] = useState(new Map());
  const [errors, setErrors] = useState([]);

  /**
   * Register a single hook
   */
  const registerHook = useCallback(
    (hook) => {
      if (!hook?.meta?.name) {
        throw new TypeError('[useHookManager] Hook must have meta.name');
      }

      try {
        addKnowledgeHook(hook);
        setRegisteredHooks((prev) => new Map(prev).set(hook.meta.name, hook));
        return true;
      } catch (err) {
        console.error('[useHookManager] Failed to register hook:', err);
        setErrors((prev) => [...prev, { hook: hook.meta.name, error: err }]);
        return false;
      }
    },
    [addKnowledgeHook]
  );

  /**
   * Register multiple hooks
   */
  const registerHooks = useCallback(
    (hooks) => {
      const results = [];
      for (const hook of hooks) {
        const success = registerHook(hook);
        results.push({ hook: hook.meta.name, success });
      }
      return results;
    },
    [registerHook]
  );

  /**
   * Unregister a single hook
   */
  const unregisterHook = useCallback(
    (hookName) => {
      try {
        removeKnowledgeHook(hookName);
        setRegisteredHooks((prev) => {
          const updated = new Map(prev);
          updated.delete(hookName);
          return updated;
        });
        return true;
      } catch (err) {
        console.error('[useHookManager] Failed to unregister hook:', err);
        return false;
      }
    },
    [removeKnowledgeHook]
  );

  /**
   * Unregister all hooks
   */
  const unregisterAll = useCallback(() => {
    for (const hookName of registeredHooks.keys()) {
      unregisterHook(hookName);
    }
    setRegisteredHooks(new Map());
  }, [registeredHooks, unregisterHook]);

  /**
   * Get hook by name
   */
  const getHook = useCallback(
    (hookName) => {
      return registeredHooks.get(hookName);
    },
    [registeredHooks]
  );

  /**
   * Check if hook is registered
   */
  const isRegistered = useCallback(
    (hookName) => {
      return registeredHooks.has(hookName);
    },
    [registeredHooks]
  );

  /**
   * Clear errors
   */
  const clearErrors = useCallback(() => {
    setErrors([]);
  }, []);

  /**
   * Get statistics
   */
  const stats = useMemo(
    () => ({
      total: registeredHooks.size,
      hooks: Array.from(registeredHooks.keys()),
      errorCount: errors.length,
    }),
    [registeredHooks, errors]
  );

  /**
   * Get all hooks as array
   */
  const hooks = useMemo(() => Array.from(registeredHooks.values()), [registeredHooks]);

  return {
    hooks,
    registeredHooks,
    registerHook,
    registerHooks,
    unregisterHook,
    unregisterAll,
    getHook,
    isRegistered,
    errors,
    clearErrors,
    stats,
  };
}
