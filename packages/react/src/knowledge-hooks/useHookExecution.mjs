/**
 * @fileoverview useHookExecution - Hook for executing knowledge hooks with lifecycle
 * @module react-hooks/knowledge-hooks/useHookExecution
 *
 * @description
 * Hook for executing knowledge hooks with full lifecycle management and result tracking.
 *
 * @example
 * ```jsx
 * import { useHookExecution } from 'unrdf/react-hooks';
 *
 * function ExecuteHook() {
 *   const { execute, result, loading, error, history } = useHookExecution();
 *
 *   const handleExecute = () => {
 *     execute('validator-hook', {
 *       payload: { data: '...' }
 *     });
 *   };
 *
 *   return (
 *     <div>
 *       <button onClick={handleExecute} disabled={loading}>
 *         Execute Hook
 *       </button>
 *       {result && <div>Success: {result.success}</div>}
 *     </div>
 *   );
 * }
 * ```
 */

import { useState, useCallback, useRef } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

/**
 * Hook for executing knowledge hooks with lifecycle
 *
 * @param {Object} [options] - Execution options
 * @param {number} [options.maxHistory=50] - Maximum execution history
 * @returns {Object} Execution operations and state
 */
export function useHookExecution(options = {}) {
  const { maxHistory = 50 } = options;
  const { engine } = useKnowledgeEngineContext();

  const [result, setResult] = useState(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const [history, setHistory] = useState([]);

  const executionCountRef = useRef(0);

  /**
   * Execute a knowledge hook
   */
  const execute = useCallback(
    async (hookName, event, execOptions = {}) => {
      if (!engine || !engine.executeKnowledgeHook) {
        throw new Error(
          '[useHookExecution] Engine not initialized or hook execution not available'
        );
      }

      const executionId = ++executionCountRef.current;

      try {
        setLoading(true);
        setError(null);

        const startTime = Date.now();
        const execResult = await engine.executeKnowledgeHook(hookName, event, execOptions);
        const duration = Date.now() - startTime;

        const historyEntry = {
          executionId,
          hookName,
          event,
          result: execResult,
          duration,
          timestamp: Date.now(),
          success: true,
        };

        setResult(execResult);
        setLoading(false);

        // Add to history
        setHistory((prev) => {
          const updated = [...prev, historyEntry];
          return updated.slice(-maxHistory);
        });

        return execResult;
      } catch (err) {
        console.error('[useHookExecution] Execution failed:', err);

        const historyEntry = {
          executionId,
          hookName,
          event,
          error: err,
          timestamp: Date.now(),
          success: false,
        };

        setError(err);
        setLoading(false);

        // Add to history
        setHistory((prev) => {
          const updated = [...prev, historyEntry];
          return updated.slice(-maxHistory);
        });

        throw err;
      }
    },
    [engine, maxHistory]
  );

  /**
   * Execute all registered hooks
   */
  const executeAll = useCallback(
    async (event, execOptions = {}) => {
      if (!engine || !engine.executeAllKnowledgeHooks) {
        throw new Error('[useHookExecution] Engine not initialized');
      }

      try {
        setLoading(true);
        setError(null);

        const results = await engine.executeAllKnowledgeHooks(event, execOptions);

        setResult(results);
        setLoading(false);

        return results;
      } catch (err) {
        console.error('[useHookExecution] Batch execution failed:', err);
        setError(err);
        setLoading(false);
        throw err;
      }
    },
    [engine]
  );

  /**
   * Clear execution results
   */
  const clear = useCallback(() => {
    setResult(null);
    setError(null);
  }, []);

  /**
   * Clear execution history
   */
  const clearHistory = useCallback(() => {
    setHistory([]);
    executionCountRef.current = 0;
  }, []);

  /**
   * Get successful executions
   */
  const getSuccessfulExecutions = useCallback(() => {
    return history.filter((entry) => entry.success);
  }, [history]);

  /**
   * Get failed executions
   */
  const getFailedExecutions = useCallback(() => {
    return history.filter((entry) => !entry.success);
  }, [history]);

  /**
   * Get execution by ID
   */
  const getExecutionById = useCallback(
    (executionId) => {
      return history.find((entry) => entry.executionId === executionId);
    },
    [history]
  );

  /**
   * Get executions by hook name
   */
  const getExecutionsByHook = useCallback(
    (hookName) => {
      return history.filter((entry) => entry.hookName === hookName);
    },
    [history]
  );

  return {
    execute,
    executeAll,
    result,
    loading,
    error,
    history,
    clear,
    clearHistory,
    getSuccessfulExecutions,
    getFailedExecutions,
    getExecutionById,
    getExecutionsByHook,
    stats: {
      totalExecutions: history.length,
      successful: history.filter((e) => e.success).length,
      failed: history.filter((e) => !e.success).length,
    },
  };
}
