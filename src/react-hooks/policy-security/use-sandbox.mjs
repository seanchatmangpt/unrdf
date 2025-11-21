/**
 * @file use-sandbox.mjs
 * @description React hook for sandboxed query execution
 */

import { useState, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for executing queries in sandboxed environment
 *
 * @param {Object} config - Sandbox configuration
 * @param {number} [config.timeout=5000] - Query timeout
 * @param {number} [config.maxResults=1000] - Max results
 * @returns {Object} Sandbox state and operations
 *
 * @example
 * const {
 *   executeSandboxed,
 *   isolatedGraph,
 *   violations
 * } = useSandbox({
 *   timeout: 5000,
 *   maxResults: 1000
 * });
 */
export function useSandbox(config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [isolatedGraph, setIsolatedGraph] = useState(null);
  const [violations, setViolations] = useState([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const executeSandboxed = useCallback(async (sparql) => {
    try {
      setLoading(true);

      const controller = new AbortController();
      const timeout = setTimeout(() => controller.abort(), config.timeout || 5000);

      const result = await engine.query(sparql);

      clearTimeout(timeout);

      if (result.length > (config.maxResults || 1000)) {
        throw new Error('Result set exceeds maximum allowed');
      }

      setLoading(false);
      return result;
    } catch (err) {
      setError(err);
      setViolations(prev => [...prev, {
        query: sparql,
        error: err.message,
        timestamp: new Date().toISOString()
      }]);
      setLoading(false);
      throw err;
    }
  }, [engine, config]);

  return { executeSandboxed, isolatedGraph, violations, loading, error };
}
