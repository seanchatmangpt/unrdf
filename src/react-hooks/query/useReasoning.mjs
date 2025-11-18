/**
 * @fileoverview useReasoning - React hook for N3 rule-based reasoning
 * @module react-hooks/query/useReasoning
 *
 * @description
 * Hook for applying N3 reasoning rules to derive new knowledge from RDF data.
 *
 * @example
 * ```jsx
 * import { useReasoning } from 'unrdf/react-hooks';
 *
 * function ReasoningComponent() {
 *   const { derivedTriples, apply, loading } = useReasoning(rules);
 *
 *   const handleReason = () => {
 *     apply().then(results => {
 *       console.log(`Derived ${results.length} new triples`);
 *     });
 *   };
 *
 *   return <button onClick={handleReason}>Apply Reasoning</button>;
 * }
 * ```
 */

import { useState, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

/**
 * Hook for N3 rule-based reasoning
 *
 * @param {string|Store} rules - N3 rules as string or Store
 * @param {Object} [options] - Reasoning options
 * @param {boolean} [options.addToStore=false] - Add derived triples to store
 * @param {Function} [options.onComplete] - Callback after reasoning
 * @returns {Object} Reasoning state and operations
 */
export function useReasoning(rules, options = {}) {
  const { addToStore = false, onComplete } = options;
  const { engine, store } = useKnowledgeEngineContext();

  const [derivedTriples, setDerivedTriples] = useState([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  /**
   * Apply reasoning rules
   */
  const apply = useCallback(async () => {
    if (!engine || !store || !rules) {
      setLoading(false);
      return [];
    }

    try {
      setLoading(true);
      setError(null);

      const result = await engine.reason(store, rules);
      const derived = result.triples || [];

      setDerivedTriples(derived);

      // Optionally add to store
      if (addToStore && derived.length > 0) {
        store.addQuads(derived);
      }

      setLoading(false);

      if (onComplete) {
        onComplete(derived);
      }

      return derived;
    } catch (err) {
      console.error('[useReasoning] Reasoning failed:', err);
      setError(err);
      setLoading(false);
      throw err;
    }
  }, [engine, store, rules, addToStore, onComplete]);

  /**
   * Clear derived triples
   */
  const clear = useCallback(() => {
    setDerivedTriples([]);
    setError(null);
  }, []);

  /**
   * Get count of derived triples
   */
  const count = derivedTriples.length;

  return {
    derivedTriples,
    loading,
    error,
    apply,
    clear,
    count
  };
}
