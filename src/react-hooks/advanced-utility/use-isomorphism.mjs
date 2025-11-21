/**
 * @file use-isomorphism.mjs
 * @description React hook for graph isomorphism detection
 */

import { useState, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for detecting graph isomorphism (structural equivalence)
 *
 * @param {Object} config - Isomorphism configuration
 * @returns {Object} Isomorphism state and operations
 *
 * @example
 * const {
 *   checkIsomorphism,
 *   isIsomorphic,
 *   mapping,
 *   canonicalize
 * } = useIsomorphism();
 */
export function useIsomorphism(config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [isIsomorphic, setIsIsomorphic] = useState(false);
  const [mapping, setMapping] = useState(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const checkIsomorphism = useCallback(async (graph1Uri, graph2Uri) => {
    try {
      setLoading(true);

      const quads1 = await engine.match(null, null, null, graph1Uri);
      const quads2 = await engine.match(null, null, null, graph2Uri);

      if (quads1.length !== quads2.length) {
        setIsIsomorphic(false);
        setLoading(false);
        return false;
      }

      const sig1 = computeSignature(quads1);
      const sig2 = computeSignature(quads2);

      const result = sig1 === sig2;
      setIsIsomorphic(result);
      setLoading(false);
      return result;
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, [engine]);

  const canonicalize = useCallback(async (graphUri) => {
    try {
      setLoading(true);
      const quads = await engine.match(null, null, null, graphUri);
      const canonical = quads.sort((a, b) =>
        quadToString(a).localeCompare(quadToString(b))
      );
      setLoading(false);
      return canonical;
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, [engine]);

  function computeSignature(quads) {
    const sorted = quads
      .map(q => quadToString(q))
      .sort()
      .join('|');
    return sorted;
  }

  function quadToString(quad) {
    return `${quad.subject.value} ${quad.predicate.value} ${quad.object.value}`;
  }

  return { checkIsomorphism, canonicalize, isIsomorphic, mapping, loading, error };
}
