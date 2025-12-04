/**
 * @file use-semantic-analyzer.mjs
 * @description React hook for semantic analysis of RDF graphs
 */

import { useState, useCallback, _useEffect } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for analyzing semantic relationships and patterns in RDF graphs
 *
 * @param {Object} config - Semantic analyzer configuration
 * @param {string[]} [config.ontologies] - Ontologies to use for analysis
 * @param {boolean} [config.inferRelationships=true] - Auto-infer relationships
 * @returns {Object} Semantic analysis state and operations
 *
 * @example
 * const {
 *   analyze,
 *   relationships,
 *   concepts,
 *   similarity,
 *   insights
 * } = useSemanticAnalyzer({
 *   ontologies: ['schema.org', 'foaf'],
 *   inferRelationships: true
 * });
 */
export function useSemanticAnalyzer(_config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [relationships, setRelationships] = useState([]);
  const [concepts, _setConcepts] = useState([]);
  const [insights, _setInsights] = useState([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const analyze = useCallback(
    async (subject) => {
      try {
        setLoading(true);
        const result = await engine.query(`
        SELECT ?p ?o WHERE { <${subject}> ?p ?o }
      `);

        const rels = result.map((b) => ({
          predicate: b.p.value,
          object: b.o.value,
          type: inferType(b.p.value),
        }));

        setRelationships(rels);
        setLoading(false);
        return rels;
      } catch (err) {
        setError(err);
        setLoading(false);
        throw err;
      }
    },
    [engine]
  );

  const findSimilar = useCallback(
    async (entity, _threshold = 0.7) => {
      try {
        setLoading(true);
        // Semantic similarity using shared properties
        const result = await engine.query(`
        SELECT ?similar (COUNT(?p) as ?score) WHERE {
          <${entity}> ?p ?o .
          ?similar ?p ?o .
          FILTER(?similar != <${entity}>)
        }
        GROUP BY ?similar
        HAVING(COUNT(?p) > 2)
        ORDER BY DESC(?score)
        LIMIT 10
      `);
        setLoading(false);
        return result;
      } catch (err) {
        setError(err);
        setLoading(false);
        throw err;
      }
    },
    [engine]
  );

  function inferType(predicate) {
    if (predicate.includes('type')) return 'classification';
    if (predicate.includes('name') || predicate.includes('label')) return 'identification';
    if (predicate.includes('related') || predicate.includes('sameAs')) return 'equivalence';
    return 'property';
  }

  return {
    analyze,
    relationships,
    concepts,
    insights,
    findSimilar,
    loading,
    error,
  };
}
