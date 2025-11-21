/**
 * @file use-nlp-query-builder.mjs
 * @description React hook for natural language to SPARQL query conversion
 */

import { useState, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for converting natural language queries to SPARQL
 *
 * @param {Object} config - NLP query builder configuration
 * @param {string} [config.model] - NLP model to use
 * @returns {Object} Query builder state and operations
 *
 * @example
 * const {
 *   buildQuery,
 *   executeNaturalQuery,
 *   suggestions,
 *   queryHistory
 * } = useNLPQueryBuilder({
 *   model: 'semantic-parser-v1'
 * });
 */
export function useNLPQueryBuilder(config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [query, setQuery] = useState('');
  const [suggestions, setSuggestions] = useState([]);
  const [queryHistory, setQueryHistory] = useState([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const buildQuery = useCallback(async (naturalLanguage) => {
    try {
      setLoading(true);
      const sparql = parseNaturalLanguage(naturalLanguage);
      setQuery(sparql);
      setLoading(false);
      return sparql;
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, []);

  const executeNaturalQuery = useCallback(async (naturalLanguage) => {
    try {
      setLoading(true);
      const sparql = await buildQuery(naturalLanguage);
      const result = await engine.query(sparql);

      setQueryHistory(prev => [...prev, {
        natural: naturalLanguage,
        sparql,
        timestamp: new Date().toISOString()
      }]);

      setLoading(false);
      return result;
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, [engine, buildQuery]);

  function parseNaturalLanguage(text) {
    const lower = text.toLowerCase();

    if (lower.includes('show') || lower.includes('list') || lower.includes('get')) {
      const match = lower.match(/(?:show|list|get)\s+(?:all\s+)?(\w+)/);
      if (match) {
        const type = match[1];
        return `SELECT * WHERE { ?s a <http://schema.org/${capitalize(type)}> } LIMIT 100`;
      }
    }

    if (lower.includes('count')) {
      const match = lower.match(/count\s+(\w+)/);
      if (match) {
        const type = match[1];
        return `SELECT (COUNT(?s) as ?count) WHERE { ?s a <http://schema.org/${capitalize(type)}> }`;
      }
    }

    return 'SELECT * WHERE { ?s ?p ?o } LIMIT 10';
  }

  function capitalize(str) {
    return str.charAt(0).toUpperCase() + str.slice(1);
  }

  return { buildQuery, executeNaturalQuery, query, suggestions, queryHistory, loading, error };
}
