/**
 * @file use-sparql-editor.mjs
 * @description React hook for SPARQL editor functionality
 */

import { useState, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for SPARQL editor with syntax highlighting and validation
 *
 * @param {Object} config - SPARQL editor configuration
 * @returns {Object} Editor state and operations
 *
 * @example
 * const {
 *   query,
 *   setQuery,
 *   validate,
 *   execute,
 *   format,
 *   errors
 * } = useSPARQLEditor();
 */
export function useSPARQLEditor(config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [query, setQuery] = useState('');
  const [errors, setErrors] = useState([]);
  const [suggestions, setSuggestions] = useState([]);
  const [loading, setLoading] = useState(false);

  const validate = useCallback((sparql) => {
    const validationErrors = [];

    if (!sparql.toUpperCase().includes('SELECT') &&
        !sparql.toUpperCase().includes('CONSTRUCT') &&
        !sparql.toUpperCase().includes('ASK')) {
      validationErrors.push({ message: 'Missing query type', line: 0 });
    }

    if (!sparql.includes('WHERE')) {
      validationErrors.push({ message: 'Missing WHERE clause', line: 0 });
    }

    setErrors(validationErrors);
    return validationErrors.length === 0;
  }, []);

  const execute = useCallback(async (sparql = query) => {
    try {
      setLoading(true);
      const result = await engine.query(sparql);
      setLoading(false);
      return result;
    } catch (err) {
      setErrors([{ message: err.message, line: 0 }]);
      setLoading(false);
      throw err;
    }
  }, [engine, query]);

  const format = useCallback((sparql) => {
    return sparql
      .replace(/\s+/g, ' ')
      .replace(/\s*{\s*/g, ' {\n  ')
      .replace(/\s*}\s*/g, '\n}')
      .trim();
  }, []);

  return { query, setQuery, validate, execute, format, errors, suggestions, loading };
}
