/**
 * @file use-reasoning-session.mjs
 * @description React hook for OWL reasoning sessions
 */

import { useState, useCallback, useEffect } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for managing OWL reasoning sessions with inference
 *
 * @param {Object} config - Reasoning configuration
 * @param {string[]} [config.rules] - Inference rules
 * @returns {Object} Reasoning session state and operations
 *
 * @example
 * const {
 *   infer,
 *   inferredTriples,
 *   applyRules,
 *   materialize
 * } = useReasoningSession({
 *   rules: ['rdfs', 'owl']
 * });
 */
export function useReasoningSession(config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [inferredTriples, setInferredTriples] = useState([]);
  const [rules, setRules] = useState(config.rules || []);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const infer = useCallback(async (subject) => {
    try {
      setLoading(true);

      const directTriples = await engine.query(`
        SELECT * WHERE { <${subject}> ?p ?o }
      `);

      const inferred = [];

      for (const rule of rules) {
        if (rule === 'rdfs:subClassOf') {
          const subClass = await engine.query(`
            SELECT ?super WHERE {
              <${subject}> rdfs:subClassOf ?super
            }
          `);
          inferred.push(...subClass);
        }

        if (rule === 'owl:sameAs') {
          const sameAs = await engine.query(`
            SELECT ?same WHERE {
              <${subject}> owl:sameAs ?same
            }
          `);
          inferred.push(...sameAs);
        }
      }

      setInferredTriples(inferred);
      setLoading(false);
      return inferred;
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, [engine, rules]);

  const materialize = useCallback(async () => {
    try {
      setLoading(true);

      for (const triple of inferredTriples) {
        await engine.insert([triple]);
      }

      setLoading(false);
      return { materialized: inferredTriples.length };
    } catch (err) {
      setError(err);
      setLoading(false);
      throw err;
    }
  }, [engine, inferredTriples]);

  return { infer, materialize, inferredTriples, rules, setRules, loading, error };
}
