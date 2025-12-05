/**
 * @file use-policy-pack.mjs
 * @description React hook for managing SHACL policy packs
 */

import { useState, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for loading and managing SHACL policy packs
 *
 * @param {Object} config - Policy pack configuration
 * @returns {Object} Policy pack state and operations
 *
 * @example
 * const {
 *   loadPolicyPack,
 *   validate,
 *   policies,
 *   violations
 * } = usePolicyPack();
 */
export function usePolicyPack(_config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [policies, setPolicies] = useState([]);
  const [violations, setViolations] = useState([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const loadPolicyPack = useCallback(
    async (packUri) => {
      try {
        setLoading(true);
        const shapes = await engine.query(`
        SELECT * WHERE {
          GRAPH <${packUri}> {
            ?shape a sh:NodeShape
          }
        }
      `);

        setPolicies(shapes.map((b) => b.shape.value));
        setLoading(false);
        return shapes;
      } catch (err) {
        setError(err);
        setLoading(false);
        throw err;
      }
    },
    [engine]
  );

  const validate = useCallback(
    async (_data) => {
      try {
        setLoading(true);
        const results = [];

        for (const policy of policies) {
          // Mock validation
          const result = {
            policy,
            conforms: Math.random() > 0.2,
            violations: [],
          };
          results.push(result);
        }

        const allViolations = results.flatMap((r) => r.violations);
        setViolations(allViolations);
        setLoading(false);
        return results;
      } catch (err) {
        setError(err);
        setLoading(false);
        throw err;
      }
    },
    [policies]
  );

  return { loadPolicyPack, validate, policies, violations, loading, error };
}
