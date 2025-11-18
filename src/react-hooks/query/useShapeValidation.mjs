/**
 * @fileoverview useShapeValidation - React hook for SHACL validation
 * @module react-hooks/query/useShapeValidation
 *
 * @description
 * Hook for validating RDF data against SHACL shapes with reactive results.
 *
 * @example
 * ```jsx
 * import { useShapeValidation } from 'unrdf/react-hooks';
 *
 * function ValidationStatus() {
 *   const { conforms, violations, validate, loading } = useShapeValidation(shapes);
 *
 *   useEffect(() => {
 *     validate();
 *   }, [store]);
 *
 *   return (
 *     <div>
 *       <h3>Validation: {conforms ? 'PASS' : 'FAIL'}</h3>
 *       {violations.map((v, i) => (
 *         <div key={i}>{v.message}</div>
 *       ))}
 *     </div>
 *   );
 * }
 * ```
 */

import { useState, useCallback, useEffect } from 'react';
import { useKnowledgeEngineContext } from '../context/useKnowledgeEngineContext.mjs';

/**
 * Hook for SHACL shape validation
 *
 * @param {string|Store} shapes - SHACL shapes as Turtle string or Store
 * @param {Object} [options] - Validation options
 * @param {boolean} [options.autoValidate=false] - Automatically validate on store changes
 * @param {Function} [options.onValidate] - Callback after validation
 * @returns {Object} Validation state and operations
 */
export function useShapeValidation(shapes, options = {}) {
  const { autoValidate = false, onValidate } = options;
  const { engine, store } = useKnowledgeEngineContext();

  const [conforms, setConforms] = useState(null);
  const [violations, setViolations] = useState([]);
  const [report, setReport] = useState(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  /**
   * Validate store against shapes
   */
  const validate = useCallback(async () => {
    if (!engine || !store || !shapes) {
      setLoading(false);
      return null;
    }

    try {
      setLoading(true);
      setError(null);

      const result = await engine.validateShacl(store, shapes);

      setConforms(result.conforms);
      setViolations(result.results || []);
      setReport(result);
      setLoading(false);

      if (onValidate) {
        onValidate(result);
      }

      return result;
    } catch (err) {
      console.error('[useShapeValidation] Validation failed:', err);
      setError(err);
      setLoading(false);
      throw err;
    }
  }, [engine, store, shapes, onValidate]);

  /**
   * Get violations by severity
   */
  const getViolationsBySeverity = useCallback((severity) => {
    return violations.filter(v => v.severity?.value?.includes(severity));
  }, [violations]);

  /**
   * Get violations by focus node
   */
  const getViolationsByFocusNode = useCallback((focusNode) => {
    const nodeValue = typeof focusNode === 'string' ? focusNode : focusNode?.value;
    return violations.filter(v => v.focusNode?.value === nodeValue);
  }, [violations]);

  /**
   * Check if validation passed
   */
  const isValid = conforms === true;

  /**
   * Get violation count
   */
  const violationCount = violations.length;

  // Auto-validate if enabled
  useEffect(() => {
    if (autoValidate && engine && store && shapes) {
      validate();
    }
  }, [autoValidate, engine, store, shapes]);

  return {
    conforms,
    violations,
    report,
    loading,
    error,
    validate,
    isValid,
    violationCount,
    getViolationsBySeverity,
    getViolationsByFocusNode
  };
}
