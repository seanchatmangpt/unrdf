/**
 * @file use-real-time-validator.mjs
 * @description React hook for continuous SHACL validation stream
 * @since 3.2.0
 */

import { useState, useCallback, useEffect, useRef } from 'react';
import { useChangeFeed } from './use-change-feed.mjs';

/**
 * Hook for real-time SHACL validation on streaming graph changes
 *
 * @since 3.2.0
 * @param {Object} config - Validator configuration
 * @param {string} [config.shapeGraph] - SHACL shapes graph URI
 * @param {Function} [config.onViolation] - Callback for validation violations
 * @param {boolean} [config.autoValidate=true] - Auto-validate on changes
 * @param {number} [config.batchSize=5] - Validation batch size
 * @returns {Object} Validator state and operations
 * @throws {Error} When validator not initialized and validate() is called
 * @throws {Error} When SHACL shape graph fails to load
 * @throws {Error} When validateBatch receives invalid change objects
 * @performance SHACL validation is CPU-intensive - use larger batchSize for throughput,
 *   smaller for latency. Disable autoValidate and batch manually for high-throughput scenarios.
 *
 * @example
 * // Auto-validate with violation callback
 * const {
 *   violations,
 *   validChanges,
 *   invalidChanges,
 *   validate,
 *   clear
 * } = useRealTimeValidator({
 *   shapeGraph: 'http://example.org/shapes',
 *   onViolation: (violation) => {
 *     console.warn('Validation failed:', violation);
 *   },
 *   autoValidate: true
 * });
 *
 * @example
 * // Manual batch validation for control
 * const { validateBatch, stats } = useRealTimeValidator({
 *   shapeGraph: 'http://example.org/shapes',
 *   autoValidate: false
 * });
 * const results = await validateBatch(pendingChanges);
 */
export function useRealTimeValidator(config = {}) {
  const { changes, start, stop, isRunning } = useChangeFeed(config.changeFeed || {});
  const [violations, setViolations] = useState([]);
  const [validChanges, setValidChanges] = useState([]);
  const [invalidChanges, setInvalidChanges] = useState([]);
  const [stats, setStats] = useState({
    totalValidated: 0,
    passed: 0,
    failed: 0,
    passRate: 0
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const validatorRef = useRef(null);
  const validationQueueRef = useRef([]);
  const processingRef = useRef(false);

  // Initialize validator
  useEffect(() => {
    let mounted = true;

    async function initializeValidator() {
      try {
        setLoading(true);

        // Import validator module
        const { RealTimeValidator } = await import(
          '../../knowledge-engine/streaming/real-time-validator.mjs'
        );

        // Create validator
        const validator = new RealTimeValidator({
          shapeGraph: config.shapeGraph,
          onViolation: (violation) => {
            if (!mounted) return;

            setViolations(prev => [...prev, {
              ...violation,
              timestamp: new Date().toISOString()
            }]);

            config.onViolation?.(violation);
          }
        });

        if (!mounted) return;

        validatorRef.current = validator;
        setLoading(false);
      } catch (err) {
        if (!mounted) return;
        setError(err);
        setLoading(false);
      }
    }

    initializeValidator();

    return () => {
      mounted = false;
    };
  }, [config.shapeGraph]);

  // Process changes for validation
  useEffect(() => {
    if (!config.autoValidate || !validatorRef.current || !changes.length) return;

    const latestChange = changes[changes.length - 1];
    validationQueueRef.current.push(latestChange);

    // Process queue if not already processing
    if (!processingRef.current) {
      processValidationQueue();
    }
  }, [changes, config.autoValidate]);

  // Process validation queue
  const processValidationQueue = useCallback(async () => {
    if (processingRef.current || validationQueueRef.current.length === 0) return;

    processingRef.current = true;

    const batchSize = config.batchSize || 5;
    const batch = validationQueueRef.current.splice(0, batchSize);

    for (const change of batch) {
      try {
        await validateChange(change);
      } catch (err) {
        setError(err);
      }
    }

    processingRef.current = false;

    // Continue processing if more in queue
    if (validationQueueRef.current.length > 0) {
      setTimeout(processValidationQueue, 0);
    }
  }, [config.batchSize]);

  // Validate a single change
  const validateChange = useCallback(async (change) => {
    if (!validatorRef.current) {
      throw new Error('Validator not initialized');
    }

    try {
      const result = await validatorRef.current.validate({
        quads: change.quads,
        operation: change.operation,
        metadata: change.metadata
      });

      if (result.conforms) {
        setValidChanges(prev => [...prev, {
          change,
          validatedAt: new Date().toISOString()
        }]);
      } else {
        setInvalidChanges(prev => [...prev, {
          change,
          violations: result.violations,
          validatedAt: new Date().toISOString()
        }]);

        // Add violations
        setViolations(prev => [...prev, ...result.violations.map(v => ({
          ...v,
          changeId: change.id,
          timestamp: new Date().toISOString()
        }))]);
      }

      // Update stats
      setStats(prev => {
        const total = prev.totalValidated + 1;
        const passed = result.conforms ? prev.passed + 1 : prev.passed;
        const failed = result.conforms ? prev.failed : prev.failed + 1;
        return {
          totalValidated: total,
          passed,
          failed,
          passRate: Math.round((passed / total) * 100)
        };
      });

      return result;
    } catch (err) {
      setError(err);
      throw err;
    }
  }, []);

  // Manually validate a change
  const validate = useCallback(async (change) => {
    return validateChange(change);
  }, [validateChange]);

  // Validate batch of changes
  const validateBatch = useCallback(async (changeBatch) => {
    if (!validatorRef.current) {
      throw new Error('Validator not initialized');
    }

    try {
      const results = await Promise.all(
        changeBatch.map(change => validateChange(change))
      );

      return results;
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [validateChange]);

  // Clear validation results
  const clear = useCallback(() => {
    setViolations([]);
    setValidChanges([]);
    setInvalidChanges([]);
    setStats({
      totalValidated: 0,
      passed: 0,
      failed: 0,
      passRate: 0
    });
  }, []);

  // Get violations by severity
  const getViolationsBySeverity = useCallback((severity) => {
    return violations.filter(v => v.severity === severity);
  }, [violations]);

  // Get violations by focus node
  const getViolationsByFocusNode = useCallback((focusNode) => {
    return violations.filter(v => v.focusNode === focusNode);
  }, [violations]);

  // Get violations by shape
  const getViolationsByShape = useCallback((shape) => {
    return violations.filter(v => v.shape === shape);
  }, [violations]);

  return {
    violations,
    validChanges,
    invalidChanges,
    stats,
    validate,
    validateBatch,
    clear,
    loading,
    error,
    isRunning,
    start,
    stop,
    getViolationsBySeverity,
    getViolationsByFocusNode,
    getViolationsByShape
  };
}
