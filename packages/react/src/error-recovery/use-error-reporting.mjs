/**
 * @file use-error-reporting.mjs
 * @description React hook for error reporting and analytics
 */
/* global navigator */

import { useState, useCallback, useEffect } from 'react';

/**
 * Hook for tracking and reporting errors
 *
 * @param {Object} config - Error reporting configuration
 * @param {Function} [config.onReport] - Report callback
 * @returns {Object} Error reporting state and operations
 *
 * @example
 * const {
 *   reportError,
 *   errors,
 *   errorRate,
 *   clearErrors
 * } = useErrorReporting({
 *   onReport: (error) => {
 *     console.log('Reporting error:', error);
 *   }
 * });
 */
export function useErrorReporting(config = {}) {
  const [errors, setErrors] = useState([]);
  const [errorRate, setErrorRate] = useState(0);

  const reportError = useCallback(
    (error, context = {}) => {
      const errorReport = {
        message: error.message,
        stack: error.stack,
        context,
        timestamp: new Date().toISOString(),
        userAgent: navigator.userAgent,
      };

      setErrors((prev) => [...prev, errorReport]);
      config.onReport?.(errorReport);

      return errorReport;
    },
    [config]
  );

  const clearErrors = useCallback(() => {
    setErrors([]);
    setErrorRate(0);
  }, []);

  useEffect(() => {
    if (errors.length === 0) return;

    const now = Date.now();
    const lastHour = errors.filter((e) => now - new Date(e.timestamp).getTime() < 3600000);

    setErrorRate(lastHour.length);
  }, [errors]);

  return { reportError, errors, errorRate, clearErrors };
}
