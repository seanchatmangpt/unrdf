/**
 * @file use-error-boundary.mjs
 * @description React hook for error boundary functionality
 */

import { useState, useCallback, useEffect } from 'react';

/**
 * Hook for implementing error boundary functionality
 *
 * @param {Object} config - Error boundary configuration
 * @param {Function} [config.onError] - Error callback
 * @param {Function} [config.fallback] - Fallback render
 * @returns {Object} Error boundary state and operations
 *
 * @example
 * const {
 *   hasError,
 *   error,
 *   resetError,
 *   captureError
 * } = useErrorBoundary({
 *   onError: (error, errorInfo) => {
 *     console.error('Caught error:', error);
 *   }
 * });
 */
export function useErrorBoundary(config = {}) {
  const [hasError, setHasError] = useState(false);
  const [error, setError] = useState(null);
  const [errorInfo, setErrorInfo] = useState(null);

  const captureError = useCallback(
    (err, info) => {
      setHasError(true);
      setError(err);
      setErrorInfo(info);
      config.onError?.(err, info);
    },
    [config]
  );

  const resetError = useCallback(() => {
    setHasError(false);
    setError(null);
    setErrorInfo(null);
  }, []);

  useEffect(() => {
    const handleError = event => {
      captureError(event.error, { componentStack: event.filename });
    };

    window.addEventListener('error', handleError);
    return () => window.removeEventListener('error', handleError);
  }, [captureError]);

  return { hasError, error, errorInfo, resetError, captureError };
}
