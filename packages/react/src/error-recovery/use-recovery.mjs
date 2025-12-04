/**
 * @file use-recovery.mjs
 * @description React hook for automatic recovery from failures
 */

import { useState, useCallback, useRef } from 'react';

/**
 * Hook for automatic recovery with retry logic
 *
 * @param {Object} config - Recovery configuration
 * @param {number} [config.maxRetries=3] - Maximum retry attempts
 * @param {number} [config.retryDelay=1000] - Delay between retries
 * @returns {Object} Recovery state and operations
 *
 * @example
 * const {
 *   executeWithRecovery,
 *   retryCount,
 *   isRecovering,
 *   lastError
 * } = useRecovery({
 *   maxRetries: 3,
 *   retryDelay: 1000
 * });
 */
export function useRecovery(config = {}) {
  const [retryCount, setRetryCount] = useState(0);
  const [isRecovering, setIsRecovering] = useState(false);
  const [lastError, setLastError] = useState(null);
  const attemptsRef = useRef(0);

  const executeWithRecovery = useCallback(
    async (fn, options = {}) => {
      const maxRetries = options.maxRetries || config.maxRetries || 3;
      const retryDelay = options.retryDelay || config.retryDelay || 1000;

      attemptsRef.current = 0;
      setIsRecovering(false);

      while (attemptsRef.current < maxRetries) {
        try {
          const result = await fn();
          setRetryCount(0);
          setLastError(null);
          return result;
        } catch (err) {
          attemptsRef.current++;
          setRetryCount(attemptsRef.current);
          setLastError(err);

          if (attemptsRef.current >= maxRetries) {
            setIsRecovering(false);
            throw err;
          }

          setIsRecovering(true);
          await new Promise((resolve) => setTimeout(resolve, retryDelay * attemptsRef.current));
        }
      }
    },
    [config]
  );

  const reset = useCallback(() => {
    setRetryCount(0);
    setIsRecovering(false);
    setLastError(null);
    attemptsRef.current = 0;
  }, []);

  return { executeWithRecovery, retryCount, isRecovering, lastError, reset };
}
