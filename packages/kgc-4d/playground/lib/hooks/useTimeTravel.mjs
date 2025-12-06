'use client';

/**
 * useTimeTravel - Hook for 4D time-travel reconstruction
 *
 * Fetches reconstructed state from /api/time-travel at a specific nanosecond timestamp
 *
 * @example
 * const { reconstructed, loading, error, travelTo } = useTimeTravel();
 * await travelTo(BigInt('1704657600000000000')); // Travel to specific t_ns
 */

import { useState, useCallback } from 'react';

/**
 * Hook for 4D time-travel reconstruction
 * @returns {Object} Time travel utilities
 */
export function useTimeTravel() {
  const [reconstructed, setReconstructed] = useState(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const travelTo = useCallback(async (targetTime) => {
    if (!targetTime) {
      setError('Target time is required');
      return;
    }

    setLoading(true);
    setError(null);

    try {
      // Convert BigInt to string for URL parameter
      const targetTimeStr = typeof targetTime === 'bigint'
        ? targetTime.toString()
        : targetTime;

      const response = await fetch(`/api/time-travel?t_ns=${targetTimeStr}`);

      if (!response.ok) {
        const errorData = await response.json();
        throw new Error(errorData.error || 'Time-travel reconstruction failed');
      }

      const data = await response.json();

      setReconstructed({
        shard: data.shard,
        t_ns: data.t_ns,
        timestamp: data.timestamp,
        quad_count: data.shard?.quad_count || 0,
      });

      return data;
    } catch (err) {
      setError(err.message);
      console.error('[useTimeTravel] Error:', err);
      return null;
    } finally {
      setLoading(false);
    }
  }, []);

  const reset = useCallback(() => {
    setReconstructed(null);
    setError(null);
  }, []);

  return {
    reconstructed,
    loading,
    error,
    travelTo,
    reset,
  };
}
