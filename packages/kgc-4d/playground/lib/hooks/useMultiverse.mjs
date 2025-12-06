'use client';

/**
 * useMultiverse - Hook for managing forked realities
 *
 * Provides fork creation, delta application, and merge functionality.
 *
 * @example
 * const { forks, createFork, applyDelta, merge, loading } = useMultiverse();
 * await createFork('my-fork', BigInt('1704657600000000000'));
 * await applyDelta('my-fork', { type: 'UPDATE', ... });
 * await merge('my-fork', 'auto');
 */

import { useState, useCallback } from 'react';

export function useMultiverse() {
  const [forks, setForks] = useState([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  /**
   * Create a new forked reality
   */
  const createFork = useCallback(async (forkId, fromTime) => {
    setLoading(true);
    setError(null);

    try {
      const fromTimeStr = typeof fromTime === 'bigint' ? fromTime.toString() : fromTime;

      const response = await fetch('/api/multiverse/fork', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ forkId, fromTime: fromTimeStr }),
      });

      if (!response.ok) {
        const errorData = await response.json();
        throw new Error(errorData.error || 'Fork creation failed');
      }

      const result = await response.json();

      // Add to forks list
      setForks((prev) => [...prev, result]);

      return result;
    } catch (err) {
      setError(err.message);
      console.error('[useMultiverse] createFork error:', err);
      return null;
    } finally {
      setLoading(false);
    }
  }, []);

  /**
   * Apply a delta to a forked Universe
   */
  const applyDelta = useCallback(async (forkId, delta) => {
    setLoading(true);
    setError(null);

    try {
      // In Phase 2, we simulate delta application
      // In production, would POST to /api/multiverse/delta
      console.log('[useMultiverse] Apply delta to fork:', forkId, delta);

      // Update fork event count locally
      setForks((prev) =>
        prev.map((f) =>
          f.forkId === forkId
            ? { ...f, eventCount: (f.eventCount || 0) + 1 }
            : f
        )
      );

      return { status: 'ACK', forkId };
    } catch (err) {
      setError(err.message);
      console.error('[useMultiverse] applyDelta error:', err);
      return { status: 'REJECT', reason: err.message };
    } finally {
      setLoading(false);
    }
  }, []);

  /**
   * Merge a fork back into main Universe
   */
  const merge = useCallback(async (forkId, strategy = 'auto') => {
    setLoading(true);
    setError(null);

    try {
      const response = await fetch('/api/multiverse/merge', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ forkId, strategy }),
      });

      const result = await response.json();

      if (result.status === 'conflict') {
        setError(`Merge conflict: ${result.message}`);
        return result;
      }

      if (result.status === 'success') {
        // Remove from forks list
        setForks((prev) => prev.filter((f) => f.forkId !== forkId));
      }

      return result;
    } catch (err) {
      setError(err.message);
      console.error('[useMultiverse] merge error:', err);
      return { status: 'error', message: err.message };
    } finally {
      setLoading(false);
    }
  }, []);

  /**
   * Discard a fork without merging
   */
  const discardFork = useCallback((forkId) => {
    setForks((prev) => prev.filter((f) => f.forkId !== forkId));
  }, []);

  /**
   * Reset state
   */
  const reset = useCallback(() => {
    setForks([]);
    setError(null);
  }, []);

  return {
    forks,
    loading,
    error,
    createFork,
    applyDelta,
    merge,
    discardFork,
    reset,
  };
}
