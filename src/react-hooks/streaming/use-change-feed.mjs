/**
 * @file use-change-feed.mjs
 * @description React hook for real-time change stream with filtering
 */

import { useState, useCallback, useEffect, useRef } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for consuming real-time change feed from the knowledge graph
 *
 * @param {Object} config - Change feed configuration
 * @param {Function} [config.filter] - Filter function for changes
 * @param {string[]} [config.operations] - Operations to track: 'insert', 'delete'
 * @param {boolean} [config.includeMetadata=false] - Include transaction metadata
 * @param {number} [config.batchSize=10] - Batch size for change events
 * @param {number} [config.batchInterval=1000] - Batch interval (ms)
 * @returns {Object} Change feed state and operations
 *
 * @example
 * const {
 *   changes,
 *   start,
 *   stop,
 *   clear,
 *   isRunning
 * } = useChangeFeed({
 *   filter: (change) => {
 *     return change.operation === 'insert' &&
 *            change.quads.some(q => q.predicate.value.includes('price'));
 *   },
 *   operations: ['insert', 'delete'],
 *   batchSize: 10,
 *   batchInterval: 1000
 * });
 */
export function useChangeFeed(config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [changes, setChanges] = useState([]);
  const [isRunning, setIsRunning] = useState(false);
  const [stats, setStats] = useState({
    totalChanges: 0,
    inserts: 0,
    deletes: 0,
    filtered: 0
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const feedRef = useRef(null);
  const changesRef = useRef([]);
  const batchRef = useRef([]);
  const batchTimerRef = useRef(null);

  // Initialize change feed
  useEffect(() => {
    if (!engine) return;

    let mounted = true;

    async function initializeFeed() {
      try {
        setLoading(true);

        // Import change feed module
        const { ChangeFeed } = await import(
          '../../knowledge-engine/streaming/change-feed.mjs'
        );

        // Create change feed
        const feed = new ChangeFeed({
          engine,
          filter: config.filter,
          operations: config.operations || ['insert', 'delete'],
          includeMetadata: config.includeMetadata || false,
          onBatchReady: (batch) => {
            if (!mounted) return;

            // Add to changes
            changesRef.current = [...changesRef.current, ...batch];
            setChanges([...changesRef.current]);

            // Update stats
            setStats(prev => ({
              totalChanges: prev.totalChanges + batch.length,
              inserts: prev.inserts + batch.filter(c => c.operation === 'insert').length,
              deletes: prev.deletes + batch.filter(c => c.operation === 'delete').length,
              filtered: prev.filtered
            }));
          }
        });

        if (!mounted) return;

        feedRef.current = feed;
        setLoading(false);
      } catch (err) {
        if (!mounted) return;
        setError(err);
        setLoading(false);
      }
    }

    initializeFeed();

    return () => {
      mounted = false;
      if (feedRef.current) {
        feedRef.current.stop?.();
      }
      if (batchTimerRef.current) {
        clearInterval(batchTimerRef.current);
      }
    };
  }, [engine, config.includeMetadata]);

  // Start change feed
  const start = useCallback(async () => {
    if (!feedRef.current) {
      throw new Error('Change feed not initialized');
    }

    try {
      await feedRef.current.start({
        batchSize: config.batchSize || 10,
        batchInterval: config.batchInterval || 1000
      });

      setIsRunning(true);
      return { success: true };
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [config.batchSize, config.batchInterval]);

  // Stop change feed
  const stop = useCallback(async () => {
    if (!feedRef.current) {
      throw new Error('Change feed not initialized');
    }

    try {
      await feedRef.current.stop();
      setIsRunning(false);
      return { success: true };
    } catch (err) {
      setError(err);
      throw err;
    }
  }, []);

  // Clear changes history
  const clear = useCallback(() => {
    changesRef.current = [];
    setChanges([]);
    setStats({
      totalChanges: 0,
      inserts: 0,
      deletes: 0,
      filtered: 0
    });
  }, []);

  // Get changes by operation
  const getChangesByOperation = useCallback((operation) => {
    return changes.filter(c => c.operation === operation);
  }, [changes]);

  // Get changes by predicate
  const getChangesByPredicate = useCallback((predicate) => {
    return changes.filter(c =>
      c.quads.some(q => q.predicate.value === predicate)
    );
  }, [changes]);

  // Get changes by subject
  const getChangesBySubject = useCallback((subject) => {
    return changes.filter(c =>
      c.quads.some(q => q.subject.value === subject)
    );
  }, [changes]);

  // Get recent changes
  const getRecentChanges = useCallback((count = 10) => {
    return changes.slice(-count);
  }, [changes]);

  // Replay changes
  const replay = useCallback(async (fromTimestamp, toTimestamp) => {
    if (!feedRef.current) {
      throw new Error('Change feed not initialized');
    }

    try {
      const historicalChanges = await feedRef.current.replay({
        from: fromTimestamp,
        to: toTimestamp
      });

      return historicalChanges;
    } catch (err) {
      setError(err);
      throw err;
    }
  }, []);

  return {
    changes,
    start,
    stop,
    clear,
    isRunning,
    stats,
    loading,
    error,
    getChangesByOperation,
    getChangesByPredicate,
    getChangesBySubject,
    getRecentChanges,
    replay
  };
}
