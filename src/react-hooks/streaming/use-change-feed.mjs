/**
 * @file use-change-feed.mjs
 * @description React hook for real-time change stream with filtering
 * @since 3.2.0
 */

import { useState, useCallback, useEffect, useRef } from 'react';
import { z } from 'zod';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Zod schema for change feed configuration validation
 * Prevents XSS/injection by validating all inputs
 */
const ChangeFeedConfigSchema = z.object({
  /** Filter function must be a function type */
  filter: z.function().optional(),
  /** Operations must be valid operation types */
  operations: z.array(
    z.enum(['insert', 'delete', 'update'])
  ).optional().default(['insert', 'delete']),
  /** Include transaction metadata */
  includeMetadata: z.boolean().optional().default(false),
  /** Batch size must be positive integer, max 10000 */
  batchSize: z.number().int().positive().max(10000).optional().default(10),
  /** Batch interval must be positive, max 60000ms */
  batchInterval: z.number().positive().max(60000).optional().default(1000)
}).strict();

/**
 * Hook for consuming real-time change feed from the knowledge graph
 *
 * @since 3.2.0
 * @param {Object} config - Change feed configuration
 * @param {Function} [config.filter] - Filter function for changes
 * @param {string[]} [config.operations] - Operations to track: 'insert', 'delete'
 * @param {boolean} [config.includeMetadata=false] - Include transaction metadata
 * @param {number} [config.batchSize=10] - Batch size for change events
 * @param {number} [config.batchInterval=1000] - Batch interval (ms)
 * @returns {Object} Change feed state and operations
 * @throws {Error} When change feed not initialized and start/stop is called
 * @throws {Error} When replay is called with invalid timestamp range
 * @performance Batching reduces render cycles; use larger batchSize for high-throughput scenarios.
 *   Memory grows with changes array size - call clear() periodically for long-running feeds.
 *
 * @example
 * // Basic usage with filtering
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
 *
 * @example
 * // Real-time price monitoring
 * const { changes, start, getChangesByPredicate } = useChangeFeed({
 *   operations: ['insert'],
 *   includeMetadata: true
 * });
 * const priceChanges = getChangesByPredicate('schema:price');
 */
export function useChangeFeed(config = {}) {
  // Validate config with Zod schema to prevent XSS/injection
  const validatedConfig = ChangeFeedConfigSchema.parse(config);

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

        // Create change feed with validated config
        const feed = new ChangeFeed({
          engine,
          filter: validatedConfig.filter,
          operations: validatedConfig.operations,
          includeMetadata: validatedConfig.includeMetadata,
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
  }, [engine, validatedConfig.includeMetadata]);

  // Start change feed
  const start = useCallback(async () => {
    if (!feedRef.current) {
      throw new Error('Change feed not initialized');
    }

    try {
      await feedRef.current.start({
        batchSize: validatedConfig.batchSize,
        batchInterval: validatedConfig.batchInterval
      });

      setIsRunning(true);
      return { success: true };
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [validatedConfig.batchSize, validatedConfig.batchInterval]);

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
