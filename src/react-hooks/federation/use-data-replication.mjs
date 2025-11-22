/**
 * @file use-data-replication.mjs
 * @description React hook for managing data replication and conflict resolution
 * @since 3.2.0
 */

import { useState, useCallback, useEffect, useRef } from 'react';
import { useFederatedSystem } from './use-federated-system.mjs';

/**
 * Hook for managing distributed data replication with conflict resolution
 *
 * @since 3.2.0
 * @param {Object} config - Replication configuration
 * @param {string} [config.strategy='eventual'] - Replication strategy: 'eventual', 'immediate'
 * @param {number} [config.replicationFactor=2] - Number of replicas
 * @param {Function} [config.conflictResolver] - Custom conflict resolution function
 * @param {boolean} [config.autoSync=true] - Enable automatic synchronization
 * @returns {Object} Replication state and operations
 * @throws {Error} When federation system not initialized
 * @throws {Error} When replication fails to reach replicationFactor nodes
 * @throws {Error} When conflict resolution fails or resolver throws
 * @throws {Error} When resolveConflict called with unknown conflictId
 * @performance autoSync creates background sync interval - increases network traffic.
 *   'immediate' strategy blocks until replication complete. Conflict detection adds overhead.
 *
 * @example
 * // Eventual consistency with auto-sync
 * const {
 *   replicate,
 *   conflicts,
 *   resolveConflict,
 *   syncStatus,
 *   loading
 * } = useDataReplication({
 *   strategy: 'eventual',
 *   replicationFactor: 3,
 *   conflictResolver: (versions) => versions[0] // Last-write-wins
 * });
 *
 * @example
 * // Replicate with version vector
 * await replicate({
 *   operation: 'insert',
 *   quads: [{ subject, predicate, object }],
 *   vector: [1, 2, 3]
 * });
 */
export function useDataReplication(config = {}) {
  const { replicate: federatedReplicate, system } = useFederatedSystem(config.federation || {});
  const [conflicts, setConflicts] = useState([]);
  const [syncStatus, setSyncStatus] = useState({
    inProgress: false,
    lastSync: null,
    pendingChanges: 0,
  });
  const [replicationStats, setReplicationStats] = useState({
    totalReplications: 0,
    successCount: 0,
    failureCount: 0,
    conflictCount: 0,
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const syncQueueRef = useRef([]);
  const syncIntervalRef = useRef(null);

  // Auto-sync setup
  useEffect(() => {
    if (!config.autoSync || !system) return;

    const syncInterval = config.syncInterval || 5000;

    syncIntervalRef.current = setInterval(async () => {
      if (syncQueueRef.current.length > 0) {
        await processSyncQueue();
      }
    }, syncInterval);

    return () => {
      if (syncIntervalRef.current) {
        clearInterval(syncIntervalRef.current);
      }
    };
  }, [system, config.autoSync, config.syncInterval]);

  // Process sync queue
  const processSyncQueue = useCallback(async () => {
    if (syncQueueRef.current.length === 0 || syncStatus.inProgress) {
      return;
    }

    setSyncStatus(prev => ({ ...prev, inProgress: true }));

    const batch = syncQueueRef.current.splice(0, 10); // Process 10 at a time

    for (const change of batch) {
      try {
        await federatedReplicate(change, {
          strategy: config.strategy || 'eventual',
          replicationFactor: config.replicationFactor || 2,
        });

        setReplicationStats(prev => ({
          ...prev,
          successCount: prev.successCount + 1,
        }));
      } catch (err) {
        setReplicationStats(prev => ({
          ...prev,
          failureCount: prev.failureCount + 1,
        }));

        // Check for conflicts
        if (err.code === 'REPLICATION_CONFLICT') {
          setConflicts(prev => [
            ...prev,
            {
              change,
              conflict: err.details,
              timestamp: new Date().toISOString(),
            },
          ]);

          setReplicationStats(prev => ({
            ...prev,
            conflictCount: prev.conflictCount + 1,
          }));
        }
      }
    }

    setSyncStatus(prev => ({
      ...prev,
      inProgress: false,
      lastSync: new Date().toISOString(),
      pendingChanges: syncQueueRef.current.length,
    }));
  }, [federatedReplicate, config, syncStatus.inProgress]);

  // Replicate a change
  const replicate = useCallback(
    async (change, options = {}) => {
      if (!system) {
        throw new Error('Federation system not initialized');
      }

      try {
        setLoading(true);

        // Add to queue if auto-sync enabled
        if (config.autoSync) {
          syncQueueRef.current.push(change);
          setSyncStatus(prev => ({
            ...prev,
            pendingChanges: syncQueueRef.current.length,
          }));

          setReplicationStats(prev => ({
            ...prev,
            totalReplications: prev.totalReplications + 1,
          }));

          setLoading(false);
          return { queued: true };
        }

        // Immediate replication
        await federatedReplicate(change, {
          strategy: options.strategy || config.strategy || 'eventual',
          stores: options.stores,
          timeout: options.timeout || 10000,
        });

        setReplicationStats(prev => ({
          ...prev,
          totalReplications: prev.totalReplications + 1,
          successCount: prev.successCount + 1,
        }));

        setLoading(false);
        return { success: true };
      } catch (err) {
        setError(err);
        setLoading(false);

        // Handle conflicts
        if (err.code === 'REPLICATION_CONFLICT') {
          const conflict = {
            change,
            conflict: err.details,
            timestamp: new Date().toISOString(),
          };

          setConflicts(prev => [...prev, conflict]);
          setReplicationStats(prev => ({
            ...prev,
            conflictCount: prev.conflictCount + 1,
          }));

          // Auto-resolve if resolver provided
          if (config.conflictResolver) {
            await resolveConflict(conflict.conflict.id, config.conflictResolver);
          }
        }

        throw err;
      }
    },
    [system, federatedReplicate, config]
  );

  // Resolve conflict
  const resolveConflict = useCallback(
    async (conflictId, resolver) => {
      const conflict = conflicts.find(c => c.conflict.id === conflictId);
      if (!conflict) {
        throw new Error(`Conflict ${conflictId} not found`);
      }

      try {
        const resolution =
          typeof resolver === 'function' ? await resolver(conflict.conflict.versions) : resolver;

        // Apply resolution
        await federatedReplicate({
          operation: 'resolve-conflict',
          conflictId,
          resolution,
          vector: conflict.conflict.mergedVector,
        });

        // Remove from conflicts
        setConflicts(prev => prev.filter(c => c.conflict.id !== conflictId));

        return { success: true, resolution };
      } catch (err) {
        setError(err);
        throw err;
      }
    },
    [conflicts, federatedReplicate]
  );

  // Get replication status
  const getStatus = useCallback(() => {
    return {
      ...syncStatus,
      ...replicationStats,
      conflictsCount: conflicts.length,
      queueSize: syncQueueRef.current.length,
    };
  }, [syncStatus, replicationStats, conflicts]);

  return {
    replicate,
    conflicts,
    resolveConflict,
    syncStatus,
    replicationStats,
    loading,
    error,
    getStatus,
  };
}
