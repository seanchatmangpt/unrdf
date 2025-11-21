/**
 * @file use-federated-system.mjs
 * @description React hook for managing federated RDF systems across multiple stores
 * @since 3.2.0
 */

import { useState, useCallback, useEffect, useRef } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for managing a federated RDF system with distributed stores
 *
 * @since 3.2.0
 * @param {Object} config - Federation configuration
 * @param {string[]} config.stores - Array of store identifiers or URLs
 * @param {string} [config.consensusProtocol='raft'] - Consensus protocol (raft, gossip, byzantine)
 * @param {number} [config.replicationFactor=3] - Number of replicas
 * @param {Object} [config.syncStrategy] - Synchronization strategy
 * @returns {Object} Federation state and operations
 * @throws {Error} When federation system not initialized
 * @throws {Error} When store registration fails (invalid endpoint, network error)
 * @throws {Error} When distributed query times out
 * @throws {Error} When replication fails to reach quorum
 * @performance Network latency dominates - use 'fastest' storeSelection for latency-sensitive queries.
 *   Consensus protocol affects write latency: raft < gossip < byzantine.
 *   Health checks run periodically - adjust interval for monitoring overhead.
 *
 * @example
 * // Basic federation setup
 * const {
 *   system,
 *   registerStore,
 *   query,
 *   replicate,
 *   health,
 *   loading
 * } = useFederatedSystem({
 *   stores: ['store1', 'store2', 'store3'],
 *   consensusProtocol: 'raft',
 *   replicationFactor: 2
 * });
 *
 * @example
 * // Distributed query with strategy
 * const result = await query('SELECT * WHERE { ?s ?p ?o }', {
 *   storeSelection: 'quorum',
 *   aggregation: 'union',
 *   timeout: 5000
 * });
 */
export function useFederatedSystem(config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [system, setSystem] = useState(null);
  const [stores, setStores] = useState([]);
  const [health, setHealth] = useState({
    status: 'initializing',
    stores: {},
    consensus: null,
    replication: null
  });
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const systemRef = useRef(null);

  // Initialize federated system
  useEffect(() => {
    let mounted = true;

    async function initializeFederation() {
      try {
        setLoading(true);

        // Import federation module
        const { createFederatedSystem } = await import(
          '../../knowledge-engine/federation/federation-coordinator.mjs'
        );

        // Create federated system
        const federatedSystem = await createFederatedSystem({
          ...config,
          engine
        });

        if (!mounted) return;

        systemRef.current = federatedSystem;
        setSystem(federatedSystem);

        // Register initial stores
        if (config.stores) {
          for (const storeId of config.stores) {
            await federatedSystem.registerStore({
              id: storeId,
              endpoint: config.endpoints?.[storeId],
              metadata: config.metadata?.[storeId]
            });
          }
        }

        // Get initial health
        const healthData = await federatedSystem.getHealth();
        setHealth(healthData);
        setLoading(false);
      } catch (err) {
        if (!mounted) return;
        setError(err);
        setLoading(false);
      }
    }

    initializeFederation();

    return () => {
      mounted = false;
      if (systemRef.current) {
        systemRef.current.shutdown?.();
      }
    };
  }, [engine, JSON.stringify(config)]);

  // Register new store
  const registerStore = useCallback(async (storeMetadata) => {
    if (!system) {
      throw new Error('Federation system not initialized');
    }

    try {
      await system.registerStore(storeMetadata);

      // Update stores list
      const updatedStores = await system.listStores();
      setStores(updatedStores);

      // Update health
      const healthData = await system.getHealth();
      setHealth(healthData);

      return { success: true, storeId: storeMetadata.id };
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [system]);

  // Unregister store
  const unregisterStore = useCallback(async (storeId) => {
    if (!system) {
      throw new Error('Federation system not initialized');
    }

    try {
      await system.unregisterStore(storeId);

      // Update stores list
      const updatedStores = await system.listStores();
      setStores(updatedStores);

      // Update health
      const healthData = await system.getHealth();
      setHealth(healthData);

      return { success: true };
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [system]);

  // Execute distributed SPARQL query
  const query = useCallback(async (sparql, options = {}) => {
    if (!system) {
      throw new Error('Federation system not initialized');
    }

    try {
      const result = await system.query(sparql, {
        timeout: options.timeout || 30000,
        storeSelection: options.storeSelection, // 'all', 'quorum', 'fastest'
        aggregation: options.aggregation || 'union', // 'union', 'intersection'
        ...options
      });

      return result;
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [system]);

  // Replicate data across stores
  const replicate = useCallback(async (change, options = {}) => {
    if (!system) {
      throw new Error('Federation system not initialized');
    }

    try {
      await system.replicate(change, {
        strategy: options.strategy || 'eventual', // 'immediate', 'eventual'
        stores: options.stores, // Specific stores to replicate to
        timeout: options.timeout || 10000
      });

      return { success: true };
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [system]);

  // Get federation statistics
  const getStats = useCallback(async () => {
    if (!system) {
      throw new Error('Federation system not initialized');
    }

    try {
      const stats = await system.getStats();
      return stats;
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [system]);

  // Refresh health status
  const refreshHealth = useCallback(async () => {
    if (!system) return;

    try {
      const healthData = await system.getHealth();
      setHealth(healthData);
      return healthData;
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [system]);

  return {
    system,
    stores,
    health,
    loading,
    error,
    registerStore,
    unregisterStore,
    query,
    replicate,
    getStats,
    refreshHealth
  };
}
