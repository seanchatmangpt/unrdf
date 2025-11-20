/**
 * @fileoverview Distributed Federation Protocol for UNRDF 2028
 * @module federation
 *
 * @description
 * Comprehensive distributed federation system for RDF stores with:
 * - RAFT consensus for coordination
 * - Federation orchestration and store management
 * - Distributed SPARQL query execution
 * - Data replication with eventual consistency
 * - Conflict resolution
 * - Network partition handling
 * - OpenTelemetry observability
 *
 * @example
 * import { createFederatedSystem } from './federation/index.mjs';
 *
 * const federation = await createFederatedSystem({
 *   federationId: 'my-federation',
 *   enableConsensus: true,
 *   replicationTopology: 'full-mesh'
 * });
 *
 * // Register stores
 * await federation.registerStore({
 *   storeId: 'store-1',
 *   endpoint: 'http://store1:3000',
 *   weight: 1.0
 * });
 *
 * // Query across federation
 * const results = await federation.query('SELECT * WHERE { ?s ?p ?o } LIMIT 10');
 */

export {
  ConsensusManager,
  createConsensusManager,
  NodeState
} from './consensus-manager.mjs';

export {
  FederationCoordinator,
  createFederationCoordinator,
  StoreHealth
} from './federation-coordinator.mjs';

export {
  DistributedQueryEngine,
  createDistributedQueryEngine,
  ExecutionStrategy,
  PlanNodeType
} from './distributed-query-engine.mjs';

export {
  DataReplicationManager,
  createDataReplicationManager,
  ReplicationTopology,
  ConflictResolution,
  ReplicationMode
} from './data-replication.mjs';

import { createFederationCoordinator } from './federation-coordinator.mjs';
import { createDistributedQueryEngine } from './distributed-query-engine.mjs';
import { createDataReplicationManager } from './data-replication.mjs';

/**
 * Create a complete federated RDF system
 *
 * @param {Object} config - Federation configuration
 * @param {string} [config.federationId] - Unique federation ID
 * @param {boolean} [config.enableConsensus=true] - Enable RAFT consensus
 * @param {string} [config.replicationTopology='full-mesh'] - Replication topology
 * @param {string} [config.conflictResolution='last-write-wins'] - Conflict resolution strategy
 * @param {string} [config.loadBalancingStrategy='weighted'] - Load balancing strategy
 * @returns {Promise<Object>} Federated system instance
 *
 * @example
 * const federation = await createFederatedSystem({
 *   federationId: 'prod-federation',
 *   enableConsensus: true,
 *   replicationTopology: 'full-mesh',
 *   conflictResolution: 'last-write-wins'
 * });
 *
 * await federation.registerStore({
 *   storeId: 'store-1',
 *   endpoint: 'http://store1:3000'
 * });
 *
 * const results = await federation.query('SELECT * WHERE { ?s ?p ?o }');
 */
export async function createFederatedSystem(config = {}) {
  // Create coordinator
  const coordinator = createFederationCoordinator({
    federationId: config.federationId,
    enableConsensus: config.enableConsensus ?? true,
    loadBalancingStrategy: config.loadBalancingStrategy ?? 'weighted'
  });

  await coordinator.initialize();

  // Create query engine
  const queryEngine = createDistributedQueryEngine(coordinator, {
    executionStrategy: config.executionStrategy ?? 'adaptive',
    enablePushdown: config.enablePushdown ?? true,
    enableJoinOptimization: config.enableJoinOptimization ?? true
  });

  // Create replication manager
  const replication = createDataReplicationManager(coordinator, {
    topology: config.replicationTopology ?? 'full-mesh',
    conflictResolution: config.conflictResolution ?? 'last-write-wins',
    mode: config.replicationMode ?? 'bidirectional'
  });

  await replication.initialize();

  // Return unified interface
  return {
    coordinator,
    queryEngine,
    replication,

    /**
     * Register a store in the federation
     * @param {Object} storeMetadata - Store metadata
     * @returns {Promise<void>}
     */
    async registerStore(storeMetadata) {
      return coordinator.registerStore(storeMetadata);
    },

    /**
     * Deregister a store from the federation
     * @param {string} storeId - Store ID
     * @returns {Promise<void>}
     */
    async deregisterStore(storeId) {
      return coordinator.deregisterStore(storeId);
    },

    /**
     * Execute a SPARQL query across the federation
     * @param {string} sparql - SPARQL query
     * @param {Object} options - Query options
     * @returns {Promise<Array>} Query results
     */
    async query(sparql, options = {}) {
      return queryEngine.execute(sparql, options);
    },

    /**
     * Replicate data across the federation
     * @param {Object} change - Change operation
     * @returns {Promise<void>}
     */
    async replicate(change) {
      return replication.replicate(change);
    },

    /**
     * Get federation statistics
     * @returns {Object} Statistics
     */
    getStats() {
      return {
        coordinator: coordinator.getStats(),
        queryEngine: queryEngine.getStats(),
        replication: replication.getStats()
      };
    },

    /**
     * Shutdown the federation
     * @returns {Promise<void>}
     */
    async shutdown() {
      await replication.shutdown();
      await coordinator.shutdown();
    }
  };
}
