/**
 * @unrdf/federation v6.0.0
 *
 * Federation - Distributed RDF Query with RAFT Consensus and Multi-Master Replication
 *
 * @module @unrdf/federation
 */

// =============================================================================
// Core Federation API (v5 + v6)
// =============================================================================

// Export coordinator factory (primary API)
export { createCoordinator } from './federation/coordinator.mjs';

// Export peer manager
export { createPeerManager, PeerConfigSchema, PeerInfoSchema } from './federation/peer-manager.mjs';

// Export distributed query functions
export {
  executeFederatedQuery,
  executeDistributedQuery,
  aggregateResults,
  routeQuery,
  QueryConfigSchema,
  QueryResultSchema,
} from './federation/distributed-query.mjs';

// Export coordinator types
export { CoordinatorConfigSchema } from './federation/coordinator.mjs';

// Export metrics
export {
  recordQuery,
  recordError,
  updatePeerMetrics,
  trackConcurrentQuery,
  getMetricsState,
  resetMetrics,
} from './federation/metrics.mjs';

// =============================================================================
// V6 Advanced Features
// =============================================================================

// Export federation coordinator with store management
export {
  createFederationCoordinator,
  FederationCoordinator,
  StoreHealth,
} from './federation/federation-coordinator.mjs';

// Export distributed query engine with optimization
export {
  createDistributedQueryEngine,
  DistributedQueryEngine,
  ExecutionStrategy,
  PlanNodeType,
} from './federation/distributed-query-engine.mjs';

// Export RAFT consensus manager
export {
  createConsensusManager,
  ConsensusManager,
  NodeState,
} from './federation/consensus-manager.mjs';

// Export data replication manager
export {
  createDataReplicationManager,
  DataReplicationManager,
  ReplicationTopology,
  ConflictResolution,
  ReplicationMode,
} from './federation/data-replication.mjs';

// Export advanced SPARQL federation with Comunica
export {
  createAdvancedFederationEngine,
  federatedQuery,
  streamFederatedQuery,
  AdvancedFederationConfigSchema,
  QueryExecutionResultSchema,
} from './advanced-sparql-federation.mjs';
