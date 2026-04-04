# Reference — @unrdf/federation

Reference material is for looking things up. It is accurate and complete, not tutorial-style.

## Index

| Document                                   | Covers                                                                                                                                                                                                                                                                                                                                                 |
| ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| [federation-api.md](./federation-api.md)   | `createCoordinator`, `createPeerManager`, `executeFederatedQuery`, `executeDistributedQuery`, `aggregateResults`, `routeQuery`, `createAdvancedFederationEngine`, `federatedQuery`, `streamFederatedQuery`, `createFederationCoordinator`, `createDistributedQueryEngine`, `createConsensusManager`, `createDataReplicationManager`, metrics functions |
| [endpoint-config.md](./endpoint-config.md) | Zod schemas: `CoordinatorConfigSchema`, `PeerConfigSchema`, `PeerInfoSchema`, `QueryConfigSchema`, `QueryResultSchema`, `AdvancedFederationConfigSchema`, `QueryExecutionResultSchema`, `FederationConfigSchema`, `ReplicationConfigSchema`, `RaftConfigSchema`                                                                                        |

## Imports

All public exports are available from the package root:

```javascript
import {
  // Primary coordinator API (v5)
  createCoordinator,
  CoordinatorConfigSchema,

  // Peer manager (use via coordinator or standalone)
  createPeerManager,
  PeerConfigSchema,
  PeerInfoSchema,

  // Low-level distributed query functions
  executeFederatedQuery,
  executeDistributedQuery,
  aggregateResults,
  routeQuery,
  QueryConfigSchema,
  QueryResultSchema,

  // Advanced Comunica-based federation
  createAdvancedFederationEngine,
  federatedQuery,
  streamFederatedQuery,
  AdvancedFederationConfigSchema,
  QueryExecutionResultSchema,

  // V6 store-level coordinator
  createFederationCoordinator,
  FederationCoordinator,
  StoreHealth,

  // V6 distributed query engine
  createDistributedQueryEngine,
  DistributedQueryEngine,
  ExecutionStrategy,
  PlanNodeType,

  // V6 RAFT consensus manager
  createConsensusManager,
  ConsensusManager,
  NodeState,

  // V6 data replication manager
  createDataReplicationManager,
  DataReplicationManager,
  ReplicationTopology,
  ConflictResolution,
  ReplicationMode,

  // OTEL metrics utilities
  recordQuery,
  recordError,
  updatePeerMetrics,
  trackConcurrentQuery,
  getMetricsState,
  resetMetrics,
} from '@unrdf/federation';
```

## Package entry points

| Entry point                         | File                                 | Exports                                           |
| ----------------------------------- | ------------------------------------ | ------------------------------------------------- |
| `@unrdf/federation`                 | `src/index.mjs`                      | All public exports listed above                   |
| `@unrdf/federation/coordinator`     | `src/coordinator.mjs`                | Deprecated alias; use `@unrdf/federation` instead |
| `@unrdf/federation/advanced-sparql` | `src/advanced-sparql-federation.mjs` | Advanced Comunica engine only                     |
