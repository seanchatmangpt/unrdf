/**
 * @unrdf/federation
 *
 * Federation - Peer Discovery and Distributed Query Execution
 *
 * @module @unrdf/federation
 */

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

// Export health endpoint
// TODO: health.mjs not yet implemented - coming in next release
// export { createHealthEndpoint } from './federation/health.mjs';

// Export metrics
export {
  getMetrics,
  getMetricsJSON,
  clearMetrics,
  resetMetrics,
  queryCounter,
  queryDuration,
  peerGauge,
  concurrentQueries,
  errorCounter,
  recordQuery,
  recordError,
  updatePeerMetrics,
  trackConcurrentQuery,
} from './federation/metrics.mjs';
