/**
 * KGC 4D Engine - Public API
 * Nanosecond-precision event logging with Git-backed snapshots
 */

// Core 4D Engine
export { KGCStore } from './store.mjs';
export { GitBackbone } from './git.mjs';
export { freezeUniverse, reconstructState, verifyReceipt } from './freeze.mjs';
export { now, toISO, fromISO, addNanoseconds, duration, VectorClock, hasClockJumpDetected, resetClockJumpDetection } from './time.mjs';
export { GRAPHS, EVENT_TYPES, PREDICATES } from './constants.mjs';

// Reusable Patterns for Client/Server Applications
export { HookRegistry } from './core/patterns/hook-registry.mjs';
export {
  createDeltaSyncReducer,
  DeltaSyncState,
  DeltaSyncActions,
} from './core/patterns/delta-sync-reducer.mjs';
export { SSEClient } from './core/patterns/sse-client.mjs';

// HDIT - Hyperdimensional Information Theory
// Practical coordinate-based event similarity and visualization
export {
  // Coordinate generation
  coordsForEvent,
  batchCoordsForEvents,
  createUniverseContext,
  // Distance and similarity
  cosineSimilarity,
  cosineDistance,
  euclideanDistance,
  euclideanDistanceSquared,
  manhattanDistance,
  findKNearest,
  findWithinThreshold,
  pairwiseDistances,
  calculateCentroid,
  normalize,
  dotProduct,
  // Guards and validation
  guardDimension,
  guardMemory,
  guardLatency,
  guardEntityCount,
  guardCoordinates,
  guardAll,
  suggestDimension,
  guardedOperation,
  // Visualization and projection
  projectPCA,
  projectRandom,
  normalizeProjection,
  createVisualizationData,
  clusterProjection,
  // Constants
  D_BROWSER,
  D_LIGHT,
  D_MEDIUM,
  D_NODE_MAX,
  D_HEAVY,
  D_DEFAULT,
  N_BROWSER_MAX,
  LATENCY_BUDGET_MS,
  OPS_BUDGET,
  ONTOLOGY_AXES,
  ONTOLOGY_DIM,
  EVENT_TYPE_ENCODING,
  calculateMemoryFootprint,
  calculateOpsPerQuery,
  validateDimension,
} from './hdit/index.mjs';

// Temporal SPARQL - Time-Travel Queries with Nanosecond Precision
export { TemporalSPARQL } from './temporal-sparql.mjs';
export { parseTemporalQuery, extractBaseSparql, hasTemporalClauses, validateTemporalQuery } from './temporal-query-parser.mjs';
export { TemporalCache } from './temporal-cache.mjs';
export { HistoryReconstructor } from './history-reconstructor.mjs';
export {
  TemporalQuerySchema,
  TemporalCacheConfigSchema,
  TemporalResultMetadataSchema,
  TemporalResultSchema,
  TimeRangeResultSchema,
  TemporalEngineOptionsSchema,
  guardTemporalQueryValid,
  guardTemporalResultValid,
  guardCacheConfigValid,
} from './schemas/temporal-sparql-schema.mjs';
