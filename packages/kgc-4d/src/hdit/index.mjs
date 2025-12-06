/**
 * @fileoverview Universal JS Event Engine - Hyperdimensional Information Theory (HDIT)
 *
 * Practical implementation of HDIT for browser/Node environments.
 *
 * Core Principle:
 *   Treat the event universe as a hyperdimensional space where similarity,
 *   clustering, and retrieval are geometric operations in ℝ^d.
 *
 * Architecture:
 *   - μ: Event → ℝ^d (coordsForEvent)
 *   - Distance metrics (cosine, euclidean, manhattan)
 *   - Guards (memory, latency, dimension)
 *   - Projection (PCA, random) for visualization
 *
 * Constraints:
 *   - Browser: d ∈ [32, 128], N ≲ 100k entities
 *   - Node: d ∈ [128, 256] for batch work
 *   - Latency: Nd ≲ 10^7 ops for interactive queries
 *
 * @module @unrdf/kgc-4d/hdit
 *
 * @example
 * import { coordsForEvent, findKNearest, D_BROWSER } from '@unrdf/kgc-4d/hdit';
 *
 * // Generate coordinates for events
 * const coords = events.map(e => coordsForEvent(e, universe, D_BROWSER));
 *
 * // Find similar events
 * const neighbors = findKNearest(queryCoords, coords, 10, 'cosine');
 *
 * @example
 * import { guardAll, D_BROWSER } from '@unrdf/kgc-4d/hdit';
 *
 * // Validate before large operation
 * const guard = guardAll({ numEntities: 100000, dimension: D_BROWSER });
 * if (!guard.allowed) {
 *   console.error('Guard failed:', guard.reason);
 * }
 */

// Coordinate generation
export {
  coordsForEvent,
  batchCoordsForEvents,
  createUniverseContext,
} from './coords.mjs';

// Distance and similarity
export {
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
} from './distance.mjs';

// Guards and validation
export {
  guardDimension,
  guardMemory,
  guardLatency,
  guardEntityCount,
  guardCoordinates,
  guardAll,
  suggestDimension,
  guardedOperation,
} from './guards.mjs';

// Visualization and projection
export {
  projectPCA,
  projectRandom,
  normalizeProjection,
  createVisualizationData,
  clusterProjection,
} from './projection.mjs';

// Constants
export {
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
} from './constants.mjs';

// Vector Engine Worker (WASM-style performance)
export { VectorEngineClient } from './vector-engine-client.mjs';
