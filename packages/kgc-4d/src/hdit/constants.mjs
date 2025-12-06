/**
 * @fileoverview HDIT dimensional constants for universal JS event engine
 *
 * Practical thresholds for hyperdimensional information theory in browser/Node.
 *
 * Memory footprint for Float32Array:
 *   RAM ≈ N × d × 4 bytes
 *
 * Browser sweet spot (N = 10^5 entities, 50-200 MB budget):
 *   d = 32  → ~13 MB
 *   d = 64  → ~26 MB  ← DEFAULT
 *   d = 128 → ~51 MB
 *
 * CPU latency budget (interactive < 10ms):
 *   Nd ≲ 10^7 operations per query
 *
 * @module @unrdf/kgc-4d/hdit/constants
 */

/**
 * Default dimension for browser environments
 * Balances memory (26MB for 100k entities) with expressiveness
 * @type {number}
 */
export const D_BROWSER = 64;

/**
 * Light dimension for memory-constrained environments
 * @type {number}
 */
export const D_LIGHT = 32;

/**
 * Medium dimension for mixed Node/browser workloads
 * @type {number}
 */
export const D_MEDIUM = 128;

/**
 * Maximum dimension for Node-only batch analytics
 * Beyond this, you need sharding or indexing
 * @type {number}
 */
export const D_NODE_MAX = 256;

/**
 * Heavy dimension - use only with low cardinality (N < 10k)
 * @type {number}
 */
export const D_HEAVY = 512;

/**
 * Default dimension for coordsForEvent
 * @type {number}
 */
export const D_DEFAULT = D_BROWSER;

/**
 * Maximum entities in browser before requiring sharding
 * Based on Nd ≲ 10^7 for interactive queries
 * @type {number}
 */
export const N_BROWSER_MAX = 100_000;

/**
 * Interactive query latency budget (milliseconds)
 * @type {number}
 */
export const LATENCY_BUDGET_MS = 10;

/**
 * Operations budget per query for interactive feel
 * @type {number}
 */
export const OPS_BUDGET = 10_000_000;

/**
 * Named dimension indices for ontology mapping
 * First few coordinates are explicitly semantic
 */
export const ONTOLOGY_AXES = {
  /** Normalized time coordinate [0, 1] */
  TIME: 0,
  /** Event type encoding */
  EVENT_TYPE: 1,
  /** Log-frequency of entity references */
  LOG_FREQUENCY: 2,
  /** Risk/importance score */
  RISK_SCORE: 3,
  /** Causality depth (vector clock sum) */
  CAUSALITY_DEPTH: 4,
  /** Mutation count */
  MUTATION_COUNT: 5,
  /** User/agent ID hash */
  AGENT_HASH: 6,
  /** Graph context hash */
  GRAPH_HASH: 7,

  /** Reserved for future ontology extensions */
  RESERVED_END: 16,
};

/**
 * Number of ontology-mapped axes
 * Remaining dimensions are "anonymous capacity axes"
 * @type {number}
 */
export const ONTOLOGY_DIM = ONTOLOGY_AXES.RESERVED_END;

/**
 * Event type encodings for EVENT_TYPE axis
 */
export const EVENT_TYPE_ENCODING = {
  CREATE: 0.2,
  UPDATE: 0.4,
  DELETE: 0.6,
  SNAPSHOT: 0.8,
  HOOK_EXECUTION: 1.0,
};

/**
 * Bytes per coordinate (Float32Array)
 * @type {number}
 */
export const BYTES_PER_COORD = 4;

/**
 * Calculate memory footprint for dimensional storage
 * @param {number} numEntities - Number of entities
 * @param {number} dimension - Vector dimension
 * @returns {number} Memory in bytes
 */
export function calculateMemoryFootprint(numEntities, dimension) {
  return numEntities * dimension * BYTES_PER_COORD;
}

/**
 * Calculate operations per query
 * @param {number} numEntities - Number of entities to compare
 * @param {number} dimension - Vector dimension
 * @returns {number} Number of multiply-add operations
 */
export function calculateOpsPerQuery(numEntities, dimension) {
  return numEntities * dimension;
}

/**
 * Validate dimension choice against environment
 * @param {number} dimension - Proposed dimension
 * @param {'browser' | 'node'} env - Target environment
 * @returns {{ valid: boolean, reason?: string }}
 */
export function validateDimension(dimension, env = 'browser') {
  if (dimension < D_LIGHT) {
    return { valid: false, reason: `Dimension ${dimension} too low (min: ${D_LIGHT})` };
  }

  if (env === 'browser' && dimension > D_MEDIUM) {
    return { valid: false, reason: `Browser dimension ${dimension} exceeds safe limit (max: ${D_MEDIUM})` };
  }

  if (env === 'node' && dimension > D_NODE_MAX) {
    return { valid: false, reason: `Node dimension ${dimension} exceeds maximum (max: ${D_NODE_MAX})` };
  }

  return { valid: true };
}
