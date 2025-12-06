/**
 * @fileoverview Dimension guards and validation for HDIT coordinate system
 *
 * Enforces memory, latency, and correctness constraints using OTEL instrumentation.
 *
 * "Andon & Poka Yoke" principle: Stop and fix root cause when limits violated.
 *
 * @module @unrdf/kgc-4d/hdit/guards
 */

import {
  D_BROWSER,
  D_LIGHT,
  D_MEDIUM,
  D_NODE_MAX,
  D_HEAVY,
  N_BROWSER_MAX,
  LATENCY_BUDGET_MS,
  OPS_BUDGET,
  calculateMemoryFootprint,
  calculateOpsPerQuery,
  validateDimension,
} from './constants.mjs';

/**
 * @typedef {Object} GuardResult
 * @property {boolean} allowed - Whether operation is allowed
 * @property {string} [reason] - Reason if not allowed
 * @property {Object} [metrics] - Performance/resource metrics
 */

/**
 * Guard: Check if dimension is safe for environment
 *
 * @param {number} dimension - Proposed dimension
 * @param {'browser' | 'node'} env - Target environment
 * @returns {GuardResult}
 *
 * @example
 * const result = guardDimension(512, 'browser');
 * if (!result.allowed) {
 *   console.error('Dimension too high:', result.reason);
 * }
 */
export function guardDimension(dimension, env = 'browser') {
  const validation = validateDimension(dimension, env);

  if (!validation.valid) {
    return {
      allowed: false,
      reason: validation.reason,
    };
  }

  return { allowed: true };
}

/**
 * Guard: Check if coordinate generation will exceed memory budget
 *
 * @param {number} numEntities - Number of entities
 * @param {number} dimension - Vector dimension
 * @param {number} [maxMemoryMB=200] - Maximum memory budget in MB
 * @returns {GuardResult}
 *
 * @example
 * const result = guardMemory(100000, 128, 100);
 * console.log(`Would use ${result.metrics.memoryMB}MB`);
 */
export function guardMemory(numEntities, dimension, maxMemoryMB = 200) {
  const memoryBytes = calculateMemoryFootprint(numEntities, dimension);
  const memoryMB = memoryBytes / (1024 * 1024);

  if (memoryMB > maxMemoryMB) {
    return {
      allowed: false,
      reason: `Memory ${memoryMB.toFixed(1)}MB exceeds budget ${maxMemoryMB}MB`,
      metrics: { memoryBytes, memoryMB, maxMemoryMB },
    };
  }

  return {
    allowed: true,
    metrics: { memoryBytes, memoryMB, maxMemoryMB },
  };
}

/**
 * Guard: Check if query will exceed latency budget
 *
 * @param {number} numEntities - Number of entities to search
 * @param {number} dimension - Vector dimension
 * @param {number} [budgetMs=LATENCY_BUDGET_MS] - Latency budget in ms
 * @returns {GuardResult}
 *
 * @example
 * const result = guardLatency(50000, 256);
 * if (!result.allowed) {
 *   console.warn('Query too slow, need indexing:', result.reason);
 * }
 */
export function guardLatency(numEntities, dimension, budgetMs = LATENCY_BUDGET_MS) {
  const ops = calculateOpsPerQuery(numEntities, dimension);

  // Rough estimate: 100M ops/sec in modern JS (conservative)
  const estimatedMs = (ops / 100_000_000) * 1000;

  if (estimatedMs > budgetMs) {
    return {
      allowed: false,
      reason: `Estimated ${estimatedMs.toFixed(1)}ms exceeds budget ${budgetMs}ms`,
      metrics: { ops, estimatedMs, budgetMs },
    };
  }

  if (ops > OPS_BUDGET) {
    return {
      allowed: false,
      reason: `Operations ${ops} exceed budget ${OPS_BUDGET}`,
      metrics: { ops, estimatedMs, budgetMs },
    };
  }

  return {
    allowed: true,
    metrics: { ops, estimatedMs, budgetMs },
  };
}

/**
 * Guard: Check if entity count is safe for browser
 *
 * @param {number} numEntities - Number of entities
 * @param {number} dimension - Vector dimension
 * @param {'browser' | 'node'} env - Target environment
 * @returns {GuardResult}
 */
export function guardEntityCount(numEntities, dimension, env = 'browser') {
  if (env === 'browser' && numEntities > N_BROWSER_MAX) {
    return {
      allowed: false,
      reason: `Entity count ${numEntities} exceeds browser safe limit ${N_BROWSER_MAX}`,
      metrics: { numEntities, limit: N_BROWSER_MAX },
    };
  }

  // Check if Nd product is reasonable
  const latencyGuard = guardLatency(numEntities, dimension);
  if (!latencyGuard.allowed) {
    return latencyGuard;
  }

  return { allowed: true, metrics: { numEntities } };
}

/**
 * Guard: Validate vector coordinates are well-formed
 *
 * @param {Float32Array} coords - Coordinate vector to validate
 * @param {number} expectedDim - Expected dimension
 * @returns {GuardResult}
 */
export function guardCoordinates(coords, expectedDim) {
  if (!(coords instanceof Float32Array)) {
    return {
      allowed: false,
      reason: 'Coordinates must be Float32Array',
    };
  }

  if (coords.length !== expectedDim) {
    return {
      allowed: false,
      reason: `Dimension mismatch: ${coords.length} vs expected ${expectedDim}`,
    };
  }

  // Check for NaN or Infinity
  for (let i = 0; i < coords.length; i++) {
    if (!Number.isFinite(coords[i])) {
      return {
        allowed: false,
        reason: `Invalid value at index ${i}: ${coords[i]}`,
      };
    }
  }

  return { allowed: true, metrics: { dimension: coords.length } };
}

/**
 * Comprehensive guard: Check all constraints for a coordinate operation
 *
 * @param {Object} config - Configuration
 * @param {number} config.numEntities - Number of entities
 * @param {number} config.dimension - Vector dimension
 * @param {'browser' | 'node'} [config.env='browser'] - Environment
 * @param {number} [config.maxMemoryMB=200] - Memory budget
 * @param {number} [config.latencyBudgetMs=LATENCY_BUDGET_MS] - Latency budget
 * @returns {GuardResult}
 *
 * @example
 * const result = guardAll({ numEntities: 100000, dimension: 128 });
 * if (!result.allowed) {
 *   throw new Error(`Guard failed: ${result.reason}`);
 * }
 */
export function guardAll(config) {
  const {
    numEntities,
    dimension,
    env = 'browser',
    maxMemoryMB = 200,
    latencyBudgetMs = LATENCY_BUDGET_MS,
  } = config;

  // Check dimension validity
  const dimGuard = guardDimension(dimension, env);
  if (!dimGuard.allowed) return dimGuard;

  // Check memory constraints
  const memGuard = guardMemory(numEntities, dimension, maxMemoryMB);
  if (!memGuard.allowed) return memGuard;

  // Check latency constraints
  const latencyGuard = guardLatency(numEntities, dimension, latencyBudgetMs);
  if (!latencyGuard.allowed) return latencyGuard;

  // Check entity count
  const entityGuard = guardEntityCount(numEntities, dimension, env);
  if (!entityGuard.allowed) return entityGuard;

  // All guards passed
  return {
    allowed: true,
    metrics: {
      ...memGuard.metrics,
      ...latencyGuard.metrics,
      ...entityGuard.metrics,
    },
  };
}

/**
 * Suggest optimal dimension for given constraints
 *
 * @param {number} numEntities - Expected number of entities
 * @param {'browser' | 'node'} [env='browser'] - Target environment
 * @param {number} [maxMemoryMB=200] - Memory budget
 * @returns {{ dimension: number, reason: string }}
 *
 * @example
 * const { dimension, reason } = suggestDimension(50000, 'browser', 100);
 * console.log(`Recommended dimension: ${dimension} (${reason})`);
 */
export function suggestDimension(numEntities, env = 'browser', maxMemoryMB = 200) {
  const candidates = [D_LIGHT, D_BROWSER, D_MEDIUM, D_NODE_MAX, D_HEAVY];

  for (const dim of candidates) {
    const result = guardAll({ numEntities, dimension: dim, env, maxMemoryMB });

    if (result.allowed) {
      return {
        dimension: dim,
        reason: `Safe for ${numEntities} entities in ${env} with ${maxMemoryMB}MB budget`,
      };
    }
  }

  // If even D_LIGHT fails, recommend sharding
  return {
    dimension: D_LIGHT,
    reason: `Entity count ${numEntities} too high - recommend sharding to <${N_BROWSER_MAX} per shard`,
  };
}

/**
 * Create OTEL span for coordinate operation with guard checks
 *
 * @param {string} operationName - Operation name
 * @param {Object} config - Guard configuration
 * @param {Function} operation - Operation to execute
 * @returns {Promise<Object>} Result with span data
 *
 * @example
 * const result = await guardedOperation('generate.coords',
 *   { numEntities: 1000, dimension: 64 },
 *   () => batchCoordsForEvents(events)
 * );
 */
export async function guardedOperation(operationName, config, operation) {
  const startTime = Date.now();
  const guardResult = guardAll(config);

  const span = {
    name: `hdit.${operationName}`,
    status: 'ok',
    duration: 0,
    attributes: {
      'service.name': 'unrdf',
      'operation.type': 'hdit',
      'hdit.dimension': config.dimension,
      'hdit.num_entities': config.numEntities,
      'hdit.env': config.env ?? 'browser',
      'guard.allowed': guardResult.allowed,
    },
  };

  if (!guardResult.allowed) {
    span.status = 'error';
    span.attributes['guard.failure_reason'] = guardResult.reason;
    span.duration = Date.now() - startTime;

    throw new Error(`Guard failed: ${guardResult.reason}`);
  }

  // Add guard metrics to span
  if (guardResult.metrics) {
    Object.entries(guardResult.metrics).forEach(([key, value]) => {
      span.attributes[`guard.${key}`] = value;
    });
  }

  try {
    const result = await operation();
    span.duration = Date.now() - startTime;
    span.attributes['operation.success'] = true;

    return { result, span };
  } catch (error) {
    span.status = 'error';
    span.duration = Date.now() - startTime;
    span.attributes['error.message'] = error.message;
    span.attributes['operation.success'] = false;

    throw error;
  }
}
