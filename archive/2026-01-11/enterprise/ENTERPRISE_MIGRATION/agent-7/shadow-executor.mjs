/**
 * Shadow Executor - Execute operations in shadow mode with comparison
 * @module agent-7/shadow-executor
 */

import { getMode, ROUTING_MODES } from './routing-modes.mjs';
import { recordMismatch } from './mismatch-ledger.mjs';

/**
 * Deep comparison of two values
 * @param {*} a - First value
 * @param {*} b - Second value
 * @returns {Object|null} Diff object or null if equal
 */
function deepCompare(a, b) {
  // Handle primitive types
  if (a === b) return null;

  // Handle null/undefined
  if (a == null || b == null) {
    return {
      type: 'NULL_MISMATCH',
      legacy: a,
      substrate: b,
    };
  }

  // Handle type mismatch
  const typeA = typeof a;
  const typeB = typeof b;
  if (typeA !== typeB) {
    return {
      type: 'TYPE_MISMATCH',
      legacyType: typeA,
      substrateType: typeB,
    };
  }

  // Handle arrays
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) {
      return {
        type: 'ARRAY_LENGTH_MISMATCH',
        legacyLength: a.length,
        substrateLength: b.length,
      };
    }

    for (let i = 0; i < a.length; i++) {
      const diff = deepCompare(a[i], b[i]);
      if (diff) {
        return {
          type: 'ARRAY_ELEMENT_MISMATCH',
          index: i,
          diff,
        };
      }
    }

    return null;
  }

  // Handle objects
  if (typeA === 'object') {
    const keysA = Object.keys(a).sort();
    const keysB = Object.keys(b).sort();

    // Check for missing/extra keys
    const missingInB = keysA.filter((k) => !(k in b));
    const extraInB = keysB.filter((k) => !(k in a));

    if (missingInB.length > 0 || extraInB.length > 0) {
      return {
        type: 'KEY_MISMATCH',
        missingInSubstrate: missingInB,
        extraInSubstrate: extraInB,
      };
    }

    // Compare values
    const mismatchedFields = [];
    for (const key of keysA) {
      const diff = deepCompare(a[key], b[key]);
      if (diff) {
        mismatchedFields.push({ field: key, diff });
      }
    }

    if (mismatchedFields.length > 0) {
      return {
        type: 'VALUE_MISMATCH',
        fields: mismatchedFields.map((m) => m.field),
        details: mismatchedFields,
      };
    }

    return null;
  }

  // Primitive value mismatch
  return {
    type: 'VALUE_MISMATCH',
    legacy: a,
    substrate: b,
  };
}

/**
 * Execute shadow write operation
 * @param {string} operation - Operation identifier
 * @param {*} payload - Write payload
 * @param {Function} legacyFn - Legacy system write function
 * @param {Function} substrateFn - Substrate system write function
 * @returns {Promise<Object>} Execution result
 */
export async function shadowWrite(operation, payload, legacyFn, substrateFn) {
  const mode = getMode(operation);

  if (mode !== ROUTING_MODES.SHADOW_WRITE) {
    throw new Error(
      `Operation ${operation} is not in SHADOW_WRITE mode (current: ${mode})`
    );
  }

  const startTime = Date.now();
  let legacyResult = null;
  let substrateResult = null;
  let legacyError = null;
  let substrateError = null;

  // Execute legacy (blocking)
  try {
    legacyResult = await legacyFn(payload);
  } catch (error) {
    legacyError = error;
    throw error; // Re-throw as legacy is authoritative
  }

  // Execute substrate (non-blocking shadow)
  try {
    substrateResult = await substrateFn(payload);
  } catch (error) {
    substrateError = error;
    // Don't throw - substrate failures are logged but don't block
  }

  const duration = Date.now() - startTime;

  // Compare results
  let diff = null;
  if (!substrateError && legacyResult !== substrateResult) {
    diff = deepCompare(legacyResult, substrateResult);
  }

  // Record mismatch if found
  if (diff) {
    recordMismatch(operation, legacyResult, substrateResult, diff);
  }

  return {
    success: true,
    mode: ROUTING_MODES.SHADOW_WRITE,
    operation,
    result: legacyResult, // Return legacy result as authoritative
    shadow: {
      executed: !substrateError,
      error: substrateError?.message || null,
      mismatch: diff !== null,
      diff,
    },
    duration,
    timestamp: Date.now(),
  };
}

/**
 * Execute shadow read operation
 * @param {string} operation - Operation identifier
 * @param {*} query - Read query
 * @param {Function} legacyFn - Legacy system read function
 * @param {Function} substrateFn - Substrate system read function
 * @returns {Promise<Object>} Execution result with comparison
 */
export async function shadowRead(operation, query, legacyFn, substrateFn) {
  const mode = getMode(operation);

  if (mode !== ROUTING_MODES.SHADOW_READ) {
    throw new Error(
      `Operation ${operation} is not in SHADOW_READ mode (current: ${mode})`
    );
  }

  const startTime = Date.now();

  // Execute both in parallel
  const [legacyResult, substrateResult] = await Promise.allSettled([
    legacyFn(query),
    substrateFn(query),
  ]);

  const duration = Date.now() - startTime;

  // Extract results and errors
  const legacy = {
    success: legacyResult.status === 'fulfilled',
    result: legacyResult.status === 'fulfilled' ? legacyResult.value : null,
    error: legacyResult.status === 'rejected' ? legacyResult.reason.message : null,
  };

  const substrate = {
    success: substrateResult.status === 'fulfilled',
    result: substrateResult.status === 'fulfilled' ? substrateResult.value : null,
    error: substrateResult.status === 'rejected' ? substrateResult.reason.message : null,
  };

  // If legacy failed, throw (authoritative)
  if (!legacy.success) {
    throw new Error(`Legacy read failed: ${legacy.error}`);
  }

  // Compare results if both succeeded
  let diff = null;
  if (legacy.success && substrate.success) {
    diff = deepCompare(legacy.result, substrate.result);
  }

  // Record mismatch if found
  if (diff) {
    recordMismatch(operation, legacy.result, substrate.result, diff);
  }

  return {
    success: true,
    mode: ROUTING_MODES.SHADOW_READ,
    operation,
    result: legacy.result, // Return legacy result as authoritative
    comparison: {
      matched: diff === null,
      diff,
      substrateFailed: !substrate.success,
      substrateError: substrate.error,
    },
    duration,
    timestamp: Date.now(),
  };
}

/**
 * Route partial traffic to substrate
 * @param {string} operation - Operation identifier
 * @param {*} payload - Request payload
 * @param {number} percentage - Percentage of traffic to route (0-100)
 * @param {Function} legacyFn - Legacy system function
 * @param {Function} substrateFn - Substrate system function
 * @returns {Promise<Object>} Execution result
 */
export async function partialServe(
  operation,
  payload,
  percentage,
  legacyFn,
  substrateFn
) {
  const mode = getMode(operation);

  if (mode !== ROUTING_MODES.PARTIAL_SERVE) {
    throw new Error(
      `Operation ${operation} is not in PARTIAL_SERVE mode (current: ${mode})`
    );
  }

  if (typeof percentage !== 'number' || percentage < 0 || percentage > 100) {
    throw new Error('Percentage must be a number between 0 and 100');
  }

  const startTime = Date.now();

  // Deterministic routing based on payload hash
  const routeToSubstrate = shouldRouteToSubstrate(payload, percentage);

  let result = null;
  let error = null;
  let routedTo = null;

  try {
    if (routeToSubstrate) {
      result = await substrateFn(payload);
      routedTo = 'substrate';
    } else {
      result = await legacyFn(payload);
      routedTo = 'legacy';
    }
  } catch (err) {
    error = err;

    // Fallback to legacy if substrate fails
    if (routeToSubstrate) {
      try {
        result = await legacyFn(payload);
        routedTo = 'legacy_fallback';
      } catch (fallbackErr) {
        throw fallbackErr;
      }
    } else {
      throw err;
    }
  }

  const duration = Date.now() - startTime;

  return {
    success: true,
    mode: ROUTING_MODES.PARTIAL_SERVE,
    operation,
    result,
    routing: {
      percentage,
      routedTo,
      hadFallback: routedTo === 'legacy_fallback',
      error: error?.message || null,
    },
    duration,
    timestamp: Date.now(),
  };
}

/**
 * Determine if request should route to substrate
 * @param {*} payload - Request payload
 * @param {number} percentage - Target percentage
 * @returns {boolean} True if should route to substrate
 */
function shouldRouteToSubstrate(payload, percentage) {
  // Simple deterministic hash based on JSON serialization
  const payloadStr = JSON.stringify(payload);
  let hash = 0;

  for (let i = 0; i < payloadStr.length; i++) {
    const char = payloadStr.charCodeAt(i);
    hash = (hash << 5) - hash + char;
    hash = hash & hash; // Convert to 32-bit integer
  }

  // Convert to 0-100 range
  const bucket = Math.abs(hash) % 100;

  return bucket < percentage;
}

/**
 * Execute operation based on current routing mode
 * @param {string} operation - Operation identifier
 * @param {*} payload - Request payload
 * @param {Function} legacyFn - Legacy system function
 * @param {Function} substrateFn - Substrate system function
 * @param {Object} options - Additional options
 * @returns {Promise<Object>} Execution result
 */
export async function execute(operation, payload, legacyFn, substrateFn, options = {}) {
  const mode = getMode(operation);
  const startTime = Date.now();

  let result = null;

  switch (mode) {
    case ROUTING_MODES.LEGACY_ONLY:
      result = await legacyFn(payload);
      break;

    case ROUTING_MODES.SUBSTRATE_ONLY:
      result = await substrateFn(payload);
      break;

    case ROUTING_MODES.SHADOW_WRITE:
      return await shadowWrite(operation, payload, legacyFn, substrateFn);

    case ROUTING_MODES.SHADOW_READ:
      return await shadowRead(operation, payload, legacyFn, substrateFn);

    case ROUTING_MODES.PARTIAL_SERVE:
      return await partialServe(
        operation,
        payload,
        options.percentage || 50,
        legacyFn,
        substrateFn
      );

    default:
      throw new Error(`Unknown routing mode: ${mode}`);
  }

  const duration = Date.now() - startTime;

  return {
    success: true,
    mode,
    operation,
    result,
    duration,
    timestamp: Date.now(),
  };
}
