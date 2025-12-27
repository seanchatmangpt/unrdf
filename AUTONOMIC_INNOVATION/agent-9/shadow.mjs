/**
 * @file Shadow mode execution for safe migration
 * @description Parallel execution of legacy and facade systems with result comparison
 */

import { mismatchReport } from './mismatch-report.mjs';
import { routingDecision } from './routing.mjs';

/**
 * @typedef {Object} ShadowWriteResult
 * @property {*} legacyResult - Result from legacy handler
 * @property {*} facadeResult - Result from facade handler
 * @property {boolean} match - Whether results match
 * @property {string} [mismatchHash] - Hash of mismatch if results differ
 * @property {Object} [mismatchReport] - Full mismatch report if results differ
 */

/**
 * @typedef {Object} ShadowReadResult
 * @property {*} legacyData - Data from legacy store
 * @property {*} facadeData - Data from facade store
 * @property {boolean} match - Whether data matches
 * @property {string} [mismatchHash] - Hash of mismatch if data differs
 * @property {Object} [mismatchReport] - Full mismatch report if data differs
 */

/**
 * Execute request against both legacy and facade handlers in parallel
 * Compare results and return both with match status
 *
 * @param {Function} legacyHandler - Legacy system handler (async)
 * @param {Function} facadeHandler - Facade system handler (async)
 * @param {*} request - Request to execute
 * @param {Object} [options] - Options
 * @param {number} [options.timeout=5000] - Timeout in milliseconds
 * @param {boolean} [options.throwOnMismatch=false] - Throw error on mismatch
 * @returns {Promise<ShadowWriteResult>} Results with match status
 */
export async function shadowWrite(legacyHandler, facadeHandler, request, options = {}) {
  const { timeout = 5000, throwOnMismatch = false } = options;

  // Execute both handlers in parallel with timeout
  const legacyPromise = Promise.race([
    legacyHandler(request),
    new Promise((_, reject) =>
      setTimeout(() => reject(new Error('Legacy handler timeout')), timeout)
    )
  ]);

  const facadePromise = Promise.race([
    facadeHandler(request),
    new Promise((_, reject) =>
      setTimeout(() => reject(new Error('Facade handler timeout')), timeout)
    )
  ]);

  let legacyResult, facadeResult, legacyError, facadeError;

  try {
    [legacyResult, facadeResult] = await Promise.allSettled([legacyPromise, facadePromise])
      .then(results => [
        results[0].status === 'fulfilled' ? results[0].value : null,
        results[1].status === 'fulfilled' ? results[1].value : null
      ]);
  } catch (err) {
    // Both failed catastrophically
    throw new Error(`Shadow write failed: ${err.message}`);
  }

  // Capture errors if any
  try {
    await legacyPromise;
  } catch (err) {
    legacyError = err;
  }

  try {
    await facadePromise;
  } catch (err) {
    facadeError = err;
  }

  // If legacy failed, throw (it's the source of truth)
  if (legacyError) {
    throw new Error(`Legacy handler failed: ${legacyError.message}`);
  }

  // Compare results
  const match = deepEqual(legacyResult, facadeResult);

  if (!match) {
    const report = mismatchReport(legacyResult, facadeResult, {
      source: 'shadowWrite',
      request: JSON.stringify(request).slice(0, 200) // Truncate for logging
    });

    if (throwOnMismatch) {
      throw new Error(`Shadow write mismatch: ${report.mismatchHash}`);
    }

    return {
      legacyResult,
      facadeResult,
      match: false,
      mismatchHash: report.mismatchHash,
      mismatchReport: report
    };
  }

  return {
    legacyResult,
    facadeResult,
    match: true
  };
}

/**
 * Query both legacy and facade stores in parallel
 * Compare data and return both with match status
 *
 * @param {Function} legacyStore - Legacy store query function (async)
 * @param {Function} facadeStore - Facade store query function (async)
 * @param {*} query - Query to execute
 * @param {Object} [options] - Options
 * @param {number} [options.timeout=5000] - Timeout in milliseconds
 * @returns {Promise<ShadowReadResult>} Data with match status
 */
export async function shadowRead(legacyStore, facadeStore, query, options = {}) {
  const { timeout = 5000 } = options;

  // Execute both queries in parallel with timeout
  const legacyPromise = Promise.race([
    legacyStore(query),
    new Promise((_, reject) =>
      setTimeout(() => reject(new Error('Legacy store timeout')), timeout)
    )
  ]);

  const facadePromise = Promise.race([
    facadeStore(query),
    new Promise((_, reject) =>
      setTimeout(() => reject(new Error('Facade store timeout')), timeout)
    )
  ]);

  let legacyData, facadeData;

  try {
    [legacyData, facadeData] = await Promise.all([legacyPromise, facadePromise]);
  } catch (err) {
    // If legacy fails, throw (it's the source of truth)
    // If only facade fails, return legacy with no match
    try {
      legacyData = await legacyPromise;
      return {
        legacyData,
        facadeData: null,
        match: false,
        mismatchReport: mismatchReport(legacyData, null, {
          source: 'shadowRead',
          error: 'Facade query failed'
        })
      };
    } catch (legacyErr) {
      throw new Error(`Legacy store failed: ${legacyErr.message}`);
    }
  }

  // Compare data
  const match = deepEqual(legacyData, facadeData);

  if (!match) {
    const report = mismatchReport(legacyData, facadeData, {
      source: 'shadowRead',
      query: JSON.stringify(query).slice(0, 200)
    });

    return {
      legacyData,
      facadeData,
      match: false,
      mismatchHash: report.mismatchHash,
      mismatchReport: report
    };
  }

  return {
    legacyData,
    facadeData,
    match: true
  };
}

/**
 * Serve request using routing decision between legacy and facade
 *
 * @param {Array<Object>} routes - Array of route configurations
 * @param {*} request - Request to serve
 * @param {Object} handlers - Handler functions
 * @param {Function} handlers.legacy - Legacy handler
 * @param {Function} handlers.facade - Facade handler
 * @param {Object} [options] - Options
 * @param {boolean} [options.shadowMode=false] - Execute non-selected handler in shadow
 * @returns {Promise<*>} Response from selected handler
 */
export async function partialServe(routes, request, handlers, options = {}) {
  const { shadowMode = false } = options;

  // Determine which handler to use
  const target = routingDecision(routes, request);

  if (target === 'facade') {
    if (shadowMode) {
      // Execute facade as primary, legacy in shadow
      const result = await shadowWrite(
        handlers.facade,
        handlers.legacy,
        request,
        { throwOnMismatch: false }
      );
      return result.legacyResult; // Return facade result (first arg to shadowWrite)
    } else {
      return handlers.facade(request);
    }
  } else {
    if (shadowMode) {
      // Execute legacy as primary, facade in shadow
      const result = await shadowWrite(
        handlers.legacy,
        handlers.facade,
        request,
        { throwOnMismatch: false }
      );
      return result.legacyResult; // Return legacy result
    } else {
      return handlers.legacy(request);
    }
  }
}

/**
 * Deep equality comparison for results
 * Handles objects, arrays, primitives, null, undefined
 *
 * @param {*} a - First value
 * @param {*} b - Second value
 * @returns {boolean} True if deeply equal
 */
function deepEqual(a, b) {
  // Handle primitives and null
  if (a === b) return true;
  if (a == null || b == null) return false;
  if (typeof a !== typeof b) return false;

  // Handle dates
  if (a instanceof Date && b instanceof Date) {
    return a.getTime() === b.getTime();
  }

  // Handle arrays
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) {
      if (!deepEqual(a[i], b[i])) return false;
    }
    return true;
  }

  // Handle objects
  if (typeof a === 'object' && typeof b === 'object') {
    const keysA = Object.keys(a).sort();
    const keysB = Object.keys(b).sort();

    if (keysA.length !== keysB.length) return false;
    if (!deepEqual(keysA, keysB)) return false;

    for (const key of keysA) {
      if (!deepEqual(a[key], b[key])) return false;
    }
    return true;
  }

  return false;
}
