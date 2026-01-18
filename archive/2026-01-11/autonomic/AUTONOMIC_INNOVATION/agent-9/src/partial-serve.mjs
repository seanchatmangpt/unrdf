/**
 * Partial Serve - Traffic routing engine
 * @module @autonomic/agent-9/partial-serve
 */

import { createHash } from 'node:crypto';
import { shadowWrite } from './shadow-write.mjs';

/**
 * @typedef {Object} PartialServeResult
 * @property {any} response - Handler response
 * @property {string} routing - 'legacy' | 'facade' | 'shadow'
 * @property {import('./mismatch-report.mjs').MismatchReport[]} mismatches - Any detected mismatches (if shadow mode)
 */

/**
 * Route requests between legacy and facade based on percentage
 * Supports random, hash-based, and shadow routing strategies
 *
 * @param {Object} router - Routing configuration
 * @param {number} [router.facadePercent=0] - Percentage to route to facade (0-100)
 * @param {string} [router.strategy='random'] - 'random' | 'hash-based' | 'shadow'
 * @param {Function} legacyHandler - Legacy implementation
 * @param {Function} facadeHandler - Facade implementation
 * @param {any} request - Request to handle
 * @returns {Promise<PartialServeResult>}
 *
 * @example
 * // Gradual rollout: 10% to facade
 * const result = await partialServe(
 *   { facadePercent: 10, strategy: 'hash-based' },
 *   legacyHandler,
 *   facadeHandler,
 *   request
 * );
 *
 * @example
 * // Full shadow mode: run both and compare
 * const result = await partialServe(
 *   { strategy: 'shadow' },
 *   legacyHandler,
 *   facadeHandler,
 *   request
 * );
 * console.log(result.mismatches); // Any detected mismatches
 */
export async function partialServe(router, legacyHandler, facadeHandler, request) {
  const { facadePercent = 0, strategy = 'random' } = router;

  // Determine routing based on strategy
  const routeDecision = selectRoute(request, facadePercent, strategy);

  if (routeDecision === 'shadow') {
    // Run both and compare
    const result = await shadowWrite(legacyHandler, facadeHandler, request, {
      useLegacyResult: true,
    });

    return {
      response: result.result,
      routing: 'shadow',
      mismatches: result.mismatch ? [result.report] : [],
    };
  }

  const handler = routeDecision === 'facade' ? facadeHandler : legacyHandler;
  const response = await handler(request);

  return {
    response,
    routing: routeDecision,
    mismatches: [],
  };
}

/**
 * Select routing destination
 * @private
 * @param {any} request
 * @param {number} facadePercent
 * @param {string} strategy
 * @returns {string} - 'legacy' | 'facade' | 'shadow'
 */
function selectRoute(request, facadePercent, strategy) {
  if (facadePercent === 0) return 'legacy';
  if (facadePercent === 100) return 'facade';

  switch (strategy) {
    case 'random':
      return Math.random() * 100 < facadePercent ? 'facade' : 'legacy';

    case 'hash-based': {
      // Deterministic routing based on request hash
      const hash = hashRequest(request);
      const bucket = hash % 100;
      return bucket < facadePercent ? 'facade' : 'legacy';
    }

    case 'shadow':
      // Always run both (100% shadow mode)
      return 'shadow';

    default:
      return 'legacy';
  }
}

/**
 * Hash request for deterministic routing
 * @private
 * @param {any} request
 * @returns {number} - Hash value (0-99)
 */
function hashRequest(request) {
  const json = JSON.stringify(request);
  const hash = createHash('sha256').update(json).digest();
  // Use first 4 bytes as integer, modulo 100
  return hash.readUInt32BE(0) % 100;
}
