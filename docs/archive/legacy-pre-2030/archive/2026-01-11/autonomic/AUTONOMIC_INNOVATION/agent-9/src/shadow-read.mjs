/**
 * Shadow Read - Query validation mode
 * @module @autonomic/agent-9/shadow-read
 */

import { canonicalDiff } from './canonical-diff.mjs';
import { createMismatchReport } from './mismatch-report.mjs';

/**
 * @typedef {Object} ShadowReadResult
 * @property {boolean} mismatch - Whether outputs differed
 * @property {any} result - The result to use (legacy by default)
 * @property {any} [legacyResult] - Legacy handler result
 * @property {any} [facadeResult] - Facade handler result
 * @property {string} [reportHash] - Content hash of mismatch report
 * @property {import('./mismatch-report.mjs').MismatchReport} [report] - Full mismatch report (if mismatch occurred)
 */

/**
 * Execute read operation in shadow mode
 * Similar to shadowWrite but optimized for read operations
 * May include result set comparison (order-independent for SPARQL results)
 *
 * @param {Function} legacyHandler - Legacy query implementation
 * @param {Function} facadeHandler - Facade query implementation
 * @param {any} query - Query input
 * @param {Object} options - Configuration
 * @param {boolean} [options.useLegacyResult=true] - Return legacy result (default: true)
 * @param {Function} [options.onMismatch] - Callback for mismatches
 * @param {number} [options.timingToleranceMs=0] - Timing tolerance for eventual consistency
 * @returns {Promise<ShadowReadResult>}
 *
 * @example
 * const result = await shadowRead(
 *   async (sparql) => legacyStore.query(sparql),
 *   async (sparql) => facadeStore.query(sparql),
 *   'SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10'
 * );
 * console.log(result.mismatch); // false if results match
 */
export async function shadowRead(legacyHandler, facadeHandler, query, options = {}) {
  const { useLegacyResult = true, onMismatch } = options;

  // Execute both handlers in parallel
  const [legacyResult, facadeResult] = await Promise.allSettled([
    legacyHandler(query),
    facadeHandler(query),
  ]);

  // Handle execution failures
  const legacySuccess = legacyResult.status === 'fulfilled';
  const facadeSuccess = facadeResult.status === 'fulfilled';

  if (!legacySuccess && !facadeSuccess) {
    throw new Error('Both legacy and facade handlers failed');
  }

  // Prepare outputs for comparison
  const legacyOutput = legacySuccess
    ? legacyResult.value
    : { error: legacyResult.reason?.message || String(legacyResult.reason) };
  const facadeOutput = facadeSuccess
    ? facadeResult.value
    : { error: facadeResult.reason?.message || String(facadeResult.reason) };

  // Compare results using canonical diff
  const diff = canonicalDiff(legacyOutput, facadeOutput);

  if (diff.hasDifference) {
    const report = await createMismatchReport({
      type: 'read',
      input: query,
      legacyOutput,
      facadeOutput,
      difference: diff,
    });

    if (onMismatch) {
      await onMismatch(report);
    }

    return {
      mismatch: true,
      result: useLegacyResult
        ? legacySuccess
          ? legacyResult.value
          : facadeResult.value
        : facadeSuccess
          ? facadeResult.value
          : legacyResult.value,
      legacyResult: legacySuccess ? legacyResult.value : undefined,
      facadeResult: facadeSuccess ? facadeResult.value : undefined,
      reportHash: report.hash,
      report,
    };
  }

  return {
    mismatch: false,
    result: legacySuccess ? legacyResult.value : facadeResult.value,
  };
}
