/**
 * Shadow Write - Parallel execution and comparison
 * @module @autonomic/agent-9/shadow-write
 */

import { canonicalDiff } from './canonical-diff.mjs';
import { createMismatchReport } from './mismatch-report.mjs';

/**
 * @typedef {Object} ShadowWriteResult
 * @property {boolean} mismatch - Whether outputs differed
 * @property {any} result - The result to use (based on useLegacyResult)
 * @property {any} [legacyResult] - Legacy handler result
 * @property {any} [facadeResult] - Facade handler result
 * @property {string} [reportHash] - Content hash of mismatch report
 * @property {import('./mismatch-report.mjs').MismatchReport} [report] - Full mismatch report (if mismatch occurred)
 */

/**
 * Execute write operation in shadow mode
 * Runs legacy and facade handlers in parallel, compares results deterministically
 *
 * @param {Function} legacyHandler - Legacy implementation (input) => Promise<result>
 * @param {Function} facadeHandler - Facade implementation (input) => Promise<result>
 * @param {any} input - Operation input
 * @param {Object} options - Configuration
 * @param {boolean} [options.useLegacyResult=true] - Return legacy result (default: true)
 * @param {Function} [options.onMismatch] - Callback for mismatches (report) => void | Promise<void>
 * @returns {Promise<ShadowWriteResult>}
 *
 * @example
 * const result = await shadowWrite(
 *   async (x) => x + 1,
 *   async (x) => x + 2,
 *   5,
 *   {
 *     onMismatch: (report) => console.error('Mismatch:', report.hash)
 *   }
 * );
 * console.log(result.mismatch); // true
 * console.log(result.result); // 6 (legacy result by default)
 */
export async function shadowWrite(legacyHandler, facadeHandler, input, options = {}) {
  const { useLegacyResult = true, onMismatch } = options;

  // Execute both handlers in parallel
  const [legacyResult, facadeResult] = await Promise.allSettled([
    legacyHandler(input),
    facadeHandler(input),
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
      type: 'write',
      input,
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
