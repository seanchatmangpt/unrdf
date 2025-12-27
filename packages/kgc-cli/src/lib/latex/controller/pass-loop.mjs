/**
 * @fileoverview Multi-pass LaTeX Compilation Controller with Fixed-Point Detection
 *
 * Executes multiple compilation passes until:
 * 1. Fixed point is reached (aux/toc files unchanged)
 * 2. No "Rerun" messages in log
 * 3. Maximum passes reached
 *
 * Design Principles:
 * - Pure coordination logic (no OTEL in business logic)
 * - Deterministic fixed-point detection via byte-level comparison
 * - Bounded execution (no infinite loops)
 * - Clear progress events for observability
 * - Separation of concerns: compilation vs resolution vs convergence
 *
 * Integration Points:
 * - Agent 3 (swiftlatex-engine): Executes single compilation pass
 * - Agent 4 (ctan-resolver): Resolves missing inputs on failure
 * - Agent 6 (diagnostics): Parses rerun messages from log
 * - Agent 10 (compile): Calls this controller for full pipeline
 *
 * @module lib/latex/controller/pass-loop
 */

import { z } from 'zod';

// =============================================================================
// Type Definitions (JSDoc)
// =============================================================================

/**
 * @typedef {Object} PassLoopOptions
 * @property {Function} compile - Single compilation function (pass: number) => Promise<CompileResult>
 * @property {Map<string, Uint8Array>} vfs - Virtual file system (mutated during resolution)
 * @property {number} [maxPasses=5] - Maximum compilation passes
 * @property {number} [maxResolveRetries=3] - Maximum resolution retry attempts
 * @property {Function} [onProgress] - Progress callback (event) => void
 * @property {Function} [onResolve] - Resolution callback (missingInputs) => Promise<Map<string, Uint8Array>>
 */

/**
 * @typedef {Object} CompileResult
 * @property {boolean} ok - Compilation succeeded
 * @property {Uint8Array} [pdf] - Generated PDF bytes
 * @property {string} [log] - Compilation log output
 * @property {Map<string, Uint8Array>} [artifacts] - Intermediate files (.aux, .toc, etc.)
 * @property {string[]} [missingInputs] - Missing input files
 * @property {string} [error] - Error message
 */

/**
 * @typedef {Object} PassLoopResult
 * @property {boolean} success - Pipeline succeeded
 * @property {Uint8Array} [pdf] - Generated PDF bytes
 * @property {number} passes - Number of passes executed
 * @property {string} log - Final compilation log
 * @property {string} [error] - Error message if failed
 * @property {string} [terminationReason] - Why loop terminated
 */

/**
 * @typedef {Object} ProgressEvent
 * @property {number} pass - Current pass number
 * @property {number} total - Total max passes
 * @property {'compiling'|'resolving'|'converged'|'failed'} status - Current status
 * @property {string[]} [missing] - Missing inputs being resolved
 */

// =============================================================================
// Validation Schemas
// =============================================================================

const PassLoopOptionsSchema = z.object({
  compile: z.function(),
  vfs: z.instanceof(Map),
  maxPasses: z.number().int().min(1).max(20).default(5),
  maxResolveRetries: z.number().int().min(0).max(10).default(3),
  onProgress: z.function().optional(),
  onResolve: z.function().optional(),
});

// =============================================================================
// Constants
// =============================================================================

/**
 * File extensions to check for fixed-point detection
 * These files contain cross-reference data that stabilizes when document converges
 */
const CONVERGENCE_FILES = ['.aux', '.toc', '.lof', '.lot'];

/**
 * Rerun detection patterns in LaTeX log
 * LaTeX emits these warnings when additional passes are needed
 */
const RERUN_PATTERNS = [
  /Rerun to get cross-references right/i,
  /Label\(s\) may have changed\. Rerun to get cross-references right/i,
  /Rerun LaTeX/i,
  /Please rerun LaTeX/i,
  /Table widths have changed\. Rerun LaTeX/i,
];

// =============================================================================
// Fixed-Point Detection
// =============================================================================

/**
 * Extract convergence-relevant files from artifacts
 * @param {Map<string, Uint8Array>} artifacts - Compilation artifacts
 * @returns {Map<string, Uint8Array>} Subset of artifacts used for convergence check
 */
function extractConvergenceFiles(artifacts) {
  const convergenceMap = new Map();

  if (!artifacts) {
    return convergenceMap;
  }

  for (const [filename, content] of artifacts.entries()) {
    // Check if file has convergence-relevant extension
    const hasConvergenceExt = CONVERGENCE_FILES.some(ext => filename.endsWith(ext));
    if (hasConvergenceExt) {
      convergenceMap.set(filename, content);
    }
  }

  return convergenceMap;
}

/**
 * Compare two artifact sets byte-for-byte
 * @param {Map<string, Uint8Array>} prev - Previous artifacts
 * @param {Map<string, Uint8Array>} curr - Current artifacts
 * @returns {boolean} True if all convergence files are identical
 */
function areArtifactsEqual(prev, curr) {
  // Extract convergence-relevant files
  const prevConvergence = extractConvergenceFiles(prev);
  const currConvergence = extractConvergenceFiles(curr);

  // Different set of files = not converged
  if (prevConvergence.size !== currConvergence.size) {
    return false;
  }

  // No convergence files to check = consider converged
  if (prevConvergence.size === 0) {
    return true;
  }

  // Compare each file byte-for-byte
  for (const [filename, prevContent] of prevConvergence.entries()) {
    const currContent = currConvergence.get(filename);

    if (!currContent) {
      return false; // File missing in current
    }

    // Byte-level comparison
    if (prevContent.length !== currContent.length) {
      return false;
    }

    for (let i = 0; i < prevContent.length; i++) {
      if (prevContent[i] !== currContent[i]) {
        return false;
      }
    }
  }

  return true;
}

/**
 * Check if log contains rerun request
 * @param {string} log - LaTeX compilation log
 * @returns {boolean} True if rerun is needed
 */
function needsRerun(log) {
  if (!log || typeof log !== 'string') {
    return false;
  }

  return RERUN_PATTERNS.some(pattern => pattern.test(log));
}

/**
 * Determine if fixed point is reached
 * @param {Map<string, Uint8Array>} prevArtifacts - Previous artifacts
 * @param {Map<string, Uint8Array>} currArtifacts - Current artifacts
 * @param {string} log - Current log
 * @returns {{ converged: boolean, reason: string }} Convergence status
 */
function checkFixedPoint(prevArtifacts, currArtifacts, log) {
  // Check for rerun messages first
  if (needsRerun(log)) {
    return {
      converged: false,
      reason: 'LaTeX requested rerun (cross-references changed)',
    };
  }

  // If first pass with no convergence files, consider converged
  // (simple documents don't need multiple passes)
  if (!prevArtifacts || prevArtifacts.size === 0) {
    const currConvergence = extractConvergenceFiles(currArtifacts);
    if (currConvergence.size === 0) {
      return {
        converged: true,
        reason: 'Single-pass document (no convergence files)',
      };
    }

    return {
      converged: false,
      reason: 'First pass (need to check for stability)',
    };
  }

  const artifactsEqual = areArtifactsEqual(prevArtifacts, currArtifacts);

  if (artifactsEqual) {
    return {
      converged: true,
      reason: 'Fixed point reached (artifacts unchanged)',
    };
  }

  return {
    converged: false,
    reason: 'Artifacts changed between passes',
  };
}

// =============================================================================
// Main Pass Loop Controller
// =============================================================================

/**
 * Execute multi-pass LaTeX compilation with fixed-point detection
 *
 * Algorithm:
 * 1. Run compilation pass
 * 2. If missing inputs and resolver available:
 *    a. Call onResolve to fetch missing files
 *    b. Augment VFS with resolved files
 *    c. Retry (up to maxResolveRetries)
 * 3. If successful:
 *    a. Check fixed-point convergence (aux/toc unchanged)
 *    b. Check for rerun messages in log
 *    c. If converged or max passes, terminate
 *    d. Otherwise, continue to next pass
 * 4. Return final result with termination reason
 *
 * Termination Conditions (in priority order):
 * 1. Compilation error (non-recoverable)
 * 2. Maximum passes reached
 * 3. Fixed point detected (artifacts stable + no rerun messages)
 * 4. Successful pass with no convergence files (single-pass document)
 *
 * @param {PassLoopOptions} options - Pass loop configuration
 * @returns {Promise<PassLoopResult>} Final compilation result
 *
 * @example
 * const result = await executePassLoop({
 *   compile: async (pass) => compileWithSwiftLatex({ ...opts, passes: 1 }),
 *   vfs: projectVfs,
 *   maxPasses: 5,
 *   maxResolveRetries: 3,
 *   onProgress: (event) => console.log(`Pass ${event.pass}/${event.total}`),
 *   onResolve: async (missing) => resolveMissingInputs({ missingInputs: missing, cacheDir })
 * });
 *
 * if (result.success) {
 *   console.log(`PDF generated in ${result.passes} passes`);
 *   console.log(`Termination: ${result.terminationReason}`);
 * }
 */
export async function executePassLoop(options) {
  // ============= Validation =============
  let validated;
  try {
    validated = PassLoopOptionsSchema.parse(options);
  } catch (err) {
    return {
      success: false,
      passes: 0,
      log: `Invalid options: ${err.message}`,
      error: err.message,
      terminationReason: 'validation_error',
    };
  }

  const {
    compile,
    vfs,
    maxPasses,
    maxResolveRetries,
    onProgress,
    onResolve,
  } = validated;

  // ============= State =============
  let pass = 0;
  let prevArtifacts = null;
  let lastLog = '';
  let lastError = '';
  let resolveRetries = 0;

  // ============= Pass Loop =============
  while (pass < maxPasses) {
    pass++;

    // Emit progress event
    onProgress?.({
      pass,
      total: maxPasses,
      status: 'compiling',
    });

    // Execute single compilation pass
    let result;
    try {
      result = await compile(pass);
    } catch (err) {
      return {
        success: false,
        passes: pass,
        log: lastLog,
        error: `Compilation threw exception: ${err.message}`,
        terminationReason: 'compile_exception',
      };
    }

    lastLog = result.log || '';

    // ============= Handle Compilation Failure =============
    if (!result.ok) {
      // Check if failure is due to missing inputs
      if (result.missingInputs && result.missingInputs.length > 0 && onResolve) {
        if (resolveRetries >= maxResolveRetries) {
          return {
            success: false,
            passes: pass - 1, // Don't count failed resolution attempt
            log: lastLog,
            error: `Failed to resolve missing inputs after ${maxResolveRetries} retries: ${result.missingInputs.join(', ')}`,
            terminationReason: 'max_resolve_retries',
          };
        }

        // Attempt resolution
        onProgress?.({
          pass,
          total: maxPasses,
          status: 'resolving',
          missing: result.missingInputs,
        });

        try {
          const resolvedMap = await onResolve(result.missingInputs);

          // Augment VFS with resolved files
          for (const [path, content] of resolvedMap.entries()) {
            vfs.set(path, content);
          }

          resolveRetries++;

          // Retry compilation (rewind pass counter so we retry the same pass)
          pass--;
          continue;
        } catch (resolveError) {
          return {
            success: false,
            passes: pass - 1, // Don't count failed resolution attempt
            log: lastLog,
            error: `Resolution failed: ${resolveError.message}`,
            terminationReason: 'resolution_error',
          };
        }
      }

      // Non-recoverable compilation error
      return {
        success: false,
        passes: pass,
        log: lastLog,
        error: result.error || 'Compilation failed',
        terminationReason: 'compilation_error',
      };
    }

    // ============= Compilation Succeeded =============

    // Reset resolve retry counter on success
    resolveRetries = 0;

    // Check if we have a PDF
    if (!result.pdf) {
      return {
        success: false,
        passes: pass,
        log: lastLog,
        error: 'Compilation succeeded but no PDF generated',
        terminationReason: 'no_pdf',
      };
    }

    // ============= Check Convergence =============
    const { converged, reason } = checkFixedPoint(
      prevArtifacts,
      result.artifacts,
      lastLog
    );

    // Update artifacts for next iteration
    prevArtifacts = result.artifacts;

    if (converged) {
      // Fixed point reached
      onProgress?.({
        pass,
        total: maxPasses,
        status: 'converged',
      });

      return {
        success: true,
        pdf: result.pdf,
        passes: pass,
        log: lastLog,
        terminationReason: reason,
      };
    }

    // ============= Check Max Passes =============
    if (pass >= maxPasses) {
      // Max passes reached (may or may not be converged)
      const warning = needsRerun(lastLog)
        ? ' (Warning: LaTeX still requesting rerun - may need more passes)'
        : '';

      return {
        success: true,
        pdf: result.pdf,
        passes: pass,
        log: lastLog,
        terminationReason: `Maximum passes reached${warning}`,
      };
    }

    // ============= Continue to Next Pass =============
    // Artifacts changed or rerun requested, continue iteration
  }

  // Should never reach here due to max passes check, but handle defensively
  return {
    success: false,
    passes: pass,
    log: lastLog,
    error: 'Pass loop terminated unexpectedly',
    terminationReason: 'unexpected_termination',
  };
}

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Check if a document needs multiple passes
 * @param {string} texContent - LaTeX source content
 * @returns {boolean} True if multi-pass likely needed
 */
export function needsMultiplePass(texContent) {
  if (!texContent || typeof texContent !== 'string') {
    return false;
  }

  // Indicators that multiple passes are needed:
  const multiPassIndicators = [
    /\\tableofcontents/,     // Table of contents
    /\\listoffigures/,       // List of figures
    /\\listoftables/,        // List of tables
    /\\ref\{/,               // Cross-references
    /\\pageref\{/,           // Page references
    /\\cite\{/,              // Citations
    /\\bibliography\{/,      // Bibliography
    /\\index\{/,             // Index
  ];

  return multiPassIndicators.some(pattern => pattern.test(texContent));
}

/**
 * Get recommended number of passes for a document
 * @param {string} texContent - LaTeX source content
 * @returns {number} Recommended pass count (1-5)
 */
export function getRecommendedPasses(texContent) {
  if (!needsMultiplePass(texContent)) {
    return 1;
  }

  // Check for bibliography (needs 3-4 passes: latex, bibtex, latex, latex)
  if (/\\bibliography\{/.test(texContent) || /\\cite\{/.test(texContent)) {
    return 4;
  }

  // Check for complex cross-references (needs 2-3 passes)
  if (/\\ref\{/.test(texContent) || /\\tableofcontents/.test(texContent)) {
    return 3;
  }

  return 2;
}

// =============================================================================
// Module Exports
// =============================================================================

export default executePassLoop;
