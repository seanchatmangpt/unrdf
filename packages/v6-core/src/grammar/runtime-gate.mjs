/**
 * @fileoverview Runtime Complexity Gate - V6 Grammar Closure
 *
 * Enforce runtime complexity bounds with timeout wrappers.
 * Emit denial receipts for timed-out or memory-exceeded operations.
 *
 * @module @unrdf/v6-core/grammar/runtime-gate
 * @version 6.0.0-alpha.1
 */

import { z } from 'zod';
import { createHash } from 'crypto';

/**
 * Runtime bounds (from CLAUDE.md and GRAMMAR_MATRIX.md)
 */
export const RUNTIME_BOUNDS = {
  sparql: {
    maxResults: 10000,
    timeoutMs: 5000,      // Default from CLAUDE.md
    maxMemoryMB: 512,
  },
  shacl: {
    maxResults: 50000,
    timeoutMs: 10000,
    maxMemoryMB: 1024,
  },
  n3: {
    maxResults: 5000,
    timeoutMs: 15000,     // Extended for reasoning
    maxMemoryMB: 2048,
  },
  owl: {
    maxResults: 10000,
    timeoutMs: 20000,     // Extended for reasoning
    maxMemoryMB: 2048,
  },
  shex: {
    maxResults: 20000,
    timeoutMs: 8000,
    maxMemoryMB: 512,
  },
};

/**
 * Runtime check result schema
 */
export const RuntimeCheckResultSchema = z.object({
  allowed: z.boolean(),
  reason: z.string().optional(),
  bounds: z.object({
    timeoutMs: z.number(),
    maxResults: z.number().optional(),
  }),
  receipt: z.object({
    timestamp: z.string(),
    decision: z.enum(['ALLOW', 'DENY']),
  }),
});

/**
 * Execution result schema
 */
export const ExecutionResultSchema = z.object({
  success: z.boolean(),
  result: z.any().optional(),
  error: z.string().optional(),
  timeout: z.boolean().optional(),
  receipt: z.object({
    timestamp: z.string(),
    executionTimeMs: z.number(),
    status: z.enum(['SUCCESS', 'TIMEOUT', 'ERROR']),
    merkleProof: z.string().optional(),
  }),
});

/**
 * Check runtime complexity before execution
 *
 * Pre-execution gate - verify compiled query can run within bounds.
 *
 * @param {Object} query - Compiled query from compiler.compileGrammar()
 * @param {Object} store - RDF store (for size estimation)
 * @param {Object} [options] - Runtime options
 * @returns {Object} Check result { allowed, reason, bounds, receipt }
 *
 * @example
 * const check = checkRuntimeComplexity(compiled, store);
 * if (check.allowed) {
 *   const result = await executeQuery(compiled);
 * } else {
 *   console.error('Runtime denied:', check.reason);
 * }
 */
export function checkRuntimeComplexity(query, store, _options = {}) {
  const timestamp = new Date().toISOString();

  // Validate inputs
  if (!query || !query.grammarType) {
    return {
      allowed: false,
      reason: 'Invalid query - missing grammarType',
      bounds: {},
      receipt: { timestamp, decision: 'DENY' },
    };
  }

  // Get runtime bounds for this grammar
  const bounds = RUNTIME_BOUNDS[query.grammarType] || RUNTIME_BOUNDS.sparql;

  // Estimate store size (if available)
  const storeSize = store?.size || 0;

  // Check if store size * complexity exceeds memory bounds
  if (query.ast?.complexity) {
    const estimatedMemoryMB = estimateMemoryUsage(query.ast.complexity, storeSize);

    if (estimatedMemoryMB > bounds.maxMemoryMB) {
      return {
        allowed: false,
        reason: `Estimated memory ${estimatedMemoryMB}MB exceeds limit ${bounds.maxMemoryMB}MB`,
        bounds,
        receipt: {
          timestamp,
          decision: 'DENY',
        },
      };
    }
  }

  // All checks passed
  return {
    allowed: true,
    bounds,
    receipt: {
      timestamp,
      decision: 'ALLOW',
    },
  };
}

/**
 * Wrap function execution with timeout and error handling
 *
 * **Guarantee**: Never hangs indefinitely. Returns timeout receipt after maxMs.
 *
 * @param {Function} fn - Function to execute
 * @param {number} maxMs - Maximum execution time in milliseconds
 * @param {Object} [context] - Execution context for receipts
 * @returns {Promise<Object>} Execution result { success, result, receipt }
 *
 * @example
 * const result = await wrapWithTimeout(
 *   () => store.query(sparql),
 *   5000,
 *   { grammarType: 'sparql', query: sparql }
 * );
 *
 * if (result.timeout) {
 *   console.error('Query timed out:', result.receipt);
 * }
 */
export async function wrapWithTimeout(fn, maxMs, context = {}) {
  const startTime = context.t_ns ? Number(context.t_ns / 1_000_000n) : Date.now();
  const timestamp = new Date(startTime).toISOString();

  // Create timeout promise
  const timeoutPromise = new Promise((_, reject) => {
    setTimeout(() => {
      reject(new Error(`Execution timeout after ${maxMs}ms`));
    }, maxMs);
  });

  // Create execution promise
  const executionPromise = (async () => {
    try {
      const result = await fn();
      return { success: true, result };
    } catch (error) {
      return { success: false, error: error.message };
    }
  })();

  // Race execution vs timeout
  try {
    const outcome = await Promise.race([executionPromise, timeoutPromise]);
    const endTime = context.t_ns_end ? Number(context.t_ns_end / 1_000_000n) : Date.now();
    const executionTimeMs = endTime - startTime;

    return {
      success: outcome.success,
      result: outcome.result,
      error: outcome.error,
      timeout: false,
      receipt: {
        timestamp,
        executionTimeMs,
        status: outcome.success ? 'SUCCESS' : 'ERROR',
      },
    };
  } catch (error) {
    // Timeout occurred
    const endTime = context.t_ns_end ? Number(context.t_ns_end / 1_000_000n) : Date.now();
    const executionTimeMs = endTime - startTime;

    return {
      success: false,
      timeout: true,
      error: error.message,
      receipt: {
        timestamp,
        executionTimeMs,
        status: 'TIMEOUT',
        merkleProof: generateTimeoutProof(context, maxMs, executionTimeMs),
      },
    };
  }
}

/**
 * Emit denial receipt for runtime rejection
 *
 * @param {Object} query - Query that was denied
 * @param {string} reason - Denial reason
 * @param {Object} [details] - Additional details
 * @returns {Object} Denial receipt
 *
 * @example
 * const receipt = emitDenialReceipt(query, 'TIMEOUT', {
 *   timeoutMs: 5000,
 *   actualMs: 6543
 * });
 */
export function emitDenialReceipt(query, reason, details = {}) {
  const timestamp = new Date().toISOString();
  const deniedInput = JSON.stringify(query).slice(0, 500);

  const merkleProof = createHash('sha256')
    .update(JSON.stringify({ deniedInput, timestamp, reason, details }))
    .digest('hex');

  return {
    type: 'runtime/denial',
    timestamp,
    merkleProof,
    deniedInput,
    reason,
    details: {
      grammarType: query.grammarType,
      ...details,
    },
  };
}

/**
 * Execute query with full runtime gating
 *
 * Complete runtime gate: pre-check → timeout-wrapped execution → receipt.
 *
 * @param {Object} compiled - Compiled query from compiler.compileGrammar()
 * @param {Function} executeFn - Execution function (store) => result
 * @param {Object} store - RDF store
 * @param {Object} [options] - Runtime options
 * @returns {Promise<Object>} Execution result with receipt
 *
 * @example
 * const result = await executeWithGate(
 *   compiled,
 *   (store) => store.query(compiled.ast.queryString),
 *   myStore
 * );
 *
 * if (result.success) {
 *   console.log('Result:', result.result);
 * } else {
 *   console.error('Denied:', result.denialReceipt);
 * }
 */
export async function executeWithGate(compiled, executeFn, store, options = {}) {
  // Pre-execution complexity check
  const check = checkRuntimeComplexity(compiled, store, options);

  if (!check.allowed) {
    return {
      success: false,
      denied: true,
      reason: check.reason,
      denialReceipt: emitDenialReceipt(compiled, 'RUNTIME_COMPLEXITY_EXCEEDED', {
        reason: check.reason,
        bounds: check.bounds,
      }),
    };
  }

  // Execute with timeout wrapper
  const timeoutMs = options.timeoutMs || check.bounds.timeoutMs;
  const result = await wrapWithTimeout(
    () => executeFn(store),
    timeoutMs,
    { grammarType: compiled.grammarType, query: compiled }
  );

  // Handle timeout
  if (result.timeout) {
    return {
      success: false,
      timeout: true,
      denialReceipt: emitDenialReceipt(compiled, 'TIMEOUT', {
        timeoutMs,
        actualMs: result.receipt.executionTimeMs,
      }),
      receipt: result.receipt,
    };
  }

  // Handle error
  if (!result.success) {
    return {
      success: false,
      error: result.error,
      receipt: result.receipt,
    };
  }

  // Success
  return {
    success: true,
    result: result.result,
    receipt: result.receipt,
  };
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Estimate memory usage for query
 * @param {Object} complexity - Complexity metrics
 * @param {number} storeSize - Number of quads in store
 * @returns {number} Estimated memory in MB
 */
function estimateMemoryUsage(complexity, storeSize) {
  // Rough estimation:
  // - Base: 10MB for runtime
  // - Per triple pattern: storeSize * 0.001 MB
  // - Per filter: 1MB
  // - Per result: 0.01 MB

  const baseMB = 10;
  const patternMB = (complexity.triplePatterns || 0) * storeSize * 0.001;
  const filterMB = (complexity.filterComplexity || 0) * 1;
  const resultMB = (storeSize * 0.01);

  return baseMB + patternMB + filterMB + resultMB;
}

/**
 * Generate Merkle proof for timeout
 * @param {Object} context - Execution context
 * @param {number} maxMs - Timeout limit
 * @param {number} actualMs - Actual execution time
 * @returns {string} Merkle proof hash
 */
function generateTimeoutProof(context, maxMs, actualMs) {
  const data = JSON.stringify({
    context,
    maxMs,
    actualMs,
    timestamp: new Date().toISOString(),
  });

  return createHash('sha256').update(data).digest('hex');
}

/**
 * Format runtime bounds for display
 * @param {string} grammarType - Grammar type
 * @returns {Object} Formatted bounds
 */
export function getRuntimeBounds(grammarType) {
  return RUNTIME_BOUNDS[grammarType] || RUNTIME_BOUNDS.sparql;
}
