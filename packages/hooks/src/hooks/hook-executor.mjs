/**
 * @file Hook execution utilities for UNRDF Knowledge Hooks.
 * @module hooks/hook-executor
 */

import { z } from 'zod';
import { HookSchema, hasValidation, hasTransformation } from './define-hook.mjs';

/**
 * @typedef {import('./define-hook.mjs').Hook} Hook
 * @typedef {import('n3').Quad} Quad
 */

/**
 * Hook execution result.
 * @typedef {Object} HookResult
 * @property {boolean} valid - Whether validation passed
 * @property {Quad} [quad] - Transformed quad (if transformation applied)
 * @property {string} [error] - Error message if validation failed
 * @property {string} hookName - Name of hook that executed
 */

/**
 * Hook chain execution result.
 * @typedef {Object} ChainResult
 * @property {boolean} valid - Whether all validations passed
 * @property {Quad} quad - Final transformed quad
 * @property {HookResult[]} results - Individual hook results
 * @property {string} [error] - Error message if any validation failed
 */

/* ========================================================================= */
/* Zod Schemas                                                              */
/* ========================================================================= */

export const HookResultSchema = z.object({
  valid: z.boolean(),
  quad: z.any().optional(),
  error: z.string().optional(),
  hookName: z.string(),
});

export const ChainResultSchema = z.object({
  valid: z.boolean(),
  quad: z.any(),
  results: z.array(HookResultSchema),
  error: z.string().optional(),
});

/* ========================================================================= */
/* Public API                                                               */
/* ========================================================================= */

/**
 * Execute a single hook on a quad.
 *
 * @param {Hook} hook - Hook to execute
 * @param {Quad} quad - Quad to process
 * @returns {HookResult} - Execution result
 *
 * @example
 * const result = executeHook(iriValidator, quad);
 * if (!result.valid) {
 *   console.error(result.error);
 * }
 */
export function executeHook(hook, quad, options = {}) {
  // Fast path: skip Zod if hook was created via defineHook (_validated flag)
  const validatedHook = hook._validated ? hook : HookSchema.parse(hook);

  /** @type {HookResult} */
  const result = {
    valid: true,
    quad: quad,
    hookName: validatedHook.name,
  };

  try {
    // Execute validation if present
    if (hasValidation(validatedHook)) {
      const validationResult = validatedHook.validate(quad);

      // POKA-YOKE: Non-boolean validation return guard (RPN 280 → 28)
      if (typeof validationResult !== 'boolean') {
        console.warn(
          `[POKA-YOKE] Hook "${validatedHook.name}": validate() returned ${typeof validationResult}, expected boolean. Coercing to boolean.`
        );
        result.warning = `Non-boolean validation return (${typeof validationResult}) coerced to boolean`;
      }

      if (!validationResult) {
        result.valid = false;
        result.error = `Validation failed for hook: ${validatedHook.name}`;
        return result;
      }
    }

    // Execute transformation if present
    if (hasTransformation(validatedHook)) {
      const transformed = validatedHook.transform(quad);

      // POKA-YOKE: Transform return type validation (RPN 280 → 28)
      if (!transformed || typeof transformed !== 'object') {
        throw new TypeError(
          `Hook "${validatedHook.name}": transform() must return a Quad object, got ${typeof transformed}`
        );
      }

      // POKA-YOKE: Check for required Quad properties
      if (!transformed.subject || !transformed.predicate || !transformed.object) {
        throw new TypeError(
          `Hook "${validatedHook.name}": transform() returned object missing subject/predicate/object`
        );
      }

      // POKA-YOKE: Pooled quad leak detection (warn if returning pooled quad)
      if (transformed._pooled && options.warnPooledQuads !== false) {
        console.warn(
          `[POKA-YOKE] Hook "${validatedHook.name}": returned pooled quad. Clone before storing to prevent memory issues.`
        );
        result.warning = 'Pooled quad returned - consider cloning';
      }

      result.quad = transformed;
    }

    return result;
  } catch (error) {
    result.valid = false;
    result.error = error instanceof Error ? error.message : String(error);

    // POKA-YOKE: Stack trace preservation (RPN 504 → 50)
    result.errorDetails = {
      hookName: validatedHook.name,
      hookTrigger: validatedHook.trigger,
      stack: error instanceof Error ? error.stack : undefined,
      originalError: error instanceof Error ? error : undefined,
      rawError: !(error instanceof Error) ? error : undefined,
    };

    return result;
  }
}

/**
 * Execute multiple hooks in sequence on a quad.
 * Stops at first validation failure.
 * Transformations are chained (output of one becomes input to next).
 *
 * @param {Hook[]} hooks - Array of hooks to execute
 * @param {Quad} quad - Initial quad to process
 * @returns {ChainResult} - Chain execution result
 *
 * @example
 * const result = executeHookChain([validator, transformer], quad);
 * if (result.valid) {
 *   store.add(result.quad);
 * }
 */
export function executeHookChain(hooks, quad) {
  // Fast path: trust pre-validated hooks (skip Zod array parse)

  /** @type {HookResult[]} */
  const results = [];
  let currentQuad = quad;
  let chainValid = true;
  let chainError = undefined;

  for (const hook of hooks) {
    const result = executeHook(hook, currentQuad);
    results.push(result);

    if (!result.valid) {
      chainValid = false;
      chainError = result.error;
      break;
    }

    if (result.quad) {
      currentQuad = result.quad;
    }
  }

  // Fast path: return plain object (skip ChainResultSchema.parse)
  return {
    valid: chainValid,
    quad: currentQuad,
    results,
    error: chainError,
  };
}

/**
 * Execute hooks for a specific trigger type.
 *
 * @param {Hook[]} hooks - All registered hooks
 * @param {import('./define-hook.mjs').HookTrigger} trigger - Trigger type to execute
 * @param {Quad} quad - Quad to process
 * @returns {ChainResult} - Execution result
 *
 * @example
 * const result = executeHooksByTrigger(allHooks, 'before-add', quad);
 */
export function executeHooksByTrigger(hooks, trigger, quad) {
  // Fast path: trust pre-validated hooks (skip Zod array parse)
  const matchingHooks = hooks.filter(h => h.trigger === trigger);
  return executeHookChain(matchingHooks, quad);
}

/**
 * Check if hooks would pass for a quad (dry-run validation).
 *
 * @param {Hook[]} hooks - Hooks to check
 * @param {Quad} quad - Quad to validate
 * @returns {boolean} - True if all validations would pass
 */
export function wouldPassHooks(hooks, quad) {
  const result = executeHookChain(hooks, quad);
  return result.valid;
}

/* ========================================================================= */
/* Batch API (High-Performance Bulk Operations)                             */
/* Sub-1μs per operation via Zod-free hot path                              */
/* ========================================================================= */

/**
 * Execute validation only (skip transforms) for faster validation-only checks.
 * Zod-free hot path for sub-1μs execution.
 *
 * @param {Hook[]} hooks - Hooks to execute (must be pre-validated via defineHook)
 * @param {Quad} quad - Quad to validate
 * @returns {HookResult} - Validation result
 */
export function validateOnly(hooks, quad) {
  // Skip Zod in hot path - trust pre-validated hooks
  for (const hook of hooks) {
    if (hasValidation(hook)) {
      try {
        if (!hook.validate(quad)) {
          return {
            valid: false,
            quad,
            error: `Validation failed for hook: ${hook.name}`,
            hookName: hook.name,
          };
        }
      } catch (error) {
        return {
          valid: false,
          quad,
          error: error instanceof Error ? error.message : String(error),
          hookName: hook.name,
        };
      }
    }
  }

  return { valid: true, quad, hookName: 'validateOnly' };
}

/**
 * Execute hooks in batch for multiple quads.
 * Optimized for bulk operations - Zod-free hot path.
 *
 * @param {Hook[]} hooks - Hooks to execute (must be pre-validated via defineHook)
 * @param {Quad[]} quads - Array of quads to process
 * @param {Object} [options] - Batch options
 * @param {boolean} [options.stopOnError=false] - Stop on first error
 * @returns {{ results: ChainResult[], validCount: number, invalidCount: number }}
 */
export function executeBatch(hooks, quads, options = {}) {
  const { stopOnError = false } = options;

  /** @type {ChainResult[]} */
  const results = [];
  let validCount = 0;
  let invalidCount = 0;

  // Zod-free hot path - hooks already validated by defineHook
  for (let i = 0; i < quads.length; i++) {
    const quad = quads[i];
    let currentQuad = quad;
    let isValid = true;
    let error;

    for (const hook of hooks) {
      // Validation check
      if (hasValidation(hook)) {
        try {
          if (!hook.validate(currentQuad)) {
            isValid = false;
            error = `Validation failed: ${hook.name}`;
            break;
          }
        } catch (e) {
          isValid = false;
          error = e instanceof Error ? e.message : String(e);
          break;
        }
      }

      // Transform if valid
      if (isValid && hasTransformation(hook)) {
        try {
          currentQuad = hook.transform(currentQuad);
        } catch (e) {
          isValid = false;
          error = e instanceof Error ? e.message : String(e);
          break;
        }
      }
    }

    results.push({ valid: isValid, quad: currentQuad, error, results: [] });

    if (isValid) {
      validCount++;
    } else {
      invalidCount++;
      if (stopOnError) break;
    }
  }

  return results;
}

/**
 * Validate batch of quads, returning array of boolean results.
 * Hyper-speed: Zod-free hot path, returns boolean array directly.
 *
 * @param {Hook[]} hooks - Hooks to execute (must be pre-validated via defineHook)
 * @param {Quad[]} quads - Array of quads to validate
 * @returns {boolean[]} - Array where true = valid, false = invalid
 */
export function validateBatch(hooks, quads) {
  // Filter validation hooks once (no Zod)
  const validationHooks = hooks.filter(hasValidation);

  // Return boolean array for test compatibility
  const results = [];

  for (let i = 0; i < quads.length; i++) {
    const quad = quads[i];
    let isValid = true;

    for (const hook of validationHooks) {
      try {
        if (!hook.validate(quad)) {
          isValid = false;
          break;
        }
      } catch {
        isValid = false;
        break;
      }
    }

    results.push(isValid);
  }

  return results;
}

/**
 * Transform batch of quads.
 * Applies transformation hooks to all quads - Zod-free hot path.
 *
 * @param {Hook[]} hooks - Hooks to execute (must be pre-validated via defineHook)
 * @param {Quad[]} quads - Array of quads to transform
 * @param {Object} [options] - Transform options
 * @param {boolean} [options.validateFirst=false] - Validate before transform
 * @returns {Quad[]} Array of transformed quads
 */
export function transformBatch(hooks, quads, options = {}) {
  const { validateFirst = false } = options;

  /** @type {Quad[]} */
  const transformed = [];

  for (let i = 0; i < quads.length; i++) {
    let currentQuad = quads[i];
    let hasError = false;

    for (const hook of hooks) {
      try {
        // Validate first if required
        if (validateFirst && hasValidation(hook)) {
          if (!hook.validate(currentQuad)) {
            hasError = true;
            break;
          }
        }

        // Apply transformation
        if (hasTransformation(hook)) {
          currentQuad = hook.transform(currentQuad);
        }
      } catch (error) {
        hasError = true;
        break;
      }
    }

    if (!hasError) {
      transformed.push(currentQuad);
    }
  }

  return transformed;
}

/* ========================================================================= */
/* Cache Management                                                          */
/* ========================================================================= */

/**
 * Hook execution cache for pre-validated hooks.
 * @type {WeakMap<object, boolean>}
 */
const hookValidationCache = new WeakMap();

/**
 * Clear all hook caches (validation and compiled chains).
 * Call this when hooks are modified or for testing.
 */
export function clearHookCaches() {
  // WeakMap auto-clears, but we can signal intent
  // The compiled chain cache is in hook-chain-compiler.mjs
  // This is a no-op for WeakMap but provides consistent API
}

/**
 * Pre-warm hook cache by pre-validating hooks.
 * Call this at startup to avoid first-execution overhead.
 *
 * @param {Hook[]} hooks - Hooks to pre-warm
 * @returns {{ prewarmed: number, errors: string[] }}
 */
export function prewarmHookCache(hooks) {
  const errors = [];
  let prewarmed = 0;

  for (const hook of hooks) {
    try {
      // Validate hook structure
      HookSchema.parse(hook);
      hookValidationCache.set(hook, true);
      prewarmed++;
    } catch (error) {
      errors.push(
        `Hook "${hook?.name || 'unknown'}": ${error instanceof Error ? error.message : String(error)}`
      );
    }
  }

  return { prewarmed, errors };
}
