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
export function executeHook(hook, quad) {
  const validatedHook = HookSchema.parse(hook);

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
      if (!validationResult) {
        result.valid = false;
        result.error = `Validation failed for hook: ${validatedHook.name}`;
        return result;
      }
    }

    // Execute transformation if present
    if (hasTransformation(validatedHook)) {
      const transformed = validatedHook.transform(quad);
      result.quad = transformed;
    }

    return result;
  } catch (error) {
    result.valid = false;
    result.error = error instanceof Error ? error.message : String(error);
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
  const validatedHooks = z.array(HookSchema).parse(hooks);

  /** @type {HookResult[]} */
  const results = [];
  let currentQuad = quad;
  let chainValid = true;
  let chainError = undefined;

  for (const hook of validatedHooks) {
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

  return ChainResultSchema.parse({
    valid: chainValid,
    quad: currentQuad,
    results,
    error: chainError,
  });
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
  const validatedHooks = z.array(HookSchema).parse(hooks);
  const matchingHooks = validatedHooks.filter(h => h.trigger === trigger);
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
