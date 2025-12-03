/**
 * @file Hook definition utilities for UNRDF Knowledge Hooks.
 * @module hooks/define-hook
 */

import { z } from 'zod';

/**
 * @typedef {import('n3').Quad} Quad
 */

/**
 * Hook trigger types.
 * @typedef {'before-add' | 'after-add' | 'before-query' | 'after-query' | 'before-remove' | 'after-remove'} HookTrigger
 */

/**
 * Validation function for hooks.
 * @callback ValidateFn
 * @param {Quad} quad - The quad to validate
 * @returns {boolean} - True if validation passes, false otherwise
 */

/**
 * Transformation function for hooks.
 * @callback TransformFn
 * @param {Quad} quad - The quad to transform
 * @returns {Quad} - The transformed quad
 */

/**
 * Hook definition configuration.
 * @typedef {Object} HookConfig
 * @property {string} name - Hook identifier
 * @property {HookTrigger} trigger - When to execute the hook
 * @property {ValidateFn} [validate] - Optional validation function
 * @property {TransformFn} [transform] - Optional transformation function
 * @property {Record<string, any>} [metadata] - Optional metadata
 */

/**
 * Defined hook with runtime state.
 * @typedef {Object} Hook
 * @property {string} name - Hook identifier
 * @property {HookTrigger} trigger - When to execute the hook
 * @property {ValidateFn} [validate] - Optional validation function
 * @property {TransformFn} [transform] - Optional transformation function
 * @property {Record<string, any>} [metadata] - Optional metadata
 */

/* ========================================================================= */
/* Zod Schemas                                                              */
/* ========================================================================= */

export const HookTriggerSchema = z.enum([
  'before-add',
  'after-add',
  'before-query',
  'after-query',
  'before-remove',
  'after-remove',
]);

export const HookConfigSchema = z.object({
  name: z.string().min(1, 'Hook name is required'),
  trigger: HookTriggerSchema,
  validate: z.function().args(z.any()).returns(z.boolean()).optional(),
  transform: z.function().args(z.any()).returns(z.any()).optional(),
  metadata: z.record(z.any()).optional(),
});

export const HookSchema = z.object({
  name: z.string(),
  trigger: HookTriggerSchema,
  validate: z.function().optional(),
  transform: z.function().optional(),
  metadata: z.record(z.any()).optional(),
});

/* ========================================================================= */
/* Public API                                                               */
/* ========================================================================= */

/**
 * Define a validation or transformation hook.
 *
 * @param {HookConfig} config - Hook configuration
 * @returns {Hook} - The defined hook
 * @throws {z.ZodError} - If configuration is invalid
 *
 * @example
 * const iriValidator = defineHook({
 *   name: 'validate-iri',
 *   trigger: 'before-add',
 *   validate: (quad) => {
 *     return quad.subject.termType === 'NamedNode';
 *   }
 * });
 */
export function defineHook(config) {
  const validated = HookConfigSchema.parse(config);

  if (!validated.validate && !validated.transform) {
    throw new Error('Hook must define either validate or transform function');
  }

  return {
    name: validated.name,
    trigger: validated.trigger,
    validate: validated.validate,
    transform: validated.transform,
    metadata: validated.metadata || {},
  };
}

/**
 * Validate a hook object.
 *
 * @param {any} hook - Hook to validate
 * @returns {boolean} - True if valid, false otherwise
 */
export function isValidHook(hook) {
  try {
    HookSchema.parse(hook);
    return hook.validate !== undefined || hook.transform !== undefined;
  } catch {
    return false;
  }
}

/**
 * Get hook metadata.
 *
 * @param {Hook} hook - Hook instance
 * @param {string} key - Metadata key
 * @returns {any} - Metadata value or undefined
 */
export function getHookMetadata(hook, key) {
  const validated = HookSchema.parse(hook);
  return validated.metadata?.[key];
}

/**
 * Check if hook has validation function.
 *
 * @param {Hook} hook - Hook instance
 * @returns {boolean} - True if hook has validate function
 */
export function hasValidation(hook) {
  const validated = HookSchema.parse(hook);
  return typeof validated.validate === 'function';
}

/**
 * Check if hook has transformation function.
 *
 * @param {Hook} hook - Hook instance
 * @returns {boolean} - True if hook has transform function
 */
export function hasTransformation(hook) {
  const validated = HookSchema.parse(hook);
  return typeof validated.transform === 'function';
}
