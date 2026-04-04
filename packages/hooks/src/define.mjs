/**
 * @unrdf/hooks/define
 *
 * Hook Definition API - Define, validate, and build knowledge hooks.
 *
 * @module @unrdf/hooks/define
 */

// ── Core hook definition ────────────────────────────────────────────────────
export {
  defineHook,
  isValidHook,
  getHookMetadata,
  hasValidation,
  hasTransformation,
  HookTriggerSchema,
  HookConfigSchema,
  HookSchema,
} from './hooks/define-hook.mjs';

// ── Knowledge hook schemas ──────────────────────────────────────────────────
export {
  KnowledgeHookSchema,
  HookMetaSchema,
  HookConditionSchema,
  HookEffectSchema,
  createKnowledgeHook,
  validateKnowledgeHook,
} from './hooks/schemas.mjs';

// ── Condition kind enums ────────────────────────────────────────────────────

/** @type {'sparql-ask'} SPARQL ASK boolean query */
export const SPARQL_ASK = 'sparql-ask';

/** @type {'sparql-select'} SPARQL SELECT tabular query */
export const SPARQL_SELECT = 'sparql-select';

/** @type {'shacl'} SHACL shapes validation */
export const SHACL = 'shacl';

/** @type {'delta'} Delta change detection */
export const DELTA = 'delta';

/** @type {'threshold'} Numeric threshold evaluation */
export const THRESHOLD = 'threshold';

/** @type {'count'} Count-based condition */
export const COUNT = 'count';

/** @type {'window'} Sliding window temporal condition */
export const WINDOW = 'window';

/** @type {'n3'} N3 forward-chaining reasoning */
export const N3 = 'n3';

/**
 * All condition kinds as a frozen object.
 * @type {Readonly<Record<string, string>>}
 */
export const ConditionKind = Object.freeze({
  SPARQL_ASK,
  SPARQL_SELECT,
  SHACL,
  DELTA,
  THRESHOLD,
  COUNT,
  WINDOW,
  N3,
});

// ── Hook definition builders ────────────────────────────────────────────────

import { defineHook as _defineHook } from './hooks/define-hook.mjs';

/**
 * Define a validation-only hook.
 *
 * @param {string} name - Hook name
 * @param {string} trigger - Hook trigger
 * @param {Function} validate - Validation function (quad) => boolean
 * @param {Record<string, any>} [metadata] - Optional metadata
 * @returns {object} Defined hook
 */
export function defineValidationHook(name, trigger, validate, metadata) {
  return _defineHook({ name, trigger, validate, metadata });
}

/**
 * Define a transformation-only hook.
 *
 * @param {string} name - Hook name
 * @param {string} trigger - Hook trigger
 * @param {Function} transform - Transform function (quad) => quad
 * @param {Record<string, any>} [metadata] - Optional metadata
 * @returns {object} Defined hook
 */
export function defineTransformHook(name, trigger, transform, metadata) {
  return _defineHook({ name, trigger, transform, metadata });
}

/**
 * Define a hook with both validation and transformation.
 *
 * @param {string} name - Hook name
 * @param {string} trigger - Hook trigger
 * @param {Function} validate - Validation function (quad) => boolean
 * @param {Function} transform - Transform function (quad) => quad
 * @param {Record<string, any>} [metadata] - Optional metadata
 * @returns {object} Defined hook
 */
export function defineValidateAndTransformHook(name, trigger, validate, transform, metadata) {
  return _defineHook({ name, trigger, validate, transform, metadata });
}
