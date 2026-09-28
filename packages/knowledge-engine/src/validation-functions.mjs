/**
 * @file Validation functions for knowledge engine schemas
 * @module validation-functions
 *
 * @description
 * Type-safe validation and creation functions for hooks, events, and conditions.
 */

import { z } from 'zod';
import {
  KnowledgeHookSchema,
  HookEventSchema,
  ConditionSchema,
} from './hook-schemas.mjs';
import {
  ManagerConfigSchema,
} from './config-schemas.mjs';
import {
  TransactionDeltaSchema,
} from './transaction-schemas.mjs';

/**
 * Validate a knowledge hook definition
 * @param {any} hook - The hook definition to validate
 * @returns {Object} Validation result
 */
export function validateKnowledgeHook(hook) {
  try {
    const validated = KnowledgeHookSchema.parse(hook);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: (error.issues || error.errors || []).map(err => ({
          path: err.path?.join('.') || 'unknown',
          message: err.message || 'Unknown error',
          code: err.code || 'unknown',
          received: err.received,
          expected: err.expected,
        })),
      };
    }
    throw error;
  }
}

/**
 * Validate a hook event
 * @param {any} event - The event to validate
 * @returns {Object} Validation result
 */
export function validateHookEvent(event) {
  try {
    const validated = HookEventSchema.parse(event);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: (error.issues || error.errors || []).map(err => ({
          path: err.path?.join('.') || 'unknown',
          message: err.message || 'Unknown error',
          code: err.code || 'unknown',
          received: err.received,
          expected: err.expected,
        })),
      };
    }
    throw error;
  }
}

/**
 * Validate a condition
 * @param {any} condition - The condition to validate
 * @returns {Object} Validation result
 */
export function validateCondition(condition) {
  try {
    const validated = ConditionSchema.parse(condition);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: (error.issues || error.errors || []).map(err => ({
          path: err.path?.join('.') || 'unknown',
          message: err.message || 'Unknown error',
          code: err.code || 'unknown',
          received: err.received,
          expected: err.expected,
        })),
      };
    }
    throw error;
  }
}

/**
 * Validate manager configuration
 * @param {any} config - The configuration to validate
 * @returns {Object} Validation result
 */
export function validateManagerConfig(config) {
  try {
    const validated = ManagerConfigSchema.parse(config);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors:
          error.errors?.map(err => ({
            path: err.path?.join('.') || 'unknown',
            message: err.message || 'Unknown error',
            code: err.code || 'unknown',
          })) || [],
      };
    }
    throw error;
  }
}

/**
 * Validate transaction delta
 * @param {any} delta - The delta to validate
 * @returns {Object} Validation result
 */
export function validateTransactionDelta(delta) {
  try {
    const validated = TransactionDeltaSchema.parse(delta);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors:
          error.errors?.map(err => ({
            path: err.path?.join('.') || 'unknown',
            message: err.message || 'Unknown error',
            code: err.code || 'unknown',
          })) || [],
      };
    }
    throw error;
  }
}

/**
 * Type-safe hook definition creator
 * @param {any} definition - The hook definition
 * @returns {Object} Validated and frozen hook definition
 */
export function createKnowledgeHook(definition) {
  try {
    const validation = validateKnowledgeHook(definition);

    if (!validation.success) {
      console.error('Validation errors:', validation.errors);
      console.error('Definition:', definition);
      const errorMessages =
        validation.errors
          ?.map(err => {
            let msg = `${err.path}: ${err.message}`;
            if (err.received !== undefined) {
              msg += ` (received: ${JSON.stringify(err.received)})`;
            }
            if (err.expected !== undefined) {
              msg += ` (expected: ${err.expected})`;
            }
            return msg;
          })
          .join(', ') || 'Unknown validation error';
      throw new TypeError(`Invalid knowledge hook definition: ${errorMessages}`);
    }

    // Apply defaults and freeze
    const normalized = {
      ...validation.data,
      determinism: { seed: 42, ...validation.data.determinism },
      receipt: { anchor: 'none', ...validation.data.receipt },
      priority: validation.data.priority ?? 50,
    };

    return Object.freeze(normalized);
  } catch (error) {
    if (error instanceof TypeError) {
      throw error;
    }
    console.error('Unexpected error in createKnowledgeHook:', error);
    throw new TypeError(`Invalid knowledge hook definition: ${error.message}`);
  }
}

/**
 * Type-safe event creator
 * @param {any} event - The event definition
 * @returns {Object} Validated event
 */
export function createHookEvent(event) {
  const validation = validateHookEvent(event);

  if (!validation.success) {
    const errorMessages =
      validation.errors
        ?.map(err => {
          let msg = `${err.path}: ${err.message}`;
          if (err.received !== undefined) {
            msg += ` (received: ${JSON.stringify(err.received)})`;
          }
          if (err.expected !== undefined) {
            msg += ` (expected: ${err.expected})`;
          }
          return msg;
        })
        .join(', ') || 'Unknown validation error';
    throw new TypeError(`Invalid hook event: ${errorMessages}`);
  }

  // Apply defaults
  const normalized = {
    ...validation.data,
    id: validation.data.id ?? crypto.randomUUID(),
    timestamp: validation.data.timestamp ?? new Date().toISOString(),
  };

  return normalized;
}

/**
 * Type-safe condition creator
 * @param {any} condition - The condition definition
 * @returns {Object} Validated condition
 */
export function createCondition(condition) {
  const validation = validateCondition(condition);

  if (!validation.success) {
    const errorMessages =
      validation.errors
        ?.map(err => {
          let msg = `${err.path}: ${err.message}`;
          if (err.received !== undefined) {
            msg += ` (received: ${JSON.stringify(err.received)})`;
          }
          if (err.expected !== undefined) {
            msg += ` (expected: ${err.expected})`;
          }
          return msg;
        })
        .join(', ') || 'Unknown validation error';
    throw new TypeError(`Invalid condition: ${errorMessages}`);
  }

  return validation.data;
}
