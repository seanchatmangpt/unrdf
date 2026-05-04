/**
 * @file MI Output Aggregation Functions
 * @module @unrdf/yawl/data/aggregation-functions
 * @description Aggregates outputs from multiple task instances in YAWL MI patterns.
 * Provides sum, avg, min, max, concat, merge, and custom aggregation functions.
 */

import { z } from 'zod';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Schema for aggregation configuration
 */
export const AggregationConfigSchema = z.object({
  strategy: z.enum(['sum', 'avg', 'min', 'max', 'concat', 'merge', 'custom']),
  field: z.string().optional(),
  customFn: z.function().optional(),
  conflictResolution: z.enum(['first', 'last', 'merge', 'error']).default('last'),
});

// =============================================================================
// Core Aggregation Functions
// =============================================================================

/**
 * Sum numeric values from MI outputs
 * @param {Array<any>} outputs - Array of task instance outputs
 * @param {string} [field] - Optional field path to extract (e.g., 'result.amount')
 * @returns {number} Total sum of numeric values
 * @throws {Error} If values are not numeric
 * @example
 * sum([{ amount: 100 }, { amount: 200 }], 'amount') // => 300
 */
export function sum(outputs, field = null) {
  if (!Array.isArray(outputs)) {
    throw new Error('Outputs must be an array');
  }

  if (outputs.length === 0) {
    return 0;
  }

  const values = field ? outputs.map(o => getNestedField(o, field)) : outputs;

  const total = values.reduce((acc, val) => {
    const num = Number(val);
    if (isNaN(num)) {
      throw new Error(`Cannot sum non-numeric value: ${val}`);
    }
    return acc + num;
  }, 0);

  return total;
}

/**
 * Average numeric values from MI outputs
 * @param {Array<any>} outputs - Array of task instance outputs
 * @param {string} [field] - Optional field path to extract
 * @returns {number} Average of numeric values
 * @throws {Error} If values are not numeric or array is empty
 * @example
 * avg([{ score: 80 }, { score: 90 }, { score: 85 }], 'score') // => 85
 */
export function avg(outputs, field = null) {
  if (!Array.isArray(outputs)) {
    throw new Error('Outputs must be an array');
  }

  if (outputs.length === 0) {
    throw new Error('Cannot average empty array');
  }

  const total = sum(outputs, field);
  return total / outputs.length;
}

/**
 * Find minimum value from MI outputs
 * @param {Array<any>} outputs - Array of task instance outputs
 * @param {string} [field] - Optional field path to extract
 * @returns {number} Minimum value
 * @throws {Error} If values are not numeric or array is empty
 * @example
 * min([{ price: 100 }, { price: 50 }, { price: 75 }], 'price') // => 50
 */
export function min(outputs, field = null) {
  if (!Array.isArray(outputs)) {
    throw new Error('Outputs must be an array');
  }

  if (outputs.length === 0) {
    throw new Error('Cannot find min of empty array');
  }

  const values = field ? outputs.map(o => getNestedField(o, field)) : outputs;

  const minimum = values.reduce((minVal, val) => {
    const num = Number(val);
    if (isNaN(num)) {
      throw new Error(`Cannot compare non-numeric value: ${val}`);
    }
    return num < minVal ? num : minVal;
  }, Number.POSITIVE_INFINITY);

  return minimum;
}

/**
 * Find maximum value from MI outputs
 * @param {Array<any>} outputs - Array of task instance outputs
 * @param {string} [field] - Optional field path to extract
 * @returns {number} Maximum value
 * @throws {Error} If values are not numeric or array is empty
 * @example
 * max([{ quantity: 10 }, { quantity: 25 }, { quantity: 15 }], 'quantity') // => 25
 */
export function max(outputs, field = null) {
  if (!Array.isArray(outputs)) {
    throw new Error('Outputs must be an array');
  }

  if (outputs.length === 0) {
    throw new Error('Cannot find max of empty array');
  }

  const values = field ? outputs.map(o => getNestedField(o, field)) : outputs;

  const maximum = values.reduce((maxVal, val) => {
    const num = Number(val);
    if (isNaN(num)) {
      throw new Error(`Cannot compare non-numeric value: ${val}`);
    }
    return num > maxVal ? num : maxVal;
  }, Number.NEGATIVE_INFINITY);

  return maximum;
}

/**
 * Concatenate arrays or strings from MI outputs
 * @param {Array<any>} outputs - Array of task instance outputs
 * @param {string} [field] - Optional field path to extract
 * @returns {Array|string} Concatenated result
 * @example
 * concat([{ items: [1, 2] }, { items: [3, 4] }], 'items') // => [1, 2, 3, 4]
 * concat([{ msg: 'Hello' }, { msg: 'World' }], 'msg') // => 'HelloWorld'
 */
export function concat(outputs, field = null) {
  if (!Array.isArray(outputs)) {
    throw new Error('Outputs must be an array');
  }

  if (outputs.length === 0) {
    return [];
  }

  const values = field ? outputs.map(o => getNestedField(o, field)) : outputs;

  const firstValue = values[0];

  if (Array.isArray(firstValue)) {
    return values.flat();
  }

  if (typeof firstValue === 'string') {
    return values.join('');
  }

  return values;
}

/**
 * Merge objects from MI outputs
 * @param {Array<Object>} outputs - Array of task instance outputs
 * @param {string} [field] - Optional field path to extract
 * @param {string} [conflictResolution='last'] - How to handle key conflicts (first|last|merge|error)
 * @returns {Object} Merged object
 * @throws {Error} If conflict resolution is 'error' and conflicts exist
 * @example
 * merge([{ a: 1 }, { b: 2 }, { c: 3 }]) // => { a: 1, b: 2, c: 3 }
 * merge([{ x: 1 }, { x: 2 }], null, 'last') // => { x: 2 }
 */
export function merge(outputs, field = null, conflictResolution = 'last') {
  if (!Array.isArray(outputs)) {
    throw new Error('Outputs must be an array');
  }

  if (outputs.length === 0) {
    return {};
  }

  const values = field ? outputs.map(o => getNestedField(o, field)) : outputs;

  const result = {};

  for (const obj of values) {
    if (typeof obj !== 'object' || obj === null || Array.isArray(obj)) {
      throw new Error(`Cannot merge non-object value: ${typeof obj}`);
    }

    for (const [key, value] of Object.entries(obj)) {
      if (key in result) {
        switch (conflictResolution) {
          case 'first':
            break;
          case 'last':
            result[key] = value;
            break;
          case 'merge':
            if (typeof result[key] === 'object' && typeof value === 'object') {
              result[key] = { ...result[key], ...value };
            } else {
              result[key] = value;
            }
            break;
          case 'error':
            throw new Error(`Key conflict detected: ${key}`);
          default:
            result[key] = value;
        }
      } else {
        result[key] = value;
      }
    }
  }

  return result;
}

/**
 * Custom aggregation function
 * @param {Array<any>} outputs - Array of task instance outputs
 * @param {Function} fn - Custom aggregation function (outputs) => result
 * @returns {any} Result of custom aggregation
 * @throws {Error} If fn is not a function
 * @example
 * custom([1, 2, 3, 4], (vals) => vals.filter(v => v > 2)) // => [3, 4]
 */
export function custom(outputs, fn) {
  if (!Array.isArray(outputs)) {
    throw new Error('Outputs must be an array');
  }

  if (typeof fn !== 'function') {
    throw new Error('Custom aggregation requires a function');
  }

  return fn(outputs);
}

// =============================================================================
// Unified Aggregation
// =============================================================================

/**
 * Aggregate MI outputs using specified strategy
 * @param {Array<any>} outputs - Array of task instance outputs
 * @param {Object} config - Aggregation configuration
 * @param {string} config.strategy - Aggregation strategy (sum|avg|min|max|concat|merge|custom)
 * @param {string} [config.field] - Optional field path to extract
 * @param {Function} [config.customFn] - Custom aggregation function (required if strategy='custom')
 * @param {string} [config.conflictResolution='last'] - Conflict resolution for merge
 * @returns {any} Aggregated result
 * @throws {Error} If configuration is invalid
 * @example
 * aggregate([{ x: 10 }, { x: 20 }], { strategy: 'sum', field: 'x' }) // => 30
 */
export function aggregate(outputs, config) {
  const validated = AggregationConfigSchema.parse(config);

  switch (validated.strategy) {
    case 'sum':
      return sum(outputs, validated.field);
    case 'avg':
      return avg(outputs, validated.field);
    case 'min':
      return min(outputs, validated.field);
    case 'max':
      return max(outputs, validated.field);
    case 'concat':
      return concat(outputs, validated.field);
    case 'merge':
      return merge(outputs, validated.field, validated.conflictResolution);
    case 'custom':
      if (!validated.customFn) {
        throw new Error('Custom strategy requires customFn');
      }
      return custom(outputs, validated.customFn);
    default:
      throw new Error(`Unknown aggregation strategy: ${validated.strategy}`);
  }
}

// =============================================================================
// Utilities
// =============================================================================

/**
 * Get nested field from object using dot notation
 * @param {Object} obj - Object to traverse
 * @param {string} path - Dot-separated path (e.g., 'result.data.amount')
 * @returns {any} Value at path or undefined
 * @example
 * getNestedField({ a: { b: { c: 42 } } }, 'a.b.c') // => 42
 */
function getNestedField(obj, path) {
  if (!path) return obj;

  const parts = path.split('.');
  let current = obj;

  for (const part of parts) {
    if (current === null || current === undefined) {
      return undefined;
    }
    current = current[part];
  }

  return current;
}

// =============================================================================
// Exports
// =============================================================================

export default {
  sum,
  avg,
  min,
  max,
  concat,
  merge,
  custom,
  aggregate,
};
