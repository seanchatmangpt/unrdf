/**
 * @file MI Input Data Slicing
 * @module @unrdf/yawl/data/data-slicing
 * @description Distributes input data to multiple task instances using various strategies.
 * Enhances basic slicing with array distribution, object partitioning, cloning, and expression-based slicing.
 */

import { z } from 'zod';
import { evaluateJSONPath } from '../multiple-instance/expression-evaluator.mjs';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Schema for slicing configuration
 */
export const SlicingConfigSchema = z.object({
  strategy: z.enum(['array', 'object', 'clone', 'expression']),
  field: z.string().optional(),
  expression: z.string().optional(),
  preserveOriginal: z.boolean().default(true),
});

// =============================================================================
// Slicing Strategies
// =============================================================================

/**
 * Array distribution - one element per instance
 * @param {Object} inputData - Input data containing array
 * @param {number} instanceCount - Number of instances
 * @param {string} [field='items'] - Field containing array
 * @param {boolean} [preserveOriginal=true] - Keep original data in slices
 * @returns {Array<Object>} Array of data slices
 * @example
 * sliceArray({ items: [1, 2, 3] }, 3) // => [{ items: [...], item: 1, itemIndex: 0 }, ...]
 */
export function sliceArray(inputData, instanceCount, field = 'items', preserveOriginal = true) {
  const slices = [];
  const items = inputData[field];

  if (!Array.isArray(items)) {
    throw new Error(`Field ${field} is not an array`);
  }

  if (items.length !== instanceCount) {
    console.warn(
      `Array length (${items.length}) != instance count (${instanceCount}). ` +
      `Distributing available items.`
    );
  }

  for (let i = 0; i < instanceCount; i++) {
    const slice = preserveOriginal ? { ...inputData } : {};
    slice.item = items[i] ?? null;
    slice.itemIndex = i;
    slice.totalInstances = instanceCount;
    slices.push(slice);
  }

  return slices;
}

/**
 * Object distribution - partition by key
 * @param {Object} inputData - Input data containing object
 * @param {number} instanceCount - Number of instances
 * @param {string} [field='data'] - Field containing object
 * @param {boolean} [preserveOriginal=true] - Keep original data in slices
 * @returns {Array<Object>} Array of data slices
 * @example
 * sliceObject({ data: { a: 1, b: 2, c: 3 } }, 3)
 * // => [{ key: 'a', value: 1, ... }, { key: 'b', value: 2, ... }, ...]
 */
export function sliceObject(inputData, instanceCount, field = 'data', preserveOriginal = true) {
  const slices = [];
  const obj = inputData[field];

  if (typeof obj !== 'object' || obj === null || Array.isArray(obj)) {
    throw new Error(`Field ${field} is not an object`);
  }

  const entries = Object.entries(obj);

  if (entries.length !== instanceCount) {
    console.warn(
      `Object keys (${entries.length}) != instance count (${instanceCount}). ` +
      `Distributing available entries.`
    );
  }

  for (let i = 0; i < instanceCount; i++) {
    const slice = preserveOriginal ? { ...inputData } : {};
    if (entries[i]) {
      const [key, value] = entries[i];
      slice.key = key;
      slice.value = value;
    } else {
      slice.key = null;
      slice.value = null;
    }
    slice.entryIndex = i;
    slice.totalInstances = instanceCount;
    slices.push(slice);
  }

  return slices;
}

/**
 * Clone distribution - same data to all instances
 * @param {Object} inputData - Input data to clone
 * @param {number} instanceCount - Number of instances
 * @returns {Array<Object>} Array of data slices (clones)
 * @example
 * sliceClone({ config: { mode: 'fast' } }, 3)
 * // => [{ config: {...}, instanceIndex: 0 }, { config: {...}, instanceIndex: 1 }, ...]
 */
export function sliceClone(inputData, instanceCount) {
  const slices = [];

  for (let i = 0; i < instanceCount; i++) {
    slices.push({
      ...inputData,
      instanceIndex: i,
      totalInstances: instanceCount,
    });
  }

  return slices;
}

/**
 * Expression-based slicing - JSONPath selects subset per instance
 * @param {Object} inputData - Input data
 * @param {number} instanceCount - Number of instances
 * @param {string} expression - JSONPath expression (e.g., '$.items[0]', '$.data.orders')
 * @param {boolean} [preserveOriginal=true] - Keep original data in slices
 * @returns {Array<Object>} Array of data slices
 * @example
 * sliceExpression({ orders: [1, 2, 3] }, 3, '$.orders')
 * // => [{ orders: [...], item: 1, ... }, ...]
 */
export function sliceExpression(inputData, instanceCount, expression, preserveOriginal = true) {
  const slices = [];

  const result = evaluateJSONPath(expression, inputData);

  let items;
  if (Array.isArray(result)) {
    items = result;
  } else if (typeof result === 'object' && result !== null) {
    items = Object.values(result);
  } else {
    items = [result];
  }

  for (let i = 0; i < instanceCount; i++) {
    const slice = preserveOriginal ? { ...inputData } : {};
    slice.item = items[i] ?? null;
    slice.itemIndex = i;
    slice.totalInstances = instanceCount;
    slices.push(slice);
  }

  return slices;
}

// =============================================================================
// Unified Slicing
// =============================================================================

/**
 * Slice input data using specified strategy
 * @param {Object} inputData - Input data to slice
 * @param {number} instanceCount - Number of instances
 * @param {Object} [config] - Slicing configuration
 * @param {string} [config.strategy='array'] - Slicing strategy (array|object|clone|expression)
 * @param {string} [config.field] - Field to slice (for array/object strategies)
 * @param {string} [config.expression] - JSONPath expression (for expression strategy)
 * @param {boolean} [config.preserveOriginal=true] - Keep original data in slices
 * @returns {Array<Object>} Array of data slices
 * @example
 * sliceInputData({ items: [1, 2, 3] }, 3, { strategy: 'array', field: 'items' })
 */
export function sliceInputData(inputData, instanceCount, config = {}) {
  const defaults = {
    strategy: 'array',
    field: 'items',
    preserveOriginal: true,
  };

  const cfg = { ...defaults, ...config };
  const validated = SlicingConfigSchema.parse(cfg);

  switch (validated.strategy) {
    case 'array':
      if (inputData[validated.field] && Array.isArray(inputData[validated.field])) {
        return sliceArray(inputData, instanceCount, validated.field, validated.preserveOriginal);
      }
      return sliceClone(inputData, instanceCount);

    case 'object':
      if (inputData[validated.field] && typeof inputData[validated.field] === 'object') {
        return sliceObject(inputData, instanceCount, validated.field, validated.preserveOriginal);
      }
      return sliceClone(inputData, instanceCount);

    case 'clone':
      return sliceClone(inputData, instanceCount);

    case 'expression':
      if (!validated.expression) {
        throw new Error('Expression strategy requires expression field');
      }
      return sliceExpression(inputData, instanceCount, validated.expression, validated.preserveOriginal);

    default:
      throw new Error(`Unknown slicing strategy: ${validated.strategy}`);
  }
}

// =============================================================================
// Exports
// =============================================================================

export default {
  sliceArray,
  sliceObject,
  sliceClone,
  sliceExpression,
  sliceInputData,
};
