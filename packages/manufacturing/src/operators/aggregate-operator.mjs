/**
 * @file μ₅: Aggregate Operator
 * @module manufacturing/operators/aggregate-operator
 * @description Combine multiple items into aggregated results
 */

import { BaseOperator } from './base.mjs';

/**
 *
 */
export class AggregateOperator extends BaseOperator {
  /**
   *
   */
  constructor() {
    super({
      name: 'aggregate',
      description: 'Combine multiple items into aggregated results',
      latencyTargetMs: 20,
      retrySafe: false,
      requires: ['data'],
    });
  }

  /**
   * Aggregate items by group key and aggregation function.
   * @param {{ data: Array, groupBy?: string, aggField?: string, aggFn?: string }} input
   * @param {object} context
   * @returns {Promise<{data: Array, groupCount: number}>}
   */
  async execute(input, context = {}) {
    const startTime = performance.now();
    const items = input.data;
    const groupBy = input.groupBy;
    const aggField = input.aggField;
    const aggFn = input.aggFn || 'count';

    if (!groupBy) {
      const duration = performance.now() - startTime;
      return {
        data: [{ _all: items.length, _count: items.length }],
        groupCount: 1,
        metrics: { duration_ms: duration },
      };
    }

    // Group items by groupBy field
    const groups = new Map();
    for (const item of items) {
      const key = String(item[groupBy] ?? 'undefined');
      if (!groups.has(key)) groups.set(key, []);
      groups.get(key).push(item);
    }

    // Apply aggregation function
    const result = [];
    for (const [key, group] of groups) {
      const entry = { [groupBy]: key };

      switch (aggFn) {
        case 'count':
          entry._count = group.length;
          break;
        case 'sum':
          entry._sum = group.reduce((sum, item) => sum + (Number(item[aggField]) || 0), 0);
          break;
        case 'avg':
          entry._avg = group.reduce((sum, item) => sum + (Number(item[aggField]) || 0), 0) / (group.length || 1);
          break;
        case 'min':
          entry._min = Math.min(...group.map(item => Number(item[aggField]) ?? Infinity));
          break;
        case 'max':
          entry._max = Math.max(...group.map(item => Number(item[aggField]) ?? -Infinity));
          break;
        case 'first':
          entry._first = group[0];
          break;
        case 'concat':
          entry._concat = group.map(item => item[aggField] ?? '').join(', ');
          break;
        default:
          entry._count = group.length;
      }

      result.push(entry);
    }

    const duration = performance.now() - startTime;
    return {
      data: result,
      groupCount: groups.size,
      metrics: { duration_ms: duration, totalItems: items.length },
    };
  }
}
