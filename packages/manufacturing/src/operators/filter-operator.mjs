/**
 * @file μ₄: Filter Operator
 * @module manufacturing/operators/filter-operator
 * @description Conditional exclusion of items based on predicates
 */

import { BaseOperator } from './base.mjs';

/**
 *
 */
export class FilterOperator extends BaseOperator {
  /**
   *
   */
  constructor() {
    super({
      name: 'filter',
      description: 'Conditional exclusion based on predicates',
      latencyTargetMs: 5,
      retrySafe: false,
      requires: ['data'],
    });
  }

  /**
   * Filter items in an array based on predicate.
   * @param {{ data: Array, predicate?: Function }} input
   * @param {object} context
   * @returns {Promise<{data: Array, passed: number, skipped: number}>}
   */
  async execute(input, context = {}) {
    const startTime = performance.now();
    const items = input.data;
    const predicate = input.predicate || (() => true);

    const passed = [];
    const skipped = [];

    for (const item of items) {
      try {
        if (predicate(item)) {
          passed.push(item);
        } else {
          skipped.push(item);
        }
      } catch (err) {
        // Predicate evaluation error: skip the item
        skipped.push(item);
      }
    }

    const duration = performance.now() - startTime;
    return {
      data: passed,
      passed: passed.length,
      skipped: skipped.length,
      metrics: { duration_ms: duration },
    };
  }
}
