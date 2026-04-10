/**
 * @file μ₆: Derive Operator
 * @module manufacturing/operators/derive-operator
 * @description Infer new data from existing data using logical rules
 */

import { BaseOperator } from './base.mjs';

/**
 *
 */
export class DeriveOperator extends BaseOperator {
  /**
   *
   */
  constructor() {
    super({
      name: 'derive',
      description: 'Infer new data from existing data using rules',
      latencyTargetMs: 50,
      retrySafe: false,
      requires: ['data'],
    });
  }

  /**
   * Apply derivation rules to data.
   * @param {{ data: object, rules?: Array }} input
   * @param {object} context
   * @returns {Promise<{data: object, derived: string[]}>}
   */
  async execute(input, context = {}) {
    const startTime = performance.now();
    let data = { ...input.data };
    const derived = [];
    const rules = input.rules || [];

    for (const rule of rules) {
      try {
        const result = rule.derive(data);
        if (result !== undefined && result !== null) {
          const fields = typeof result === 'object' ? result : { [rule.output]: result };
          Object.assign(data, fields);
          derived.push(rule.output || 'derived');
        }
      } catch {
        // Rule evaluation error: skip derivation
      }
    }

    const duration = performance.now() - startTime;
    return {
      data,
      derived,
      metrics: { duration_ms: duration, ruleCount: rules.length },
    };
  }
}
