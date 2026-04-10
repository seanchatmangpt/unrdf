/**
 * @file μ₁: Validate Operator
 * @module manufacturing/operators/validate-operator
 * @description Schema coherence and structural validation of input data
 */

import { BaseOperator } from './base.mjs';

/**
 *
 */
export class ValidateOperator extends BaseOperator {
  /**
   *
   */
  constructor() {
    super({
      name: 'validate',
      description: 'Schema coherence and structural validation',
      latencyTargetMs: 15,
      retrySafe: false,
      requires: ['data'],
    });
  }

  /**
   * Validate data against schema constraints.
   * @param {{ data: object, schema?: object, rules?: Array }} input
   * @param {object} context
   * @returns {Promise<{valid: boolean, errors: string[], data: object}>}
   */
  async execute(input, context = {}) {
    const startTime = performance.now();
    const { data, schema, rules = [] } = input;
    const errors = [];

    // Schema-based validation (if schema provided)
    if (schema) {
      for (const [field, constraint] of Object.entries(schema)) {
        const value = data?.[field];
        if (constraint.required && (value === undefined || value === null)) {
          errors.push(`Required field "${field}" is missing`);
          continue;
        }
        if (constraint.type && value !== undefined) {
          const actualType = Array.isArray(value) ? 'array' : typeof value;
          if (actualType !== constraint.type) {
            errors.push(`Field "${field}" expected ${constraint.type}, got ${actualType}`);
          }
        }
        if (constraint.pattern && value !== undefined) {
          const regex = new RegExp(constraint.pattern);
          if (!regex.test(String(value))) {
            errors.push(`Field "${field}" does not match pattern ${constraint.pattern}`);
          }
        }
        if (constraint.min !== undefined && value < constraint.min) {
          errors.push(`Field "${field}" value ${value} below minimum ${constraint.min}`);
        }
        if (constraint.max !== undefined && value > constraint.max) {
          errors.push(`Field "${field}" value ${value} above maximum ${constraint.max}`);
        }
      }
    }

    // Rule-based validation
    for (const rule of rules) {
      if (!rule.check(data)) {
        errors.push(rule.message);
      }
    }

    const duration = performance.now() - startTime;
    return {
      valid: errors.length === 0,
      errors,
      data,
      metrics: { duration_ms: duration, ruleCount: rules.length },
    };
  }
}
