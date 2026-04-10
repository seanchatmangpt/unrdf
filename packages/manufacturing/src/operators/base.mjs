/**
 * @file Base Operator Class
 * @module manufacturing/operators/base
 * @description Abstract base for all μ(O) operators with OTel instrumentation
 */

import { OPERATOR_CODES, OperatorError } from '../error.mjs';

/**
 *
 */
export class BaseOperator {
  /**
   * @param {object} opts
   * @param {string} opts.name - Operator identifier (e.g., 'validate')
   * @param {string} opts.description - Human-readable purpose
   * @param {number} opts.latencyTargetMs - Expected max latency in ms
   * @param {boolean} opts.retrySafe - Whether failed execution is retryable
   * @param {string[]} opts.requires - Input keys this operator needs
   */
  constructor({ name, description, latencyTargetMs, retrySafe = false, requires = [] }) {
    this.name = name;
    this.description = description;
    this.latencyTargetMs = latencyTargetMs;
    this.retrySafe = retrySafe;
    this.requires = requires;
  }

  /**
   * Execute the operator. Subclasses must implement.
   * @param {object} input - Operator input data
   * @param {object} context - Execution context (tracing, metadata)
   * @returns {Promise<object>} Operator result
   */
  async execute(input, context = {}) {
    throw new Error(`execute() not implemented by ${this.name}`);
  }

  /**
   * Validate input before execution.
   * @param {object} input
   * @returns {{ valid: boolean, errors: string[] }}
   */
  validateInput(input) {
    const errors = [];
    for (const key of this.requires) {
      if (input[key] === undefined) {
        errors.push(`Missing required input: "${key}"`);
      }
    }
    return { valid: errors.length === 0, errors };
  }

  /**
   * Create a standardized operator error.
   * @param {string} code
   * @param {string} message
   * @param {object} context
   * @returns {OperatorError}
   */
  error(code, message, context = {}) {
    return new OperatorError(this.name, code, message, context);
  }

  /**
   * Get operator metadata for registry.
   */
  get metadata() {
    return {
      name: this.name,
      description: this.description,
      latencyTargetMs: this.latencyTargetMs,
      retrySafe: this.retrySafe,
      requires: this.requires,
    };
  }
}
