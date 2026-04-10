/**
 * @file Operator Registry
 * @module manufacturing/operators/registry
 * @description Dynamic operator registry with metadata lookup
 */

import { OperatorError } from '../error.mjs';

/**
 *
 */
export class OperatorRegistry {
  /**
   *
   */
  constructor() {
    this._operators = new Map();
  }

  /**
   * Register an operator instance.
   * @param {BaseOperator} operator
   */
  register(operator) {
    if (this._operators.has(operator.name)) {
      throw new Error(`Operator "${operator.name}" already registered`);
    }
    this._operators.set(operator.name, operator);
  }

  /**
   * Get operator by name.
   * @param {string} name
   * @returns {BaseOperator}
   */
  get(name) {
    const op = this._operators.get(name);
    if (!op) throw new OperatorError('registry', 'UNKNOWN_ERROR', `Operator "${name}" not found`);
    return op;
  }

  /**
   * Check if operator exists.
   * @param {string} name
   * @returns {boolean}
   */
  has(name) {
    return this._operators.has(name);
  }

  /**
   * List all registered operators.
   * @returns {Array<{name: string, description: string, latencyTargetMs: number, retrySafe: boolean}>}
   */
  list() {
    return [...this._operators.values()].map(op => op.metadata);
  }

  /**
   * Get operator count.
   * @returns {number}
   */
  get size() {
    return this._operators.size;
  }
}
