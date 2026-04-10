/**
 * @file Operator Executor
 * @module manufacturing/pipeline/operator-executor
 * @description Execute a single operator with timeout, state management, and error handling
 */

import { OperatorRegistry } from '../operators/registry.mjs';
import { OperatorError } from '../error.mjs';

/**
 *
 */
export class OperatorExecutor {
  /**
   * @param {OperatorRegistry} registry
   * @param {{ defaultTimeoutMs?: number }} opts
   */
  constructor(registry, { defaultTimeoutMs = 5000 } = {}) {
    this.registry = registry;
    this.defaultTimeoutMs = defaultTimeoutMs;
  }

  /**
   * Execute a single operator by name.
   * @param {string} operatorName
   * @param {object} input
   * @param {{ timeoutMs?: number, context?: object }} opts
   * @returns {Promise<object>}
   */
  async execute(operatorName, input, opts = {}) {
    const timeoutMs = opts.timeoutMs ?? this.defaultTimeoutMs;
    const context = opts.context ?? {};

    const operator = this.registry.get(operatorName);

    // Validate input
    const validation = operator.validateInput(input);
    if (!validation.valid) {
      throw new OperatorError(
        operatorName,
        'VALIDATE_FORMAT',
        `Input validation failed: ${validation.errors.join(', ')}`,
        { errors: validation.errors },
      );
    }

    // Execute with timeout
    return Promise.race([
      operator.execute(input, context),
      new Promise((_, reject) =>
        setTimeout(() => reject(new Error(`Operator "${operatorName}" timed out after ${timeoutMs}ms`)), timeoutMs),
      ),
    ]);
  }

  /**
   * Execute an operator with automatic error handling.
   * @param {string} operatorName
   * @param {object} input
   * @param {object} opts
   * @returns {Promise<{ success: boolean, result?: object, error?: OperatorError }>}
   */
  async executeSafe(operatorName, input, opts = {}) {
    try {
      const result = await this.execute(operatorName, input, opts);
      return { success: true, result };
    } catch (err) {
      const error = err instanceof OperatorError
        ? err
        : new OperatorError(operatorName, 'UNKNOWN_ERROR', err.message);
      return { success: false, error };
    }
  }
}
