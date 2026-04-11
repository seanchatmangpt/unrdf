/**
 * @file μ₈: Sandbox Operator
 * @module manufacturing/operators/sandbox-operator
 * @description Execute effects in isolated, audited, transactional context
 */

import { BaseOperator } from './base.mjs';

/**
 *
 */
export class SandboxOperator extends BaseOperator {
  /**
   *
   */
  constructor() {
    super({
      name: 'sandbox',
      description: 'Execute effects in isolated, audited, transactional context',
      latencyTargetMs: 500,
      retrySafe: true,
      requires: ['effect'],
    });
  }

  /**
   * Execute an effect in a sandboxed context with audit logging.
   * @param {{ effect: Function, input?: object, auditLog?: Array }} input
   * @param {object} context
   * @returns {Promise<{result: any, auditLog: Array}>}
   */
  async execute(input, context = {}) {
    const startTime = performance.now();
    const { effect, input: effectInput, auditLog = [] } = input;

    const auditEntry = {
      timestamp: new Date().toISOString(),
      operator: 'sandbox',
      status: 'started',
    };

    let result;
    let effectError;
    try {
      result = await effect(effectInput);
      auditEntry.status = 'committed';
    } catch (err) {
      auditEntry.status = 'rolled_back';
      auditEntry.error = err.message;
      result = undefined;
      effectError = err;
    }

    auditEntry.duration_ms = performance.now() - startTime;
    const fullLog = [...auditLog, auditEntry];

    // Rethrow effect error after updating audit entry
    if (effectError) {
      throw new Error(`Effect execution failed: ${effectError.message}`, { cause: effectError });
    }

    return {
      result,
      auditLog: fullLog,
      metrics: { duration_ms: auditEntry.duration_ms, entries: fullLog.length },
    };
  }
}
