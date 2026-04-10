/**
 * @file μ₇: Monitor Operator
 * @module manufacturing/operators/monitor-operator
 * @description Emit observability signals without modifying data
 */

import { BaseOperator } from './base.mjs';

/**
 *
 */
export class MonitorOperator extends BaseOperator {
  /**
   *
   */
  constructor() {
    super({
      name: 'monitor',
      description: 'Emit observability signals without modifying data',
      latencyTargetMs: 15,
      retrySafe: true,
      requires: ['data'],
    });
  }

  /**
   * Record metrics about the data without modifying it.
   * @param {{ data: object, labels?: object }} input
   * @param {object} context
   * @returns {Promise<{data: object, metrics: object}>}
   */
  async execute(input, context = {}) {
    const startTime = performance.now();
    const data = input.data;

    // Collect metrics about the data
    const metrics = {
      duration_ms: 0,
      timestamp: new Date().toISOString(),
      labels: input.labels || {},
      data: {
        type: Array.isArray(data) ? 'array' : typeof data,
        size: Array.isArray(data) ? data.length : Object.keys(data).length,
        keys: Array.isArray(data) ? [] : Object.keys(data),
      },
    };

    // Check for any nested metrics in context
    if (context.pipelineId) metrics.pipelineId = context.pipelineId;
    if (context.stage) metrics.stage = context.stage;

    metrics.duration_ms = performance.now() - startTime;
    return { data, metrics };
  }
}
