/**
 * @file μ₂: Transform Operator
 * @module manufacturing/operators/transform-operator
 * @description Normalize and convert data to canonical representation
 */

import { BaseOperator } from './base.mjs';

/**
 *
 */
export class TransformOperator extends BaseOperator {
  /**
   *
   */
  constructor() {
    super({
      name: 'transform',
      description: 'Normalize and convert data to canonical representation',
      latencyTargetMs: 10,
      retrySafe: false,
      requires: ['data'],
    });
  }

  /**
   * Apply transformations to data.
   * @param {{ data: object, transformations?: Array }} input
   * @param {object} context
   * @returns {Promise<{data: object, applied: string[]}>}
   */
  async execute(input, context = {}) {
    const startTime = performance.now();
    let data = structuredClone(input.data);
    const applied = [];
    const transformations = input.transformations || [];

    for (const transform of transformations) {
      switch (transform.type) {
        case 'rename':
          if (data[transform.from] !== undefined) {
            data[transform.to] = data[transform.from];
            delete data[transform.from];
            applied.push(`rename:${transform.from}→${transform.to}`);
          }
          break;
        case 'convert':
          if (transform.converter && data[transform.field] !== undefined) {
            data[transform.field] = transform.converter(data[transform.field]);
            applied.push(`convert:${transform.field}`);
          }
          break;
        case 'delete':
          if (data[transform.field] !== undefined) {
            delete data[transform.field];
            applied.push(`delete:${transform.field}`);
          }
          break;
        case 'default':
          if (data[transform.field] === undefined) {
            data[transform.field] = transform.value;
            applied.push(`default:${transform.field}`);
          }
          break;
        case 'map':
          if (transform.fields) {
            const mapped = {};
            for (const [newKey, sourceKey] of Object.entries(transform.fields)) {
              mapped[newKey] = data[sourceKey];
              applied.push(`map:${sourceKey}→${newKey}`);
            }
            data = mapped;
          }
          break;
        default:
          if (transform.fn) {
            data = transform.fn(data);
            applied.push('custom');
          }
          break;
      }
    }

    const duration = performance.now() - startTime;
    return { data, applied, metrics: { duration_ms: duration } };
  }
}
