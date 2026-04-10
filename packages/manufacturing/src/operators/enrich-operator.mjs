/**
 * @file μ₃: Enrich Operator
 * @module manufacturing/operators/enrich-operator
 * @description Add contextual information by joining with external data sources
 */

import { BaseOperator } from './base.mjs';

/**
 *
 */
export class EnrichOperator extends BaseOperator {
  /**
   *
   */
  constructor() {
    super({
      name: 'enrich',
      description: 'Add contextual information from external data sources',
      latencyTargetMs: 200,
      retrySafe: true,
      requires: ['data'],
    });
  }

  /**
   * Enrich data with context from a lookup function.
   * @param {{ data: object, enrichments?: Array }} input
   * @param {object} context
   * @returns {Promise<{data: object, enrichments: string[]}>}
   */
  async execute(input, context = {}) {
    const startTime = performance.now();
    let data = { ...input.data };
    const enrichmentNames = [];
    const enrichments = input.enrichments || [];

    for (const enrichment of enrichments) {
      const { field, lookup, fallback } = enrichment;

      try {
        if (lookup) {
          const result = typeof lookup === 'function' ? await lookup(data) : lookup;
          if (result !== undefined && result !== null) {
            data[field] = result;
            enrichmentNames.push(field);
          } else if (fallback !== undefined) {
            data[field] = fallback;
            enrichmentNames.push(`${field} (fallback)`);
          }
        } else if (fallback !== undefined) {
          data[field] = fallback;
          enrichmentNames.push(`${field} (fallback)`);
        }
      } catch (err) {
        if (fallback !== undefined) {
          data[field] = fallback;
          enrichmentNames.push(`${field} (fallback after error)`);
        }
        // else: skip enrichment, keep data unchanged
      }
    }

    const duration = performance.now() - startTime;
    return {
      data,
      enrichments: enrichmentNames,
      metrics: { duration_ms: duration, enrichmentCount: enrichmentNames.length },
    };
  }
}
