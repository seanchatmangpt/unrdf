/**
 * @fileoverview OTEL Span Validation Utilities
 * @module test/utils/otel-validator
 *
 * @description
 * Helper utilities for validating OpenTelemetry spans, metrics, and traces
 * in test suites. Provides assertion helpers and validation functions.
 */

import { describe, it, expect } from 'vitest';

/**
 * Span status codes from OTEL spec
 */
export const SpanStatusCode = {
  UNSET: 0,
  OK: 1,
  ERROR: 2
};

/**
 * Validate that a span has required attributes
 * @param {Object} span - OTEL span object
 * @param {Array<string>} requiredAttributes - Array of required attribute names
 * @returns {Object} Validation result
 */
export function validateSpanAttributes(span, requiredAttributes) {
  const result = {
    valid: true,
    missing: [],
    present: []
  };

  for (const attr of requiredAttributes) {
    if (span.attributes && span.attributes[attr] !== undefined) {
      result.present.push(attr);
    } else {
      result.valid = false;
      result.missing.push(attr);
    }
  }

  return result;
}

/**
 * Validate span status
 * @param {Object} span - OTEL span object
 * @param {number} expectedStatus - Expected status code
 * @returns {boolean} True if status matches
 */
export function validateSpanStatus(span, expectedStatus) {
  if (!span.status) return false;
  return span.status.code === expectedStatus;
}

/**
 * Validate span duration is within threshold
 * @param {Object} span - OTEL span object
 * @param {number} maxDuration - Maximum duration in milliseconds
 * @returns {Object} Validation result
 */
export function validateSpanDuration(span, maxDuration) {
  const duration = span.attributes?.['kgc.transaction.duration_ms'] ||
                   span.attributes?.['duration_ms'] || 0;

  return {
    valid: duration <= maxDuration,
    duration,
    threshold: maxDuration
  };
}

/**
 * Assert span exists with name
 * @param {Array} spans - Array of spans
 * @param {string} spanName - Expected span name
 */
export function assertSpanExists(spans, spanName) {
  const span = spans.find(s => s.name === spanName);
  expect(span, `Span '${spanName}' should exist`).toBeDefined();
  return span;
}

/**
 * Assert span has attribute with value
 * @param {Object} span - OTEL span object
 * @param {string} attrName - Attribute name
 * @param {*} expectedValue - Expected value (optional)
 */
export function assertSpanAttribute(span, attrName, expectedValue = undefined) {
  expect(span.attributes, `Span should have attributes`).toBeDefined();
  expect(span.attributes[attrName], `Span should have attribute '${attrName}'`).toBeDefined();

  if (expectedValue !== undefined) {
    expect(span.attributes[attrName], `Attribute '${attrName}' should match expected value`).toBe(expectedValue);
  }
}

/**
 * Assert span completed successfully
 * @param {Object} span - OTEL span object
 */
export function assertSpanSucceeded(span) {
  expect(span.status, 'Span should have status').toBeDefined();
  expect(span.status.code, 'Span should have OK status').toBe(SpanStatusCode.OK);
}

/**
 * Assert span failed with error
 * @param {Object} span - OTEL span object
 */
export function assertSpanFailed(span) {
  expect(span.status, 'Span should have status').toBeDefined();
  expect(span.status.code, 'Span should have ERROR status').toBe(SpanStatusCode.ERROR);
}

/**
 * Assert span performance meets threshold
 * @param {Object} span - OTEL span object
 * @param {number} maxDuration - Maximum duration in ms
 */
export function assertSpanPerformance(span, maxDuration) {
  const result = validateSpanDuration(span, maxDuration);
  expect(result.valid,
    `Span duration ${result.duration}ms should be under ${maxDuration}ms`
  ).toBe(true);
}

/**
 * Create a mock span for testing
 * @param {Object} options - Span options
 * @returns {Object} Mock span object
 */
export function createMockSpan(options = {}) {
  const {
    name = 'test.span',
    attributes = {},
    status = { code: SpanStatusCode.OK },
    startTime = Date.now(),
    endTime = null
  } = options;

  return {
    name,
    attributes: {
      'service.name': 'test-service',
      ...attributes
    },
    status,
    startTime,
    endTime,
    setAttributes: function(attrs) {
      Object.assign(this.attributes, attrs);
    },
    setStatus: function(newStatus) {
      this.status = newStatus;
    },
    recordException: function(error) {
      this.attributes['error.type'] = error.constructor.name;
      this.attributes['error.message'] = error.message;
    },
    end: function() {
      this.endTime = Date.now();
      const duration = this.endTime - this.startTime;
      this.attributes['duration_ms'] = duration;
    }
  };
}

/**
 * Create mock tracer for testing
 * @returns {Object} Mock tracer
 */
export function createMockTracer() {
  const spans = [];

  return {
    spans,
    startSpan: function(name, options = {}) {
      const span = createMockSpan({
        name,
        attributes: options.attributes || {}
      });
      spans.push(span);
      return span;
    },
    getSpans: function() {
      return spans;
    },
    clear: function() {
      spans.length = 0;
    }
  };
}

/**
 * Validate OTEL metrics structure
 * @param {Object} metrics - Metrics object
 * @param {Object} schema - Expected schema
 * @returns {Object} Validation result
 */
export function validateMetricsSchema(metrics, schema) {
  const result = {
    valid: true,
    errors: []
  };

  for (const [key, type] of Object.entries(schema)) {
    if (metrics[key] === undefined) {
      result.valid = false;
      result.errors.push(`Missing metric: ${key}`);
    } else if (typeof metrics[key] !== type) {
      result.valid = false;
      result.errors.push(`Metric '${key}' has wrong type: expected ${type}, got ${typeof metrics[key]}`);
    }
  }

  return result;
}

/**
 * Extract span tree from flat span list
 * @param {Array} spans - Flat list of spans
 * @returns {Object} Hierarchical span tree
 */
export function buildSpanTree(spans) {
  const tree = {
    roots: [],
    byId: new Map()
  };

  // First pass: index all spans
  for (const span of spans) {
    const id = span.attributes?.['kgc.transaction.id'] ||
               span.attributes?.['kgc.hook.id'] ||
               span.name;
    tree.byId.set(id, { span, children: [] });
  }

  // Second pass: build hierarchy
  for (const span of spans) {
    const id = span.attributes?.['kgc.transaction.id'] ||
               span.attributes?.['kgc.hook.id'] ||
               span.name;
    const parentId = span.attributes?.['parent.id'];

    if (parentId && tree.byId.has(parentId)) {
      tree.byId.get(parentId).children.push(tree.byId.get(id));
    } else {
      tree.roots.push(tree.byId.get(id));
    }
  }

  return tree;
}

/**
 * Calculate span statistics
 * @param {Array} spans - Array of spans
 * @returns {Object} Statistics
 */
export function calculateSpanStats(spans) {
  const stats = {
    total: spans.length,
    succeeded: 0,
    failed: 0,
    totalDuration: 0,
    avgDuration: 0,
    minDuration: Infinity,
    maxDuration: 0
  };

  for (const span of spans) {
    // Count successes/failures
    if (span.status?.code === SpanStatusCode.OK) {
      stats.succeeded++;
    } else if (span.status?.code === SpanStatusCode.ERROR) {
      stats.failed++;
    }

    // Calculate durations
    const duration = span.attributes?.['duration_ms'] ||
                     span.attributes?.['kgc.transaction.duration_ms'] || 0;

    stats.totalDuration += duration;
    stats.minDuration = Math.min(stats.minDuration, duration);
    stats.maxDuration = Math.max(stats.maxDuration, duration);
  }

  stats.avgDuration = stats.total > 0 ? stats.totalDuration / stats.total : 0;

  return stats;
}

/**
 * Validate v3.1.0 feature spans
 * @param {Array} spans - Array of spans
 * @returns {Object} Validation result
 */
export function validateV3_1Features(spans) {
  const result = {
    valid: true,
    features: {
      'isolated-vm': false,
      'browser-support': false,
      'policy-pack': false,
      'knowledge-hooks': false
    },
    missing: []
  };

  // Check for isolated-vm spans
  const isolatedVmSpans = spans.filter(s =>
    s.name.includes('isolated-vm') ||
    s.attributes?.['sandbox.engine'] === 'isolated-vm'
  );
  result.features['isolated-vm'] = isolatedVmSpans.length > 0;

  // Check for browser feature spans
  const browserSpans = spans.filter(s =>
    s.name.includes('browser') ||
    s.attributes?.['environment'] === 'browser'
  );
  result.features['browser-support'] = browserSpans.length > 0;

  // Check for policy pack spans
  const policySpans = spans.filter(s =>
    s.name.includes('policy') ||
    s.attributes?.['hook.type'] === 'policy'
  );
  result.features['policy-pack'] = policySpans.length > 0;

  // Check for knowledge hooks spans
  const hookSpans = spans.filter(s =>
    s.name.includes('kgc.hook') ||
    s.attributes?.['kgc.hook.id']
  );
  result.features['knowledge-hooks'] = hookSpans.length > 0;

  // Identify missing features
  for (const [feature, present] of Object.entries(result.features)) {
    if (!present) {
      result.valid = false;
      result.missing.push(feature);
    }
  }

  return result;
}

/**
 * Format span for debugging
 * @param {Object} span - OTEL span
 * @returns {string} Formatted span string
 */
export function formatSpan(span) {
  const status = span.status?.code === SpanStatusCode.OK ? '✓' :
                 span.status?.code === SpanStatusCode.ERROR ? '✗' : '?';
  const duration = span.attributes?.['duration_ms'] ||
                   span.attributes?.['kgc.transaction.duration_ms'] || 'N/A';

  return `[${status}] ${span.name} (${duration}ms)`;
}

/**
 * Print span tree for debugging
 * @param {Object} tree - Span tree from buildSpanTree
 * @param {number} indent - Indentation level
 */
export function printSpanTree(tree, indent = 0) {
  const spaces = ' '.repeat(indent);

  for (const node of tree.roots) {
    console.log(spaces + formatSpan(node.span));
    if (node.children.length > 0) {
      printSpanTree({ roots: node.children }, indent + 2);
    }
  }
}

export default {
  SpanStatusCode,
  validateSpanAttributes,
  validateSpanStatus,
  validateSpanDuration,
  assertSpanExists,
  assertSpanAttribute,
  assertSpanSucceeded,
  assertSpanFailed,
  assertSpanPerformance,
  createMockSpan,
  createMockTracer,
  validateMetricsSchema,
  buildSpanTree,
  calculateSpanStats,
  validateV3_1Features,
  formatSpan,
  printSpanTree
};
