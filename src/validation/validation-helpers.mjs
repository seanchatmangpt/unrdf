/**
 * @file Validation Helper Utilities
 * @module validation/validation-helpers
 *
 * @description
 * Helper utilities for OTEL span-based validation, replacing traditional
 * test assertions with span analysis and metric validation.
 */

import { z } from 'zod';
import { trace, metrics } from '@opentelemetry/api';

// Validation schemas
const _SpanValidationSchema = z.object({
  name: z.string(),
  status: z.enum(['ok', 'error']),
  duration: z.number().min(0),
  attributes: z.record(z.any()),
  events: z
    .array(
      z.object({
        name: z.string(),
        timestamp: z.number(),
        attributes: z.record(z.any()).optional(),
      })
    )
    .optional(),
});

const _MetricValidationSchema = z.object({
  name: z.string(),
  value: z.number(),
  unit: z.string().optional(),
  attributes: z.record(z.any()).optional(),
  timestamp: z.number(),
});

/**
 * Validation helper utilities for OTEL span-based testing
 */
export class ValidationHelpers {
  /**
   * Create validation helpers
   * @param {Object} [config] - Configuration
   */
  constructor(config = {}) {
    this.config = {
      defaultTimeout: 30000,
      defaultThresholds: {
        maxLatency: 1000,
        maxErrorRate: 0.01,
        minThroughput: 1,
        maxMemoryUsage: 100 * 1024 * 1024, // 100MB
      },
      ...config,
    };

    this.tracer = trace.getTracer('validation-helpers');
    this.meter = metrics.getMeter('validation-helpers');
  }

  /**
   * Assert that a span exists with expected attributes
   * @param {Array} spans - Collected spans
   * @param {string} spanName - Expected span name
   * @param {Object} [expectedAttributes] - Expected attributes
   * @returns {Object} Assertion result
   */
  assertSpanExists(spans, spanName, expectedAttributes = {}) {
    const span = spans.find(s => s.name === spanName);

    if (!span) {
      return {
        passed: false,
        message: `Expected span '${spanName}' not found`,
        actual: spans.map(s => s.name),
        expected: spanName,
      };
    }

    // Check attributes
    const missingAttributes = [];
    const mismatchedAttributes = [];

    for (const [key, expectedValue] of Object.entries(expectedAttributes)) {
      if (!(key in span.attributes)) {
        missingAttributes.push(key);
      } else if (span.attributes[key] !== expectedValue) {
        mismatchedAttributes.push({
          key,
          expected: expectedValue,
          actual: span.attributes[key],
        });
      }
    }

    if (missingAttributes.length > 0 || mismatchedAttributes.length > 0) {
      return {
        passed: false,
        message: `Span '${spanName}' has attribute mismatches`,
        missingAttributes,
        mismatchedAttributes,
      };
    }

    return {
      passed: true,
      message: `Span '${spanName}' exists with correct attributes`,
      span,
    };
  }

  /**
   * Assert that a span has a specific status
   * @param {Array} spans - Collected spans
   * @param {string} spanName - Span name
   * @param {string} expectedStatus - Expected status ('ok' or 'error')
   * @returns {Object} Assertion result
   */
  assertSpanStatus(spans, spanName, expectedStatus) {
    const span = spans.find(s => s.name === spanName);

    if (!span) {
      return {
        passed: false,
        message: `Span '${spanName}' not found`,
        expected: expectedStatus,
      };
    }

    if (span.status !== expectedStatus) {
      return {
        passed: false,
        message: `Span '${spanName}' has status '${span.status}', expected '${expectedStatus}'`,
        actual: span.status,
        expected: expectedStatus,
      };
    }

    return {
      passed: true,
      message: `Span '${spanName}' has correct status '${expectedStatus}'`,
      span,
    };
  }

  /**
   * Assert that a span duration is within threshold
   * @param {Array} spans - Collected spans
   * @param {string} spanName - Span name
   * @param {number} maxDuration - Maximum allowed duration in ms
   * @returns {Object} Assertion result
   */
  assertSpanDuration(spans, spanName, maxDuration) {
    const span = spans.find(s => s.name === spanName);

    if (!span) {
      return {
        passed: false,
        message: `Span '${spanName}' not found`,
        expected: `duration < ${maxDuration}ms`,
      };
    }

    if (span.duration > maxDuration) {
      return {
        passed: false,
        message: `Span '${spanName}' duration ${span.duration}ms exceeds threshold ${maxDuration}ms`,
        actual: span.duration,
        expected: `duration < ${maxDuration}ms`,
      };
    }

    return {
      passed: true,
      message: `Span '${spanName}' duration ${span.duration}ms is within threshold`,
      span,
    };
  }

  /**
   * Assert that no error spans exist
   * @param {Array} spans - Collected spans
   * @returns {Object} Assertion result
   */
  assertNoErrorSpans(spans) {
    const errorSpans = spans.filter(s => s.status === 'error');

    if (errorSpans.length > 0) {
      return {
        passed: false,
        message: `Found ${errorSpans.length} error spans`,
        errorSpans: errorSpans.map(s => ({
          name: s.name,
          status: s.status,
          attributes: s.attributes,
        })),
      };
    }

    return {
      passed: true,
      message: 'No error spans found',
      totalSpans: spans.length,
    };
  }

  /**
   * Assert that a metric value is within threshold
   * @param {Object} metrics - Collected metrics
   * @param {string} metricName - Metric name
   * @param {number} expectedValue - Expected value
   * @param {number} [tolerance] - Tolerance percentage (default: 10%)
   * @returns {Object} Assertion result
   */
  assertMetricValue(metrics, metricName, expectedValue, tolerance = 0.1) {
    const actualValue = metrics[metricName];

    if (actualValue === undefined) {
      return {
        passed: false,
        message: `Metric '${metricName}' not found`,
        expected: expectedValue,
      };
    }

    const toleranceValue = expectedValue * tolerance;
    const minValue = expectedValue - toleranceValue;
    const maxValue = expectedValue + toleranceValue;

    if (actualValue < minValue || actualValue > maxValue) {
      return {
        passed: false,
        message: `Metric '${metricName}' value ${actualValue} outside tolerance range [${minValue}, ${maxValue}]`,
        actual: actualValue,
        expected: expectedValue,
        tolerance: tolerance,
      };
    }

    return {
      passed: true,
      message: `Metric '${metricName}' value ${actualValue} is within tolerance`,
      actual: actualValue,
      expected: expectedValue,
    };
  }

  /**
   * Assert that error rate is below threshold
   * @param {Object} metrics - Collected metrics
   * @param {number} maxErrorRate - Maximum allowed error rate (0-1)
   * @returns {Object} Assertion result
   */
  assertErrorRate(metrics, maxErrorRate) {
    const errorRate = metrics.errorRate || 0;

    if (errorRate > maxErrorRate) {
      return {
        passed: false,
        message: `Error rate ${errorRate} exceeds threshold ${maxErrorRate}`,
        actual: errorRate,
        expected: `error rate < ${maxErrorRate}`,
      };
    }

    return {
      passed: true,
      message: `Error rate ${errorRate} is within threshold`,
      actual: errorRate,
      expected: `error rate < ${maxErrorRate}`,
    };
  }

  /**
   * Assert that latency is below threshold
   * @param {Object} metrics - Collected metrics
   * @param {number} maxLatency - Maximum allowed latency in ms
   * @returns {Object} Assertion result
   */
  assertLatency(metrics, maxLatency) {
    const latency = metrics.latency || 0;

    if (latency > maxLatency) {
      return {
        passed: false,
        message: `Latency ${latency}ms exceeds threshold ${maxLatency}ms`,
        actual: latency,
        expected: `latency < ${maxLatency}ms`,
      };
    }

    return {
      passed: true,
      message: `Latency ${latency}ms is within threshold`,
      actual: latency,
      expected: `latency < ${maxLatency}ms`,
    };
  }

  /**
   * Assert that throughput is above threshold
   * @param {Object} metrics - Collected metrics
   * @param {number} minThroughput - Minimum required throughput
   * @returns {Object} Assertion result
   */
  assertThroughput(metrics, minThroughput) {
    const throughput = metrics.throughput || 0;

    if (throughput < minThroughput) {
      return {
        passed: false,
        message: `Throughput ${throughput} below threshold ${minThroughput}`,
        actual: throughput,
        expected: `throughput >= ${minThroughput}`,
      };
    }

    return {
      passed: true,
      message: `Throughput ${throughput} meets threshold`,
      actual: throughput,
      expected: `throughput >= ${minThroughput}`,
    };
  }

  /**
   * Assert that memory usage is below threshold
   * @param {Object} metrics - Collected metrics
   * @param {number} maxMemoryUsage - Maximum allowed memory usage in bytes
   * @returns {Object} Assertion result
   */
  assertMemoryUsage(metrics, maxMemoryUsage) {
    const memoryUsage = metrics.memoryUsage || 0;

    if (memoryUsage > maxMemoryUsage) {
      return {
        passed: false,
        message: `Memory usage ${memoryUsage} bytes exceeds threshold ${maxMemoryUsage} bytes`,
        actual: memoryUsage,
        expected: `memory usage < ${maxMemoryUsage} bytes`,
      };
    }

    return {
      passed: true,
      message: `Memory usage ${memoryUsage} bytes is within threshold`,
      actual: memoryUsage,
      expected: `memory usage < ${maxMemoryUsage} bytes`,
    };
  }

  /**
   * Assert that a specific event exists in a span
   * @param {Array} spans - Collected spans
   * @param {string} spanName - Span name
   * @param {string} eventName - Event name
   * @param {Object} [expectedAttributes] - Expected event attributes
   * @returns {Object} Assertion result
   */
  assertSpanEvent(spans, spanName, eventName, expectedAttributes = {}) {
    const span = spans.find(s => s.name === spanName);

    if (!span) {
      return {
        passed: false,
        message: `Span '${spanName}' not found`,
        expected: `event '${eventName}'`,
      };
    }

    if (!span.events || span.events.length === 0) {
      return {
        passed: false,
        message: `Span '${spanName}' has no events`,
        expected: `event '${eventName}'`,
      };
    }

    const event = span.events.find(e => e.name === eventName);

    if (!event) {
      return {
        passed: false,
        message: `Event '${eventName}' not found in span '${spanName}'`,
        actual: span.events.map(e => e.name),
        expected: eventName,
      };
    }

    // Check event attributes
    const missingAttributes = [];
    const mismatchedAttributes = [];

    for (const [key, expectedValue] of Object.entries(expectedAttributes)) {
      if (!(key in event.attributes)) {
        missingAttributes.push(key);
      } else if (event.attributes[key] !== expectedValue) {
        mismatchedAttributes.push({
          key,
          expected: expectedValue,
          actual: event.attributes[key],
        });
      }
    }

    if (missingAttributes.length > 0 || mismatchedAttributes.length > 0) {
      return {
        passed: false,
        message: `Event '${eventName}' in span '${spanName}' has attribute mismatches`,
        missingAttributes,
        mismatchedAttributes,
      };
    }

    return {
      passed: true,
      message: `Event '${eventName}' exists in span '${spanName}' with correct attributes`,
      event,
    };
  }

  /**
   * Assert that a span has a specific attribute value
   * @param {Array} spans - Collected spans
   * @param {string} spanName - Span name
   * @param {string} attributeName - Attribute name
   * @param {any} expectedValue - Expected attribute value
   * @returns {Object} Assertion result
   */
  assertSpanAttribute(spans, spanName, attributeName, expectedValue) {
    const span = spans.find(s => s.name === spanName);

    if (!span) {
      return {
        passed: false,
        message: `Span '${spanName}' not found`,
        expected: `${attributeName} = ${expectedValue}`,
      };
    }

    if (!(attributeName in span.attributes)) {
      return {
        passed: false,
        message: `Attribute '${attributeName}' not found in span '${spanName}'`,
        actual: Object.keys(span.attributes),
        expected: attributeName,
      };
    }

    if (span.attributes[attributeName] !== expectedValue) {
      return {
        passed: false,
        message: `Attribute '${attributeName}' in span '${spanName}' has value '${span.attributes[attributeName]}', expected '${expectedValue}'`,
        actual: span.attributes[attributeName],
        expected: expectedValue,
      };
    }

    return {
      passed: true,
      message: `Attribute '${attributeName}' in span '${spanName}' has correct value`,
      actual: span.attributes[attributeName],
      expected: expectedValue,
    };
  }

  /**
   * Create a validation rule function
   * @param {string} name - Rule name
   * @param {Function} condition - Rule condition function
   * @param {string} severity - Rule severity ('error', 'warning', 'info')
   * @returns {Object} Validation rule
   */
  createValidationRule(name, condition, severity = 'error') {
    return {
      name,
      condition,
      severity,
    };
  }

  /**
   * Create a performance threshold rule
   * @param {string} metricName - Metric name
   * @param {number} threshold - Threshold value
   * @param {string} operator - Comparison operator ('<', '>', '<=', '>=', '==', '!=')
   * @returns {Object} Validation rule
   */
  createPerformanceRule(metricName, threshold, operator = '<') {
    return this.createValidationRule(
      `performance.${metricName}`,
      (spans, metrics) => {
        const value = metrics[metricName];
        if (value === undefined) return false;

        switch (operator) {
          case '<':
            return value < threshold;
          case '>':
            return value > threshold;
          case '<=':
            return value <= threshold;
          case '>=':
            return value >= threshold;
          case '==':
            return value === threshold;
          case '!=':
            return value !== threshold;
          default:
            return false;
        }
      },
      'error'
    );
  }

  /**
   * Create a span existence rule
   * @param {string} spanName - Expected span name
   * @param {Object} [expectedAttributes] - Expected attributes
   * @returns {Object} Validation rule
   */
  createSpanExistenceRule(spanName, expectedAttributes = {}) {
    return this.createValidationRule(
      `span.exists.${spanName}`,
      (spans, _metrics) => {
        const span = spans.find(s => s.name === spanName);
        if (!span) return false;

        // Check attributes
        for (const [key, expectedValue] of Object.entries(expectedAttributes)) {
          if (span.attributes[key] !== expectedValue) {
            return false;
          }
        }

        return true;
      },
      'error'
    );
  }

  /**
   * Create a span status rule
   * @param {string} spanName - Span name
   * @param {string} expectedStatus - Expected status
   * @returns {Object} Validation rule
   */
  createSpanStatusRule(spanName, expectedStatus) {
    return this.createValidationRule(
      `span.status.${spanName}`,
      (spans, _metrics) => {
        const span = spans.find(s => s.name === spanName);
        return span && span.status === expectedStatus;
      },
      'error'
    );
  }
}

/**
 * Create validation helpers instance
 * @param {Object} [config] - Configuration
 * @returns {ValidationHelpers} Validation helpers instance
 */
export function createValidationHelpers(config = {}) {
  return new ValidationHelpers(config);
}

/**
 * Default validation helpers instance
 */
export const defaultValidationHelpers = createValidationHelpers();
