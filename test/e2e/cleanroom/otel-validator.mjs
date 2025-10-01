/**
 * @file OTEL Trace Validator
 * @module otel-validator
 *
 * @description
 * Comprehensive OTEL trace validation with performance SLA checks.
 */

import { PerformanceMetrics } from './scenario-framework.mjs';

/**
 * Performance SLA definitions
 */
export const performanceTargets = {
  'cli.startup': { p99: 100000 }, // 100ms in microseconds
  'cli.graph.create': { p99: 50000 }, // 50ms
  'cli.graph.validate': { p99: 100000 }, // 100ms
  'cli.store.import': { p99: 200000 }, // 200ms
  'cli.store.query': { p99: 100000 }, // 100ms
  'cli.hook.create': { p99: 50000 }, // 50ms
  'cli.hook.eval': { p99: 100000 }, // 100ms
  'cli.policy.apply': { p99: 100000 }, // 100ms

  'sidecar.transaction.apply': { p99: 2000 }, // 2ms
  'sidecar.transaction.commit': { p99: 5000 }, // 5ms
  'sidecar.hook.evaluate': { p99: 2000 }, // 2ms
  'sidecar.hook.register': { p99: 1000 }, // 1ms
  'sidecar.condition.evaluate': { p99: 1000 }, // 1ms
  'sidecar.query.execute': { p99: 50000 }, // 50ms
  'sidecar.store.add': { p99: 2000 }, // 2ms
  'sidecar.store.serialize': { p99: 10000 }, // 10ms
  'sidecar.storage.loadReceipts': { p99: 20000 }, // 20ms
  'sidecar.policy.activate': { p99: 5000 }, // 5ms
};

/**
 * OTEL validator for trace validation
 */
export class OTELValidator {
  constructor(jaegerClient) {
    this.jaegerClient = jaegerClient;
  }

  /**
   * Validate trace exists
   */
  async validateTraceExists(traceId) {
    const traces = await this.jaegerClient.getTracesByTraceId(traceId);

    if (!traces || traces.length === 0) {
      throw new Error(`No traces found for trace ID: ${traceId}`);
    }

    return traces;
  }

  /**
   * Validate span exists in trace
   */
  async validateSpanExists(traceId, operationName) {
    const traces = await this.jaegerClient.getTracesByTraceId(traceId);

    if (!traces || traces.length === 0) {
      throw new Error(`No traces found for trace ID: ${traceId}`);
    }

    const allSpans = traces.flatMap(trace => trace.spans || []);
    const span = allSpans.find(s =>
      s.operationName === operationName ||
      s.name === operationName
    );

    if (!span) {
      throw new Error(`Span not found: ${operationName}`);
    }

    return span;
  }

  /**
   * Validate all expected spans exist
   */
  async validateAllSpans(traceId, expectedSpans) {
    const traces = await this.jaegerClient.getTracesByTraceId(traceId);

    if (!traces || traces.length === 0) {
      throw new Error(`No traces found for trace ID: ${traceId}`);
    }

    const allSpans = traces.flatMap(trace => trace.spans || []);
    const missingSpans = [];

    for (const expectedSpan of expectedSpans) {
      const span = allSpans.find(s =>
        s.operationName === expectedSpan ||
        s.name === expectedSpan
      );

      if (!span) {
        missingSpans.push(expectedSpan);
      }
    }

    if (missingSpans.length > 0) {
      throw new Error(`Missing expected spans: ${missingSpans.join(', ')}`);
    }

    return allSpans;
  }

  /**
   * Validate span performance against SLA
   */
  async validatePerformance(traceId, operationName, maxDuration) {
    const span = await this.validateSpanExists(traceId, operationName);
    const duration = span.duration || 0;

    if (duration > maxDuration) {
      throw new Error(
        `Span ${operationName} duration ${duration}µs exceeds max ${maxDuration}µs`
      );
    }

    return { duration, maxDuration, passed: true };
  }

  /**
   * Validate all performance targets
   */
  async validateAllPerformance(traces) {
    const allSpans = traces.flatMap(trace => trace.spans || []);
    const violations = [];

    for (const [operation, limits] of Object.entries(performanceTargets)) {
      const spans = allSpans.filter(s =>
        s.operationName === operation ||
        s.name === operation
      );

      if (spans.length === 0) continue;

      const durations = spans.map(s => s.duration || 0);
      const p99 = PerformanceMetrics.calculateP99(durations);

      if (p99 > limits.p99) {
        violations.push({
          operation,
          p99,
          limit: limits.p99,
          violation: p99 - limits.p99,
        });
      }
    }

    if (violations.length > 0) {
      const violationMessages = violations.map(v =>
        `${v.operation}: p99 ${v.p99}µs exceeds ${v.limit}µs by ${v.violation}µs`
      );
      throw new Error(
        `Performance SLA violations:\n${violationMessages.join('\n')}`
      );
    }

    return { passed: true, violations: [] };
  }

  /**
   * Validate trace context propagation
   */
  async validateContextPropagation(traceId, expectedServices) {
    const traces = await this.jaegerClient.getTracesByTraceId(traceId);

    if (!traces || traces.length === 0) {
      throw new Error(`No traces found for trace ID: ${traceId}`);
    }

    const servicesFound = new Set();
    const allSpans = traces.flatMap(trace => trace.spans || []);

    for (const span of allSpans) {
      if (span.process?.serviceName) {
        servicesFound.add(span.process.serviceName);
      }
    }

    const missingServices = expectedServices.filter(
      service => !servicesFound.has(service)
    );

    if (missingServices.length > 0) {
      throw new Error(`Missing expected services: ${missingServices.join(', ')}`);
    }

    return { services: Array.from(servicesFound), passed: true };
  }

  /**
   * Validate span parent-child relationships
   */
  async validateSpanHierarchy(traceId) {
    const traces = await this.jaegerClient.getTracesByTraceId(traceId);

    if (!traces || traces.length === 0) {
      throw new Error(`No traces found for trace ID: ${traceId}`);
    }

    const allSpans = traces.flatMap(trace => trace.spans || []);
    const spanMap = new Map(allSpans.map(s => [s.spanID, s]));

    // Validate all spans have valid parent references
    for (const span of allSpans) {
      const references = span.references || [];

      for (const ref of references) {
        if (ref.refType === 'CHILD_OF' || ref.refType === 'FOLLOWS_FROM') {
          const parentSpan = spanMap.get(ref.spanID);

          if (!parentSpan) {
            throw new Error(
              `Orphaned span: ${span.operationName} references missing parent ${ref.spanID}`
            );
          }
        }
      }
    }

    return { passed: true, spanCount: allSpans.length };
  }

  /**
   * Validate no error spans
   */
  async validateNoErrors(traceId) {
    const traces = await this.jaegerClient.getTracesByTraceId(traceId);

    if (!traces || traces.length === 0) {
      throw new Error(`No traces found for trace ID: ${traceId}`);
    }

    const allSpans = traces.flatMap(trace => trace.spans || []);
    const errorSpans = [];

    for (const span of allSpans) {
      const tags = span.tags || [];
      const hasError = tags.some(tag =>
        tag.key === 'error' && tag.value === true
      );

      if (hasError) {
        errorSpans.push({
          operationName: span.operationName || span.name,
          spanId: span.spanID,
          tags,
        });
      }
    }

    if (errorSpans.length > 0) {
      const errorMessages = errorSpans.map(e =>
        `${e.operationName} (${e.spanId})`
      );
      throw new Error(
        `Found error spans:\n${errorMessages.join('\n')}`
      );
    }

    return { passed: true, errorCount: 0 };
  }

  /**
   * Generate performance report
   */
  async generatePerformanceReport(traces) {
    const allSpans = traces.flatMap(trace => trace.spans || []);
    const report = {
      totalSpans: allSpans.length,
      operations: {},
    };

    // Group spans by operation
    const operationGroups = {};
    for (const span of allSpans) {
      const operation = span.operationName || span.name || 'unknown';
      if (!operationGroups[operation]) {
        operationGroups[operation] = [];
      }
      operationGroups[operation].push(span.duration || 0);
    }

    // Calculate statistics for each operation
    for (const [operation, durations] of Object.entries(operationGroups)) {
      report.operations[operation] = {
        count: durations.length,
        min: Math.min(...durations),
        max: Math.max(...durations),
        avg: PerformanceMetrics.calculateAverage(durations),
        p50: PerformanceMetrics.calculateP50(durations),
        p95: PerformanceMetrics.calculateP95(durations),
        p99: PerformanceMetrics.calculateP99(durations),
        target: performanceTargets[operation]?.p99 || null,
        meetsTarget: performanceTargets[operation]
          ? PerformanceMetrics.calculateP99(durations) <= performanceTargets[operation].p99
          : null,
      };
    }

    return report;
  }
}

export default OTELValidator;
