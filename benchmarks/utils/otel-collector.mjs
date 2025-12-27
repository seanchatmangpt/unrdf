/**
 * OTEL Span Collection and Validation for Benchmarks
 *
 * Lightweight OTEL-compatible span collector without external dependencies.
 * Collects timing and metadata without injecting OTEL into core logic.
 */

/**
 * @typedef {Object} SpanMetadata
 * @property {number} duration - Span duration in milliseconds
 * @property {string} status - Span status ('ok' | 'error')
 * @property {Record<string, any>} attributes - Span attributes
 */

export class OtelCollector {
  /**
   * @param {string} benchmarkName - Name of the benchmark
   */
  constructor(benchmarkName) {
    this.benchmarkName = benchmarkName;
    this.spans = [];
    this.rootSpan = null;
  }

  /**
   * Start a new span for an operation
   * @param {string} operationName - Name of the operation
   * @param {Record<string, any>} [attributes={}] - Initial attributes
   * @returns {Object} Span object with startTime
   */
  startSpan(operationName, attributes = {}) {
    const spanData = {
      operationName,
      startTime: Date.now(),
      startTimeHr: process.hrtime.bigint(),
      attributes: {
        'benchmark.name': this.benchmarkName,
        ...attributes
      },
      status: 'unset'
    };

    this.spans.push(spanData);

    if (!this.rootSpan && operationName === this.benchmarkName) {
      this.rootSpan = spanData;
    }

    return spanData;
  }

  /**
   * End a span with success/failure status
   * @param {Object} spanData - Span data from startSpan()
   * @param {boolean} success - Whether operation succeeded
   * @param {Record<string, any>} [metadata={}] - Additional metadata
   */
  endSpan(spanData, success, metadata = {}) {
    const endTimeHr = process.hrtime.bigint();
    const durationNs = Number(endTimeHr - spanData.startTimeHr);
    const duration = durationNs / 1_000_000; // Convert to milliseconds

    spanData.endTime = Date.now();
    spanData.duration = duration;
    spanData.durationNs = durationNs;
    spanData.success = success;
    spanData.status = success ? 'ok' : 'error';
    spanData.metadata = metadata;

    // Merge additional attributes
    Object.assign(spanData.attributes, {
      'duration.ms': duration,
      'duration.ns': durationNs,
      'span.status': spanData.status,
      ...metadata
    });
  }

  /**
   * Get the root benchmark span
   * @returns {Object|null} Root span data
   */
  getRootSpan() {
    return this.rootSpan;
  }

  /**
   * Export all collected spans
   * @returns {Array<Object>} Array of span data
   */
  exportSpans() {
    return this.spans.map(s => ({
      operationName: s.operationName,
      duration: s.duration,
      success: s.success,
      attributes: s.attributes,
      metadata: s.metadata
    }));
  }

  /**
   * Validate that all spans have required attributes
   * @returns {{ valid: boolean, errors: Array<string> }}
   */
  validateSpans() {
    const errors = [];

    for (const spanData of this.spans) {
      if (spanData.duration === undefined) {
        errors.push(`Span '${spanData.operationName}' missing duration`);
      }

      if (spanData.success === undefined) {
        errors.push(`Span '${spanData.operationName}' missing success status`);
      }

      if (!spanData.attributes || !spanData.attributes['benchmark.name']) {
        errors.push(`Span '${spanData.operationName}' missing benchmark.name attribute`);
      }
    }

    return {
      valid: errors.length === 0,
      errors
    };
  }

  /**
   * Get summary statistics from all spans
   * @returns {Object} Summary stats
   */
  getSummary() {
    const durations = this.spans
      .filter(s => s.duration !== undefined)
      .map(s => s.duration);

    const successCount = this.spans.filter(s => s.success).length;
    const failureCount = this.spans.filter(s => !s.success).length;

    return {
      totalSpans: this.spans.length,
      successCount,
      failureCount,
      successRate: this.spans.length > 0 ? successCount / this.spans.length : 0,
      totalDuration: durations.reduce((a, b) => a + b, 0),
      avgDuration: durations.length > 0 ? durations.reduce((a, b) => a + b, 0) / durations.length : 0
    };
  }
}
