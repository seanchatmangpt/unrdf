/**
 * @fileoverview Profiling Middleware
 *
 * @description
 * Measures execution time, memory usage, and tracks telemetry spans
 * in OpenTelemetry format. Reports performance metrics and warnings
 * for slow operations.
 *
 * @module cli/middleware/profiling
 * @version 1.0.0
 * @license MIT
 */

/**
 * @typedef {Object} PerformanceMetrics
 * @property {number} executionTimeMs - Total execution time in milliseconds
 * @property {Object} memory - Memory usage statistics
 * @property {number} memory.heapUsed - Heap memory used in bytes
 * @property {number} memory.heapTotal - Total heap size in bytes
 * @property {number} memory.external - External memory in bytes
 * @property {number} memory.rss - Resident set size in bytes
 * @property {number} memory.delta - Memory change during execution
 * @property {Array<Object>} spans - OTEL-format spans
 * @property {Array<string>} warnings - Performance warnings
 */

/**
 * @typedef {Object} OTELSpan
 * @property {string} traceId - Trace identifier
 * @property {string} spanId - Span identifier
 * @property {string} name - Span name
 * @property {string} kind - Span kind (INTERNAL, CLIENT, SERVER)
 * @property {number} startTimeUnixNano - Start time in nanoseconds
 * @property {number} endTimeUnixNano - End time in nanoseconds
 * @property {Array<Object>} attributes - Span attributes
 * @property {Object} status - Span status
 */

/**
 * Performance thresholds for warnings
 */
const THRESHOLDS = {
  executionTimeMs: 5000,     // Warn if command takes > 5s
  memoryDeltaMB: 100,        // Warn if memory grows > 100MB
  heapUsedMB: 500            // Warn if heap exceeds 500MB
};

/**
 * Generate a random hex ID
 * @param {number} length - Length in bytes
 * @returns {string}
 */
function generateId(length) {
  const bytes = new Uint8Array(length);
  if (typeof globalThis.crypto !== 'undefined') {
    globalThis.crypto.getRandomValues(bytes);
  } else {
    for (let i = 0; i < length; i++) {
      bytes[i] = Math.floor(Math.random() * 256);
    }
  }
  return Array.from(bytes).map(b => b.toString(16).padStart(2, '0')).join('');
}

/**
 * Convert hrtime to nanoseconds
 * @param {bigint} hrtime - High-resolution time
 * @returns {number}
 */
function hrtimeToNanos(hrtime) {
  return Number(hrtime);
}

/**
 * Get current memory usage
 * @returns {Object}
 */
function getMemoryUsage() {
  const usage = process.memoryUsage();
  return {
    heapUsed: usage.heapUsed,
    heapTotal: usage.heapTotal,
    external: usage.external,
    rss: usage.rss,
    heapUsedMB: usage.heapUsed / (1024 * 1024),
    rssMB: usage.rss / (1024 * 1024)
  };
}

/**
 * Create an OTEL-format span
 * @param {string} name - Span name
 * @param {string} traceId - Trace ID
 * @param {string} [parentSpanId] - Parent span ID
 * @returns {OTELSpan}
 */
export function createSpan(name, traceId, parentSpanId = null) {
  const span = {
    traceId,
    spanId: generateId(8),
    parentSpanId,
    name,
    kind: 'SPAN_KIND_INTERNAL',
    startTimeUnixNano: hrtimeToNanos(process.hrtime.bigint()),
    endTimeUnixNano: null,
    attributes: [],
    events: [],
    status: { code: 'STATUS_CODE_UNSET' }
  };

  return {
    ...span,
    /**
     * Set span attribute
     * @param {string} key - Attribute key
     * @param {*} value - Attribute value
     */
    setAttribute(key, value) {
      this.attributes.push({
        key,
        value: { stringValue: String(value) }
      });
    },
    /**
     * Add span event
     * @param {string} name - Event name
     * @param {Object} [attributes] - Event attributes
     */
    addEvent(name, attributes = {}) {
      this.events.push({
        name,
        timeUnixNano: hrtimeToNanos(process.hrtime.bigint()),
        attributes: Object.entries(attributes).map(([key, value]) => ({
          key,
          value: { stringValue: String(value) }
        }))
      });
    },
    /**
     * End the span
     * @param {string} [statusCode] - Status code (OK, ERROR)
     */
    end(statusCode = 'STATUS_CODE_OK') {
      this.endTimeUnixNano = hrtimeToNanos(process.hrtime.bigint());
      this.status = { code: statusCode };
    },
    /**
     * Get span duration in milliseconds
     * @returns {number}
     */
    getDurationMs() {
      if (!this.endTimeUnixNano) return null;
      return (this.endTimeUnixNano - this.startTimeUnixNano) / 1e6;
    },
    /**
     * Export span in OTEL format
     * @returns {Object}
     */
    toOTEL() {
      return {
        traceId: this.traceId,
        spanId: this.spanId,
        parentSpanId: this.parentSpanId,
        name: this.name,
        kind: this.kind,
        startTimeUnixNano: String(this.startTimeUnixNano),
        endTimeUnixNano: String(this.endTimeUnixNano),
        attributes: this.attributes,
        events: this.events,
        status: this.status
      };
    }
  };
}

/**
 * Create a profiling context
 * @returns {Object}
 */
export function createProfilingContext() {
  const traceId = generateId(16);
  return {
    traceId,
    spans: [],
    metrics: {
      executionTimeMs: 0,
      memory: null,
      memoryDelta: 0
    },
    warnings: [],
    startMemory: getMemoryUsage(),
    startTime: process.hrtime.bigint()
  };
}

/**
 * Check thresholds and generate warnings
 * @param {Object} metrics - Performance metrics
 * @returns {Array<string>} Warnings
 */
function checkThresholds(metrics) {
  const warnings = [];

  if (metrics.executionTimeMs > THRESHOLDS.executionTimeMs) {
    warnings.push(
      `Slow execution: ${metrics.executionTimeMs.toFixed(2)}ms exceeds threshold of ${THRESHOLDS.executionTimeMs}ms`
    );
  }

  if (metrics.memory?.deltaInMB > THRESHOLDS.memoryDeltaMB) {
    warnings.push(
      `High memory growth: ${metrics.memory.deltaInMB.toFixed(2)}MB exceeds threshold of ${THRESHOLDS.memoryDeltaMB}MB`
    );
  }

  if (metrics.memory?.heapUsedMB > THRESHOLDS.heapUsedMB) {
    warnings.push(
      `High heap usage: ${metrics.memory.heapUsedMB.toFixed(2)}MB exceeds threshold of ${THRESHOLDS.heapUsedMB}MB`
    );
  }

  return warnings;
}

/**
 * Profiling middleware handler
 * @param {Object} context - Middleware context
 * @returns {Promise<Object>} Modified context
 */
export async function profilingMiddleware(context) {
  // Initialize profiling context
  const profiling = createProfilingContext();

  // Create root span for command
  const rootSpan = createSpan(`cli.command.${context.command}`, profiling.traceId);
  rootSpan.setAttribute('cli.command', context.command);
  rootSpan.setAttribute('cli.version', context.meta?.version || '1.0.0');
  rootSpan.setAttribute('cli.args', JSON.stringify(context.args));

  profiling.spans.push(rootSpan);

  // Add profiling utilities to context
  context.profiling = {
    traceId: profiling.traceId,

    /**
     * Create a child span
     * @param {string} name - Span name
     * @returns {Object}
     */
    startSpan(name) {
      const span = createSpan(name, profiling.traceId, rootSpan.spanId);
      profiling.spans.push(span);
      return span;
    },

    /**
     * Record a metric
     * @param {string} name - Metric name
     * @param {number} value - Metric value
     * @param {string} unit - Metric unit
     */
    recordMetric(name, value, unit = '') {
      rootSpan.addEvent('metric', { name, value, unit });
    },

    /**
     * Add checkpoint
     * @param {string} name - Checkpoint name
     */
    checkpoint(name) {
      const elapsed = Number(process.hrtime.bigint() - profiling.startTime) / 1e6;
      rootSpan.addEvent('checkpoint', { name, elapsedMs: elapsed });
    },

    /**
     * Get current metrics
     * @returns {Object}
     */
    getMetrics() {
      const currentMemory = getMemoryUsage();
      return {
        executionTimeMs: Number(process.hrtime.bigint() - profiling.startTime) / 1e6,
        memory: {
          ...currentMemory,
          deltaInMB: (currentMemory.heapUsed - profiling.startMemory.heapUsed) / (1024 * 1024)
        }
      };
    }
  };

  // Add cleanup handler
  context._cleanup = context._cleanup || [];
  context._cleanup.push(() => {
    // End root span
    const hasErrors = context.errors.length > 0;
    rootSpan.end(hasErrors ? 'STATUS_CODE_ERROR' : 'STATUS_CODE_OK');

    // Calculate final metrics
    const endMemory = getMemoryUsage();
    const metrics = {
      executionTimeMs: Number(process.hrtime.bigint() - profiling.startTime) / 1e6,
      memory: {
        start: profiling.startMemory,
        end: endMemory,
        heapUsedMB: endMemory.heapUsedMB,
        deltaInMB: (endMemory.heapUsed - profiling.startMemory.heapUsed) / (1024 * 1024)
      },
      spans: profiling.spans.map(s => s.toOTEL())
    };

    // Check thresholds
    const warnings = checkThresholds(metrics);
    metrics.warnings = warnings;

    // Store metrics in context
    context.performanceMetrics = metrics;

    // Log warnings if verbose
    if (context.verbose && warnings.length > 0) {
      console.log('\nPerformance Warnings:');
      warnings.forEach(w => console.log(`  - ${w}`));
    }

    // Log summary if verbose
    if (context.verbose) {
      console.log('\nPerformance Summary:');
      console.log(`  Execution time: ${metrics.executionTimeMs.toFixed(2)}ms`);
      console.log(`  Memory used: ${metrics.memory.heapUsedMB.toFixed(2)}MB`);
      console.log(`  Memory delta: ${metrics.memory.deltaInMB.toFixed(2)}MB`);
      console.log(`  Spans recorded: ${metrics.spans.length}`);
    }
  });

  return context;
}

/**
 * Export spans in OTEL JSON format
 * @param {Object} context - Middleware context
 * @returns {Object} OTEL export format
 */
export function exportSpans(context) {
  if (!context.performanceMetrics?.spans) {
    return { resourceSpans: [] };
  }

  return {
    resourceSpans: [{
      resource: {
        attributes: [
          { key: 'service.name', value: { stringValue: 'papers-thesis-cli' } },
          { key: 'service.version', value: { stringValue: '1.0.0' } }
        ]
      },
      scopeSpans: [{
        scope: { name: 'cli', version: '1.0.0' },
        spans: context.performanceMetrics.spans
      }]
    }]
  };
}

/**
 * Set custom thresholds
 * @param {Object} newThresholds - New threshold values
 */
export function setThresholds(newThresholds) {
  Object.assign(THRESHOLDS, newThresholds);
}

export default profilingMiddleware;
