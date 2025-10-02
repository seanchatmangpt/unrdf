/**
 * @file OpenTelemetry integration for sidecar client
 * @module sidecar/telemetry
 *
 * @description
 * OpenTelemetry tracing, metrics, and logging for KGC sidecar client.
 */

import { trace, metrics, context } from '@opentelemetry/api';
import { z } from 'zod';

/**
 * Telemetry configuration schema
 */
const TelemetryConfigSchema = z.object({
  serviceName: z.string().default('kgc-sidecar-client'),
  enableTracing: z.boolean().default(true),
  enableMetrics: z.boolean().default(true),
  enableLogging: z.boolean().default(true)
});

/**
 * Telemetry manager for sidecar client
 */
export class SidecarTelemetry {
  /**
   * Create telemetry manager
   * @param {Object} [config] - Telemetry configuration
   */
  constructor(config = {}) {
    this.config = TelemetryConfigSchema.parse(config);
    this.tracer = null;
    this.meter = null;
    this.metrics = new Map();
    this._initialize();
  }

  /**
   * Initialize telemetry
   * @private
   */
  _initialize() {
    if (this.config.enableTracing) {
      this.tracer = trace.getTracer(this.config.serviceName);
    }

    if (this.config.enableMetrics) {
      this.meter = metrics.getMeter(this.config.serviceName);
      this._createMetrics();
    }
  }

  /**
   * Create metrics
   * @private
   */
  _createMetrics() {
    // Request counter
    this.metrics.set('requests', this.meter.createCounter('sidecar.client.requests', {
      description: 'Total number of sidecar requests'
    }));

    // Request duration histogram
    this.metrics.set('duration', this.meter.createHistogram('sidecar.client.duration', {
      description: 'Request duration in milliseconds',
      unit: 'ms'
    }));

    // Error counter
    this.metrics.set('errors', this.meter.createCounter('sidecar.client.errors', {
      description: 'Total number of errors'
    }));

    // Circuit breaker state gauge
    this.metrics.set('circuit_state', this.meter.createObservableGauge('sidecar.client.circuit.state', {
      description: 'Circuit breaker state (0=closed, 1=half_open, 2=open)'
    }));

    // Connection pool size gauge
    this.metrics.set('pool_size', this.meter.createObservableGauge('sidecar.client.pool.size', {
      description: 'Connection pool size'
    }));

    // Health status gauge
    this.metrics.set('health', this.meter.createObservableGauge('sidecar.client.health', {
      description: 'Health status (0=unknown, 1=healthy, 2=degraded, 3=unhealthy)'
    }));
  }

  /**
   * Start span
   * @param {string} name - Span name
   * @param {Object} [attributes] - Span attributes
   * @returns {Object} Span context
   */
  startSpan(name, attributes = {}) {
    if (!this.tracer) {
      return null;
    }

    const span = this.tracer.startSpan(name, {
      attributes: {
        'service.name': this.config.serviceName,
        ...attributes
      }
    });

    return { span, context: trace.setSpan(context.active(), span) };
  }

  /**
   * End span
   * @param {Object} spanContext - Span context
   * @param {Object} [attributes] - Additional attributes
   */
  endSpan(spanContext, attributes = {}) {
    if (!spanContext || !spanContext.span) {
      return;
    }

    if (attributes) {
      spanContext.span.setAttributes(attributes);
    }

    spanContext.span.end();
  }

  /**
   * Add span event
   * @param {Object} spanContext - Span context
   * @param {string} name - Event name
   * @param {Object} [attributes] - Event attributes
   */
  addSpanEvent(spanContext, name, attributes = {}) {
    if (!spanContext || !spanContext.span) {
      return;
    }

    spanContext.span.addEvent(name, attributes);
  }

  /**
   * Record metric
   * @param {string} name - Metric name
   * @param {number} value - Metric value
   * @param {Object} [attributes] - Metric attributes
   */
  recordMetric(name, value, attributes = {}) {
    const metric = this.metrics.get(name);

    if (!metric) {
      return;
    }

    if (metric.add) {
      metric.add(value, attributes);
    } else {
      metric.record(value, attributes);
    }
  }

  /**
   * Wrap function with tracing
   * @param {string} name - Span name
   * @param {Function} fn - Function to wrap
   * @returns {Function} Wrapped function
   */
  traced(name, fn) {
    return async (...args) => {
      const spanContext = this.startSpan(name);
      const startTime = Date.now();

      try {
        const result = await fn(...args);
        const duration = Date.now() - startTime;

        this.endSpan(spanContext, { success: true, duration });
        this.recordMetric('duration', duration, { operation: name });
        this.recordMetric('requests', 1, { operation: name, status: 'success' });

        return result;
      } catch (error) {
        const duration = Date.now() - startTime;

        this.endSpan(spanContext, {
          success: false,
          error: error.message,
          duration
        });

        this.recordMetric('duration', duration, { operation: name });
        this.recordMetric('requests', 1, { operation: name, status: 'error' });
        this.recordMetric('errors', 1, { operation: name, error: error.code });

        throw error;
      }
    };
  }
}

/**
 * Create telemetry manager
 * @param {Object} [config] - Telemetry configuration
 * @returns {SidecarTelemetry} Telemetry manager
 */
export function createSidecarTelemetry(config) {
  return new SidecarTelemetry(config);
}

export default SidecarTelemetry;
