/**
 * @file OpenTelemetry Tracer for CLI
 * @module cli/utils/otel-tracer
 *
 * @description
 * Initializes OpenTelemetry SDK with Jaeger exporter for CLI operations.
 * Ensures traces are actually exported and visible in Jaeger UI.
 * Provides singleton tracer instance for all CLI commands.
 */

import { trace, context, SpanStatusCode } from '@opentelemetry/api';
import { NodeSDK } from '@opentelemetry/sdk-node';
import { JaegerExporter } from '@opentelemetry/exporter-jaeger';
import { Resource } from '@opentelemetry/resources';
import { SemanticResourceAttributes } from '@opentelemetry/semantic-conventions';

/**
 * Global OTEL SDK instance
 */
let sdk = null;
let tracer = null;
let initialized = false;

/**
 * Initialize OpenTelemetry SDK with Jaeger exporter
 * @returns {Promise<Object>} Tracer instance
 */
export async function initializeTracer() {
  if (initialized) {
    return tracer;
  }

  try {
    // Configure resource attributes
    const resource = new Resource({
      [SemanticResourceAttributes.SERVICE_NAME]: 'unrdf-cli',
      [SemanticResourceAttributes.SERVICE_VERSION]: '2.1.0',
      [SemanticResourceAttributes.SERVICE_NAMESPACE]: 'unrdf',
      [SemanticResourceAttributes.DEPLOYMENT_ENVIRONMENT]: process.env.NODE_ENV || 'development'
    });

    // Configure Jaeger exporter
    const jaegerEndpoint = process.env.JAEGER_ENDPOINT || 'http://localhost:14268/api/traces';
    const exporter = new JaegerExporter({
      endpoint: jaegerEndpoint,
      // Agent host/port for UDP transport (alternative to HTTP)
      host: process.env.JAEGER_AGENT_HOST || 'localhost',
      port: parseInt(process.env.JAEGER_AGENT_PORT || '6832', 10)
    });

    // Initialize NodeSDK with Jaeger exporter
    // NodeSDK will automatically create a BatchSpanProcessor
    sdk = new NodeSDK({
      resource,
      traceExporter: exporter
    });

    await sdk.start();

    // Get tracer instance
    tracer = trace.getTracer('unrdf-cli', '2.1.0');
    initialized = true;

    if (process.env.OTEL_DEBUG) {
      console.log('[OTEL] Tracer initialized');
      console.log(`[OTEL] Exporting to Jaeger: ${jaegerEndpoint}`);
      console.log(`[OTEL] Service: unrdf-cli`);
    }

    return tracer;
  } catch (error) {
    console.warn(`[OTEL] Failed to initialize tracer: ${error.message}`);
    // Return no-op tracer on failure
    tracer = trace.getTracer('unrdf-cli-noop');
    initialized = true;
    return tracer;
  }
}

/**
 * Get the tracer instance (initializes if needed)
 * @returns {Promise<Object>} Tracer instance
 */
export async function getTracer() {
  if (!initialized) {
    return initializeTracer();
  }
  return tracer;
}

/**
 * Shutdown the tracer and flush pending spans
 * @returns {Promise<void>}
 */
export async function shutdownTracer() {
  if (!sdk || !initialized) {
    return;
  }

  try {
    if (process.env.OTEL_DEBUG) {
      console.log('[OTEL] Flushing spans and shutting down...');
    }

    // Shutdown will flush all pending spans
    await sdk.shutdown();

    if (process.env.OTEL_DEBUG) {
      console.log('[OTEL] Shutdown complete');
    }

    initialized = false;
    sdk = null;
    tracer = null;
  } catch (error) {
    console.warn(`[OTEL] Error during shutdown: ${error.message}`);
  }
}

/**
 * Wrap an async function with a span
 * @param {string} spanName - Name of the span
 * @param {Function} fn - Async function to wrap
 * @param {Object} [attributes] - Span attributes
 * @returns {Function} Wrapped function
 */
export function traced(spanName, fn, attributes = {}) {
  return async function tracedFn(...args) {
    const currentTracer = await getTracer();
    const span = currentTracer.startSpan(spanName, {
      attributes: {
        'code.function': fn.name || 'anonymous',
        ...attributes
      }
    });

    try {
      return await context.with(trace.setSpan(context.active(), span), () => fn(...args));
    } catch (error) {
      span.recordException(error);
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message
      });
      throw error;
    } finally {
      span.end();
    }
  };
}

/**
 * Start a manual span
 * @param {string} spanName - Name of the span
 * @param {Object} [attributes] - Span attributes
 * @returns {Promise<Object>} Span object
 */
export async function startSpan(spanName, attributes = {}) {
  const currentTracer = await getTracer();
  return currentTracer.startSpan(spanName, { attributes });
}

/**
 * Get the current trace ID for logging
 * @returns {string} Trace ID
 */
export function getCurrentTraceId() {
  const spanContext = trace.getSpanContext(context.active());
  return spanContext?.traceId || 'no-trace-id';
}

/**
 * Print trace information to console
 * @param {string} operation - Operation name
 */
export function printTraceInfo(operation) {
  const traceId = getCurrentTraceId();
  const jaegerUrl = process.env.JAEGER_UI_URL || 'http://localhost:16686';

  if (traceId && traceId !== 'no-trace-id') {
    console.log(`\n🔍 Trace ID: ${traceId}`);
    console.log(`   View in Jaeger: ${jaegerUrl}/trace/${traceId}`);
  }
}
