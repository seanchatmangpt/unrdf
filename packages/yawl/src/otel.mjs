/**
 * OpenTelemetry Instrumentation for YAWL Workflow Engine
 *
 * Provides observability for critical workflow operations:
 * - Workflow creation
 * - Case management
 * - Task lifecycle (enable, start, complete)
 * - Resource allocation
 * - Receipt generation
 *
 * @module @unrdf/yawl/otel
 */

import { trace, metrics, context } from '@opentelemetry/api';
import { Resource } from '@opentelemetry/resources';
import { BasicTracerProvider, ConsoleSpanExporter, SimpleSpanProcessor } from '@opentelemetry/sdk-trace-base';
import { ATTR_SERVICE_NAME, ATTR_SERVICE_VERSION } from '@opentelemetry/semantic-conventions';

// =============================================================================
// Configuration
// =============================================================================

const ENABLED = process.env.OTEL_ENABLED !== 'false';
const SERVICE_NAME = '@unrdf/yawl';
const SERVICE_VERSION = '6.0.0';

// =============================================================================
// Provider Setup
// =============================================================================

let tracerProvider;
let tracer;
let meter;

if (ENABLED) {
  // Create resource with service information
  const resource = new Resource({
    [ATTR_SERVICE_NAME]: SERVICE_NAME,
    [ATTR_SERVICE_VERSION]: SERVICE_VERSION,
  });

  // Create tracer provider
  tracerProvider = new BasicTracerProvider({
    resource,
  });

  // Add console exporter for development (would be OTLP in production)
  tracerProvider.addSpanProcessor(
    new SimpleSpanProcessor(new ConsoleSpanExporter())
  );

  // Register the provider
  tracerProvider.register();

  // Get tracer and meter instances
  tracer = trace.getTracer(SERVICE_NAME, SERVICE_VERSION);
  meter = metrics.getMeter(SERVICE_NAME, SERVICE_VERSION);
} else {
  // No-op tracer and meter when disabled
  tracer = trace.getTracer('noop');
  meter = metrics.getMeter('noop');
}

// =============================================================================
// Metrics Setup
// =============================================================================

// Counters
export const casesCreatedCounter = meter.createCounter('yawl.cases.created', {
  description: 'Total number of workflow cases created',
  unit: '1',
});

export const tasksCompletedCounter = meter.createCounter('yawl.tasks.completed', {
  description: 'Total number of tasks completed',
  unit: '1',
});

export const receiptsGeneratedCounter = meter.createCounter('yawl.receipts.generated', {
  description: 'Total number of cryptographic receipts generated',
  unit: '1',
});

export const resourcesAllocatedCounter = meter.createCounter('yawl.resources.allocated', {
  description: 'Total number of resources allocated',
  unit: '1',
});

export const workflowsCreatedCounter = meter.createCounter('yawl.workflows.created', {
  description: 'Total number of workflows created',
  unit: '1',
});

// Histograms (duration metrics)
export const caseCreationHistogram = meter.createHistogram('yawl.case.creation.duration', {
  description: 'Duration of case creation in milliseconds',
  unit: 'ms',
});

export const taskCompletionHistogram = meter.createHistogram('yawl.task.completion.duration', {
  description: 'Duration of task completion in milliseconds',
  unit: 'ms',
});

export const resourceAllocationHistogram = meter.createHistogram('yawl.resource.allocation.duration', {
  description: 'Duration of resource allocation in milliseconds',
  unit: 'ms',
});

export const workflowCreationHistogram = meter.createHistogram('yawl.workflow.creation.duration', {
  description: 'Duration of workflow creation in milliseconds',
  unit: 'ms',
});

export const receiptGenerationHistogram = meter.createHistogram('yawl.receipt.generation.duration', {
  description: 'Duration of receipt generation in milliseconds',
  unit: 'ms',
});

// UpDownCounters (gauges)
export const activeCasesGauge = meter.createUpDownCounter('yawl.cases.active', {
  description: 'Number of currently active cases',
  unit: '1',
});

export const resourcesAvailableGauge = meter.createUpDownCounter('yawl.resources.available', {
  description: 'Number of currently available resources',
  unit: '1',
});

// =============================================================================
// Exported Tracer
// =============================================================================

export { tracer, meter, context };

/**
 * Shutdown the tracer provider (for graceful shutdown)
 * @returns {Promise<void>}
 */
export async function shutdown() {
  if (tracerProvider) {
    await tracerProvider.shutdown();
  }
}

/**
 * Start a span with common attributes
 *
 * @param {string} name - Span name
 * @param {Object} [attributes] - Span attributes
 * @param {Object} [options] - Span options
 * @returns {import('@opentelemetry/api').Span}
 */
export function startSpan(name, attributes = {}, options = {}) {
  return tracer.startSpan(name, {
    attributes,
    ...options,
  });
}

/**
 * Execute a function within a span
 *
 * @template T
 * @param {string} name - Span name
 * @param {Object} attributes - Span attributes
 * @param {() => T | Promise<T>} fn - Function to execute
 * @returns {Promise<T>}
 */
export async function withSpan(name, attributes, fn) {
  const span = startSpan(name, attributes);

  try {
    const result = await Promise.resolve(fn());
    span.setStatus({ code: 1 }); // OK
    return result;
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message }); // ERROR
    throw error;
  } finally {
    span.end();
  }
}
