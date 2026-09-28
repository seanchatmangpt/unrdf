/**
 * OpenTelemetry Provider Configuration
 *
 * Refactored to follow OpenTelemetry SDK best practices.
 * Uses NodeTracerProvider with SimpleSpanProcessor for validation.
 */

import { NodeTracerProvider } from '@opentelemetry/sdk-trace-node';
import { BatchSpanProcessor } from '@opentelemetry/sdk-trace-base';

let globalProvider = null;
let processor = null;
const pendingSpans = new Map(); // Map<validationId, Array<Span>> (used when no callback registered)
const spanCollectors = new Map(); // Map<validationId, onSpanEnd|null> for validations in flight
let spanExporter = null;
let shutdownPromise = null;
let exporterCalled = false;

/**
 * Validate non-empty string input (poka-yoke: prevents invalid validationId)
 * @param {string} value - Value to validate
 * @param {string} name - Name of parameter for error message
 * @throws {Error} If value is not a non-empty string
 */
function validateNonEmptyString(value, name) {
  if (typeof value !== 'string' || value.length === 0) {
    throw new Error(`${name} must be a non-empty string, got: ${typeof value}`);
  }
}

/**
 * Route a converted span to the validation that is currently running.
 * Validations run one at a time, so the most recently registered collector owns
 * every span exported while it is active.
 * @param {Object} spanData - Converted span data
 */
function dispatchSpan(spanData) {
  const ids = [...spanCollectors.keys()];
  const ownerId = ids[ids.length - 1];
  if (ownerId === undefined) {
    console.error(`[OTEL Exporter] Dropping span '${spanData.name}': no active validation`);
    return;
  }
  const onSpanEnd = spanCollectors.get(ownerId);
  if (typeof onSpanEnd === 'function') {
    onSpanEnd(spanData);
    return;
  }
  if (!pendingSpans.has(ownerId)) {
    pendingSpans.set(ownerId, []);
  }
  pendingSpans.get(ownerId).push(spanData);
}

/**
 * Create the span exporter shared by all validations.
 * @returns {{export: Function, shutdown: Function}}
 */
function createSpanExporter() {
  return {
    export: (spans) => {
      console.log(`[OTEL Exporter] export() called with ${spans?.length || 0} spans`);
      if (!spans || !Array.isArray(spans) || spans.length === 0) {
        console.log(`[OTEL Exporter] No spans to export`);
        return Promise.resolve({ code: 0 });
      }

      console.log(`[OTEL Exporter] Processing ${spans.length} spans...`);

      spans.forEach((span, index) => {
        try {
          console.log(`[OTEL Exporter] Processing span ${index + 1}/${spans.length}: ${span.name || 'unnamed'}`);

          // Convert span data
          const duration = span.duration
            ? span.duration[0] * 1000 + span.duration[1] / 1000000
            : 0;

          const timestamp = span.startTime
            ? span.startTime[0] * 1000 + span.startTime[1] / 1000000
            : Date.now();

          let status = 'unset';
          if (span.status) {
            if (span.status.code === 1) status = 'ok';
            else if (span.status.code === 2) status = 'error';
          }

          const attributes = {};
          if (span.attributes) {
            if (span.attributes instanceof Map) {
              span.attributes.forEach((value, key) => {
                attributes[key] = value;
              });
            } else if (typeof span.attributes === 'object') {
              Object.entries(span.attributes).forEach(([key, value]) => {
                attributes[key] = value;
              });
            }
          }

          const spanCtx = typeof span.spanContext === 'function' ? span.spanContext() : span.spanContext;
          const spanData = {
            name: span.name,
            status: status,
            duration: duration,
            attributes: attributes,
            timestamp: timestamp,
            spanId: spanCtx?.spanId || '',
            traceId: spanCtx?.traceId || '',
          };

          console.log(`[OTEL Exporter] Converted span: ${spanData.name}`);

          dispatchSpan(spanData);
        } catch (error) {
          console.error(`[OTEL Exporter] Error processing span:`, error);
        }
      });

      return Promise.resolve({ code: 0 });
    },
    shutdown: () => Promise.resolve(),
  };
}

/**
 * Initialize OTEL provider with span processor
 *
 * **Best Practice**: Creates NodeTracerProvider following SDK patterns.
 * The provider is registered globally, making spans available to all tracers.
 *
 * @param {string} validationId - Validation ID to track spans
 * @param {Function} [onSpanEnd] - Callback when span ends (receives span data object)
 * @returns {Promise<NodeTracerProvider>} The initialized provider
 * @throws {Error} If validationId is invalid
 */
export async function ensureProviderInitialized(validationId, onSpanEnd = null) {
  // Poka-yoke: Validate input
  validateNonEmptyString(validationId, 'validationId');
  if (onSpanEnd !== null && typeof onSpanEnd !== 'function') {
    throw new Error(`onSpanEnd must be a function or null, got: ${typeof onSpanEnd}`);
  }
  spanCollectors.set(validationId, onSpanEnd);

  // Create provider if not already initialized (lazy initialization)
  if (!globalProvider) {
    spanExporter = createSpanExporter();

    // Create and register provider
    // Use BatchSpanProcessor with immediate flush for synchronous span collection
    processor = new BatchSpanProcessor(spanExporter, {
      scheduledDelayMillis: 0,      // Export immediately (no batching delay)
      exportTimeoutMillis: 100,     // Short timeout
      maxQueueSize: 1000,           // Max queued spans
    });
    globalProvider = new NodeTracerProvider({
      spanProcessors: [processor],
    });

    // Register provider globally - this is the correct way per OpenTelemetry SDK
    globalProvider.register();

    console.log('[OTEL Provider] NodeTracerProvider registered globally');
  }

  return globalProvider;
}

/**
 * Shutdown provider and cleanup
 *
 * **Best Practice**: Gracefully shutdown follows SDK patterns.
 * This should be called when validation is complete.
 *
 * @param {string} [validationId] - Optional validation ID to shutdown
 */
export async function shutdownProvider(validationId) {
  if (validationId !== undefined) {
    validateNonEmptyString(validationId, 'validationId');
    spanCollectors.delete(validationId);
  }

  // If no more validations or no validationId provided, shutdown provider
  if ((validationId === undefined || spanCollectors.size === 0) && globalProvider) {
    if (!shutdownPromise) {
      shutdownPromise = globalProvider.shutdown()
        .then(() => {
          globalProvider = null;
          processor = null;
          spanCollectors.clear();
          shutdownPromise = null;
        })
        .catch((error) => {
          console.error('[OTEL Provider] Shutdown error:', error);
          shutdownPromise = null;
        });
    }
    return shutdownPromise;
  }

  return Promise.resolve();
}

/**
 * Force flush all pending spans
 *
 * **Best Practice**: Force flush ensures all data is exported before shutdown.
 *
 * @returns {Promise<void>}
 */
export async function forceFlush() {
  if (processor) {
    try {
      await Promise.race([
        processor.forceFlush(),
        new Promise((_, reject) =>
          setTimeout(() => reject(new Error('forceFlush timeout after 5s')), 5000)
        )
      ]);
    } catch (error) {
      console.error('[OTEL Provider] Error during forceFlush:', error.message);
    }
  }
}

/**
 * Get current provider
 */
export function getProvider() {
  return globalProvider;
}

/**
 * Get span exporter (for direct access)
 *
 * @returns {Object} Span exporter object
 */
export function getSpanExporter() {
  return spanExporter;
}

/**
 * Get pending spans for a validation ID
 *
 * **Poka-yoke**: Returns spans for the specified validation, clearing them from pending storage
 *
 * @param {string} validationId - Validation ID to get spans for
 * @returns {Array<Object>} Array of span data objects
 */
export function getPendingSpans(validationId) {
  validateNonEmptyString(validationId, 'validationId');
  const spans = pendingSpans.get(validationId) || [];
  pendingSpans.delete(validationId);
  return spans;
}
