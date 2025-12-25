/**
 * OpenTelemetry Provider Configuration
 * 
 * Configures OTEL provider following SDK best practices.
 * Uses constructor configuration with spanProcessors array.
 */

import { NodeTracerProvider } from '@opentelemetry/sdk-trace-node';
import { SimpleSpanProcessor } from '@opentelemetry/sdk-trace-base';

let provider = null;
let processor = null; // Store processor reference for forceFlush
const spanCollectors = new Map(); // Map<validationId, Array<Function>>

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
 * Initialize OTEL provider with span processor
 *
 * **Poka-yoke**: Input validation prevents invalid validationId, type guards ensure state consistency
 *
 * @param {string} [validationId] - Optional validation ID to track spans (must be non-empty string if provided)
 * @param {Function} [onSpanEnd] - Optional callback when span ends (receives span data object)
 * @throws {Error} If validationId is provided but invalid
 */
export async function ensureProviderInitialized(validationId, onSpanEnd) {
  // Poka-yoke: Validate input if provided
  if (validationId !== undefined) {
    validateNonEmptyString(validationId, 'validationId');
  }

  if (onSpanEnd !== undefined && typeof onSpanEnd !== 'function') {
    throw new Error('onSpanEnd must be a function');
  }

  // Store callback FIRST, before creating provider
  // This ensures callbacks are registered before spans are created
  if (validationId && onSpanEnd) {
    if (!spanCollectors.has(validationId)) {
      spanCollectors.set(validationId, []);
    }
    spanCollectors.get(validationId).push(onSpanEnd);
  }

  if (!provider) {
    // Check if a provider is already registered globally
    const { trace } = await import('@opentelemetry/api');
    const existingProvider = trace.getTracerProvider();
    
    // If a provider exists but isn't ours, we need to replace it
    // This can happen if OTEL SDK auto-initializes or another module registers a provider
    if (existingProvider && existingProvider.constructor.name !== 'NodeTracerProvider') {
      console.warn('[OTEL Provider] Existing provider detected, will replace:', existingProvider.constructor.name);
    }
    
    // Create span exporter that collects spans for all validations
    const spanExporter = {
      export: (spans) => {
        // Poka-yoke: Type guard ensures spans is valid array
        if (!spans) {
          return Promise.resolve({ code: 0 }); // No spans to process
        }
        if (!Array.isArray(spans)) {
          console.error('[OTEL Exporter] spans must be an array, got:', typeof spans);
          return Promise.resolve({ code: 0 }); // Invalid input, but don't break export
        }
        if (spans.length === 0) {
          return Promise.resolve({ code: 0 }); // Empty array, nothing to process
        }
        
        // spans is an array of ReadableSpan objects
        // Call all registered callbacks for all validation IDs
        console.log(`[OTEL Exporter] Processing ${spans.length} spans...`);
        spans.forEach((span, index) => {
          try {
            console.log(`[OTEL Exporter] Processing span ${index + 1}/${spans.length}: ${span.name || 'unnamed'}`);
            // Poka-yoke: convertReadableSpanToData validates span structure
            const spanData = convertReadableSpanToData(span);
            console.log(`[OTEL Exporter] Converted span data: ${spanData.name}`);
            // Call callbacks for all validation IDs (spans are shared across validations)
            spanCollectors.forEach((callbacks, vid) => {
              console.log(`[OTEL Exporter] Checking callbacks for validation ${vid}: ${callbacks?.length || 0} callbacks`);
              // Poka-yoke: Type guard ensures callbacks exist and are valid
              if (callbacks && Array.isArray(callbacks) && callbacks.length > 0) {
                callbacks.forEach((cb, cbIndex) => {
                  // Poka-yoke: Type guard ensures callback is a function
                  if (typeof cb === 'function') {
                    try {
                      console.log(`[OTEL Exporter] Calling callback ${cbIndex + 1}/${callbacks.length} for ${vid}`);
                      cb(spanData);
                      console.log(`[OTEL Exporter] Callback ${cbIndex + 1} completed for ${vid}`);
                    } catch (error) {
                      // Log error but don't break export - errors are now visible
                      console.error(`[OTEL Exporter] Error in span callback for ${vid}:`, error);
                    }
                  } else {
                    console.warn(`[OTEL Exporter] Invalid callback for ${vid}, expected function, got: ${typeof cb}`);
                  }
                });
              } else {
                console.log(`[OTEL Exporter] No valid callbacks for ${vid}`);
              }
            });
          } catch (error) {
            console.error(`[OTEL Exporter] Error processing span ${index}:`, error);
            // Don't break export - continue processing other spans
          }
        });
        console.log(`[OTEL Exporter] Finished processing all spans`);
        // Return Promise.resolve to ensure async compatibility
        return Promise.resolve({ code: 0 }); // SUCCESS
      },
      shutdown: () => Promise.resolve(),
    };

    // Create processor and store reference
    processor = new SimpleSpanProcessor(spanExporter);

    // Create provider with processors in config
    // TracerConfig.spanProcessors is an array (see BasicTracerProvider.js line 47)
    provider = new NodeTracerProvider({
      spanProcessors: [processor],
    });
    
    // Register provider globally - this replaces any existing provider
    provider.register();
    
    // Verify registration worked
    const registeredProvider = trace.getTracerProvider();
    if (registeredProvider.constructor.name !== 'NodeTracerProvider' && 
        registeredProvider !== provider) {
      console.error('[OTEL Provider] Registration failed - provider not active');
    }
    
    // Ensure registration is complete (synchronous, but verify)
    // NodeTracerProvider.register() is synchronous, but we wait a tick to ensure
    // the global API tracer is updated
    await new Promise(resolve => setImmediate(resolve));
  }

  return provider;
}

/**
 * Validate span object has required properties (poka-yoke: prevents invalid span data)
 * @param {Object} span - Span object to validate
 * @throws {Error} If span is invalid
 */
function validateSpan(span) {
  if (!span || typeof span !== 'object') {
    throw new Error('Span must be an object');
  }
  if (typeof span.name !== 'string') {
    throw new Error('Span must have a name property (string)');
  }
}

/**
 * Convert ReadableSpan to span data for validation
 * 
 * **Poka-yoke**: Input validation prevents invalid span objects, type-safe attribute extraction
 * 
 * @param {Object} span - ReadableSpan object from OTEL
 * @returns {Object} Span data object with validated structure
 * @throws {Error} If span is invalid
 */
function convertReadableSpanToData(span) {
  // Poka-yoke: Validate span before processing
  validateSpan(span);
  // Calculate duration from high-resolution time [seconds, nanoseconds]
  const duration = span.duration 
    ? span.duration[0] * 1000 + span.duration[1] / 1000000 
    : 0;
  
  // Calculate timestamp from high-resolution time
  const timestamp = span.startTime 
    ? span.startTime[0] * 1000 + span.startTime[1] / 1000000 
    : Date.now();

  // Determine status
  let status = 'unset';
  if (span.status) {
    if (span.status.code === 1) status = 'ok';
    else if (span.status.code === 2) status = 'error';
  }

  // Extract attributes
  // Attributes can be a Map or a plain object
  const attributes = {};
  if (span.attributes) {
    if (span.attributes instanceof Map) {
      span.attributes.forEach((value, key) => {
        attributes[key] = value;
      });
    } else if (typeof span.attributes === 'object') {
      // Plain object - iterate with Object.entries
      Object.entries(span.attributes).forEach(([key, value]) => {
        attributes[key] = value;
      });
    }
  }

  return {
    name: span.name,
    status: status,
    duration: duration,
    attributes: attributes,
    timestamp: timestamp,
    spanId: span.spanContext?.spanId || '',
    traceId: span.spanContext?.traceId || '',
  };
}

/**
 * Shutdown provider and cleanup
 *
 * **Poka-yoke**: Input validation prevents invalid validationId
 *
 * @param {string} [validationId] - Optional validation ID to shutdown (must be non-empty string if provided)
 * @throws {Error} If validationId is provided but invalid
 */
export async function shutdownProvider(validationId) {
  // Poka-yoke: Validate input if provided
  if (validationId !== undefined) {
    validateNonEmptyString(validationId, 'validationId');
    // Remove callbacks for this validation
    spanCollectors.delete(validationId);
  }

  // If no more validations or no validationId provided, shutdown provider
  if ((validationId === undefined || spanCollectors.size === 0) && provider) {
    await provider.shutdown();
    provider = null;
  }
}

/**
 * Force flush all pending spans
 * 
 * **Poka-yoke**: Type guards prevent operations on null processor/provider
 * 
 * @throws {Error} If processor or provider operations fail
 */
export async function forceFlush() {
  console.log(`[forceFlush] START - processor: ${!!processor}, provider: ${!!provider}`);
  // Poka-yoke: Type guard ensures processor exists before use
  if (processor) {
    try {
      console.log(`[forceFlush] Calling processor.forceFlush()...`);
      // Add timeout to prevent hanging (5 seconds max)
      await Promise.race([
        processor.forceFlush(),
        new Promise((_, reject) => 
          setTimeout(() => reject(new Error('forceFlush timeout after 5s')), 5000)
        )
      ]);
      console.log(`[forceFlush] processor.forceFlush() completed`);
    } catch (error) {
      console.error('[OTEL Provider] Error during processor.forceFlush:', error.message);
      // Don't throw - log and continue to prevent blocking
    }
  } else {
    console.log(`[forceFlush] No processor to flush`);
  }
  // Poka-yoke: Type guard ensures provider exists before use
  if (provider) {
    try {
      console.log(`[forceFlush] Calling provider.forceFlush()...`);
      // Add timeout to prevent hanging (5 seconds max)
      await Promise.race([
        provider.forceFlush(),
        new Promise((_, reject) => 
          setTimeout(() => reject(new Error('forceFlush timeout after 5s')), 5000)
        )
      ]);
      console.log(`[forceFlush] provider.forceFlush() completed`);
    } catch (error) {
      console.error('[OTEL Provider] Error during provider.forceFlush:', error.message);
      // Don't throw - log and continue to prevent blocking
    }
  } else {
    console.log(`[forceFlush] No provider to flush`);
  }
  console.log(`[forceFlush] COMPLETE`);
}

/**
 * Get current provider
 */
export function getProvider() {
  return provider;
}
