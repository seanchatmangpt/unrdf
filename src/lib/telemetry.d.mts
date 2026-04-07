/**
 * Get (or create) the UNRDF pipeline tracer.
 * Safe to call multiple times even when no OTel SDK is registered.
 *
 * @param {string} [name] - Tracer name override
 * @returns {import('@opentelemetry/api').Tracer}
 */
export function getTracer(name?: string): import("@opentelemetry/api").Tracer;
/**
 * Wrap an async function with an OpenTelemetry pipeline span.
 *
 * Creates a span named `pipeline.<stageName>`, sets `SpanKind.INTERNAL`,
 * records timing, and handles errors automatically.
 *
 * @param {string} stageName - Pipeline stage identifier (e.g. 'validate', 'admit')
 * @param {Function} handler - async (span) => result
 * @returns {Promise<*>} The handler's return value
 */
export function withPipelineSpan(stageName: string, handler: Function): Promise<any>;
/**
 * Wrap an async LLM API call with an OpenTelemetry span.
 *
 * Creates a span named `llm.<operationName>`, sets `SpanKind.CLIENT`,
 * records timing, and captures response metadata (tokens, finish_reason).
 *
 * @param {string} operationName - Descriptive operation name (e.g. 'groq-query')
 * @param {Function} handler - async (span) => response
 * @returns {Promise<*>} The handler's return value (typically the API response)
 */
export function withLLMSpan(operationName: string, handler: Function): Promise<any>;
