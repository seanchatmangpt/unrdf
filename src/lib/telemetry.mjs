/**
 * @file Pipeline and LLM Telemetry Helpers
 * @module lib/telemetry
 *
 * @description
 * Provides `withPipelineSpan` and `withLLMSpan` wrappers for instrumenting
 * UNRDF pipeline stages and LLM API calls with OpenTelemetry.
 *
 * These helpers follow the OTel semantic conventions from the Weaver registry
 * (otel/registry/*.yaml) and are opt-in: they only create spans when an OTel
 * SDK is initialised (e.g. via `@opentelemetry/sdk-node` or the sidecar
 * telemetry module). When no SDK is active they silently pass through.
 *
 * Usage (pipeline stage):
 *   import { withPipelineSpan } from './telemetry.mjs';
 *   const result = await withPipelineSpan('validate', async (span) => {
 *     span.setAttributes({ 'pipeline.stage': 'validate' });
 *     // ... existing function body ...
 *     return result;
 *   });
 *
 * Usage (LLM call):
 *   import { withLLMSpan } from './telemetry.mjs';
 *   const response = await withLLMSpan('groq-query', async (span) => {
 *     span.setAttributes({ 'llm.model': 'llama-3.3-70b', 'llm.provider': 'groq' });
 *     // ... existing API call ...
 *     return response;
 *   });
 */

import { trace, SpanStatusCode, SpanKind } from '@opentelemetry/api';

const TRACER_NAME = 'unrdf-pipeline';
const TRACER_VERSION = '1.0.0';

/**
 * Get (or create) the UNRDF pipeline tracer.
 * Safe to call multiple times even when no OTel SDK is registered.
 *
 * @param {string} [name] - Tracer name override
 * @returns {import('@opentelemetry/api').Tracer}
 */
export function getTracer(name = TRACER_NAME) {
  return trace.getTracer(name, TRACER_VERSION);
}

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
export async function withPipelineSpan(stageName, handler) {
  const tracer = getTracer();

  return tracer.startActiveSpan(
    `pipeline.${stageName}`,
    {
      kind: SpanKind.INTERNAL,
      attributes: {
        'pipeline.stage': stageName,
      },
    },
    async (span) => {
      try {
        const result = await handler(span);
        span.setStatus({ code: SpanStatusCode.OK });
        return result;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    },
  );
}

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
export async function withLLMSpan(operationName, handler) {
  const tracer = getTracer();

  return tracer.startActiveSpan(
    `llm.${operationName}`,
    {
      kind: SpanKind.CLIENT,
      attributes: {
        'gen_ai.operation.name': operationName,
      },
    },
    async (span) => {
      try {
        const result = await handler(span);
        span.setStatus({ code: SpanStatusCode.OK });

        // Capture response metadata when available
        if (result) {
          const attrs = {};
          if (result.model) attrs['gen_ai.response.model'] = result.model;
          if (result.usage) {
            attrs['gen_ai.usage.input_tokens'] = result.usage.prompt_tokens ?? 0;
            attrs['gen_ai.usage.output_tokens'] = result.usage.completion_tokens ?? 0;
            attrs['gen_ai.usage.total_tokens'] =
              (result.usage.prompt_tokens ?? 0) + (result.usage.completion_tokens ?? 0);
          }
          if (result.finish_reason) {
            attrs['gen_ai.response.finish_reasons'] = Array.isArray(result.finish_reason)
              ? result.finish_reason
              : [result.finish_reason];
          }
          if (Object.keys(attrs).length > 0) {
            span.setAttributes(attrs);
          }
        }

        return result;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    },
  );
}
