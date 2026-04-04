/**
 * OpenLLMetry Instrumentation for UNRDF
 *
 * Provides OpenTelemetry tracing for LLM calls with semantic conventions
 * compliant with https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-spans/
 *
 * Supports:
 *   - ai SDK (Vercel AI SDK) via experimental_telemetry
 *   - OpenAI direct calls via @opentelemetry/instrumentation-openai
 *   - Anthropic direct calls via custom span wrapper
 *
 * Usage:
 *   import { instrumentAI } from './openllmetry/instrumentation.mjs';
 *   await instrumentAI({ otlpEndpoint: 'localhost:4317' });
 *
 *   // Then use ai SDK with telemetry enabled:
 *   import { generateText } from 'ai';
 *   const result = await generateText({
 *     model: openai('gpt-4o'),
 *     prompt: 'Hello',
 *     experimental_telemetry: { isEnabled: true },
 *   });
 */

import { trace, SpanStatusCode, SpanKind } from '@opentelemetry/api';
import { Resource as _Resource } from '@opentelemetry/resources';
import { ATTR_SERVICE_NAME as _ATTR_SERVICE_NAME, ATTR_SERVICE_VERSION as _ATTR_SERVICE_VERSION } from '@opentelemetry/semantic-conventions';

let _instrumented = false;

/**
 * Create a tracer for LLM operations.
 */
export function getLLMTracer(name = 'unrdf-llm') {
  return trace.getTracer(name, '1.0.0');
}

/**
 * Wrap an LLM call with OpenTelemetry tracing using GenAI semantic conventions.
 *
 * @param {object} options
 * @param {string} options.provider - LLM provider (openai, anthropic, google, etc.)
 * @param {string} options.model - Model ID (gpt-4o, claude-3-opus, etc.)
 * @param {string} options.operation - Operation type (generate, embed, stream, etc.)
 * @param {Function} handler - The actual LLM call function
 * @param {object} [options.attributes] - Additional span attributes
 */
export async function withLLMSpan({ provider, model, operation, handler, attributes = {} }) {
  const tracer = getLLMTracer();

  return tracer.startActiveSpan(
    `gen_ai.${operation}`,
    {
      kind: SpanKind.CLIENT,
      attributes: {
        'gen_ai.system': provider,
        'gen_ai.request.model': model,
        'gen_ai.operation.name': operation,
        ...attributes,
      },
    },
    async (span) => {
      try {
        const _startTime = Date.now();
        const result = await handler(span);

        // Record response attributes
        if (result) {
          span.setAttributes({
            'gen_ai.response.model': result.model || model,
            'gen_ai.response.finish_reasons': Array.isArray(result.finish_reason)
              ? result.finish_reason
              : [result.finish_reason || 'unknown'],
          });

          // Record token usage if available
          if (result.usage) {
            const inputTokens = result.usage.prompt_tokens || 0
            const outputTokens = result.usage.completion_tokens || 0
            span.setAttributes({
              'gen_ai.usage.input_tokens': inputTokens,
              'gen_ai.usage.output_tokens': outputTokens,
              'gen_ai.usage.total_tokens': inputTokens + outputTokens,
            });
          }
        }

        span.setStatus({ code: SpanStatusCode.OK });
        return result;
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message,
        });
        throw error;
      } finally {
        span.end();
      }
    }
  );
}

/**
 * Wrap an AI SDK call with telemetry.
 * Returns the experimental_telemetry config to pass to the SDK.
 *
 * @param {object} [options]
 * @param {string} [options.functionId] - Function identifier for telemetry
 * @param {object} [options.metadata] - Additional telemetry metadata
 */
export function getAISDKTelemetryConfig({ functionId, metadata = {} } = {}) {
  return {
    isEnabled: true,
    functionId: functionId || 'unrdf-llm-call',
    metadata: {
      service: 'unrdf',
      environment: process.env.NODE_ENV || 'development',
      ...metadata,
    },
  };
}

/**
 * Instrument AI SDK streams with per-tool-call tracing.
 *
 * @param {string} toolName - Name of the AI tool being called
 * @param {Function} handler - The tool handler function
 * @param {object} [options]
 */
export function instrumentAIToolCall(toolName, handler, options = {}) {
  const tracer = getLLMTracer();

  return async (args) => {
    return tracer.startActiveSpan(
      `ai.toolCall`,
      {
        kind: SpanKind.INTERNAL,
        attributes: {
          'ai.operationId': 'ai.toolCall',
          'operation.name': 'ai.toolCall',
          'ai.toolCall.name': toolName,
          'ai.toolCall.args': typeof args === 'string' ? args : JSON.stringify(args),
          ...options.attributes,
        },
      },
      async (span) => {
        const _startTime = Date.now();
        try {
          const result = await handler(args);
          span.setAttributes({
            'ai.toolCall.result': typeof result === 'string' ? result.slice(0, 500) : JSON.stringify(result).slice(0, 500),
          });
          span.setStatus({ code: SpanStatusCode.OK });
          return result;
        } catch (error) {
          span.recordException(error);
          span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
          throw error;
        } finally {
          span.end();
        }
      }
    );
  };
}

/**
 * Mark instrumentation as complete (for health checks).
 */
export function setInstrumented() {
  _instrumented = true;
}

/**
 *
 */
export function isInstrumented() {
  return _instrumented;
}
