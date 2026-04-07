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
import _resources from '@opentelemetry/resources';
const _Resource = _resources.Resource;
import _semconv from '@opentelemetry/semantic-conventions';
const _ATTR_SERVICE_NAME = _semconv.ATTR_SERVICE_NAME;
const _ATTR_SERVICE_VERSION = _semconv.ATTR_SERVICE_VERSION;

let _instrumented = false;

/**
 * Provider-specific attribute maps.
 * Each entry maps response fields to gen_ai semantic convention attributes.
 */
const PROVIDER_RESPONSE_KEYS = {
  openai: {
    model: 'model',
    finishReason: 'finish_reason' in {} ? 'finish_reason' : 'finish_reason',
    promptTokens: 'usage.prompt_tokens',
    completionTokens: 'usage.completion_tokens',
  },
  groq: {
    model: 'model',
    finishReason: 'finish_reason',
    promptTokens: 'usage.prompt_tokens',
    completionTokens: 'usage.completion_tokens',
  },
  anthropic: {
    model: 'model',
    finishReason: 'stop_reason',
    promptTokens: 'usage.input_tokens',
    completionTokens: 'usage.output_tokens',
  },
};

/**
 * Create a tracer for LLM operations.
 */
export function getLLMTracer(name = 'unrdf-llm') {
  return trace.getTracer(name, '1.0.0');
}

/**
 * Start an LLM span with gen_ai semantic conventions.
 *
 * Creates a CLIENT span and sets initial request attributes.
 * The caller is responsible for ending the span via `endLLMSpan`.
 *
 * @param {string} name - Span name (e.g. 'gen_ai.chat')
 * @param {object} attributes - Initial attributes including at minimum:
 *   - `gen_ai.system` — provider name
 *   - `gen_ai.request.model` — model name
 *   - `gen_ai.operation.name` — operation description
 * @returns {import('@opentelemetry/api').Span} The started span
 */
export function startLLMSpan(name, attributes = {}) {
  const tracer = getLLMTracer();
  return tracer.startSpan(name, {
    kind: SpanKind.CLIENT,
    attributes: {
      'gen_ai.system': 'unknown',
      'gen_ai.request.model': 'unknown',
      'gen_ai.operation.name': name,
      ...attributes,
    },
  });
}

/**
 * End an LLM span, recording response attributes.
 *
 * Sets response model, finish reason, and token usage following
 * gen_ai.* conventions. Handles provider-specific response shapes
 * (openai, groq, anthropic).
 *
 * @param {import('@opentelemetry/api').Span} span - Span to end
 * @param {object} response - LLM API response object
 * @param {string} [provider='openai'] - Provider name for key mapping
 */
export function endLLMSpan(span, response, provider = 'openai') {
  if (!span) return;

  try {
    const keys = PROVIDER_RESPONSE_KEYS[provider] ?? PROVIDER_RESPONSE_KEYS.openai;

    const attrs = {};

    // Response model
    if (response?.model) {
      attrs['gen_ai.response.model'] = response.model;
    }

    // Finish reasons
    if (response?.finish_reason !== undefined) {
      attrs['gen_ai.response.finish_reasons'] = Array.isArray(response.finish_reason)
        ? response.finish_reason
        : [String(response.finish_reason)];
    } else if (response?.stop_reason !== undefined) {
      attrs['gen_ai.response.finish_reasons'] = Array.isArray(response.stop_reason)
        ? response.stop_reason
        : [String(response.stop_reason)];
    }

    // Token usage
    if (response?.usage) {
      const inputTokens = response.usage.prompt_tokens ?? response.usage.input_tokens ?? 0;
      const outputTokens = response.usage.completion_tokens ?? response.usage.output_tokens ?? 0;
      attrs['gen_ai.usage.input_tokens'] = inputTokens;
      attrs['gen_ai.usage.output_tokens'] = outputTokens;
      attrs['gen_ai.usage.total_tokens'] = inputTokens + outputTokens;
    }

    if (Object.keys(attrs).length > 0) {
      span.setAttributes(attrs);
    }
  } catch {
    // Never throw from telemetry
  } finally {
    span.end();
  }
}

/**
 * Wrap an LLM call with auto-created/ended span.
 *
 * Convenience wrapper that combines startLLMSpan + endLLMSpan.
 * Supports openai, groq, and anthropic providers with automatic
 * response attribute extraction.
 *
 * @param {string} provider - LLM provider ('groq', 'openai', 'anthropic')
 * @param {string} model - Model identifier (e.g. 'gpt-4o', 'llama-3.3-70b')
 * @param {Function} fn - async () => response — the actual LLM call
 * @returns {Promise<*>} The LLM response
 *
 * @example
 *   const response = await withLLMTrace('groq', 'llama-3.3-70b', async () => {
 *     return await groq.chat.completions.create({ ... });
 *   });
 */
export async function withLLMTrace(provider, model, fn) {
  const span = startLLMSpan(`gen_ai.${provider}.chat`, {
    'gen_ai.system': provider,
    'gen_ai.request.model': model,
    'gen_ai.operation.name': 'chat',
  });

  try {
    const result = await fn();
    span.setStatus({ code: SpanStatusCode.OK });
    endLLMSpan(span, result, provider);
    return result;
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
    endLLMSpan(span, null, provider);
    throw error;
  }
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
