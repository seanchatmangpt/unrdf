/**
 * @file Standardized OTEL Tracing for CLI Commands
 * @module cli/utils/tracing
 *
 * FM-009: Consistent OTEL instrumentation across all commands
 * Provides simple wrapper for adding telemetry without boilerplate
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';

/**
 * Get tracer instance for CLI
 */
export function getTracer() {
  return trace.getTracer('unrdf-cli');
}

/**
 * Wrap an async function with OTEL tracing
 *
 * @param {string} spanName - Name of the span (e.g., 'hook.create', 'store.query')
 * @param {Function} fn - Async function to wrap
 * @param {Object} options - Options
 * @param {Object} options.attributes - Initial span attributes
 * @param {Function} options.onSuccess - Called with (span, result) on success
 * @param {Function} options.onError - Called with (span, error) on error
 * @returns {Function} Wrapped function
 *
 * @example
 * const createHook = withTracing('hook.create', async (name, type) => {
 *   // ... implementation ...
 * }, {
 *   attributes: { 'hook.type': type }
 * });
 *
 * await createHook('my-hook', 'sparql-ask');
 */
export function withTracing(spanName, fn, options = {}) {
  const {
    attributes = {},
    onSuccess,
    onError
  } = options;

  return async function traced(...args) {
    const tracer = getTracer();

    return await tracer.startActiveSpan(spanName, async (span) => {
      try {
        // Set initial attributes
        span.setAttributes(attributes);

        // Execute function
        const result = await fn(...args);

        // Success handling
        span.setStatus({ code: SpanStatusCode.OK });

        if (onSuccess) {
          onSuccess(span, result);
        }

        return result;
      } catch (error) {
        // Error handling
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message
        });

        if (onError) {
          onError(span, error);
        }

        throw error;
      } finally {
        span.end();
      }
    });
  };
}

/**
 * Wrap command run function with tracing
 *
 * @param {string} commandName - Command name (e.g., 'hook create')
 * @param {Function} runFn - Command run function
 * @param {Object} options - Options
 * @returns {Function} Wrapped run function
 *
 * @example
 * export const createCommand = defineCommand({
 *   meta: { name: 'create' },
 *   async run(ctx) {
 *     return withCommandTracing('hook.create', async () => {
 *       // ... command logic ...
 *     }, {
 *       extractAttributes: (ctx) => ({
 *         'hook.name': ctx.args.name,
 *         'hook.type': ctx.args.type
 *       })
 *     })(ctx);
 *   }
 * });
 */
export function withCommandTracing(commandName, runFn, options = {}) {
  const { extractAttributes } = options;

  return async function tracedCommand(ctx) {
    const tracer = getTracer();

    return await tracer.startActiveSpan(`command.${commandName}`, async (span) => {
      try {
        // Extract attributes from context if provided
        if (extractAttributes) {
          const attrs = extractAttributes(ctx);
          span.setAttributes(attrs);
        }

        // Set common attributes
        span.setAttributes({
          'cli.command': commandName,
          'cli.version': process.env.npm_package_version || 'unknown'
        });

        const result = await runFn(ctx);

        span.setStatus({ code: SpanStatusCode.OK });
        return result;
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
    });
  };
}

/**
 * Create a traced operation (manual span control)
 *
 * @param {string} spanName - Span name
 * @param {Object} attributes - Initial attributes
 * @returns {Object} Span controller
 *
 * @example
 * const operation = createTracedOperation('file.read', { 'file.path': filePath });
 * try {
 *   const data = await readFile(filePath);
 *   operation.success({ 'file.size': data.length });
 *   return data;
 * } catch (error) {
 *   operation.error(error);
 *   throw error;
 * } finally {
 *   operation.end();
 * }
 */
export function createTracedOperation(spanName, attributes = {}) {
  const tracer = getTracer();
  const span = tracer.startSpan(spanName);

  span.setAttributes(attributes);

  return {
    span,
    setAttribute(key, value) {
      span.setAttribute(key, value);
    },
    setAttributes(attrs) {
      span.setAttributes(attrs);
    },
    success(additionalAttrs = {}) {
      span.setAttributes(additionalAttrs);
      span.setStatus({ code: SpanStatusCode.OK });
    },
    error(error, additionalAttrs = {}) {
      span.recordException(error);
      span.setAttributes(additionalAttrs);
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message
      });
    },
    end() {
      span.end();
    }
  };
}

/**
 * Add event to current span
 *
 * @param {string} name - Event name
 * @param {Object} attributes - Event attributes
 *
 * @example
 * addSpanEvent('validation.completed', {
 *   'validation.passed': true,
 *   'validation.duration_ms': 150
 * });
 */
export function addSpanEvent(name, attributes = {}) {
  const span = trace.getActiveSpan();
  if (span) {
    span.addEvent(name, attributes);
  }
}

/**
 * Set attribute on current span
 *
 * @param {string} key - Attribute key
 * @param {any} value - Attribute value
 *
 * @example
 * setSpanAttribute('user.id', userId);
 */
export function setSpanAttribute(key, value) {
  const span = trace.getActiveSpan();
  if (span) {
    span.setAttribute(key, value);
  }
}

export default {
  getTracer,
  withTracing,
  withCommandTracing,
  createTracedOperation,
  addSpanEvent,
  setSpanAttribute
};
