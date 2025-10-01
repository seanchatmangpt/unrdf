import { trace, context, SpanStatusCode } from '@opentelemetry/api'

const tracer = trace.getTracer('knowledge-platform')

export function traced(name, fn) {
  return async function tracedFn(...args) {
    const span = tracer.startSpan(name)
    try {
      return await context.with(trace.setSpan(context.active(), span), () => fn(...args))
    } catch (err) {
      span.recordException(err)
      span.setStatus({ code: SpanStatusCode.ERROR, message: err.message })
      throw err
    } finally {
      span.end()
    }
  }
}
/**
 * Wrap a synchronous function in a span.
 * @param {string} name
 * @param {Function} fn
 */
export function tracedSync(name, fn) {
  return function tracedSyncFn(...args) {
    const span = tracer.startSpan(name)
    try {
      return fn(...args)
    } catch (err) {
      span.recordException(err)
      span.setStatus({ code: SpanStatusCode.ERROR, message: err.message })
      throw err
    } finally {
      span.end()
    }
  }
}