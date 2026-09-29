/**
 * @file Federation tracing helper
 * @module federation/tracing
 *
 * Implements .claude/rules/scoped/p2-otel-tracing.md:
 * - span names are `federation.<operation>`
 * - every span carries federation.node_id / federation.peer_count / federation.receipt_count
 *   (defaulted to 0 when not applicable) and federation.quorum_id when one is supplied
 * - span status is set explicitly on every path (OK, or ERROR + recordException)
 * - the span is ended exactly once, in a finally
 */

import { trace, context, SpanStatusCode } from '@opentelemetry/api';

export const TRACER_NAME = '@unrdf/federation';

/** @returns {import('@opentelemetry/api').Tracer} */
export function getTracer() {
  return trace.getTracer(TRACER_NAME);
}

/**
 * Default node identifier when the caller has none.
 * @returns {string}
 */
export function defaultNodeId() {
  return process.env.FEDERATION_NODE_ID || `node-${process.pid}`;
}

/**
 * Build the required base attributes for a federation span started with
 * `tracer.startActiveSpan(name, { attributes }, fn)`.
 *
 * @param {Object} [info]
 * @param {string} [info.nodeId]
 * @param {string} [info.quorumId]
 * @param {number} [info.peerCount]
 * @param {number} [info.receiptCount]
 * @returns {Object<string, string|number>}
 */
export function spanAttributes({ nodeId, quorumId, peerCount = 0, receiptCount = 0 } = {}) {
  const attrs = {
    'federation.node_id': nodeId || defaultNodeId(),
    'federation.peer_count': peerCount,
    'federation.receipt_count': receiptCount,
  };
  if (quorumId) attrs['federation.quorum_id'] = quorumId;
  return attrs;
}

/**
 * Run `fn(span)` inside a federation span.
 * Works for sync and async functions; the span is ended exactly once in a finally.
 * A thrown error (or rejection) records the exception and sets ERROR status; a normal
 * return sets OK unless `fn` already set a status on the span.
 *
 * @template T
 * @param {string} name - Span name, must start with `federation.`
 * @param {Object} attributes - Extra attributes (federation.* keys recommended)
 * @param {(span: import('@opentelemetry/api').Span) => T} fn
 * @param {{parent?: import('@opentelemetry/api').Span}} [opts]
 * @returns {T}
 */
export function withSpan(name, attributes, fn, opts = {}) {
  if (!name.startsWith('federation.')) {
    throw new Error(`Federation span name must start with "federation.": ${name}`);
  }
  const ctx = opts.parent ? trace.setSpan(context.active(), opts.parent) : context.active();
  const span = getTracer().startSpan(
    name,
    {
      attributes: {
        'federation.node_id': defaultNodeId(),
        'federation.peer_count': 0,
        'federation.receipt_count': 0,
        ...attributes,
      },
    },
    ctx
  );

  let statusSet = false;
  const origSetStatus = span.setStatus.bind(span);
  span.setStatus = status => {
    statusSet = true;
    return origSetStatus(status);
  };

  const fail = error => {
    const err = error instanceof Error ? error : new Error(String(error));
    span.recordException(err);
    span.setStatus({ code: SpanStatusCode.ERROR, message: err.message });
  };

  let result;
  try {
    result = fn(span);
  } catch (error) {
    fail(error);
    span.end();
    throw error;
  }

  if (result && typeof result.then === 'function') {
    return /** @type {any} */ (
      result
        .then(
          value => {
            if (!statusSet) span.setStatus({ code: SpanStatusCode.OK });
            return value;
          },
          error => {
            fail(error);
            throw error;
          }
        )
        .finally(() => span.end())
    );
  }

  try {
    if (!statusSet) span.setStatus({ code: SpanStatusCode.OK });
  } finally {
    span.end();
  }
  return result;
}
