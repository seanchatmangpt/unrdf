/**
 * @file Telemetry Middleware
 * @module cli-v2/middleware/telemetry
 */

import { trace, context, SpanStatusCode } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf-cli');

/**
 * Telemetry middleware for command tracing
 * @param {Object} ctx - CLI context
 */
export async function telemetryMiddleware(ctx) {
  const config = ctx.config || {};

  if (!config.telemetry || !config.telemetry.enabled) {
    return;
  }

  // Create span for CLI command
  const span = tracer.startSpan('cli-command', {
    attributes: {
      'cli.command': process.argv.slice(2).join(' '),
      'cli.version': '2.0.0'
    }
  });

  // Add span to context
  ctx.telemetry = {
    span,
    startTime: Date.now()
  };

  // Override console.log to capture output metrics
  const originalLog = console.log;
  let outputLines = 0;

  console.log = (...args) => {
    outputLines++;
    originalLog(...args);
  };

  // Cleanup on exit
  const cleanup = () => {
    const duration = Date.now() - ctx.telemetry.startTime;

    span.setAttributes({
      'cli.duration_ms': duration,
      'cli.output_lines': outputLines
    });

    span.setStatus({ code: SpanStatusCode.OK });
    span.end();

    console.log = originalLog;
  };

  ctx.cleanup = cleanup;
  process.on('exit', cleanup);
}
