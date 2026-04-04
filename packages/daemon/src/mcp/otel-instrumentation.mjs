/**
 * @file MCP Tool OTEL Instrumentation
 * @module @unrdf/daemon/mcp/otel-instrumentation
 *
 * @description
 * OTEL instrumentation wrapper for all MCP tools
 */

import { withSpan, createTracer } from '../integrations/otel-tracer.mjs';
import { getCurrentTraceContext } from '../integrations/otel-context.mjs';

const tracer = createTracer('unrdf-daemon-mcp');

/**
 * Wrap an MCP tool handler with OTEL span instrumentation
 * @param {string} toolName - Name of the MCP tool
 * @param {Function} handler - Handler function
 * @returns {Function} Instrumented handler
 */
export function withMcpSpan(toolName, handler) {
  return async (args) => {
    return withSpan(tracer, `mcp.tool.${toolName}`, async (span) => {
      // Set semantic attributes
      span.setAttributes({
        'mcp.tool.name': toolName,
        'mcp.tool.args': JSON.stringify(args),
        'mcp.server.name': 'unrdf-daemon-mcp',
      });

      try {
        // Execute handler
        const result = await handler(args);

        // Record success
        span.setAttributes({
          'mcp.tool.success': true,
          'mcp.tool.result_size': JSON.stringify(result).length,
        });

        return result;
      } catch (error) {
        // Record failure
        span.setAttributes({
          'mcp.tool.success': false,
        });
        throw error;
      }
    });
  };
}

/**
 * Get trace context for logging
 * @returns {Object|null} Current trace context
 */
export function getTraceContext() {
  return getCurrentTraceContext();
}
