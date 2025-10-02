/**
 * @file Context Get Command
 * @module cli-v2/commands/context
 *
 * @description
 * Get context details with OpenTelemetry instrumentation
 */

import { defineCommand } from "citty";
import { trace, SpanStatusCode } from "@opentelemetry/api";
import { ContextManager } from "../../core/context.mjs";
import { formatOutput } from "../../formatters/index.mjs";

const tracer = trace.getTracer("unrdf-context-get");

export const getCommand = defineCommand({
  meta: {
    name: "get",
    description: "Get context details",
  },
  args: {
    name: {
      type: "positional",
      description: "Context name",
      required: true,
    },
    output: {
      type: "string",
      description: "Output format",
      default: "json",
    },
  },
  async run(ctx) {
    return await tracer.startActiveSpan("context.get", async (span) => {
      try {
        span.setAttributes({
          "context.name": ctx.args.name,
          "output.format": ctx.args.output,
        });

        const manager = new ContextManager();
        await manager.init();

        const context = manager.getContext(ctx.args.name);

        if (!context) {
          span.setAttributes({
            "context.found": false,
            "error.type": "context_not_found",
          });
          span.setStatus({
            code: SpanStatusCode.ERROR,
            message: "Context not found",
          });
          console.error(`Context not found: ${ctx.args.name}`);
          process.exit(1);
        }

        span.setAttributes({
          "context.found": true,
          "context.sidecar.endpoint": context.sidecar?.endpoint || "none",
          "output.format": ctx.args.output,
        });

        span.setStatus({ code: SpanStatusCode.OK });
        console.log(formatOutput(context, ctx.args.output));
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        console.error(`Failed to get context: ${error.message}`);
        process.exit(1);
      } finally {
        span.end();
      }
    });
  },
});
