/**
 * @file Context List Command
 * @module cli-v2/commands/context
 *
 * @description
 * List all contexts with OpenTelemetry instrumentation
 */

import { defineCommand } from "citty";
import { trace, SpanStatusCode } from "@opentelemetry/api";
import { formatOutput } from "../../formatters/index.mjs";
import { ContextManager } from "../../core/context.mjs";

const tracer = trace.getTracer("unrdf-context-list");

export const listCommand = defineCommand({
  meta: {
    name: "list",
    description: "List all contexts",
  },
  args: {
    output: {
      type: "string",
      description: "Output format",
      default: "table",
    },
  },
  async run(ctx) {
    return await tracer.startActiveSpan("context.list", async (span) => {
      try {
        span.setAttribute("output.format", ctx.args.output);

        const manager = new ContextManager();
        await manager.init();

        const contexts = manager.listContexts();

        const formatted = contexts.map((c) => ({
          name: c.name,
          sidecar: c.sidecar?.endpoint || "N/A",
          current: c.current ? "*" : "",
        }));

        span.setAttributes({
          "context.count": contexts.length,
          "current.context": manager.currentContext || "none",
          "output.format": ctx.args.output,
        });

        span.setStatus({ code: SpanStatusCode.OK });
        console.log(
          formatOutput(formatted, ctx.args.output, {
            columns: ["name", "sidecar", "current"],
            headers: ["NAME", "SIDECAR", "CURRENT"],
          }),
        );
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        console.error(`Failed to list contexts: ${error.message}`);
        process.exit(1);
      } finally {
        span.end();
      }
    });
  },
});
