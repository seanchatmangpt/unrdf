/**
 * @file Context Delete Command
 * @module cli-v2/commands/context
 *
 * @description
 * Delete a context with OpenTelemetry instrumentation
 */

import { defineCommand } from "citty";
import { trace, SpanStatusCode } from "@opentelemetry/api";
import { ContextManager } from "../../core/context.mjs";

const tracer = trace.getTracer("unrdf-context-delete");

export const deleteCommand = defineCommand({
  meta: {
    name: "delete",
    description: "Delete a context",
  },
  args: {
    name: {
      type: "positional",
      description: "Context name",
      required: true,
    },
  },
  async run(ctx) {
    return await tracer.startActiveSpan("context.delete", async (span) => {
      try {
        span.setAttribute("context.name", ctx.args.name);

        const manager = new ContextManager();
        await manager.init();

        await manager.deleteContext(ctx.args.name);

        span.setAttributes({
          "context.deleted": true,
        });

        span.setStatus({ code: SpanStatusCode.OK });
        console.log(`✅ Context deleted: ${ctx.args.name}`);
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        console.error(`Failed to delete context: ${error.message}`);
        process.exit(1);
      } finally {
        span.end();
      }
    });
  },
});
