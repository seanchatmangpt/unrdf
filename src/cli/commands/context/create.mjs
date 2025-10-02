/**
 * @file Context Create Command
 * @module cli-v2/commands/context
 *
 * @description
 * Create a new context with OpenTelemetry instrumentation
 */

import { defineCommand } from "citty";
import { trace, SpanStatusCode } from "@opentelemetry/api";
import { ContextManager } from "../../core/context.mjs";

const tracer = trace.getTracer("unrdf-context-create");

export const createCommand = defineCommand({
  meta: {
    name: "create",
    description: "Create a new context",
  },
  args: {
    name: {
      type: "positional",
      description: "Context name",
      required: true,
    },
    sidecar: {
      type: "string",
      description: "Sidecar endpoint",
      default: "http://localhost:50051",
    },
  },
  async run(ctx) {
    return await tracer.startActiveSpan("context.create", async (span) => {
      try {
        span.setAttributes({
          "context.name": ctx.args.name,
          "context.sidecar.endpoint": ctx.args.sidecar,
        });

        const manager = new ContextManager();
        await manager.init();

        const config = {
          sidecar: {
            endpoint: ctx.args.sidecar,
          },
        };

        await manager.createContext(ctx.args.name, config);

        span.setAttributes({
          "context.created": true,
          "context.sidecar.endpoint": ctx.args.sidecar,
        });

        span.setStatus({ code: SpanStatusCode.OK });
        console.log(`✅ Context created: ${ctx.args.name}`);
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        console.error(`Failed to create context: ${error.message}`);
        process.exit(1);
      } finally {
        span.end();
      }
    });
  },
});
