/**
 * @file Context Current Command
 * @module cli-v2/commands/context
 *
 * @description
 * Show current context with OpenTelemetry instrumentation
 */

import { defineCommand } from "citty";
import { trace, SpanStatusCode } from "@opentelemetry/api";
import { ContextManager } from "../../core/context.mjs";

const tracer = trace.getTracer("unrdf-context-current");

export const currentCommand = defineCommand({
  meta: {
    name: "current",
    description: "Show current context",
  },
  async run() {
    return await tracer.startActiveSpan("context.current", async (span) => {
      try {
        const manager = new ContextManager();
        await manager.init();

        const current = manager.getCurrentContext();

        if (!current) {
          span.setAttributes({
            "current.context.found": false,
            "current.context": "none",
          });
          span.setStatus({ code: SpanStatusCode.OK });
          console.log("No current context set");
          return;
        }

        span.setAttributes({
          "current.context.found": true,
          "current.context": current.name,
          "current.context.sidecar.endpoint":
            current.sidecar?.endpoint || "none",
        });

        span.setStatus({ code: SpanStatusCode.OK });
        console.log(`Current context: ${current.name}`);
        console.log(`Sidecar: ${current.sidecar?.endpoint || "N/A"}`);
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        console.error(`Failed to get current context: ${error.message}`);
        process.exit(1);
      } finally {
        span.end();
      }
    });
  },
});
