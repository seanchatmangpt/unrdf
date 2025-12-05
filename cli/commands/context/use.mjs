/**
 * @file Context Use Command
 * @module cli-v2/commands/context
 *
 * @description
 * Switch to a different context with OpenTelemetry instrumentation
 */

import { defineCommand } from "citty";
import { trace, SpanStatusCode } from "@opentelemetry/api";
import { ContextManager } from "../../core/context.mjs";
import { getContextManager as getContextLockManager } from "../../utils/context-singleton.mjs";

const tracer = trace.getTracer("unrdf-context-use");

export const useCommand = defineCommand({
  meta: {
    name: "use",
    description: "Switch to a different context",
  },
  args: {
    name: {
      type: "positional",
      description: "Context name",
      required: true,
    },
  },
  async run(ctx) {
    return await tracer.startActiveSpan("context.use", async (span) => {
      // FM-CLI-007: Acquire context lock to prevent race conditions
      const lockManager = getContextLockManager();
      let lock;

      try {
        span.setAttribute("context.name", ctx.args.name);

        // Acquire lock before switching context
        lock = await lockManager.acquireLock(ctx.args.name);

        const manager = new ContextManager();
        await manager.init();

        await manager.useContext(ctx.args.name);

        span.setAttributes({
          "context.switched": true,
          "current.context": ctx.args.name,
        });

        span.setStatus({ code: SpanStatusCode.OK });
        console.log(`✅ Switched to context: ${ctx.args.name}`);
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        console.error(`Failed to switch context: ${error.message}`);
        process.exit(1);
      } finally {
        // Release lock
        if (lock) {
          lock.release();
        }
        span.end();
      }
    });
  },
});
