/**
 * @fileoverview Use Context Command
 *
 * @description
 * CLI command to switch to a different context.
 *
 * @module cli/commands/context/use
 * @version 2.4.0
 * @license MIT
 */

import { defineCommand } from 'citty';
import { ContextManager } from '../../core/context.mjs';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf-cli-context-use', '2.4.0');

/**
 * Use context command
 */
export const useCommand = defineCommand({
  meta: {
    name: 'use',
    description: 'Switch to a context',
  },
  args: {
    name: {
      type: 'positional',
      description: 'Context name',
      required: true,
    },
  },
  async run(ctx) {
    return tracer.startActiveSpan('cli.context.use', async span => {
      try {
        const { name } = ctx.args;

        span.setAttribute('context.name', name);

        const manager = new ContextManager();
        await manager.init();

        await manager.useContext(name);

        console.log(`✅ Switched to context: ${name}`);

        span.setStatus({ code: 1 }); // OK
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        console.error(`Failed to switch context: ${error.message}`);
        process.exit(1);
      } finally {
        span.end();
      }
    });
  },
});
