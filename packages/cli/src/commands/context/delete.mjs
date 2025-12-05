/**
 * @fileoverview Delete Context Command
 *
 * @description
 * CLI command to delete a context.
 *
 * @module cli/commands/context/delete
 * @version 2.4.0
 * @license MIT
 */

import { defineCommand } from 'citty';
import { ContextManager } from '../../core/context.mjs';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf-cli-context-delete', '2.4.0');

/**
 * Delete context command
 */
export const deleteCommand = defineCommand({
  meta: {
    name: 'delete',
    description: 'Delete a context',
  },
  args: {
    name: {
      type: 'positional',
      description: 'Context name',
      required: true,
    },
  },
  async run(ctx) {
    return tracer.startActiveSpan('cli.context.delete', async span => {
      try {
        const { name } = ctx.args;

        span.setAttribute('context.name', name);

        const manager = new ContextManager();
        await manager.init();

        await manager.deleteContext(name);

        console.log(`✅ Context deleted: ${name}`);

        span.setStatus({ code: 1 }); // OK
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        console.error(`Failed to delete context: ${error.message}`);
        process.exit(1);
      } finally {
        span.end();
      }
    });
  },
});
