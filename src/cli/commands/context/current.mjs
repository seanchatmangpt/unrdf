/**
 * @fileoverview Current Context Command
 *
 * @description
 * CLI command to show the current active context.
 *
 * @module cli/commands/context/current
 * @version 2.4.0
 * @license MIT
 */

import { defineCommand } from 'citty';
import { ContextManager } from '../../core/context.mjs';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf-cli-context-current', '2.4.0');

/**
 * Current context command
 */
export const currentCommand = defineCommand({
  meta: {
    name: 'current',
    description: 'Show current context',
  },
  args: {},
  async run() {
    return tracer.startActiveSpan('cli.context.current', async (span) => {
      try {
        const manager = new ContextManager();
        await manager.init();

        const context = manager.getCurrentContext();

        if (!context) {
          console.log('No current context set');
          span.setAttribute('context.current', 'none');
          span.setStatus({ code: 1 }); // OK
          return;
        }

        span.setAttribute('context.current', context.name);

        console.log(`Current context: ${context.name}`);
        console.log(`Sidecar: ${context.sidecar.endpoint}`);
        console.log(`Created: ${context.createdAt}`);
        if (context.updatedAt) {
          console.log(`Updated: ${context.updatedAt}`);
        }

        span.setStatus({ code: 1 }); // OK
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        console.error(`Failed to get current context: ${error.message}`);
        process.exit(1);
      } finally {
        span.end();
      }
    });
  },
});
