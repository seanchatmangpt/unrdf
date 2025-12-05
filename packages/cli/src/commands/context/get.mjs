/**
 * @fileoverview Get Context Command
 *
 * @description
 * CLI command to get details of a specific context.
 *
 * @module cli/commands/context/get
 * @version 2.4.0
 * @license MIT
 */

import { defineCommand } from 'citty';
import { ContextManager } from '../../core/context.mjs';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf-cli-context-get', '2.4.0');

/**
 * Get context command
 */
export const getCommand = defineCommand({
  meta: {
    name: 'get',
    description: 'Get context details',
  },
  args: {
    name: {
      type: 'positional',
      description: 'Context name',
      required: true,
    },
    output: {
      type: 'string',
      description: 'Output format (json, yaml, table)',
      default: 'json',
      alias: 'o',
    },
  },
  async run(ctx) {
    return tracer.startActiveSpan('cli.context.get', async span => {
      try {
        const { name, output } = ctx.args;

        span.setAttribute('context.name', name);
        span.setAttribute('context.output', output);

        const manager = new ContextManager();
        await manager.init();

        const context = manager.getContext(name);

        if (!context) {
          console.error(`Context not found: ${name}`);
          process.exit(1);
          return;
        }

        if (output === 'json') {
          console.log(JSON.stringify(context, null, 2));
        } else if (output === 'table') {
          console.log(`Name: ${context.name}`);
          console.log(`Sidecar: ${context.sidecar.endpoint}`);
          console.log(`Created: ${context.createdAt}`);
          if (context.updatedAt) {
            console.log(`Updated: ${context.updatedAt}`);
          }
        } else {
          // Default to JSON
          console.log(JSON.stringify(context, null, 2));
        }

        span.setStatus({ code: 1 }); // OK
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        console.error(`Failed to get context: ${error.message}`);
        process.exit(1);
      } finally {
        span.end();
      }
    });
  },
});
