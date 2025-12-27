/**
 * @fileoverview List Contexts Command
 *
 * @description
 * CLI command to list all available contexts.
 *
 * @module cli/commands/context/list
 * @version 2.4.0
 * @license MIT
 */

import { defineCommand } from 'citty';
import { ContextManager } from '../../core/context.mjs';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf-cli-context-list', '2.4.0');

/**
 * List contexts command
 */
export const listCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List all contexts',
  },
  args: {
    output: {
      type: 'string',
      description: 'Output format (json, table)',
      default: 'table',
      alias: 'o',
    },
  },
  async run(ctx) {
    return tracer.startActiveSpan('cli.context.list', async span => {
      try {
        const { output } = ctx.args;

        span.setAttribute('context.output', output);

        const manager = new ContextManager();
        await manager.init();

        const contexts = manager.listContexts();
        const current = manager.currentContext;

        span.setAttribute('context.count', contexts.length);

        if (contexts.length === 0) {
          console.log('No contexts found');
          span.setStatus({ code: 1 }); // OK
          return;
        }

        if (output === 'json') {
          console.log(JSON.stringify({ contexts, current }, null, 2));
        } else if (output === 'table') {
          console.log('Available contexts:');
          console.log('');
          contexts.forEach(context => {
            const isCurrent = context.name === current;
            const marker = isCurrent ? '* ' : '  ';
            console.log(`${marker}${context.name}`);
            console.log(`    Sidecar: ${context.sidecar.endpoint}`);
            console.log(`    Created: ${context.createdAt}`);
            if (context.updatedAt) {
              console.log(`    Updated: ${context.updatedAt}`);
            }
            console.log('');
          });

          if (current) {
            console.log(`Current context: ${current}`);
          } else {
            console.log('No current context set');
          }
        }

        span.setStatus({ code: 1 }); // OK
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        console.error(`Failed to list contexts: ${error.message}`);
        process.exit(1);
      } finally {
        span.end();
      }
    });
  },
});
