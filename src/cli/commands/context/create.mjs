/**
 * @fileoverview Create Context Command
 *
 * @description
 * CLI command to create a new context with sidecar configuration.
 *
 * @module cli/commands/context/create
 * @version 2.4.0
 * @license MIT
 */

import { defineCommand } from 'citty';
import { ContextManager } from '../../core/context.mjs';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf-cli-context-create', '2.4.0');

/**
 * Create context command
 */
export const createCommand = defineCommand({
  meta: {
    name: 'create',
    description: 'Create a new context',
  },
  args: {
    name: {
      type: 'positional',
      description: 'Context name',
      required: true,
    },
    sidecar: {
      type: 'string',
      description: 'Sidecar endpoint URL',
      required: true,
      alias: 's',
    },
  },
  async run(ctx) {
    return tracer.startActiveSpan('cli.context.create', async span => {
      try {
        const { name, sidecar } = ctx.args;

        span.setAttribute('context.name', name);
        span.setAttribute('context.sidecar', sidecar);

        const manager = new ContextManager();
        await manager.init();

        const config = {
          sidecar: {
            endpoint: sidecar,
          },
        };

        await manager.createContext(name, config);

        console.log(`✅ Context created: ${name}`);

        span.setStatus({ code: 1 }); // OK
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        console.error(`Failed to create context: ${error.message}`);
        process.exit(1);
      } finally {
        span.end();
      }
    });
  },
});
