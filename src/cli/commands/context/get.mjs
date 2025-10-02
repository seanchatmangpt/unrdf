/**
 * @file Context Get Command
 */

import { defineCommand } from 'citty';
import { ContextManager } from '../../core/context.mjs';
import { formatOutput } from '../../formatters/index.mjs';

export const getCommand = defineCommand({
  meta: {
    name: 'get',
    description: 'Get context details'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Context name',
      required: true
    },
    output: {
      type: 'string',
      description: 'Output format',
      default: 'json'
    }
  },
  async run(ctx) {
    const manager = new ContextManager();
    await manager.init();

    const context = manager.getContext(ctx.args.name);
    if (!context) {
      console.error(`Context not found: ${ctx.args.name}`);
      process.exit(1);
    }

    console.log(formatOutput(context, ctx.args.output));
  }
});
