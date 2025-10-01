/**
 * @file Context List Command
 */

import { defineCommand } from 'citty';
import { formatOutput } from '../../formatters/index.mjs';
import { ContextManager } from '../../core/context.mjs';

export const listCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List all contexts'
  },
  args: {
    output: {
      type: 'string',
      description: 'Output format',
      default: 'table'
    }
  },
  async run(ctx) {
    const manager = new ContextManager();
    await manager.init();

    const contexts = manager.listContexts();

    const formatted = contexts.map(c => ({
      name: c.name,
      sidecar: c.sidecar?.endpoint || 'N/A',
      current: c.current ? '*' : ''
    }));

    console.log(formatOutput(formatted, ctx.args.output, {
      columns: ['name', 'sidecar', 'current'],
      headers: ['NAME', 'SIDECAR', 'CURRENT']
    }));
  }
});
