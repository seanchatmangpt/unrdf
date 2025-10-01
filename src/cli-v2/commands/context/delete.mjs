/**
 * @file Context Delete Command
 */

import { defineCommand } from 'citty';
import { ContextManager } from '../../core/context.mjs';

export const deleteCommand = defineCommand({
  meta: {
    name: 'delete',
    description: 'Delete a context'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Context name',
      required: true
    }
  },
  async run(ctx) {
    const manager = new ContextManager();
    await manager.init();

    try {
      await manager.deleteContext(ctx.args.name);
      console.log(`✅ Context deleted: ${ctx.args.name}`);
    } catch (error) {
      console.error(`Failed to delete context: ${error.message}`);
      process.exit(1);
    }
  }
});
