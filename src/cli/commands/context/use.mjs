/**
 * @file Context Use Command
 */

import { defineCommand } from 'citty';
import { ContextManager } from '../../core/context.mjs';

export const useCommand = defineCommand({
  meta: {
    name: 'use',
    description: 'Switch to a different context'
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
      await manager.useContext(ctx.args.name);
      console.log(`✅ Switched to context: ${ctx.args.name}`);
    } catch (error) {
      console.error(`Failed to switch context: ${error.message}`);
      process.exit(1);
    }
  }
});
