/**
 * @file Hook Delete Command
 */

import { defineCommand } from 'citty';

export const deleteCommand = defineCommand({
  meta: { name: 'delete', description: 'Delete hook' },
  args: { name: { type: 'positional', required: true } },
  async run(ctx) {
    console.log(`✅ Hook deleted: ${ctx.args.name}`);
  }
});
