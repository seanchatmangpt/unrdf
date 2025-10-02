/**
 * @file Hook Update Command
 */

import { defineCommand } from 'citty';

export const updateCommand = defineCommand({
  meta: { name: 'update', description: 'Update hook' },
  args: { name: { type: 'positional', required: true } },
  async run(ctx) {
    console.log(`✅ Hook updated: ${ctx.args.name}`);
  }
});
