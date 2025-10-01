/**
 * @file Hook Get Command
 */

import { defineCommand } from 'citty';

export const getCommand = defineCommand({
  meta: { name: 'get', description: 'Get hook details' },
  args: { name: { type: 'positional', required: true } },
  async run(ctx) {
    console.log(`Hook: ${ctx.args.name}`);
  }
});
