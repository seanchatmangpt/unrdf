/**
 * @file Policy Get Command
 */

import { defineCommand } from 'citty';

export const getCommand = defineCommand({
  meta: { name: 'get', description: 'Get policy pack details' },
  args: { name: { type: 'positional', required: true } },
  async run(ctx) {
    console.log(`Policy Pack: ${ctx.args.name}`);
  }
});
