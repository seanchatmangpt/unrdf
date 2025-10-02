/**
 * @file Policy Describe Command
 */

import { defineCommand } from 'citty';

export const describeCommand = defineCommand({
  meta: { name: 'describe', description: 'Describe policy pack' },
  args: { name: { type: 'positional', required: true } },
  async run(ctx) {
    console.log(`Policy Pack: ${ctx.args.name}`);
    console.log(`Hooks: 5`);
    console.log(`Active: true`);
  }
});
