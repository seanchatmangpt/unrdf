/**
 * @file Hook Describe Command
 */

import { defineCommand } from 'citty';

export const describeCommand = defineCommand({
  meta: { name: 'describe', description: 'Describe hook' },
  args: { name: { type: 'positional', required: true } },
  async run(ctx) {
    console.log(`Hook: ${ctx.args.name}`);
    console.log(`Type: sparql-ask`);
    console.log(`Policy: compliance`);
  }
});
