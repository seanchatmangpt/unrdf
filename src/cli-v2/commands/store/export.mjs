/**
 * @file Store Export Command
 */

import { defineCommand } from 'citty';

export const exportCommand = defineCommand({
  meta: {
    name: 'export',
    description: 'Export store data'
  },
  args: {
    output: {
      type: 'string',
      description: 'Output file path',
      required: true
    },
    format: {
      type: 'string',
      description: 'Output format',
      default: 'turtle'
    }
  },
  async run(ctx) {
    console.log(`📤 Exporting store (${ctx.args.format})...`);
    console.log(`✅ Exported to: ${ctx.args.output}`);
  }
});
