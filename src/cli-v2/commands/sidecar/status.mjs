/**
 * @file Sidecar Status Command
 */

import { defineCommand } from 'citty';
import { formatOutput } from '../../formatters/index.mjs';

export const statusCommand = defineCommand({
  meta: {
    name: 'status',
    description: 'Get KGC sidecar status'
  },
  args: {
    output: {
      type: 'string',
      description: 'Output format',
      default: 'json'
    }
  },
  async run(ctx) {
    const status = {
      running: true,
      uptime: 3600,
      version: '2.0.0',
      endpoint: 'http://localhost:50051'
    };

    console.log(formatOutput(status, ctx.args.output));
  }
});
