/**
 * @file Hook History Command
 * @module cli-v2/commands/hook/history
 */

import { defineCommand } from 'citty';
import { formatOutput } from '../../formatters/index.mjs';

export const historyCommand = defineCommand({
  meta: {
    name: 'history',
    description: 'Show evaluation history for a hook'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Hook name',
      required: true
    },
    limit: {
      type: 'string',
      description: 'Maximum number of records',
      default: '10'
    },
    output: {
      type: 'string',
      description: 'Output format',
      default: 'table'
    }
  },
  async run(ctx) {
    const { name, limit, output } = ctx.args;

    const history = [
      { timestamp: '2025-10-01T10:00:00Z', fired: true, duration: 123 },
      { timestamp: '2025-10-01T09:00:00Z', fired: false, duration: 45 }
    ];

    console.log(`📋 Evaluation History for ${name}:`);
    console.log(formatOutput(history, output, {
      columns: ['timestamp', 'fired', 'duration'],
      headers: ['TIMESTAMP', 'FIRED', 'DURATION (ms)']
    }));
  }
});
