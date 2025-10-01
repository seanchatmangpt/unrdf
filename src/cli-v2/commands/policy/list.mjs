/**
 * @file Policy List Command
 */

import { defineCommand } from 'citty';
import { formatOutput } from '../../formatters/index.mjs';

export const listCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List all policy packs'
  },
  args: {
    output: {
      type: 'string',
      description: 'Output format',
      default: 'table'
    },
    active: {
      type: 'boolean',
      description: 'Show only active policy packs',
      default: false
    }
  },
  async run(ctx) {
    const policies = [
      { name: 'compliance', hooks: 5, active: true },
      { name: 'security', hooks: 8, active: false }
    ];

    const output = formatOutput(policies, ctx.args.output, {
      columns: ['name', 'hooks', 'active'],
      headers: ['NAME', 'HOOKS', 'ACTIVE']
    });

    console.log(output);
  }
});
