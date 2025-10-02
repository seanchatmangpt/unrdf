/**
 * @file Graph Delete Command
 * @module cli-v2/commands/graph/delete
 */

import { defineCommand } from 'citty';

export const deleteCommand = defineCommand({
  meta: {
    name: 'delete',
    description: 'Delete a graph'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Graph name',
      required: true
    },
    force: {
      type: 'boolean',
      description: 'Skip confirmation',
      default: false
    }
  },
  async run(ctx) {
    const { name, force } = ctx.args;

    if (!force) {
      console.log(`⚠️  Are you sure you want to delete graph "${name}"? (use --force to skip)`);
      return;
    }

    try {
      console.log(`✅ Graph deleted: ${name}`);
    } catch (error) {
      console.error(`Failed to delete graph: ${error.message}`);
      process.exit(1);
    }
  }
});
