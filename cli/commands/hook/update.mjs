/**
 * @file Hook Update Command - TODO
 *
 * STATUS: Not yet implemented
 *
 * NOTE: Hook updates require design decision:
 * - Should we support in-place updates or require delete+create?
 * - What hook properties can be updated (enabled, handler, metadata)?
 * - How to handle running hooks during updates?
 */

import { defineCommand } from 'citty';

export const updateCommand = defineCommand({
  meta: {
    name: 'update',
    description: 'Update hook properties [TODO: Not yet implemented]'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Hook name or ID',
      required: true
    },
    '--enable': {
      type: 'boolean',
      description: 'Enable the hook'
    },
    '--disable': {
      type: 'boolean',
      description: 'Disable the hook'
    }
  },
  async run(ctx) {
    const { name } = ctx.args;

    console.error(`❌ Command not yet implemented: hook update`);
    console.error(`\nThis command requires design decisions about hook update semantics.`);
    console.error(`\nFor now, you can:`);
    console.error(`  • Delete the hook: unrdf hook delete ${name}`);
    console.error(`  • Create a new hook: unrdf hook create ${name} ...`);
    process.exit(1);
  }
});
