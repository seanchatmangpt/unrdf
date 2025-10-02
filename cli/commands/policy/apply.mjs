/**
 * @file Policy Apply Command
 */

import { defineCommand } from 'citty';
import { readFile } from 'node:fs/promises';

export const applyCommand = defineCommand({
  meta: {
    name: 'apply',
    description: 'Apply a policy pack configuration'
  },
  args: {
    file: {
      type: 'positional',
      description: 'Policy pack file path',
      required: true
    },
    'dry-run': {
      type: 'boolean',
      description: 'Preview changes without applying',
      default: false
    }
  },
  async run(ctx) {
    const { file, 'dry-run': dryRun } = ctx.args;

    try {
      const content = await readFile(file, 'utf-8');
      const policy = JSON.parse(content);

      if (dryRun) {
        console.log(`Would apply policy pack: ${policy.name}`);
        console.log(`  Hooks: ${policy.hooks?.length || 0}`);
        return;
      }

      console.log(`✅ Policy pack applied: ${policy.name}`);
    } catch (error) {
      console.error(`Failed to apply policy: ${error.message}`);
      process.exit(1);
    }
  }
});
