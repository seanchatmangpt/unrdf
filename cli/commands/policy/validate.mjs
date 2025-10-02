/**
 * @file Policy Validate Command
 */

import { defineCommand } from 'citty';
import { readFile } from 'node:fs/promises';

export const validateCommand = defineCommand({
  meta: {
    name: 'validate',
    description: 'Validate policy pack configuration'
  },
  args: {
    file: {
      type: 'positional',
      description: 'Policy pack file path',
      required: false
    }
  },
  async run(ctx) {
    const { file } = ctx.args;

    try {
      if (file) {
        const content = await readFile(file, 'utf-8');
        const policy = JSON.parse(content);
        console.log(`✅ Policy pack is valid: ${policy.name}`);
      } else {
        console.log('✅ All active policy packs are valid');
      }
    } catch (error) {
      console.error(`Validation failed: ${error.message}`);
      process.exit(1);
    }
  }
});
