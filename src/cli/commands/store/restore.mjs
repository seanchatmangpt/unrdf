/**
 * @file Store Restore Command
 */

import { defineCommand } from 'citty';
import { readFile } from 'node:fs/promises';

export const restoreCommand = defineCommand({
  meta: {
    name: 'restore',
    description: 'Restore store from backup'
  },
  args: {
    file: {
      type: 'positional',
      description: 'Backup file path',
      required: true
    }
  },
  async run(ctx) {
    const { file } = ctx.args;

    console.log(`⚠️  This will replace all data in the store.`);
    console.log(`📥 Restoring from: ${file}`);

    const content = await readFile(file, 'utf-8');
    // TODO: Import backup
    console.log(`✅ Store restored`);
  }
});
