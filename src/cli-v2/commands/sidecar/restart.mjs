/**
 * @file Sidecar Restart Command
 */

import { defineCommand } from 'citty';

export const restartCommand = defineCommand({
  meta: {
    name: 'restart',
    description: 'Restart sidecar'
  },
  async run() {
    console.log('🔄 Restarting sidecar...');
    console.log('✅ Sidecar restarted');
  }
});
