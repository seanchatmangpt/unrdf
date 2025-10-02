/**
 * @file Context Current Command
 */

import { defineCommand } from 'citty';
import { ContextManager } from '../../core/context.mjs';

export const currentCommand = defineCommand({
  meta: {
    name: 'current',
    description: 'Show current context'
  },
  async run() {
    const manager = new ContextManager();
    await manager.init();

    const current = manager.getCurrentContext();
    if (!current) {
      console.log('No current context set');
    } else {
      console.log(`Current context: ${current.name}`);
      console.log(`Sidecar: ${current.sidecar?.endpoint || 'N/A'}`);
    }
  }
});
