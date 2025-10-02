/**
 * @file Context Create Command
 */

import { defineCommand } from 'citty';
import { ContextManager } from '../../core/context.mjs';

export const createCommand = defineCommand({
  meta: {
    name: 'create',
    description: 'Create a new context'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Context name',
      required: true
    },
    sidecar: {
      type: 'string',
      description: 'Sidecar endpoint',
      default: 'http://localhost:50051'
    }
  },
  async run(ctx) {
    const manager = new ContextManager();
    await manager.init();

    const config = {
      sidecar: {
        endpoint: ctx.args.sidecar
      }
    };

    await manager.createContext(ctx.args.name, config);
    console.log(`✅ Context created: ${ctx.args.name}`);
  }
});
