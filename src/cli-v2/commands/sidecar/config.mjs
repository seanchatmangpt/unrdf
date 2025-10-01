/**
 * @file Sidecar Config Command
 */

import { defineCommand } from 'citty';

export const configCommand = defineCommand({
  meta: {
    name: 'config',
    description: 'Manage sidecar configuration'
  },
  subCommands: {
    get: defineCommand({
      meta: {
        name: 'get',
        description: 'Get configuration value'
      },
      args: {
        key: {
          type: 'positional',
          description: 'Configuration key',
          required: true
        }
      },
      async run(ctx) {
        console.log(`${ctx.args.key}: value`);
      }
    }),
    set: defineCommand({
      meta: {
        name: 'set',
        description: 'Set configuration value'
      },
      args: {
        key: {
          type: 'positional',
          description: 'Configuration key',
          required: true
        },
        value: {
          type: 'positional',
          description: 'Configuration value',
          required: true
        }
      },
      async run(ctx) {
        console.log(`✅ Set ${ctx.args.key} = ${ctx.args.value}`);
      }
    })
  }
});
