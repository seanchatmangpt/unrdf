/**
 * @file Hook List Command
 * @module cli-v2/commands/hook/list
 */

import { defineCommand } from 'citty';
import { formatOutput } from '../../formatters/index.mjs';
import { KnowledgeHookManager } from '../../../src/knowledge-engine/knowledge-hook-manager.mjs';

export const listCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List all knowledge hooks'
  },
  args: {
    output: {
      type: 'string',
      description: 'Output format (json, yaml, table)',
      default: 'table'
    },
    policy: {
      type: 'string',
      description: 'Filter by policy pack'
    }
  },
  async run(ctx) {
    try {
      const manager = new KnowledgeHookManager();
      const hooks = manager.getKnowledgeHooks();

      const formatted = hooks.map(hook => ({
        name: hook.meta.name,
        type: hook.when.kind,
        policy: hook.meta.policy || 'default'
      }));

      const output = formatOutput(formatted, ctx.args.output, {
        columns: ['name', 'type', 'policy'],
        headers: ['NAME', 'TYPE', 'POLICY']
      });

      console.log(output);
    } catch (error) {
      console.error(`Failed to list hooks: ${error.message}`);
      process.exit(1);
    }
  }
});
