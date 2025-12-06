/**
 * @file Hook Get Command - THREE-TIER ARCHITECTURE
 * @architecture CLI → Domain Service → Package
 *
 * Alias for 'hook describe' - provides hook inspection
 */

import { defineCommand } from 'citty';
import { getHookService } from '../../domain/index.mjs';

export const getCommand = defineCommand({
  meta: {
    name: 'get',
    description: 'Get hook details (alias for describe)'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Hook name or ID',
      required: true
    }
  },
  async run(ctx) {
    try {
      const { name } = ctx.args;

      // DOMAIN LAYER: Get hook details via service
      const service = getHookService();
      const allHooks = await service.listHooks({});
      const hook = allHooks.hooks.find(h => h.id === name || h.name === name);

      if (!hook) {
        console.error(`❌ Hook not found: ${name}`);
        console.error(`\n📋 Available hooks:`);
        allHooks.hooks.forEach(h => {
          console.error(`   • ${h.id || h.name} (${h.trigger})`);
        });
        process.exit(1);
      }

      // PRESENTATION LAYER: Display hook information (same as describe)
      console.log(`\n🪝 Hook: ${hook.name}`);
      console.log(`${'═'.repeat(60)}`);
      console.log(`ID:            ${hook.id || 'N/A'}`);
      console.log(`Trigger:       ${hook.trigger}`);
      console.log(`Enabled:       ${hook.enabled ? '✅ Yes' : '❌ No'}`);
      console.log(`Policy:        ${hook.policy || 'default'}`);

      if (hook.meta) {
        console.log(`\n📋 Metadata:`);
        if (hook.meta.description) {
          console.log(`   Description: ${hook.meta.description}`);
        }
        if (hook.meta.createdAt) {
          console.log(`   Created:     ${new Date(hook.meta.createdAt).toLocaleString()}`);
        }
      }

      console.log('');

    } catch (error) {
      console.error(`❌ Failed to get hook: ${error.message}`);
      process.exit(1);
    }
  }
});
