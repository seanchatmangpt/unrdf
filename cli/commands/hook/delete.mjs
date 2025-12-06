/**
 * @file Hook Delete Command - THREE-TIER ARCHITECTURE
 * @architecture CLI → Domain Service → Package
 *
 * Provides full access to hook unregistration capabilities
 */

import { defineCommand } from 'citty';
import { executeWithConfirmation, shouldSkipConfirmation } from '../../utils/confirmation.mjs';
import { getHookService } from '../../domain/index.mjs';

export const deleteCommand = defineCommand({
  meta: {
    name: 'delete',
    description: 'Unregister a hook'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Hook name or ID',
      required: true
    },
    force: {
      type: 'boolean',
      description: 'Skip confirmation',
      default: false
    }
  },
  async run(ctx) {
    try {
      const { name, force } = ctx.args;

      // DOMAIN LAYER: Get hook info for confirmation
      const service = getHookService();
      const allHooks = await service.listHooks({});
      const hook = allHooks.hooks.find(h => h.id === name || h.name === name);

      if (!hook) {
        console.error(`❌ Hook not found: ${name}`);
        console.error(`\n📋 Available hooks:`);
        allHooks.hooks.forEach(h => {
          console.error(`   • ${h.id || h.name} (${h.trigger}) - ${h.enabled ? 'enabled' : 'disabled'}`);
        });
        process.exit(1);
      }

      // PRESENTATION LAYER: Confirmation dialog
      if (!shouldSkipConfirmation(ctx) && !force) {
        await executeWithConfirmation(
          ctx,
          {
            action: 'delete',
            resource: 'hook',
            name: hook.name,
            summary: `Unregister hook "${hook.name}" (${hook.trigger})`
          },
          async () => {
            // DOMAIN LAYER: Unregister hook
            const result = await service.unregisterHook(hook.id || hook.name);
            console.log(`✅ Hook unregistered: ${hook.name}`);
            console.log(`   Previous state: ${result.wasEnabled ? 'enabled' : 'disabled'}`);
          }
        );
      } else {
        // DOMAIN LAYER: Unregister hook without confirmation
        const result = await service.unregisterHook(hook.id || hook.name);
        console.log(`✅ Hook unregistered: ${hook.name}`);
        console.log(`   Previous state: ${result.wasEnabled ? 'enabled' : 'disabled'}`);
      }

    } catch (error) {
      console.error(`❌ Failed to unregister hook: ${error.message}`);
      process.exit(1);
    }
  }
});
