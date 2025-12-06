/**
 * @file Hook Create Command - THREE-TIER ARCHITECTURE
 * @architecture CLI → Domain Service → Package
 *
 * Provides full access to hook registration capabilities
 */

import { defineCommand } from 'citty';
import { readFile } from 'node:fs/promises';
import { getHookService } from '../../domain/index.mjs';

export const createCommand = defineCommand({
  meta: {
    name: 'create',
    description: 'Register a new knowledge hook'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Hook name',
      required: true
    },
    trigger: {
      type: 'string',
      description: 'Hook trigger event (before-add, after-add, before-delete, etc.)',
      required: true
    },
    handler: {
      type: 'string',
      description: 'Path to handler function file (JavaScript)'
    },
    policy: {
      type: 'string',
      description: 'Policy name to associate hook with',
      default: 'default'
    },
    description: {
      type: 'string',
      description: 'Hook description'
    },
    enabled: {
      type: 'boolean',
      description: 'Enable hook immediately',
      default: true
    }
  },
  async run(ctx) {
    try {
      // PRESENTATION LAYER: Parse CLI arguments
      const { name, trigger, handler, policy, description, enabled } = ctx.args;

      // Load handler function if file provided
      let handlerFn;
      if (handler) {
        const handlerCode = await readFile(handler, 'utf-8');
        // Create function from code (simplified - production would use safer eval)
        handlerFn = new Function('event', 'context', handlerCode);
      } else {
        // Default handler
        handlerFn = async (event) => {
          console.log(`Hook ${name} executed on trigger: ${trigger}`);
          return { success: true };
        };
      }

      // DOMAIN LAYER: Register hook via service
      const service = getHookService();
      const result = await service.registerHook({
        name,
        trigger,
        handler: handlerFn,
        meta: {
          policy,
          description,
          createdAt: new Date().toISOString()
        },
        enabled
      });

      // PRESENTATION LAYER: Display results
      console.log(`✅ Hook registered: ${result.name}`);
      console.log(`   Trigger: ${trigger}`);
      console.log(`   Policy: ${policy}`);
      console.log(`   Enabled: ${enabled ? 'Yes' : 'No'}`);
      if (description) {
        console.log(`   Description: ${description}`);
      }

    } catch (error) {
      console.error(`❌ Failed to register hook: ${error.message}`);
      process.exit(1);
    }
  }
});
