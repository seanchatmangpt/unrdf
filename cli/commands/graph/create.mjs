/**
 * @file Graph Create Command - THREE-TIER ARCHITECTURE
 * @architecture CLI → Domain Service → Package
 *
 * Provides full access to graph creation capabilities
 */

import { defineCommand } from 'citty';
import { getGraphService } from '../../domain/index.mjs';

export const createCommand = defineCommand({
  meta: {
    name: 'create',
    description: 'Create a new named graph'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Graph name (URI)',
      required: true
    },
    'add-metadata': {
      type: 'boolean',
      description: 'Add creation metadata',
      default: false
    }
  },
  async run(ctx) {
    try {
      const { name } = ctx.args;

      // PRESENTATION LAYER: Prepare metadata
      const metadata = ctx.args['add-metadata']
        ? {
            created: new Date().toISOString(),
            creator: 'unrdf-cli'
          }
        : undefined;

      // DOMAIN LAYER: Create graph via service
      const service = getGraphService();
      const result = await service.createGraph({
        name,
        metadata
      });

      // PRESENTATION LAYER: Display results
      console.log(`✅ Graph created: ${result.graph}`);
      if (metadata) {
        console.log(`   Created: ${metadata.created}`);
      }

    } catch (error) {
      console.error(`❌ Failed to create graph: ${error.message}`);
      process.exit(1);
    }
  }
});
