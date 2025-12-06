/**
 * @file Store Stats Command - REFACTORED to use domain layer
 * @architecture CLI → Domain Service → Package
 *
 * BEFORE (2-tier): STUB (mock data only)
 * AFTER (3-tier): Command → StoreService.getStats() → Package
 *
 * BENEFITS:
 * - Now shows REAL statistics from store
 * - Includes graph list and counts
 * - Testable business logic
 */

import { defineCommand } from 'citty';
import { getStoreService } from '../../domain/index.mjs';

export const statsCommand = defineCommand({
  meta: {
    name: 'stats',
    description: 'Show store statistics'
  },
  args: {
    'include-graphs': {
      type: 'boolean',
      description: 'Include graph list',
      default: true
    }
  },
  async run(ctx) {
    try {
      // DOMAIN LAYER: Get stats via service
      const service = getStoreService();
      const stats = await service.getStats({
        includeGraphs: ctx.args['include-graphs'],
        includeTriples: true
      });

      // PRESENTATION LAYER: Format and display
      console.log('📊 Store Statistics:');
      console.log(`  Total quads: ${stats.totalQuads.toLocaleString()}`);

      if (stats.graphs && stats.graphs.length > 0) {
        console.log(`  Named graphs: ${stats.graphCount}`);
        console.log('\n  Graphs:');
        stats.graphs.forEach(graph => {
          console.log(`    • ${graph}`);
        });
      }

      if (stats.defaultGraphQuads !== undefined) {
        console.log(`\n  Default graph quads: ${stats.defaultGraphQuads.toLocaleString()}`);
      }

    } catch (error) {
      console.error(`❌ Failed to get statistics: ${error.message}`);
      process.exit(1);
    }
  }
});
