/**
 * @file Graph List Command - THREE-TIER ARCHITECTURE
 * @architecture CLI → Domain Service → Package
 *
 * Provides full access to graph listing and statistics
 */

import { defineCommand } from 'citty';
import { formatOutput } from '../../formatters/index.mjs';
import { getGraphService } from '../../domain/index.mjs';

export const listCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List all graphs in the store'
  },
  args: {
    output: {
      type: 'string',
      description: 'Output format (json, table)',
      default: 'table'
    },
    'include-stats': {
      type: 'boolean',
      description: 'Include statistics for each graph',
      default: false
    },
    'sort-by': {
      type: 'string',
      description: 'Sort by (name, size)',
      default: 'name'
    }
  },
  async run(ctx) {
    try {
      // DOMAIN LAYER: Get graphs via service
      const service = getGraphService();
      const result = await service.listGraphs({
        includeStats: ctx.args['include-stats'],
        sortBy: ctx.args['sort-by']
      });

      // PRESENTATION LAYER: Format and display
      if (result.graphs.length === 0) {
        console.log('ℹ️  No graphs found in store');
        return;
      }

      const formatted = result.graphs.map(g => ({
        name: g.name,
        type: g.type,
        ...(g.stats && {
          quads: g.stats.quadCount,
          subjects: g.stats.subjectCount
        })
      }));

      const columns = ctx.args['include-stats']
        ? ['name', 'type', 'quads', 'subjects']
        : ['name', 'type'];

      const headers = ctx.args['include-stats']
        ? ['NAME', 'TYPE', 'QUADS', 'SUBJECTS']
        : ['NAME', 'TYPE'];

      const output = formatOutput(formatted, ctx.args.output, {
        columns,
        headers
      });

      console.log(output);
      console.log(`\n📊 Total graphs: ${result.metadata.totalGraphs} (${result.metadata.totalQuads} quads)`);

    } catch (error) {
      console.error(`❌ Failed to list graphs: ${error.message}`);
      process.exit(1);
    }
  }
});
