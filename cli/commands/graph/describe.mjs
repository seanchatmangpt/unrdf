/**
 * @file Graph Describe Command - THREE-TIER ARCHITECTURE
 * @architecture CLI → Domain Service → Package
 *
 * Provides full access to graph inspection capabilities
 */

import { defineCommand } from 'citty';
import { getGraphService } from '../../domain/index.mjs';

export const describeCommand = defineCommand({
  meta: {
    name: 'describe',
    description: 'Show detailed information about a graph'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Graph name (URI)',
      required: true
    },
    'show-sample': {
      type: 'boolean',
      description: 'Show sample quads from the graph',
      default: false
    },
    'sample-size': {
      type: 'string',
      description: 'Number of sample quads to show',
      default: '5'
    }
  },
  async run(ctx) {
    try {
      const { name } = ctx.args;

      // DOMAIN LAYER: Get graph statistics via service
      const service = getGraphService();

      // Check if graph exists
      const exists = await service.graphExists(name);
      if (!exists) {
        const allGraphs = await service.listGraphs({});
        console.error(`❌ Graph not found: ${name}`);
        if (allGraphs.graphs.length > 0) {
          console.error(`\n📋 Available graphs:`);
          allGraphs.graphs.forEach(g => {
            console.error(`   • ${g.name} (${g.type})`);
          });
        } else {
          console.error(`\nNo graphs found in store.`);
        }
        process.exit(1);
      }

      // Get detailed statistics
      const stats = await service.getGraphStats(name);

      // PRESENTATION LAYER: Display detailed graph information
      console.log(`\n📊 Graph: ${name}`);
      console.log(`${'═'.repeat(60)}`);
      console.log(`Type:          Named Graph`);
      console.log(`Quad Count:    ${stats.quadCount.toLocaleString()}`);
      console.log(`Subject Count: ${stats.subjectCount.toLocaleString()}`);

      // Show sample quads if requested
      if (ctx.args['show-sample'] && stats.quadCount > 0) {
        const sampleSize = parseInt(ctx.args['sample-size'], 10);
        const { getStoreService } = await import('../../domain/index.mjs');
        const storeService = getStoreService();

        const sampleQuery = `SELECT ?s ?p ?o WHERE { GRAPH <${name}> { ?s ?p ?o } } LIMIT ${sampleSize}`;
        const result = await storeService.executeQuery({ query: sampleQuery });

        console.log(`\n📋 Sample Quads (${Math.min(sampleSize, result.metadata.rowCount)}):`);
        result.data.forEach((row, idx) => {
          console.log(`   ${idx + 1}. <${row.s.value}> <${row.p.value}> ${row.o.termType === 'Literal' ? `"${row.o.value}"` : `<${row.o.value}>`}`);
        });
      }

      console.log('');

    } catch (error) {
      console.error(`❌ Failed to describe graph: ${error.message}`);
      process.exit(1);
    }
  }
});
