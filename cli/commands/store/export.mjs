/**
 * @file Store Export Command
 */

import { defineCommand } from 'citty';
import { writeFile } from 'node:fs/promises';
import { dirname } from 'node:path';
import { mkdir } from 'node:fs/promises';

export const exportCommand = defineCommand({
  meta: {
    name: 'export',
    description: 'Export store data'
  },
  args: {
    output: {
      type: 'string',
      description: 'Output file path',
      required: true
    },
    format: {
      type: 'string',
      description: 'Output format (turtle, ntriples, nquads, jsonld, rdfxml)',
      default: 'turtle'
    },
    graph: {
      type: 'string',
      description: 'Graph to export (exports all graphs if not specified)'
    }
  },
  async run(ctx) {
    try {
      const { output, format, graph } = ctx.args;

      console.log(`📤 Exporting store (${format})...`);

      // Get store instance
      const { getStore } = await import('../../utils/store-instance.mjs');
      const store = getStore();

      // Map format names to Oxigraph format strings
      const formatMap = {
        'turtle': 'text/turtle',
        'ntriples': 'application/n-triples',
        'nquads': 'application/n-quads',
        'jsonld': 'application/ld+json',
        'rdfxml': 'application/rdf+xml'
      };

      const oxigraphFormat = formatMap[format] || format;

      // Export data
      const data = store.dump({
        format: oxigraphFormat,
        from_graph: graph ? { value: graph, termType: 'NamedNode' } : undefined
      });

      // Ensure output directory exists
      const outputDir = dirname(output);
      await mkdir(outputDir, { recursive: true });

      // Write to file
      await writeFile(output, data, 'utf-8');

      const quadCount = store.size();
      console.log(`✅ Exported ${quadCount} quads to: ${output}`);
    } catch (error) {
      console.error(`❌ Export failed: ${error.message}`);
      process.exit(1);
    }
  }
});
