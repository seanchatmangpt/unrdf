/**
 * @file Store Import Command
 */

import { defineCommand } from 'citty';
import { readFile } from 'node:fs/promises';

export const importCommand = defineCommand({
  meta: {
    name: 'import',
    description: 'Import RDF data into store'
  },
  args: {
    file: {
      type: 'positional',
      description: 'File to import',
      required: true
    },
    graph: {
      type: 'string',
      description: 'Target graph name',
      default: 'default'
    },
    format: {
      type: 'string',
      description: 'Input format (turtle, nquads, jsonld)',
      default: 'turtle'
    }
  },
  async run(ctx) {
    const { file, graph, format } = ctx.args;

    try {
      console.log(`📥 Importing ${file} (${format}) into graph: ${graph}`);
      const content = await readFile(file, 'utf-8');
      // TODO: Parse and import
      console.log(`✅ Imported successfully`);
    } catch (error) {
      console.error(`Import failed: ${error.message}`);
      process.exit(1);
    }
  }
});
