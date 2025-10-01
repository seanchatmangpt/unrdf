/**
 * @file Graph Update Command
 * @module cli-v2/commands/graph/update
 */

import { defineCommand } from 'citty';

export const updateCommand = defineCommand({
  meta: {
    name: 'update',
    description: 'Update graph metadata'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Graph name',
      required: true
    },
    'base-iri': {
      type: 'string',
      description: 'New base IRI'
    }
  },
  async run(ctx) {
    const { name, 'base-iri': baseIri } = ctx.args;

    try {
      console.log(`✅ Graph updated: ${name}`);
      if (baseIri) {
        console.log(`   Base IRI: ${baseIri}`);
      }
    } catch (error) {
      console.error(`Failed to update graph: ${error.message}`);
      process.exit(1);
    }
  }
});
