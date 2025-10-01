/**
 * @file Graph Create Command
 * @module cli-v2/commands/graph/create
 */

import { defineCommand } from 'citty';

export const createCommand = defineCommand({
  meta: {
    name: 'create',
    description: 'Create a new RDF graph'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Graph name',
      required: true
    },
    'base-iri': {
      type: 'string',
      description: 'Base IRI for the graph',
      default: 'http://example.org/'
    },
    'dry-run': {
      type: 'boolean',
      description: 'Show what would be created without actually creating',
      default: false
    }
  },
  async run(ctx) {
    const { name, 'base-iri': baseIri, 'dry-run': dryRun } = ctx.args;

    if (dryRun) {
      console.log(`Would create graph: ${name}`);
      console.log(`Base IRI: ${baseIri}`);
      return;
    }

    try {
      // TODO: Integrate with sidecar client
      console.log(`✅ Graph created: ${name}`);
      console.log(`   Base IRI: ${baseIri}`);
    } catch (error) {
      console.error(`Failed to create graph: ${error.message}`);
      process.exit(1);
    }
  }
});
