/**
 * @file Graph Get Command
 * @module cli-v2/commands/graph/get
 */

import { defineCommand } from 'citty';
import { formatOutput } from '../../formatters/index.mjs';

export const getCommand = defineCommand({
  meta: {
    name: 'get',
    description: 'Get details of a specific graph'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Graph name',
      required: true
    },
    output: {
      type: 'string',
      description: 'Output format',
      default: 'json'
    }
  },
  async run(ctx) {
    const { name, output } = ctx.args;

    try {
      // TODO: Fetch graph details from sidecar
      const graph = {
        name,
        baseIri: 'http://example.org/',
        triples: 1234,
        created: '2025-10-01T08:00:00Z',
        updated: '2025-10-01T10:00:00Z'
      };

      console.log(formatOutput(graph, output));
    } catch (error) {
      console.error(`Failed to get graph: ${error.message}`);
      process.exit(1);
    }
  }
});
