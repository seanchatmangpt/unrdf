/**
 * @file Graph List Command
 * @module cli-v2/commands/graph/list
 */

import { defineCommand } from 'citty';
import { formatOutput } from '../../formatters/index.mjs';

export const listCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List all RDF graphs'
  },
  args: {
    output: {
      type: 'string',
      description: 'Output format (json, yaml, table, tree)',
      default: 'table'
    },
    namespace: {
      type: 'string',
      description: 'Filter by namespace'
    }
  },
  async run(ctx) {
    try {
      // TODO: Integrate with sidecar client to fetch graphs
      const graphs = [
        { name: 'default', triples: 1234, updated: '2025-10-01T10:00:00Z' },
        { name: 'schema', triples: 567, updated: '2025-10-01T09:30:00Z' }
      ];

      const output = formatOutput(graphs, ctx.args.output, {
        columns: ['name', 'triples', 'updated'],
        headers: ['NAME', 'TRIPLES', 'UPDATED']
      });

      console.log(output);
    } catch (error) {
      console.error(`Failed to list graphs: ${error.message}`);
      process.exit(1);
    }
  }
});
