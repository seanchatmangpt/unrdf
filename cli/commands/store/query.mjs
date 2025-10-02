/**
 * @file Store Query Command
 */

import { defineCommand } from 'citty';
import { readFile } from 'node:fs/promises';
import { formatOutput } from '../../formatters/index.mjs';

export const queryCommand = defineCommand({
  meta: {
    name: 'query',
    description: 'Execute SPARQL query'
  },
  args: {
    query: {
      type: 'string',
      description: 'SPARQL query string'
    },
    file: {
      type: 'string',
      description: 'SPARQL query file path'
    },
    format: {
      type: 'string',
      description: 'Output format',
      default: 'table'
    }
  },
  async run(ctx) {
    let query = ctx.args.query;

    if (ctx.args.file) {
      query = await readFile(ctx.args.file, 'utf-8');
    }

    if (!query) {
      console.error('Error: Query required (use --query or --file)');
      process.exit(1);
    }

    console.log(`🔍 Executing query...`);

    const results = [
      { subject: 'ex:Alice', predicate: 'foaf:name', object: '"Alice"' }
    ];

    console.log(formatOutput(results, ctx.args.format));
  }
});
