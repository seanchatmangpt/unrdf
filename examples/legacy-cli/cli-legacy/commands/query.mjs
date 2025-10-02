/**
 * @file Query Command
 * @module cli/commands/query
 *
 * @description
 * Execute SPARQL queries against RDF data with multiple output formats.
 */

import { readFile, writeFile } from 'node:fs/promises';
import { useStoreContext } from '../../context/index.mjs';
import { useGraph, useTurtle } from '../../composables/index.mjs';
import { QueryError } from '../utils/error-handler.mjs';
import { validateRequiredArgs, getArg } from '../utils/context-wrapper.mjs';

/**
 * Format query results as table
 * @param {Array} results - Query results
 * @returns {string} Formatted table
 */
function formatTable(results) {
  if (results.length === 0) {
    return 'No results found.';
  }

  const headers = Object.keys(results[0]);
  const colWidths = headers.map(h =>
    Math.max(h.length, ...results.map(r => String(r[h] || '').length))
  );

  const headerRow = headers.map((h, i) => h.padEnd(colWidths[i])).join(' | ');
  const separator = headers.map((_, i) => '-'.repeat(colWidths[i])).join('-+-');
  const dataRows = results.map(row =>
    headers.map((h, i) => String(row[h] || '').padEnd(colWidths[i])).join(' | ')
  ).join('\n');

  return `${headerRow}\n${separator}\n${dataRows}`;
}

/**
 * Format query results as CSV
 * @param {Array} results - Query results
 * @returns {string} CSV formatted results
 */
function formatCSV(results) {
  if (results.length === 0) {
    return '';
  }

  const headers = Object.keys(results[0]);
  const headerRow = headers.join(',');
  const dataRows = results.map(row =>
    headers.map(h => `"${String(row[h] || '').replace(/"/g, '""')}"`).join(',')
  ).join('\n');

  return `${headerRow}\n${dataRows}`;
}

/**
 * Execute SPARQL query
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function queryCommand(ctx, config) {
  const { args } = ctx;

  // Validate required arguments
  validateRequiredArgs(args, ['input']);

  console.log('🔍 Executing SPARQL query...');

  const store = useStoreContext();
  const graph = useGraph();
  const turtle = await useTurtle();

  // Load data
  const inputData = await readFile(args.input, 'utf-8');
  const quads = await turtle.parse(inputData);
  store.add(...quads);

  // Get query
  let query;
  if (args.query) {
    query = args.query;
  } else if (args['query-file']) {
    query = await readFile(args['query-file'], 'utf-8');
  } else {
    throw new QueryError('Query required: use --query or --query-file');
  }

  // Execute query
  const results = await graph.select(query);

  // Format output
  const format = getArg(args, 'format', 'table');
  let output;

  switch (format) {
    case 'json':
      output = JSON.stringify(results, null, 2);
      break;
    case 'csv':
      output = formatCSV(results);
      break;
    case 'table':
    default:
      output = formatTable(results);
      break;
  }

  console.log(output);

  // Write output if requested
  if (args.output) {
    await writeFile(args.output, output);
    console.log(`📄 Results written to ${args.output}`);
  }
}

/**
 * Export command definition for testing
 */
export const queryCommandMeta = {
  name: 'query',
  description: 'Query RDF data with SPARQL',
  args: {
    input: {
      type: 'positional',
      description: 'Input file path',
      required: true
    },
    query: {
      type: 'string',
      description: 'SPARQL query string'
    },
    'query-file': {
      type: 'string',
      description: 'Path to SPARQL query file'
    },
    format: {
      type: 'string',
      description: 'Output format (table, json, csv)',
      default: 'table'
    },
    output: {
      type: 'string',
      description: 'Output file path'
    }
  }
};
