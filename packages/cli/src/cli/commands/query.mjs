/**
 * @fileoverview Query commands for SPARQL execution
 *
 * @description
 * CLI commands for executing SPARQL queries against RDF graphs.
 * Supports SELECT, ASK, CONSTRUCT queries with formatted output.
 *
 * @module cli/commands/query
 */

import { defineCommand } from 'citty';
import { z } from 'zod';
import { readFile } from 'node:fs/promises';
import { executeQuery, executeSelect, executeAsk, executeConstruct } from '@unrdf/core';
import { loadGraph } from './graph.mjs';
import { table } from 'table';

/**
 * Validation schemas
 */
const querySchema = z.string().min(1, 'Query is required');
const outputFormatSchema = z.enum(['table', 'json', 'csv']).default('table');

/**
 * Format SELECT results as table
 * @param {Array} bindings - Query bindings
 * @returns {string} Formatted table
 */
function formatTable(bindings) {
  if (bindings.length === 0) {
    return 'No results';
  }

  // Get all variable names
  const variables = Object.keys(bindings[0]);

  // Create table data
  const data = [
    variables, // Header row
    ...bindings.map(binding =>
      variables.map(v => {
        const term = binding[v];
        if (!term) return '';
        if (term.termType === 'Literal') {
          return term.value;
        }
        return term.value;
      })
    ),
  ];

  return table(data);
}

/**
 * Format results as JSON
 * @param {Array} bindings - Query bindings
 * @returns {string} JSON string
 */
function formatJSON(bindings) {
  return JSON.stringify(bindings, null, 2);
}

/**
 * Format results as CSV
 * @param {Array} bindings - Query bindings
 * @returns {string} CSV string
 */
function formatCSV(bindings) {
  if (bindings.length === 0) {
    return '';
  }

  const variables = Object.keys(bindings[0]);
  const header = variables.join(',');
  const rows = bindings.map(binding =>
    variables
      .map(v => {
        const term = binding[v];
        if (!term) return '';
        const value = term.value.replace(/"/g, '""');
        return `"${value}"`;
      })
      .join(',')
  );

  return [header, ...rows].join('\n');
}

/**
 * Execute SPARQL query command
 */
export const queryCommand = defineCommand({
  meta: {
    name: 'query',
    description: 'Execute a SPARQL query against a graph',
  },
  args: {
    graph: {
      type: 'positional',
      description: 'Path to the RDF graph file',
      required: true,
    },
    query: {
      type: 'positional',
      description: 'SPARQL query string',
      required: true,
    },
    format: {
      type: 'string',
      description: 'Output format (table, json, csv)',
      default: 'table',
      alias: 'f',
    },
  },
  async run(ctx) {
    try {
      const graphPath = z.string().parse(ctx.args.graph);
      const sparql = querySchema.parse(ctx.args.query);
      const format = outputFormatSchema.parse(ctx.args.format);

      // Load graph
      const store = await loadGraph(graphPath);

      // Execute query
      const result = executeQuery(store, sparql);

      // Format and display results
      if (typeof result === 'boolean') {
        // ASK query
        console.log(result ? '✅ true' : '❌ false');
      } else if (Array.isArray(result)) {
        // SELECT query
        let output;
        switch (format) {
          case 'json':
            output = formatJSON(result);
            break;
          case 'csv':
            output = formatCSV(result);
            break;
          default:
            output = formatTable(result);
        }
        console.log(output);
      } else {
        // CONSTRUCT query (returns store)
        const quads = result.getQuads();
        console.log(`✅ Constructed ${quads.length} triples`);
        quads.forEach(q => {
          console.log(`${q.subject.value} ${q.predicate.value} ${q.object.value}`);
        });
      }
    } catch (error) {
      console.error(`❌ Query failed: ${error.message}`);
      throw error;
    }
  },
});

/**
 * Execute query from file command
 */
export const queryFileCommand = defineCommand({
  meta: {
    name: 'query-file',
    description: 'Execute a SPARQL query from a file',
  },
  args: {
    graph: {
      type: 'positional',
      description: 'Path to the RDF graph file',
      required: true,
    },
    queryFile: {
      type: 'positional',
      description: 'Path to SPARQL query file',
      required: true,
    },
    format: {
      type: 'string',
      description: 'Output format (table, json, csv)',
      default: 'table',
      alias: 'f',
    },
  },
  async run(ctx) {
    try {
      const graphPath = z.string().parse(ctx.args.graph);
      const queryFilePath = z.string().parse(ctx.args.queryFile);
      const format = outputFormatSchema.parse(ctx.args.format);

      // Read query from file
      const sparql = await readFile(queryFilePath, 'utf8');

      // Execute using query command logic
      const store = await loadGraph(graphPath);
      const result = executeQuery(store, sparql);

      // Format and display results
      if (typeof result === 'boolean') {
        console.log(result ? '✅ true' : '❌ false');
      } else if (Array.isArray(result)) {
        let output;
        switch (format) {
          case 'json':
            output = formatJSON(result);
            break;
          case 'csv':
            output = formatCSV(result);
            break;
          default:
            output = formatTable(result);
        }
        console.log(output);
      } else {
        const quads = result.getQuads();
        console.log(`✅ Constructed ${quads.length} triples`);
        quads.forEach(q => {
          console.log(`${q.subject.value} ${q.predicate.value} ${q.object.value}`);
        });
      }
    } catch (error) {
      console.error(`❌ Query file failed: ${error.message}`);
      throw error;
    }
  },
});

/**
 * Export query functions
 */
export { formatTable, formatJSON, formatCSV };
