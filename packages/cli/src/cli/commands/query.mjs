/**
 * Query Command - SPARQL Query Execution
 *
 * Execute SPARQL queries against RDF stores
 *
 * @module cli/commands/query
 */

import { defineCommand } from 'citty';
import { readFileSync, existsSync } from 'node:fs';
import { createStore } from '@unrdf/core';
import { Parser } from '@unrdf/core/rdf/n3-justified-only';
import { table } from 'table';
import { detectRdfParserFormat } from '../../lib/rdf-format-detect.mjs';

/**
 * Main query command - execute SPARQL query
 */
export const queryCommand = defineCommand({
  meta: {
    name: 'query',
    description: 'Execute SPARQL query',
  },
  args: {
    file: {
      type: 'string',
      description: 'RDF data file',
      required: true,
    },
    query: {
      type: 'string',
      description: 'SPARQL query string',
      required: true,
    },
    format: {
      type: 'string',
      description: 'Output format (table, json, csv)',
      default: 'table',
    },
  },
  async run({ args }) {
    const { file, query: queryString, format } = args;

    if (!existsSync(file)) {
      console.error(`❌ File not found: ${file}`);
      process.exit(1);
    }

    try {
      // Load RDF data
      const store = createStore();
      const content = readFileSync(file, 'utf-8');
      const rdfFormat = detectRdfParserFormat(file);

      const parser = new Parser({ format: rdfFormat });
      await new Promise((resolve, reject) => {
        parser.parse(content, (error, quad) => {
          if (error) {
            reject(error);
            return;
          }
          if (quad) {
            store.add(quad);
          } else {
            resolve();
          }
        });
      });

      console.log(`📊 Loaded ${store.size} quads from ${file}`);
      console.log(`🔍 Executing query...\n`);

      // Execute query using the executor for proper result formatting
      const { executeQuerySync } = await import('@unrdf/core/sparql/executor-sync');
      const queryResult = executeQuerySync(store, queryString);

      // Detect query type from result
      const isSELECT = queryResult.type === 'select';
      const isASK = typeof queryResult === 'boolean';

      // Handle ASK queries
      if (isASK) {
        console.log(queryResult ? '✅ true' : '❌ false');
        return;
      }

      // Extract the actual results array
      const resultArray = isSELECT ? queryResult.rows : queryResult;

      // Format and display results
      if (resultArray.length === 0) {
        console.log('No results found.');
        return;
      }

      switch (format) {
        case 'json':
          outputJSON(resultArray, isSELECT);
          break;
        case 'csv':
          outputCSV(resultArray, isSELECT);
          break;
        case 'table':
        default:
          outputTable(resultArray, isSELECT);
          break;
      }

      console.log(`\n✅ ${resultArray.length} results`);
    } catch (error) {
      console.error(`❌ Query error: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * Query from file command
 */
export const queryFileCommand = defineCommand({
  meta: {
    name: 'query-file',
    description: 'Execute SPARQL query from file',
  },
  args: {
    data: {
      type: 'string',
      description: 'RDF data file',
      required: true,
    },
    query: {
      type: 'string',
      description: 'SPARQL query file (.sparql)',
      required: true,
    },
    format: {
      type: 'string',
      description: 'Output format (table, json, csv)',
      default: 'table',
    },
  },
  async run({ args }) {
    const { data, query: queryFile, format } = args;

    if (!existsSync(data)) {
      console.error(`❌ Data file not found: ${data}`);
      process.exit(1);
    }

    if (!existsSync(queryFile)) {
      console.error(`❌ Query file not found: ${queryFile}`);
      process.exit(1);
    }

    try {
      const queryString = readFileSync(queryFile, 'utf-8');

      // Reuse queryCommand logic
      await queryCommand.run({
        args: {
          file: data,
          query: queryString,
          format,
        },
      });
    } catch (error) {
      console.error(`❌ Error: ${error.message}`);
      process.exit(1);
    }
  },
});

// Helper functions

/**
 * Output results as table
 */
function outputTable(results, isSELECT) {
  if (isSELECT) {
    // SELECT results are objects with variable bindings
    if (results.length === 0) return;

    const firstResult = results[0];
    const variables = Object.keys(firstResult).filter(k => k !== 'get');

    const data = results.map((binding, idx) => {
      const row = [idx + 1];
      for (const variable of variables) {
        const term = binding[variable];
        // Handle both wrapped term objects and raw values
        const value = term && typeof term === 'object' && 'value' in term ? term.value : term;
        row.push(value ? shortenIRI(String(value)) : '-');
      }
      return row;
    });

    const tableData = [['#', ...variables], ...data];

    console.log(
      table(tableData, {
        header: {
          alignment: 'center',
          content: 'Query Results',
        },
      })
    );
  } else {
    // CONSTRUCT/DESCRIBE results are quads
    const data = results.map((quad, idx) => [
      idx + 1,
      shortenIRI(quad.subject.value),
      shortenIRI(quad.predicate.value),
      shortenIRI(quad.object.value),
      quad.graph ? shortenIRI(quad.graph.value) : '-',
    ]);

    const tableData = [['#', 'Subject', 'Predicate', 'Object', 'Graph'], ...data];

    console.log(
      table(tableData, {
        header: {
          alignment: 'center',
          content: 'Query Results',
        },
      })
    );
  }
}

/**
 * Output results as JSON
 */
function outputJSON(results, isSELECT) {
  if (isSELECT) {
    // SELECT results are objects with variable bindings
    const jsonResults = results.map(binding => {
      const obj = {};
      for (const [variable, term] of Object.entries(binding)) {
        if (variable === 'get') continue;
        // Handle both wrapped term objects and raw values
        obj[variable] = term && typeof term === 'object' && 'value' in term ? term.value : term;
      }
      return obj;
    });
    console.log(JSON.stringify(jsonResults, null, 2));
  } else {
    // CONSTRUCT/DESCRIBE results are quads
    const jsonResults = results.map(quad => ({
      subject: quad.subject.value,
      predicate: quad.predicate.value,
      object: quad.object.value,
      graph: quad.graph ? quad.graph.value : null,
    }));
    console.log(JSON.stringify(jsonResults, null, 2));
  }
}

/**
 * Output results as CSV
 */
function outputCSV(results, isSELECT) {
  if (isSELECT) {
    // SELECT results are objects with variable bindings
    if (results.length === 0) return;

    const firstResult = results[0];
    const variables = Object.keys(firstResult).filter(k => k !== 'get');

    // Header
    console.log(variables.join(','));

    // Data
    for (const binding of results) {
      const parts = variables.map(variable => {
        const term = binding[variable];
        // Handle both wrapped term objects and raw values
        const value = term && typeof term === 'object' && 'value' in term ? term.value : term;
        return value ? escapeCSV(String(value)) : '';
      });
      console.log(parts.join(','));
    }
  } else {
    // CONSTRUCT/DESCRIBE results are quads
    console.log('subject,predicate,object,graph');
    for (const quad of results) {
      const parts = [
        escapeCSV(quad.subject.value),
        escapeCSV(quad.predicate.value),
        escapeCSV(quad.object.value),
        quad.graph ? escapeCSV(quad.graph.value) : '',
      ];
      console.log(parts.join(','));
    }
  }
}

/**
 * Escape CSV field
 */
function escapeCSV(value) {
  if (value.includes(',') || value.includes('"') || value.includes('\n')) {
    return `"${value.replace(/"/g, '""')}"`;
  }
  return value;
}

/**
 * Shorten IRI for display
 */
function shortenIRI(iri) {
  if (iri.length > 50) {
    const parts = iri.split('/');
    return parts.length > 1 ? '...' + parts[parts.length - 1] : iri.substring(0, 47) + '...';
  }
  return iri;
}
