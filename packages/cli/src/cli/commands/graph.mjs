/**
 * Graph Command - RDF Graph Operations
 *
 * Commands for graph management:
 * - create: Create new graph
 * - load: Load RDF data into graph
 * - query: Execute SPARQL query
 * - dump: Export graph data
 * - stats: Show graph statistics
 *
 * @module cli/commands/graph
 */

import { defineCommand } from 'citty';
import { readFileSync, writeFileSync, existsSync } from 'node:fs';
import { createStore, namedNode, literal, quad, iterateQuads } from '@unrdf/core';
import { Parser, Writer } from 'n3';

/**
 * Create a new graph
 */
const createCommand = defineCommand({
  meta: {
    name: 'create',
    description: 'Create a new RDF graph',
  },
  args: {
    name: {
      type: 'string',
      description: 'Graph name',
      required: true,
    },
    file: {
      type: 'string',
      description: 'Output file path (default: <name>.nq)',
      required: false,
    },
  },
  async run({ args }) {
    const graphName = args.name;
    const outputFile = args.file || `${graphName}.nq`;

    try {
      const store = createStore();

      // Create an empty named graph
      const graphIRI = namedNode(`urn:graph:${graphName}`);

      // Add metadata triple
      store.add(
        quad(
          graphIRI,
          namedNode('http://purl.org/dc/terms/created'),
          literal(new Date().toISOString()),
          graphIRI
        )
      );

      // Export to file
      const writer = new Writer({ format: 'N-Quads' });
      for (const q of iterateQuads(store)) {
        writer.addQuad(q);
      }
      writer.end((error, result) => {
        if (error) throw error;
        writeFileSync(outputFile, result, 'utf-8');
      });

      console.log(`✅ Created graph: ${graphName}`);
      console.log(`📁 File: ${outputFile}`);
      console.log(`📊 Quads: ${store.size}`);
    } catch (error) {
      console.error(`❌ Error creating graph: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * Load RDF data into graph
 */
const loadCommand = defineCommand({
  meta: {
    name: 'load',
    description: 'Load RDF data into a graph',
  },
  args: {
    file: {
      type: 'string',
      description: 'RDF file to load (Turtle, N-Triples, N-Quads)',
      required: true,
    },
    graph: {
      type: 'string',
      description: 'Target graph name',
      required: false,
    },
    format: {
      type: 'string',
      description: 'RDF format (turtle, ntriples, nquads) - auto-detected if not specified',
      required: false,
    },
  },
  async run({ args }) {
    const { file, graph: graphName, format } = args;

    if (!existsSync(file)) {
      console.error(`❌ File not found: ${file}`);
      process.exit(1);
    }

    try {
      const store = createStore();
      const content = readFileSync(file, 'utf-8');

      // Auto-detect format
      const detectedFormat = format || detectFormat(file);

      const parser = new Parser({ format: detectedFormat });
      const graphIRI = graphName ? namedNode(`urn:graph:${graphName}`) : null;

      let quadCount = 0;

      await new Promise((resolve, reject) => {
        parser.parse(content, (error, parsedQuad, _prefixes) => {
          if (error) {
            reject(error);
            return;
          }

          if (parsedQuad) {
            // If graph name specified, override the graph
            const finalQuad = graphIRI
              ? quad(parsedQuad.subject, parsedQuad.predicate, parsedQuad.object, graphIRI)
              : parsedQuad;

            store.add(finalQuad);
            quadCount++;
          } else {
            // Parsing complete
            resolve();
          }
        });
      });

      console.log(`✅ Loaded data from: ${file}`);
      console.log(`📊 Quads loaded: ${quadCount}`);
      if (graphName) {
        console.log(`📂 Target graph: ${graphName}`);
      }
    } catch (error) {
      console.error(`❌ Error loading data: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * Execute SPARQL query
 */
const queryCommand = defineCommand({
  meta: {
    name: 'query',
    description: 'Execute SPARQL query on graph',
  },
  args: {
    file: {
      type: 'string',
      description: 'Graph file to query',
      required: true,
    },
    query: {
      type: 'string',
      description: 'SPARQL query string',
      required: true,
    },
    format: {
      type: 'string',
      description: 'Output format (table, json, turtle)',
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
      const store = createStore();
      const content = readFileSync(file, 'utf-8');

      // Load data
      const parser = new Parser({ format: detectFormat(file) });
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

      // Execute query
      const results = store.query(queryString);

      // Format output
      if (format === 'json') {
        console.log(JSON.stringify(Array.from(results), null, 2));
      } else if (format === 'table') {
        formatResultsTable(results);
      } else {
        // Default: show quads
        for (const result of results) {
          console.log(formatQuad(result));
        }
      }
    } catch (error) {
      console.error(`❌ Query error: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * Dump graph data
 */
const dumpCommand = defineCommand({
  meta: {
    name: 'dump',
    description: 'Export graph data to file',
  },
  args: {
    file: {
      type: 'string',
      description: 'Source graph file',
      required: true,
    },
    output: {
      type: 'string',
      description: 'Output file',
      required: true,
    },
    format: {
      type: 'string',
      description: 'Output format (turtle, ntriples, nquads)',
      default: 'turtle',
    },
  },
  async run({ args }) {
    const { file, output, format } = args;

    if (!existsSync(file)) {
      console.error(`❌ File not found: ${file}`);
      process.exit(1);
    }

    try {
      const store = createStore();
      const content = readFileSync(file, 'utf-8');

      // Load data
      const parser = new Parser({ format: detectFormat(file) });
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

      // Export with writer
      const writer = new Writer({ format: formatToMimeType(format) });
      for (const q of iterateQuads(store)) {
        writer.addQuad(q);
      }

      writer.end((error, result) => {
        if (error) {
          console.error(`❌ Export error: ${error.message}`);
          process.exit(1);
        }
        writeFileSync(output, result, 'utf-8');
        console.log(`✅ Exported to: ${output}`);
        console.log(`📊 Quads: ${store.size}`);
        console.log(`📝 Format: ${format}`);
      });
    } catch (error) {
      console.error(`❌ Export error: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * Show graph statistics
 */
const statsCommand = defineCommand({
  meta: {
    name: 'stats',
    description: 'Show graph statistics',
  },
  args: {
    file: {
      type: 'string',
      description: 'Graph file to analyze',
      required: true,
    },
  },
  async run({ args }) {
    const { file } = args;

    if (!existsSync(file)) {
      console.error(`❌ File not found: ${file}`);
      process.exit(1);
    }

    try {
      const store = createStore();
      const content = readFileSync(file, 'utf-8');

      // Load data
      const parser = new Parser({ format: detectFormat(file) });
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

      // Calculate statistics
      const subjects = new Set();
      const predicates = new Set();
      const objects = new Set();
      const graphs = new Set();

      for (const q of iterateQuads(store)) {
        subjects.add(q.subject.value);
        predicates.add(q.predicate.value);
        objects.add(q.object.value);
        if (q.graph) {
          graphs.add(q.graph.value);
        }
      }

      console.log('\n📊 Graph Statistics');
      console.log('═'.repeat(50));
      console.log(`Total Quads:     ${store.size}`);
      console.log(`Unique Subjects: ${subjects.size}`);
      console.log(`Unique Predicates: ${predicates.size}`);
      console.log(`Unique Objects:  ${objects.size}`);
      console.log(`Named Graphs:    ${graphs.size}`);
      console.log('═'.repeat(50));
    } catch (error) {
      console.error(`❌ Error analyzing graph: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * Main graph command
 */
export const graphCommand = defineCommand({
  meta: {
    name: 'graph',
    description: 'RDF graph operations',
  },
  subCommands: {
    create: createCommand,
    load: loadCommand,
    query: queryCommand,
    dump: dumpCommand,
    stats: statsCommand,
  },
});

// Helper functions

/**
 * Detect RDF format from file extension
 */
function detectFormat(filename) {
  const ext = filename.split('.').pop().toLowerCase();
  switch (ext) {
    case 'ttl':
      return 'Turtle';
    case 'nt':
      return 'N-Triples';
    case 'nq':
      return 'N-Quads';
    case 'jsonld':
      return 'JSON-LD';
    case 'rdf':
    case 'xml':
      return 'RDF/XML';
    default:
      return 'Turtle'; // default
  }
}

/**
 * Convert format name to MIME type
 */
function formatToMimeType(format) {
  switch (format.toLowerCase()) {
    case 'turtle':
    case 'ttl':
      return 'Turtle';
    case 'ntriples':
    case 'nt':
      return 'N-Triples';
    case 'nquads':
    case 'nq':
      return 'N-Quads';
    default:
      return 'Turtle';
  }
}

/**
 * Format quad for display
 */
function formatQuad(quad) {
  return `${quad.subject.value} ${quad.predicate.value} ${quad.object.value} ${quad.graph ? quad.graph.value : '.'}`;
}

/**
 * Format query results as table
 */
function formatResultsTable(results) {
  const rows = Array.from(results);

  if (rows.length === 0) {
    console.log('No results');
    return;
  }

  console.log('\n📋 Query Results');
  console.log('─'.repeat(100));

  rows.forEach((row, idx) => {
    console.log(`${idx + 1}. ${formatQuad(row)}`);
  });

  console.log('─'.repeat(100));
  console.log(`Total: ${rows.length} results`);
}
