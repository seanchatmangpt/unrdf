/**
 * @fileoverview Graph commands for RDF graph management
 *
 * @description
 * CLI commands for creating, loading, saving, and managing RDF graphs.
 * Supports multiple formats: Turtle, N-Triples, N-Quads.
 *
 * @module cli/commands/graph
 */

import { defineCommand } from 'citty';
import { z } from 'zod';
import { readFile, writeFile, unlink, mkdir } from 'node:fs/promises';
import path from 'node:path';
import { OxigraphStore, dataFactory } from '@unrdf/oxigraph';
import { Writer } from '@unrdf/core/rdf/n3-justified-only';

/**
 * Validation schema for graph commands
 */
const graphPathSchema = z.string().min(1, 'Path is required');
const formatSchema = z.enum(['turtle', 'ntriples', 'nquads', 'trig']).default('turtle');

/**
 * Detect format from file extension
 * @param {string} filePath - File path
 * @returns {string} Format name (Oxigraph style)
 */
function detectFormat(filePath) {
  const ext = path.extname(filePath).toLowerCase();
  const formatMap = {
    '.ttl': 'ttl',
    '.nt': 'nt',
    '.nq': 'nq',
    '.trig': 'trig',
    '.n3': 'n3',
    '.rdf': 'rdf',
    '.jsonld': 'jsonld',
    '.json': 'jsonld',
  };
  return formatMap[ext] || 'ttl';
}

/**
 * Map format name to Oxigraph format string
 * @param {string} format - Format name
 * @returns {string} Oxigraph format string
 */
function toOxigraphFormat(format) {
  const oxFormatMap = {
    turtle: 'text/turtle',
    ttl: 'text/turtle',
    ntriples: 'application/n-triples',
    nt: 'application/n-triples',
    nquads: 'application/n-quads',
    nq: 'application/n-quads',
    trig: 'application/trig',
    n3: 'text/n3',
    rdf: 'application/rdf+xml',
    jsonld: 'application/ld+json',
    json: 'application/ld+json',
  };
  return oxFormatMap[format] || 'text/turtle';
}

/**
 * Load RDF graph from file using Oxigraph
 * @param {string} filePath - Path to RDF file
 * @param {string} [format] - RDF format (auto-detected if not provided)
 * @returns {Promise<Object>} Oxigraph Store
 */
export async function loadGraph(filePath, format) {
  try {
    const content = await readFile(filePath, 'utf8');
    const actualFormat = format || detectFormat(filePath);
    const oxFormat = toOxigraphFormat(actualFormat);

    // Create Oxigraph store
    const store = new OxigraphStore();

    // Only load if content is non-empty (handle empty graphs)
    if (content.trim().length > 0) {
      store.load(content, { format: oxFormat });
    }

    return store;
  } catch (error) {
    throw new Error(`Parse error: ${error.message}`);
  }
}

/**
 * Save RDF graph to file using Oxigraph
 * @param {Object} store - Oxigraph Store
 * @param {string} filePath - Output file path
 * @param {string} [format] - Output format (auto-detected if not provided)
 * @returns {Promise<void>}
 */
export async function saveGraph(store, filePath, format) {
  try {
    const actualFormat = format || detectFormat(filePath);
    const oxFormat = toOxigraphFormat(actualFormat);

    // Hybrid serialization adapter: N3.Writer for triple formats, Oxigraph for quad formats
    let serialized;
    const isTripleFormat = ['text/turtle', 'application/n-triples', 'text/n3'].includes(oxFormat);

    if (isTripleFormat) {
      // Use N3.Writer for triple formats (Turtle, N-Triples, N3)
      const writer = new Writer({ format: actualFormat });
      const quads = store.getQuads ? store.getQuads() : store.match();
      quads.forEach(q => writer.addQuad(q));
      serialized = await new Promise((resolve, reject) => {
        writer.end((error, result) => {
          if (error) reject(error);
          else resolve(result);
        });
      });
    } else {
      // Use Oxigraph dump for quad formats (N-Quads, TriG)
      serialized = store.dump({ format: oxFormat });
    }

    await writeFile(filePath, serialized, 'utf8');
  } catch (error) {
    throw new Error(`Write error: ${error.message}`);
  }
}

/**
 * Create command - Create new RDF graph file
 */
export const createCommand = defineCommand({
  meta: {
    name: 'create',
    description: 'Create a new RDF graph file',
  },
  args: {
    path: {
      type: 'positional',
      description: 'Path to the new graph file',
      required: true,
    },
    format: {
      type: 'string',
      description: 'RDF format (turtle, ntriples, nquads)',
      default: 'turtle',
      alias: 'f',
    },
  },
  async run(ctx) {
    try {
      const filePath = graphPathSchema.parse(ctx.args.path);
      const format = formatSchema.parse(ctx.args.format);

      // Ensure directory exists
      const dir = path.dirname(filePath);
      await mkdir(dir, { recursive: true });

      // Create empty Oxigraph store and save
      const store = new OxigraphStore();
      await saveGraph(store, filePath, format);

      console.log(`✅ Created graph: ${filePath}`);
      console.log(`   Format: ${format}`);
    } catch (error) {
      console.error(`❌ Create failed: ${error.message}`);
      throw error;
    }
  },
});

/**
 * Delete command - Delete graph file
 */
export const deleteCommand = defineCommand({
  meta: {
    name: 'delete',
    description: 'Delete a graph file',
  },
  args: {
    path: {
      type: 'positional',
      description: 'Path to the graph file',
      required: true,
    },
  },
  async run(ctx) {
    try {
      const filePath = graphPathSchema.parse(ctx.args.path);
      await unlink(filePath);
      console.log(`✅ Deleted graph: ${filePath}`);
    } catch (error) {
      console.error(`❌ Delete failed: ${error.message}`);
      throw error;
    }
  },
});

/**
 * Describe command - Show graph statistics
 */
export const describeCommand = defineCommand({
  meta: {
    name: 'describe',
    description: 'Show graph statistics',
  },
  args: {
    path: {
      type: 'positional',
      description: 'Path to the graph file',
      required: true,
    },
  },
  async run(ctx) {
    try {
      const filePath = graphPathSchema.parse(ctx.args.path);
      const store = await loadGraph(filePath);

      const quads = store.getQuads();
      const subjects = new Set();
      const predicates = new Set();
      const objects = new Set();

      quads.forEach(q => {
        subjects.add(q.subject.value);
        predicates.add(q.predicate.value);
        objects.add(q.object.value);
      });

      console.log(`📊 Graph: ${filePath}`);
      console.log(`   Triples: ${quads.length}`);
      console.log(`   Subjects: ${subjects.size}`);
      console.log(`   Predicates: ${predicates.size}`);
      console.log(`   Objects: ${objects.size}`);
    } catch (error) {
      console.error(`❌ Describe failed: ${error.message}`);
      throw error;
    }
  },
});

/**
 * Merge command - Merge two graphs
 */
export const mergeCommand = defineCommand({
  meta: {
    name: 'merge',
    description: 'Merge two RDF graphs',
  },
  args: {
    input1: {
      type: 'positional',
      description: 'First graph file',
      required: true,
    },
    input2: {
      type: 'positional',
      description: 'Second graph file',
      required: true,
    },
    output: {
      type: 'positional',
      description: 'Output graph file',
      required: true,
    },
    format: {
      type: 'string',
      description: 'Output format',
      alias: 'f',
    },
  },
  async run(ctx) {
    try {
      const input1 = graphPathSchema.parse(ctx.args.input1);
      const input2 = graphPathSchema.parse(ctx.args.input2);
      const output = graphPathSchema.parse(ctx.args.output);

      // Load both graphs
      const store1 = await loadGraph(input1);
      const store2 = await loadGraph(input2);

      // Merge into new Oxigraph store (default graph only)
      const mergedStore = new OxigraphStore();
      const quads1 = store1.getQuads();
      const quads2 = store2.getQuads();

      // Create quads in default graph using dataFactory
      for (const q of quads1) {
        const normalizedQuad = dataFactory.quad(
          q.subject,
          q.predicate,
          q.object,
          dataFactory.defaultGraph()
        );
        mergedStore.add(normalizedQuad);
      }
      for (const q of quads2) {
        const normalizedQuad = dataFactory.quad(
          q.subject,
          q.predicate,
          q.object,
          dataFactory.defaultGraph()
        );
        mergedStore.add(normalizedQuad);
      }

      // Save merged graph
      await saveGraph(mergedStore, output, ctx.args.format);

      const totalQuads = mergedStore.getQuads().length;
      console.log(`✅ Merged graphs to: ${output}`);
      console.log(`   Total triples: ${totalQuads}`);
    } catch (error) {
      console.error(`❌ Merge failed: ${error.message}`);
      throw error;
    }
  },
});

/**
 * Graph command (parent command)
 */
export const graphCommand = defineCommand({
  meta: {
    name: 'graph',
    description: 'Manage RDF graphs',
  },
  subCommands: {
    create: createCommand,
    delete: deleteCommand,
    describe: describeCommand,
    merge: mergeCommand,
  },
});
