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
import { createStore, addQuad, getQuads } from '@unrdf/core';
import { Parser, Writer } from 'n3';

/**
 * Validation schema for graph commands
 */
const graphPathSchema = z.string().min(1, 'Path is required');
const formatSchema = z.enum(['turtle', 'ntriples', 'nquads', 'trig']).default('turtle');

/**
 * Detect format from file extension
 * @param {string} filePath - File path
 * @returns {string} Format name
 */
function detectFormat(filePath) {
  const ext = path.extname(filePath).toLowerCase();
  const formatMap = {
    '.ttl': 'turtle',
    '.nt': 'ntriples',
    '.nq': 'nquads',
    '.trig': 'trig',
    '.n3': 'turtle',
  };
  return formatMap[ext] || 'turtle';
}

/**
 * Load RDF graph from file
 * @param {string} filePath - Path to RDF file
 * @param {string} [format] - RDF format (auto-detected if not provided)
 * @returns {Promise<Object>} N3 Store
 */
export async function loadGraph(filePath, format) {
  const content = await readFile(filePath, 'utf8');
  const actualFormat = format || detectFormat(filePath);

  return new Promise((resolve, reject) => {
    const store = createStore();
    const parser = new Parser({ format: actualFormat });

    parser.parse(content, (error, quad, _prefixes) => {
      if (error) {
        reject(new Error(`Parse error: ${error.message}`));
      } else if (quad) {
        addQuad(store, quad);
      } else {
        resolve(store);
      }
    });
  });
}

/**
 * Save RDF graph to file
 * @param {Object} store - N3 Store
 * @param {string} filePath - Output file path
 * @param {string} [format] - Output format (auto-detected if not provided)
 * @returns {Promise<void>}
 */
export async function saveGraph(store, filePath, format) {
  const actualFormat = format || detectFormat(filePath);
  const writer = new Writer({ format: actualFormat });

  const quads = getQuads(store);
  quads.forEach(q => writer.addQuad(q));

  return new Promise((resolve, reject) => {
    writer.end((error, result) => {
      if (error) {
        reject(new Error(`Write error: ${error.message}`));
      } else {
        writeFile(filePath, result, 'utf8')
          .then(() => resolve())
          .catch(reject);
      }
    });
  });
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

      // Create empty store and save
      const store = createStore();
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

      const quads = getQuads(store);
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

      // Merge into new store
      const mergedStore = createStore();
      getQuads(store1).forEach(q => addQuad(mergedStore, q));
      getQuads(store2).forEach(q => addQuad(mergedStore, q));

      // Save merged graph
      await saveGraph(mergedStore, output, ctx.args.format);

      const totalQuads = getQuads(mergedStore).length;
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
