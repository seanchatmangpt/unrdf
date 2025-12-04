/**
 * Custom CLI Commands for Graph Operations
 * @module custom-commands
 */

import { defineCommand } from 'citty';
import { readFileSync, writeFileSync } from 'fs';
import { OxigraphStore } from '@unrdf/oxigraph';
import { z } from 'zod';

/**
 * Map format name to Oxigraph format string
 * @param {string} format - Format name
 * @returns {string} Oxigraph format string
 */
function toOxigraphFormat(format) {
  const oxFormatMap = {
    'turtle': 'text/turtle',
    'ttl': 'text/turtle',
    'ntriples': 'application/n-triples',
    'nt': 'application/n-triples',
    'nquads': 'application/n-quads',
    'nq': 'application/n-quads',
    'trig': 'application/trig',
    'n3': 'text/n3',
    'rdf': 'application/rdf+xml',
    'jsonld': 'application/ld+json',
    'json': 'application/ld+json',
  };
  return oxFormatMap[format] || 'text/turtle';
}

/**
 * Load graph from file using Oxigraph
 * @param {string} filepath - Path to graph file
 * @returns {Promise<Object>} Oxigraph Store
 */
export async function loadGraph(filepath) {
  const inputSchema = z.string().min(1);
  const validPath = inputSchema.parse(filepath);

  const content = readFileSync(validPath, 'utf-8');
  const store = new OxigraphStore();
  store.load(content, { format: toOxigraphFormat('turtle') });
  return store;
}

/**
 * Save graph to file using Oxigraph
 * @param {Object} store - Oxigraph Store
 * @param {string} filepath - Output file path
 * @param {string} format - Output format (turtle, ntriples, etc)
 * @returns {Promise<void>}
 */
export async function saveGraph(store, filepath, format = 'turtle') {
  const inputSchema = z.object({
    store: z.any(),
    filepath: z.string().min(1),
    format: z.enum(['turtle', 'ntriples', 'n-quads', 'trig'])
  });

  inputSchema.parse({ store, filepath, format });

  const oxFormat = toOxigraphFormat(format);
  const serialized = store.dump({ format: oxFormat });
  writeFileSync(filepath, serialized);
}

/**
 * Get graph statistics using Oxigraph
 * @param {Object} store - Oxigraph Store
 * @returns {object}
 */
export function getGraphStats(store) {
  const quads = store.getQuads();
  const subjects = new Set();
  const predicates = new Set();
  const objects = new Set();

  for (const quad of quads) {
    subjects.add(quad.subject.value);
    predicates.add(quad.predicate.value);
    objects.add(quad.object.value);
  }

  return {
    totalQuads: quads.length,
    uniqueSubjects: subjects.size,
    uniquePredicates: predicates.size,
    uniqueObjects: objects.size
  };
}

/**
 * Merge multiple graphs using Oxigraph
 * @param {Object[]} stores - Array of Oxigraph Stores
 * @returns {Object} Merged Oxigraph Store
 */
export function mergeGraphs(stores) {
  const inputSchema = z.array(z.any()).min(1);
  inputSchema.parse(stores);

  const merged = new OxigraphStore();

  for (const store of stores) {
    const quads = store.getQuads();
    for (const quad of quads) {
      merged.add(quad);
    }
  }

  return merged;
}

/**
 * Define load command
 */
export const loadCommand = defineCommand({
  meta: {
    name: 'load',
    description: 'Load RDF graph from file'
  },
  args: {
    input: {
      type: 'positional',
      description: 'Input file path',
      required: true
    }
  },
  async run({ args }) {
    const store = await loadGraph(args.input);
    const stats = getGraphStats(store);
    console.log('Graph loaded successfully');
    console.log(`Total quads: ${stats.totalQuads}`);
    console.log(`Unique subjects: ${stats.uniqueSubjects}`);
    console.log(`Unique predicates: ${stats.uniquePredicates}`);
    console.log(`Unique objects: ${stats.uniqueObjects}`);
  }
});

/**
 * Define stats command
 */
export const statsCommand = defineCommand({
  meta: {
    name: 'stats',
    description: 'Display graph statistics'
  },
  args: {
    input: {
      type: 'positional',
      description: 'Input file path',
      required: true
    }
  },
  async run({ args }) {
    const store = await loadGraph(args.input);
    const stats = getGraphStats(store);

    console.log('Graph Statistics:');
    console.log('================');
    console.log(`Total Quads:      ${stats.totalQuads}`);
    console.log(`Unique Subjects:  ${stats.uniqueSubjects}`);
    console.log(`Unique Predicates: ${stats.uniquePredicates}`);
    console.log(`Unique Objects:   ${stats.uniqueObjects}`);
  }
});

/**
 * Define merge command
 */
export const mergeCommand = defineCommand({
  meta: {
    name: 'merge',
    description: 'Merge multiple RDF graphs'
  },
  args: {
    inputs: {
      type: 'positional',
      description: 'Input file paths (comma-separated)',
      required: true
    },
    output: {
      type: 'string',
      description: 'Output file path',
      required: true
    },
    format: {
      type: 'string',
      description: 'Output format',
      default: 'turtle'
    }
  },
  async run({ args }) {
    const inputs = args.inputs.split(',').map(s => s.trim());
    const stores = await Promise.all(inputs.map(loadGraph));
    const merged = mergeGraphs(stores);
    await saveGraph(merged, args.output, args.format);

    const stats = getGraphStats(merged);
    console.log(`Merged ${inputs.length} graphs`);
    console.log(`Output: ${args.output}`);
    console.log(`Total quads: ${stats.totalQuads}`);
  }
});
