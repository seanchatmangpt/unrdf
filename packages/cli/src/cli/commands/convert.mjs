/**
 * @fileoverview Conversion commands for RDF format conversion
 *
 * @description
 * CLI commands for converting between RDF formats.
 * Supports Turtle, N-Triples, N-Quads, and JSON-LD.
 *
 * @module cli/commands/convert
 */

import { defineCommand } from 'citty';
import { z } from 'zod';
import { loadGraph, saveGraph } from './graph.mjs';

/**
 * Validation schemas
 */
const formatSchema = z.enum(['turtle', 'ntriples', 'nquads', 'trig', 'jsonld']);

/**
 * Convert to JSON-LD representation
 * @param {Object} store - N3 Store
 * @returns {Object} JSON-LD object
 */
function toJSONLD(store) {
  const quads = store.getQuads();
  const graph = quads.map(q => ({
    '@id': q.subject.value,
    [q.predicate.value]: {
      '@value': q.object.value,
      '@type': q.object.datatype?.value,
    },
  }));

  return {
    '@context': {},
    '@graph': graph,
  };
}

/**
 * Convert format command
 */
export const convertCommand = defineCommand({
  meta: {
    name: 'convert',
    description: 'Convert RDF graph between formats',
  },
  args: {
    input: {
      type: 'positional',
      description: 'Input graph file',
      required: true,
    },
    output: {
      type: 'positional',
      description: 'Output graph file',
      required: true,
    },
    format: {
      type: 'string',
      description: 'Output format (turtle, ntriples, nquads, jsonld)',
      alias: 'f',
    },
  },
  async run(ctx) {
    try {
      const input = z.string().parse(ctx.args.input);
      const output = z.string().parse(ctx.args.output);
      const format = ctx.args.format ? formatSchema.parse(ctx.args.format) : undefined;

      // Load input graph
      const store = await loadGraph(input);

      // Handle JSON-LD separately
      if (format === 'jsonld') {
        const jsonld = toJSONLD(store);
        const { writeFile } = await import('node:fs/promises');
        await writeFile(output, JSON.stringify(jsonld, null, 2), 'utf8');
        console.log(`✅ Converted to JSON-LD: ${output}`);
        return;
      }

      // Save in target format
      await saveGraph(store, output, format);

      const quadCount = store.getQuads().length;
      console.log(`✅ Converted ${input} -> ${output}`);
      console.log(`   Format: ${format || 'auto-detect'}`);
      console.log(`   Triples: ${quadCount}`);
    } catch (error) {
      console.error(`❌ Convert failed: ${error.message}`);
      throw error;
    }
  },
});

/**
 * To Turtle command
 */
export const toTurtleCommand = defineCommand({
  meta: {
    name: 'to-turtle',
    description: 'Convert graph to Turtle format',
  },
  args: {
    input: {
      type: 'positional',
      description: 'Input graph file',
      required: true,
    },
    output: {
      type: 'positional',
      description: 'Output Turtle file',
      required: true,
    },
  },
  async run(ctx) {
    try {
      const input = z.string().parse(ctx.args.input);
      const output = z.string().parse(ctx.args.output);

      const store = await loadGraph(input);
      await saveGraph(store, output, 'turtle');

      console.log(`✅ Converted to Turtle: ${output}`);
    } catch (error) {
      console.error(`❌ To Turtle failed: ${error.message}`);
      throw error;
    }
  },
});

/**
 * To N-Triples command
 */
export const toNTriplesCommand = defineCommand({
  meta: {
    name: 'to-ntriples',
    description: 'Convert graph to N-Triples format',
  },
  args: {
    input: {
      type: 'positional',
      description: 'Input graph file',
      required: true,
    },
    output: {
      type: 'positional',
      description: 'Output N-Triples file',
      required: true,
    },
  },
  async run(ctx) {
    try {
      const input = z.string().parse(ctx.args.input);
      const output = z.string().parse(ctx.args.output);

      const store = await loadGraph(input);
      await saveGraph(store, output, 'ntriples');

      console.log(`✅ Converted to N-Triples: ${output}`);
    } catch (error) {
      console.error(`❌ To N-Triples failed: ${error.message}`);
      throw error;
    }
  },
});

/**
 * To JSON command
 */
export const toJSONCommand = defineCommand({
  meta: {
    name: 'to-json',
    description: 'Convert graph to JSON-LD',
  },
  args: {
    input: {
      type: 'positional',
      description: 'Input graph file',
      required: true,
    },
    output: {
      type: 'positional',
      description: 'Output JSON file',
      required: true,
    },
  },
  async run(ctx) {
    try {
      const input = z.string().parse(ctx.args.input);
      const output = z.string().parse(ctx.args.output);

      const store = await loadGraph(input);
      const jsonld = toJSONLD(store);

      const { writeFile } = await import('node:fs/promises');
      await writeFile(output, JSON.stringify(jsonld, null, 2), 'utf8');

      console.log(`✅ Converted to JSON-LD: ${output}`);
    } catch (error) {
      console.error(`❌ To JSON failed: ${error.message}`);
      throw error;
    }
  },
});
