/**
 * Convert Command - RDF Format Conversion
 *
 * Convert RDF data between formats:
 * - Turtle (.ttl)
 * - N-Triples (.nt)
 * - N-Quads (.nq)
 * - JSON-LD (.jsonld)
 *
 * @module cli/commands/convert
 */

import { defineCommand } from 'citty';
import { readFileSync, writeFileSync, existsSync } from 'node:fs';
import { createStore, iterateQuads } from '@unrdf/core';
import { Parser, Writer } from '@unrdf/core/rdf/n3-justified-only';

/**
 * Main convert command
 */
export const convertCommand = defineCommand({
  meta: {
    name: 'convert',
    description: 'Convert RDF between formats',
  },
  args: {
    input: {
      type: 'string',
      description: 'Input RDF file',
      required: true,
    },
    output: {
      type: 'string',
      description: 'Output file',
      required: true,
    },
    from: {
      type: 'string',
      description: 'Input format (turtle, ntriples, nquads) - auto-detected if not specified',
      required: false,
    },
    to: {
      type: 'string',
      description: 'Output format (turtle, ntriples, nquads) - auto-detected if not specified',
      required: false,
    },
  },
  async run({ args }) {
    const { input, output, from, to } = args;

    if (!existsSync(input)) {
      console.error(`❌ Input file not found: ${input}`);
      process.exit(1);
    }

    try {
      const inputFormat = from || detectFormat(input);
      const outputFormat = to || detectFormat(output);

      console.log(`🔄 Converting ${input} (${inputFormat}) -> ${output} (${outputFormat})`);

      // Load data
      const store = createStore();
      const content = readFileSync(input, 'utf-8');

      const parser = new Parser({ format: inputFormat });
      let quadCount = 0;

      await new Promise((resolve, reject) => {
        parser.parse(content, (error, quad) => {
          if (error) {
            reject(error);
            return;
          }
          if (quad) {
            store.add(quad);
            quadCount++;
          } else {
            resolve();
          }
        });
      });

      console.log(`📊 Loaded ${quadCount} quads`);

      // Write output
      const writer = new Writer({ format: outputFormat });
      for (const quad of iterateQuads(store)) {
        writer.addQuad(quad);
      }

      writer.end((error, result) => {
        if (error) {
          console.error(`❌ Conversion error: ${error.message}`);
          process.exit(1);
        }

        writeFileSync(output, result, 'utf-8');
        console.log(`✅ Converted successfully`);
        console.log(`📁 Output: ${output}`);
        console.log(`📊 Quads: ${quadCount}`);
      });
    } catch (error) {
      console.error(`❌ Conversion error: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * Convert to Turtle shorthand
 */
export const toTurtleCommand = defineCommand({
  meta: {
    name: 'to-turtle',
    description: 'Convert RDF to Turtle format',
  },
  args: {
    input: {
      type: 'string',
      description: 'Input RDF file',
      required: true,
    },
    output: {
      type: 'string',
      description: 'Output Turtle file',
      required: false,
    },
  },
  async run({ args }) {
    const input = args.input;
    const output = args.output || input.replace(/\.[^.]+$/, '.ttl');

    await convertCommand.run({
      args: {
        input,
        output,
        to: 'Turtle',
      },
    });
  },
});

/**
 * Convert to N-Triples shorthand
 */
export const toNTriplesCommand = defineCommand({
  meta: {
    name: 'to-ntriples',
    description: 'Convert RDF to N-Triples format',
  },
  args: {
    input: {
      type: 'string',
      description: 'Input RDF file',
      required: true,
    },
    output: {
      type: 'string',
      description: 'Output N-Triples file',
      required: false,
    },
  },
  async run({ args }) {
    const input = args.input;
    const output = args.output || input.replace(/\.[^.]+$/, '.nt');

    await convertCommand.run({
      args: {
        input,
        output,
        to: 'N-Triples',
      },
    });
  },
});

/**
 * Convert to JSON shorthand
 */
export const toJSONCommand = defineCommand({
  meta: {
    name: 'to-json',
    description: 'Convert RDF to JSON representation',
  },
  args: {
    input: {
      type: 'string',
      description: 'Input RDF file',
      required: true,
    },
    output: {
      type: 'string',
      description: 'Output JSON file',
      required: false,
    },
  },
  async run({ args }) {
    const { input, output: outputFile } = args;

    if (!existsSync(input)) {
      console.error(`❌ Input file not found: ${input}`);
      process.exit(1);
    }

    try {
      // Load data
      const store = createStore();
      const content = readFileSync(input, 'utf-8');
      const inputFormat = detectFormat(input);

      const parser = new Parser({ format: inputFormat });
      const quads = [];

      await new Promise((resolve, reject) => {
        parser.parse(content, (error, quad) => {
          if (error) {
            reject(error);
            return;
          }
          if (quad) {
            store.add(quad);
            quads.push({
              subject: quad.subject.value,
              predicate: quad.predicate.value,
              object: quad.object.value,
              graph: quad.graph ? quad.graph.value : null,
            });
          } else {
            resolve();
          }
        });
      });

      const jsonOutput = JSON.stringify(quads, null, 2);
      const output = outputFile || input.replace(/\.[^.]+$/, '.json');

      writeFileSync(output, jsonOutput, 'utf-8');

      console.log(`✅ Converted to JSON`);
      console.log(`📁 Output: ${output}`);
      console.log(`📊 Quads: ${quads.length}`);
    } catch (error) {
      console.error(`❌ Conversion error: ${error.message}`);
      process.exit(1);
    }
  },
});

// Helper functions

/**
 * Detect RDF format from filename
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
    default:
      return 'Turtle';
  }
}
