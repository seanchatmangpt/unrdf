/**
 * @fileoverview Hook eval command - Evaluate knowledge hooks
 *
 * @description
 * CLI command for evaluating knowledge hooks against RDF graphs.
 * Migrated from legacy CLI with full v2 integration.
 *
 * @module cli/commands/hook/eval
 * @version 2.4.0
 * @license MIT
 */

import { defineCommand } from 'citty';
import { readFile, writeFile } from 'fs/promises';
import { z } from 'zod';
import { evaluateHook } from '../../../knowledge-engine/hook-management.mjs';
import { createStore } from '@unrdf/core';
import { OxigraphStore } from '@unrdf/oxigraph';

/**
 * Validation schema for eval command arguments
 */
const evalArgsSchema = z.object({
  hook: z.string().describe('Path to hook definition file (JSON)'),
  data: z.string().optional().describe('Path to RDF data file (Turtle/N-Triples)'),
  format: z.enum(['table', 'json', 'yaml']).default('table'),
  output: z.string().optional().describe('Output file path'),
  verbose: z.boolean().default(false),
});

/**
 * Parse RDF data from file
 * @param {string} filePath - Path to RDF file
 * @param {string} format - File format (turtle, ntriples, etc.)
 * @returns {Promise<OxigraphStore>} Parsed RDF store
 * @private
 */
async function parseRDFFile(filePath, format = 'turtle') {
  const content = await readFile(filePath, 'utf-8');
  const store = new OxigraphStore();

  // Map format to MIME type
  const mimeTypes = {
    turtle: 'text/turtle',
    ntriples: 'application/n-triples',
    nquads: 'application/n-quads',
    trig: 'application/trig',
    n3: 'text/n3',
    rdf: 'application/rdf+xml',
    jsonld: 'application/ld+json'
  };

  const mimeType = mimeTypes[format] || 'text/turtle';
  store.load(content, { format: mimeType });
  return store;
}

/**
 * Format evaluation result for output
 * @param {Object} result - Evaluation result
 * @param {string} format - Output format
 * @private
 */
function formatResult(result, format) {
  switch (format) {
    case 'json':
      console.log(JSON.stringify(result, null, 2));
      break;
    case 'yaml':
      console.log('result:');
      console.log(`  fired: ${result.fired}`);
      console.log(`  type: ${result.type || 'unknown'}`);
      console.log(`  executionTime: ${result.executionTime || 0}ms`);
      if (result.value !== undefined) {
        console.log(`  value: ${result.value}`);
      }
      if (result.violations) {
        console.log(`  violations: ${result.violations.length}`);
      }
      break;
    case 'table':
    default:
      console.log(`\n🔥 Hook Evaluation Result`);
      console.log('─'.repeat(60));
      console.log(`Status: ${result.fired ? '✅ FIRED' : '❌ NOT FIRED'}`);
      console.log(`Type: ${result.type || 'unknown'}`);
      console.log(`Duration: ${result.executionTime || 0}ms`);

      if (result.type === 'threshold') {
        console.log(`Value: ${result.value} ${result.operator || ''} ${result.threshold || ''}`);
      }

      if (result.type === 'shacl' && result.violations) {
        console.log(`Violations: ${result.violations.length}`);
        if (result.violations.length > 0 && result.violations.length <= 5) {
          result.violations.forEach((v, i) => {
            console.log(`  ${i + 1}. ${v.message || v}`);
          });
        }
      }

      if (result.error) {
        console.log(`Error: ${result.error}`);
      }
      console.log('─'.repeat(60));
      break;
  }
}

/**
 * Hook eval command
 */
export const evalCommand = defineCommand({
  meta: {
    name: 'eval',
    description: 'Evaluate a knowledge hook against RDF data',
  },
  args: {
    hook: {
      type: 'positional',
      description: 'Path to hook definition file (JSON)',
      required: true,
    },
    data: {
      type: 'string',
      description: 'Path to RDF data file (Turtle/N-Triples)',
      alias: 'd',
    },
    format: {
      type: 'string',
      description: 'Output format (table, json, yaml)',
      default: 'table',
      alias: 'f',
    },
    output: {
      type: 'string',
      description: 'Output file path',
      alias: 'o',
    },
    verbose: {
      type: 'boolean',
      description: 'Show detailed output',
      alias: 'v',
    },
  },
  async run(ctx) {
    try {
      // Validate arguments
      const args = evalArgsSchema.parse(ctx.args);

      console.log(`🔍 Evaluating hook: ${args.hook}`);

      // Load hook definition
      let hookDef;
      try {
        const hookJson = await readFile(args.hook, 'utf-8');
        hookDef = JSON.parse(hookJson);
      } catch (error) {
        throw new Error(`Failed to load hook: ${error.message}`);
      }

      // Load data if provided
      let store = createStore();
      if (args.data) {
        try {
          const oxStore = await parseRDFFile(args.data);
          // Convert to core store
          store = createStore();
          for (const quad of oxStore.getQuads()) {
            store.add(quad);
          }
          console.log(`📊 Loaded ${store.size} triples from ${args.data}`);
        } catch (error) {
          throw new Error(`Failed to load data: ${error.message}`);
        }
      }

      // Evaluate hook
      const result = await evaluateHook(hookDef, store, {
        verbose: args.verbose,
        basePath: process.cwd(),
      });

      // Format result for display (evaluateHook returns event object with satisfied property)
      const displayResult = {
        fired: result.satisfied || false,
        type: hookDef.when?.kind || 'unknown',
        executionTime: result.executionTime || 0,
        value: result.value,
        operator: result.operator,
        threshold: result.threshold,
        violations: result.violations,
        error: result.error,
        output: result.output,
      };

      // Format and output result
      formatResult(displayResult, args.format);

      // Write output if requested
      if (args.output) {
        await writeFile(args.output, JSON.stringify(result, null, 2));
        console.log(`\n📄 Result written to ${args.output}`);
      }

      // Exit with appropriate code
      process.exit(displayResult.fired ? 0 : 1);
    } catch (error) {
      console.error(`❌ Hook evaluation failed: ${error.message}`);
      if (ctx.args.verbose) {
        console.error(error.stack);
      }
      process.exit(1);
    }
  },
});
