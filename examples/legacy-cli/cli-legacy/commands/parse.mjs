/**
 * @file Parse Command
 * @module cli/commands/parse
 *
 * @description
 * Parse RDF data from various formats with comprehensive error handling.
 */

import { readFile, writeFile } from 'node:fs/promises';
import { useStoreContext } from '../../context/index.mjs';
import { useTurtle } from '../../composables/index.mjs';
import { ParseError } from '../utils/error-handler.mjs';
import { validateRequiredArgs, getArg } from '../utils/context-wrapper.mjs';

/**
 * Parse RDF data
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function parseCommand(ctx, _config) {
  const { args } = ctx;

  // Validate required arguments
  validateRequiredArgs(args, ['input']);

  console.log('🔄 Parsing RDF data...');

  const store = useStoreContext();
  const turtle = await useTurtle();

  // Read input data
  let inputData;
  try {
    inputData = await readFile(args.input, 'utf-8');
  } catch (error) {
    throw new ParseError(`Failed to read input file: ${args.input}`);
  }

  // Parse based on format
  const format = getArg(args, 'format', 'turtle');
  const startTime = Date.now();

  let quads;
  switch (format) {
    case 'turtle':
      quads = await turtle.parse(inputData);
      break;
    case 'n-quads':
      throw new ParseError('N-Quads parsing not yet implemented');
    default:
      throw new ParseError(`Unsupported format: ${format}`);
  }

  // Add to store
  store.add(...quads);

  const duration = Date.now() - startTime;
  console.log(`✅ Parsed ${quads.length} triples successfully in ${duration}ms`);

  // Write output if requested
  if (args.output) {
    const serialized = await turtle.serialize();
    await writeFile(args.output, serialized);
    console.log(`📄 Output written to ${args.output}`);
  }
}

/**
 * Export command definition for testing
 */
export const parseCommandMeta = {
  name: 'parse',
  description: 'Parse RDF data from various formats',
  args: {
    input: {
      type: 'positional',
      description: 'Input file path',
      required: true,
    },
    format: {
      type: 'string',
      description: 'Input format (turtle, n-quads)',
      default: 'turtle',
    },
    output: {
      type: 'string',
      description: 'Output file path',
    },
  },
};
