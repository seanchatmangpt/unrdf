/**
 * @file Validate Command
 * @module cli/commands/validate
 *
 * @description
 * Validate RDF data against SHACL shapes with comprehensive reporting.
 */

import { readFile, writeFile } from 'node:fs/promises';
import { useStoreContext } from '../../context/index.mjs';
import { useTurtle, useValidator } from '../../composables/index.mjs';
import { ValidationError } from '../utils/error-handler.mjs';
import { validateRequiredArgs } from '../utils/context-wrapper.mjs';

/**
 * Validate RDF data against SHACL shapes
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function validateCommand(ctx, _config) {
  const { args } = ctx;

  // Validate required arguments
  validateRequiredArgs(args, ['data', 'shape']);

  console.log('✅ Validating RDF data...');

  const store = useStoreContext();
  const turtle = await useTurtle();
  const validator = await useValidator();

  // Load data
  const dataContent = await readFile(args.data, 'utf-8');
  const dataQuads = await turtle.parse(dataContent);
  store.add(...dataQuads);

  // Load shapes
  const shapeContent = await readFile(args.shape, 'utf-8');
  const shapeQuads = await turtle.parse(shapeContent);

  // Validate
  const report = await validator.validate(store.store, shapeQuads);

  // Display results
  if (report.conforms) {
    console.log('✅ Validation passed');
  } else {
    console.log('❌ Validation failed');
    console.log(`Found ${report.results.length} violations:`);

    for (const result of report.results) {
      console.log(`  - ${result.message} (${result.severity})`);
    }

    // Exit with error code if validation fails
    if (!args.output) {
      throw new ValidationError('Data validation failed');
    }
  }

  // Write output if requested
  if (args.output) {
    await writeFile(args.output, JSON.stringify(report, null, 2));
    console.log(`📄 Validation report written to ${args.output}`);
  }
}

/**
 * Export command definition for testing
 */
export const validateCommandMeta = {
  name: 'validate',
  description: 'Validate RDF data against SHACL shapes',
  args: {
    data: {
      type: 'positional',
      description: 'Data file path',
      required: true,
    },
    shape: {
      type: 'string',
      description: 'SHACL shapes file path',
      required: true,
    },
    output: {
      type: 'string',
      description: 'Output file path for validation report',
    },
  },
};
