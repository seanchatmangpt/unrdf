#!/usr/bin/env node
/**
 * Format Conversion CLI Example
 * Demonstrates RDF format conversion capabilities
 * @module index
 */

import { defineCommand, runMain } from 'citty';
import { readFileSync, writeFileSync } from 'fs';
import { detectFormat, convertFormat, validateRDF, getFormatInfo } from './converter.mjs';

/**
 * Convert command
 */
const convertCommand = defineCommand({
  meta: {
    name: 'convert',
    description: 'Convert RDF between formats'
  },
  args: {
    input: {
      type: 'positional',
      description: 'Input file path',
      required: true
    },
    output: {
      type: 'string',
      description: 'Output file path',
      required: true
    },
    from: {
      type: 'string',
      description: 'Input format (auto-detected if not specified)'
    },
    to: {
      type: 'string',
      description: 'Output format',
      default: 'turtle'
    }
  },
  async run({ args }) {
    const content = readFileSync(args.input, 'utf-8');
    const inputFormat = args.from || detectFormat(args.input);
    const outputFormat = args.to || detectFormat(args.output);

    console.log(`Converting from ${inputFormat} to ${outputFormat}...`);

    const converted = await convertFormat(content, inputFormat, outputFormat);
    writeFileSync(args.output, converted);

    console.log(`Converted successfully: ${args.output}`);
  }
});

/**
 * Validate command
 */
const validateCommand = defineCommand({
  meta: {
    name: 'validate',
    description: 'Validate RDF syntax'
  },
  args: {
    input: {
      type: 'positional',
      description: 'Input file path',
      required: true
    },
    format: {
      type: 'string',
      description: 'Format to validate (auto-detected if not specified)'
    }
  },
  async run({ args }) {
    const content = readFileSync(args.input, 'utf-8');
    const format = args.format || detectFormat(args.input);

    console.log(`Validating ${args.input} as ${format}...`);

    const isValid = await validateRDF(content, format);

    if (isValid) {
      console.log('✓ Valid RDF');
    } else {
      console.log('✗ Invalid RDF');
      process.exit(1);
    }
  }
});

/**
 * Info command
 */
const infoCommand = defineCommand({
  meta: {
    name: 'info',
    description: 'Display format information'
  },
  args: {
    format: {
      type: 'positional',
      description: 'Format name',
      required: true
    }
  },
  async run({ args }) {
    const info = getFormatInfo(args.format);

    console.log('Format Information:');
    console.log('==================');
    console.log(`Format:     ${info.format}`);
    console.log(`Extensions: ${info.extensions.join(', ')}`);
    console.log(`Supported:  ${info.supported ? 'Yes' : 'No'}`);
  }
});

/**
 * Formats command
 */
const formatsCommand = defineCommand({
  meta: {
    name: 'formats',
    description: 'List supported formats'
  },
  async run() {
    console.log('Supported RDF Formats:');
    console.log('=====================');
    console.log('turtle   - Turtle (.ttl, .turtle)');
    console.log('ntriples - N-Triples (.nt, .ntriples)');
    console.log('nquads   - N-Quads (.nq, .nquads)');
    console.log('trig     - TriG (.trig)');
    console.log('jsonld   - JSON-LD (.jsonld, .json)');
  }
});

/**
 * Main CLI application
 */
const main = defineCommand({
  meta: {
    name: 'rdf-convert',
    version: '1.0.0',
    description: 'RDF format conversion CLI'
  },
  subCommands: {
    convert: convertCommand,
    validate: validateCommand,
    info: infoCommand,
    formats: formatsCommand
  }
});

runMain(main);
