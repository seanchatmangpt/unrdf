#!/usr/bin/env node

/**
 * @fileoverview UNRDF CLI - Command-line interface for RDF operations
 *
 * @description
 * Main entry point for the @unrdf/cli package.
 * Provides commands for graph management, querying, context, and conversion.
 *
 * @module cli/main
 */

import { defineCommand, runMain } from 'citty';
import { graphCommand } from './commands/graph.mjs';
import { queryCommand, queryFileCommand } from './commands/query.mjs';
import { contextCommand } from './commands/context.mjs';
import {
  convertCommand,
  toTurtleCommand,
  toNTriplesCommand,
  toJSONCommand,
} from './commands/convert.mjs';

/**
 * Main CLI application
 */
const main = defineCommand({
  meta: {
    name: 'unrdf',
    version: '5.0.0-alpha.0',
    description: 'UNRDF CLI - Command-line tools for RDF graph operations',
  },
  subCommands: {
    graph: graphCommand,
    query: queryCommand,
    'query-file': queryFileCommand,
    context: contextCommand,
    convert: convertCommand,
    'to-turtle': toTurtleCommand,
    'to-ntriples': toNTriplesCommand,
    'to-json': toJSONCommand,
  },
});

// Run the CLI
runMain(main);
