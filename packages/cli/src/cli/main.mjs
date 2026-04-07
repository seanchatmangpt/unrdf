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
import { daemonCommand } from './commands/daemon.mjs';
import { syncCommand } from './commands/sync.mjs';
import { templateCommand } from './commands/template.mjs';
import { hooksCommand } from './commands/hooks.mjs';
import { mcpCommand } from './commands/mcp.mjs';

/**
 * Main CLI application
 */
export const main = defineCommand({
  meta: {
    name: 'unrdf',
    version: '26.4.4',
    description: 'UNRDF CLI - Command-line tools for RDF graph operations',
  },
  subCommands: {
    // RDF Graph Operations
    graph: graphCommand,
    query: queryCommand,
    'query-file': queryFileCommand,
    context: contextCommand,
    convert: convertCommand,
    'to-turtle': toTurtleCommand,
    'to-ntriples': toNTriplesCommand,
    'to-json': toJSONCommand,

    // Knowledge Hooks
    hooks: hooksCommand,

    // Background Operations
    daemon: daemonCommand,
    mcp: mcpCommand,

    // Code Generation
    sync: syncCommand,
    template: templateCommand,
  },
});

// Run the CLI
runMain(main);
