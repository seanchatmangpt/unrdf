#!/usr/bin/env node

/**
 * @fileoverview Papers-Thesis CLI - Command-line interface for academic document generation
 *
 * @description
 * Provides CLI commands for generating research papers and theses using
 * templates, SPARQL queries, and RDF knowledge graph operations.
 * Built with citty, nunjucks, and unrdf.
 *
 * @module papers-thesis-cli
 * @version 1.0.0
 * @license MIT
 */

import { defineCommand, runMain } from 'citty';
import { papersCommand } from './commands/papers.mjs';
import { thesisCommand } from './commands/thesis.mjs';
import { configCommand } from './commands/config.mjs';
import { metaCommand } from './commands/meta.mjs';

/**
 * Global CLI arguments available to all commands
 * @type {import('citty').ArgsDef}
 */
export const globalArgs = {
  quiet: {
    type: 'boolean',
    alias: 'q',
    description: 'Suppress non-essential output',
    default: false
  },
  format: {
    type: 'string',
    alias: 'f',
    description: 'Output format (json, yaml, table, latex)',
    default: 'table'
  },
  output: {
    type: 'string',
    alias: 'o',
    description: 'Output file path'
  },
  verbose: {
    type: 'boolean',
    alias: 'v',
    description: 'Enable verbose output',
    default: false
  },
  config: {
    type: 'string',
    alias: 'c',
    description: 'Path to config file'
  }
};

/**
 * Main CLI application
 * @type {import('citty').CommandDef}
 */
const main = defineCommand({
  meta: {
    name: 'playground',
    version: '1.0.0',
    description: 'Papers-Thesis CLI - Generate academic documents with RDF knowledge graphs'
  },
  args: globalArgs,
  subCommands: {
    papers: papersCommand,
    thesis: thesisCommand,
    config: configCommand,
    meta: metaCommand
  },
  async run({ args }) {
    // If no subcommand provided, show help
    if (!args._[0]) {
      console.log(`
Papers-Thesis CLI v1.0.0

Generate academic papers and theses using templates and RDF knowledge graphs.

Commands:
  papers    Manage research papers (generate, list, validate)
  thesis    Manage thesis documents (generate, list, schedule)
  config    Manage CLI configuration
  meta      Introspection and meta operations

Global Options:
  -q, --quiet    Suppress non-essential output
  -f, --format   Output format (json, yaml, table, latex)
  -o, --output   Output file path
  -v, --verbose  Enable verbose output
  -c, --config   Path to config file

Examples:
  playground papers generate --title "My Paper" --author "Alice"
  playground thesis generate --title "PhD Thesis" --author "Bob"
  playground meta sparql "SELECT * WHERE { ?s ?p ?o } LIMIT 10"
  playground config list

Run 'playground <command> --help' for more information on a command.
      `.trim());
    }
  }
});

// Run the CLI
runMain(main);
