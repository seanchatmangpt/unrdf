#!/usr/bin/env node
/**
 * @fileoverview Playground CLI - Main entry point
 *
 * @description
 * Citty + Nunjucks + UNRDF demonstration CLI for academic document management.
 * Provides commands for managing research papers, thesis documents,
 * configuration, and meta operations.
 *
 * @module playground-cli
 * @version 2.0.0
 * @license MIT
 */

import { defineCommand, runMain } from 'citty';
import { papersCommand } from './commands/papers.mjs';
import { thesisCommand } from './commands/thesis.mjs';
import { configCommand } from './commands/config.mjs';
import { metaCommand } from './commands/meta.mjs';

// =============================================================================
// Global Arguments
// =============================================================================

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
  verbose: {
    type: 'boolean',
    alias: 'v',
    description: 'Enable verbose output',
    default: false
  },
  format: {
    type: 'string',
    alias: 'f',
    description: 'Output format (json, yaml, table)',
    default: 'table'
  },
  output: {
    type: 'string',
    alias: 'o',
    description: 'Output file path'
  },
  config: {
    type: 'string',
    alias: 'c',
    description: 'Path to configuration file'
  }
};

// =============================================================================
// Main CLI Definition
// =============================================================================

/**
 * Main CLI command definition
 * @type {import('citty').CommandDef}
 */
const main = defineCommand({
  meta: {
    name: 'playground',
    version: '2.0.0',
    description: 'Citty + Nunjucks + UNRDF playground CLI demonstrating autonomic knowledge graph systems'
  },
  args: globalArgs,
  subCommands: {
    papers: papersCommand,
    thesis: thesisCommand,
    config: configCommand,
    meta: metaCommand
  },
  run({ args, rawArgs }) {
    // Only show help if no subcommand is being executed
    // citty calls run() for all commands in chain, check if we have subcommands
    if (rawArgs && rawArgs.length > 0) {
      return; // Subcommand will handle output
    }

    console.log(`
\x1b[36mPlayground CLI v2.0.0\x1b[0m

Generate academic papers and theses using templates and RDF knowledge graphs.

\x1b[33mCommands:\x1b[0m
  \x1b[32mpapers\x1b[0m    Manage research papers (generate, list, validate, convert, info)
  \x1b[32mthesis\x1b[0m    Manage thesis documents (generate, list, schedule, defense, committee)
  \x1b[32mconfig\x1b[0m    Manage CLI configuration (get, set, list, reset, edit, validate)
  \x1b[32mmeta\x1b[0m      Introspection and meta operations (introspect, ontology, sparql, completions, middleware, telemetry, version)

\x1b[33mGlobal Options:\x1b[0m
  -q, --quiet    Suppress non-essential output
  -v, --verbose  Enable verbose output
  -f, --format   Output format (json, yaml, table)
  -o, --output   Output file path
  -c, --config   Path to config file
  -h, --help     Show help
  --version      Show version

\x1b[33mExamples:\x1b[0m
  \x1b[90m# Generate an IMRAD paper\x1b[0m
  playground papers generate --title "My Paper" --author "Alice"

  \x1b[90m# Generate a PhD thesis with schedule\x1b[0m
  playground thesis generate --title "PhD Thesis" --author "Bob" --schedule

  \x1b[90m# List paper families\x1b[0m
  playground papers list --verbose

  \x1b[90m# Execute SPARQL query\x1b[0m
  playground meta sparql "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

  \x1b[90m# Show configuration\x1b[0m
  playground config list

  \x1b[90m# Generate shell completions\x1b[0m
  playground meta completions bash

Run '\x1b[36mplayground <command> --help\x1b[0m' for more information on a command.
    `.trim());

    if (args.verbose) {
      console.log('\n\x1b[90mVerbose mode enabled\x1b[0m');
    }
  }
});

// =============================================================================
// Run CLI
// =============================================================================

runMain(main);
