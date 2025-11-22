#!/usr/bin/env node

/**
 * @fileoverview UNRDF CLI - Command-line interface for RDF store management
 *
 * @description
 * Provides CLI commands for managing RDF stores including backup, restore,
 * import, query operations. All commands are instrumented with OpenTelemetry
 * for observability and monitoring.
 *
 * @module cli
 * @version 2.1.1
 * @license MIT
 */

import { defineCommand, runMain } from 'citty';
import { storeCommand } from './commands/store.mjs';
import { initCommand } from './commands/init.mjs';

/**
 * Main CLI application
 */
const main = defineCommand({
  meta: {
    name: 'unrdf',
    version: '2.1.1',
    description: 'Autonomic RDF framework CLI - Knowledge Hooks, policy packs, and audit trails'
  },
  subCommands: {
    init: initCommand,
    store: storeCommand
  }
});

// Run the CLI
runMain(main);
