#!/usr/bin/env node
/**
 * Graph Commands CLI Example
 * Demonstrates custom CLI commands for graph operations
 * @module index
 */

import { defineCommand, runMain } from 'citty';
import { loadCommand, statsCommand, mergeCommand } from './custom-commands.mjs';

/**
 * Main CLI application
 */
const main = defineCommand({
  meta: {
    name: 'graph-cli',
    version: '1.0.0',
    description: 'Custom graph operations CLI'
  },
  subCommands: {
    load: loadCommand,
    stats: statsCommand,
    merge: mergeCommand
  }
});

runMain(main);
