#!/usr/bin/env node

/**
 * @file CLI Binary Entry Point
 * @module cli
 * @description
 * Main binary entry point for the @unrdf/cli package.
 * This file is used as the `unrdf` command in package.json bin configuration.
 * Executes the main CLI when run as a binary command.
 */

// Import and execute the main CLI
// Note: main.mjs already contains the runMain() call at the module level
// so we just need to import it to trigger execution
import './cli/main.mjs';
