#!/usr/bin/env node

/**
 * @file OTEL Validation CLI
 * @module scripts/validate-otel
 *
 * @description
 * Run comprehensive OTEL validation for KGC-4D Playground.
 *
 * Usage:
 *   node scripts/validate-otel.mjs [--verbose] [--filter <pattern>]
 *
 * Examples:
 *   node scripts/validate-otel.mjs
 *   node scripts/validate-otel.mjs --verbose
 *   node scripts/validate-otel.mjs --filter persistence
 */

import { runValidationCLI } from '../lib/otel/validation-runner.mjs';

// Parse CLI arguments
const args = process.argv.slice(2);
const options = {
  verbose: args.includes('--verbose'),
  filter: null,
};

// Parse --filter argument
const filterIdx = args.indexOf('--filter');
if (filterIdx !== -1 && filterIdx + 1 < args.length) {
  options.filter = args[filterIdx + 1];
}

// Run validation
console.log('🔍 KGC-4D Playground OTEL Validation Runner\n');
await runValidationCLI(options);
