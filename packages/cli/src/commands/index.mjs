/**
 * @file CLI Commands - Re-export Aggregator
 * @module @unrdf/cli/commands
 * @description
 * Re-exports all CLI command definitions for programmatic access.
 */

// Init command
export { initCommand } from './init.mjs';

// Graph commands
export { validateCommand } from './graph/validate.mjs';

// Hook commands
export { evalCommand } from './hook/eval.mjs';
