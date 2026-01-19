/**
 * @file CLI Library Entry Point
 * @module @unrdf/cli
 * @description
 * Main library entry point for the @unrdf/cli package.
 * Exports CLI commands and utilities for programmatic use.
 */

// Re-export the main CLI for programmatic use
// Note: The CLI can be run directly via the `unrdf` command-line binary
// This module provides programmatic access to CLI functionality

export { CommandSchema, CommandResultSchema, executeCommand, testCLIDeterminism } from './cli-receipts.mjs';
