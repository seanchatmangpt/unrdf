#!/usr/bin/env node

/**
 * @file UNRDF CLI - Rewritten Modular Implementation
 * @module cli-new
 *
 * @description
 * Production-ready CLI for UNRDF framework with clean separation of concerns,
 * comprehensive error handling, and full testability using citty-test-utils patterns.
 *
 * Key improvements:
 * - Modular command structure for maintainability
 * - Dependency injection for testability
 * - Comprehensive error handling with proper exit codes
 * - Configuration management with validation
 * - Hook coordination for swarm integration
 * - Performance optimized with the critical 20% focus
 */

import { defineCommand, runMain } from 'citty';
import { withContext } from './cli/utils/context-wrapper.mjs';
import { parseCommand, queryCommand, validateCommand } from './cli/commands/index.mjs';

/**
 * Main CLI command with subcommands
 */
const main = defineCommand({
  meta: {
    name: 'unrdf',
    version: '2.0.0',
    description: 'UNRDF - Opinionated composable framework for RDF knowledge operations'
  },
  subCommands: {
    /**
     * Parse command - Parse RDF data from various formats
     */
    parse: defineCommand({
      meta: {
        name: 'parse',
        description: 'Parse RDF data from various formats'
      },
      args: {
        input: {
          type: 'positional',
          description: 'Input file path',
          required: true
        },
        format: {
          type: 'string',
          description: 'Input format (turtle, n-quads)',
          default: 'turtle'
        },
        output: {
          type: 'string',
          description: 'Output file path'
        },
        verbose: {
          type: 'boolean',
          description: 'Enable verbose output',
          default: false
        }
      },
      run: withContext(parseCommand, 'parse')
    }),

    /**
     * Query command - Execute SPARQL queries
     */
    query: defineCommand({
      meta: {
        name: 'query',
        description: 'Query RDF data with SPARQL'
      },
      args: {
        input: {
          type: 'positional',
          description: 'Input file path',
          required: true
        },
        query: {
          type: 'string',
          description: 'SPARQL query string'
        },
        'query-file': {
          type: 'string',
          description: 'Path to SPARQL query file'
        },
        format: {
          type: 'string',
          description: 'Output format (table, json, csv)',
          default: 'table'
        },
        output: {
          type: 'string',
          description: 'Output file path'
        },
        verbose: {
          type: 'boolean',
          description: 'Enable verbose output',
          default: false
        }
      },
      run: withContext(queryCommand, 'query')
    }),

    /**
     * Validate command - Validate RDF against SHACL shapes
     */
    validate: defineCommand({
      meta: {
        name: 'validate',
        description: 'Validate RDF data against SHACL shapes'
      },
      args: {
        data: {
          type: 'positional',
          description: 'Data file path',
          required: true
        },
        shape: {
          type: 'string',
          description: 'SHACL shapes file path',
          required: true
        },
        output: {
          type: 'string',
          description: 'Output file path for validation report'
        },
        verbose: {
          type: 'boolean',
          description: 'Enable verbose output',
          default: false
        }
      },
      run: withContext(validateCommand, 'validate')
    })
  }
});

// Run the CLI
runMain(main);
