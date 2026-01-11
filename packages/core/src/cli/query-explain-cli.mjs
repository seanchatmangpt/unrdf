#!/usr/bin/env node
/**
 * @file CLI for SPARQL query explanation
 * @module @unrdf/core/cli/query-explain-cli
 */

import { readFileSync } from 'node:fs';
import { explainQuery, formatPlanAsTree } from '../viz/query-explainer.mjs';

/**
 * Parse command line arguments
 * @returns {Object} Parsed arguments
 */
function parseArgs() {
  const args = process.argv.slice(2);

  const config = {
    query: null,
    file: null,
    format: 'tree',
    optimizations: false,
  };

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];

    if (arg === '-q' || arg === '--query') {
      config.query = args[++i];
    } else if (arg === '-f' || arg === '--file') {
      config.file = args[++i];
    } else if (arg === '--format') {
      config.format = args[++i];
    } else if (arg === '--optimizations') {
      config.optimizations = true;
    } else if (arg === '-h' || arg === '--help') {
      console.log(`
SPARQL Query Explainer CLI

Usage: query-explain-cli [options]

Options:
  -q, --query <query>     SPARQL query string
  -f, --file <file>       SPARQL query file
  --format <format>       Output format: tree|json (default: tree)
  --optimizations         Include optimization suggestions
  -h, --help              Show this help

Examples:
  query-explain-cli -q "SELECT * WHERE { ?s ?p ?o }"
  query-explain-cli -f query.sparql --optimizations
      `);
      process.exit(0);
    }
  }

  return config;
}

/**
 * Main CLI function
 */
function main() {
  try {
    const config = parseArgs();

    let query = config.query;

    if (!query && config.file) {
      query = readFileSync(config.file, 'utf-8');
    }

    if (!query) {
      console.error('Error: Query required. Use --help for usage.');
      process.exit(1);
    }

    const plan = explainQuery(query, {
      includeOptimizations: config.optimizations,
    });

    if (config.format === 'json') {
      console.log(JSON.stringify(plan, null, 2));
    } else {
      console.log(formatPlanAsTree(plan));
    }
  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  }
}

main();
