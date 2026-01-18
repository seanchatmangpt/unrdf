#!/usr/bin/env node
/**
 * @file CLI for RDF graph visualization
 * @module @unrdf/core/cli/graph-viz-cli
 */

import { readFileSync, writeFileSync } from 'node:fs';
import { streamingParse } from '../rdf/n3-justified-only.mjs';
import { createStore } from '../rdf/store.mjs';
import { toDOT, toMermaid, toASCII, toHTML } from '../viz/graph-visualizer.mjs';

/**
 * Parse command line arguments
 * @returns {Object} Parsed arguments
 */
function parseArgs() {
  const args = process.argv.slice(2);

  const config = {
    input: null,
    output: null,
    format: 'ascii',
    limit: null,
    direction: 'TB',
  };

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];

    if (arg === '-i' || arg === '--input') {
      config.input = args[++i];
    } else if (arg === '-o' || arg === '--output') {
      config.output = args[++i];
    } else if (arg === '-f' || arg === '--format') {
      config.format = args[++i];
    } else if (arg === '-l' || arg === '--limit') {
      config.limit = parseInt(args[++i], 10);
    } else if (arg === '-d' || arg === '--direction') {
      config.direction = args[++i];
    } else if (arg === '-h' || arg === '--help') {
      console.log(`
RDF Graph Visualizer CLI

Usage: graph-viz-cli [options]

Options:
  -i, --input <file>      Input RDF file (Turtle/N-Triples)
  -o, --output <file>     Output file (default: stdout)
  -f, --format <format>   Output format: ascii|dot|mermaid|html (default: ascii)
  -l, --limit <n>         Limit number of triples to visualize
  -d, --direction <dir>   Graph direction: TB|LR|BT|RL (default: TB)
  -h, --help              Show this help

Examples:
  graph-viz-cli -i data.ttl
  graph-viz-cli -i data.ttl -f dot -o graph.dot
  graph-viz-cli -i data.ttl -f html -o graph.html
      `);
      process.exit(0);
    }
  }

  return config;
}

/**
 * Load RDF file into store
 * @param {string} filepath - Path to RDF file
 * @returns {Promise<Object>} Store with loaded data
 */
async function loadRDF(filepath) {
  const store = createStore();
  const content = readFileSync(filepath, 'utf-8');
  const quads = await streamingParse(content);

  for (const quad of quads) {
    store.add(quad);
  }

  return store;
}

/**
 * Main CLI function
 */
async function main() {
  try {
    const config = parseArgs();

    if (!config.input) {
      console.error('Error: Input file required. Use --help for usage.');
      process.exit(1);
    }

    const store = await loadRDF(config.input);

    const options = {
      limit: config.limit,
      direction: config.direction,
    };

    let output;
    switch (config.format) {
      case 'dot':
        output = toDOT(store, options);
        break;
      case 'mermaid':
        output = toMermaid(store, options);
        break;
      case 'html':
        output = toHTML(store, options);
        break;
      case 'ascii':
      default:
        output = toASCII(store, options);
        break;
    }

    if (config.output) {
      writeFileSync(config.output, output, 'utf-8');
      console.log(`Visualization saved to ${config.output}`);
    } else {
      console.log(output);
    }
  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  }
}

main();
