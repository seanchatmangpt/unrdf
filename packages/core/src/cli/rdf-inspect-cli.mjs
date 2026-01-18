#!/usr/bin/env node
/**
 * @file CLI for RDF graph inspection
 * @module @unrdf/core/cli/rdf-inspect-cli
 */

import { readFileSync } from 'node:fs';
import { streamingParse } from '../rdf/n3-justified-only.mjs';
import { createStore } from '../rdf/store.mjs';
import {
  generateInspectionReport,
  getGraphStatistics,
  analyzeNamespaces,
  detectOrphans,
} from '../debug/rdf-inspector.mjs';

/**
 * Parse command line arguments
 * @returns {Object} Parsed arguments
 */
function parseArgs() {
  const args = process.argv.slice(2);

  const config = {
    input: null,
    format: 'report',
    namespaces: false,
    orphans: false,
    json: false,
  };

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];

    if (arg === '-i' || arg === '--input') {
      config.input = args[++i];
    } else if (arg === '--namespaces') {
      config.namespaces = true;
    } else if (arg === '--orphans') {
      config.orphans = true;
    } else if (arg === '--json') {
      config.json = true;
    } else if (arg === '-h' || arg === '--help') {
      console.log(`
RDF Graph Inspector CLI

Usage: rdf-inspect-cli [options]

Options:
  -i, --input <file>      Input RDF file (Turtle/N-Triples)
  --namespaces            Show namespace analysis
  --orphans               Show orphaned nodes
  --json                  Output as JSON
  -h, --help              Show this help

Examples:
  rdf-inspect-cli -i data.ttl
  rdf-inspect-cli -i data.ttl --namespaces --orphans
  rdf-inspect-cli -i data.ttl --json
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

    if (config.json) {
      const stats = getGraphStatistics(store, {
        includeNamespaces: config.namespaces,
        includeOrphans: config.orphans,
        includeQuality: true,
      });
      console.log(JSON.stringify(stats, null, 2));
    } else if (config.namespaces) {
      const namespaces = analyzeNamespaces(store);
      console.log('\nNamespace Analysis:');
      console.log('═══════════════════════════════════════════════════');
      for (const ns of namespaces) {
        console.log(`${ns.namespace.padEnd(50)} ${ns.count} uses`);
      }
    } else if (config.orphans) {
      const orphans = detectOrphans(store);
      console.log('\nOrphaned Nodes:');
      console.log('═══════════════════════════════════════════════════');
      for (const orphan of orphans) {
        console.log(`  ${orphan}`);
      }
      console.log(`\nTotal: ${orphans.length} orphaned node(s)`);
    } else {
      const report = generateInspectionReport(store);
      console.log(report);
    }
  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  }
}

main();
