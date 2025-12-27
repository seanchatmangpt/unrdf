#!/usr/bin/env node

/**
 * @fileoverview Dependency Analyzer CLI
 *
 * Command-line interface for the dependency analyzer toolkit.
 * Provides commands for analyzing, visualizing, and optimizing
 * monorepo dependency graphs.
 *
 * @module dependency-analyzer/cli
 *
 * @example
 * ```bash
 * # Full analysis
 * node cli.mjs analyze --root /path/to/monorepo
 *
 * # Quick health check
 * node cli.mjs health
 *
 * # Change impact analysis
 * node cli.mjs impact @unrdf/core
 *
 * # Export to JSON
 * node cli.mjs analyze --format json --output analysis.json
 * ```
 */

import { resolve, dirname } from 'node:path';
import { writeFileSync, existsSync } from 'node:fs';
import { fileURLToPath } from 'node:url';

import {
  analyzeDependencies,
  generateDetailedReports,
  generateGOSReceipt,
  quickHealthCheck,
  getChangeImpact,
  buildDependencyGraph,
  visualizeGraphAsText
} from './index.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));

/**
 * CLI Configuration
 */
const CLI_CONFIG = {
  name: 'dependency-analyzer',
  version: '1.0.0',
  description: 'Monorepo dependency analysis toolkit'
};

/**
 * Available commands
 */
const COMMANDS = {
  analyze: {
    description: 'Full dependency analysis',
    options: {
      '--root, -r': 'Root path of the monorepo (default: current directory)',
      '--format, -f': 'Output format: text, json, or both (default: text)',
      '--output, -o': 'Output file path (default: stdout)',
      '--usage': 'Analyze actual import usage (slower)',
      '--dev': 'Include dev dependencies',
      '--verbose, -v': 'Verbose output'
    }
  },
  health: {
    description: 'Quick health check',
    options: {
      '--root, -r': 'Root path of the monorepo'
    }
  },
  impact: {
    description: 'Analyze change impact for a package',
    options: {
      '--root, -r': 'Root path of the monorepo',
      '<package>': 'Package name to analyze'
    }
  },
  graph: {
    description: 'Visualize dependency graph',
    options: {
      '--root, -r': 'Root path of the monorepo',
      '--output, -o': 'Output file path'
    }
  },
  receipt: {
    description: 'Generate GOS denial/approval receipt',
    options: {
      '--root, -r': 'Root path of the monorepo',
      '--output, -o': 'Output file path'
    }
  }
};

/**
 * Print help message
 */
function printHelp() {
  console.log(`
${CLI_CONFIG.name} v${CLI_CONFIG.version}
${CLI_CONFIG.description}

Usage: node cli.mjs <command> [options]

Commands:
`);

  for (const [name, config] of Object.entries(COMMANDS)) {
    console.log(`  ${name.padEnd(12)} ${config.description}`);
  }

  console.log(`
Options:
  --help, -h     Show this help message
  --version      Show version

Examples:
  node cli.mjs analyze --root /path/to/monorepo
  node cli.mjs health
  node cli.mjs impact @unrdf/core
  node cli.mjs graph --output DEPENDENCY-GRAPH.txt
  node cli.mjs receipt --output receipt.json
`);
}

/**
 * Parse command line arguments
 * @param {string[]} args - Raw arguments
 * @returns {Object} Parsed arguments
 */
function parseArgs(args) {
  const parsed = {
    command: null,
    options: {},
    positional: []
  };

  let i = 0;
  while (i < args.length) {
    const arg = args[i];

    if (arg.startsWith('--')) {
      const key = arg.slice(2);
      const nextArg = args[i + 1];

      if (nextArg && !nextArg.startsWith('-')) {
        parsed.options[key] = nextArg;
        i += 2;
      } else {
        parsed.options[key] = true;
        i++;
      }
    } else if (arg.startsWith('-')) {
      const key = arg.slice(1);
      const nextArg = args[i + 1];

      if (nextArg && !nextArg.startsWith('-')) {
        parsed.options[key] = nextArg;
        i += 2;
      } else {
        parsed.options[key] = true;
        i++;
      }
    } else if (!parsed.command) {
      parsed.command = arg;
      i++;
    } else {
      parsed.positional.push(arg);
      i++;
    }
  }

  // Normalize option aliases
  const aliases = {
    r: 'root',
    f: 'format',
    o: 'output',
    v: 'verbose',
    h: 'help'
  };

  for (const [alias, full] of Object.entries(aliases)) {
    if (parsed.options[alias] && !parsed.options[full]) {
      parsed.options[full] = parsed.options[alias];
      delete parsed.options[alias];
    }
  }

  return parsed;
}

/**
 * Get root path from options or default
 * @param {Object} options - Parsed options
 * @returns {string} Resolved root path
 */
function getRootPath(options) {
  if (options.root) {
    return resolve(options.root);
  }

  // Try to find monorepo root by looking for package.json with workspaces
  let currentDir = process.cwd();
  while (currentDir !== '/') {
    const pkgPath = resolve(currentDir, 'package.json');
    if (existsSync(pkgPath)) {
      try {
        const pkg = JSON.parse(require('fs').readFileSync(pkgPath, 'utf-8'));
        if (pkg.workspaces || pkg.private) {
          return currentDir;
        }
      } catch (e) {
        // Continue searching
      }
    }
    currentDir = dirname(currentDir);
  }

  return process.cwd();
}

/**
 * Command: analyze
 */
async function cmdAnalyze(options, positional) {
  const rootPath = getRootPath(options);
  console.log(`Analyzing: ${rootPath}`);

  const analysisOptions = {
    analyzeUsage: !!options.usage,
    includeDevDependencies: !!options.dev,
    verbose: !!options.verbose
  };

  const analysis = analyzeDependencies(rootPath, analysisOptions);

  const format = options.format || 'text';

  if (format === 'json' || format === 'both') {
    const jsonOutput = JSON.stringify(analysis.json, null, 2);

    if (options.output) {
      const outputPath = format === 'json'
        ? options.output
        : options.output.replace(/\.[^.]+$/, '.json');
      writeFileSync(outputPath, jsonOutput);
      console.log(`\nJSON output written to: ${outputPath}`);
    } else if (format === 'json') {
      console.log(jsonOutput);
    }
  }

  if (format === 'text' || format === 'both') {
    const textOutput = analysis.summary;

    if (options.output && format === 'text') {
      writeFileSync(options.output, textOutput);
      console.log(`\nText output written to: ${options.output}`);
    } else {
      console.log('\n' + textOutput);
    }

    // If verbose, print detailed reports
    if (options.verbose) {
      const reports = generateDetailedReports(analysis);
      console.log('\n' + reports.cycles);
      console.log('\n' + reports.metrics);
      console.log('\n' + reports.optimizations);
    }
  }

  // Exit with error if critical issues
  if (analysis.cycles.hasCycles) {
    process.exitCode = 1;
  }
}

/**
 * Command: health
 */
async function cmdHealth(options) {
  const rootPath = getRootPath(options);
  console.log(`Health check: ${rootPath}\n`);

  const result = quickHealthCheck(rootPath);

  if (result.passed) {
    console.log('[PASS] ' + result.message);
    console.log('\nDetails:');
    console.log(`  Packages: ${result.details.totalPackages}`);
    console.log(`  Dependencies: ${result.details.totalEdges}`);
    console.log('  Cycles: None');
    process.exitCode = 0;
  } else {
    console.log('[FAIL] ' + result.message);
    console.log('\nDetails:');
    console.log(`  Packages: ${result.details.totalPackages}`);
    console.log(`  Dependencies: ${result.details.totalEdges}`);
    console.log(`  Cycles: ${result.details.cycleCount}`);
    process.exitCode = 1;
  }
}

/**
 * Command: impact
 */
async function cmdImpact(options, positional) {
  const rootPath = getRootPath(options);
  const packageName = positional[0];

  if (!packageName) {
    console.error('Error: Package name required');
    console.log('Usage: node cli.mjs impact <package-name>');
    process.exitCode = 1;
    return;
  }

  console.log(`Impact analysis for: ${packageName}\n`);

  const impact = getChangeImpact(rootPath, packageName);

  console.log(`Direct dependents (${impact.direct.length}):`);
  for (const pkg of impact.direct) {
    console.log(`  - ${pkg}`);
  }

  console.log(`\nTransitive dependents (${impact.transitive.length}):`);
  for (const pkg of impact.transitive) {
    console.log(`  - ${pkg}`);
  }

  console.log(`\nTotal impact: ${impact.total} packages would be affected by changes`);
}

/**
 * Command: graph
 */
async function cmdGraph(options) {
  const rootPath = getRootPath(options);
  console.log(`Building graph: ${rootPath}\n`);

  const graph = buildDependencyGraph({ rootPath });
  const visualization = visualizeGraphAsText(graph);

  if (options.output) {
    writeFileSync(options.output, visualization);
    console.log(`Graph written to: ${options.output}`);
  } else {
    console.log(visualization);
  }
}

/**
 * Command: receipt
 */
async function cmdReceipt(options) {
  const rootPath = getRootPath(options);
  console.log(`Generating receipt: ${rootPath}\n`);

  const analysis = analyzeDependencies(rootPath);
  const receipt = generateGOSReceipt(analysis);

  const output = JSON.stringify(receipt, null, 2);

  if (options.output) {
    writeFileSync(options.output, output);
    console.log(`Receipt written to: ${options.output}`);
  } else {
    console.log(output);
  }

  if (receipt.type === 'DENIAL') {
    process.exitCode = 1;
  }
}

/**
 * Main entry point
 */
async function main() {
  const args = process.argv.slice(2);
  const { command, options, positional } = parseArgs(args);

  // Handle help and version
  if (options.help || command === 'help') {
    printHelp();
    return;
  }

  if (options.version) {
    console.log(`${CLI_CONFIG.name} v${CLI_CONFIG.version}`);
    return;
  }

  // Execute command
  try {
    switch (command) {
      case 'analyze':
        await cmdAnalyze(options, positional);
        break;
      case 'health':
        await cmdHealth(options);
        break;
      case 'impact':
        await cmdImpact(options, positional);
        break;
      case 'graph':
        await cmdGraph(options);
        break;
      case 'receipt':
        await cmdReceipt(options);
        break;
      default:
        if (command) {
          console.error(`Unknown command: ${command}`);
        }
        printHelp();
        process.exitCode = 1;
    }
  } catch (error) {
    console.error('Error:', error.message);
    if (options.verbose) {
      console.error(error.stack);
    }
    process.exitCode = 1;
  }
}

// Run if executed directly
main().catch(err => {
  console.error('Fatal error:', err);
  process.exit(1);
});
