#!/usr/bin/env node

/**
 * @file Governance Substrate CLI - Main Entry Point
 * @module governance-substrate-cli
 *
 * @description
 * CLI for governance substrate operations: validate, propose, admit, project
 *
 * Commands:
 * - validate: Validate substrate + policies
 * - propose: Propose a Δ capsule
 * - admit: Run admissibility and emit receipt
 * - project: Project artifacts from admitted universe
 */

import { parseArgs } from 'node:util';
import { readFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';

// Command imports
import { validateCommand } from './commands/validate.mjs';
import { proposeCommand } from './commands/propose.mjs';
import { admitCommand } from './commands/admit.mjs';
import { projectCommand } from './commands/project.mjs';

/**
 * CLI configuration
 */
const CLI_CONFIG = {
  name: 'governance-substrate',
  version: '1.0.0',
  description: 'Governance Substrate CLI - Universe validation and admission control',
};

/**
 * Command registry
 * @type {Map<string, Function>}
 */
const COMMANDS = new Map([
  ['validate', validateCommand],
  ['propose', proposeCommand],
  ['admit', admitCommand],
  ['project', projectCommand],
]);

/**
 * Display help message
 */
function displayHelp() {
  console.log(`
${CLI_CONFIG.name} v${CLI_CONFIG.version}
${CLI_CONFIG.description}

USAGE:
  node ./src/cli.mjs <command> [options]

COMMANDS:
  validate    Validate universe and policy
              --universe <path>  Path to universe TTL file
              --policy <path>    Path to policy TTL file
              --json             Output JSON format

  propose     Propose a Δ capsule
              --delta <path>     Path to delta TTL file
              --json             Output JSON format

  admit       Run admission gate and emit receipt
              --delta <path>     Path to delta TTL file
              --out <path>       Output directory for receipts
              --json             Output JSON format

  project     Project artifacts from admitted universe
              --epoch <id>       Epoch identifier (e.g., τ_001)
              --out <path>       Output directory for artifacts
              --json             Output JSON format

GLOBAL OPTIONS:
  --help, -h         Show this help message
  --version, -v      Show version
  --json             Output machine-readable JSON

EXAMPLES:
  # Validate universe and policy
  node ./src/cli.mjs validate --universe ./ontologies/registry.ttl --policy ./policies/system-policy.ttl

  # Propose a delta
  node ./src/cli.mjs propose --delta ./overlays/bu/studios.delta.ttl --json

  # Admit delta with receipt
  node ./src/cli.mjs admit --delta ./overlays/bu/studios.delta.ttl --out ./receipts/admissions/

  # Project artifacts
  node ./src/cli.mjs project --epoch τ_001 --out ./dist/
`);
}

/**
 * Display version
 */
function displayVersion() {
  console.log(`${CLI_CONFIG.name} v${CLI_CONFIG.version}`);
}

/**
 * Parse command line arguments
 * @param {string[]} args - Process arguments
 * @returns {{command: string, options: Object}}
 */
function parseCliArgs(args) {
  // First arg is node, second is script path, third is command
  const [,, command, ...rest] = args;

  if (!command || command === '--help' || command === '-h') {
    displayHelp();
    process.exit(0);
  }

  if (command === '--version' || command === '-v') {
    displayVersion();
    process.exit(0);
  }

  // Simple argument parser
  const options = {};
  for (let i = 0; i < rest.length; i++) {
    const arg = rest[i];
    if (arg.startsWith('--')) {
      const key = arg.slice(2);
      const nextArg = rest[i + 1];

      if (nextArg && !nextArg.startsWith('--')) {
        options[key] = nextArg;
        i++; // Skip next arg as it's the value
      } else {
        options[key] = true; // Flag without value
      }
    }
  }

  return { command, options };
}

/**
 * Execute CLI command
 * @param {string} command - Command name
 * @param {Object} options - Command options
 * @returns {Promise<void>}
 */
async function executeCli(command, options) {
  const commandFn = COMMANDS.get(command);

  if (!commandFn) {
    console.error(`Error: Unknown command '${command}'`);
    console.error(`Run 'node ./src/cli.mjs --help' for usage information`);
    process.exit(1);
  }

  try {
    const result = await commandFn(options);

    // Output result
    if (options.json) {
      console.log(JSON.stringify(result, null, 2));
    } else {
      displayHumanReadable(command, result);
    }

    // Exit with appropriate code
    let exitCode = 0;

    // Determine exit code based on command result
    if (result.status === 'invalid') {
      exitCode = 1;
    } else if (result.decision === 'deny') {
      exitCode = 1;
    }
    // For propose, admit (allow), and project - exit 0

    process.exit(exitCode);
  } catch (error) {
    console.error(`Error executing command '${command}': ${error.message}`);
    if (options.json) {
      console.log(JSON.stringify({ error: error.message, stack: error.stack }, null, 2));
    }
    process.exit(1);
  }
}

/**
 * Display human-readable output
 * @param {string} command - Command name
 * @param {Object} result - Command result
 */
function displayHumanReadable(command, result) {
  switch (command) {
    case 'validate':
      console.log(`\n=== Validation Result ===`);
      console.log(`Status: ${result.status}`);
      if (result.errors.length > 0) {
        console.log(`\nErrors (${result.errors.length}):`);
        result.errors.forEach((err, i) => console.log(`  ${i + 1}. ${err}`));
      }
      if (result.warnings.length > 0) {
        console.log(`\nWarnings (${result.warnings.length}):`);
        result.warnings.forEach((warn, i) => console.log(`  ${i + 1}. ${warn}`));
      }
      if (result.status === 'valid') {
        console.log(`\n✅ Validation passed`);
      } else {
        console.log(`\n❌ Validation failed`);
      }
      break;

    case 'propose':
      console.log(`\n=== Delta Capsule Proposal ===`);
      console.log(`Capsule ID: ${result.capsuleId}`);
      console.log(`Hash: ${result.hash}`);
      console.log(`\nPreview of additions:`);
      result.preview.forEach(item => console.log(`  - ${item}`));
      break;

    case 'admit':
      console.log(`\n=== Admission Decision ===`);
      console.log(`Decision: ${result.decision.toUpperCase()}`);
      console.log(`Receipt: ${result.receipt.path}`);
      console.log(`Receipt Hash: ${result.receipt.hash}`);
      console.log(`\nReasoning:`);
      result.reasoning.invariants.forEach(inv => {
        console.log(`  - ${inv.name}: ${inv.satisfied ? '✅' : '❌'}`);
      });
      break;

    case 'project':
      console.log(`\n=== Artifact Projection ===`);
      console.log(`Epoch: ${result.epoch || 'latest'}`);
      console.log(`Timestamp: ${new Date(result.timestamp).toISOString()}`);
      console.log(`\nArtifacts (${result.artifacts.length}):`);
      result.artifacts.forEach(artifact => {
        console.log(`  - ${artifact.name}`);
        console.log(`    Path: ${artifact.path}`);
        console.log(`    Hash: ${artifact.hash}`);
      });
      break;
  }
}

/**
 * Main CLI entry point
 */
async function main() {
  const { command, options } = parseCliArgs(process.argv);
  await executeCli(command, options);
}

// Run CLI
main().catch((error) => {
  console.error(`Fatal error: ${error.message}`);
  process.exit(1);
});
