#!/usr/bin/env node
/**
 * @fileoverview KGC Probe CLI - Knowledge Graph Construction via Receipt-Driven Analysis
 *
 * CLI follows noun-verb pattern:
 *   kgc probe scan --root <path> --out <dir> --budget-ms <n>
 *   kgc probe verify --in <dir>
 *   kgc probe diff <dir1> <dir2>
 *   kgc probe report --in <dir>
 *
 * Design principles:
 * - Deterministic: Same inputs → same outputs
 * - Receipt-driven: Every operation produces verifiable receipts
 * - Composable: Commands can be chained
 * - Fast: Respects timeout SLAs (5s default)
 */

import { parseArgs } from 'node:util';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { createOrchestrator } from '../src/orchestrator.mjs';
import { verifyReceiptChain, verifyManifest } from '../src/receipt.mjs';

/**
 * Parse CLI arguments
 *
 * @param {string[]} argv - Process arguments
 * @returns {Object} - Parsed arguments
 */
function parseCliArgs(argv) {
  const options = {
    root: {
      type: 'string',
      multiple: true,
      short: 'r',
      default: [process.cwd()]
    },
    out: {
      type: 'string',
      short: 'o',
      default: './kgc-output'
    },
    'budget-ms': {
      type: 'string',
      default: '5000'
    },
    samples: {
      type: 'string',
      default: '10'
    },
    'net-allow': {
      type: 'string',
      multiple: true
    },
    in: {
      type: 'string',
      short: 'i'
    },
    help: {
      type: 'boolean',
      short: 'h'
    }
  };

  try {
    const { values, positionals } = parseArgs({
      args: argv.slice(2),
      options,
      allowPositionals: true
    });

    return {
      command: positionals[0],
      subcommand: positionals[1],
      args: positionals.slice(2),
      options: {
        ...values,
        budgetMs: parseInt(values['budget-ms'], 10),
        samples: parseInt(values.samples, 10)
      }
    };
  } catch (err) {
    console.error('Error parsing arguments:', err.message);
    process.exit(1);
  }
}

/**
 * Print usage help
 */
function printHelp() {
  console.log(`
KGC Probe - Knowledge Graph Construction via Receipt-Driven Analysis

USAGE:
  kgc probe scan [options]       Scan codebase and generate observations
  kgc probe verify [options]     Verify receipt chain integrity
  kgc probe diff <dir1> <dir2>   Diff two observation sets
  kgc probe report [options]     Generate human-readable report

SCAN OPTIONS:
  --root, -r <path>       Allowed root path (repeatable)
  --out, -o <dir>         Output directory (default: ./kgc-output)
  --budget-ms <n>         Time budget per agent in ms (default: 5000)
  --samples <n>           Benchmark sample count (default: 10)
  --net-allow <host>      Allowed network host (repeatable)

VERIFY OPTIONS:
  --in, -i <dir>          Input directory to verify

REPORT OPTIONS:
  --in, -i <dir>          Input directory for report

EXAMPLES:
  # Scan current directory
  kgc probe scan --root . --out ./output

  # Scan with custom budget
  kgc probe scan --root ./src --budget-ms 10000

  # Verify scan output
  kgc probe verify --in ./output

  # Generate report
  kgc probe report --in ./output

  # Diff two scans
  kgc probe diff ./scan1 ./scan2
`);
}

/**
 * Execute 'kgc probe scan' command
 *
 * @param {Object} options - CLI options
 */
async function cmdProbeScan(options) {
  console.log('[KGC Probe] Starting scan...');
  console.log(`  Roots: ${options.root.join(', ')}`);
  console.log(`  Output: ${options.out}`);
  console.log(`  Budget: ${options.budgetMs}ms per agent`);
  console.log(`  Samples: ${options.samples}`);

  const orchestrator = createOrchestrator(options);

  // TODO: This is where agents 2-10 will be invoked
  // For now, emit a stub observation to demonstrate the pipeline

  const { createObservation } = await import('../src/observation.mjs');

  const stubObservations = [
    createObservation({
      category: 'file',
      severity: 'info',
      message: 'Package structure initialized (stub from Agent 1)',
      data: {
        packageName: '@unrdf/kgc-probe',
        version: '0.1.0'
      },
      metadata: {
        agentId: 'agent-1-orchestrator',
        probeVersion: '0.1.0',
        budgetMs: options.budgetMs,
        actualMs: 0,
        timestamp: new Date().toISOString()
      },
      tags: ['stub', 'initialization']
    })
  ];

  orchestrator.registerAgentOutput('agent-1-orchestrator', stubObservations);

  const result = await orchestrator.writeOutputs();

  console.log('\n[KGC Probe] Scan complete!');
  console.log(`  Observations: ${result.observationCount}`);
  console.log(`  Receipts: ${result.receiptCount}`);
  console.log(`  Manifest hash: ${result.manifestHash}`);
  console.log(`  Guard denials: ${result.guardDenials}`);
  console.log('\nOutput files:');
  console.log(`  ${result.files.observations}`);
  console.log(`  ${result.files.turtle}`);
  console.log(`  ${result.files.report}`);
  console.log(`  ${result.files.receipts}`);
  console.log(`  ${result.files.manifest}`);
  console.log(`  ${result.files.guardStats}`);
}

/**
 * Execute 'kgc probe verify' command
 *
 * @param {Object} options - CLI options
 */
async function cmdProbeVerify(options) {
  if (!options.in) {
    console.error('Error: --in <dir> is required for verify command');
    process.exit(1);
  }

  console.log(`[KGC Probe] Verifying ${options.in}...`);

  try {
    // Read receipts and manifest
    const receiptsPath = join(options.in, 'receipts.json');
    const manifestPath = join(options.in, 'manifest.json');

    const receiptsData = await readFile(receiptsPath, 'utf-8');
    const manifestData = await readFile(manifestPath, 'utf-8');

    const receipts = JSON.parse(receiptsData);
    const manifest = JSON.parse(manifestData);

    // Verify chain
    const chainValid = verifyReceiptChain(receipts);
    const manifestValid = verifyManifest(manifest, receipts);

    console.log('\nVerification results:');
    console.log(`  Receipt chain: ${chainValid ? '✅ VALID' : '❌ INVALID'}`);
    console.log(`  Manifest: ${manifestValid ? '✅ VALID' : '❌ INVALID'}`);
    console.log(`  Total receipts: ${receipts.length}`);
    console.log(`  Chain hash: ${manifest.chainHash}`);

    if (chainValid && manifestValid) {
      console.log('\n✅ All verifications passed!');
      process.exit(0);
    } else {
      console.log('\n❌ Verification failed!');
      process.exit(1);
    }
  } catch (err) {
    console.error('Verification error:', err.message);
    process.exit(1);
  }
}

/**
 * Execute 'kgc probe diff' command
 *
 * @param {string[]} args - Directory arguments
 */
async function cmdProbeDiff(args) {
  if (args.length < 2) {
    console.error('Error: diff requires two directories');
    console.error('Usage: kgc probe diff <dir1> <dir2>');
    process.exit(1);
  }

  console.log(`[KGC Probe] Diff ${args[0]} vs ${args[1]}...`);

  // TODO: Implement diff logic (compare observations, receipts, manifests)
  console.log('TODO: Diff implementation (stub)');
}

/**
 * Execute 'kgc probe report' command
 *
 * @param {Object} options - CLI options
 */
async function cmdProbeReport(options) {
  if (!options.in) {
    console.error('Error: --in <dir> is required for report command');
    process.exit(1);
  }

  console.log(`[KGC Probe] Generating report for ${options.in}...`);

  try {
    // Check if report.md already exists
    const reportPath = join(options.in, 'report.md');
    const observationsPath = join(options.in, 'observations.json');

    try {
      // Try to read existing report first
      const existingReport = await readFile(reportPath, 'utf-8');
      console.log('\n' + existingReport);
    } catch {
      // If report doesn't exist, generate from observations
      const { renderReport } = await import('../src/reporters/markdown.mjs');
      const observationsData = await readFile(observationsPath, 'utf-8');
      const observations = JSON.parse(observationsData);
      const report = renderReport(observations);
      console.log('\n' + report);
    }
  } catch (err) {
    console.error('Report generation error:', err.message);
    process.exit(1);
  }
}

/**
 * Main CLI entry point
 */
async function main() {
  const { command, subcommand, args, options } = parseCliArgs(process.argv);

  if (options.help || !command) {
    printHelp();
    process.exit(0);
  }

  if (command !== 'probe') {
    console.error(`Unknown command: ${command}`);
    console.error('Expected: kgc probe <subcommand>');
    process.exit(1);
  }

  switch (subcommand) {
    case 'scan':
      await cmdProbeScan(options);
      break;
    case 'verify':
      await cmdProbeVerify(options);
      break;
    case 'diff':
      await cmdProbeDiff(args);
      break;
    case 'report':
      await cmdProbeReport(options);
      break;
    default:
      console.error(`Unknown subcommand: ${subcommand}`);
      console.error('Expected: scan, verify, diff, or report');
      process.exit(1);
  }
}

// Run CLI
main().catch(err => {
  console.error('Fatal error:', err);
  process.exit(1);
});
