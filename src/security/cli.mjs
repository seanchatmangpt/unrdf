#!/usr/bin/env node
/**
 * @fileoverview Security CLI - Command-line interface for security scanning
 *
 * Usage:
 *   node src/security/cli.mjs <command> [options]
 *
 * Commands:
 *   quick-check     Run fast security check (secrets + injection)
 *   full-check      Run all security invariant checks
 *   dashboard       Generate security dashboard
 *   secrets         Scan for hardcoded secrets
 *   injection       Scan for injection vulnerabilities
 *   audit           Audit dependencies for CVEs
 *   licenses        Check license compliance
 *   receipts        Validate audit trail receipts
 *
 * @module security/cli
 */

import { parseArgs } from 'node:util';
import { resolve, join } from 'node:path';
import { existsSync, writeFileSync, mkdirSync } from 'node:fs';

import security from './index.mjs';
import secretDetector from './secret-detector.mjs';
import injectionChecker from './injection-checker.mjs';
import dependencyAuditor from './dependency-auditor.mjs';
import licenseChecker from './license-checker.mjs';
import auditTrailValidator from './audit-trail-validator.mjs';
import dashboard from './dashboard.mjs';

/**
 * CLI configuration
 */
const CLI_CONFIG = {
  name: 'security',
  version: '1.0.0',
  description: 'UNRDF Monorepo Security Scanner'
};

/**
 * Parse command-line arguments
 */
function parseCliArgs() {
  const { values, positionals } = parseArgs({
    args: process.argv.slice(2),
    options: {
      help: { type: 'boolean', short: 'h', default: false },
      version: { type: 'boolean', short: 'v', default: false },
      output: { type: 'string', short: 'o' },
      format: { type: 'string', short: 'f', default: 'text' },
      json: { type: 'boolean', default: false },
      quiet: { type: 'boolean', short: 'q', default: false },
      staged: { type: 'boolean', default: false },
      online: { type: 'boolean', default: true },
      'exit-code': { type: 'boolean', default: true }
    },
    allowPositionals: true,
    strict: false
  });

  return {
    command: positionals[0] || 'help',
    directory: positionals[1] || process.cwd(),
    ...values,
    format: values.json ? 'json' : values.format
  };
}

/**
 * Display help message
 */
function showHelp() {
  console.log(`
${CLI_CONFIG.name} v${CLI_CONFIG.version}
${CLI_CONFIG.description}

Usage:
  ${CLI_CONFIG.name} <command> [directory] [options]

Commands:
  quick-check     Fast security check (secrets + injection only)
  full-check      Run all security invariant checks
  dashboard       Generate security dashboard
  secrets         Scan for hardcoded secrets
  injection       Scan for injection vulnerabilities
  audit           Audit dependencies for CVEs
  licenses        Check license compliance
  receipts        Validate audit trail receipts

Options:
  -h, --help         Show this help message
  -v, --version      Show version number
  -o, --output       Output file path
  -f, --format       Output format (text, json, html)
  --json             Output as JSON
  -q, --quiet        Suppress non-essential output
  --staged           Only check staged files (for pre-commit)
  --online           Enable online CVE database check (default: true)
  --exit-code        Exit with non-zero code on failure (default: true)

Examples:
  ${CLI_CONFIG.name} quick-check                    # Quick check current directory
  ${CLI_CONFIG.name} full-check ./src               # Full check src directory
  ${CLI_CONFIG.name} dashboard -o report.html       # Generate HTML dashboard
  ${CLI_CONFIG.name} secrets --json > secrets.json  # Export secrets as JSON
  ${CLI_CONFIG.name} audit --online                 # Online dependency audit
`);
}

/**
 * Display version
 */
function showVersion() {
  console.log(`${CLI_CONFIG.name} v${CLI_CONFIG.version}`);
}

/**
 * Format output based on options
 */
function formatOutput(result, format) {
  switch (format) {
    case 'json':
      return JSON.stringify(result, null, 2);
    case 'html':
      if (result.metrics) {
        return dashboard.formatHtmlReport(result);
      }
      return JSON.stringify(result, null, 2);
    case 'text':
    default:
      if (typeof result.formatReport === 'function') {
        return result.formatReport();
      }
      if (result.summary) {
        return formatSummary(result);
      }
      return JSON.stringify(result, null, 2);
  }
}

/**
 * Format summary as text
 */
function formatSummary(result) {
  const lines = [];

  lines.push(`Status: ${result.passed ? 'PASSED' : 'FAILED'}`);

  if (result.summary) {
    lines.push(`Total: ${result.summary.total || 0}`);

    if (result.summary.bySeverity) {
      lines.push('');
      lines.push('By Severity:');
      for (const [sev, count] of Object.entries(result.summary.bySeverity)) {
        if (count > 0) {
          lines.push(`  ${sev}: ${count}`);
        }
      }
    }
  }

  return lines.join('\n');
}

/**
 * Write output to file or stdout
 */
function writeOutput(content, outputPath, quiet) {
  if (outputPath) {
    const dir = resolve(outputPath, '..');
    if (!existsSync(dir)) {
      mkdirSync(dir, { recursive: true });
    }
    writeFileSync(outputPath, content);
    if (!quiet) {
      console.log(`Output written to: ${outputPath}`);
    }
  } else {
    console.log(content);
  }
}

/**
 * Command: quick-check
 */
async function cmdQuickCheck(args) {
  const directory = resolve(args.directory);

  if (!args.quiet) {
    console.log(`Running quick security check on: ${directory}\n`);
  }

  const result = await security.quickCheck(directory);

  if (args.format === 'json') {
    console.log(JSON.stringify(result, null, 2));
  } else {
    console.log(`Status: ${result.passed ? 'PASSED' : 'FAILED'}`);
    console.log(`Secrets: ${result.secrets.total} (${result.secrets.critical} critical)`);
    console.log(`Injection: ${result.injection.total} (${result.injection.critical} critical)`);
  }

  return result.passed ? 0 : 1;
}

/**
 * Command: full-check
 */
async function cmdFullCheck(args) {
  const directory = resolve(args.directory);

  if (!args.quiet) {
    console.log(`Running full security check on: ${directory}\n`);
  }

  const result = await security.checkAllInvariants(directory);

  const output = args.format === 'json'
    ? JSON.stringify(result, null, 2)
    : security.formatReport(result);

  writeOutput(output, args.output, args.quiet);

  return result.passed ? 0 : 1;
}

/**
 * Command: dashboard
 */
async function cmdDashboard(args) {
  const directory = resolve(args.directory);

  if (!args.quiet) {
    console.log(`Generating security dashboard for: ${directory}\n`);
  }

  const result = await dashboard.generateDashboard(directory);

  let output;
  switch (args.format) {
    case 'json':
      output = dashboard.formatJsonReport(result);
      break;
    case 'html':
      output = dashboard.formatHtmlReport(result);
      break;
    default:
      output = dashboard.formatTextReport(result);
  }

  writeOutput(output, args.output, args.quiet);

  return result.metrics.passed ? 0 : 1;
}

/**
 * Command: secrets
 */
async function cmdSecrets(args) {
  const directory = resolve(args.directory);

  if (!args.quiet) {
    console.log(`Scanning for secrets in: ${directory}\n`);
  }

  const result = await secretDetector.scanDirectory(directory);

  const output = args.format === 'json'
    ? JSON.stringify(result, null, 2)
    : secretDetector.formatReport(result);

  writeOutput(output, args.output, args.quiet);

  return result.passed ? 0 : 1;
}

/**
 * Command: injection
 */
async function cmdInjection(args) {
  const directory = resolve(args.directory);

  if (!args.quiet) {
    console.log(`Scanning for injection vulnerabilities in: ${directory}\n`);
  }

  const result = await injectionChecker.scanDirectory(directory);

  const output = args.format === 'json'
    ? JSON.stringify(result, null, 2)
    : injectionChecker.formatReport(result);

  writeOutput(output, args.output, args.quiet);

  return result.passed ? 0 : 1;
}

/**
 * Command: audit
 */
async function cmdAudit(args) {
  const directory = resolve(args.directory);

  if (!args.quiet) {
    console.log(`Auditing dependencies in: ${directory}\n`);
  }

  const result = await dependencyAuditor.auditDirectory(directory, {
    online: args.online
  });

  const output = args.format === 'json'
    ? JSON.stringify(result, null, 2)
    : dependencyAuditor.formatReport(result);

  writeOutput(output, args.output, args.quiet);

  return result.passed ? 0 : 1;
}

/**
 * Command: licenses
 */
async function cmdLicenses(args) {
  const directory = resolve(args.directory);

  if (!args.quiet) {
    console.log(`Checking licenses in: ${directory}\n`);
  }

  const result = await licenseChecker.checkDirectory(directory);

  const output = args.format === 'json'
    ? JSON.stringify(result, null, 2)
    : licenseChecker.formatReport(result);

  writeOutput(output, args.output, args.quiet);

  return result.passed ? 0 : 1;
}

/**
 * Command: receipts
 */
async function cmdReceipts(args) {
  const directory = resolve(args.directory, 'receipts');

  if (!existsSync(directory)) {
    console.log(`No receipts directory found at: ${directory}`);
    return 0;
  }

  if (!args.quiet) {
    console.log(`Validating receipts in: ${directory}\n`);
  }

  const result = auditTrailValidator.validateDirectory(directory);

  const output = args.format === 'json'
    ? JSON.stringify(result, null, 2)
    : auditTrailValidator.formatReport(result);

  writeOutput(output, args.output, args.quiet);

  return result.passed ? 0 : 1;
}

/**
 * Main entry point
 */
async function main() {
  const args = parseCliArgs();

  if (args.help) {
    showHelp();
    process.exit(0);
  }

  if (args.version) {
    showVersion();
    process.exit(0);
  }

  const commands = {
    'quick-check': cmdQuickCheck,
    'full-check': cmdFullCheck,
    'dashboard': cmdDashboard,
    'secrets': cmdSecrets,
    'injection': cmdInjection,
    'audit': cmdAudit,
    'licenses': cmdLicenses,
    'receipts': cmdReceipts,
    'help': () => { showHelp(); return 0; }
  };

  const handler = commands[args.command];

  if (!handler) {
    console.error(`Unknown command: ${args.command}`);
    console.error('Run with --help for usage information.');
    process.exit(1);
  }

  try {
    const exitCode = await handler(args);

    if (args['exit-code']) {
      process.exit(exitCode);
    }
  } catch (error) {
    console.error(`Error: ${error.message}`);
    if (process.env.DEBUG) {
      console.error(error.stack);
    }
    process.exit(1);
  }
}

main();
