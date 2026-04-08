/**
 * @file Doctor Command - Comprehensive Health Check
 * @module cli/commands/doctor
 *
 * @description
 * Provides comprehensive health checks for the UNRDF development environment
 * and system state. Checks environment, system status, code quality, and integrations.
 */

import { defineCommand } from 'citty';
import { checkEnvironment } from './checks/env.mjs';
import { checkSystem } from './checks/system.mjs';
import { checkQuality } from './checks/quality.mjs';
import { checkIntegrations } from './checks/integration.mjs';
import { checkOTEL } from './checks/otel.mjs';
import { checkKubernetes } from './checks/kubernetes.mjs';
import { formatHuman } from './formatters/human.mjs';
import { formatJSON } from './formatters/json.mjs';
import { formatYAML } from './formatters/yaml.mjs';
import { applyAutoFix } from './fixes/index.mjs';

/**
 * Mode definitions with timeout and check categories
 */
const MODES = {
  quick: {
    timeout: 30000,
    categories: ['env', 'system'],
  },
  standard: {
    timeout: 120000,
    categories: ['env', 'system', 'quality'],
  },
  full: {
    timeout: 300000,
    categories: ['env', 'system', 'quality', 'integration', 'otel', 'kubernetes'],
  },
};

/**
 * Run all checks for a given mode with timeout enforcement
 */
async function runAllChecks(args, categories) {
  const results = {
    categories: [],
    totalChecks: 0,
    passedChecks: 0,
    warnings: 0,
    failedChecks: 0,
  };

  const checkFunctions = {
    env: checkEnvironment,
    system: checkSystem,
    quality: checkQuality,
    integration: checkIntegrations,
    otel: checkOTEL,
    kubernetes: checkKubernetes,
  };

  // Get timeout for current mode (default to standard mode if not found)
  const mode = MODES[args.mode.toLowerCase()] || MODES.standard;
  const modeTimeout = mode.timeout;

  for (const category of categories) {
    const checkFn = checkFunctions[category];
    if (!checkFn) {
      console.warn(`Unknown category: ${category}`);
      continue;
    }

    try {
      // Wrap category check in timeout using Promise.race
      const categoryResult = await Promise.race([
        checkFn(),
        new Promise((_, reject) =>
          setTimeout(
            () => reject(new Error(`Timeout after ${modeTimeout}ms`)),
            modeTimeout
          )
        ),
      ]);

      results.categories.push(categoryResult);

      for (const check of categoryResult.checks) {
        results.totalChecks++;
        if (check.status === 'pass') {
          results.passedChecks++;
        } else if (check.status === 'warn') {
          results.warnings++;
        } else {
          results.failedChecks++;
        }
      }
    } catch (error) {
      // Handle timeout or other errors
      const isTimeout = error.message.includes('Timeout');
      results.categories.push({
        category: category.charAt(0).toUpperCase() + category.slice(1),
        checks: [
          {
            name: 'Category check',
            status: 'error',
            actual: isTimeout
              ? `Category check exceeded ${modeTimeout}ms timeout`
              : error.message,
            expected: `Check to complete within ${modeTimeout}ms`,
            fix: isTimeout
              ? 'Optimize check performance or increase timeout with --mode'
              : 'Check to complete without errors',
          },
        ],
      });
      results.failedChecks++;
    }
  }

  return results;
}

/**
 * Format output based on requested format
 */
function formatOutput(results, format) {
  switch (format) {
    case 'json':
      return formatJSON(results);
    case 'yaml':
      return formatYAML(results);
    case 'human':
    default:
      return formatHuman(results);
  }
}

/**
 * Apply auto-fixes to failed checks
 */
async function applyFixes(results, args) {
  let fixedCount = 0;
  const fixResults = [];

  for (const category of results.categories) {
    for (const check of category.checks) {
      if (check.status !== 'pass' && check.fix) {
        try {
          console.log(`\n🔧 Applying fix for: ${check.name}`);
          await applyAutoFix(check);
          fixedCount++;
          fixResults.push({
            check: check.name,
            status: 'fixed',
          });
        } catch (error) {
          fixResults.push({
            check: check.name,
            status: 'failed',
            error: error.message,
          });
        }
      }
    }
  }

  return { fixedCount, fixResults };
}

/**
 * Watch mode - continuously run checks
 */
async function watchMode(args, categories) {
  const interval = 5000; // 5 seconds

  // Handle Ctrl+C gracefully
  let watching = true;
  process.on('SIGINT', () => {
    console.log('\n\n🛑 Stopping watch mode...');
    watching = false;
  });

  console.log('🔍 Watch mode enabled (refresh every 5s)');
  console.log('Press Ctrl+C to exit\n');

  while (watching) {
    // Clear screen (platform-specific)
    const clearCmd = process.platform === 'win32' ? 'cls' : 'clear';
    try {
      console.log('\x1Bc'); // ANSI clear screen
    } catch {
      // Fallback if clear fails
    }

    // Run checks
    const results = await runAllChecks(args, categories);

    // Format and display
    const formatted = formatOutput(results, args.format);
    console.log(formatted);
    console.log(`\n⏱️  Next refresh in ${interval / 1000}s... (Ctrl+C to exit)`);

    // Wait for interval
    await new Promise((resolve) => setTimeout(resolve, interval));
  }

  console.log('✅ Watch mode stopped');
}

export const doctor = defineCommand({
  meta: {
    name: 'doctor',
    description: 'Comprehensive health check for UNRDF development environment and system state',
  },
  args: {
    mode: {
      type: 'string',
      default: 'standard',
      description: 'Check mode: quick (30s), standard (2min), full (5min)',
      alias: 'm',
    },
    category: {
      type: 'string',
      default: null,
      description: 'Run specific category only (env, system, quality, integration, otel, kubernetes)',
      alias: 'c',
    },
    format: {
      type: 'string',
      default: 'human',
      description: 'Output format: human, json, yaml',
      alias: 'f',
    },
    fix: {
      type: 'boolean',
      default: false,
      description: 'Attempt automatic fixes for issues found',
      alias: 'x',
    },
    watch: {
      type: 'boolean',
      default: false,
      description: 'Continuous monitoring mode (refresh every 5s)',
      alias: 'w',
    },
  },
  async run({ args }) {
    try {
      // Determine categories to check
      let categories;
      if (args.category) {
        categories = [args.category.toLowerCase()];
      } else {
        const mode = MODES[args.mode.toLowerCase()] || MODES.standard;
        categories = mode.categories;
      }

      // Watch mode
      if (args.watch) {
        await watchMode(args, categories);
        return;
      }

      // Single run mode
      const results = await runAllChecks(args, categories);

      // Format output
      const formatted = formatOutput(results, args.format);
      console.log(formatted);

      // Apply fixes if requested
      if (args.fix && results.failedChecks > 0) {
        console.log('\n🔧 Attempting auto-remediation...\n');
        const { fixedCount, fixResults } = await applyFixes(results, args);

        console.log(`\n✅ Fixed ${fixedCount} issue(s)`);
        for (const result of fixResults) {
          if (result.status === 'failed') {
            console.log(`  ❌ ${result.check}: ${result.error}`);
          }
        }
      }

      // Exit with appropriate code
      process.exit(results.failedChecks > 0 ? 1 : 0);
    } catch (error) {
      console.error(`❌ Doctor command failed: ${error.message}`);
      console.error(error.stack);
      process.exit(1);
    }
  },
});
