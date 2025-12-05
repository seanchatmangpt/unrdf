/**
 * @file UNRDF init command - Project initialization and wiring
 * @module cli/commands/init
 *
 * @description
 * Orchestrates full project initialization: understands the stack, extracts the domain,
 * infers patterns, installs a generator loop, proves it works, and sets up drift detection.
 *
 * Usage:
 *   unrdf init                    # Initialize current directory
 *   unrdf init --root /path      # Initialize specific path
 *   unrdf init --dry-run         # Preview without applying
 *   unrdf init --verbose         # Show detailed progress
 */

import { defineCommand } from 'citty';
import { resolve } from 'path';
import { cwd } from 'process';
import { createProjectInitializationPipeline } from '../../project-engine/initialize.mjs';

/**
 * Format duration in human-readable format
 * @private
 */
function formatDuration(ms) {
  if (ms < 1000) return `${Math.round(ms)}ms`;
  return `${(ms / 1000).toFixed(2)}s`;
}

/**
 * Format byte size
 * @private
 */
function _formatBytes(bytes) {
  if (bytes < 1024) return `${bytes}B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)}KB`;
  return `${(bytes / (1024 * 1024)).toFixed(1)}MB`;
}

/**
 * Print initialization report
 * @private
 */
function printReport(report) {
  console.log('\n' + '='.repeat(70));
  console.log('  PROJECT INITIALIZATION REPORT');
  console.log('='.repeat(70) + '\n');

  // Stack profile
  const stackProfile = report.stackProfile || 'unknown';
  const frameworks = report.stats?.frameworks?.join(', ') || 'none detected';
  console.log(`📦 Tech Stack: ${stackProfile}`);
  console.log(`   └─ Detected: ${frameworks}`);

  // Features
  console.log(`\n🎯 Features: ${report.stats?.featureCount || 0}`);
  if (report.features && Array.isArray(report.features) && report.features.length > 0) {
    const byRole = {};
    report.features.forEach(f => {
      if (f && f.roles && typeof f.roles === 'object') {
        const roles = Object.entries(f.roles)
          .filter(([, has]) => has)
          .map(([role]) => role);
        roles.forEach(r => {
          byRole[r] = (byRole[r] || 0) + 1;
        });
      }
    });

    Object.entries(byRole).forEach(([role, count]) => {
      console.log(`   └─ ${role}: ${count}`);
    });

    // Features with issues
    const missingTests = report.features.filter(f => f.hasMissingTests);
    if (missingTests.length > 0) {
      console.log(`\n   ⚠️  Missing Tests: ${missingTests.map(f => f.name).join(', ')}`);
    }
  }

  // Domain model
  console.log(`\n📊 Domain Model: ${report.stats?.domainEntityCount || 0} entities`);
  if (
    report.domainEntities &&
    Array.isArray(report.domainEntities) &&
    report.domainEntities.length > 0
  ) {
    report.domainEntities.slice(0, 5).forEach(e => {
      console.log(`   └─ ${e.name} (${e.fieldCount} fields)`);
    });
    if (report.domainEntities.length > 5) {
      console.log(`   └─ ... and ${report.domainEntities.length - 5} more`);
    }
  }

  // Files
  console.log(`\n📄 Files: ${report.stats?.totalFiles || 0}`);
  const byRole = report.stats?.filesByRole || {};
  Object.entries(byRole)
    .slice(0, 5)
    .forEach(([role, count]) => {
      console.log(`   └─ ${role}: ${count}`);
    });
  if (Object.keys(byRole).length > 5) {
    console.log(`   └─ ... and ${Object.keys(byRole).length - 5} more roles`);
  }

  // Test coverage
  if (report.stats?.testCoverageAverage !== undefined) {
    const coverage = Math.round(report.stats.testCoverageAverage);
    const indicator = coverage >= 80 ? '✅' : coverage >= 60 ? '⚠️ ' : '❌';
    console.log(`\n${indicator} Test Coverage: ${coverage}%`);
  }

  // Summary
  if (report.summary) {
    console.log(`\n💡 Summary:\n   ${report.summary}`);
  }

  console.log('\n' + '='.repeat(70));
  console.log('✨ Initialization complete! Your project is now fully wired.\n');
}

/**
 * Print receipt summary
 * @private
 */
function printReceipt(receipt) {
  if (!receipt || !receipt.phases) return;

  console.log('📋 Phase Summary:\n');

  const phases = [
    ['scan', 'File System Scan'],
    ['stackDetection', 'Stack Detection'],
    ['projectModel', 'Project Model'],
    ['fileRoles', 'File Classification'],
    ['domainInference', 'Domain Inference'],
    ['templateInference', 'Template Inference'],
    ['snapshot', 'Baseline Snapshot'],
    ['hooks', 'Hook Registration'],
    ['report', 'Report Generation'],
  ];

  phases.forEach(([key, label]) => {
    const phase = receipt.phases[key];
    if (phase && phase.success) {
      const duration = formatDuration(phase.duration);
      console.log(`   ✓ ${label.padEnd(25)} ${duration.padStart(10)}`);
    } else if (phase) {
      console.log(`   ✗ ${label.padEnd(25)} FAILED`);
    }
  });

  console.log(`\n   Total: ${formatDuration(receipt.totalDuration)}`);
}

/**
 * UNRDF init command
 */
export const initCommand = defineCommand({
  meta: {
    name: 'init',
    description: 'Initialize and wire a project with UNRDF',
  },

  args: {
    root: {
      type: 'string',
      description: 'Project root path (default: current directory)',
      alias: 'r',
    },
    'dry-run': {
      type: 'boolean',
      description: 'Preview changes without applying',
      alias: 'd',
    },
    verbose: {
      type: 'boolean',
      description: 'Show detailed progress',
      alias: 'v',
    },
    'skip-snapshot': {
      type: 'boolean',
      description: 'Skip baseline snapshot creation',
    },
    'skip-hooks': {
      type: 'boolean',
      description: 'Skip hook registration',
    },
  },

  async run(context) {
    try {
      const projectRoot = context.args.root ? resolve(context.args.root) : cwd();
      const dryRun = context.args['dry-run'] || false;
      const verbose = context.args.verbose || false;
      const skipSnapshot = context.args['skip-snapshot'] || false;
      const skipHooks = context.args['skip-hooks'] || false;

      if (verbose) {
        console.log(`\n🚀 Initializing UNRDF project at: ${projectRoot}`);
        console.log(`   Dry-run: ${dryRun ? 'yes' : 'no'}`);
        console.log(`   Verbose: ${verbose ? 'yes' : 'no'}\n`);
      }

      // Run initialization pipeline
      const result = await createProjectInitializationPipeline(projectRoot, {
        dryRun,
        skipSnapshot,
        skipHooks,
        verbose,
      });

      if (!result.success) {
        console.error(`\n❌ Initialization failed!`);
        if (result.error) {
          console.error(`   Error: ${result.error}`);
        }
        process.exit(1);
      }

      // Print receipt
      if (verbose) {
        printReceipt(result.receipt);
      }

      // Print human-readable report
      if (result.report) {
        printReport(result.report);
      }

      // Show next steps
      console.log('📚 Next Steps:\n');
      console.log('   1. Review the generated project structure');
      console.log('   2. Run tests: pnpm test');
      console.log('   3. Build the project: pnpm build');
      console.log('   4. Check drift: unrdf drift --check');
      console.log('   5. Generate new features: unrdf generate --entity <name>\n');

      process.exit(0);
    } catch (error) {
      console.error(`\n❌ Initialization error!`);
      console.error(`   ${error.message}`);
      if (context.args.verbose) {
        console.error(`\n${error.stack}`);
      }
      process.exit(1);
    }
  },
});
