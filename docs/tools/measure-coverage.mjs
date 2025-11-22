#!/usr/bin/env node

/**
 * @fileoverview Documentation Coverage Measurement Tool
 *
 * Analyzes source code exports and compares against documentation
 * to calculate API coverage percentage.
 *
 * @module docs/tools/measure-coverage
 * @version 1.0.0
 * @license MIT
 */

import { readFile, readdir, stat, mkdir, writeFile } from 'node:fs/promises';
import { join, dirname, basename, extname } from 'node:path';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Configuration for coverage measurement
 */
const config = {
  sourceModules: [
    '../../src/index.mjs',
    '../../src/knowledge-engine/index.mjs',
    '../../src/react-hooks/index.mjs',
    '../../src/composables/index.mjs',
    '../../src/cli/index.mjs'
  ],
  docsDirectory: '../reference/api/',
  reportsDirectory: './reports/',
  threshold: {
    critical: 80,
    warning: 95,
    target: 100
  }
};

/**
 * Extract exported symbols from a module file
 * @param {string} filePath - Path to the module
 * @returns {Promise<string[]>} Array of export names
 */
async function extractExports(filePath) {
  const absolutePath = join(__dirname, filePath);
  const exports = [];

  try {
    const content = await readFile(absolutePath, 'utf-8');

    // Match named exports: export { name } or export { name as alias }
    const namedExportRegex = /export\s*\{\s*([^}]+)\s*\}/g;
    let match;
    while ((match = namedExportRegex.exec(content)) !== null) {
      const names = match[1].split(',').map((n) => {
        const parts = n.trim().split(/\s+as\s+/);
        return parts[parts.length - 1].trim();
      });
      exports.push(...names.filter((n) => n && n !== '*'));
    }

    // Match direct exports: export function name, export const name, export class name
    const directExportRegex = /export\s+(function|const|let|var|class)\s+(\w+)/g;
    while ((match = directExportRegex.exec(content)) !== null) {
      exports.push(match[2]);
    }

    // Match export * from (re-exports)
    const reExportRegex = /export\s+\*\s+from\s+['"]([^'"]+)['"]/g;
    while ((match = reExportRegex.exec(content)) !== null) {
      const reExportPath = match[1];
      if (!reExportPath.startsWith('@') && !reExportPath.startsWith('node:')) {
        const resolvedPath = join(dirname(filePath), reExportPath);
        const extension = extname(resolvedPath) || '.mjs';
        const fullPath = extension ? resolvedPath : `${resolvedPath}.mjs`;
        try {
          const subExports = await extractExports(fullPath);
          exports.push(...subExports);
        } catch {
          // Ignore unresolvable re-exports
        }
      }
    }
  } catch (error) {
    console.warn(`Warning: Could not read ${filePath}: ${error.message}`);
  }

  return [...new Set(exports)];
}

/**
 * Scan documentation directory for documented APIs
 * @param {string} docsDir - Path to docs directory
 * @returns {Promise<Set<string>>} Set of documented API names
 */
async function scanDocumentation(docsDir) {
  const documented = new Set();
  const absoluteDir = join(__dirname, docsDir);

  /**
   * Recursively scan directory
   * @param {string} dir - Directory to scan
   */
  async function scanDir(dir) {
    try {
      const entries = await readdir(dir, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = join(dir, entry.name);

        if (entry.isDirectory()) {
          await scanDir(fullPath);
        } else if (entry.name.endsWith('.md')) {
          const content = await readFile(fullPath, 'utf-8');

          // Extract API names from markdown headers
          const headerRegex = /^#{1,3}\s+(?:function\s+|class\s+|const\s+)?(\w+)/gm;
          let match;
          while ((match = headerRegex.exec(content)) !== null) {
            documented.add(match[1]);
          }

          // Extract from code blocks with function signatures
          const codeBlockRegex =
            /```(?:javascript|js|mjs)\n(?:export\s+)?(?:function|const|class)\s+(\w+)/gm;
          while ((match = codeBlockRegex.exec(content)) !== null) {
            documented.add(match[1]);
          }

          // Extract from @api or @function annotations
          const annotationRegex = /@(?:api|function|class|method)\s+(\w+)/gm;
          while ((match = annotationRegex.exec(content)) !== null) {
            documented.add(match[1]);
          }
        }
      }
    } catch (error) {
      console.warn(`Warning: Could not scan ${dir}: ${error.message}`);
    }
  }

  await scanDir(absoluteDir);
  return documented;
}

/**
 * Calculate coverage statistics
 * @param {string[]} allExports - All exported symbols
 * @param {Set<string>} documented - Documented symbols
 * @returns {Object} Coverage statistics
 */
function calculateCoverage(allExports, documented) {
  const total = allExports.length;
  const documentedCount = allExports.filter((e) => documented.has(e)).length;
  const missing = allExports.filter((e) => !documented.has(e));
  const coverage = total > 0 ? (documentedCount / total) * 100 : 100;

  return {
    total,
    documented: documentedCount,
    missing,
    coverage: Math.round(coverage * 100) / 100
  };
}

/**
 * Generate coverage report
 * @returns {Promise<Object>} Coverage report
 */
async function generateReport() {
  console.log('Measuring documentation coverage...\n');

  const allExports = [];
  const moduleStats = {};

  // Extract exports from each source module
  for (const modulePath of config.sourceModules) {
    const moduleName = basename(dirname(modulePath));
    const exports = await extractExports(modulePath);
    allExports.push(...exports);
    moduleStats[moduleName] = exports;
    console.log(`  ${moduleName}: ${exports.length} exports`);
  }

  // Scan documentation
  const documented = await scanDocumentation(config.docsDirectory);
  console.log(`\nDocumented APIs found: ${documented.size}`);

  // Calculate overall coverage
  const uniqueExports = [...new Set(allExports)];
  const overall = calculateCoverage(uniqueExports, documented);

  // Calculate per-module coverage
  const moduleReports = {};
  for (const [moduleName, exports] of Object.entries(moduleStats)) {
    moduleReports[moduleName] = calculateCoverage(exports, documented);
  }

  // Determine status
  let status = 'PASS';
  let statusEmoji = '';
  if (overall.coverage < config.threshold.critical) {
    status = 'CRITICAL';
    statusEmoji = '[X]';
  } else if (overall.coverage < config.threshold.warning) {
    status = 'WARNING';
    statusEmoji = '[!]';
  } else {
    statusEmoji = '[OK]';
  }

  const report = {
    timestamp: new Date().toISOString(),
    status,
    overall,
    modules: moduleReports,
    thresholds: config.threshold
  };

  // Print summary
  console.log('\n' + '='.repeat(60));
  console.log('DOCUMENTATION COVERAGE REPORT');
  console.log('='.repeat(60));
  console.log(`\n${statusEmoji} Overall Coverage: ${overall.coverage}%`);
  console.log(`   Total Exports: ${overall.total}`);
  console.log(`   Documented: ${overall.documented}`);
  console.log(`   Missing: ${overall.missing.length}`);

  if (overall.missing.length > 0 && overall.missing.length <= 20) {
    console.log('\nMissing documentation for:');
    for (const name of overall.missing) {
      console.log(`   - ${name}`);
    }
  } else if (overall.missing.length > 20) {
    console.log(`\nMissing documentation for ${overall.missing.length} exports.`);
    console.log('See full report for details.');
  }

  console.log('\nPer-Module Coverage:');
  for (const [name, stats] of Object.entries(moduleReports)) {
    const icon = stats.coverage >= 95 ? '[OK]' : stats.coverage >= 80 ? '[!]' : '[X]';
    console.log(`   ${icon} ${name}: ${stats.coverage}% (${stats.documented}/${stats.total})`);
  }

  // Save report
  const reportsDir = join(__dirname, config.reportsDirectory);
  await mkdir(reportsDir, { recursive: true });

  const reportPath = join(reportsDir, 'coverage-report.json');
  await writeFile(reportPath, JSON.stringify(report, null, 2));
  console.log(`\nReport saved to: ${reportPath}`);

  return report;
}

// Run if executed directly
if (process.argv[1] === fileURLToPath(import.meta.url)) {
  generateReport()
    .then((report) => {
      process.exit(report.status === 'CRITICAL' ? 1 : 0);
    })
    .catch((error) => {
      console.error('Error generating coverage report:', error);
      process.exit(1);
    });
}

export { generateReport, extractExports, scanDocumentation, calculateCoverage };
