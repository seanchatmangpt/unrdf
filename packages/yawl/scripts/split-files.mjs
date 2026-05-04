#!/usr/bin/env node
/**
 * Automated file splitting script for YAWL package
 * Splits 5 oversized files into smaller modules following SPLITTING_PLAN.md
 *
 * Big Bang 80/20 approach: Well-specified transformation with deterministic rules
 */

import { readFileSync, writeFileSync, mkdirSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const YAWL_ROOT = join(__dirname, '..');

/**
 * Split configuration - defines line ranges for each split
 */
const SPLIT_CONFIG = {
  'test/yawl-patterns.test.mjs': {
    outputDir: 'test/patterns',
    splits: [
      {
        file: 'test-utils.mjs',
        lines: [1, 103],
        description: 'Shared test utilities and helpers'
      },
      {
        file: 'pattern-basic.test.mjs',
        lines: [1, 19, 109, 501], // Header + WP1-WP7
        imports: './test-utils.mjs',
        description: 'Van der Aalst WP1-WP7'
      },
      {
        file: 'pattern-controlflow.test.mjs',
        lines: [1, 19, 507, 664],
        imports: './test-utils.mjs',
        description: 'Control Flow: Cycles, nested conditionals, deferred choice'
      },
      {
        file: 'pattern-resources.test.mjs',
        lines: [1, 19, 670, 827],
        imports: './test-utils.mjs',
        description: 'Resource allocation and pool tests'
      },
      {
        file: 'pattern-cancellation.test.mjs',
        lines: [1, 19, 833, 999],
        imports: './test-utils.mjs',
        description: 'Cancellation tests'
      },
      {
        file: 'pattern-timetravel.test.mjs',
        lines: [1, 19, 1005, 1178],
        imports: './test-utils.mjs',
        description: 'Time-travel and replay tests'
      },
      {
        file: 'pattern-receipts.test.mjs',
        lines: [1, 19, 1184, 1320],
        imports: './test-utils.mjs',
        description: 'Receipt verification tests'
      },
      {
        file: 'pattern-integration.test.mjs',
        lines: [1, 19, 1326, 1530],
        imports: './test-utils.mjs',
        description: 'Integration tests (full lifecycle)'
      },
      {
        file: 'pattern-advanced.test.mjs',
        lines: [1, 19, 1530, 1740],
        imports: './test-utils.mjs',
        description: 'Advanced patterns WP8-WP20'
      }
    ]
  }
};

/**
 * Extract lines from file content
 * @param {string} content - File content
 * @param {number[]} lineRanges - Array of [start, end, start, end, ...] line numbers (1-indexed)
 * @returns {string} Extracted content
 */
function extractLines(content, lineRanges) {
  const lines = content.split('\n');
  const result = [];

  for (let i = 0; i < lineRanges.length; i += 2) {
    const start = lineRanges[i] - 1; // Convert to 0-indexed
    const end = lineRanges[i + 1];
    result.push(lines.slice(start, end).join('\n'));
  }

  return result.join('\n\n');
}

/**
 * Split a single file according to configuration
 * @param {string} sourcePath - Source file path
 * @param {Object} config - Split configuration
 */
function splitFile(sourcePath, config) {
  const fullPath = join(YAWL_ROOT, sourcePath);
  const content = readFileSync(fullPath, 'utf-8');

  // Create output directory
  const outputDir = join(YAWL_ROOT, config.outputDir);
  mkdirSync(outputDir, { recursive: true });

  console.log(`\nSplitting ${sourcePath}:`);
  console.log(`  Output dir: ${config.outputDir}`);
  console.log(`  Splits: ${config.splits.length} files\n`);

  // Create each split
  for (const split of config.splits) {
    const outputPath = join(outputDir, split.file);
    let splitContent = extractLines(content, split.lines);

    // Add import statement if specified
    if (split.imports) {
      const importLine = `\nimport { createTestWorkflow, createTestEngine, measureTime } from '${split.imports}';\n`;
      // Insert after the main imports block (after line ~24 in test files)
      const lines = splitContent.split('\n');
      const insertIndex = lines.findIndex(l => l.includes('from \'../src/index.mjs\'')) + 1;
      lines.splice(insertIndex, 0, importLine);
      splitContent = lines.join('\n');
    }

    writeFileSync(outputPath, splitContent, 'utf-8');

    const lineCount = splitContent.split('\n').length;
    console.log(`  ✓ ${split.file} (${lineCount} lines) - ${split.description}`);

    if (lineCount > 500) {
      console.warn(`    ⚠️  WARNING: File exceeds 500 lines!`);
    }
  }
}

/**
 * Main execution
 */
function main() {
  console.log('YAWL File Splitting Script');
  console.log('===========================\n');

  for (const [sourcePath, config] of Object.entries(SPLIT_CONFIG)) {
    try {
      splitFile(sourcePath, config);
    } catch (error) {
      console.error(`\n❌ Error splitting ${sourcePath}:`);
      console.error(error.message);
      process.exit(1);
    }
  }

  console.log('\n✅ File splitting complete!');
  console.log('\nNext steps:');
  console.log('  1. Review the split files in test/patterns/');
  console.log('  2. Run: timeout 10s pnpm test --filter @unrdf/yawl');
  console.log('  3. Check file sizes: wc -l packages/yawl/test/patterns/*.mjs');
}

main();
