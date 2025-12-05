#!/usr/bin/env node

/**
 * Generate Doctests Script - Build-time doctest generation
 * Called via pretest hook to ensure doctests are fresh before each test run
 *
 * Usage: node scripts/generate-doctests.mjs [options]
 * Options:
 *   --verbose    Show detailed output
 *   --srcDir     Source directory (default: src)
 *   --testDir    Test output directory (default: test/doctest)
 */

import { generateAllDoctests } from '../src/doctest/runner.mjs';

async function main() {
  const args = process.argv.slice(2);
  const verbose = args.includes('--verbose');

  // Parse command line arguments - resolve relative to package.json directory
  const packageDir = new URL('../', import.meta.url).pathname;
  let srcDir = args[args.indexOf('--srcDir') + 1] || 'src';
  let testDir = args[args.indexOf('--testDir') + 1] || 'test/doctest';

  // Resolve to absolute paths if not already absolute
  if (!srcDir.startsWith('/')) {
    srcDir = new URL(`../${srcDir}`, import.meta.url).pathname;
  }
  if (!testDir.startsWith('/')) {
    testDir = new URL(`../${testDir}`, import.meta.url).pathname;
  }

  try {
    const result = await generateAllDoctests({ srcDir, testDir, verbose });

    if (verbose) {
      console.log(`\n📊 Doctest Generation Complete:`);
      console.log(`   Total examples: ${result.totalExamples}`);
      console.log(`   Files processed: ${result.filesProcessed}`);
      console.log(`   Files with examples: ${result.filesWithExamples}`);
    }

    process.exit(result.success ? 0 : 1);
  } catch (error) {
    console.error('❌ Doctest generation failed:', error.message);
    if (verbose) {
      console.error(error.stack);
    }
    process.exit(1);
  }
}

main();
