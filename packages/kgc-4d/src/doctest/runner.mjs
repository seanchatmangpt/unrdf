/**
 * Doctest Runner - Orchestrate extraction and transformation for all source files
 * Entry point for doctest generation pipeline
 */

import { writeFileSync, mkdirSync, readdirSync } from 'fs';
import { join } from 'path';
import { extractExamples } from './extractor.mjs';
import { transformToVitest } from './transformer.mjs';

/**
 * Generate doctest files for all source modules
 * @param {Object} options - Configuration {srcDir, testDir, verbose}
 */
export async function generateAllDoctests(options = {}) {
  const {
    srcDir = 'src',
    testDir = 'test/doctest',
    verbose = false
  } = options;

  // Ensure test directory exists
  mkdirSync(testDir, { recursive: true });

  // Get all source files
  const sourceFiles = readdirSync(srcDir)
    .filter(f => f.endsWith('.mjs') && !f.startsWith('doctest'))
    .sort();

  let totalExamples = 0;
  const results = [];

  for (const sourceFile of sourceFiles) {
    const sourcePath = join(srcDir, sourceFile);

    try {
      const examples = extractExamples(sourcePath);

      if (examples.length > 0) {
        const testFileName = sourceFile.replace('.mjs', '.doctest.test.mjs');
        const testPath = join(testDir, testFileName);

        const testCode = transformToVitest(examples, sourceFile);
        writeFileSync(testPath, testCode, 'utf-8');

        totalExamples += examples.length;
        results.push({
          sourceFile,
          testFileName,
          exampleCount: examples.length,
          status: 'generated'
        });

        if (verbose) {
          console.log(`✅ Generated ${testPath} (${examples.length} examples)`);
        }
      } else {
        if (verbose) {
          console.log(`⏭️  No examples in ${sourcePath}`);
        }
      }
    } catch (error) {
      results.push({
        sourceFile,
        status: 'error',
        error: error.message
      });
      console.error(`❌ Error processing ${sourceFile}: ${error.message}`);
    }
  }

  return {
    totalExamples,
    filesProcessed: sourceFiles.length,
    filesWithExamples: results.filter(r => r.status === 'generated').length,
    results,
    success: true
  };
}

/**
 * CLI interface
 */
if (process.argv[1] === new URL(import.meta.url).pathname) {
  const verbose = process.argv.includes('--verbose');
  const srcDir = process.argv[2] || 'src';
  const testDir = process.argv[3] || 'test/doctest';

  try {
    const result = await generateAllDoctests({ srcDir, testDir, verbose });

    console.log(`\n📊 Doctest Generation Summary:`);
    console.log(`   Total examples: ${result.totalExamples}`);
    console.log(`   Files processed: ${result.filesProcessed}`);
    console.log(`   Files with examples: ${result.filesWithExamples}`);

    if (verbose) {
      console.log(`\nResults:`, JSON.stringify(result.results, null, 2));
    }

    process.exit(result.success ? 0 : 1);
  } catch (error) {
    console.error('Fatal error:', error.message);
    process.exit(1);
  }
}
