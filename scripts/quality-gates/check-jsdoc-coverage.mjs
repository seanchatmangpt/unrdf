#!/usr/bin/env node
/**
 * JSDoc Coverage Checker
 * Verifies JSDoc type annotation coverage
 */

import { readFileSync, writeFileSync } from 'fs';
import { execSync } from 'child_process';

/**
 * Find all source files
 * @returns {string[]} Array of file paths
 */
function findSourceFiles() {
  try {
    const output = execSync(
      'find packages/*/src -name "*.js" -o -name "*.mjs" | grep -v node_modules',
      { encoding: 'utf8' }
    );
    return output.trim().split('\n').filter(Boolean);
  } catch (error) {
    console.error('Failed to find source files:', error.message);
    return [];
  }
}

/**
 * Check JSDoc coverage in file
 * @param {string} filepath - File path
 * @returns {Object} Coverage stats
 */
function checkFileJSDoc(filepath) {
  try {
    const content = readFileSync(filepath, 'utf8');

    // Count functions
    const functionPattern = /(?:export\s+)?(?:async\s+)?function\s+(\w+)/g;
    const functions = [...content.matchAll(functionPattern)];

    // Count JSDoc comments
    const jsdocPattern = /\/\*\*[\s\S]*?\*\//g;
    const jsdocs = [...content.matchAll(jsdocPattern)];

    return {
      filepath,
      total_functions: functions.length,
      documented_functions: jsdocs.length,
      coverage: functions.length > 0
        ? Math.round((jsdocs.length / functions.length) * 100)
        : 100
    };
  } catch (error) {
    console.warn(`Failed to check ${filepath}:`, error.message);
    return {
      filepath,
      total_functions: 0,
      documented_functions: 0,
      coverage: 0
    };
  }
}

/**
 * Main execution
 */
function main() {
  console.log('Checking JSDoc coverage...\n');

  const files = findSourceFiles();
  const results = files.map(checkFileJSDoc);

  const totalFunctions = results.reduce((sum, r) => sum + r.total_functions, 0);
  const documentedFunctions = results.reduce((sum, r) => sum + r.documented_functions, 0);
  const overallCoverage = totalFunctions > 0
    ? Math.round((documentedFunctions / totalFunctions) * 100)
    : 100;

  // Find poorly documented files
  const poorlyDocumented = results.filter(r => r.coverage < 80 && r.total_functions > 0);

  const summary = {
    coverage: overallCoverage,
    total_files: files.length,
    total_functions: totalFunctions,
    documented_functions: documentedFunctions,
    poorly_documented: poorlyDocumented.length,
    files: results
  };

  // Write results
  writeFileSync('jsdoc-coverage.json', JSON.stringify(summary, null, 2));

  // Report
  console.log('JSDoc Coverage Summary:');
  console.log(`  Overall Coverage: ${overallCoverage}%`);
  console.log(`  Total Files: ${files.length}`);
  console.log(`  Total Functions: ${totalFunctions}`);
  console.log(`  Documented: ${documentedFunctions}`);

  if (poorlyDocumented.length > 0) {
    console.log(`\n⚠️  ${poorlyDocumented.length} files with <80% coverage:`);
    poorlyDocumented.slice(0, 10).forEach(f => {
      console.log(`  - ${f.filepath}: ${f.coverage}%`);
    });
  }

  console.log('\n✅ JSDoc coverage check completed');
  console.log('Results saved to: jsdoc-coverage.json');
}

main();
