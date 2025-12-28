#!/usr/bin/env node
/**
 * @file Statistics counter for UNRDF codebase
 * @module count-stats
 * @description Counts various statistics for quality reporting
 */

import { execSync } from 'child_process';
import { existsSync } from 'fs';

/**
 * Count files matching a pattern
 * @param {string} pattern - Glob pattern
 * @returns {number} File count
 */
function countFiles(pattern) {
  try {
    const result = execSync(`find . -name "${pattern}" -type f | wc -l`, {
      encoding: 'utf-8',
      cwd: process.cwd()
    });
    return parseInt(result.trim(), 10);
  } catch {
    return 0;
  }
}

/**
 * Count lines in files matching pattern
 * @param {string} pattern - File pattern
 * @returns {number} Line count
 */
function countLines(pattern) {
  try {
    const result = execSync(
      `find . -name "${pattern}" -type f -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print $1}'`,
      { encoding: 'utf-8', cwd: process.cwd() }
    );
    return parseInt(result.trim(), 10) || 0;
  } catch {
    return 0;
  }
}

/**
 * Count occurrences of a pattern in files
 * @param {string} searchPattern - Pattern to search for
 * @param {string} filePattern - Files to search in
 * @returns {number} Occurrence count
 */
function countOccurrences(searchPattern, filePattern = '*.mjs') {
  try {
    const result = execSync(
      `grep -r "${searchPattern}" --include="${filePattern}" . 2>/dev/null | wc -l`,
      { encoding: 'utf-8', cwd: process.cwd() }
    );
    return parseInt(result.trim(), 10);
  } catch {
    return 0;
  }
}

/**
 * Get codebase statistics
 * @returns {Object} Statistics object
 */
export function getStats() {
  return {
    files: {
      mjs: countFiles('*.mjs'),
      test: countFiles('*.test.mjs'),
      schema: countFiles('*.schema.mjs'),
      json: countFiles('*.json'),
      md: countFiles('*.md')
    },
    lines: {
      source: countLines('*.mjs'),
      tests: countLines('*.test.mjs')
    },
    patterns: {
      todos: countOccurrences('TODO'),
      fixmes: countOccurrences('FIXME'),
      skippedTests: countOccurrences('it.skip\\|describe.skip', '*.test.mjs'),
      zodImports: countOccurrences("from 'zod'\\|from \"zod\""),
      n3Imports: countOccurrences("from 'n3'\\|from \"n3\""),
      exports: countOccurrences('^export ')
    },
    quality: {
      hasTests: existsSync('test') || countFiles('*.test.mjs') > 0,
      hasLint: existsSync('eslint.config.mjs') || existsSync('.eslintrc.json'),
      hasClaude: existsSync('.claude'),
      hasReadme: existsSync('README.md')
    }
  };
}

/**
 * Format stats for display
 * @param {Object} stats - Statistics object
 * @returns {string} Formatted string
 */
export function formatStats(stats) {
  const lines = [
    '=== UNRDF Codebase Statistics ===',
    '',
    'Files:',
    `  Source files (.mjs): ${stats.files.mjs}`,
    `  Test files: ${stats.files.test}`,
    `  Schema files: ${stats.files.schema}`,
    `  JSON files: ${stats.files.json}`,
    `  Markdown files: ${stats.files.md}`,
    '',
    'Lines of Code:',
    `  Source: ${stats.lines.source.toLocaleString()}`,
    `  Tests: ${stats.lines.tests.toLocaleString()}`,
    '',
    'Code Quality Indicators:',
    `  TODOs: ${stats.patterns.todos} ${stats.patterns.todos > 0 ? '⚠️' : '✓'}`,
    `  FIXMEs: ${stats.patterns.fixmes} ${stats.patterns.fixmes > 0 ? '⚠️' : '✓'}`,
    `  Skipped tests: ${stats.patterns.skippedTests} ${stats.patterns.skippedTests > 0 ? '❌' : '✓'}`,
    `  Zod imports: ${stats.patterns.zodImports}`,
    `  N3 imports: ${stats.patterns.n3Imports} ${stats.patterns.n3Imports > 0 ? '⚠️ (should be 0)' : '✓'}`,
    `  Exports: ${stats.patterns.exports}`,
    '',
    'Configuration:',
    `  Has tests: ${stats.quality.hasTests ? '✓' : '❌'}`,
    `  Has lint config: ${stats.quality.hasLint ? '✓' : '❌'}`,
    `  Has .claude: ${stats.quality.hasClaude ? '✓' : '❌'}`,
    `  Has README: ${stats.quality.hasReadme ? '✓' : '❌'}`,
    ''
  ];

  return lines.join('\n');
}

// CLI interface
if (process.argv[1] === import.meta.url.replace('file://', '')) {
  const stats = getStats();
  console.log(formatStats(stats));

  // Exit with error if quality issues found
  const hasIssues = stats.patterns.todos > 0 ||
                    stats.patterns.skippedTests > 0 ||
                    stats.patterns.n3Imports > 0;

  process.exit(hasIssues ? 1 : 0);
}
