#!/usr/bin/env node
/**
 * @file Quality Checks - Automated V6 Code Quality Enforcement
 * @module scripts/quality-checks
 * @description
 * Enforces UNRDF V6 Code Quality Standards with measurable, automated checks.
 * See: docs/standards/V6-CODE-QUALITY-STANDARDS.md
 */

import { readFileSync, readdirSync, statSync, lstatSync, realpathSync } from 'fs';
import { join, relative } from 'path';
import { execSync } from 'child_process';

// =============================================================================
// CONFIGURATION
// =============================================================================

const RULES = {
  maxLines: 500,
  maxLinesTest: 800,
  maxFunctionLines: 50,
  maxExports: 15,
  maxComplexity: 10,
  maxDepth: 3,
  minCoverage: 80,
};

const PATHS = {
  packages: join(process.cwd(), 'packages'),
  exclude: ['node_modules', 'dist', 'build', '.next'],
};

const COLORS = {
  reset: '\x1b[0m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
};

// =============================================================================
// UTILITY FUNCTIONS
// =============================================================================

/**
 * Colorize output
 * @param {string} text - Text to colorize
 * @param {string} color - Color name
 * @returns {string}
 */
function colorize(text, color) {
  return `${COLORS[color] || ''}${text}${COLORS.reset}`;
}

/**
 * Find all .mjs files recursively
 * @param {string} dir - Directory to search
 * @param {string[]} excludeDirs - Directories to exclude
 * @returns {string[]}
 */
function findMjsFiles(dir, excludeDirs = []) {
  const files = [];
  const visited = new Set();

  function walk(currentDir) {
    // Prevent infinite loops from symlinks
    const realPath = realpathSync(currentDir);
    if (visited.has(realPath)) return;
    visited.add(realPath);

    const entries = readdirSync(currentDir);

    for (const entry of entries) {
      const fullPath = join(currentDir, entry);

      try {
        const stat = lstatSync(fullPath);

        // Skip symbolic links
        if (stat.isSymbolicLink()) continue;

        if (stat.isDirectory()) {
          if (!excludeDirs.includes(entry)) {
            walk(fullPath);
          }
        } else if (entry.endsWith('.mjs')) {
          files.push(fullPath);
        }
      } catch (error) {
        // Skip files we can't access
        continue;
      }
    }
  }

  walk(dir);
  return files;
}

/**
 * Count non-comment, non-blank lines
 * @param {string} filePath - File to analyze
 * @returns {number}
 */
function countCodeLines(filePath) {
  const content = readFileSync(filePath, 'utf-8');
  const lines = content.split('\n');

  let inBlockComment = false;
  let codeLines = 0;

  for (const line of lines) {
    const trimmed = line.trim();

    // Skip blank lines
    if (!trimmed) continue;

    // Handle block comments
    if (trimmed.startsWith('/*')) {
      inBlockComment = true;
    }
    if (inBlockComment) {
      if (trimmed.includes('*/')) {
        inBlockComment = false;
      }
      continue;
    }

    // Skip single-line comments
    if (trimmed.startsWith('//')) continue;

    codeLines++;
  }

  return codeLines;
}

/**
 * Count exported functions in file
 * @param {string} filePath - File to analyze
 * @returns {number}
 */
function countExports(filePath) {
  const content = readFileSync(filePath, 'utf-8');
  const exportPattern = /^export\s+(async\s+)?function\s+\w+/gm;
  const matches = content.match(exportPattern) || [];
  return matches.length;
}

/**
 * Check for forbidden patterns
 * @param {string} filePath - File to analyze
 * @returns {Object}
 */
function checkForbiddenPatterns(filePath) {
  const content = readFileSync(filePath, 'utf-8');
  const relativePath = relative(process.cwd(), filePath);
  const issues = [];

  // Check N3 imports (except justified modules)
  if (content.match(/from ['"]n3['"]/)) {
    if (!filePath.includes('n3-justified-only')) {
      issues.push({
        rule: 'N3_IMPORT',
        severity: 'critical',
        message: `Forbidden N3 import. Use @unrdf/oxigraph instead.`,
        line: findLineNumber(content, /from ['"]n3['"]/),
      });
    }
  }

  // Check console.log in src/ directories
  if (filePath.includes('/src/') && !filePath.includes('/cli/')) {
    const consoleMatches = content.match(/console\.(log|warn|error|info)/g);
    if (consoleMatches) {
      issues.push({
        rule: 'CONSOLE_LOG',
        severity: 'high',
        message: `Found ${consoleMatches.length} console.* statements. Use OTEL or structured logger.`,
        count: consoleMatches.length,
      });
    }
  }

  // Check default exports (except config files)
  if (!filePath.match(/\.config\.mjs$/)) {
    if (content.match(/^export\s+default/m)) {
      issues.push({
        rule: 'DEFAULT_EXPORT',
        severity: 'medium',
        message: 'Default export found. Use named exports instead.',
        line: findLineNumber(content, /^export\s+default/m),
      });
    }
  }

  // Check TODO/FIXME without issue reference
  const todoPattern = /\/\/\s*(TODO|FIXME|XXX|HACK)(?!\(#\d+)/gi;
  const todoMatches = content.match(todoPattern);
  if (todoMatches) {
    issues.push({
      rule: 'TODO_WITHOUT_ISSUE',
      severity: 'low',
      message: `Found ${todoMatches.length} TODO/FIXME without issue reference.`,
      count: todoMatches.length,
    });
  }

  return issues;
}

/**
 * Find line number of pattern match
 * @param {string} content - File content
 * @param {RegExp} pattern - Pattern to find
 * @returns {number}
 */
function findLineNumber(content, pattern) {
  const lines = content.split('\n');
  for (let i = 0; i < lines.length; i++) {
    if (pattern.test(lines[i])) {
      return i + 1;
    }
  }
  return -1;
}

/**
 * Analyze function lengths
 * @param {string} filePath - File to analyze
 * @returns {Array}
 */
function analyzeFunctionLengths(filePath) {
  const content = readFileSync(filePath, 'utf-8');
  const lines = content.split('\n');
  const violations = [];

  let inFunction = false;
  let functionName = '';
  let functionStart = 0;
  let braceCount = 0;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Detect function start
    const funcMatch = line.match(/^export\s+(async\s+)?function\s+(\w+)/);
    if (funcMatch) {
      inFunction = true;
      functionName = funcMatch[2];
      functionStart = i + 1;
      braceCount = 0;
    }

    if (inFunction) {
      // Count braces
      for (const char of line) {
        if (char === '{') braceCount++;
        if (char === '}') braceCount--;
      }

      // Function ended
      if (braceCount === 0 && line.includes('}')) {
        const functionLength = i - functionStart + 1;
        if (functionLength > RULES.maxFunctionLines) {
          violations.push({
            name: functionName,
            lines: functionLength,
            start: functionStart,
            end: i + 1,
          });
        }
        inFunction = false;
      }
    }
  }

  return violations;
}

// =============================================================================
// QUALITY CHECKS
// =============================================================================

/**
 * Check file sizes
 * @param {string[]} files - Files to check
 * @returns {Object}
 */
function checkFileSizes(files) {
  const violations = [];

  for (const file of files) {
    const lines = countCodeLines(file);
    const isTest = file.includes('.test.mjs');
    const maxAllowed = isTest ? RULES.maxLinesTest : RULES.maxLines;

    if (lines > maxAllowed) {
      violations.push({
        file: relative(process.cwd(), file),
        lines,
        maxAllowed,
        excess: lines - maxAllowed,
        percentage: Math.round((lines / maxAllowed - 1) * 100),
      });
    }
  }

  return {
    total: files.length,
    violations: violations.length,
    passed: files.length - violations.length,
    details: violations.sort((a, b) => b.lines - a.lines),
  };
}

/**
 * Check export counts
 * @param {string[]} files - Files to check
 * @returns {Object}
 */
function checkExportCounts(files) {
  const violations = [];

  for (const file of files) {
    if (file.includes('.test.mjs')) continue; // Skip tests

    const exports = countExports(file);
    if (exports > RULES.maxExports) {
      violations.push({
        file: relative(process.cwd(), file),
        exports,
        excess: exports - RULES.maxExports,
      });
    }
  }

  return {
    total: files.length,
    violations: violations.length,
    passed: files.length - violations.length,
    details: violations.sort((a, b) => b.exports - a.exports),
  };
}

/**
 * Check forbidden patterns
 * @param {string[]} files - Files to check
 * @returns {Object}
 */
function checkForbiddenPatternsAll(files) {
  const allIssues = [];

  for (const file of files) {
    const issues = checkForbiddenPatterns(file);
    if (issues.length > 0) {
      allIssues.push({
        file: relative(process.cwd(), file),
        issues,
      });
    }
  }

  return {
    total: files.length,
    filesWithIssues: allIssues.length,
    totalIssues: allIssues.reduce((sum, f) => sum + f.issues.length, 0),
    details: allIssues,
  };
}

/**
 * Check function lengths
 * @param {string[]} files - Files to check
 * @returns {Object}
 */
function checkFunctionLengths(files) {
  const allViolations = [];

  for (const file of files) {
    if (file.includes('.test.mjs')) continue; // Skip tests

    const violations = analyzeFunctionLengths(file);
    if (violations.length > 0) {
      allViolations.push({
        file: relative(process.cwd(), file),
        violations,
      });
    }
  }

  return {
    total: files.length,
    filesWithIssues: allViolations.length,
    totalViolations: allViolations.reduce((sum, f) => sum + f.violations.length, 0),
    details: allViolations,
  };
}

// =============================================================================
// REPORTING
// =============================================================================

/**
 * Print report section
 * @param {string} title - Section title
 * @param {Object} results - Check results
 */
function printSection(title, results) {
  console.log(`\n${colorize('═'.repeat(80), 'cyan')}`);
  console.log(colorize(title, 'cyan'));
  console.log(colorize('═'.repeat(80), 'cyan'));

  const passRate = Math.round((results.passed / results.total) * 100);
  const status = results.violations === 0 ? 'PASS' : 'FAIL';
  const statusColor = status === 'PASS' ? 'green' : 'red';

  console.log(`Status: ${colorize(status, statusColor)}`);
  console.log(`Total: ${results.total}`);
  console.log(`Passed: ${colorize(results.passed, 'green')}`);
  console.log(`Violations: ${colorize(results.violations, 'red')}`);
  console.log(`Pass Rate: ${passRate}%`);
}

/**
 * Print file size violations
 * @param {Array} violations - Violations to print
 */
function printFileSizeViolations(violations) {
  if (violations.length === 0) return;

  console.log(`\n${colorize('Top 10 Largest Files:', 'yellow')}`);
  violations.slice(0, 10).forEach((v, i) => {
    console.log(
      `  ${i + 1}. ${v.file} - ${colorize(v.lines, 'red')} lines ` +
      `(+${v.percentage}%, max: ${v.maxAllowed})`
    );
  });
}

/**
 * Print export violations
 * @param {Array} violations - Violations to print
 */
function printExportViolations(violations) {
  if (violations.length === 0) return;

  console.log(`\n${colorize('God Objects (>15 exports):', 'yellow')}`);
  violations.forEach((v, i) => {
    console.log(
      `  ${i + 1}. ${v.file} - ${colorize(v.exports, 'red')} exports ` +
      `(excess: ${v.excess})`
    );
  });
}

/**
 * Print forbidden pattern issues
 * @param {Array} details - Issues to print
 */
function printForbiddenPatterns(details) {
  if (details.length === 0) return;

  console.log(`\n${colorize('Forbidden Patterns:', 'yellow')}`);

  // Group by severity
  const critical = [];
  const high = [];
  const medium = [];
  const low = [];

  for (const { file, issues } of details) {
    for (const issue of issues) {
      const item = { file, ...issue };
      switch (issue.severity) {
        case 'critical': critical.push(item); break;
        case 'high': high.push(item); break;
        case 'medium': medium.push(item); break;
        case 'low': low.push(item); break;
      }
    }
  }

  if (critical.length > 0) {
    console.log(`\n  ${colorize('CRITICAL', 'red')}:`);
    critical.forEach(i => console.log(`    - ${i.file}: ${i.message}`));
  }

  if (high.length > 0) {
    console.log(`\n  ${colorize('HIGH', 'red')}:`);
    high.slice(0, 10).forEach(i => console.log(`    - ${i.file}: ${i.message}`));
    if (high.length > 10) {
      console.log(`    ... and ${high.length - 10} more`);
    }
  }

  if (medium.length > 0) {
    console.log(`\n  ${colorize('MEDIUM', 'yellow')}:`);
    console.log(`    Found ${medium.length} default exports`);
  }

  if (low.length > 0) {
    console.log(`\n  ${colorize('LOW', 'yellow')}:`);
    console.log(`    Found ${low.length} TODO/FIXME without issue reference`);
  }
}

/**
 * Print function length violations
 * @param {Array} details - Violations to print
 */
function printFunctionLengthViolations(details) {
  if (details.length === 0) return;

  console.log(`\n${colorize('Long Functions (>50 lines):', 'yellow')}`);

  const top = details
    .flatMap(f => f.violations.map(v => ({ file: f.file, ...v })))
    .sort((a, b) => b.lines - a.lines)
    .slice(0, 10);

  top.forEach((v, i) => {
    console.log(
      `  ${i + 1}. ${v.file}:${v.name}() - ${colorize(v.lines, 'red')} lines ` +
      `(lines ${v.start}-${v.end})`
    );
  });
}

/**
 * Print summary
 * @param {Object} results - All check results
 */
function printSummary(results) {
  console.log(`\n${colorize('═'.repeat(80), 'cyan')}`);
  console.log(colorize('SUMMARY', 'cyan'));
  console.log(colorize('═'.repeat(80), 'cyan'));

  const checks = [
    { name: 'File Sizes', result: results.fileSizes },
    { name: 'Export Counts', result: results.exports },
    { name: 'Forbidden Patterns', result: results.patterns },
    { name: 'Function Lengths', result: results.functions },
  ];

  const totalChecks = checks.length;
  const passedChecks = checks.filter(c => c.result.violations === 0).length;
  const overallScore = Math.round((passedChecks / totalChecks) * 100);

  console.log('\nCheck Results:');
  checks.forEach(c => {
    const status = c.result.violations === 0 ? '✓' : '✗';
    const color = c.result.violations === 0 ? 'green' : 'red';
    console.log(`  ${colorize(status, color)} ${c.name}: ${c.result.violations} violations`);
  });

  console.log(`\n${colorize('Overall Quality Score:', 'cyan')} ${overallScore}/100`);

  if (overallScore === 100) {
    console.log(colorize('\n🎉 ALL CHECKS PASSED!', 'green'));
  } else if (overallScore >= 90) {
    console.log(colorize('\n✓ Good quality - minor issues', 'yellow'));
  } else if (overallScore >= 70) {
    console.log(colorize('\n⚠ Fair quality - improvements needed', 'yellow'));
  } else {
    console.log(colorize('\n✗ Poor quality - significant issues', 'red'));
  }

  return overallScore;
}

// =============================================================================
// MAIN EXECUTION
// =============================================================================

/**
 * Main execution
 */
async function main() {
  console.log(colorize('UNRDF V6 Code Quality Checks', 'cyan'));
  console.log(colorize('See: docs/standards/V6-CODE-QUALITY-STANDARDS.md\n', 'blue'));

  // Find all .mjs files
  console.log('Scanning codebase...');
  const files = findMjsFiles(PATHS.packages, PATHS.exclude);
  console.log(`Found ${files.length} .mjs files\n`);

  // Run checks
  const results = {
    fileSizes: checkFileSizes(files),
    exports: checkExportCounts(files),
    patterns: checkForbiddenPatternsAll(files),
    functions: checkFunctionLengths(files),
  };

  // Print reports
  printSection('1. FILE SIZE CHECK', results.fileSizes);
  printFileSizeViolations(results.fileSizes.details);

  printSection('2. EXPORT COUNT CHECK', results.exports);
  printExportViolations(results.exports.details);

  printSection('3. FORBIDDEN PATTERNS CHECK', {
    total: results.patterns.total,
    passed: results.patterns.total - results.patterns.filesWithIssues,
    violations: results.patterns.filesWithIssues,
  });
  printForbiddenPatterns(results.patterns.details);

  printSection('4. FUNCTION LENGTH CHECK', {
    total: results.functions.total,
    passed: results.functions.total - results.functions.filesWithIssues,
    violations: results.functions.filesWithIssues,
  });
  printFunctionLengthViolations(results.functions.details);

  // Print summary
  const score = printSummary(results);

  // Exit code based on score
  const exitCode = score >= 90 ? 0 : 1;
  process.exit(exitCode);
}

main().catch(error => {
  console.error(colorize('ERROR:', 'red'), error.message);
  process.exit(1);
});
