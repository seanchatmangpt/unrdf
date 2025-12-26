#!/usr/bin/env node
/**
 * @file quality-report.mjs
 * @description Code Quality Dashboard - Generates comprehensive quality metrics
 *
 * Metrics tracked (80/20 focus):
 * - JSDoc coverage % (target: 100%)
 * - Test coverage % (target: 80%)
 * - Linting violations (target: 0)
 * - Type errors (target: 0)
 * - Complexity metrics (cyclomatic complexity)
 *
 * Usage:
 *   node scripts/quality-report.mjs [--package=<name>] [--json]
 *
 * @example
 *   # Full workspace report
 *   node scripts/quality-report.mjs
 *
 *   # Single package report
 *   node scripts/quality-report.mjs --package=core
 *
 *   # JSON output for CI
 *   node scripts/quality-report.mjs --json
 */

import { execSync } from 'child_process';
import { readFileSync, existsSync, readdirSync, statSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const rootDir = join(__dirname, '..');

/**
 * Executes command and returns output
 * @param {string} cmd - Command to execute
 * @param {boolean} silent - Suppress errors
 * @returns {string} Command output
 */
function exec(cmd, silent = false) {
  try {
    return execSync(cmd, {
      cwd: rootDir,
      encoding: 'utf8',
      stdio: silent ? 'pipe' : 'inherit'
    });
  } catch (error) {
    if (!silent) throw error;
    return '';
  }
}

/**
 * Analyzes JSDoc coverage in source files
 * @param {string} pkgPath - Package path
 * @returns {{coverage: number, documented: number, total: number}}
 */
function analyzeJSDocCoverage(pkgPath) {
  const srcPath = join(pkgPath, 'src');
  if (!existsSync(srcPath)) {
    return { coverage: 100, documented: 0, total: 0 };
  }

  let totalFunctions = 0;
  let documentedFunctions = 0;

  function analyzeFile(filePath) {
    const content = readFileSync(filePath, 'utf8');
    const lines = content.split('\n');

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();

      // Detect function declarations, exports, and class methods
      const isFunctionDeclaration = /^(export\s+)?(async\s+)?function\s+\w+/.test(line);
      const isMethodDefinition = /^\w+\s*\([^)]*\)\s*\{/.test(line);
      const isClassDeclaration = /^(export\s+)?class\s+\w+/.test(line);

      if (isFunctionDeclaration || isMethodDefinition || isClassDeclaration) {
        totalFunctions++;

        // Check if previous non-empty line is a JSDoc comment
        let hasJSDoc = false;
        for (let j = i - 1; j >= 0; j--) {
          const prevLine = lines[j].trim();
          if (prevLine === '') continue;
          if (prevLine.includes('/**') || prevLine.includes('*/')) {
            hasJSDoc = true;
          }
          break;
        }

        if (hasJSDoc) {
          documentedFunctions++;
        }
      }
    }
  }

  function walkDir(dir) {
    const files = readdirSync(dir);
    for (const file of files) {
      const filePath = join(dir, file);
      const stat = statSync(filePath);

      if (stat.isDirectory()) {
        walkDir(filePath);
      } else if (file.endsWith('.mjs') || file.endsWith('.js')) {
        analyzeFile(filePath);
      }
    }
  }

  walkDir(srcPath);

  const coverage = totalFunctions === 0 ? 100 : Math.round((documentedFunctions / totalFunctions) * 100);
  return { coverage, documented: documentedFunctions, total: totalFunctions };
}

/**
 * Gets test coverage from vitest
 * @param {string} pkgPath - Package path
 * @returns {{coverage: number, lines: number, statements: number, functions: number, branches: number}}
 */
function getTestCoverage(pkgPath) {
  const coverageFile = join(pkgPath, 'coverage', 'coverage-summary.json');

  if (!existsSync(coverageFile)) {
    return { coverage: 0, lines: 0, statements: 0, functions: 0, branches: 0 };
  }

  try {
    const coverage = JSON.parse(readFileSync(coverageFile, 'utf8'));
    const total = coverage.total;

    return {
      coverage: Math.round(total.lines.pct),
      lines: Math.round(total.lines.pct),
      statements: Math.round(total.statements.pct),
      functions: Math.round(total.functions.pct),
      branches: Math.round(total.branches.pct)
    };
  } catch {
    return { coverage: 0, lines: 0, statements: 0, functions: 0, branches: 0 };
  }
}

/**
 * Gets linting violations count
 * @param {string} pkgPath - Package path
 * @returns {{total: number, errors: number, warnings: number}}
 */
function getLintViolations(pkgPath) {
  try {
    const output = exec(`pnpm --dir "${pkgPath}" lint 2>&1`, true);

    // Parse ESLint output for error/warning counts
    const errorMatch = output.match(/(\d+)\s+error/);
    const warningMatch = output.match(/(\d+)\s+warning/);

    const errors = errorMatch ? parseInt(errorMatch[1], 10) : 0;
    const warnings = warningMatch ? parseInt(warningMatch[1], 10) : 0;

    return { total: errors + warnings, errors, warnings };
  } catch {
    return { total: 0, errors: 0, warnings: 0 };
  }
}

/**
 * Analyzes cyclomatic complexity
 * @param {string} pkgPath - Package path
 * @returns {{average: number, max: number, files: number}}
 */
function analyzeComplexity(pkgPath) {
  const srcPath = join(pkgPath, 'src');
  if (!existsSync(srcPath)) {
    return { average: 0, max: 0, files: 0 };
  }

  let totalComplexity = 0;
  let maxComplexity = 0;
  let fileCount = 0;

  function analyzeFile(filePath) {
    const content = readFileSync(filePath, 'utf8');

    // Simple complexity heuristic: count decision points
    const ifCount = (content.match(/\bif\s*\(/g) || []).length;
    const forCount = (content.match(/\bfor\s*\(/g) || []).length;
    const whileCount = (content.match(/\bwhile\s*\(/g) || []).length;
    const caseCount = (content.match(/\bcase\s+/g) || []).length;
    const ternaryCount = (content.match(/\?[^?]/g) || []).length;
    const logicalCount = (content.match(/&&|\|\|/g) || []).length;

    const complexity = 1 + ifCount + forCount + whileCount + caseCount + ternaryCount + Math.floor(logicalCount / 2);

    totalComplexity += complexity;
    maxComplexity = Math.max(maxComplexity, complexity);
    fileCount++;
  }

  function walkDir(dir) {
    const files = readdirSync(dir);
    for (const file of files) {
      const filePath = join(dir, file);
      const stat = statSync(filePath);

      if (stat.isDirectory()) {
        walkDir(filePath);
      } else if (file.endsWith('.mjs') || file.endsWith('.js')) {
        analyzeFile(filePath);
      }
    }
  }

  walkDir(srcPath);

  return {
    average: fileCount === 0 ? 0 : Math.round(totalComplexity / fileCount),
    max: maxComplexity,
    files: fileCount
  };
}

/**
 * Generates quality report for a package
 * @param {string} pkgName - Package name
 * @returns {Object} Quality metrics
 */
function generatePackageReport(pkgName) {
  const pkgPath = join(rootDir, 'packages', pkgName);

  if (!existsSync(pkgPath)) {
    throw new Error(`Package not found: ${pkgName}`);
  }

  const jsdoc = analyzeJSDocCoverage(pkgPath);
  const coverage = getTestCoverage(pkgPath);
  const lint = getLintViolations(pkgPath);
  const complexity = analyzeComplexity(pkgPath);

  // Calculate overall quality score (weighted)
  const score = Math.round(
    (jsdoc.coverage * 0.2) +      // 20% weight on documentation
    (coverage.coverage * 0.4) +    // 40% weight on test coverage
    ((lint.total === 0 ? 100 : Math.max(0, 100 - lint.total * 5)) * 0.2) + // 20% weight on lint
    ((complexity.average <= 10 ? 100 : Math.max(0, 100 - (complexity.average - 10) * 5)) * 0.2) // 20% weight on complexity
  );

  return {
    package: pkgName,
    score,
    jsdoc,
    coverage,
    lint,
    complexity
  };
}

/**
 * Generates full workspace report
 * @returns {Object} Workspace quality metrics
 */
function generateWorkspaceReport() {
  const packagesDir = join(rootDir, 'packages');
  const packages = readdirSync(packagesDir).filter(name => {
    const pkgPath = join(packagesDir, name);
    return statSync(pkgPath).isDirectory();
  });

  const reports = packages.map(pkg => {
    try {
      return generatePackageReport(pkg);
    } catch (error) {
      console.error(`Error analyzing ${pkg}:`, error.message);
      return null;
    }
  }).filter(Boolean);

  // Calculate workspace averages
  const totalScore = reports.reduce((sum, r) => sum + r.score, 0);
  const avgJSDoc = Math.round(reports.reduce((sum, r) => sum + r.jsdoc.coverage, 0) / reports.length);
  const avgCoverage = Math.round(reports.reduce((sum, r) => sum + r.coverage.coverage, 0) / reports.length);
  const totalLintIssues = reports.reduce((sum, r) => sum + r.lint.total, 0);
  const avgComplexity = Math.round(reports.reduce((sum, r) => sum + r.complexity.average, 0) / reports.length);

  return {
    workspace: {
      score: Math.round(totalScore / reports.length),
      packages: reports.length,
      avgJSDoc,
      avgCoverage,
      totalLintIssues,
      avgComplexity
    },
    packages: reports
  };
}

/**
 * Formats report as human-readable text
 * @param {Object} report - Quality report
 */
function formatTextReport(report) {
  console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('  📊 UNRDF Code Quality Dashboard');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

  if (report.workspace) {
    const ws = report.workspace;
    console.log('🎯 Workspace Overview');
    console.log(`   Overall Quality Score: ${ws.score}/100 ${getScoreEmoji(ws.score)}`);
    console.log(`   Packages Analyzed: ${ws.packages}`);
    console.log(`   Avg JSDoc Coverage: ${ws.avgJSDoc}% (target: 100%)`);
    console.log(`   Avg Test Coverage: ${ws.avgCoverage}% (target: 80%)`);
    console.log(`   Total Lint Issues: ${ws.totalLintIssues} (target: 0)`);
    console.log(`   Avg Complexity: ${ws.avgComplexity} (target: ≤10)`);
    console.log('\n' + '─'.repeat(62) + '\n');

    console.log('📦 Package Details\n');
    for (const pkg of report.packages) {
      console.log(`   ${pkg.package}`);
      console.log(`   ├─ Score: ${pkg.score}/100 ${getScoreEmoji(pkg.score)}`);
      console.log(`   ├─ JSDoc: ${pkg.jsdoc.coverage}% (${pkg.jsdoc.documented}/${pkg.jsdoc.total} functions)`);
      console.log(`   ├─ Tests: ${pkg.coverage.coverage}% (L:${pkg.coverage.lines}% S:${pkg.coverage.statements}% F:${pkg.coverage.functions}% B:${pkg.coverage.branches}%)`);
      console.log(`   ├─ Lint: ${pkg.lint.total} issues (${pkg.lint.errors}E, ${pkg.lint.warnings}W)`);
      console.log(`   └─ Complexity: avg ${pkg.complexity.average}, max ${pkg.complexity.max} (${pkg.complexity.files} files)`);
      console.log('');
    }
  } else {
    // Single package report
    const pkg = report;
    console.log(`📦 Package: ${pkg.package}\n`);
    console.log(`   Overall Score: ${pkg.score}/100 ${getScoreEmoji(pkg.score)}\n`);
    console.log(`   📝 JSDoc Coverage: ${pkg.jsdoc.coverage}%`);
    console.log(`      Documented: ${pkg.jsdoc.documented}/${pkg.jsdoc.total} functions`);
    console.log(`      Target: 100%\n`);
    console.log(`   🧪 Test Coverage: ${pkg.coverage.coverage}%`);
    console.log(`      Lines: ${pkg.coverage.lines}%`);
    console.log(`      Statements: ${pkg.coverage.statements}%`);
    console.log(`      Functions: ${pkg.coverage.functions}%`);
    console.log(`      Branches: ${pkg.coverage.branches}%`);
    console.log(`      Target: 80%+\n`);
    console.log(`   ⚠️  Linting Violations: ${pkg.lint.total}`);
    console.log(`      Errors: ${pkg.lint.errors}`);
    console.log(`      Warnings: ${pkg.lint.warnings}`);
    console.log(`      Target: 0\n`);
    console.log(`   🔄 Complexity Metrics:`);
    console.log(`      Average: ${pkg.complexity.average}`);
    console.log(`      Maximum: ${pkg.complexity.max}`);
    console.log(`      Files: ${pkg.complexity.files}`);
    console.log(`      Target: ≤10 average\n`);
  }

  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
}

/**
 * Gets emoji indicator for score
 * @param {number} score - Quality score
 * @returns {string} Emoji
 */
function getScoreEmoji(score) {
  if (score >= 90) return '🟢';
  if (score >= 70) return '🟡';
  if (score >= 50) return '🟠';
  return '🔴';
}

// CLI handler
const args = process.argv.slice(2);
const packageArg = args.find(arg => arg.startsWith('--package='));
const jsonOutput = args.includes('--json');

try {
  let report;

  if (packageArg) {
    const pkgName = packageArg.split('=')[1];
    report = generatePackageReport(pkgName);
  } else {
    report = generateWorkspaceReport();
  }

  if (jsonOutput) {
    console.log(JSON.stringify(report, null, 2));
  } else {
    formatTextReport(report);
  }

  // Exit with error code if quality is below threshold
  const score = report.workspace ? report.workspace.score : report.score;
  process.exit(score >= 70 ? 0 : 1);
} catch (error) {
  console.error('Error generating quality report:', error.message);
  process.exit(1);
}
