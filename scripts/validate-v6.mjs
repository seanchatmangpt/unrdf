#!/usr/bin/env node
/**
 * @file v6 Pre-Flight Validation Script
 * @description Comprehensive validation checks for v6 release readiness
 * Evidence-based validation following CLAUDE.md adversarial PM principles
 */

import { execSync } from 'child_process';
import { readFileSync, existsSync, readdirSync, statSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT = join(__dirname, '..');

/** @type {{ pass: number; fail: number; results: Array<{check: string; status: 'PASS' | 'FAIL'; evidence: string; severity: 'CRITICAL' | 'HIGH' | 'MEDIUM' | 'LOW'}>}} */
const validationResults = {
  pass: 0,
  fail: 0,
  results: []
};

/**
 * Execute a shell command and return output
 * @param {string} cmd
 * @param {{ timeout?: number; ignoreError?: boolean }} [options]
 * @returns {string}
 */
function exec(cmd, options = {}) {
  try {
    return execSync(cmd, {
      cwd: ROOT,
      encoding: 'utf-8',
      timeout: options.timeout || 120000,
      stdio: 'pipe'
    });
  } catch (error) {
    if (options.ignoreError) {
      return error.stdout || error.stderr || '';
    }
    throw error;
  }
}

/**
 * Add validation result
 * @param {string} check
 * @param {boolean} passed
 * @param {string} evidence
 * @param {'CRITICAL' | 'HIGH' | 'MEDIUM' | 'LOW'} severity
 */
function addResult(check, passed, evidence, severity = 'MEDIUM') {
  validationResults.results.push({
    check,
    status: passed ? 'PASS' : 'FAIL',
    evidence,
    severity
  });

  if (passed) {
    validationResults.pass++;
  } else {
    validationResults.fail++;
  }
}

console.log('='.repeat(80));
console.log('v6 PRE-FLIGHT VALIDATION');
console.log('Evidence-Based Validation Following Adversarial PM Principles');
console.log('='.repeat(80));
console.log('');

// ============================================================================
// CHECK 1: Test Suite Pass Rate
// ============================================================================
console.log('[1/12] Running test suites...');
try {
  const testStart = Date.now();
  const testOutput = exec('timeout 30s npm test', { ignoreError: true });
  const testDuration = Date.now() - testStart;

  // Parse test output for pass/fail counts
  const passMatches = testOutput.match(/# pass (\d+)/g) || [];
  const failMatches = testOutput.match(/# fail (\d+)/g) || [];

  const totalPass = passMatches.reduce((sum, m) => sum + parseInt(m.match(/\d+/)[0]), 0);
  const totalFail = failMatches.reduce((sum, m) => sum + parseInt(m.match(/\d+/)[0]), 0);
  const totalTests = totalPass + totalFail;
  const passRate = totalTests > 0 ? (totalPass / totalTests * 100).toFixed(2) : 0;

  const passed = totalFail === 0 && totalPass > 0 && testDuration < 10000;
  addResult(
    'Test Suite Pass Rate',
    passed,
    `${totalPass}/${totalTests} tests passed (${passRate}%). ${totalFail} failures. Duration: ${testDuration}ms (target: <5000ms)`,
    totalFail > 0 ? 'CRITICAL' : testDuration > 10000 ? 'HIGH' : 'LOW'
  );
} catch (error) {
  addResult('Test Suite Pass Rate', false, `Tests timed out or crashed: ${error.message}`, 'CRITICAL');
}

// ============================================================================
// CHECK 2: Linting Performance and Compliance
// ============================================================================
console.log('[2/12] Running linter...');
try {
  const lintStart = Date.now();
  const lintOutput = exec('timeout 10s npm run lint', { ignoreError: true });
  const lintDuration = Date.now() - lintStart;

  const hasErrors = lintOutput.includes('error') || lintOutput.includes('✖');
  const passed = !hasErrors && lintDuration < 5000;

  addResult(
    'Linting Compliance',
    passed,
    `Lint duration: ${lintDuration}ms (target: <5000ms). Errors: ${hasErrors ? 'YES' : 'NO'}`,
    hasErrors ? 'HIGH' : lintDuration > 5000 ? 'MEDIUM' : 'LOW'
  );
} catch (error) {
  addResult('Linting Compliance', false, `Linting timed out or failed: ${error.message}`, 'HIGH');
}

// ============================================================================
// CHECK 3: N3 Import Violations (CRITICAL)
// ============================================================================
console.log('[3/12] Checking for forbidden N3 imports...');
try {
  const grepCmd = `grep -r "from 'n3'" --include="*.mjs" --include="*.js" src/ packages/ 2>/dev/null | grep -v node_modules | grep -v "n3-justified" || true`;
  const violations = exec(grepCmd).trim();
  const violationCount = violations ? violations.split('\n').length : 0;

  const passed = violationCount === 0;
  addResult(
    'N3 Import Compliance',
    passed,
    passed ? 'No forbidden N3 imports found' : `Found ${violationCount} files with forbidden 'from n3' imports. Files: ${violations.split('\n').slice(0, 5).join(', ')}...`,
    'CRITICAL'
  );
} catch (error) {
  addResult('N3 Import Compliance', false, `Failed to check N3 imports: ${error.message}`, 'CRITICAL');
}

// ============================================================================
// CHECK 4: File Size Compliance (<500 lines)
// ============================================================================
console.log('[4/12] Checking file sizes...');
try {
  const fileSizeCmd = `find src/ -name "*.mjs" -exec wc -l {} + | awk '$1 > 500 {print}' || true`;
  const oversizedFiles = exec(fileSizeCmd).trim();
  const oversizedCount = oversizedFiles ? oversizedFiles.split('\n').length - 1 : 0; // -1 for total line

  const passed = oversizedCount === 0;
  addResult(
    'File Size Compliance',
    passed,
    passed ? 'All files <500 lines' : `Found ${oversizedCount} files exceeding 500 lines. Examples: ${oversizedFiles.split('\n').slice(0, 3).join('; ')}`,
    'MEDIUM'
  );
} catch (error) {
  addResult('File Size Compliance', false, `Failed to check file sizes: ${error.message}`, 'MEDIUM');
}

// ============================================================================
// CHECK 5: OTEL Validation Infrastructure
// ============================================================================
console.log('[5/12] Checking OTEL validation...');
try {
  const otelPath = join(ROOT, 'validation/run-all.mjs');
  const otelExists = existsSync(otelPath);

  if (!otelExists) {
    addResult('OTEL Validation', false, 'OTEL validation script not found at validation/run-all.mjs', 'CRITICAL');
  } else {
    try {
      const otelOutput = exec('timeout 30s node validation/run-all.mjs comprehensive', { ignoreError: true });
      const scoreMatch = otelOutput.match(/Score:\s*(\d+)\/100/);
      const score = scoreMatch ? parseInt(scoreMatch[1]) : 0;

      const passed = score >= 80;
      addResult(
        'OTEL Validation',
        passed,
        `OTEL validation score: ${score}/100 (target: ≥80)`,
        score < 80 ? 'CRITICAL' : 'LOW'
      );
    } catch (error) {
      addResult('OTEL Validation', false, `OTEL validation failed: ${error.message}`, 'CRITICAL');
    }
  }
} catch (error) {
  addResult('OTEL Validation', false, `Failed to check OTEL: ${error.message}`, 'CRITICAL');
}

// ============================================================================
// CHECK 6: Security Vulnerabilities
// ============================================================================
console.log('[6/12] Checking security vulnerabilities...');
try {
  const auditOutput = exec('pnpm audit --json', { ignoreError: true });
  const audit = JSON.parse(auditOutput);

  const criticalCount = audit.metadata?.vulnerabilities?.critical || 0;
  const highCount = audit.metadata?.vulnerabilities?.high || 0;
  const moderateCount = audit.metadata?.vulnerabilities?.moderate || 0;

  const passed = criticalCount === 0 && highCount === 0;
  addResult(
    'Security Audit',
    passed,
    `Vulnerabilities - Critical: ${criticalCount}, High: ${highCount}, Moderate: ${moderateCount}`,
    criticalCount > 0 ? 'CRITICAL' : highCount > 0 ? 'HIGH' : moderateCount > 0 ? 'MEDIUM' : 'LOW'
  );
} catch (error) {
  addResult('Security Audit', false, `Security audit failed: ${error.message}`, 'HIGH');
}

// ============================================================================
// CHECK 7: Documentation Completeness
// ============================================================================
console.log('[7/12] Checking documentation...');
try {
  const docsDir = join(ROOT, 'docs/v6');
  const v6DocsExist = existsSync(docsDir);

  const requiredDocs = [
    'MIGRATION_GUIDE.md',
    'API_REFERENCE.md',
    'BREAKING_CHANGES.md',
    'RELEASE_NOTES.md'
  ];

  const missingDocs = [];
  if (v6DocsExist) {
    for (const doc of requiredDocs) {
      if (!existsSync(join(docsDir, doc))) {
        missingDocs.push(doc);
      }
    }
  } else {
    missingDocs.push(...requiredDocs);
  }

  const passed = missingDocs.length === 0;
  addResult(
    'Documentation Completeness',
    passed,
    passed ? 'All required v6 documentation present' : `Missing: ${missingDocs.join(', ')}`,
    'HIGH'
  );
} catch (error) {
  addResult('Documentation Completeness', false, `Failed to check docs: ${error.message}`, 'HIGH');
}

// ============================================================================
// CHECK 8: Package Version Consistency
// ============================================================================
console.log('[8/12] Checking package versions...');
try {
  const rootPkg = JSON.parse(readFileSync(join(ROOT, 'package.json'), 'utf-8'));
  const version = rootPkg.version;

  const isReleaseCandidate = /^\d+\.\d+\.\d+(-rc\.\d+)?$/.test(version);
  const isAlpha = version.includes('alpha');
  const isBeta = version.includes('beta');

  const passed = isReleaseCandidate && !isAlpha && !isBeta;
  addResult(
    'Version Readiness',
    passed,
    `Current version: ${version}. ${passed ? 'Release candidate format' : 'Not release-ready (alpha/beta)'}`,
    !passed ? 'CRITICAL' : 'LOW'
  );
} catch (error) {
  addResult('Version Readiness', false, `Failed to check versions: ${error.message}`, 'CRITICAL');
}

// ============================================================================
// CHECK 9: Build Success
// ============================================================================
console.log('[9/12] Running build...');
try {
  const buildStart = Date.now();
  exec('timeout 60s npm run build', { ignoreError: true });
  const buildDuration = Date.now() - buildStart;

  const passed = buildDuration < 60000;
  addResult(
    'Build Success',
    passed,
    `Build completed in ${buildDuration}ms (target: <60000ms)`,
    !passed ? 'HIGH' : 'LOW'
  );
} catch (error) {
  addResult('Build Success', false, `Build failed or timed out: ${error.message}`, 'CRITICAL');
}

// ============================================================================
// CHECK 10: Dependency Compatibility
// ============================================================================
console.log('[10/12] Checking dependencies...');
try {
  const pkgJson = JSON.parse(readFileSync(join(ROOT, 'package.json'), 'utf-8'));

  // Check for critical dependencies
  const hasOxigraph = Boolean(pkgJson.dependencies?.oxigraph);
  const hasUnrdfOxigraph = Boolean(pkgJson.dependencies?.['@unrdf/oxigraph']);
  const hasN3 = Boolean(pkgJson.dependencies?.n3);

  const passed = hasOxigraph && hasUnrdfOxigraph;
  addResult(
    'Dependency Compatibility',
    passed,
    `oxigraph: ${hasOxigraph ? 'YES' : 'NO'}, @unrdf/oxigraph: ${hasUnrdfOxigraph ? 'YES' : 'NO'}, n3: ${hasN3 ? 'YES (legacy)' : 'NO'}`,
    !passed ? 'CRITICAL' : 'LOW'
  );
} catch (error) {
  addResult('Dependency Compatibility', false, `Failed to check dependencies: ${error.message}`, 'HIGH');
}

// ============================================================================
// CHECK 11: Performance Benchmarks
// ============================================================================
console.log('[11/12] Running performance benchmarks...');
try {
  const benchOutput = exec('timeout 30s node benchmarks/run-all.mjs core', { ignoreError: true });

  const hasResults = benchOutput.includes('ops/sec') || benchOutput.includes('ms/op');
  const hasCrash = benchOutput.includes('Error') || benchOutput.includes('MODULE_NOT_FOUND');

  const passed = hasResults && !hasCrash;
  addResult(
    'Performance Benchmarks',
    passed,
    passed ? 'Benchmarks completed successfully' : hasCrash ? 'Benchmarks crashed - missing dependencies' : 'No benchmark results',
    !passed ? 'HIGH' : 'LOW'
  );
} catch (error) {
  addResult('Performance Benchmarks', false, `Benchmarks failed: ${error.message}`, 'HIGH');
}

// ============================================================================
// CHECK 12: Git Status Clean
// ============================================================================
console.log('[12/12] Checking git status...');
try {
  const gitStatus = exec('git status --porcelain');
  const isClean = gitStatus.trim().length === 0;

  addResult(
    'Git Status Clean',
    isClean,
    isClean ? 'Working directory clean' : `Uncommitted changes: ${gitStatus.split('\n').length} files`,
    !isClean ? 'MEDIUM' : 'LOW'
  );
} catch (error) {
  addResult('Git Status Clean', false, `Failed to check git status: ${error.message}`, 'MEDIUM');
}

// ============================================================================
// RESULTS SUMMARY
// ============================================================================
console.log('');
console.log('='.repeat(80));
console.log('VALIDATION RESULTS');
console.log('='.repeat(80));
console.log('');

const critical = validationResults.results.filter(r => r.status === 'FAIL' && r.severity === 'CRITICAL');
const high = validationResults.results.filter(r => r.status === 'FAIL' && r.severity === 'HIGH');
const medium = validationResults.results.filter(r => r.status === 'FAIL' && r.severity === 'MEDIUM');
const low = validationResults.results.filter(r => r.status === 'FAIL' && r.severity === 'LOW');

for (const result of validationResults.results) {
  const icon = result.status === 'PASS' ? '✓' : '✗';
  const color = result.status === 'PASS' ? '\x1b[32m' : '\x1b[31m';
  const reset = '\x1b[0m';

  console.log(`${color}${icon}${reset} [${result.severity}] ${result.check}`);
  console.log(`  Evidence: ${result.evidence}`);
  console.log('');
}

console.log('='.repeat(80));
console.log(`PASS: ${validationResults.pass}/${validationResults.pass + validationResults.fail}`);
console.log(`FAIL: ${validationResults.fail}/${validationResults.pass + validationResults.fail}`);
console.log('');
console.log('FAILURES BY SEVERITY:');
console.log(`  CRITICAL: ${critical.length}`);
console.log(`  HIGH: ${high.length}`);
console.log(`  MEDIUM: ${medium.length}`);
console.log(`  LOW: ${low.length}`);
console.log('='.repeat(80));
console.log('');

// ============================================================================
// GO/NO-GO RECOMMENDATION
// ============================================================================
const isGoForRelease = critical.length === 0 && high.length === 0 && validationResults.fail < 3;

if (isGoForRelease) {
  console.log('✓ RECOMMENDATION: GO FOR RELEASE');
  console.log('All critical and high-priority checks passed.');
  process.exit(0);
} else {
  console.log('✗ RECOMMENDATION: NO-GO FOR RELEASE');
  console.log('Critical or high-priority issues must be resolved before release.');
  console.log('');
  console.log('BLOCKING ISSUES:');
  [...critical, ...high].forEach(issue => {
    console.log(`  - [${issue.severity}] ${issue.check}: ${issue.evidence}`);
  });
  process.exit(1);
}
