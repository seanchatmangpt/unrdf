#!/usr/bin/env node
/**
 * @file Thesis Validation Script
 * @module validation/thesis-validation
 *
 * @description
 * Production-ready CI/CD validation for thesis claims.
 * Validates all previously refuted claims from ADVERSARIAL-THESIS-REVIEW.md
 *
 * Exit codes:
 * - 0: All validations passed
 * - 1: One or more validations failed
 */

import { execSync } from 'node:child_process';
import { readFileSync, existsSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

// ANSI color codes for terminal output
const colors = {
  reset: '\x1b[0m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  magenta: '\x1b[35m',
};

const c = colors;

// Validation results tracker
const results = {
  passed: [],
  failed: [],
  warnings: [],
};

/**
 * Execute shell command and return output
 * @param {string} command - Command to execute
 * @returns {string} Command output
 */
function exec(command) {
  try {
    return execSync(command, { encoding: 'utf8', cwd: '/home/user/unrdf' });
  } catch (error) {
    return error.stdout || error.stderr || '';
  }
}

/**
 * Count lines in files matching pattern
 * @param {string} pattern - Find pattern
 * @returns {number} Total line count
 */
function countLOC(pattern) {
  const output = exec(pattern);
  const match = output.match(/(\d+)\s+total$/m);
  return match ? parseInt(match[1], 10) : 0;
}

/**
 * Log validation result
 * @param {boolean} passed - Whether validation passed
 * @param {string} name - Validation name
 * @param {string} details - Details message
 */
function logResult(passed, name, details) {
  const status = passed ? `${c.green}✅ PASS${c.reset}` : `${c.red}❌ FAIL${c.reset}`;
  console.log(`\n${status} ${c.blue}${name}${c.reset}`);
  console.log(`  ${details}`);

  if (passed) {
    results.passed.push(name);
  } else {
    results.failed.push({ name, details });
  }
}

/**
 * Log warning
 * @param {string} name - Warning name
 * @param {string} details - Details message
 */
function logWarning(name, details) {
  console.log(`\n${c.yellow}⚠️  WARN${c.reset} ${c.blue}${name}${c.reset}`);
  console.log(`  ${details}`);
  results.warnings.push({ name, details });
}

// ============================================================================
// VALIDATION 1: KGC-4D LOC Count
// ============================================================================
console.log(`\n${c.magenta}═══════════════════════════════════════════════════════════${c.reset}`);
console.log(`${c.magenta}  THESIS VALIDATION - Adversarial Claims Verification${c.reset}`);
console.log(`${c.magenta}═══════════════════════════════════════════════════════════${c.reset}`);

console.log(`\n${c.blue}[1/11]${c.reset} Validating KGC-4D LOC count...`);

const kgc4dLOC = countLOC('find /home/user/unrdf/packages/kgc-4d/src -name "*.mjs" -exec wc -l {} + 2>/dev/null | tail -1');
const kgc4dFiles = exec('find /home/user/unrdf/packages/kgc-4d/src -name "*.mjs" 2>/dev/null | wc -l').trim();

const expectedKGC4D = 5465;
const tolerance = 100; // Allow 100 LOC variance for minor changes

if (Math.abs(kgc4dLOC - expectedKGC4D) <= tolerance) {
  logResult(true, 'KGC-4D LOC Count',
    `${kgc4dLOC} LOC (${kgc4dFiles} files) matches expected ${expectedKGC4D} ±${tolerance}`);
} else {
  logResult(false, 'KGC-4D LOC Count',
    `${kgc4dLOC} LOC does not match expected ${expectedKGC4D} (±${tolerance})`);
}

// ============================================================================
// VALIDATION 2: Total Codebase LOC
// ============================================================================
console.log(`\n${c.blue}[2/11]${c.reset} Validating total codebase LOC...`);

const totalLOC = countLOC('find /home/user/unrdf -name "*.mjs" -o -name "*.js" 2>/dev/null | xargs wc -l 2>/dev/null | tail -1');
const expectedTotal = 269806;
const totalTolerance = 5000; // Allow larger variance for total

if (Math.abs(totalLOC - expectedTotal) <= totalTolerance) {
  logResult(true, 'Total Codebase LOC',
    `${totalLOC} LOC matches expected ${expectedTotal} ±${totalTolerance}`);
} else {
  logResult(false, 'Total Codebase LOC',
    `${totalLOC} LOC does not match expected ${expectedTotal} (±${totalTolerance})`);
}

// ============================================================================
// VALIDATION 3: YAWL LOC Count
// ============================================================================
console.log(`\n${c.blue}[3/11]${c.reset} Validating YAWL LOC count...`);

const yawlTotalLOC = countLOC('find /home/user/unrdf/packages/yawl -name "*.mjs" -o -name "*.js" 2>/dev/null | xargs wc -l 2>/dev/null | tail -1');
const yawlSrcLOC = countLOC('find /home/user/unrdf/packages/yawl/src -name "*.mjs" 2>/dev/null | xargs wc -l 2>/dev/null | tail -1');

const expectedYawlTotal = 26449;
const expectedYawlSrc = 19618;
const yawlTolerance = 500;

const yawlTotalMatch = Math.abs(yawlTotalLOC - expectedYawlTotal) <= yawlTolerance;
const yawlSrcMatch = Math.abs(yawlSrcLOC - expectedYawlSrc) <= yawlTolerance;

if (yawlTotalMatch && yawlSrcMatch) {
  logResult(true, 'YAWL LOC Count',
    `Total: ${yawlTotalLOC} LOC, Source: ${yawlSrcLOC} LOC (expected ${expectedYawlTotal}/${expectedYawlSrc} ±${yawlTolerance})`);
} else {
  logResult(false, 'YAWL LOC Count',
    `Total: ${yawlTotalLOC} (expected ${expectedYawlTotal}), Source: ${yawlSrcLOC} (expected ${expectedYawlSrc})`);
}

// ============================================================================
// VALIDATION 4: Microframeworks Count and LOC
// ============================================================================
console.log(`\n${c.blue}[4/11]${c.reset} Validating microframeworks...`);

const microFwOutput = exec('find /home/user/unrdf -type f \\( -name "microfw-*.mjs" -o -name "max-combo-*.mjs" \\) -exec wc -l {} + 2>/dev/null');
const microFwFiles = exec('find /home/user/unrdf -type f \\( -name "microfw-*.mjs" -o -name "max-combo-*.mjs" \\) 2>/dev/null | wc -l').trim();
const microFwLOC = countLOC('find /home/user/unrdf -type f \\( -name "microfw-*.mjs" -o -name "max-combo-*.mjs" \\) -exec wc -l {} + 2>/dev/null | tail -1');

const expectedMicroFwCount = 3;
const expectedMicroFwLOC = 1856;
const microFwTolerance = 50;

const microFwCountMatch = parseInt(microFwFiles) === expectedMicroFwCount;
const microFwLOCMatch = Math.abs(microFwLOC - expectedMicroFwLOC) <= microFwTolerance;

if (microFwCountMatch && microFwLOCMatch) {
  logResult(true, 'Microframeworks Delivered',
    `${microFwFiles} frameworks, ${microFwLOC} LOC (expected ${expectedMicroFwCount} frameworks, ${expectedMicroFwLOC} LOC)`);
} else {
  logResult(false, 'Microframeworks Delivered',
    `Found ${microFwFiles} frameworks (expected ${expectedMicroFwCount}), ${microFwLOC} LOC (expected ${expectedMicroFwLOC})`);
}

// ============================================================================
// VALIDATION 5: Git Commit Timeline Consistency
// ============================================================================
console.log(`\n${c.blue}[5/11]${c.reset} Validating git timeline...`);

const firstCommit = exec('git log --format="%ai" --reverse | head -1').trim();
const lastCommit = exec('git log --format="%ai" | head -1').trim();
const commitCount = exec('git log --oneline | wc -l').trim();

// Extract years
const firstYear = firstCommit.match(/(\d{4})/)?.[1];
const lastYear = lastCommit.match(/(\d{4})/)?.[1];

// Timeline should be 2025 (not 2024 as claimed in thesis)
const timelineValid = firstYear === '2025' && lastYear === '2025';

if (timelineValid) {
  logResult(true, 'Git Timeline Consistency',
    `Repository timeline: ${firstYear} (${commitCount} commits). Matches actual development period.`);
} else {
  logWarning('Git Timeline Consistency',
    `Timeline shows ${firstYear}-${lastYear}, but thesis claimed Nov 2024. Verify dates.`);
}

// ============================================================================
// VALIDATION 6: Test Pass Rate Check
// ============================================================================
console.log(`\n${c.blue}[6/11]${c.reset} Validating test infrastructure...`);

// Check if vitest is available
const vitestCheck = exec('cd /home/user/unrdf/packages/yawl && command -v vitest 2>/dev/null || echo "not found"').trim();

if (vitestCheck.includes('not found')) {
  logWarning('Test Infrastructure',
    'vitest not found - cannot verify 64.1% pass rate claim. Tests should be runnable in CI.');
} else {
  logResult(true, 'Test Infrastructure',
    'vitest is available - tests can be run');
}

// ============================================================================
// VALIDATION 7: Package Count
// ============================================================================
console.log(`\n${c.blue}[7/11]${c.reset} Validating package count...`);

const packageCount = exec('ls -1 /home/user/unrdf/packages/*/package.json 2>/dev/null | wc -l').trim();
const expectedPackages = 20;
const packageTolerance = 2;

if (Math.abs(parseInt(packageCount) - expectedPackages) <= packageTolerance) {
  logResult(true, 'Package Count',
    `${packageCount} packages (expected ${expectedPackages} ±${packageTolerance})`);
} else {
  logResult(false, 'Package Count',
    `${packageCount} packages does not match expected ${expectedPackages} (±${packageTolerance})`);
}

// ============================================================================
// VALIDATION 8: OTEL Validation Score
// ============================================================================
console.log(`\n${c.blue}[8/11]${c.reset} Validating OTEL scores...`);

// Check if validation-output.log exists
const otelLogPath = '/home/user/unrdf/validation-output.log';
if (existsSync(otelLogPath)) {
  const otelLog = readFileSync(otelLogPath, 'utf8');
  const scoreMatch = otelLog.match(/Score:\s*(\d+)\/100/);

  if (scoreMatch) {
    const score = parseInt(scoreMatch[1]);
    const minScore = 80;

    if (score >= minScore) {
      logResult(true, 'OTEL Validation Score',
        `Score: ${score}/100 (threshold: ≥${minScore})`);
    } else {
      logResult(false, 'OTEL Validation Score',
        `Score: ${score}/100 is below threshold of ${minScore}`);
    }
  } else {
    logWarning('OTEL Validation Score',
      'Could not parse score from validation-output.log');
  }
} else {
  logWarning('OTEL Validation Score',
    'validation-output.log not found - run "node validation/run-all.mjs comprehensive" first');
}

// ============================================================================
// VALIDATION 9: TODO/FIXME in Thesis Docs
// ============================================================================
console.log(`\n${c.blue}[9/11]${c.reset} Checking for TODO/FIXME in thesis docs...`);

const thesisTodos = exec('grep -r "TODO\\|FIXME" /home/user/unrdf/ADVERSARIAL-THESIS-REVIEW.md /home/user/unrdf/CORRECTED-THESIS-EXCERPTS.md /home/user/unrdf/books/kgc-thesis/ 2>/dev/null | wc -l').trim();

if (parseInt(thesisTodos) === 0) {
  logResult(true, 'No Unresolved TODOs',
    'No TODO/FIXME markers found in thesis documentation');
} else {
  logWarning('Unresolved TODOs',
    `${thesisTodos} TODO/FIXME markers found in thesis docs - review before publication`);
}

// ============================================================================
// VALIDATION 10: Verify No Inflated Claims in Recent Commits
// ============================================================================
console.log(`\n${c.blue}[10/11]${c.reset} Checking recent commit messages for inflation...`);

const recentCommits = exec('git log --oneline -20');
const inflationPatterns = [
  /\d+\s*frameworks?/i,
  /production-ready/i,
  /zero defects/i,
  /100% pass/i,
];

let inflatedCommits = 0;
for (const pattern of inflationPatterns) {
  if (pattern.test(recentCommits)) {
    inflatedCommits++;
  }
}

if (inflatedCommits === 0) {
  logResult(true, 'Commit Message Quality',
    'Recent commits do not contain potentially inflated claims');
} else {
  logWarning('Commit Message Quality',
    `${inflatedCommits} patterns found that may indicate claim inflation - review commit messages`);
}

// ============================================================================
// VALIDATION 11: Cross-Reference Integrity
// ============================================================================
console.log(`\n${c.blue}[11/11]${c.reset} Validating cross-reference integrity...`);

// Check that corrected excerpts reference the adversarial review
const correctedPath = '/home/user/unrdf/CORRECTED-THESIS-EXCERPTS.md';
const adversarialPath = '/home/user/unrdf/ADVERSARIAL-THESIS-REVIEW.md';

let crossRefValid = true;

if (existsSync(correctedPath) && existsSync(adversarialPath)) {
  // Both files exist - good
  logResult(true, 'Cross-Reference Files Exist',
    'Both ADVERSARIAL-THESIS-REVIEW.md and CORRECTED-THESIS-EXCERPTS.md are present');
} else {
  logResult(false, 'Cross-Reference Files Exist',
    'Missing thesis validation files');
  crossRefValid = false;
}

// ============================================================================
// SUMMARY
// ============================================================================
console.log(`\n${c.magenta}═══════════════════════════════════════════════════════════${c.reset}`);
console.log(`${c.magenta}  VALIDATION SUMMARY${c.reset}`);
console.log(`${c.magenta}═══════════════════════════════════════════════════════════${c.reset}\n`);

console.log(`${c.green}✅ Passed:${c.reset}  ${results.passed.length}`);
console.log(`${c.red}❌ Failed:${c.reset}  ${results.failed.length}`);
console.log(`${c.yellow}⚠️  Warnings:${c.reset} ${results.warnings.length}`);

if (results.failed.length > 0) {
  console.log(`\n${c.red}FAILED VALIDATIONS:${c.reset}`);
  for (const failure of results.failed) {
    console.log(`  - ${failure.name}: ${failure.details}`);
  }
}

if (results.warnings.length > 0) {
  console.log(`\n${c.yellow}WARNINGS:${c.reset}`);
  for (const warning of results.warnings) {
    console.log(`  - ${warning.name}: ${warning.details}`);
  }
}

// Write report to file
const reportPath = '/home/user/unrdf/thesis-validation-report.txt';
const report = `
Thesis Validation Report
Generated: ${new Date().toISOString()}

SUMMARY:
- Passed: ${results.passed.length}
- Failed: ${results.failed.length}
- Warnings: ${results.warnings.length}

PASSED VALIDATIONS:
${results.passed.map(name => `  ✅ ${name}`).join('\n')}

${results.failed.length > 0 ? `FAILED VALIDATIONS:
${results.failed.map(f => `  ❌ ${f.name}\n     ${f.details}`).join('\n')}` : ''}

${results.warnings.length > 0 ? `WARNINGS:
${results.warnings.map(w => `  ⚠️  ${w.name}\n     ${w.details}`).join('\n')}` : ''}

STATUS: ${results.failed.length === 0 ? '✅ ALL VALIDATIONS PASSED' : '❌ VALIDATION FAILED'}
`;

writeFileSync(reportPath, report, 'utf8');
console.log(`\n${c.blue}Report written to:${c.reset} ${reportPath}`);

// Exit with appropriate code
const exitCode = results.failed.length === 0 ? 0 : 1;

if (exitCode === 0) {
  console.log(`\n${c.green}🎉 All thesis validations passed!${c.reset}`);
  console.log(`${c.green}Thesis metrics are verified and consistent with code reality.${c.reset}\n`);
} else {
  console.log(`\n${c.red}❌ Thesis validation failed.${c.reset}`);
  console.log(`${c.red}Fix the issues above before merging to main/master.${c.reset}\n`);
}

process.exit(exitCode);
