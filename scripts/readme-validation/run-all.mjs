#!/usr/bin/env node

/**
 * Run all README validation checks
 *
 * Orchestrates:
 * 1. Extract code examples
 * 2. Validate syntax
 * 3. Check links
 * 4. Validate API coverage
 * 5. Generate comprehensive report
 */

import { spawn } from 'child_process';
import { writeFile } from 'fs/promises';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const projectRoot = join(__dirname, '../..');

/**
 * Run a validation script
 * @param {string} scriptName - Script filename
 * @param {string} description - Human-readable description
 * @returns {Promise<{success: boolean, duration: number, output: string}>}
 */
function runValidation(scriptName, description) {
  return new Promise((resolve) => {
    const startTime = Date.now();
    const scriptPath = join(__dirname, scriptName);

    console.log(`\n${'='.repeat(60)}`);
    console.log(`🔍 ${description}`);
    console.log('='.repeat(60));

    const child = spawn('node', [scriptPath], {
      cwd: projectRoot,
      stdio: 'pipe'
    });

    let output = '';

    child.stdout.on('data', (data) => {
      const text = data.toString();
      output += text;
      process.stdout.write(text);
    });

    child.stderr.on('data', (data) => {
      const text = data.toString();
      output += text;
      process.stderr.write(text);
    });

    child.on('close', (code) => {
      const duration = Date.now() - startTime;
      resolve({
        success: code === 0,
        duration,
        output,
        exitCode: code
      });
    });
  });
}

/**
 * Generate validation report
 * @param {Array} results - Validation results
 * @returns {string} - Report text
 */
function generateReport(results, totalDuration) {
  const lines = [];

  lines.push('='.repeat(70));
  lines.push('📊 KGC-4D README VALIDATION REPORT');
  lines.push('='.repeat(70));
  lines.push(`Generated: ${new Date().toISOString()}`);
  lines.push(`Total Duration: ${(totalDuration / 1000).toFixed(2)}s`);
  lines.push('');

  // Summary table
  const passed = results.filter(r => r.success).length;
  const failed = results.filter(r => !r.success).length;
  const total = results.length;
  const score = Math.round((passed / total) * 100);

  lines.push('SUMMARY:');
  lines.push(`  ✅ Passed: ${passed}/${total}`);
  lines.push(`  ❌ Failed: ${failed}/${total}`);
  lines.push(`  📈 Score: ${score}/100`);
  lines.push('');

  // Individual results
  lines.push('DETAILED RESULTS:');
  lines.push('');

  results.forEach((result, index) => {
    const icon = result.success ? '✅' : '❌';
    const status = result.success ? 'PASS' : 'FAIL';
    const duration = (result.duration / 1000).toFixed(2);

    lines.push(`${index + 1}. ${icon} ${result.name}`);
    lines.push(`   Status: ${status}`);
    lines.push(`   Duration: ${duration}s`);

    if (!result.success) {
      lines.push(`   Exit Code: ${result.exitCode}`);
      // Extract error summary from output
      const errorLines = result.output
        .split('\n')
        .filter(l => l.includes('❌') || l.includes('Error') || l.includes('FAIL'))
        .slice(0, 5); // First 5 error lines

      if (errorLines.length > 0) {
        lines.push('   Errors:');
        errorLines.forEach(err => {
          lines.push(`     ${err.trim()}`);
        });
      }
    }
    lines.push('');
  });

  // Quality gates
  lines.push('QUALITY GATES:');
  lines.push('');

  const gates = [
    { name: 'Code Examples Execute', pass: results[0]?.success, blocker: true },
    { name: 'Link Validation', pass: results[1]?.success, blocker: true },
    { name: 'API Coverage', pass: results[2]?.success, blocker: true },
    { name: 'Overall Score ≥90', pass: score >= 90, blocker: false }
  ];

  gates.forEach(gate => {
    const icon = gate.pass ? '✅' : '❌';
    const blocker = gate.blocker ? '🚫 BLOCKER' : '';
    lines.push(`  ${icon} ${gate.name} ${blocker}`);
  });
  lines.push('');

  // Final verdict
  const blockersFailed = gates.filter(g => g.blocker && !g.pass).length;

  lines.push('='.repeat(70));
  if (blockersFailed > 0) {
    lines.push('❌ VALIDATION FAILED');
    lines.push(`   ${blockersFailed} blocker(s) failed`);
    lines.push('   README is NOT ready for merge');
  } else if (score >= 90) {
    lines.push('✅ VALIDATION PASSED');
    lines.push('   All quality gates passed');
    lines.push('   README is ready for merge');
  } else {
    lines.push('⚠️  VALIDATION PARTIAL');
    lines.push(`   Score ${score}/100 below threshold (90))`);
    lines.push('   Fix non-blocker issues before merge');
  }
  lines.push('='.repeat(70));

  return lines.join('\n');
}

async function main() {
  console.log('🚀 Starting README validation suite...\n');

  const overallStart = Date.now();
  const validations = [
    { script: 'extract-examples.mjs', name: 'Code Example Extraction & Syntax' },
    { script: 'check-links.mjs', name: 'Link Validation' },
    { script: 'validate-api-coverage.mjs', name: 'API Coverage' }
  ];

  const results = [];

  // Run validations sequentially (to avoid output interleaving)
  for (const validation of validations) {
    const result = await runValidation(validation.script, validation.name);
    results.push({
      ...result,
      name: validation.name
    });
  }

  const totalDuration = Date.now() - overallStart;

  // Generate report
  console.log('\n');
  const report = generateReport(results, totalDuration);
  console.log(report);

  // Write report to file
  const reportPath = join(projectRoot, 'tmp/readme-validation-report.txt');
  await writeFile(reportPath, report, 'utf-8');
  console.log(`\n📄 Full report written to: ${reportPath}\n`);

  // Exit with appropriate code
  const failed = results.filter(r => !r.success).length;
  if (failed > 0) {
    process.exit(1);
  } else {
    process.exit(0);
  }
}

main().catch(error => {
  console.error('❌ Fatal error running validation suite:', error);
  process.exit(1);
});
