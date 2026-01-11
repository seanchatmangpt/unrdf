#!/usr/bin/env node
/**
 * @file Security Penetration Test Runner
 * @module test/security-testing/run-penetration-tests
 * @description
 * Orchestrates comprehensive security penetration testing and generates
 * detailed security posture reports.
 *
 * CRITICAL: ALL tests must pass for production deployment.
 */

import { spawn } from 'child_process';
import { writeFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Test suites in order of execution
 */
const TEST_SUITES = [
  {
    name: 'Injection Attacks',
    file: '01-injection-attacks.test.mjs',
    critical: true,
    description: 'SQL/SPARQL, command, path traversal, XSS attacks',
  },
  {
    name: 'Authentication Attacks',
    file: '02-authentication-attacks.test.mjs',
    critical: true,
    description: 'Brute force, timing attacks, API key enumeration',
  },
  {
    name: 'DoS Attacks',
    file: '03-dos-attacks.test.mjs',
    critical: true,
    description: 'Request flooding, large payloads, slowloris',
  },
  {
    name: 'Data Exfiltration',
    file: '04-data-exfiltration.test.mjs',
    critical: true,
    description: 'Secret exposure, error leakage, debug endpoints',
  },
  {
    name: 'Cryptographic Attacks',
    file: '05-cryptographic-attacks.test.mjs',
    critical: true,
    description: 'Hash collisions, Merkle tree, receipt tampering',
  },
];

/**
 * Run a single test suite
 */
async function runTestSuite(suite) {
  return new Promise((resolve) => {
    const testFile = join(__dirname, suite.file);

    console.log(`\n${'='.repeat(80)}`);
    console.log(`Running: ${suite.name}`);
    console.log(`Description: ${suite.description}`);
    console.log(`Critical: ${suite.critical ? 'YES' : 'NO'}`);
    console.log(`${'='.repeat(80)}\n`);

    const startTime = Date.now();

    const vitest = spawn(
      'npx',
      ['vitest', 'run', testFile, '--reporter=verbose'],
      {
        stdio: 'inherit',
        shell: true,
      }
    );

    vitest.on('close', (code) => {
      const duration = Date.now() - startTime;

      resolve({
        suite: suite.name,
        file: suite.file,
        passed: code === 0,
        exitCode: code,
        duration,
        critical: suite.critical,
      });
    });

    vitest.on('error', (error) => {
      resolve({
        suite: suite.name,
        file: suite.file,
        passed: false,
        exitCode: 1,
        error: error.message,
        duration: Date.now() - startTime,
        critical: suite.critical,
      });
    });
  });
}

/**
 * Generate security report
 */
function generateReport(results, totalDuration) {
  const lines = [];

  lines.push('='.repeat(80));
  lines.push('SECURITY PENETRATION TEST REPORT');
  lines.push('='.repeat(80));
  lines.push('');
  lines.push(`Generated: ${new Date().toISOString()}`);
  lines.push(`Total Duration: ${(totalDuration / 1000).toFixed(2)}s`);
  lines.push('');

  // Summary
  const totalTests = results.length;
  const passed = results.filter((r) => r.passed).length;
  const failed = results.filter((r) => !r.passed).length;
  const criticalFailed = results.filter((r) => !r.passed && r.critical).length;

  lines.push('-'.repeat(80));
  lines.push('SUMMARY');
  lines.push('-'.repeat(80));
  lines.push(`Total Test Suites: ${totalTests}`);
  lines.push(`Passed: ${passed}`);
  lines.push(`Failed: ${failed}`);
  lines.push(`Critical Failures: ${criticalFailed}`);
  lines.push('');

  // Overall Status
  const overallStatus =
    failed === 0 ? 'PASS - Production Ready' : 'FAIL - Not Production Ready';
  const statusSymbol = failed === 0 ? '✅' : '❌';

  lines.push(`Status: ${statusSymbol} ${overallStatus}`);
  lines.push('');

  // Security Posture
  lines.push('-'.repeat(80));
  lines.push('SECURITY POSTURE');
  lines.push('-'.repeat(80));

  if (failed === 0) {
    lines.push('✅ ALL ATTACK SCENARIOS BLOCKED');
    lines.push('✅ No vulnerabilities detected');
    lines.push('✅ All security measures effective');
    lines.push('✅ System is hardened against penetration attempts');
  } else {
    lines.push('❌ VULNERABILITIES DETECTED');
    lines.push(
      `❌ ${criticalFailed} critical vulnerabilities require immediate attention`
    );
    lines.push('❌ System is NOT production ready');
    lines.push('❌ Remediation required before deployment');
  }
  lines.push('');

  // Detailed Results
  lines.push('-'.repeat(80));
  lines.push('DETAILED RESULTS');
  lines.push('-'.repeat(80));
  lines.push('');

  for (const result of results) {
    const status = result.passed ? '✅ PASS' : '❌ FAIL';
    const priority = result.critical ? '[CRITICAL]' : '[STANDARD]';

    lines.push(`${status} ${priority} ${result.suite}`);
    lines.push(`   File: ${result.file}`);
    lines.push(`   Duration: ${(result.duration / 1000).toFixed(2)}s`);
    lines.push(`   Exit Code: ${result.exitCode}`);

    if (result.error) {
      lines.push(`   Error: ${result.error}`);
    }

    lines.push('');
  }

  // Attack Coverage
  lines.push('-'.repeat(80));
  lines.push('ATTACK COVERAGE');
  lines.push('-'.repeat(80));
  lines.push('');
  lines.push('✅ SQL/SPARQL Injection Attacks');
  lines.push('✅ Command Injection Attacks');
  lines.push('✅ Path Traversal Attacks');
  lines.push('✅ Cross-Site Scripting (XSS)');
  lines.push('✅ Authentication Brute Force');
  lines.push('✅ Timing Attacks');
  lines.push('✅ API Key Enumeration');
  lines.push('✅ Request Flooding (DoS)');
  lines.push('✅ Large Payload Attacks');
  lines.push('✅ Slowloris Attacks');
  lines.push('✅ Secret Exposure');
  lines.push('✅ Error Message Leakage');
  lines.push('✅ Debug Endpoint Exposure');
  lines.push('✅ Hash Collision Attacks');
  lines.push('✅ Merkle Tree Attacks');
  lines.push('✅ Receipt Tampering');
  lines.push('✅ Weak Randomness');
  lines.push('');

  // Recommendations
  lines.push('-'.repeat(80));
  lines.push('RECOMMENDATIONS');
  lines.push('-'.repeat(80));
  lines.push('');

  if (failed === 0) {
    lines.push('No immediate action required.');
    lines.push('');
    lines.push('Continuous Security Measures:');
    lines.push('  • Run penetration tests before each release');
    lines.push('  • Monitor audit logs for attack patterns');
    lines.push('  • Keep dependencies updated');
    lines.push('  • Review security hardening quarterly');
    lines.push('  • Conduct external security audits annually');
  } else {
    lines.push('IMMEDIATE ACTION REQUIRED:');
    lines.push('');

    const failedSuites = results.filter((r) => !r.passed);
    failedSuites.forEach((result) => {
      lines.push(`  ${result.critical ? '🔴 CRITICAL' : '🟡 STANDARD'}: ${result.suite}`);
      lines.push(`     - Review test output for specific vulnerabilities`);
      lines.push(`     - Implement missing security controls`);
      lines.push(`     - Re-test after remediation`);
      lines.push('');
    });

    lines.push('DO NOT DEPLOY TO PRODUCTION until all tests pass.');
  }

  lines.push('');
  lines.push('='.repeat(80));
  lines.push('END OF REPORT');
  lines.push('='.repeat(80));

  return lines.join('\n');
}

/**
 * Main execution
 */
async function main() {
  console.log('\n' + '='.repeat(80));
  console.log('UNRDF SECURITY PENETRATION TESTING SUITE');
  console.log('='.repeat(80));
  console.log('');
  console.log('This suite simulates real-world attack scenarios to validate');
  console.log('security hardening measures. All attacks MUST be blocked.');
  console.log('');

  const overallStartTime = Date.now();
  const results = [];

  // Run all test suites sequentially
  for (const suite of TEST_SUITES) {
    const result = await runTestSuite(suite);
    results.push(result);

    // Stop on critical failure if desired (optional)
    // if (result.critical && !result.passed) {
    //   console.log('\n❌ CRITICAL TEST FAILURE - Stopping execution\n');
    //   break;
    // }
  }

  const totalDuration = Date.now() - overallStartTime;

  // Generate report
  const report = generateReport(results, totalDuration);

  // Write to file
  const reportPath = join(__dirname, 'PENETRATION-TEST-REPORT.md');
  writeFileSync(reportPath, report);

  // Print report
  console.log('\n');
  console.log(report);
  console.log(`\nReport saved to: ${reportPath}`);

  // Exit with appropriate code
  const failed = results.filter((r) => !r.passed).length;
  process.exit(failed > 0 ? 1 : 0);
}

main().catch((error) => {
  console.error('Fatal error running penetration tests:', error);
  process.exit(1);
});
