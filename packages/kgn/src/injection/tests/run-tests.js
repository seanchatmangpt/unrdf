/**
 * KGEN Injection Tests Runner
 *
 * Comprehensive test runner for the injection system with
 * coverage reporting and BDD scenario validation.
 */

import { exec } from 'child_process';
import { promisify } from 'util';
import { promises as fs } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const execAsync = promisify(exec);
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const rootDir = join(__dirname, '../../..');

/**
 *
 */
async function runTests() {
  console.log('🚀 Starting KGEN Injection System Tests');
  console.log('=' .repeat(50));

  const results = {
    unit: null,
    integration: null,
    bdd: null,
    coverage: null,
    errors: []
  };

  try {
    // 1. Run unit tests
    console.log('\n📦 Running Unit Tests...');
    try {
      const unitResult = await execAsync('npm test', {
        cwd: rootDir,
        env: { ...process.env, NODE_ENV: 'test' }
      });

      results.unit = {
        success: true,
        output: unitResult.stdout,
        duration: extractTestDuration(unitResult.stdout)
      };

      console.log('✅ Unit tests passed');
    } catch (error) {
      results.unit = {
        success: false,
        error: error.message,
        output: error.stdout || error.stderr
      };
      results.errors.push('Unit tests failed');
      console.log('❌ Unit tests failed:', error.message);
    }

    // 2. Run integration tests
    console.log('\n🔗 Running Integration Tests...');
    try {
      const integrationResult = await execAsync('npm run test:integration', {
        cwd: rootDir,
        env: { ...process.env, NODE_ENV: 'test' }
      });

      results.integration = {
        success: true,
        output: integrationResult.stdout,
        duration: extractTestDuration(integrationResult.stdout)
      };

      console.log('✅ Integration tests passed');
    } catch (error) {
      results.integration = {
        success: false,
        error: error.message,
        output: error.stdout || error.stderr
      };
      results.errors.push('Integration tests failed');
      console.log('❌ Integration tests failed:', error.message);
    }

    // 3. Run BDD feature tests
    console.log('\n🥒 Running BDD Feature Tests...');
    try {
      const bddResult = await execAsync('npm run test:cucumber', {
        cwd: rootDir,
        env: { ...process.env, NODE_ENV: 'test' }
      });

      results.bdd = {
        success: true,
        output: bddResult.stdout,
        scenarios: extractBddScenarios(bddResult.stdout)
      };

      console.log('✅ BDD feature tests passed');
    } catch (error) {
      results.bdd = {
        success: false,
        error: error.message,
        output: error.stdout || error.stderr
      };
      results.errors.push('BDD tests failed');
      console.log('❌ BDD tests failed:', error.message);
    }

    // 4. Generate coverage report
    console.log('\n📊 Generating Coverage Report...');
    try {
      const coverageResult = await execAsync('npm run test:coverage', {
        cwd: rootDir,
        env: { ...process.env, NODE_ENV: 'test' }
      });

      const coverage = extractCoverageStats(coverageResult.stdout);
      results.coverage = {
        success: true,
        ...coverage
      };

      console.log(`✅ Coverage: ${coverage.statements}% statements, ${coverage.branches}% branches`);
    } catch (error) {
      results.coverage = {
        success: false,
        error: error.message
      };
      console.log('⚠️ Coverage generation failed:', error.message);
    }

    // 5. Generate final report
    await generateTestReport(results);

  } catch (error) {
    console.error('❌ Test runner failed:', error);
    results.errors.push(`Runner error: ${error.message}`);
  }

  // 6. Print summary
  printTestSummary(results);

  // Exit with appropriate code
  const hasFailures = results.errors.length > 0;
  process.exit(hasFailures ? 1 : 0);
}

function extractTestDuration(output) {
  const match = output.match(/Tests?.*?(\d+(?:\.\d+)?(?:m?s))/i);
  return match ? match[1] : 'unknown';
}

function extractBddScenarios(output) {
  const scenarioMatch = output.match(/(\d+) scenarios?/i);
  const passedMatch = output.match(/(\d+) passed/i);
  const failedMatch = output.match(/(\d+) failed/i);

  return {
    total: scenarioMatch ? parseInt(scenarioMatch[1]) : 0,
    passed: passedMatch ? parseInt(passedMatch[1]) : 0,
    failed: failedMatch ? parseInt(failedMatch[1]) : 0
  };
}

function extractCoverageStats(output) {
  const statementsMatch = output.match(/Statements\s*:\s*(\d+(?:\.\d+)?)%/);
  const branchesMatch = output.match(/Branches\s*:\s*(\d+(?:\.\d+)?)%/);
  const functionsMatch = output.match(/Functions\s*:\s*(\d+(?:\.\d+)?)%/);
  const linesMatch = output.match(/Lines\s*:\s*(\d+(?:\.\d+)?)%/);

  return {
    statements: statementsMatch ? parseFloat(statementsMatch[1]) : 0,
    branches: branchesMatch ? parseFloat(branchesMatch[1]) : 0,
    functions: functionsMatch ? parseFloat(functionsMatch[1]) : 0,
    lines: linesMatch ? parseFloat(linesMatch[1]) : 0
  };
}

async function generateTestReport(results) {
  const report = {
    timestamp: new Date().toISOString(),
    component: 'KGEN Injection System',
    version: '1.0.0',
    summary: {
      total_test_suites: 0,
      passed_test_suites: 0,
      failed_test_suites: 0,
      coverage_percentage: results.coverage?.statements || 0,
      errors: results.errors
    },
    results: {
      unit_tests: results.unit,
      integration_tests: results.integration,
      bdd_tests: results.bdd,
      coverage: results.coverage
    }
  };

  // Count test suites
  [results.unit, results.integration, results.bdd].forEach(result => {
    if (result) {
      report.summary.total_test_suites++;
      if (result.success) {
        report.summary.passed_test_suites++;
      } else {
        report.summary.failed_test_suites++;
      }
    }
  });

  // Write report to file
  const reportPath = join(rootDir, 'test-results', 'injection-test-report.json');
  await fs.mkdir(dirname(reportPath), { recursive: true });
  await fs.writeFile(reportPath, JSON.stringify(report, null, 2));

  console.log(`\n📄 Test report written to: ${reportPath}`);
}

function printTestSummary(results) {
  console.log('\n' + '='.repeat(50));
  console.log('🏁 Test Summary');
  console.log('='.repeat(50));

  const suites = [
    { name: 'Unit Tests', result: results.unit },
    { name: 'Integration Tests', result: results.integration },
    { name: 'BDD Feature Tests', result: results.bdd }
  ];

  suites.forEach(suite => {
    const status = suite.result?.success ? '✅ PASS' : '❌ FAIL';
    console.log(`${suite.name}: ${status}`);
  });

  if (results.coverage?.success) {
    console.log(`\nCode Coverage:`);
    console.log(`  Statements: ${results.coverage.statements}%`);
    console.log(`  Branches: ${results.coverage.branches}%`);
    console.log(`  Functions: ${results.coverage.functions}%`);
    console.log(`  Lines: ${results.coverage.lines}%`);

    // Check if coverage meets requirements
    const meetsRequirements = results.coverage.statements >= 90 &&
                             results.coverage.branches >= 85 &&
                             results.coverage.functions >= 90;

    console.log(`\nCoverage Requirements: ${meetsRequirements ? '✅ MET' : '❌ NOT MET'}`);
    console.log(`  Target: 90% statements, 85% branches, 90% functions`);
  }

  if (results.bdd?.scenarios) {
    console.log(`\nBDD Scenarios:`);
    console.log(`  Total: ${results.bdd.scenarios.total}`);
    console.log(`  Passed: ${results.bdd.scenarios.passed}`);
    console.log(`  Failed: ${results.bdd.scenarios.failed}`);
  }

  if (results.errors.length > 0) {
    console.log(`\n❌ Errors (${results.errors.length}):`);
    results.errors.forEach((error, index) => {
      console.log(`  ${index + 1}. ${error}`);
    });
  }

  const overallStatus = results.errors.length === 0 ? '✅ SUCCESS' : '❌ FAILURE';
  console.log(`\nOverall Result: ${overallStatus}`);

  // Recommendations
  if (results.coverage?.statements < 90) {
    console.log('\n💡 Recommendation: Increase test coverage to meet 90% target');
  }

  if (results.errors.length > 0) {
    console.log('\n💡 Recommendation: Fix failing tests before deployment');
  }

  console.log('='.repeat(50));
}

// Run tests if this file is executed directly
if (process.argv[1] === __filename) {
  runTests().catch(console.error);
}

export { runTests };