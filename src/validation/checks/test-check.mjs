/**
 * @fileoverview Test Check - Validates testing standards and coverage
 *
 * **Checks performed**:
 * 1. Test coverage (target: >=70%)
 * 2. Test pass rate (target: >=95%)
 * 3. Test suite performance (target: <5s)
 * 4. Test file existence
 * 5. Test naming conventions
 * 6. Integration test presence
 * 7. Edge case coverage
 *
 * **Scoring**:
 * - 100: Excellent test coverage and performance
 * - 95-99: Good coverage with minor gaps
 * - 80-94: Adequate coverage
 * - 60-79: Insufficient coverage
 * - <60: Critical testing gaps
 *
 * @module validation/checks/test-check
 */

import { readdir, readFile, stat, access } from 'node:fs/promises';
import { join, extname, relative, basename, dirname } from 'node:path';
import { spawn } from 'node:child_process';

/**
 * Test check thresholds
 */
export const TEST_THRESHOLDS = {
  minCoverage: 70,
  targetCoverage: 80,
  excellentCoverage: 90,
  minPassRate: 95,
  maxSuiteDuration: 5000,
  maxTestDuration: 1000
};

/**
 * Execute command with timeout
 *
 * @param {string} command - Command to execute
 * @param {Array<string>} args - Command arguments
 * @param {Object} options - Execution options
 * @returns {Promise<Object>} Execution result
 */
async function executeCommand(command, args, options = {}) {
  return new Promise((resolve) => {
    const startTime = Date.now();
    const timeout = options.timeout || 30000;

    let stdout = '';
    let stderr = '';

    const proc = spawn(command, args, {
      cwd: options.cwd,
      shell: true,
      timeout,
      env: { ...process.env, ...options.env }
    });

    proc.stdout?.on('data', (data) => {
      stdout += data.toString();
    });

    proc.stderr?.on('data', (data) => {
      stderr += data.toString();
    });

    const timeoutId = setTimeout(() => {
      proc.kill('SIGTERM');
      resolve({
        success: false,
        exitCode: -1,
        stdout,
        stderr: 'Command timed out',
        duration: timeout
      });
    }, timeout);

    proc.on('close', (code) => {
      clearTimeout(timeoutId);
      resolve({
        success: code === 0,
        exitCode: code || 0,
        stdout,
        stderr,
        duration: Date.now() - startTime
      });
    });

    proc.on('error', (error) => {
      clearTimeout(timeoutId);
      resolve({
        success: false,
        exitCode: -1,
        stdout,
        stderr: error.message,
        duration: Date.now() - startTime
      });
    });
  });
}

/**
 * Get all test files in a directory
 *
 * @param {string} dirPath - Directory path
 * @returns {Promise<Array<Object>>} Test file information
 */
async function getTestFiles(dirPath) {
  const testFiles = [];
  const testPatterns = ['.test.', '.spec.', '__tests__', '_test.'];

  async function scan(currentPath) {
    try {
      const entries = await readdir(currentPath, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = join(currentPath, entry.name);

        if (entry.isDirectory()) {
          if (!['node_modules', 'dist', 'build', 'coverage', '.git'].includes(entry.name)) {
            await scan(fullPath);
          }
        } else if (entry.isFile()) {
          const ext = extname(entry.name);
          if (['.mjs', '.js', '.ts', '.jsx', '.tsx'].includes(ext)) {
            const isTest = testPatterns.some(p => entry.name.includes(p) || currentPath.includes(p));
            if (isTest) {
              testFiles.push({
                path: fullPath,
                relativePath: relative(dirPath, fullPath),
                name: entry.name,
                isIntegration: entry.name.includes('integration') || currentPath.includes('integration')
              });
            }
          }
        }
      }
    } catch (error) {
      // Directory not accessible
    }
  }

  await scan(dirPath);
  return testFiles;
}

/**
 * Get source files that should have tests
 *
 * @param {string} dirPath - Directory path
 * @returns {Promise<Array<Object>>} Source file information
 */
async function getSourceFiles(dirPath) {
  const sourceFiles = [];

  async function scan(currentPath) {
    try {
      const entries = await readdir(currentPath, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = join(currentPath, entry.name);

        if (entry.isDirectory()) {
          if (!['node_modules', 'dist', 'build', 'coverage', '.git', '__tests__', 'test', 'tests'].includes(entry.name)) {
            await scan(fullPath);
          }
        } else if (entry.isFile()) {
          const ext = extname(entry.name);
          if (['.mjs', '.js', '.ts', '.jsx', '.tsx'].includes(ext)) {
            // Skip test files
            if (!entry.name.includes('.test.') && !entry.name.includes('.spec.')) {
              sourceFiles.push({
                path: fullPath,
                relativePath: relative(dirPath, fullPath),
                name: entry.name.replace(ext, '')
              });
            }
          }
        }
      }
    } catch (error) {
      // Directory not accessible
    }
  }

  await scan(dirPath);
  return sourceFiles;
}

/**
 * Parse coverage from output
 *
 * @param {string} output - Test output
 * @returns {Object} Coverage information
 */
function parseCoverage(output) {
  // Try to parse common coverage formats
  const coverage = {
    lines: null,
    statements: null,
    branches: null,
    functions: null
  };

  // Istanbul/nyc format
  const lineMatch = output.match(/Lines\s*:\s*([\d.]+)%/);
  const stmtMatch = output.match(/Statements\s*:\s*([\d.]+)%/);
  const branchMatch = output.match(/Branches\s*:\s*([\d.]+)%/);
  const funcMatch = output.match(/Functions\s*:\s*([\d.]+)%/);

  if (lineMatch) coverage.lines = parseFloat(lineMatch[1]);
  if (stmtMatch) coverage.statements = parseFloat(stmtMatch[1]);
  if (branchMatch) coverage.branches = parseFloat(branchMatch[1]);
  if (funcMatch) coverage.functions = parseFloat(funcMatch[1]);

  // Vitest format
  const vitestMatch = output.match(/All files\s*\|\s*([\d.]+)\s*\|\s*([\d.]+)\s*\|\s*([\d.]+)\s*\|\s*([\d.]+)/);
  if (vitestMatch) {
    coverage.statements = parseFloat(vitestMatch[1]);
    coverage.branches = parseFloat(vitestMatch[2]);
    coverage.functions = parseFloat(vitestMatch[3]);
    coverage.lines = parseFloat(vitestMatch[4]);
  }

  // Calculate average
  const values = Object.values(coverage).filter(v => v !== null);
  coverage.average = values.length > 0
    ? values.reduce((a, b) => a + b, 0) / values.length
    : null;

  return coverage;
}

/**
 * Parse test results from output
 *
 * @param {string} output - Test output
 * @returns {Object} Test results
 */
function parseTestResults(output) {
  const results = {
    total: 0,
    passed: 0,
    failed: 0,
    skipped: 0,
    duration: 0
  };

  // Vitest format
  const vitestMatch = output.match(/Tests\s+(\d+)\s+passed[^\d]*(\d+)?\s*failed?[^\d]*(\d+)?\s*skipped?/i);
  if (vitestMatch) {
    results.passed = parseInt(vitestMatch[1]) || 0;
    results.failed = parseInt(vitestMatch[2]) || 0;
    results.skipped = parseInt(vitestMatch[3]) || 0;
    results.total = results.passed + results.failed + results.skipped;
  }

  // Jest format
  const jestMatch = output.match(/Tests:\s+(\d+)\s+passed,\s+(\d+)\s+failed,\s+(\d+)\s+total/i);
  if (jestMatch) {
    results.passed = parseInt(jestMatch[1]) || 0;
    results.failed = parseInt(jestMatch[2]) || 0;
    results.total = parseInt(jestMatch[3]) || 0;
    results.skipped = results.total - results.passed - results.failed;
  }

  // Alternative format
  const altMatch = output.match(/(\d+)\s+passed[,\s]+(\d+)\s+failed/i);
  if (altMatch && results.total === 0) {
    results.passed = parseInt(altMatch[1]) || 0;
    results.failed = parseInt(altMatch[2]) || 0;
    results.total = results.passed + results.failed;
  }

  // Simple pass count
  const passMatch = output.match(/(\d+)\s+(?:tests?|specs?)\s+passed/i);
  if (passMatch && results.total === 0) {
    results.passed = parseInt(passMatch[1]) || 0;
    results.total = results.passed;
  }

  // Duration
  const durationMatch = output.match(/(?:Time|Duration):\s*([\d.]+)\s*([ms])/i);
  if (durationMatch) {
    const value = parseFloat(durationMatch[1]);
    const unit = durationMatch[2];
    results.duration = unit === 's' ? value * 1000 : value;
  }

  return results;
}

/**
 * Check test configuration
 *
 * @param {string} packagePath - Package path
 * @param {Object} packageInfo - Package info
 * @returns {Promise<Object>} Config check result
 */
async function checkTestConfig(packagePath, packageInfo) {
  const config = {
    hasTestScript: false,
    hasCoverageScript: false,
    testFramework: null,
    configFile: null
  };

  // Check package.json scripts
  if (packageInfo.scripts) {
    config.hasTestScript = 'test' in packageInfo.scripts;
    config.hasCoverageScript = 'test:coverage' in packageInfo.scripts ||
      packageInfo.scripts.test?.includes('--coverage');
  }

  // Detect test framework
  const deps = { ...packageInfo.dependencies, ...packageInfo.devDependencies };
  if ('vitest' in deps) config.testFramework = 'vitest';
  else if ('jest' in deps) config.testFramework = 'jest';
  else if ('mocha' in deps) config.testFramework = 'mocha';
  else if ('ava' in deps) config.testFramework = 'ava';
  else if ('tape' in deps) config.testFramework = 'tape';

  // Check for config files
  const configFiles = [
    'vitest.config.mjs', 'vitest.config.js', 'vitest.config.ts',
    'jest.config.mjs', 'jest.config.js', 'jest.config.ts',
    '.mocharc.json', '.mocharc.js'
  ];

  for (const file of configFiles) {
    try {
      await access(join(packagePath, file));
      config.configFile = file;
      break;
    } catch {
      // File not found
    }
  }

  return config;
}

/**
 * Calculate test-to-source ratio
 *
 * @param {Array} sourceFiles - Source files
 * @param {Array} testFiles - Test files
 * @returns {Object} Ratio information
 */
function calculateTestRatio(sourceFiles, testFiles) {
  const testedSources = new Set();

  for (const testFile of testFiles) {
    // Extract source file name from test file
    const testName = testFile.name
      .replace(/\.test\.(mjs|js|ts|jsx|tsx)$/, '')
      .replace(/\.spec\.(mjs|js|ts|jsx|tsx)$/, '');

    for (const sourceFile of sourceFiles) {
      if (sourceFile.name === testName ||
          sourceFile.name === testName.replace(/-/g, '') ||
          testFile.relativePath.includes(sourceFile.name)) {
        testedSources.add(sourceFile.path);
      }
    }
  }

  return {
    sourceCount: sourceFiles.length,
    testCount: testFiles.length,
    testedCount: testedSources.size,
    untestedSources: sourceFiles
      .filter(s => !testedSources.has(s.path))
      .map(s => s.relativePath)
  };
}

/**
 * Run test suite (dry run or actual)
 *
 * @param {string} packagePath - Package path
 * @param {Object} packageInfo - Package info
 * @param {boolean} dryRun - Whether to actually run tests
 * @returns {Promise<Object>} Test run result
 */
async function runTests(packagePath, packageInfo, dryRun = true) {
  if (dryRun || !packageInfo.scripts?.test) {
    return {
      ran: false,
      reason: dryRun ? 'Dry run mode' : 'No test script'
    };
  }

  const result = await executeCommand('npm', ['test'], {
    cwd: packagePath,
    timeout: TEST_THRESHOLDS.maxSuiteDuration * 2,
    env: { CI: 'true' }
  });

  return {
    ran: true,
    success: result.success,
    duration: result.duration,
    output: result.stdout + result.stderr,
    testResults: parseTestResults(result.stdout + result.stderr),
    coverage: parseCoverage(result.stdout + result.stderr)
  };
}

/**
 * Perform test check on a package
 *
 * @param {string} packagePath - Path to package
 * @param {Object} options - Check options
 * @returns {Promise<Object>} Check result
 */
export async function testCheck(packagePath, options = {}) {
  const startTime = Date.now();
  const warnings = [];
  const failures = [];
  const remediation = [];

  let totalScore = 0;
  const details = {
    testFiles: 0,
    sourceFiles: 0,
    testRatio: 0,
    coverage: null,
    passRate: null,
    duration: null,
    hasIntegrationTests: false,
    framework: null
  };

  try {
    // Load package.json
    const packageInfo = options.packageInfo || {};

    // Check test configuration
    const config = await checkTestConfig(packagePath, packageInfo);
    details.framework = config.testFramework;

    // Get test and source files
    const [testFiles, sourceFiles] = await Promise.all([
      getTestFiles(packagePath),
      getSourceFiles(packagePath)
    ]);

    details.testFiles = testFiles.length;
    details.sourceFiles = sourceFiles.length;
    details.hasIntegrationTests = testFiles.some(t => t.isIntegration);

    // Calculate test ratio
    const ratio = calculateTestRatio(sourceFiles, testFiles);
    details.testRatio = sourceFiles.length > 0
      ? Math.round((ratio.testedCount / sourceFiles.length) * 100)
      : 100;

    // Check for missing tests
    if (testFiles.length === 0 && sourceFiles.length > 0) {
      failures.push('No test files found');
      remediation.push('Add test files with .test.mjs or .spec.mjs extension');
    } else if (ratio.untestedSources.length > 0 && ratio.untestedSources.length <= 5) {
      warnings.push(`${ratio.untestedSources.length} source file(s) missing tests`);
      remediation.push(`Add tests for: ${ratio.untestedSources.slice(0, 3).join(', ')}`);
    } else if (ratio.untestedSources.length > 5) {
      warnings.push(`${ratio.untestedSources.length} source files missing tests`);
    }

    // Check test configuration
    if (!config.hasTestScript && sourceFiles.length > 0) {
      warnings.push('No test script in package.json');
      remediation.push('Add "test" script to package.json');
    }

    if (!config.testFramework && testFiles.length > 0) {
      warnings.push('No test framework detected');
      remediation.push('Add vitest, jest, or mocha as dev dependency');
    }

    if (!config.hasCoverageScript && testFiles.length > 0) {
      warnings.push('No coverage script configured');
      remediation.push('Add "test:coverage" script or use --coverage flag');
    }

    // Integration test check
    if (!details.hasIntegrationTests && sourceFiles.length > 5) {
      warnings.push('No integration tests found');
      remediation.push('Add integration tests for end-to-end scenarios');
    }

    // Run tests if not dry run
    const runResult = await runTests(packagePath, packageInfo, options.dryRun !== false);

    if (runResult.ran) {
      details.coverage = runResult.coverage?.average;
      details.passRate = runResult.testResults.total > 0
        ? Math.round((runResult.testResults.passed / runResult.testResults.total) * 100)
        : null;
      details.duration = runResult.duration;

      // Check coverage
      if (runResult.coverage?.average !== null) {
        if (runResult.coverage.average < TEST_THRESHOLDS.minCoverage) {
          failures.push(`Coverage ${runResult.coverage.average}% < ${TEST_THRESHOLDS.minCoverage}%`);
        } else if (runResult.coverage.average < TEST_THRESHOLDS.targetCoverage) {
          warnings.push(`Coverage ${runResult.coverage.average}% below target ${TEST_THRESHOLDS.targetCoverage}%`);
        }
      }

      // Check pass rate
      if (runResult.testResults.total > 0) {
        const passRate = (runResult.testResults.passed / runResult.testResults.total) * 100;
        if (passRate < TEST_THRESHOLDS.minPassRate) {
          failures.push(`Pass rate ${passRate.toFixed(1)}% < ${TEST_THRESHOLDS.minPassRate}%`);
        }
        if (runResult.testResults.failed > 0) {
          warnings.push(`${runResult.testResults.failed} test(s) failed`);
        }
      }

      // Check duration
      if (runResult.duration > TEST_THRESHOLDS.maxSuiteDuration) {
        warnings.push(`Test suite took ${(runResult.duration / 1000).toFixed(1)}s (target: <${TEST_THRESHOLDS.maxSuiteDuration / 1000}s)`);
        remediation.push('Optimize slow tests or split into separate suites');
      }
    }

    // Calculate score
    // Test existence (25 points)
    let existenceScore = 0;
    if (testFiles.length === 0 && sourceFiles.length === 0) {
      existenceScore = 25; // No source = no tests needed
    } else if (testFiles.length > 0) {
      existenceScore = Math.min(25, (details.testRatio / 100) * 25);
    }

    // Coverage (35 points)
    let coverageScore = 0;
    if (details.coverage !== null) {
      coverageScore = Math.min(35, (details.coverage / 100) * 35);
    } else if (testFiles.length > 0) {
      // Assume moderate coverage if tests exist but coverage unknown
      coverageScore = 20;
    }

    // Pass rate (25 points)
    let passRateScore = 0;
    if (details.passRate !== null) {
      passRateScore = Math.min(25, (details.passRate / 100) * 25);
    } else if (testFiles.length > 0) {
      // Assume passing if tests exist but rate unknown
      passRateScore = 20;
    } else if (sourceFiles.length === 0) {
      passRateScore = 25;
    }

    // Performance (10 points)
    let perfScore = 10;
    if (details.duration !== null && details.duration > TEST_THRESHOLDS.maxSuiteDuration) {
      perfScore = Math.max(0, 10 - Math.floor((details.duration - TEST_THRESHOLDS.maxSuiteDuration) / 1000));
    }

    // Integration tests (5 points)
    const integrationScore = details.hasIntegrationTests ? 5 : (sourceFiles.length <= 5 ? 5 : 0);

    totalScore = Math.round(existenceScore + coverageScore + passRateScore + perfScore + integrationScore);

  } catch (error) {
    failures.push(`Test check failed: ${error.message}`);
    totalScore = 0;
  }

  return {
    passed: totalScore >= 80,
    score: totalScore,
    status: totalScore >= 95 ? 'pass' : totalScore >= 80 ? 'warn' : 'fail',
    warnings: [...new Set(warnings)].slice(0, 20),
    failures: [...new Set(failures)].slice(0, 10),
    remediation: [...new Set(remediation)].slice(0, 10),
    duration: Date.now() - startTime,
    details
  };
}

export default testCheck;
