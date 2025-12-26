#!/usr/bin/env node
/**
 * @file Visual Test Report Generator
 * @description
 * Generates HTML test summary with:
 * - Test results overview
 * - Flaky tests detection (failed then passed)
 * - Slow tests (>100ms)
 * - Coverage gaps
 */

import { readFile, writeFile, readdir } from 'fs/promises';
import { join } from 'path';
import { existsSync } from 'fs';

/**
 * Parse vitest JSON output
 * @param {string} filePath - Path to JSON report
 * @returns {Promise<Object>} Parsed results
 */
async function parseVitestResults(filePath) {
  try {
    const content = await readFile(filePath, 'utf-8');
    return JSON.parse(content);
  } catch (error) {
    console.warn(`Could not parse ${filePath}:`, error.message);
    return null;
  }
}

/**
 * Detect flaky tests (tests that failed then passed)
 * @param {Array} testResults - Test results
 * @returns {Array} Flaky test info
 */
function detectFlakyTests(testResults) {
  const flaky = [];
  const testHistory = new Map();

  for (const result of testResults) {
    const key = `${result.file}::${result.name}`;
    if (!testHistory.has(key)) {
      testHistory.set(key, []);
    }
    testHistory.get(key).push(result.status);
  }

  for (const [test, history] of testHistory) {
    if (history.includes('failed') && history.includes('passed')) {
      flaky.push({ test, history });
    }
  }

  return flaky;
}

/**
 * Identify slow tests (>100ms)
 * @param {Array} testResults - Test results
 * @returns {Array} Slow tests
 */
function identifySlowTests(testResults) {
  return testResults
    .filter((test) => test.duration > 100)
    .sort((a, b) => b.duration - a.duration)
    .slice(0, 20); // Top 20 slowest
}

/**
 * Calculate coverage gaps
 * @param {Object} coverage - Coverage data
 * @returns {Array} Files with low coverage
 */
function calculateCoverageGaps(coverage) {
  if (!coverage) return [];

  const gaps = [];
  const threshold = 80;

  for (const [file, data] of Object.entries(coverage)) {
    const lineCoverage = (data.lines.covered / data.lines.total) * 100;
    const branchCoverage = data.branches.total > 0
      ? (data.branches.covered / data.branches.total) * 100
      : 100;

    if (lineCoverage < threshold || branchCoverage < threshold) {
      gaps.push({
        file,
        lineCoverage: lineCoverage.toFixed(2),
        branchCoverage: branchCoverage.toFixed(2),
      });
    }
  }

  return gaps.sort((a, b) => a.lineCoverage - b.lineCoverage);
}

/**
 * Generate HTML report
 * @param {Object} data - Report data
 * @returns {string} HTML content
 */
function generateHTML(data) {
  const {
    timestamp,
    totalTests,
    passed,
    failed,
    skipped,
    duration,
    flakyTests,
    slowTests,
    coverageGaps,
  } = data;

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>UNRDF Test Report</title>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      background: #f5f5f5;
      padding: 20px;
      line-height: 1.6;
    }
    .container { max-width: 1200px; margin: 0 auto; }
    .header {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      padding: 30px;
      border-radius: 10px;
      margin-bottom: 30px;
    }
    .header h1 { font-size: 2em; margin-bottom: 10px; }
    .header .timestamp { opacity: 0.9; font-size: 0.9em; }
    .stats {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
      gap: 20px;
      margin-bottom: 30px;
    }
    .stat-card {
      background: white;
      padding: 20px;
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    .stat-card h3 {
      font-size: 0.9em;
      color: #666;
      margin-bottom: 10px;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }
    .stat-card .value {
      font-size: 2.5em;
      font-weight: bold;
      color: #333;
    }
    .stat-card.passed .value { color: #10b981; }
    .stat-card.failed .value { color: #ef4444; }
    .stat-card.duration .value { font-size: 1.8em; }
    .section {
      background: white;
      padding: 25px;
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      margin-bottom: 20px;
    }
    .section h2 {
      font-size: 1.3em;
      margin-bottom: 15px;
      padding-bottom: 10px;
      border-bottom: 2px solid #f0f0f0;
    }
    .section.warning h2 { color: #f59e0b; }
    .section.error h2 { color: #ef4444; }
    table {
      width: 100%;
      border-collapse: collapse;
    }
    th, td {
      text-align: left;
      padding: 12px;
      border-bottom: 1px solid #f0f0f0;
    }
    th {
      background: #f9fafb;
      font-weight: 600;
      color: #374151;
      font-size: 0.9em;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }
    tr:hover { background: #f9fafb; }
    .badge {
      display: inline-block;
      padding: 4px 12px;
      border-radius: 12px;
      font-size: 0.85em;
      font-weight: 500;
    }
    .badge.slow { background: #fef3c7; color: #92400e; }
    .badge.flaky { background: #fee2e2; color: #991b1b; }
    .badge.gap { background: #dbeafe; color: #1e40af; }
    .empty-state {
      text-align: center;
      padding: 40px;
      color: #9ca3af;
    }
    .empty-state svg {
      width: 48px;
      height: 48px;
      margin-bottom: 10px;
      opacity: 0.5;
    }
  </style>
</head>
<body>
  <div class="container">
    <div class="header">
      <h1>UNRDF Test Report</h1>
      <div class="timestamp">Generated: ${timestamp}</div>
    </div>

    <div class="stats">
      <div class="stat-card">
        <h3>Total Tests</h3>
        <div class="value">${totalTests}</div>
      </div>
      <div class="stat-card passed">
        <h3>Passed</h3>
        <div class="value">${passed}</div>
      </div>
      <div class="stat-card failed">
        <h3>Failed</h3>
        <div class="value">${failed}</div>
      </div>
      <div class="stat-card duration">
        <h3>Duration</h3>
        <div class="value">${(duration / 1000).toFixed(2)}s</div>
      </div>
    </div>

    ${
      flakyTests.length > 0
        ? `
    <div class="section error">
      <h2>🔄 Flaky Tests (${flakyTests.length})</h2>
      <p style="color: #666; margin-bottom: 15px;">
        Tests that failed then passed. These should be investigated and fixed.
      </p>
      <table>
        <thead>
          <tr>
            <th>Test</th>
            <th>History</th>
          </tr>
        </thead>
        <tbody>
          ${flakyTests
            .map(
              (test) => `
            <tr>
              <td>${test.test}</td>
              <td>
                ${test.history.map((status) => `<span class="badge flaky">${status}</span>`).join(' → ')}
              </td>
            </tr>
          `
            )
            .join('')}
        </tbody>
      </table>
    </div>
    `
        : `
    <div class="section">
      <h2>🔄 Flaky Tests</h2>
      <div class="empty-state">
        <div>✓ No flaky tests detected</div>
      </div>
    </div>
    `
    }

    ${
      slowTests.length > 0
        ? `
    <div class="section warning">
      <h2>🐌 Slow Tests (${slowTests.length})</h2>
      <p style="color: #666; margin-bottom: 15px;">
        Tests taking longer than 100ms. Consider optimization or mocking.
      </p>
      <table>
        <thead>
          <tr>
            <th>Test</th>
            <th>File</th>
            <th>Duration</th>
          </tr>
        </thead>
        <tbody>
          ${slowTests
            .map(
              (test) => `
            <tr>
              <td>${test.name}</td>
              <td style="font-family: monospace; font-size: 0.9em;">${test.file}</td>
              <td><span class="badge slow">${test.duration.toFixed(2)}ms</span></td>
            </tr>
          `
            )
            .join('')}
        </tbody>
      </table>
    </div>
    `
        : `
    <div class="section">
      <h2>🐌 Slow Tests</h2>
      <div class="empty-state">
        <div>✓ All tests complete in under 100ms</div>
      </div>
    </div>
    `
    }

    ${
      coverageGaps.length > 0
        ? `
    <div class="section">
      <h2>📊 Coverage Gaps (${coverageGaps.length})</h2>
      <p style="color: #666; margin-bottom: 15px;">
        Files with less than 80% line or branch coverage.
      </p>
      <table>
        <thead>
          <tr>
            <th>File</th>
            <th>Line Coverage</th>
            <th>Branch Coverage</th>
          </tr>
        </thead>
        <tbody>
          ${coverageGaps
            .map(
              (gap) => `
            <tr>
              <td style="font-family: monospace; font-size: 0.9em;">${gap.file}</td>
              <td><span class="badge gap">${gap.lineCoverage}%</span></td>
              <td><span class="badge gap">${gap.branchCoverage}%</span></td>
            </tr>
          `
            )
            .join('')}
        </tbody>
      </table>
    </div>
    `
        : `
    <div class="section">
      <h2>📊 Coverage Gaps</h2>
      <div class="empty-state">
        <div>✓ All files meet 80% coverage threshold</div>
      </div>
    </div>
    `
    }
  </div>
</body>
</html>`;
}

/**
 * Main execution
 */
async function main() {
  console.log('Generating test report...\n');

  // Mock data for demonstration (would parse actual vitest JSON in production)
  const reportData = {
    timestamp: new Date().toISOString(),
    totalTests: 156,
    passed: 154,
    failed: 2,
    skipped: 0,
    duration: 8432,
    flakyTests: [
      {
        test: 'packages/hooks/test/async-hooks.test.mjs::handles concurrent updates',
        history: ['failed', 'passed', 'passed'],
      },
    ],
    slowTests: [
      {
        name: 'processes large dataset',
        file: 'packages/streaming/test/batch.test.mjs',
        duration: 245.32,
      },
      {
        name: 'federation sync',
        file: 'packages/federation/test/sync.test.mjs',
        duration: 156.89,
      },
    ],
    coverageGaps: [
      {
        file: 'packages/core/src/legacy-adapter.mjs',
        lineCoverage: '45.23',
        branchCoverage: '38.12',
      },
      {
        file: 'packages/hooks/src/experimental.mjs',
        lineCoverage: '72.50',
        branchCoverage: '65.00',
      },
    ],
  };

  const html = generateHTML(reportData);
  const outputPath = join(process.cwd(), 'test-report.html');

  await writeFile(outputPath, html, 'utf-8');

  console.log(`✓ Test report generated: ${outputPath}`);
  console.log(`\nSummary:`);
  console.log(`  Total: ${reportData.totalTests}`);
  console.log(`  Passed: ${reportData.passed}`);
  console.log(`  Failed: ${reportData.failed}`);
  console.log(`  Flaky: ${reportData.flakyTests.length}`);
  console.log(`  Slow: ${reportData.slowTests.length}`);
  console.log(`  Coverage Gaps: ${reportData.coverageGaps.length}`);
}

main().catch((error) => {
  console.error('Error generating report:', error);
  process.exit(1);
});
