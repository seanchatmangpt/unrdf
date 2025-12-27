#!/usr/bin/env node
/**
 * @file Generate interactive dashboard from 80/20 benchmark results
 * @description Converts benchmark JSON to Chart.js dashboard
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

/**
 * Convert benchmark results to dashboard format
 */
function convertBenchmarksToDashboardFormat(benchmarkData) {
  const converted = {
    metadata: {
      timestamp: benchmarkData.timestamp || new Date().toISOString(),
      nodeVersion: process.version,
      platform: process.platform,
      iterations: 100,
      testSizes: [10, 100, 1000, 10000],
      generatedFrom: 'hyper-advanced-80-20-benchmarks'
    },
    summary: {
      totalBenchmarks: benchmarkData.summary.totalBenchmarks,
      meanLatency: 0,
      p95Latency: 0,
      p99Latency: 0,
      totalTests: benchmarkData.summary.totalTests,
      passedTests: benchmarkData.summary.passed,
      failedTests: benchmarkData.summary.failed
    },
    hookConfigurations: [],
    results: [],
    triggerResults: []
  };

  // Extract hook configurations and results
  const latencies = [];
  const p95Latencies = [];
  const p99Latencies = [];

  benchmarkData.benchmarks.forEach(benchmark => {
    const benchmarkName = benchmark.name;

    // Add hook configuration
    if (!converted.hookConfigurations.find(c => c.name === benchmarkName)) {
      converted.hookConfigurations.push({
        name: benchmarkName,
        description: `Performance benchmark for ${benchmarkName}`,
        tests: benchmark.tests.length
      });
    }

    // Add results for each test
    benchmark.tests.forEach(test => {
      // Skip if no latency data (concurrent tests only have throughput)
      if (!test.latency) {
        return;
      }

      const scenarioName = test.scale || test.complexity || `${benchmarkName}-${Object.keys(test).join('-')}`;
      const dataSize = test.hookCount || test.iterations || 100;

      const result = {
        hookConfig: benchmarkName,
        scenario: scenarioName,
        dataSize: dataSize,
        latency: {
          mean: parseFloat(test.latency.mean.toFixed(2)),
          p50: parseFloat(test.latency.p50.toFixed(2)),
          p95: parseFloat(test.latency.p95.toFixed(2)),
          p99: parseFloat(test.latency.p99.toFixed(2))
        },
        throughput: test.throughputHooksPerSec || test.throughputOpsPerSec || 0,
        memory: {
          delta: 0,
          peak: 0
        },
        overhead: ((test.latency.p95 / (test.latency.mean || 1)) * 100).toFixed(2),
        passed: test.passed
      };

      converted.results.push(result);

      // Collect latencies for summary
      latencies.push(test.latency.mean);
      p95Latencies.push(test.latency.p95);
      p99Latencies.push(test.latency.p99);
    });

    // Add trigger/operation type results
    if (benchmark.name === 'hook-execution-latency') {
      benchmark.tests.forEach(test => {
        if (test.latency) {
          converted.triggerResults.push({
            trigger: test.complexity || 'unknown',
            latency: {
              mean: parseFloat(test.latency.mean.toFixed(2)),
              p95: parseFloat(test.latency.p95.toFixed(2)),
              p99: parseFloat(test.latency.p99.toFixed(2))
            },
            throughput: test.throughputOpsPerSec || 0
          });
        }
      });
    }
  });

  // Calculate summary statistics
  if (latencies.length > 0) {
    converted.summary.meanLatency = parseFloat((latencies.reduce((a, b) => a + b) / latencies.length).toFixed(2));
    converted.summary.p95Latency = parseFloat((p95Latencies.reduce((a, b) => a + b) / p95Latencies.length).toFixed(2));
    converted.summary.p99Latency = parseFloat((p99Latencies.reduce((a, b) => a + b) / p99Latencies.length).toFixed(2));
  }

  return converted;
}

/**
 * Generate HTML dashboard with Chart.js visualizations
 */
function generateDashboard(dashboardData) {
  const html = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Knowledge Hook Performance Dashboard</title>
  <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.0/dist/chart.umd.min.js"></script>
  <style>
    * {
      margin: 0;
      padding: 0;
      box-sizing: border-box;
    }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: #333;
      padding: 20px;
    }
    .container {
      max-width: 1400px;
      margin: 0 auto;
    }
    .header {
      background: white;
      border-radius: 12px;
      padding: 30px;
      margin-bottom: 20px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.1);
    }
    .header h1 {
      color: #667eea;
      font-size: 2.5em;
      margin-bottom: 10px;
    }
    .metadata {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
      gap: 15px;
      margin-top: 20px;
    }
    .metadata-item {
      background: #f7fafc;
      padding: 15px;
      border-radius: 8px;
      border-left: 4px solid #667eea;
    }
    .metadata-item label {
      display: block;
      font-size: 0.85em;
      color: #718096;
      margin-bottom: 5px;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }
    .metadata-item value {
      display: block;
      font-size: 1.2em;
      font-weight: 600;
      color: #2d3748;
    }
    .summary-stats {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
      gap: 15px;
      margin-bottom: 20px;
    }
    .stat-card {
      background: white;
      border-radius: 12px;
      padding: 20px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      text-align: center;
    }
    .stat-card .value {
      font-size: 2.5em;
      font-weight: 700;
      color: #667eea;
      margin-bottom: 5px;
    }
    .stat-card .label {
      font-size: 1em;
      color: #718096;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }
    .stat-card.passed {
      border-top: 4px solid #48bb78;
    }
    .stat-card.failed {
      border-top: 4px solid #f56565;
    }
    .charts-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(600px, 1fr));
      gap: 20px;
      margin-bottom: 20px;
    }
    .chart-container {
      background: white;
      border-radius: 12px;
      padding: 20px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.1);
    }
    .chart-container h3 {
      color: #667eea;
      margin-bottom: 15px;
      font-size: 1.3em;
    }
    .chart-wrapper {
      position: relative;
      height: 400px;
    }
    .table-container {
      background: white;
      border-radius: 12px;
      padding: 20px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      overflow-x: auto;
    }
    .table-container h2 {
      color: #667eea;
      margin-bottom: 15px;
      font-size: 1.5em;
    }
    table {
      width: 100%;
      border-collapse: collapse;
      font-size: 0.9em;
    }
    thead {
      background: #667eea;
      color: white;
    }
    th {
      padding: 12px;
      text-align: left;
      font-weight: 600;
      cursor: pointer;
      user-select: none;
    }
    th:hover {
      background: #5568d3;
    }
    td {
      padding: 12px;
      border-bottom: 1px solid #e2e8f0;
    }
    tbody tr:hover {
      background: #f7fafc;
    }
    .status-badge {
      display: inline-block;
      padding: 4px 8px;
      border-radius: 4px;
      font-size: 0.85em;
      font-weight: 600;
    }
    .status-badge.pass {
      background: #c6f6d5;
      color: #22543d;
    }
    .status-badge.fail {
      background: #fed7d7;
      color: #742a2a;
    }
    .export-btn {
      background: #667eea;
      color: white;
      border: none;
      padding: 10px 20px;
      border-radius: 6px;
      cursor: pointer;
      font-size: 1em;
      margin-bottom: 15px;
      transition: all 0.2s;
    }
    .export-btn:hover {
      background: #5568d3;
      transform: translateY(-2px);
      box-shadow: 0 4px 8px rgba(0,0,0,0.2);
    }
  </style>
</head>
<body>
  <div class="container">
    <div class="header">
      <h1>🚀 Knowledge Hook Performance Dashboard (80/20 Benchmarks)</h1>
      <p style="color: #718096; font-size: 1.1em; margin-top: 10px;">
        Hyper-advanced performance analysis of the 20% metrics driving 80% of impact
      </p>
      <div class="metadata">
        <div class="metadata-item">
          <label>Generated</label>
          <value>${new Date(dashboardData.metadata.timestamp).toLocaleString()}</value>
        </div>
        <div class="metadata-item">
          <label>Node Version</label>
          <value>${dashboardData.metadata.nodeVersion}</value>
        </div>
        <div class="metadata-item">
          <label>Platform</label>
          <value>${dashboardData.metadata.platform}</value>
        </div>
        <div class="metadata-item">
          <label>Total Benchmarks</label>
          <value>${dashboardData.summary.totalBenchmarks}</value>
        </div>
        <div class="metadata-item">
          <label>Total Tests</label>
          <value>${dashboardData.summary.totalTests}</value>
        </div>
      </div>
    </div>

    <div class="summary-stats">
      <div class="stat-card passed">
        <div class="value">${dashboardData.summary.meanLatency.toFixed(2)}ms</div>
        <div class="label">Mean Latency</div>
      </div>
      <div class="stat-card">
        <div class="value">${dashboardData.summary.p95Latency.toFixed(2)}ms</div>
        <div class="label">P95 Latency</div>
      </div>
      <div class="stat-card">
        <div class="value">${dashboardData.summary.p99Latency.toFixed(2)}ms</div>
        <div class="label">P99 Latency</div>
      </div>
      <div class="stat-card ${dashboardData.summary.passedTests > dashboardData.summary.failedTests ? 'passed' : 'failed'}">
        <div class="value">${dashboardData.summary.passedTests}/${dashboardData.summary.totalTests}</div>
        <div class="label">Tests Passed</div>
      </div>
    </div>

    <div class="charts-grid">
      <div class="chart-container">
        <h3>📊 Latency by Benchmark (Mean)</h3>
        <div class="chart-wrapper">
          <canvas id="latencyChart"></canvas>
        </div>
      </div>

      <div class="chart-container">
        <h3>⚡ Throughput Performance</h3>
        <div class="chart-wrapper">
          <canvas id="throughputChart"></canvas>
        </div>
      </div>

      <div class="chart-container">
        <h3>🎯 Latency Distribution (P50/P95/P99)</h3>
        <div class="chart-wrapper">
          <canvas id="percentileChart"></canvas>
        </div>
      </div>

      <div class="chart-container">
        <h3>📈 Performance Overhead %</h3>
        <div class="chart-wrapper">
          <canvas id="overheadChart"></canvas>
        </div>
      </div>
    </div>

    <div class="table-container">
      <h2>📋 Detailed Benchmark Results</h2>
      <button class="export-btn" onclick="exportToCSV()">📥 Export to CSV</button>
      <table>
        <thead>
          <tr>
            <th>Benchmark</th>
            <th>Scenario</th>
            <th>Mean Latency (ms)</th>
            <th>P95 Latency (ms)</th>
            <th>P99 Latency (ms)</th>
            <th>Throughput (ops/sec)</th>
            <th>Overhead %</th>
            <th>Status</th>
          </tr>
        </thead>
        <tbody id="resultsBody"></tbody>
      </table>
    </div>
  </div>

  <script>
    const data = ${JSON.stringify(dashboardData)};
    const colors = [
      'rgba(102, 126, 234, 0.8)',
      'rgba(118, 75, 162, 0.8)',
      'rgba(237, 100, 166, 0.8)',
      'rgba(255, 154, 158, 0.8)',
      'rgba(255, 198, 93, 0.8)',
      'rgba(160, 212, 104, 0.8)'
    ];

    // Group results by benchmark
    const byBenchmark = {};
    data.results.forEach(r => {
      if (!byBenchmark[r.hookConfig]) byBenchmark[r.hookConfig] = [];
      byBenchmark[r.hookConfig].push(r);
    });

    // Latency Chart
    new Chart(document.getElementById('latencyChart'), {
      type: 'bar',
      data: {
        labels: Object.keys(byBenchmark),
        datasets: [{
          label: 'Mean Latency (ms)',
          data: Object.values(byBenchmark).map(results =>
            (results.reduce((sum, r) => sum + r.latency.mean, 0) / results.length).toFixed(2)
          ),
          backgroundColor: colors.slice(0, Object.keys(byBenchmark).length)
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        scales: { y: { beginAtZero: true } }
      }
    });

    // Throughput Chart
    new Chart(document.getElementById('throughputChart'), {
      type: 'line',
      data: {
        labels: Object.keys(byBenchmark),
        datasets: [{
          label: 'Throughput (ops/sec)',
          data: Object.values(byBenchmark).map(results =>
            (results.reduce((sum, r) => sum + r.throughput, 0) / results.length).toFixed(0)
          ),
          borderColor: colors[0],
          backgroundColor: colors[0].replace('0.8', '0.2'),
          fill: true,
          tension: 0.4
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        scales: { y: { beginAtZero: true } }
      }
    });

    // Percentile Chart
    if (data.triggerResults && data.triggerResults.length > 0) {
      new Chart(document.getElementById('percentileChart'), {
        type: 'bar',
        data: {
          labels: data.triggerResults.map(r => r.trigger),
          datasets: [
            {
              label: 'Mean',
              data: data.triggerResults.map(r => r.latency.mean),
              backgroundColor: colors[0]
            },
            {
              label: 'P95',
              data: data.triggerResults.map(r => r.latency.p95),
              backgroundColor: colors[1]
            },
            {
              label: 'P99',
              data: data.triggerResults.map(r => r.latency.p99),
              backgroundColor: colors[2]
            }
          ]
        },
        options: {
          responsive: true,
          maintainAspectRatio: false,
          scales: { y: { beginAtZero: true } }
        }
      });
    }

    // Overhead Chart
    new Chart(document.getElementById('overheadChart'), {
      type: 'bar',
      data: {
        labels: Object.keys(byBenchmark),
        datasets: [{
          label: 'Overhead %',
          data: Object.values(byBenchmark).map(results =>
            (results.reduce((sum, r) => sum + parseFloat(r.overhead), 0) / results.length).toFixed(2)
          ),
          backgroundColor: colors.slice(0, Object.keys(byBenchmark).length)
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        scales: { y: { beginAtZero: true } }
      }
    });

    // Populate table
    const tbody = document.getElementById('resultsBody');
    data.results.forEach(r => {
      const row = document.createElement('tr');
      row.innerHTML = \`
        <td>\${r.hookConfig}</td>
        <td>\${r.scenario}</td>
        <td>\${r.latency.mean.toFixed(2)}</td>
        <td>\${r.latency.p95.toFixed(2)}</td>
        <td>\${r.latency.p99.toFixed(2)}</td>
        <td>\${r.throughput.toFixed(0)}</td>
        <td>\${r.overhead}%</td>
        <td><span class="status-badge \${r.passed ? 'pass' : 'fail'}">\${r.passed ? '✓ PASS' : '✗ FAIL'}</span></td>
      \`;
      tbody.appendChild(row);
    });

    function exportToCSV() {
      const headers = ['Benchmark', 'Scenario', 'Mean Latency (ms)', 'P95 Latency (ms)', 'P99 Latency (ms)', 'Throughput (ops/sec)', 'Overhead %', 'Status'];
      const rows = [headers];
      data.results.forEach(r => {
        rows.push([
          r.hookConfig,
          r.scenario,
          r.latency.mean,
          r.latency.p95,
          r.latency.p99,
          r.throughput,
          r.overhead,
          r.passed ? 'PASS' : 'FAIL'
        ]);
      });
      const csv = rows.map(row => row.join(',')).join('\\n');
      const blob = new Blob([csv], { type: 'text/csv' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = 'knowledge-hooks-benchmarks.csv';
      a.click();
    }
  </script>
</body>
</html>`;

  return html;
}

/**
 * Main script
 */
function main() {
  const benchmarkPath = path.join(__dirname, 'results', 'benchmark-results.json');
  const dashboardPath = path.join(__dirname, 'dashboard.html');

  console.log('🚀 Generating Knowledge Hook Performance Dashboard');
  console.log('─'.repeat(60));

  // Check if benchmark data exists
  if (!fs.existsSync(benchmarkPath)) {
    console.error('❌ Benchmark data not found:', benchmarkPath);
    console.log('Run: node --expose-gc benchmarks/runner.mjs');
    process.exit(1);
  }

  // Read benchmark data
  console.log('📖 Reading benchmark results...');
  const benchmarkData = JSON.parse(fs.readFileSync(benchmarkPath, 'utf8'));

  // Convert to dashboard format
  console.log('🔄 Converting to dashboard format...');
  const dashboardData = convertBenchmarksToDashboardFormat(benchmarkData);

  // Generate dashboard
  console.log('🎨 Generating HTML dashboard...');
  const html = generateDashboard(dashboardData);

  // Write dashboard
  fs.writeFileSync(dashboardPath, html);

  console.log('✅ Dashboard generated successfully!');
  console.log(`📁 Output: ${dashboardPath}`);
  console.log('\n🌐 Open in browser:');
  console.log(`   file://${dashboardPath}`);
}

main();
