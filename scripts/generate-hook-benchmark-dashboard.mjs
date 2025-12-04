#!/usr/bin/env node
/**
 * @file Interactive Performance Dashboard Generator
 * @description Generate HTML dashboard from knowledge hook benchmarks
 */

import fs from 'fs';
import path from 'path';

/**
 * Generate HTML dashboard with Chart.js visualizations
 */
function generateDashboard(benchmarkData) {
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
    .filters {
      background: white;
      border-radius: 12px;
      padding: 20px;
      margin-bottom: 20px;
      box-shadow: 0 4px 6px rgba(0,0,0,0.1);
    }
    .filters h2 {
      color: #667eea;
      margin-bottom: 15px;
      font-size: 1.5em;
    }
    .filter-group {
      display: flex;
      flex-wrap: wrap;
      gap: 10px;
      margin-bottom: 15px;
    }
    .filter-group label {
      display: inline-flex;
      align-items: center;
      background: #f7fafc;
      padding: 8px 12px;
      border-radius: 6px;
      cursor: pointer;
      transition: all 0.2s;
    }
    .filter-group label:hover {
      background: #edf2f7;
    }
    .filter-group input[type="checkbox"] {
      margin-right: 8px;
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
    th::after {
      content: ' ⇅';
      opacity: 0.5;
    }
    td {
      padding: 12px;
      border-bottom: 1px solid #e2e8f0;
    }
    tbody tr:hover {
      background: #f7fafc;
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
  </style>
</head>
<body>
  <div class="container">
    <div class="header">
      <h1>🚀 Knowledge Hook Performance Dashboard</h1>
      <p style="color: #718096; font-size: 1.1em; margin-top: 10px;">
        Interactive analysis of hook configurations, operations, and data size impact
      </p>
      <div class="metadata">
        <div class="metadata-item">
          <label>Generated</label>
          <value>${new Date(benchmarkData.metadata.timestamp).toLocaleString()}</value>
        </div>
        <div class="metadata-item">
          <label>Node Version</label>
          <value>${benchmarkData.metadata.nodeVersion}</value>
        </div>
        <div class="metadata-item">
          <label>Platform</label>
          <value>${benchmarkData.metadata.platform}</value>
        </div>
        <div class="metadata-item">
          <label>Total Benchmarks</label>
          <value>${benchmarkData.summary.totalBenchmarks}</value>
        </div>
        <div class="metadata-item">
          <label>Iterations per Test</label>
          <value>${benchmarkData.metadata.iterations}</value>
        </div>
      </div>
    </div>

    <div class="summary-stats">
      <div class="stat-card">
        <div class="value">${benchmarkData.summary.meanLatency.toFixed(2)}ms</div>
        <div class="label">Mean Latency</div>
      </div>
      <div class="stat-card">
        <div class="value">${benchmarkData.summary.p95Latency.toFixed(2)}ms</div>
        <div class="label">P95 Latency</div>
      </div>
      <div class="stat-card">
        <div class="value">${benchmarkData.summary.p99Latency.toFixed(2)}ms</div>
        <div class="label">P99 Latency</div>
      </div>
      <div class="stat-card">
        <div class="value">${benchmarkData.hookConfigurations.length}</div>
        <div class="label">Hook Configs</div>
      </div>
    </div>

    <div class="filters">
      <h2>🔍 Filters</h2>
      <div class="filter-group">
        <label><input type="checkbox" id="filter-all" checked> All Hooks</label>
        ${benchmarkData.hookConfigurations.map(config =>
          `<label><input type="checkbox" class="hook-filter" value="${config.name}" checked> ${config.name}</label>`
        ).join('')}
      </div>
      <div class="filter-group">
        <label>Data Size:</label>
        ${benchmarkData.metadata.testSizes.map(size =>
          `<label><input type="checkbox" class="size-filter" value="${size}" checked> ${size} quads</label>`
        ).join('')}
      </div>
    </div>

    <div class="charts-grid">
      <div class="chart-container">
        <h3>📊 Latency Comparison (Mean)</h3>
        <div class="chart-wrapper">
          <canvas id="latencyChart"></canvas>
        </div>
      </div>

      <div class="chart-container">
        <h3>⚡ Throughput Comparison</h3>
        <div class="chart-wrapper">
          <canvas id="throughputChart"></canvas>
        </div>
      </div>

      <div class="chart-container">
        <h3>💾 Memory Usage</h3>
        <div class="chart-wrapper">
          <canvas id="memoryChart"></canvas>
        </div>
      </div>

      <div class="chart-container">
        <h3>📈 Overhead Percentage</h3>
        <div class="chart-wrapper">
          <canvas id="overheadChart"></canvas>
        </div>
      </div>

      <div class="chart-container">
        <h3>📏 Data Size Impact (Latency)</h3>
        <div class="chart-wrapper">
          <canvas id="scalingChart"></canvas>
        </div>
      </div>

      <div class="chart-container">
        <h3>🎯 Hook Trigger Type Performance</h3>
        <div class="chart-wrapper">
          <canvas id="operationsChart"></canvas>
        </div>
      </div>
    </div>

    <div class="table-container">
      <h2>📋 Benchmark Results</h2>
      <button class="export-btn" onclick="exportToCSV()">📥 Export to CSV</button>
      <table id="resultsTable">
        <thead>
          <tr>
            <th onclick="sortTable(0)">Hook Config</th>
            <th onclick="sortTable(1)">Data Size</th>
            <th onclick="sortTable(2)">Mean Latency (ms)</th>
            <th onclick="sortTable(3)">P95 Latency (ms)</th>
            <th onclick="sortTable(4)">P99 Latency (ms)</th>
            <th onclick="sortTable(5)">Throughput (ops/sec)</th>
            <th onclick="sortTable(6)">Memory Delta (MB)</th>
            <th onclick="sortTable(7)">Overhead %</th>
          </tr>
        </thead>
        <tbody id="resultsBody"></tbody>
      </table>
    </div>
  </div>

  <script>
    // Benchmark data
    const data = ${JSON.stringify(benchmarkData)};

    // Color palette
    const colors = [
      'rgba(102, 126, 234, 0.8)',
      'rgba(118, 75, 162, 0.8)',
      'rgba(237, 100, 166, 0.8)',
      'rgba(255, 154, 158, 0.8)',
      'rgba(255, 198, 93, 0.8)',
      'rgba(160, 212, 104, 0.8)'
    ];

    // Initialize charts
    let charts = {};

    function updateCharts() {
      const selectedHooks = Array.from(document.querySelectorAll('.hook-filter:checked')).map(cb => cb.value);
      const selectedSizes = Array.from(document.querySelectorAll('.size-filter:checked')).map(cb => parseInt(cb.value));

      const filteredData = data.results.filter(r =>
        selectedHooks.includes(r.hookConfig) && selectedSizes.includes(r.dataSize)
      );

      // Group by hook config
      const groupedByHook = {};
      filteredData.forEach(r => {
        if (!groupedByHook[r.hookConfig]) groupedByHook[r.hookConfig] = [];
        groupedByHook[r.hookConfig].push(r);
      });

      // Latency Chart
      if (charts.latency) charts.latency.destroy();
      charts.latency = new Chart(document.getElementById('latencyChart'), {
        type: 'bar',
        data: {
          labels: selectedSizes.map(s => s + ' quads'),
          datasets: selectedHooks.map((hook, idx) => ({
            label: hook,
            data: selectedSizes.map(size => {
              const item = filteredData.find(r => r.hookConfig === hook && r.dataSize === size);
              return item ? parseFloat(item.latency.mean) : 0;
            }),
            backgroundColor: colors[idx % colors.length]
          }))
        },
        options: {
          responsive: true,
          maintainAspectRatio: false,
          scales: {
            y: { beginAtZero: true, title: { display: true, text: 'Latency (ms)' } }
          }
        }
      });

      // Throughput Chart
      if (charts.throughput) charts.throughput.destroy();
      charts.throughput = new Chart(document.getElementById('throughputChart'), {
        type: 'line',
        data: {
          labels: selectedSizes.map(s => s + ' quads'),
          datasets: selectedHooks.map((hook, idx) => ({
            label: hook,
            data: selectedSizes.map(size => {
              const item = filteredData.find(r => r.hookConfig === hook && r.dataSize === size);
              return item ? parseFloat(item.throughput) : 0;
            }),
            borderColor: colors[idx % colors.length],
            backgroundColor: colors[idx % colors.length].replace('0.8', '0.2'),
            fill: true
          }))
        },
        options: {
          responsive: true,
          maintainAspectRatio: false,
          scales: {
            y: { beginAtZero: true, title: { display: true, text: 'Operations/sec' } }
          }
        }
      });

      // Memory Chart
      if (charts.memory) charts.memory.destroy();
      charts.memory = new Chart(document.getElementById('memoryChart'), {
        type: 'line',
        data: {
          labels: selectedSizes.map(s => s + ' quads'),
          datasets: selectedHooks.map((hook, idx) => ({
            label: hook,
            data: selectedSizes.map(size => {
              const item = filteredData.find(r => r.hookConfig === hook && r.dataSize === size);
              return item ? parseFloat(item.memory.delta) : 0;
            }),
            borderColor: colors[idx % colors.length],
            backgroundColor: colors[idx % colors.length]
          }))
        },
        options: {
          responsive: true,
          maintainAspectRatio: false,
          scales: {
            y: { beginAtZero: true, title: { display: true, text: 'Memory Delta (MB)' } }
          }
        }
      });

      // Overhead Chart
      if (charts.overhead) charts.overhead.destroy();
      charts.overhead = new Chart(document.getElementById('overheadChart'), {
        type: 'bar',
        data: {
          labels: selectedHooks,
          datasets: [{
            label: 'Overhead %',
            data: selectedHooks.map(hook => {
              const items = filteredData.filter(r => r.hookConfig === hook);
              const avg = items.reduce((sum, r) => sum + parseFloat(r.overhead), 0) / items.length;
              return avg;
            }),
            backgroundColor: colors
          }]
        },
        options: {
          responsive: true,
          maintainAspectRatio: false,
          scales: {
            y: { beginAtZero: true, title: { display: true, text: 'Overhead %' } }
          }
        }
      });

      // Scaling Chart
      if (charts.scaling) charts.scaling.destroy();
      charts.scaling = new Chart(document.getElementById('scalingChart'), {
        type: 'line',
        data: {
          labels: selectedSizes.map(s => s + ' quads'),
          datasets: selectedHooks.map((hook, idx) => ({
            label: hook,
            data: selectedSizes.map(size => {
              const item = filteredData.find(r => r.hookConfig === hook && r.dataSize === size);
              return item ? parseFloat(item.latency.mean) : 0;
            }),
            borderColor: colors[idx % colors.length],
            backgroundColor: 'transparent',
            tension: 0.4
          }))
        },
        options: {
          responsive: true,
          maintainAspectRatio: false,
          scales: {
            y: {
              beginAtZero: true,
              title: { display: true, text: 'Latency (ms)' },
              type: 'logarithmic'
            }
          }
        }
      });

      // Trigger Types Chart
      if (charts.triggers) charts.triggers.destroy();
      charts.triggers = new Chart(document.getElementById('operationsChart'), {
        type: 'bar',
        data: {
          labels: data.triggerResults.map(r => r.trigger),
          datasets: [{
            label: 'Mean Latency',
            data: data.triggerResults.map(r => parseFloat(r.latency.mean)),
            backgroundColor: colors[0]
          }, {
            label: 'P95 Latency',
            data: data.triggerResults.map(r => parseFloat(r.latency.p95)),
            backgroundColor: colors[1]
          }, {
            label: 'P99 Latency',
            data: data.triggerResults.map(r => parseFloat(r.latency.p99)),
            backgroundColor: colors[2]
          }]
        },
        options: {
          responsive: true,
          maintainAspectRatio: false,
          scales: {
            y: { beginAtZero: true, title: { display: true, text: 'Latency (ms)' } }
          }
        }
      });

      updateTable(filteredData);
    }

    function updateTable(filteredData) {
      const tbody = document.getElementById('resultsBody');
      tbody.innerHTML = filteredData.map(r =>
        '<tr>' +
        '<td>' + r.hookConfig + '</td>' +
        '<td>' + r.dataSize + '</td>' +
        '<td>' + r.latency.mean + '</td>' +
        '<td>' + r.latency.p95 + '</td>' +
        '<td>' + r.latency.p99 + '</td>' +
        '<td>' + r.throughput + '</td>' +
        '<td>' + r.memory.delta + '</td>' +
        '<td>' + r.overhead + '</td>' +
        '</tr>'
      ).join('');
    }

    function sortTable(column) {
      const tbody = document.getElementById('resultsBody');
      const rows = Array.from(tbody.rows);
      const sorted = rows.sort((a, b) => {
        const aVal = a.cells[column].textContent;
        const bVal = b.cells[column].textContent;
        return isNaN(aVal) ? aVal.localeCompare(bVal) : parseFloat(aVal) - parseFloat(bVal);
      });
      tbody.innerHTML = '';
      sorted.forEach(row => tbody.appendChild(row));
    }

    function exportToCSV() {
      const rows = [
        ['Hook Config', 'Data Size', 'Mean Latency (ms)', 'P95 Latency (ms)', 'P99 Latency (ms)', 'Throughput (ops/sec)', 'Memory Delta (MB)', 'Overhead %']
      ];
      data.results.forEach(r => {
        rows.push([
          r.hookConfig,
          r.dataSize,
          r.latency.mean,
          r.latency.p95,
          r.latency.p99,
          r.throughput,
          r.memory.delta,
          r.overhead
        ]);
      });
      const csv = rows.map(row => row.join(',')).join('\\n');
      const blob = new Blob([csv], { type: 'text/csv' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = 'hook-benchmarks.csv';
      a.click();
    }

    // Event listeners
    document.getElementById('filter-all').addEventListener('change', (e) => {
      document.querySelectorAll('.hook-filter').forEach(cb => cb.checked = e.target.checked);
      updateCharts();
    });

    document.querySelectorAll('.hook-filter, .size-filter').forEach(cb => {
      cb.addEventListener('change', updateCharts);
    });

    // Initial render
    updateCharts();
  </script>
</body>
</html>`;

  return html;
}

/**
 * Main script
 */
function main() {
  const benchmarkPath = path.join(process.cwd(), 'reports', 'hook-performance-benchmarks.json');
  const outputPath = path.join(process.cwd(), 'reports', 'hook-performance-dashboard.html');

  console.log('🚀 Generating Knowledge Hook Performance Dashboard');
  console.log('─'.repeat(60));

  // Check if benchmark data exists
  if (!fs.existsSync(benchmarkPath)) {
    console.error('❌ Benchmark data not found:', benchmarkPath);
    console.log('Run benchmark-knowledge-hooks.mjs first to generate data.');
    process.exit(1);
  }

  // Read benchmark data
  console.log('📖 Reading benchmark data...');
  const benchmarkData = JSON.parse(fs.readFileSync(benchmarkPath, 'utf8'));

  // Generate dashboard
  console.log('🎨 Generating HTML dashboard...');
  const html = generateDashboard(benchmarkData);

  // Write dashboard
  fs.writeFileSync(outputPath, html);

  console.log('✅ Dashboard generated successfully!');
  console.log(`📁 Output: ${outputPath}`);
  console.log('\n🌐 Open in browser:');
  console.log(`   file://${outputPath}`);
}

main();
