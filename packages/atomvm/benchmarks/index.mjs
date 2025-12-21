/**
 * Comprehensive Performance Benchmark Suite - Master Runner
 *
 * Executes all performance benchmarks and generates detailed reports.
 *
 * @module benchmarks/index
 */

import { performance } from 'perf_hooks';
import { writeFileSync } from 'fs';
import { join } from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

import { runComprehensiveLatencyBenchmarks } from './latency-benchmark.mjs';
import { runComprehensiveMemoryBenchmarks } from './memory-benchmark.mjs';
import { runComprehensiveStateMachineBenchmarks } from './state-machine-benchmark.mjs';
import { runComprehensiveSLABenchmarks } from './sla-overhead-benchmark.mjs';
import { runComprehensiveCircuitBreakerBenchmarks } from './circuit-breaker-benchmark.mjs';
import { runComprehensiveConcurrencyBenchmarks } from './concurrency-benchmark.mjs';
import { runComprehensiveResourceBenchmarks } from './resource-usage-benchmark.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Generate HTML performance report
 * @param {Object} results - All benchmark results
 * @param {number} totalTime - Total benchmark execution time
 * @returns {string} HTML report
 */
function generateHTMLReport(results, totalTime) {
  const timestamp = new Date().toISOString();

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>AtomVM Performance Benchmark Report</title>
  <style>
    body {
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      max-width: 1200px;
      margin: 0 auto;
      padding: 20px;
      background: #f5f5f5;
    }
    .header {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      padding: 30px;
      border-radius: 10px;
      margin-bottom: 30px;
    }
    .header h1 {
      margin: 0;
      font-size: 2.5em;
    }
    .header p {
      margin: 10px 0 0 0;
      opacity: 0.9;
    }
    .section {
      background: white;
      padding: 25px;
      margin-bottom: 20px;
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    .section h2 {
      margin-top: 0;
      color: #667eea;
      border-bottom: 2px solid #667eea;
      padding-bottom: 10px;
    }
    .metric {
      display: flex;
      justify-content: space-between;
      padding: 10px 0;
      border-bottom: 1px solid #eee;
    }
    .metric:last-child {
      border-bottom: none;
    }
    .metric-name {
      font-weight: 600;
      color: #333;
    }
    .metric-value {
      color: #666;
      font-family: 'Courier New', monospace;
    }
    .status-pass {
      color: #27ae60;
      font-weight: bold;
    }
    .status-fail {
      color: #e74c3c;
      font-weight: bold;
    }
    .status-warn {
      color: #f39c12;
      font-weight: bold;
    }
    table {
      width: 100%;
      border-collapse: collapse;
      margin: 20px 0;
    }
    th, td {
      padding: 12px;
      text-align: left;
      border-bottom: 1px solid #ddd;
    }
    th {
      background: #667eea;
      color: white;
      font-weight: 600;
    }
    tr:hover {
      background: #f9f9f9;
    }
    .summary-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
      gap: 20px;
      margin: 20px 0;
    }
    .summary-card {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      padding: 20px;
      border-radius: 8px;
      text-align: center;
    }
    .summary-card h3 {
      margin: 0 0 10px 0;
      font-size: 1em;
      opacity: 0.9;
    }
    .summary-card .value {
      font-size: 2em;
      font-weight: bold;
      margin: 10px 0;
    }
    .summary-card .unit {
      font-size: 0.9em;
      opacity: 0.8;
    }
  </style>
</head>
<body>
  <div class="header">
    <h1>AtomVM Performance Benchmark Report</h1>
    <p>Generated: ${timestamp}</p>
    <p>Total Benchmark Duration: ${totalTime.toFixed(2)}s</p>
  </div>

  <div class="section">
    <h2>Executive Summary</h2>
    <div class="summary-grid">
      <div class="summary-card">
        <h3>Latency (P50)</h3>
        <div class="value">${results.latency.benchmark_1000.percentiles.p50}</div>
        <div class="unit">ms</div>
      </div>
      <div class="summary-card">
        <h3>SLA Compliance</h3>
        <div class="value">${results.latency.benchmark_1000.sla.met ? '✓' : '✗'}</div>
        <div class="unit">${results.latency.benchmark_1000.sla.complianceRate}%</div>
      </div>
      <div class="summary-card">
        <h3>Throughput</h3>
        <div class="value">${results.concurrency.throughput_100.throughput}</div>
        <div class="unit">ops/sec</div>
      </div>
      <div class="summary-card">
        <h3>Memory Growth</h3>
        <div class="value">${results.memory.growth.growthRate}</div>
        <div class="unit">MB/s</div>
      </div>
    </div>
  </div>

  <div class="section">
    <h2>1. Latency Measurements</h2>
    <table>
      <thead>
        <tr>
          <th>Iterations</th>
          <th>Mean (ms)</th>
          <th>P50 (ms)</th>
          <th>P95 (ms)</th>
          <th>P99 (ms)</th>
          <th>SLA Met</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>100</td>
          <td>${results.latency.benchmark_100.mean}</td>
          <td>${results.latency.benchmark_100.percentiles.p50}</td>
          <td>${results.latency.benchmark_100.percentiles.p95}</td>
          <td>${results.latency.benchmark_100.percentiles.p99}</td>
          <td class="${results.latency.benchmark_100.sla.met ? 'status-pass' : 'status-fail'}">
            ${results.latency.benchmark_100.sla.met ? '✓ YES' : '✗ NO'}
          </td>
        </tr>
        <tr>
          <td>1,000</td>
          <td>${results.latency.benchmark_1000.mean}</td>
          <td>${results.latency.benchmark_1000.percentiles.p50}</td>
          <td>${results.latency.benchmark_1000.percentiles.p95}</td>
          <td>${results.latency.benchmark_1000.percentiles.p99}</td>
          <td class="${results.latency.benchmark_1000.sla.met ? 'status-pass' : 'status-fail'}">
            ${results.latency.benchmark_1000.sla.met ? '✓ YES' : '✗ NO'}
          </td>
        </tr>
        <tr>
          <td>10,000</td>
          <td>${results.latency.benchmark_10000.mean}</td>
          <td>${results.latency.benchmark_10000.percentiles.p50}</td>
          <td>${results.latency.benchmark_10000.percentiles.p95}</td>
          <td>${results.latency.benchmark_10000.percentiles.p99}</td>
          <td class="${results.latency.benchmark_10000.sla.met ? 'status-pass' : 'status-fail'}">
            ${results.latency.benchmark_10000.sla.met ? '✓ YES' : '✗ NO'}
          </td>
        </tr>
      </tbody>
    </table>
    <div class="metric">
      <span class="metric-name">SLA Target</span>
      <span class="metric-value">&lt;10ms per roundtrip</span>
    </div>
    <div class="metric">
      <span class="metric-name">Error Rate Target</span>
      <span class="metric-value">&lt;0.1% (1 per 1000)</span>
    </div>
  </div>

  <div class="section">
    <h2>2. Memory Profiling</h2>
    <div class="metric">
      <span class="metric-name">Startup Memory (Initial Heap)</span>
      <span class="metric-value">${results.memory.startup.initial.heapUsed} MB</span>
    </div>
    <div class="metric">
      <span class="metric-name">Startup Memory (Post-Load Heap)</span>
      <span class="metric-value">${results.memory.startup.postLoad.heapUsed} MB (+${results.memory.startup.delta.heapUsed} MB)</span>
    </div>
    <div class="metric">
      <span class="metric-name">Growth Rate (1000 ops)</span>
      <span class="metric-value">${results.memory.growth.growthRate} MB/s</span>
    </div>
    <div class="metric">
      <span class="metric-name">Memory Per Operation</span>
      <span class="metric-value">${results.memory.growth.memoryPerOperation} KB</span>
    </div>
    <div class="metric">
      <span class="metric-name">Memory Leak Detected</span>
      <span class="metric-value ${results.memory.leaks.leakDetected ? 'status-warn' : 'status-pass'}">
        ${results.memory.leaks.leakDetected ? '⚠ YES' : '✓ NO'}
      </span>
    </div>
    <div class="metric">
      <span class="metric-name">Baseline Growth (100 iterations)</span>
      <span class="metric-value">${results.memory.leaks.baselineGrowth} MB</span>
    </div>
  </div>

  <div class="section">
    <h2>3. State Machine Performance</h2>
    <div class="metric">
      <span class="metric-name">State Check Mean (10,000 checks)</span>
      <span class="metric-value">${results.stateMachine.stateChecks.mean}µs</span>
    </div>
    <div class="metric">
      <span class="metric-name">State Check Throughput</span>
      <span class="metric-value">${results.stateMachine.stateChecks.throughput} checks/sec</span>
    </div>
    <div class="metric">
      <span class="metric-name">Transition Mean (1,000 transitions)</span>
      <span class="metric-value">${results.stateMachine.transitions.meanPerTransition}µs per transition</span>
    </div>
    <div class="metric">
      <span class="metric-name">Transition Throughput</span>
      <span class="metric-value">${results.stateMachine.transitions.throughput} transitions/sec</span>
    </div>
    <div class="metric">
      <span class="metric-name">Performance Degradation</span>
      <span class="metric-value ${results.stateMachine.degradation.degradationDetected ? 'status-warn' : 'status-pass'}">
        ${results.stateMachine.degradation.degradation}% (${results.stateMachine.degradation.degradationDetected ? '⚠ Detected' : '✓ None'})
      </span>
    </div>
  </div>

  <div class="section">
    <h2>4. SLA Enforcement Overhead</h2>
    <div class="metric">
      <span class="metric-name">Without SLA Tracking</span>
      <span class="metric-value">${results.sla.overhead.withoutSLA.mean}ms avg</span>
    </div>
    <div class="metric">
      <span class="metric-name">With SLA Tracking</span>
      <span class="metric-value">${results.sla.overhead.withSLA.mean}ms avg</span>
    </div>
    <div class="metric">
      <span class="metric-name">Overhead Percentage</span>
      <span class="metric-value ${results.sla.overhead.overhead.acceptable ? 'status-pass' : 'status-warn'}">
        ${results.sla.overhead.overhead.percentage}% (${results.sla.overhead.overhead.acceptable ? '✓ Acceptable' : '⚠ High'})
      </span>
    </div>
    <div class="metric">
      <span class="metric-name">Validation Throughput</span>
      <span class="metric-value">${results.sla.validation.throughput} validations/sec</span>
    </div>
  </div>

  <div class="section">
    <h2>5. Circuit Breaker Performance</h2>
    <div class="metric">
      <span class="metric-name">Circuit Breaker Overhead</span>
      <span class="metric-value ${results.circuitBreaker.overhead.overhead.acceptable ? 'status-pass' : 'status-warn'}">
        ${results.circuitBreaker.overhead.overhead.percentage}% (${results.circuitBreaker.overhead.overhead.acceptable ? '✓ Acceptable' : '⚠ High'})
      </span>
    </div>
    <div class="metric">
      <span class="metric-name">State Transition Mean</span>
      <span class="metric-value">${results.circuitBreaker.stateTransitions.mean}ms per cycle</span>
    </div>
    <div class="metric">
      <span class="metric-name">Failure Detection Mean</span>
      <span class="metric-value">${results.circuitBreaker.failureDetection.mean}ms</span>
    </div>
    <div class="metric">
      <span class="metric-name">Timeout Accuracy</span>
      <span class="metric-value">${results.circuitBreaker.timeoutAccuracy.accuracy}%</span>
    </div>
  </div>

  <div class="section">
    <h2>6. Concurrency Testing</h2>
    <table>
      <thead>
        <tr>
          <th>Concurrent Ops</th>
          <th>Throughput (ops/sec)</th>
          <th>Avg Latency (ms)</th>
          <th>P95 (ms)</th>
          <th>P99 (ms)</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>10</td>
          <td>${results.concurrency.throughput_10.throughput}</td>
          <td>${results.concurrency.throughput_10.avgLatency}</td>
          <td>${results.concurrency.throughput_10.p95}</td>
          <td>${results.concurrency.throughput_10.p99}</td>
        </tr>
        <tr>
          <td>100</td>
          <td>${results.concurrency.throughput_100.throughput}</td>
          <td>${results.concurrency.throughput_100.avgLatency}</td>
          <td>${results.concurrency.throughput_100.p95}</td>
          <td>${results.concurrency.throughput_100.p99}</td>
        </tr>
        <tr>
          <td>1,000</td>
          <td>${results.concurrency.throughput_1000.throughput}</td>
          <td>${results.concurrency.throughput_1000.avgLatency}</td>
          <td>${results.concurrency.throughput_1000.p95}</td>
          <td>${results.concurrency.throughput_1000.p99}</td>
        </tr>
      </tbody>
    </table>
    <div class="metric">
      <span class="metric-name">Thread Contention Rate (100 ops)</span>
      <span class="metric-value">${results.concurrency.contention_100.contentionRate}%</span>
    </div>
    <div class="metric">
      <span class="metric-name">Bottleneck Detected (100 ops)</span>
      <span class="metric-value ${results.concurrency.bottlenecks_100.bottleneckDetected ? 'status-warn' : 'status-pass'}">
        ${results.concurrency.bottlenecks_100.bottleneckDetected ? '⚠ YES' : '✓ NO'}
      </span>
    </div>
    <div class="metric">
      <span class="metric-name">Deadlock Detected (100 ops)</span>
      <span class="metric-value ${results.concurrency.deadlocks_100.deadlockDetected ? 'status-fail' : 'status-pass'}">
        ${results.concurrency.deadlocks_100.deadlockDetected ? '✗ YES' : '✓ NO'}
      </span>
    </div>
  </div>

  <div class="section">
    <h2>7. Resource Usage</h2>
    <div class="metric">
      <span class="metric-name">CPU Utilization (1000 ops)</span>
      <span class="metric-value">${results.resources.cpu.utilization}%</span>
    </div>
    <div class="metric">
      <span class="metric-name">CPU Total Time</span>
      <span class="metric-value">${results.resources.cpu.cpu.total}ms</span>
    </div>
    <div class="metric">
      <span class="metric-name">Memory Growth (1000 ops)</span>
      <span class="metric-value">${results.resources.memory.growth} MB</span>
    </div>
    <div class="metric">
      <span class="metric-name">Memory Per Operation</span>
      <span class="metric-value">${results.resources.memory.memoryPerOp} KB</span>
    </div>
    <div class="metric">
      <span class="metric-name">Optimization Opportunities</span>
      <span class="metric-value">${results.resources.optimizations.length} found</span>
    </div>
  </div>

  <div class="section">
    <h2>Recommendations</h2>
    ${generateRecommendations(results)}
  </div>
</body>
</html>`;
}

/**
 * Generate recommendations based on results
 * @param {Object} results - Benchmark results
 * @returns {string} HTML recommendations
 */
function generateRecommendations(results) {
  const recommendations = [];

  // Check SLA compliance
  if (!results.latency.benchmark_1000.sla.met) {
    recommendations.push({
      severity: 'HIGH',
      category: 'Latency',
      issue: `SLA not met: ${results.latency.benchmark_1000.sla.complianceRate}% compliance (target: ≥99.9%)`,
      action: 'Optimize critical path operations to reduce roundtrip latency below 10ms',
    });
  }

  // Check memory leaks
  if (results.memory.leaks.leakDetected) {
    recommendations.push({
      severity: 'HIGH',
      category: 'Memory',
      issue: `Memory leak detected: ${results.memory.leaks.baselineGrowth} MB baseline growth`,
      action: 'Review object lifecycle and ensure proper cleanup of resources',
    });
  }

  // Check performance degradation
  if (results.stateMachine.degradation.degradationDetected) {
    recommendations.push({
      severity: 'MEDIUM',
      category: 'State Machine',
      issue: `Performance degradation: ${results.stateMachine.degradation.degradation}% over time`,
      action: 'Investigate state machine implementation for potential memory or complexity issues',
    });
  }

  // Check SLA overhead
  if (!results.sla.overhead.overhead.acceptable) {
    recommendations.push({
      severity: 'MEDIUM',
      category: 'SLA Tracking',
      issue: `High SLA tracking overhead: ${results.sla.overhead.overhead.percentage}% (threshold: <5%)`,
      action: 'Consider optimizing SLA tracking implementation or reducing tracking frequency',
    });
  }

  // Check concurrency issues
  if (results.concurrency.bottlenecks_100.bottleneckDetected) {
    recommendations.push({
      severity: 'MEDIUM',
      category: 'Concurrency',
      issue: 'Bottleneck detected in concurrent operations',
      action: 'Review resource contention and consider parallelizing bottleneck operations',
    });
  }

  if (results.concurrency.deadlocks_100.deadlockDetected) {
    recommendations.push({
      severity: 'HIGH',
      category: 'Concurrency',
      issue: `Deadlock detected: ${results.concurrency.deadlocks_100.timeouts} timeouts`,
      action: 'Review locking strategy and ensure proper timeout handling',
    });
  }

  // Add resource optimization recommendations
  results.resources.optimizations.forEach(opt => {
    recommendations.push({
      severity: opt.severity.toUpperCase(),
      category: opt.type,
      issue: opt.description,
      action: opt.recommendation,
    });
  });

  if (recommendations.length === 0) {
    return '<p class="status-pass">✓ No critical issues detected. All performance metrics are within acceptable ranges.</p>';
  }

  let html = '<table><thead><tr><th>Severity</th><th>Category</th><th>Issue</th><th>Recommended Action</th></tr></thead><tbody>';

  recommendations.sort((a, b) => {
    const severityOrder = { HIGH: 0, MEDIUM: 1, LOW: 2 };
    return severityOrder[a.severity] - severityOrder[b.severity];
  });

  recommendations.forEach(rec => {
    const severityClass = rec.severity === 'HIGH' ? 'status-fail' : rec.severity === 'MEDIUM' ? 'status-warn' : 'status-pass';
    html += `<tr>
      <td class="${severityClass}">${rec.severity}</td>
      <td>${rec.category}</td>
      <td>${rec.issue}</td>
      <td>${rec.action}</td>
    </tr>`;
  });

  html += '</tbody></table>';
  return html;
}

/**
 * Run all benchmarks and generate report
 */
async function runAllBenchmarks() {
  console.log('\n╔════════════════════════════════════════════════════════════════╗');
  console.log('║                                                                ║');
  console.log('║      ATOMVM COMPREHENSIVE PERFORMANCE ANALYSIS SUITE           ║');
  console.log('║                                                                ║');
  console.log('╚════════════════════════════════════════════════════════════════╝\n');

  const startTime = performance.now();
  const results = {};

  try {
    // Run all benchmark suites
    console.log('\n[1/7] Running Latency Benchmarks...');
    results.latency = await runComprehensiveLatencyBenchmarks();

    console.log('\n[2/7] Running Memory Profiling Benchmarks...');
    results.memory = await runComprehensiveMemoryBenchmarks();

    console.log('\n[3/7] Running State Machine Benchmarks...');
    results.stateMachine = await runComprehensiveStateMachineBenchmarks();

    console.log('\n[4/7] Running SLA Overhead Benchmarks...');
    results.sla = await runComprehensiveSLABenchmarks();

    console.log('\n[5/7] Running Circuit Breaker Benchmarks...');
    results.circuitBreaker = await runComprehensiveCircuitBreakerBenchmarks();

    console.log('\n[6/7] Running Concurrency Benchmarks...');
    results.concurrency = await runComprehensiveConcurrencyBenchmarks();

    console.log('\n[7/7] Running Resource Usage Benchmarks...');
    results.resources = await runComprehensiveResourceBenchmarks();

    const totalTime = (performance.now() - startTime) / 1000; // Convert to seconds

    // Generate reports
    console.log('\n\n╔════════════════════════════════════════════════════════════════╗');
    console.log('║                    GENERATING REPORTS                          ║');
    console.log('╚════════════════════════════════════════════════════════════════╝\n');

    // Generate HTML report
    const htmlReport = generateHTMLReport(results, totalTime);
    const htmlPath = join(__dirname, '../docs/performance-report.html');
    writeFileSync(htmlPath, htmlReport);
    console.log(`✓ HTML Report: ${htmlPath}`);

    // Generate JSON report
    const jsonReport = JSON.stringify(
      {
        timestamp: new Date().toISOString(),
        totalTime: totalTime.toFixed(2),
        results,
      },
      null,
      2
    );
    const jsonPath = join(__dirname, '../docs/performance-report.json');
    writeFileSync(jsonPath, jsonReport);
    console.log(`✓ JSON Report: ${jsonPath}`);

    console.log(`\n╔════════════════════════════════════════════════════════════════╗`);
    console.log(`║                   BENCHMARK COMPLETE                           ║`);
    console.log(`╚════════════════════════════════════════════════════════════════╝`);
    console.log(`\nTotal Execution Time: ${totalTime.toFixed(2)}s`);
    console.log(`\nView the detailed report: file://${htmlPath}\n`);

    return results;
  } catch (error) {
    console.error('\n❌ Benchmark failed:', error);
    throw error;
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllBenchmarks().catch(error => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
}

export { runAllBenchmarks };
