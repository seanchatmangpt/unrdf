#!/usr/bin/env node
/**
 * @file Load test results analyzer and report generator
 * @module test/load-testing/analyze-results
 * @description Analyzes load test results and generates comprehensive reports
 */

import { readFile, writeFile, readdir } from 'node:fs/promises';
import { join } from 'node:path';

/**
 * Analyze load test results
 * @param {Object} results - Test results
 * @returns {Object} Analysis
 */
function analyzeResults(results) {
  const analysis = {
    timestamp: new Date().toISOString(),
    overallStatus: 'PASS',
    summary: {},
    bottlenecks: [],
    recommendations: [],
    performanceMetrics: {}
  };

  // Analyze each test
  for (const [testName, result] of Object.entries(results.tests)) {
    if (result.skipped) continue;

    const testAnalysis = analyzeTest(testName, result);
    analysis.summary[testName] = testAnalysis;

    // Collect bottlenecks
    if (testAnalysis.bottlenecks.length > 0) {
      analysis.bottlenecks.push(...testAnalysis.bottlenecks);
    }

    // Update overall status
    if (testAnalysis.status === 'FAIL') {
      analysis.overallStatus = 'FAIL';
    }
  }

  // Generate recommendations
  analysis.recommendations = generateRecommendations(analysis);

  // Extract key performance metrics
  analysis.performanceMetrics = extractPerformanceMetrics(results);

  return analysis;
}

/**
 * Analyze a single test
 * @param {string} testName - Test name
 * @param {Object} result - Test result
 * @returns {Object} Test analysis
 */
function analyzeTest(testName, result) {
  const analysis = {
    testName,
    status: 'PASS',
    bottlenecks: [],
    metrics: {}
  };

  // Check pass/fail criteria
  if (result.passed) {
    const failed = Object.entries(result.passed)
      .filter(([key, value]) => !value)
      .map(([key]) => key);

    if (failed.length > 0) {
      analysis.status = 'FAIL';
      analysis.failedCriteria = failed;
    }
  }

  // Extract metrics (handle different test structures)
  if (result.latency) {
    analysis.metrics.latency = {
      p50: result.latency.p50,
      p95: result.latency.p95 || result.latency.p90, // Fallback to p90 if p95 missing
      p99: result.latency.p99,
      mean: result.latency.mean
    };

    // Check for latency bottlenecks
    if (result.latency.p99 > 1000) {
      analysis.bottlenecks.push({
        type: 'HIGH_LATENCY_P99',
        severity: 'HIGH',
        value: result.latency.p99,
        threshold: 1000,
        description: `P99 latency (${result.latency.p99.toFixed(2)}ms) exceeds 1000ms`
      });
    }

    if (result.latency.p95 > 500) {
      analysis.bottlenecks.push({
        type: 'HIGH_LATENCY_P95',
        severity: 'MEDIUM',
        value: result.latency.p95,
        threshold: 500,
        description: `P95 latency (${result.latency.p95.toFixed(2)}ms) exceeds 500ms`
      });
    }
  }

  if (result.throughput) {
    analysis.metrics.throughput = {
      requestsPerSecond: result.throughput.requestsPerSecond,
      totalRequests: result.throughput.totalRequests
    };

    // Check for low throughput
    if (result.throughput.requestsPerSecond < 50 && testName !== 'sustained') {
      analysis.bottlenecks.push({
        type: 'LOW_THROUGHPUT',
        severity: 'MEDIUM',
        value: result.throughput.requestsPerSecond,
        threshold: 50,
        description: `Throughput (${result.throughput.requestsPerSecond.toFixed(2)} req/s) below expected`
      });
    }
  }

  if (result.errors) {
    analysis.metrics.errors = {
      total: result.errors.total,
      errorRate: result.errors.errorRate
    };

    // Check for high error rate
    if (result.errors.errorRate > 0.1) {
      analysis.bottlenecks.push({
        type: 'HIGH_ERROR_RATE',
        severity: 'CRITICAL',
        value: result.errors.errorRate,
        threshold: 0.1,
        description: `Error rate (${result.errors.errorRate.toFixed(4)}%) exceeds 0.1%`
      });
    }
  }

  // Memory leak detection (for sustained/soak tests)
  if (result.memory || (result.resources && result.resources.memory)) {
    const memory = result.memory || result.resources.memory;
    if (memory.leakDetected) {
      analysis.bottlenecks.push({
        type: 'MEMORY_LEAK',
        severity: 'CRITICAL',
        value: memory.growthRateMB,
        threshold: 1,
        description: `Possible memory leak detected (${memory.growthRateMB?.toFixed(2) || 'N/A'} MB/min growth)`
      });
    }
  }

  // CPU usage (for soak tests)
  if (result.resources && result.resources.cpu) {
    const cpu = result.resources.cpu;
    if (cpu.average > 80) {
      analysis.bottlenecks.push({
        type: 'HIGH_CPU_USAGE',
        severity: 'HIGH',
        value: cpu.average,
        threshold: 80,
        description: `High average CPU usage (${cpu.average.toFixed(2)}%)`
      });
    }
  }

  return analysis;
}

/**
 * Generate recommendations based on analysis
 * @param {Object} analysis - Test analysis
 * @returns {Array<Object>} Recommendations
 */
function generateRecommendations(analysis) {
  const recommendations = [];

  // Group bottlenecks by type
  const bottlenecksByType = {};
  for (const bottleneck of analysis.bottlenecks) {
    if (!bottlenecksByType[bottleneck.type]) {
      bottlenecksByType[bottleneck.type] = [];
    }
    bottlenecksByType[bottleneck.type].push(bottleneck);
  }

  // Generate recommendations for each bottleneck type
  for (const [type, bottlenecks] of Object.entries(bottlenecksByType)) {
    switch (type) {
      case 'HIGH_LATENCY_P99':
      case 'HIGH_LATENCY_P95':
        recommendations.push({
          priority: 'HIGH',
          category: 'PERFORMANCE',
          title: 'Optimize Request Processing',
          description: 'High tail latencies detected. Consider implementing request queuing, connection pooling, or caching strategies.',
          impact: 'Will improve P95/P99 latencies by 30-50%',
          effort: 'MEDIUM',
          actions: [
            'Enable request caching for frequently accessed data',
            'Implement connection pooling for database/RDF store',
            'Add request prioritization/throttling',
            'Profile slow endpoints to identify bottlenecks'
          ]
        });
        break;

      case 'LOW_THROUGHPUT':
        recommendations.push({
          priority: 'MEDIUM',
          category: 'SCALABILITY',
          title: 'Increase Throughput Capacity',
          description: 'System throughput is below targets. Consider horizontal scaling or optimizing request handlers.',
          impact: 'Will increase throughput by 2-3x',
          effort: 'MEDIUM',
          actions: [
            'Enable clustering/multi-process mode',
            'Optimize async request handling',
            'Review and optimize database queries',
            'Consider using a load balancer'
          ]
        });
        break;

      case 'HIGH_ERROR_RATE':
        recommendations.push({
          priority: 'CRITICAL',
          category: 'RELIABILITY',
          title: 'Reduce Error Rate',
          description: 'Unacceptable error rate detected. This must be addressed before production deployment.',
          impact: 'Critical for production readiness',
          effort: 'HIGH',
          actions: [
            'Review error logs to identify root causes',
            'Implement circuit breakers for failing dependencies',
            'Add request validation and error handling',
            'Increase resource limits (memory, file descriptors)',
            'Add health checks and graceful degradation'
          ]
        });
        break;

      case 'MEMORY_LEAK':
        recommendations.push({
          priority: 'CRITICAL',
          category: 'RELIABILITY',
          title: 'Fix Memory Leak',
          description: 'Memory leak detected in sustained testing. This will cause crashes in production.',
          impact: 'Critical - prevents long-term stability',
          effort: 'HIGH',
          actions: [
            'Profile memory usage with heap snapshots',
            'Review event listeners and ensure proper cleanup',
            'Check for circular references in caches',
            'Implement memory usage monitoring and alerts',
            'Review third-party dependencies for leaks'
          ]
        });
        break;

      case 'HIGH_CPU_USAGE':
        recommendations.push({
          priority: 'HIGH',
          category: 'PERFORMANCE',
          title: 'Optimize CPU Usage',
          description: 'High CPU usage detected. May limit scalability and increase costs.',
          impact: 'Will reduce CPU usage by 20-40%',
          effort: 'MEDIUM',
          actions: [
            'Profile CPU-intensive operations',
            'Optimize algorithms and data structures',
            'Consider offloading work to worker threads',
            'Review and optimize SPARQL queries',
            'Enable CPU-based auto-scaling'
          ]
        });
        break;
    }
  }

  // Add general recommendations
  if (recommendations.length === 0) {
    recommendations.push({
      priority: 'LOW',
      category: 'OPTIMIZATION',
      title: 'Continue Performance Monitoring',
      description: 'All tests passed! Continue monitoring production metrics to ensure sustained performance.',
      impact: 'Proactive performance management',
      effort: 'LOW',
      actions: [
        'Set up production performance monitoring',
        'Configure alerts for latency/error thresholds',
        'Run load tests regularly (weekly/monthly)',
        'Track performance trends over time'
      ]
    });
  }

  return recommendations.sort((a, b) => {
    const priority = { CRITICAL: 0, HIGH: 1, MEDIUM: 2, LOW: 3 };
    return priority[a.priority] - priority[b.priority];
  });
}

/**
 * Extract key performance metrics
 * @param {Object} results - Test results
 * @returns {Object} Performance metrics
 */
function extractPerformanceMetrics(results) {
  const metrics = {
    latency: {},
    throughput: {},
    reliability: {},
    resources: {}
  };

  // Extract from each test
  for (const [testName, result] of Object.entries(results.tests)) {
    if (result.skipped) continue;

    if (result.latency) {
      metrics.latency[testName] = {
        p50: result.latency.p50,
        p95: result.latency.p95,
        p99: result.latency.p99
      };
    }

    if (result.throughput) {
      metrics.throughput[testName] = {
        requestsPerSecond: result.throughput.requestsPerSecond,
        totalRequests: result.throughput.totalRequests
      };
    }

    if (result.errors) {
      metrics.reliability[testName] = {
        errorRate: result.errors.errorRate,
        totalErrors: result.errors.total
      };
    }
  }

  return metrics;
}

/**
 * Generate markdown report
 * @param {Object} results - Test results
 * @param {Object} analysis - Analysis
 * @returns {string} Markdown report
 */
function generateMarkdownReport(results, analysis) {
  let md = '# Load Testing Performance Report\n\n';
  md += `**Generated:** ${new Date().toISOString()}\n\n`;
  md += `**Overall Status:** ${analysis.overallStatus === 'PASS' ? '✅ PASS' : '❌ FAIL'}\n\n`;

  // Executive Summary
  md += '## Executive Summary\n\n';
  const totalTests = Object.keys(results.tests).filter(k => !results.tests[k].skipped).length;
  const passedTests = Object.values(analysis.summary).filter(t => t.status === 'PASS').length;
  md += `- **Total Tests:** ${totalTests}\n`;
  md += `- **Passed:** ${passedTests}\n`;
  md += `- **Failed:** ${totalTests - passedTests}\n`;
  md += `- **Bottlenecks Identified:** ${analysis.bottlenecks.length}\n`;
  md += `- **Recommendations:** ${analysis.recommendations.length}\n\n`;

  // Test Results
  md += '## Test Results\n\n';

  for (const [testName, summary] of Object.entries(analysis.summary)) {
    const status = summary.status === 'PASS' ? '✅' : '❌';
    md += `### ${status} ${testName.toUpperCase()}\n\n`;

    if (summary.metrics.latency) {
      md += '**Latency Metrics:**\n\n';
      md += '| Metric | Value |\n';
      md += '|--------|-------|\n';
      if (summary.metrics.latency.p50 !== undefined) {
        md += `| P50 | ${summary.metrics.latency.p50.toFixed(2)} ms |\n`;
      }
      if (summary.metrics.latency.p95 !== undefined) {
        md += `| P95 | ${summary.metrics.latency.p95.toFixed(2)} ms |\n`;
      }
      if (summary.metrics.latency.p99 !== undefined) {
        md += `| P99 | ${summary.metrics.latency.p99.toFixed(2)} ms |\n`;
      }
      if (summary.metrics.latency.mean !== undefined) {
        md += `| Mean | ${summary.metrics.latency.mean.toFixed(2)} ms |\n`;
      }
      md += '\n';
    }

    if (summary.metrics.throughput) {
      md += '**Throughput Metrics:**\n\n';
      md += '| Metric | Value |\n';
      md += '|--------|-------|\n';
      if (summary.metrics.throughput.requestsPerSecond !== undefined) {
        md += `| Requests/sec | ${summary.metrics.throughput.requestsPerSecond.toFixed(2)} |\n`;
      }
      if (summary.metrics.throughput.totalRequests !== undefined) {
        md += `| Total Requests | ${summary.metrics.throughput.totalRequests} |\n`;
      }
      md += '\n';
    }

    if (summary.metrics.errors) {
      md += '**Error Metrics:**\n\n';
      md += '| Metric | Value |\n';
      md += '|--------|-------|\n';
      if (summary.metrics.errors.errorRate !== undefined) {
        md += `| Error Rate | ${summary.metrics.errors.errorRate.toFixed(4)}% |\n`;
      }
      if (summary.metrics.errors.total !== undefined) {
        md += `| Total Errors | ${summary.metrics.errors.total} |\n`;
      }
      md += '\n';
    }

    if (summary.bottlenecks.length > 0) {
      md += '**Bottlenecks:**\n\n';
      for (const bottleneck of summary.bottlenecks) {
        md += `- **${bottleneck.type}** (${bottleneck.severity}): ${bottleneck.description}\n`;
      }
      md += '\n';
    }
  }

  // Bottlenecks
  if (analysis.bottlenecks.length > 0) {
    md += '## Identified Bottlenecks\n\n';
    md += '| Type | Severity | Description | Value | Threshold |\n';
    md += '|------|----------|-------------|-------|----------|\n';

    for (const bottleneck of analysis.bottlenecks) {
      md += `| ${bottleneck.type} | ${bottleneck.severity} | ${bottleneck.description} | ${bottleneck.value} | ${bottleneck.threshold} |\n`;
    }
    md += '\n';
  }

  // Recommendations
  md += '## Recommendations\n\n';

  for (const rec of analysis.recommendations) {
    const priorityEmoji = {
      CRITICAL: '🔴',
      HIGH: '🟠',
      MEDIUM: '🟡',
      LOW: '🟢'
    };

    md += `### ${priorityEmoji[rec.priority]} ${rec.title}\n\n`;
    md += `**Priority:** ${rec.priority} | **Category:** ${rec.category} | **Effort:** ${rec.effort}\n\n`;
    md += `${rec.description}\n\n`;
    md += `**Expected Impact:** ${rec.impact}\n\n`;
    md += '**Actions:**\n\n';
    for (const action of rec.actions) {
      md += `- ${action}\n`;
    }
    md += '\n';
  }

  // Performance Targets
  md += '## Performance Targets vs Actual\n\n';
  md += '| Metric | Target | Actual | Status |\n';
  md += '|--------|--------|--------|--------|\n';

  // Baseline test targets
  if (analysis.summary.baseline) {
    const baseline = analysis.summary.baseline;
    if (baseline.metrics.latency?.p95 !== undefined) {
      md += `| Baseline P95 Latency | <100ms | ${baseline.metrics.latency.p95.toFixed(2)}ms | ${baseline.metrics.latency.p95 < 100 ? '✅' : '❌'} |\n`;
    }
    if (baseline.metrics.latency?.p99 !== undefined) {
      md += `| Baseline P99 Latency | <500ms | ${baseline.metrics.latency.p99.toFixed(2)}ms | ${baseline.metrics.latency.p99 < 500 ? '✅' : '❌'} |\n`;
    }
    if (baseline.metrics.errors?.errorRate !== undefined) {
      md += `| Baseline Error Rate | <0.1% | ${baseline.metrics.errors.errorRate.toFixed(4)}% | ${baseline.metrics.errors.errorRate < 0.1 ? '✅' : '❌'} |\n`;
    }
  }

  md += '\n';

  // Production Readiness
  md += '## Production Readiness Assessment\n\n';

  const criticalIssues = analysis.bottlenecks.filter(b => b.severity === 'CRITICAL');
  const highIssues = analysis.bottlenecks.filter(b => b.severity === 'HIGH');

  if (criticalIssues.length === 0 && highIssues.length === 0 && analysis.overallStatus === 'PASS') {
    md += '✅ **READY FOR PRODUCTION**\n\n';
    md += 'All load tests passed and no critical issues detected. System meets performance targets.\n\n';
  } else if (criticalIssues.length > 0) {
    md += '❌ **NOT READY FOR PRODUCTION**\n\n';
    md += `Critical issues must be resolved: ${criticalIssues.length} critical, ${highIssues.length} high priority.\n\n`;
  } else {
    md += '⚠️ **CONDITIONAL READINESS**\n\n';
    md += `${highIssues.length} high-priority issues should be addressed before production deployment.\n\n`;
  }

  return md;
}

/**
 * Main function
 */
async function main() {
  const args = process.argv.slice(2);
  const resultsDir = join(process.cwd(), 'test/load-testing/results');

  let resultsFile;

  if (args[0]) {
    resultsFile = args[0];
  } else {
    // Find most recent results file
    const files = await readdir(resultsDir);
    const resultFiles = files.filter(f => f.startsWith('load-test-results-') && f.endsWith('.json'));

    if (resultFiles.length === 0) {
      console.error('No results files found. Run tests first.');
      process.exit(1);
    }

    resultFiles.sort().reverse();
    resultsFile = join(resultsDir, resultFiles[0]);
  }

  console.log(`Analyzing results from: ${resultsFile}`);

  // Read results
  const resultsData = await readFile(resultsFile, 'utf-8');
  const results = JSON.parse(resultsData);

  // Analyze
  const analysis = analyzeResults(results);

  // Generate reports
  const markdownReport = generateMarkdownReport(results, analysis);

  // Save analysis
  const analysisFile = resultsFile.replace('.json', '-analysis.json');
  await writeFile(analysisFile, JSON.stringify(analysis, null, 2));
  console.log(`✓ Analysis saved to: ${analysisFile}`);

  // Save markdown report
  const reportFile = resultsFile.replace('.json', '-report.md');
  await writeFile(reportFile, markdownReport);
  console.log(`✓ Report saved to: ${reportFile}`);

  // Print summary
  console.log('\n' + '='.repeat(60));
  console.log('ANALYSIS SUMMARY');
  console.log('='.repeat(60));
  console.log(`Overall Status: ${analysis.overallStatus}`);
  console.log(`Bottlenecks: ${analysis.bottlenecks.length}`);
  console.log(`Recommendations: ${analysis.recommendations.length}`);
  console.log('='.repeat(60));

  process.exit(analysis.overallStatus === 'PASS' ? 0 : 1);
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch((err) => {
    console.error('Fatal error:', err);
    process.exit(1);
  });
}

export { analyzeResults, generateMarkdownReport };
