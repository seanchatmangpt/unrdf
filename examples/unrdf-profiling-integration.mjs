/**
 * @fileoverview UNRDF Performance Profiling Integration Example
 * Demonstrates how to integrate profiling into UNRDF operations
 */

import { createProfiler } from '../packages/core/src/profiling/profiler.mjs';
import { Reporter } from '../packages/core/src/profiling/reporter.mjs';

/**
 * Example UNRDF operation profiling wrapper
 */
class UnrdfProfiler {
  constructor() {
    this.profiler = createProfiler({
      enableLatency: true,
      enableMemory: true,
      enableCpu: false,
      enableOtel: true,
      labels: ['unrdf', 'v3.1.0'],
    });

    // Performance budgets for UNRDF operations
    this.budgets = {
      parse: { p95: 50, p99: 100 },
      query: { p95: 100, p99: 200 },
      'hook-execution': { p95: 80, p99: 150 },
      'transaction-commit': { p95: 120, p99: 250 },
      'sparql-query': { p95: 100, p99: 200 },
      'shacl-validation': { p95: 80, p99: 150 },
    };
  }

  /**
   * Profile a parse operation
   */
  async profileParse(input, options = {}) {
    return await this.profiler.profile('parse', async () => {
      // Simulate parse operation
      await this.simulateWork(30, 50);
      return {
        triples: Math.floor(Math.random() * 1000),
        format: options.format || 'turtle',
      };
    });
  }

  /**
   * Profile a query operation
   */
  async profileQuery(query, options = {}) {
    return await this.profiler.profile('query', async () => {
      // Simulate query operation
      await this.simulateWork(60, 100);
      return {
        results: Math.floor(Math.random() * 500),
        queryType: options.type || 'SELECT',
      };
    });
  }

  /**
   * Profile hook execution
   */
  async profileHook(hookName, _context) {
    return await this.profiler.profile('hook-execution', async () => {
      // Simulate hook execution
      await this.simulateWork(40, 80);
      return {
        hookName,
        executed: true,
        changes: Math.floor(Math.random() * 10),
      };
    });
  }

  /**
   * Profile transaction commit
   */
  async profileTransactionCommit(transaction) {
    return await this.profiler.profile('transaction-commit', async () => {
      // Simulate transaction commit
      await this.simulateWork(70, 120);
      return {
        committed: true,
        operations: transaction.operations || 0,
      };
    });
  }

  /**
   * Profile SPARQL query
   */
  async profileSparqlQuery(_sparql) {
    return await this.profiler.profile('sparql-query', async () => {
      // Simulate SPARQL query
      await this.simulateWork(50, 100);
      return {
        bindings: Math.floor(Math.random() * 200),
      };
    });
  }

  /**
   * Profile SHACL validation
   */
  async profileShaclValidation(_shapes, _data) {
    return await this.profiler.profile('shacl-validation', async () => {
      // Simulate SHACL validation
      await this.simulateWork(40, 80);
      return {
        conforms: Math.random() > 0.1,
        violations: Math.floor(Math.random() * 5),
      };
    });
  }

  /**
   * Check all operations against performance budgets
   */
  checkBudgets() {
    const results = [];

    for (const [operation, budget] of Object.entries(this.budgets)) {
      const stats = this.profiler.getStats(operation);

      if (stats && stats.latency) {
        const check = this.profiler.latencyProfiler.checkBudget(stats.latency, budget);

        results.push({
          operation,
          passed: check.passed,
          p95: stats.latency.p95,
          budget: budget.p95,
          violations: check.violations,
        });
      }
    }

    return results;
  }

  /**
   * Generate performance report
   */
  generateReport() {
    const operations = Object.keys(this.budgets);
    const report = {
      timestamp: new Date().toISOString(),
      operations: {},
    };

    for (const operation of operations) {
      const stats = this.profiler.getStats(operation);
      if (stats) {
        report.operations[operation] = {
          count: stats.count,
          latency: stats.latency,
          memory: stats.memory,
        };
      }
    }

    return report;
  }

  /**
   * Simulate work
   */
  async simulateWork(minMs, maxMs) {
    const delay = Math.random() * (maxMs - minMs) + minMs;
    await new Promise(resolve => setTimeout(resolve, delay));
  }
}

/**
 * Example: Profile UNRDF operations
 */
async function exampleUnrdfProfiling() {
  console.log('\n╔═══════════════════════════════════════════════════════════╗');
  console.log('║     UNRDF Performance Profiling Integration Example      ║');
  console.log('╚═══════════════════════════════════════════════════════════╝\n');

  const profiler = new UnrdfProfiler();

  console.log('Running profiled UNRDF operations...\n');

  // Profile parse operations
  console.log('1. Parse Operations:');
  for (let i = 0; i < 10; i++) {
    const { _result, _profile } = await profiler.profileParse('<http://example.org/data>', {
      format: 'turtle',
    });
    process.stdout.write('.');
  }
  console.log(' ✓\n');

  // Profile query operations
  console.log('2. Query Operations:');
  for (let i = 0; i < 10; i++) {
    const { _result, _profile } = await profiler.profileQuery('SELECT * WHERE { ?s ?p ?o }', {
      type: 'SELECT',
    });
    process.stdout.write('.');
  }
  console.log(' ✓\n');

  // Profile hook executions
  console.log('3. Hook Executions:');
  for (let i = 0; i < 10; i++) {
    const { _result, _profile } = await profiler.profileHook('validation-hook', {
      data: 'test',
    });
    process.stdout.write('.');
  }
  console.log(' ✓\n');

  // Profile transaction commits
  console.log('4. Transaction Commits:');
  for (let i = 0; i < 10; i++) {
    const { _result, _profile } = await profiler.profileTransactionCommit({
      operations: Math.floor(Math.random() * 10),
    });
    process.stdout.write('.');
  }
  console.log(' ✓\n');

  // Profile SPARQL queries
  console.log('5. SPARQL Queries:');
  for (let i = 0; i < 10; i++) {
    const { _result, _profile } = await profiler.profileSparqlQuery(
      'SELECT ?s WHERE { ?s a <http://example.org/Person> }'
    );
    process.stdout.write('.');
  }
  console.log(' ✓\n');

  // Profile SHACL validations
  console.log('6. SHACL Validations:');
  for (let i = 0; i < 10; i++) {
    const { _result, _profile } = await profiler.profileShaclValidation(['shape1', 'shape2'], {
      data: 'test',
    });
    process.stdout.write('.');
  }
  console.log(' ✓\n');

  console.log('═'.repeat(60));
  console.log('Performance Budget Check:\n');

  const budgetChecks = profiler.checkBudgets();
  let allPassed = true;

  for (const check of budgetChecks) {
    const status = check.passed ? '✓' : '✗';
    const symbol = check.passed ? '' : '⚠️ ';

    console.log(
      `${symbol}${status} ${check.operation.padEnd(25)} p95: ${check.p95.toFixed(2).padStart(7)}ms (budget: ${check.budget}ms)`
    );

    if (!check.passed) {
      allPassed = false;
      check.violations.forEach(v => {
        console.log(`   └─ ${v.metric}: exceeded by ${v.exceeded.toFixed(2)}ms`);
      });
    }
  }

  console.log('\n' + '═'.repeat(60));

  if (allPassed) {
    console.log('✓ All operations meet performance budgets!\n');
  } else {
    console.log('⚠️  Some operations exceeded performance budgets\n');
  }

  // Generate and display full report
  console.log('Generating detailed performance report...\n');

  const report = profiler.generateReport();

  console.log('Performance Summary:');
  console.log('  Timestamp:', report.timestamp);
  console.log('  Operations profiled:', Object.keys(report.operations).length);
  console.log('\nDetailed Statistics:\n');

  for (const [operation, stats] of Object.entries(report.operations)) {
    console.log(`  ${operation}:`);
    console.log(`    Samples:      ${stats.count}`);
    if (stats.latency) {
      console.log(`    Mean:         ${stats.latency.mean.toFixed(2)} ms`);
      console.log(`    p50:          ${stats.latency.p50.toFixed(2)} ms`);
      console.log(`    p95:          ${stats.latency.p95.toFixed(2)} ms`);
      console.log(`    p99:          ${stats.latency.p99.toFixed(2)} ms`);
    }
    if (stats.memory) {
      console.log(`    Memory (avg): ${formatBytes(stats.memory.mean)}`);
    }
    console.log('');
  }

  // Save report as JSON
  try {
    Reporter.save(report, 'unrdf-performance-report.json', {
      format: 'json',
      dir: '.',
    });
    console.log('✓ Performance report saved: unrdf-performance-report.json\n');
  } catch (err) {
    console.log('⚠️  Could not save report:', err.message, '\n');
  }
}

/**
 * Example: Continuous performance monitoring
 */
async function exampleContinuousMonitoring() {
  console.log('\n╔═══════════════════════════════════════════════════════════╗');
  console.log('║          Continuous Performance Monitoring Example        ║');
  console.log('╚═══════════════════════════════════════════════════════════╝\n');

  const profiler = new UnrdfProfiler();
  let totalOperations = 0;

  console.log('Monitoring performance (10 iterations)...\n');

  for (let iteration = 1; iteration <= 10; iteration++) {
    // Run mixed workload
    await profiler.profileParse('data');
    await profiler.profileQuery('query');
    await profiler.profileHook('hook', {});

    totalOperations += 3;

    // Check budgets every iteration
    const budgetChecks = profiler.checkBudgets();
    const violations = budgetChecks.filter(c => !c.passed).length;

    console.log(
      `  Iteration ${iteration}/10: ${totalOperations} operations, ${violations} budget violations`
    );
  }

  console.log('\nFinal Performance Summary:');

  const budgetChecks = profiler.checkBudgets();
  for (const check of budgetChecks) {
    if (check.p95) {
      const status = check.passed ? '✓' : '✗';
      console.log(`  ${status} ${check.operation}: p95=${check.p95.toFixed(2)}ms`);
    }
  }

  console.log('');
}

/**
 * Example: Regression detection
 */
async function exampleRegressionDetection() {
  console.log('\n╔═══════════════════════════════════════════════════════════╗');
  console.log('║            Performance Regression Detection Example       ║');
  console.log('╚═══════════════════════════════════════════════════════════╝\n');

  const baselineProfiler = new UnrdfProfiler();
  const currentProfiler = new UnrdfProfiler();

  console.log('Establishing baseline performance...');

  // Run baseline
  for (let i = 0; i < 5; i++) {
    await baselineProfiler.profileParse('data');
    await baselineProfiler.profileQuery('query');
  }

  const baselineStats = baselineProfiler.profiler.getStats('parse');
  console.log(`  Baseline parse p95: ${baselineStats.latency.p95.toFixed(2)}ms\n`);

  console.log('Simulating performance regression...');

  // Simulate regression by adding extra delay
  currentProfiler.simulateWork = async function (minMs, maxMs) {
    const delay = Math.random() * (maxMs - minMs) + minMs;
    await new Promise(resolve => setTimeout(resolve, delay * 1.3)); // 30% slower
  };

  for (let i = 0; i < 5; i++) {
    await currentProfiler.profileParse('data');
    await currentProfiler.profileQuery('query');
  }

  const currentStats = currentProfiler.profiler.getStats('parse');
  console.log(`  Current parse p95: ${currentStats.latency.p95.toFixed(2)}ms\n`);

  // Detect regression
  const change =
    ((currentStats.latency.p95 - baselineStats.latency.p95) / baselineStats.latency.p95) * 100;

  if (change > 10) {
    console.log(`⚠️  REGRESSION DETECTED!`);
    console.log(`  Parse performance degraded by ${change.toFixed(2)}%`);
    console.log(`  Action: Investigate and fix before deployment\n`);
  } else if (change < -10) {
    console.log(`✓ PERFORMANCE IMPROVEMENT!`);
    console.log(`  Parse performance improved by ${Math.abs(change).toFixed(2)}%\n`);
  } else {
    console.log(`✓ Performance stable (change: ${change.toFixed(2)}%)\n`);
  }
}

// Helper
function formatBytes(bytes) {
  if (!bytes) return '0 B';
  if (Math.abs(bytes) < 1024) return bytes + ' B';
  const units = ['KB', 'MB', 'GB'];
  let u = -1;
  do {
    bytes /= 1024;
    ++u;
  } while (Math.abs(bytes) >= 1024 && u < units.length - 1);
  return bytes.toFixed(2) + ' ' + units[u];
}

// Run examples
async function main() {
  try {
    await exampleUnrdfProfiling();
    await exampleContinuousMonitoring();
    await exampleRegressionDetection();

    console.log('╔═══════════════════════════════════════════════════════════╗');
    console.log('║              All Integration Examples Complete            ║');
    console.log('╚═══════════════════════════════════════════════════════════╝\n');
  } catch (error) {
    console.error('Error:', error);
    process.exit(1);
  }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}
