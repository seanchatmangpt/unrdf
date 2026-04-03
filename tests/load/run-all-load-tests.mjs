/**
 * Load Test Suite Runner
 *
 * Orchestrates all load tests and generates comprehensive report
 */

import { spawn } from 'child_process';
import fs from 'fs';
import path from 'path';

/**
 * Run command with timeout
 */
function runCommand(command, args, timeoutSec) {
  return new Promise((resolve, reject) => {
    console.log(`\n🚀 Running: ${command} ${args.join(' ')}`);
    console.log(`   Timeout: ${timeoutSec}s\n`);

    const startTime = Date.now();
    const child = spawn(command, args, {
      stdio: 'inherit',
      shell: true,
    });

    const timeout = setTimeout(() => {
      child.kill('SIGTERM');
      reject(new Error(`Command timed out after ${timeoutSec}s`));
    }, timeoutSec * 1000);

    child.on('exit', (code) => {
      clearTimeout(timeout);
      const duration = ((Date.now() - startTime) / 1000).toFixed(1);

      if (code === 0) {
        console.log(`\n✅ Completed in ${duration}s\n`);
        resolve({ code, duration });
      } else {
        console.log(`\n❌ Failed with code ${code} after ${duration}s\n`);
        reject(new Error(`Command failed with code ${code}`));
      }
    });

    child.on('error', (error) => {
      clearTimeout(timeout);
      reject(error);
    });
  });
}

/**
 * Find latest report file matching pattern
 */
function findLatestReport(pattern) {
  const files = fs.readdirSync('tests/load')
    .filter(f => f.match(pattern))
    .map(f => ({
      name: f,
      path: path.join('tests/load', f),
      mtime: fs.statSync(path.join('tests/load', f)).mtime,
    }))
    .sort((a, b) => b.mtime - a.mtime);

  return files.length > 0 ? files[0].path : null;
}

/**
 * Generate comprehensive summary
 */
function generateSummary(results) {
  console.log('\n' + '='.repeat(60));
  console.log('📊 LOAD TEST SUITE SUMMARY');
  console.log('='.repeat(60));

  const summary = {
    timestamp: new Date().toISOString(),
    tests: results,
    verdict: {
      baselineBenchmark: results.baseline.success ? '✅ PASS' : '❌ FAIL',
      sustainedLoad: results.sustained.success ? '✅ PASS' : '❌ FAIL',
      memoryProfile: results.memory.success ? '✅ PASS' : '❌ FAIL',
    },
  };

  // Parse reports
  if (results.baseline.reportPath) {
    const baseline = JSON.parse(fs.readFileSync(results.baseline.reportPath, 'utf8'));
    summary.baselineMetrics = {
      throughput: baseline.throughput,
      queryLatency: baseline.benchmarks['10000_quads']?.queries,
    };

    console.log('\n🎯 Baseline Performance:');
    console.log(`   Throughput: ${baseline.throughput.opsPerSec.toFixed(0)} ops/sec`);
    if (baseline.benchmarks['10000_quads']?.queries) {
      const q = baseline.benchmarks['10000_quads'].queries;
      console.log(`   Query p50: ${q.p50.toFixed(3)}ms, p99: ${q.p99.toFixed(3)}ms`);
    }
  }

  if (results.sustained.reportPath) {
    const sustained = JSON.parse(fs.readFileSync(results.sustained.reportPath, 'utf8'));
    summary.sustainedMetrics = {
      operations: sustained.summary.totalOperations,
      memoryGrowth: sustained.memory.growthPercent,
      qualityGates: sustained.qualityGates,
    };

    console.log('\n⏱️ Sustained Load (24h simulation):');
    console.log(`   Operations: ${sustained.summary.totalOperations}`);
    console.log(`   Memory Growth: ${sustained.memory.growthPercent}% ${sustained.qualityGates.memoryGrowth}`);
    console.log(`   Latency: ${sustained.qualityGates.latencyStability}`);
  }

  if (results.memory.reportPath) {
    const memory = JSON.parse(fs.readFileSync(results.memory.reportPath, 'utf8'));
    summary.memoryMetrics = {
      growth: memory.analysis.growth,
      leaks: memory.analysis.leaks,
    };

    console.log('\n💾 Memory Profile:');
    if (memory.analysis.growth) {
      console.log(`   Growth: ${memory.analysis.growth.heapGrowthMB} MB (${memory.analysis.growth.growthPercent}%)`);
      console.log(`   Rate: ${memory.analysis.growth.growthRateKBPerSec} KB/sec`);
    }
    console.log(`   Leaks: ${memory.analysis.leaks.length === 0 ? '✅ None' : `⚠️ ${memory.analysis.leaks.length} detected`}`);
  }

  // Overall verdict
  const allPassed = Object.values(summary.verdict).every(v => v.includes('✅'));

  console.log('\n' + '='.repeat(60));
  console.log('FINAL VERDICT:');
  Object.entries(summary.verdict).forEach(([test, result]) => {
    console.log(`  ${test}: ${result}`);
  });
  console.log('='.repeat(60));

  if (allPassed) {
    console.log('\n🎉 ALL LOAD TESTS PASSED\n');
  } else {
    console.log('\n❌ SOME LOAD TESTS FAILED\n');
  }

  // Save summary
  const summaryPath = `tests/load/load-test-summary-${Date.now()}.json`;
  fs.writeFileSync(summaryPath, JSON.stringify(summary, null, 2));
  console.log(`📄 Summary saved: ${summaryPath}\n`);

  return { allPassed, summary };
}

/**
 * Main runner
 */
async function runAllTests() {
  console.log('🧪 UNRDF Load Test Suite');
  console.log('Running comprehensive load testing...\n');

  const results = {
    baseline: { success: false, reportPath: null },
    sustained: { success: false, reportPath: null },
    memory: { success: false, reportPath: null },
  };

  try {
    // 1. Baseline benchmark (5 minutes)
    console.log('=' .repeat(60));
    console.log('1️⃣ BASELINE PERFORMANCE BENCHMARK');
    console.log('='.repeat(60));

    await runCommand('node', ['tests/load/baseline-benchmark.mjs'], 300);
    results.baseline.success = true;
    results.baseline.reportPath = findLatestReport(/baseline-benchmark-.*\.json/);

    // 2. Sustained load test (60 minutes)
    console.log('=' .repeat(60));
    console.log('2️⃣ SUSTAINED LOAD TEST (24h simulation)');
    console.log('='.repeat(60));

    await runCommand('node', ['tests/load/sustained-load.mjs'], 3700); // 61 min timeout
    results.sustained.success = true;
    results.sustained.reportPath = findLatestReport(/load-test-report-.*\.json/);

    // 3. Memory profiling (10 minutes with --expose-gc)
    console.log('=' .repeat(60));
    console.log('3️⃣ MEMORY PROFILING');
    console.log('='.repeat(60));

    await runCommand('node', ['--expose-gc', 'tests/load/memory-profiler.mjs'], 700);
    results.memory.success = true;
    results.memory.reportPath = findLatestReport(/memory-profile-.*\.json/);

  } catch (error) {
    console.error(`\n❌ Test failed: ${error.message}`);
  }

  // Generate summary
  const { allPassed } = generateSummary(results);

  process.exit(allPassed ? 0 : 1);
}

// Run all tests
runAllTests().catch(error => {
  console.error('Fatal error:', error);
  process.exit(1);
});
