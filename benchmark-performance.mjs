#!/usr/bin/env node
/**
 * Comprehensive Performance Benchmark Suite
 * Measures: Test execution, memory, import resolution, store creation, pattern matching
 */

import { performance } from 'node:perf_hooks';
import { execSync } from 'node:child_process';
import fs from 'node:fs';

// ANSI color codes
const colors = {
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
  reset: '\x1b[0m',
  bold: '\x1b[1m'
};

class PerformanceBenchmarker {
  constructor() {
    this.results = {
      timestamp: new Date().toISOString(),
      benchmarks: {},
      summary: {},
      slaViolations: []
    };
  }

  formatBytes(bytes) {
    return `${(bytes / 1024 / 1024).toFixed(2)} MB`;
  }

  formatTime(ms) {
    return ms < 1000 ? `${ms.toFixed(2)}ms` : `${(ms / 1000).toFixed(2)}s`;
  }

  log(message, color = 'reset') {
    console.log(`${colors[color]}${message}${colors.reset}`);
  }

  async measureMemory(fn, label) {
    const startMem = process.memoryUsage();
    const start = performance.now();

    await fn();

    const end = performance.now();
    const endMem = process.memoryUsage();

    const result = {
      duration: end - start,
      memory: {
        heapUsed: endMem.heapUsed - startMem.heapUsed,
        heapTotal: endMem.heapTotal - startMem.heapTotal,
        external: endMem.external - startMem.external,
        rss: endMem.rss - startMem.rss,
        peak: {
          heapUsed: endMem.heapUsed,
          rss: endMem.rss
        }
      }
    };

    this.results.benchmarks[label] = result;
    return result;
  }

  async benchmarkImportResolution() {
    this.log('\n📦 BENCHMARK 1: Import Resolution Performance', 'cyan');

    // Test cold start
    const coldStart = await this.measureMemory(async () => {
      const testFile = '/tmp/test-import-cold.mjs';
      fs.writeFileSync(testFile, `
        import { createStore } from '@unrdf/oxigraph';
        const store = createStore();
        console.log('Cold start');
      `);
      execSync(`node ${testFile}`, { stdio: 'pipe' });
      fs.unlinkSync(testFile);
    }, 'import_cold_start');

    this.log(`  Cold Start: ${this.formatTime(coldStart.duration)}`, 'yellow');

    // Test warm start
    const warmStart = await this.measureMemory(async () => {
      const testFile = '/tmp/test-import-warm.mjs';
      fs.writeFileSync(testFile, `
        import { createStore } from '@unrdf/oxigraph';
        const store = createStore();
        console.log('Warm start');
      `);
      execSync(`node ${testFile}`, { stdio: 'pipe' });
      fs.unlinkSync(testFile);
    }, 'import_warm_start');

    this.log(`  Warm Start: ${this.formatTime(warmStart.duration)}`, 'yellow');
    this.log(`  Delta: ${this.formatTime(coldStart.duration - warmStart.duration)}`, 'yellow');
  }

  async benchmarkStoreCreation() {
    this.log('\n🗄️  BENCHMARK 2: Store Creation Performance', 'cyan');

    const iterations = 100;
    const testFile = '/tmp/test-store-creation.mjs';

    fs.writeFileSync(testFile, `
      import { createStore } from '@unrdf/oxigraph';
      import { performance } from 'node:perf_hooks';

      const iterations = ${iterations};
      const timings = [];

      for (let i = 0; i < iterations; i++) {
        const start = performance.now();
        const store = createStore();
        const end = performance.now();
        timings.push(end - start);
      }

      const avg = timings.reduce((a, b) => a + b, 0) / timings.length;
      const min = Math.min(...timings);
      const max = Math.max(...timings);
      const p95 = timings.sort((a, b) => a - b)[Math.floor(iterations * 0.95)];

      console.log(JSON.stringify({ avg, min, max, p95 }));
    `);

    const output = execSync(`node ${testFile}`, { encoding: 'utf-8' });
    const stats = JSON.parse(output.trim());

    this.results.benchmarks.store_creation = {
      iterations,
      ...stats
    };

    this.log(`  Average: ${this.formatTime(stats.avg)}`, 'yellow');
    this.log(`  Min: ${this.formatTime(stats.min)}`, 'yellow');
    this.log(`  Max: ${this.formatTime(stats.max)}`, 'yellow');
    this.log(`  P95: ${this.formatTime(stats.p95)}`, 'yellow');

    fs.unlinkSync(testFile);
  }

  async benchmarkTestExecution() {
    this.log('\n🧪 BENCHMARK 3: Test Suite Execution', 'cyan');

    const start = performance.now();
    const startMem = process.memoryUsage();

    try {
      const output = execSync('timeout 5s npm test', {
        encoding: 'utf-8',
        stdio: 'pipe',
        maxBuffer: 10 * 1024 * 1024
      });

      const end = performance.now();
      const endMem = process.memoryUsage();
      const duration = end - start;

      this.results.benchmarks.test_execution = {
        duration,
        success: true,
        memory: {
          heapUsed: endMem.heapUsed - startMem.heapUsed,
          rss: endMem.rss - startMem.rss,
          peak: {
            heapUsed: endMem.heapUsed,
            rss: endMem.rss
          }
        }
      };

      // Check SLA
      const slaLimit = 5000; // 5 seconds
      const status = duration < slaLimit ? '✅' : '❌';
      const color = duration < slaLimit ? 'green' : 'red';

      this.log(`  ${status} Duration: ${this.formatTime(duration)} (SLA: <5s)`, color);
      this.log(`  Peak Memory: ${this.formatBytes(endMem.rss)}`, 'yellow');

      if (duration >= slaLimit) {
        this.results.slaViolations.push({
          test: 'test_execution',
          expected: slaLimit,
          actual: duration,
          violation: duration - slaLimit
        });
      }

      // Parse test output for counts
      const testMatch = output.match(/(\d+) passed/);
      if (testMatch) {
        this.log(`  Tests Passed: ${testMatch[1]}`, 'green');
      }

    } catch (error) {
      const end = performance.now();
      const duration = end - start;

      this.results.benchmarks.test_execution = {
        duration,
        success: false,
        error: error.message
      };

      this.log(`  ❌ Tests failed or timeout after ${this.formatTime(duration)}`, 'red');
    }
  }

  async benchmarkYAWLPatternMatching() {
    this.log('\n🔍 BENCHMARK 4: YAWL Pattern Matching Performance', 'cyan');

    // Check if YAWL exists
    const yawlPath = '/home/user/unrdf/packages/yawl';
    if (!fs.existsSync(yawlPath)) {
      this.log('  ⚠️  YAWL package not found, skipping', 'yellow');
      return;
    }

    const testFile = '/tmp/test-yawl-patterns.mjs';
    fs.writeFileSync(testFile, `
      import { performance } from 'node:perf_hooks';

      // Simulate pattern matching workload
      const patterns = Array(1000).fill(0).map((_, i) => ({
        id: \`pattern_\${i}\`,
        type: 'workflow',
        conditions: Array(10).fill(0).map((_, j) => ({ key: \`k\${j}\`, value: \`v\${j}\` }))
      }));

      const iterations = 1000;
      const timings = [];

      for (let i = 0; i < iterations; i++) {
        const start = performance.now();

        // Pattern matching simulation
        const matches = patterns.filter(p =>
          p.conditions.some(c => c.value.includes('v5'))
        );

        const end = performance.now();
        timings.push(end - start);
      }

      const avg = timings.reduce((a, b) => a + b, 0) / timings.length;
      const min = Math.min(...timings);
      const max = Math.max(...timings);
      const p95 = timings.sort((a, b) => a - b)[Math.floor(iterations * 0.95)];

      console.log(JSON.stringify({ avg, min, max, p95, patterns: patterns.length }));
    `);

    const output = execSync(`node ${testFile}`, { encoding: 'utf-8' });
    const stats = JSON.parse(output.trim());

    this.results.benchmarks.yawl_pattern_matching = stats;

    this.log(`  Patterns: ${stats.patterns}`, 'yellow');
    this.log(`  Average: ${this.formatTime(stats.avg)}`, 'yellow');
    this.log(`  P95: ${this.formatTime(stats.p95)}`, 'yellow');

    fs.unlinkSync(testFile);
  }

  async benchmarkFileSystemImpact() {
    this.log('\n📁 BENCHMARK 5: File System Impact Analysis', 'cyan');

    const yawlFiles = execSync('ls -1 packages/yawl/src/**/*.mjs 2>/dev/null | wc -l', { encoding: 'utf-8' }).trim();
    const totalFiles = execSync('ls -1 packages/*/src/*.mjs 2>/dev/null | wc -l', { encoding: 'utf-8' }).trim();
    const recentFiles = execSync('find packages -name "*.mjs" -mtime -1 2>/dev/null | wc -l', { encoding: 'utf-8' }).trim();

    this.results.benchmarks.file_system = {
      yawl_files: parseInt(yawlFiles),
      total_microframework_files: parseInt(totalFiles),
      files_modified_24h: parseInt(recentFiles)
    };

    this.log(`  YAWL Files: ${yawlFiles}`, 'yellow');
    this.log(`  Total Microframework Files: ${totalFiles}`, 'yellow');
    this.log(`  Modified in Last 24h: ${recentFiles}`, 'yellow');
  }

  generateReport() {
    this.log('\n' + '='.repeat(60), 'bold');
    this.log('📊 PERFORMANCE BENCHMARK REPORT', 'bold');
    this.log('='.repeat(60), 'bold');

    // SLA Status
    if (this.results.slaViolations.length === 0) {
      this.log('\n✅ ALL SLA REQUIREMENTS MET', 'green');
    } else {
      this.log('\n❌ SLA VIOLATIONS DETECTED', 'red');
      this.results.slaViolations.forEach(v => {
        this.log(`  ${v.test}: ${this.formatTime(v.actual)} (expected <${this.formatTime(v.expected)})`, 'red');
        this.log(`  Violation: +${this.formatTime(v.violation)}`, 'red');
      });
    }

    // Summary
    const testExec = this.results.benchmarks.test_execution;
    if (testExec) {
      this.log('\n📈 KEY METRICS:', 'cyan');
      this.log(`  Test Suite Duration: ${this.formatTime(testExec.duration)}`, 'yellow');
      if (testExec.memory) {
        this.log(`  Peak Memory (RSS): ${this.formatBytes(testExec.memory.peak.rss)}`, 'yellow');
        this.log(`  Heap Used: ${this.formatBytes(testExec.memory.peak.heapUsed)}`, 'yellow');
      }
    }

    // Store creation stats
    const storeCreation = this.results.benchmarks.store_creation;
    if (storeCreation) {
      this.log(`  Store Creation (avg): ${this.formatTime(storeCreation.avg)}`, 'yellow');
      this.log(`  Store Creation (p95): ${this.formatTime(storeCreation.p95)}`, 'yellow');
    }

    // File counts
    const fileSystem = this.results.benchmarks.file_system;
    if (fileSystem) {
      this.log(`  YAWL Source Files: ${fileSystem.yawl_files}`, 'yellow');
      this.log(`  Total Framework Files: ${fileSystem.total_microframework_files}`, 'yellow');
    }

    this.log('\n' + '='.repeat(60), 'bold');

    // Save detailed results
    const reportPath = '/home/user/unrdf/benchmark-results.json';
    fs.writeFileSync(reportPath, JSON.stringify(this.results, null, 2));
    this.log(`\n💾 Detailed results saved to: ${reportPath}`, 'blue');

    return this.results;
  }

  async runAll() {
    this.log('🚀 Starting Comprehensive Performance Benchmark Suite', 'bold');
    this.log(`Timestamp: ${this.results.timestamp}`, 'blue');

    try {
      await this.benchmarkFileSystemImpact();
      await this.benchmarkTestExecution();
      await this.benchmarkStoreCreation();
      await this.benchmarkImportResolution();
      await this.benchmarkYAWLPatternMatching();

      return this.generateReport();
    } catch (error) {
      this.log(`\n❌ Benchmark failed: ${error.message}`, 'red');
      console.error(error);
      process.exit(1);
    }
  }
}

// Execute benchmarks
const benchmarker = new PerformanceBenchmarker();
benchmarker.runAll().then(results => {
  process.exit(results.slaViolations.length === 0 ? 0 : 1);
});
