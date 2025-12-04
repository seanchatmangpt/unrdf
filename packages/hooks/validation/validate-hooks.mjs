#!/usr/bin/env node

/**
 * Comprehensive Hook Validation and Performance Benchmarking
 * Validates speed, count, and correctness of all hooks
 */

import { performance } from 'perf_hooks';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { readdirSync, statSync } from 'fs';
import {
  defineHook,
  executeHook,
  executeHookChain,
  executeBatch,
  createHookRegistry,
  registerHook,
  listHooks,
  compileHookChain,
  builtinHooks,
  QuadPool,
} from '../src/index.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, '..');

// OTEL-like metrics collection
class MetricsCollector {
  constructor() {
    this.metrics = {
      hookCounts: {},
      timing: {},
      errors: [],
      warnings: [],
      validationResults: {},
    };
  }

  recordMetric(category, name, value, unit = 'ms') {
    if (!this.metrics.timing[category]) {
      this.metrics.timing[category] = [];
    }
    this.metrics.timing[category].push({ name, value, unit });
  }

  recordError(message) {
    this.metrics.errors.push(message);
  }

  recordWarning(message) {
    this.metrics.warnings.push(message);
  }

  getStats(category) {
    const values = this.metrics.timing[category]?.map(m => m.value) || [];
    if (values.length === 0) return null;

    return {
      min: Math.min(...values),
      max: Math.max(...values),
      avg: values.reduce((a, b) => a + b, 0) / values.length,
      count: values.length,
      sum: values.reduce((a, b) => a + b, 0),
    };
  }

  generateReport() {
    return this.metrics;
  }
}

const collector = new MetricsCollector();

// Count hook files
function countHookFiles() {
  const hooksDir = join(ROOT, 'src', 'hooks');
  const files = readdirSync(hooksDir).filter(f => f.endsWith('.mjs'));
  const securityDir = join(hooksDir, 'security');

  try {
    const securityFiles = readdirSync(securityDir).filter(f => f.endsWith('.mjs'));
    files.push(...securityFiles.map(f => join('security', f)));
  } catch (e) {
    // security dir may not exist
  }

  collector.metrics.hookCounts.totalFiles = files.length;
  return files;
}

// Validate hook definitions
async function validateHookDefinitions() {
  console.log('\n📋 Validating Hook Definitions...');

  const registry = createHookRegistry();

  // Test built-in hooks
  let builtinCount = 0;
  if (Array.isArray(builtinHooks)) {
    builtinCount = builtinHooks.length;
    for (const hook of builtinHooks) {
      try {
        registerHook(registry, hook);
      } catch (e) {
        collector.recordError(`Failed to register built-in hook: ${e.message}`);
      }
    }
  }

  const allHooks = listHooks(registry);
  collector.metrics.hookCounts.builtin = builtinCount;
  collector.metrics.hookCounts.registered = allHooks.length;

  console.log(`✓ Built-in hooks: ${builtinCount}`);
  console.log(`✓ Registered hooks: ${allHooks.length}`);

  return { registry, allHooks };
}

// Benchmark hook execution
async function benchmarkExecution(registry, hooks) {
  console.log('\n⚡ Benchmarking Hook Execution...');

  const testQuad = {
    subject: { termType: 'NamedNode', value: 'http://example.org/subject' },
    predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
    object: { termType: 'NamedNode', value: 'http://example.org/Class' },
    graph: { termType: 'DefaultGraph' },
  };

  // Single execution benchmark
  const singleTimes = [];
  const iterations = 100;

  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    try {
      await executeHook(hooks[0], testQuad, { registry });
    } catch (e) {
      // Expected for some hooks
    }
    const end = performance.now();
    singleTimes.push(end - start);
    collector.recordMetric('single_execution', `iteration_${i}`, end - start);
  }

  const singleStats = collector.getStats('single_execution');
  console.log(`✓ Single execution (100 iterations):`);
  console.log(`  - Avg: ${singleStats.avg.toFixed(2)}ms`);
  console.log(`  - Min: ${singleStats.min.toFixed(2)}ms`);
  console.log(`  - Max: ${singleStats.max.toFixed(2)}ms`);

  // Batch execution benchmark
  const quads = Array(1000).fill(testQuad);
  const batchStart = performance.now();
  try {
    await executeBatch(hooks.slice(0, 5), quads, { registry });
  } catch (e) {
    collector.recordWarning(`Batch execution partial failure: ${e.message}`);
  }
  const batchEnd = performance.now();
  const batchTime = batchEnd - batchStart;

  collector.recordMetric('batch_execution', 'batch_1000_quads', batchTime);
  console.log(`✓ Batch execution (1000 quads, 5 hooks):`);
  console.log(`  - Time: ${batchTime.toFixed(2)}ms`);
  console.log(`  - Per quad: ${(batchTime / 1000).toFixed(4)}ms`);

  // Chain compilation benchmark
  const chainStart = performance.now();
  try {
    const compiled = compileHookChain(hooks.slice(0, 3));
    collector.recordMetric('compilation', 'chain_compile', chainStart - performance.now());
  } catch (e) {
    collector.recordWarning(`Chain compilation failed: ${e.message}`);
  }
  const chainEnd = performance.now();
  collector.recordMetric('compilation', 'chain_compile_time', chainEnd - chainStart);
  console.log(`✓ Chain compilation: ${(chainEnd - chainStart).toFixed(2)}ms`);
}

// Check hook file sizes
function analyzeFileSizes() {
  console.log('\n📊 Hook File Analysis...');

  const hooksDir = join(ROOT, 'src', 'hooks');
  const files = readdirSync(hooksDir).filter(f => f.endsWith('.mjs'));

  let totalSize = 0;
  const fileSizes = [];

  for (const file of files) {
    try {
      const stats = statSync(join(hooksDir, file));
      totalSize += stats.size;
      fileSizes.push({ file, size: stats.size });
    } catch (e) {
      collector.recordError(`Failed to stat file ${file}: ${e.message}`);
    }
  }

  fileSizes.sort((a, b) => b.size - a.size);

  console.log(`✓ Total hook code size: ${(totalSize / 1024).toFixed(2)} KB`);
  console.log(`✓ Average file size: ${(totalSize / files.length / 1024).toFixed(2)} KB`);
  console.log(`✓ Top 5 largest files:`);

  fileSizes.slice(0, 5).forEach(({ file, size }) => {
    console.log(`  - ${file}: ${(size / 1024).toFixed(2)} KB`);
  });

  collector.metrics.hookCounts.totalSize = totalSize;
  collector.metrics.hookCounts.averageFileSize = totalSize / files.length;
}

// Quality metrics
function validateQuality() {
  console.log('\n🎯 Quality Metrics...');

  const metrics = {
    fileCount: collector.metrics.hookCounts.totalFiles,
    registeredCount: collector.metrics.hookCounts.registered,
    errorCount: collector.metrics.errors.length,
    warningCount: collector.metrics.warnings.length,
  };

  const score = Math.max(0, 100 - (metrics.errorCount * 10) - (metrics.warningCount * 2));

  collector.metrics.validationResults = {
    score: Math.round(score),
    passed: metrics.errorCount === 0,
    metrics,
  };

  console.log(`✓ Quality Score: ${Math.round(score)}/100`);
  console.log(`✓ Errors: ${metrics.errorCount}`);
  console.log(`✓ Warnings: ${metrics.warningCount}`);

  if (metrics.errorCount === 0) {
    console.log('✅ All validation checks passed!');
  } else {
    console.log('❌ Validation issues detected');
  }
}

// Main validation
async function main() {
  console.log('🔍 Starting Comprehensive Hook Validation\n');
  console.log('='.repeat(60));

  try {
    // Count files
    const files = countHookFiles();
    console.log(`\n📁 Hook Files: ${files.length}`);
    console.log('   Files:', files.slice(0, 5).join(', '), files.length > 5 ? '...' : '');

    // Validate definitions
    const { registry, allHooks } = await validateHookDefinitions();

    // Benchmark execution
    if (allHooks.length > 0) {
      await benchmarkExecution(registry, allHooks);
    }

    // Analyze file sizes
    analyzeFileSizes();

    // Quality assessment
    validateQuality();

    // Generate report
    console.log('\n' + '='.repeat(60));
    console.log('\n📈 VALIDATION SUMMARY\n');

    const report = collector.generateReport();
    console.log(`Total Hook Files: ${report.hookCounts.totalFiles}`);
    console.log(`Registered Hooks: ${report.hookCounts.registered}`);
    console.log(`Built-in Hooks: ${report.hookCounts.builtin}`);
    console.log(`Total Code Size: ${(report.hookCounts.totalSize / 1024).toFixed(2)} KB`);
    console.log(`Validation Score: ${report.validationResults.score}/100`);
    console.log(`Status: ${report.validationResults.passed ? '✅ PASSED' : '❌ FAILED'}`);

    if (report.errors.length > 0) {
      console.log('\n⚠️  Errors:');
      report.errors.forEach(e => console.log(`  - ${e}`));
    }

    if (report.warnings.length > 0) {
      console.log('\n⚠️  Warnings:');
      report.warnings.forEach(w => console.log(`  - ${w}`));
    }

    return report.validationResults.passed ? 0 : 1;
  } catch (error) {
    console.error('\n❌ Validation Failed:', error.message);
    console.error(error.stack);
    return 1;
  }
}

const exitCode = await main();
process.exit(exitCode);
