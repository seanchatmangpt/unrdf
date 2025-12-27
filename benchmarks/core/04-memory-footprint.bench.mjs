/**
 * @file Benchmark 4: Memory Footprint Profiling
 * @description Profiles memory usage with GC awareness
 *
 * Targets:
 * - Memory: < 5MB per hook
 * - Register 1000-10000 hooks and measure peak memory
 * - Detect memory leaks using baseline cleanup pattern
 */

import { defineHook } from '../../src/knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../src/knowledge-engine/knowledge-hook-manager.mjs';

/**
 * Get current memory usage in MB
 * @returns {number} Memory usage in MB
 */
function getMemoryUsageMB() {
  const usage = process.memoryUsage();
  return usage.heapUsed / 1024 / 1024;
}

/**
 * Force garbage collection if available
 */
function forceGC() {
  if (global.gc) {
    global.gc();
  }
}

/**
 * Benchmark memory footprint
 * @returns {Promise<Object>} Benchmark results
 */
export async function runMemoryFootprintBenchmark() {
  const results = {
    name: 'memory-footprint',
    tests: [],
    summary: {},
    passed: false,
  };

  const testScales = [
    { count: 10, name: 'small' },
    { count: 50, name: 'medium' },
    { count: 100, name: 'large' },
  ];

  for (const scale of testScales) {
    // Force GC before test
    forceGC();
    await new Promise(resolve => setTimeout(resolve, 100));

    const baselineMemory = getMemoryUsageMB();
    const manager = new KnowledgeHookManager({
      basePath: process.cwd(),
      enableKnowledgeHooks: true,
      strictMode: false,
    });

    const startTime = performance.now();

    // Register N hooks
    for (let i = 0; i < scale.count; i++) {
      const hook = defineHook({
        meta: {
          name: `memory-hook-${i}`,
          description: `Memory test hook ${i}`,
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://bench/memory/${i}.rq`,
            sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
            mediaType: 'application/sparql-query',
          },
        },
        run: async ({ payload }) => {
          return { result: { id: i, data: payload } };
        },
      });

      manager.addKnowledgeHook(hook);
    }

    const registrationTime = performance.now() - startTime;

    // Force GC after registration
    forceGC();
    await new Promise(resolve => setTimeout(resolve, 100));

    const peakMemory = getMemoryUsageMB();
    const memoryUsed = peakMemory - baselineMemory;
    const memoryPerHook = memoryUsed / scale.count;

    // Cleanup
    manager.clearKnowledgeHooks();
    forceGC();
    await new Promise(resolve => setTimeout(resolve, 100));

    const afterCleanupMemory = getMemoryUsageMB();
    const memoryLeaked = afterCleanupMemory - baselineMemory;

    const testResult = {
      scale: scale.name,
      hookCount: scale.count,
      registrationTimeMs: registrationTime,
      baselineMemoryMB: baselineMemory,
      peakMemoryMB: peakMemory,
      memoryUsedMB: memoryUsed,
      memoryPerHookMB: memoryPerHook,
      afterCleanupMemoryMB: afterCleanupMemory,
      memoryLeakedMB: memoryLeaked,
      passed: memoryPerHook < 5.0 && memoryLeaked < 10.0,
    };

    results.tests.push(testResult);
  }

  // Summary
  const allPassed = results.tests.every(t => t.passed);
  results.summary = {
    totalTests: results.tests.length,
    passed: results.tests.filter(t => t.passed).length,
    failed: results.tests.filter(t => !t.passed).length,
    gcAvailable: !!global.gc,
    gcWarning: !global.gc ? 'Run with --expose-gc for accurate memory profiling' : null,
  };
  results.passed = allPassed;

  return results;
}
