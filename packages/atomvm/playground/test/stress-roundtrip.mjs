/**
 * Stress Test Suite for JS→Erlang→JS Roundtrips
 * 
 * Tests various permutations of roundtrip patterns to validate SLA compliance
 * and identify edge cases.
 * 
 * @module stress-roundtrip
 */

import { getBridge } from '../src/kgc4d-bridge.mjs';
import { startRoundtrip, endRoundtrip, getSLAStats, resetSLAStats, OPERATION_TYPES, getSLAReport } from '../../src/roundtrip-sla.mjs';

const bridge = getBridge();

/**
 * Test configuration
 */
const CONFIG = {
  // Operation counts
  SEQUENTIAL_COUNT: 100,
  PARALLEL_COUNT: 50,
  NESTED_DEPTH: 5,
  CHAINED_LENGTH: 20,
  
  // Timing
  DELAY_MS: 1,
  TIMEOUT_MS: 5000,
  
  // Error injection
  ERROR_RATE: 0.05, // 5% error rate for testing
};

/**
 * Generate test permutations
 */
function generatePermutations() {
  return [
    {
      name: 'sequential_emit_events',
      description: 'Sequential emit_event operations',
      run: async () => {
        for (let i = 0; i < CONFIG.SEQUENTIAL_COUNT; i++) {
          await bridge.emitEvent(`TEST_EVENT_${i}`, { index: i });
        }
      },
    },
    {
      name: 'parallel_emit_events',
      description: 'Parallel emit_event operations',
      run: async () => {
        const promises = [];
        for (let i = 0; i < CONFIG.PARALLEL_COUNT; i++) {
          promises.push(bridge.emitEvent(`TEST_EVENT_${i}`, { index: i }));
        }
        await Promise.all(promises);
      },
    },
    {
      name: 'sequential_register_hooks',
      description: 'Sequential register_hook operations',
      run: async () => {
        for (let i = 0; i < CONFIG.SEQUENTIAL_COUNT; i++) {
          await bridge.registerHook({
            name: `test_hook_${i}`,
            trigger: 'before-add',
            validate: () => true,
          });
        }
      },
    },
    {
      name: 'parallel_register_hooks',
      description: 'Parallel register_hook operations',
      run: async () => {
        const promises = [];
        for (let i = 0; i < CONFIG.PARALLEL_COUNT; i++) {
          promises.push(bridge.registerHook({
            name: `test_hook_${i}`,
            trigger: 'before-add',
            validate: () => true,
          }));
        }
        await Promise.all(promises);
      },
    },
    {
      name: 'sequential_process_intents',
      description: 'Sequential process_intent operations',
      run: async () => {
        for (let i = 0; i < CONFIG.SEQUENTIAL_COUNT; i++) {
          await bridge.processIntent(`intent_${i}`, {
            description: `Test intent ${i}`,
          });
        }
      },
    },
    {
      name: 'parallel_process_intents',
      description: 'Parallel process_intent operations',
      run: async () => {
        const promises = [];
        for (let i = 0; i < CONFIG.PARALLEL_COUNT; i++) {
          promises.push(bridge.processIntent(`intent_${i}`, {
            description: `Test intent ${i}`,
          }));
        }
        await Promise.all(promises);
      },
    },
    {
      name: 'mixed_operations_sequential',
      description: 'Mixed operations in sequence',
      run: async () => {
        for (let i = 0; i < CONFIG.SEQUENTIAL_COUNT; i++) {
          if (i % 3 === 0) {
            await bridge.emitEvent(`EVENT_${i}`, { index: i });
          } else if (i % 3 === 1) {
            await bridge.registerHook({
              name: `hook_${i}`,
              trigger: 'before-add',
              validate: () => true,
            });
          } else {
            await bridge.processIntent(`intent_${i}`, {
              description: `Intent ${i}`,
            });
          }
        }
      },
    },
    {
      name: 'mixed_operations_parallel',
      description: 'Mixed operations in parallel',
      run: async () => {
        const promises = [];
        for (let i = 0; i < CONFIG.PARALLEL_COUNT; i++) {
          if (i % 3 === 0) {
            promises.push(bridge.emitEvent(`EVENT_${i}`, { index: i }));
          } else if (i % 3 === 1) {
            promises.push(bridge.registerHook({
              name: `hook_${i}`,
              trigger: 'before-add',
              validate: () => true,
            }));
          } else {
            promises.push(bridge.processIntent(`intent_${i}`, {
              description: `Intent ${i}`,
            }));
          }
        }
        await Promise.all(promises);
      },
    },
    {
      name: 'nested_operations',
      description: 'Nested operations (operation within operation)',
      run: async () => {
        async function nestedOperation(depth) {
          if (depth === 0) return;
          await bridge.emitEvent(`NESTED_${depth}`, { depth });
          await nestedOperation(depth - 1);
        }
        await nestedOperation(CONFIG.NESTED_DEPTH);
      },
    },
    {
      name: 'chained_operations',
      description: 'Chained operations (operation triggers next)',
      run: async () => {
        let lastResult = null;
        for (let i = 0; i < CONFIG.CHAINED_LENGTH; i++) {
          const result = await bridge.emitEvent(`CHAINED_${i}`, {
            previous: lastResult?.receipt?.id || null,
            index: i,
          });
          lastResult = result;
        }
      },
    },
    {
      name: 'burst_pattern',
      description: 'Burst pattern (sudden spike)',
      run: async () => {
        // Initial quiet period
        await new Promise(resolve => setTimeout(resolve, 100));
        
        // Burst
        const promises = [];
        for (let i = 0; i < CONFIG.PARALLEL_COUNT * 2; i++) {
          promises.push(bridge.emitEvent(`BURST_${i}`, { index: i }));
        }
        await Promise.all(promises);
        
        // Quiet period
        await new Promise(resolve => setTimeout(resolve, 100));
      },
    },
    {
      name: 'sustained_load',
      description: 'Sustained load pattern',
      run: async () => {
        const startTime = Date.now();
        const duration = 2000; // 2 seconds
        
        while (Date.now() - startTime < duration) {
          await bridge.emitEvent(`SUSTAINED_${Date.now()}`, {
            timestamp: Date.now(),
          });
          await new Promise(resolve => setTimeout(resolve, CONFIG.DELAY_MS));
        }
      },
    },
    {
      name: 'ramp_up_pattern',
      description: 'Ramp-up pattern (gradual increase)',
      run: async () => {
        for (let batch = 1; batch <= 10; batch++) {
          const promises = [];
          for (let i = 0; i < batch; i++) {
            promises.push(bridge.emitEvent(`RAMP_${batch}_${i}`, {
              batch,
              index: i,
            }));
          }
          await Promise.all(promises);
          await new Promise(resolve => setTimeout(resolve, 50));
        }
      },
    },
    {
      name: 'error_injection',
      description: 'Error injection (simulated failures)',
      run: async () => {
        let errorCount = 0;
        for (let i = 0; i < CONFIG.SEQUENTIAL_COUNT; i++) {
          try {
            // Simulate error based on error rate
            if (Math.random() < CONFIG.ERROR_RATE) {
              // Force error by passing invalid data
              await bridge.emitEvent(null, null);
              errorCount++;
            } else {
              await bridge.emitEvent(`VALID_${i}`, { index: i });
            }
          } catch (error) {
            errorCount++;
            // Expected error, continue
          }
        }
        console.log(`[Stress Test] Error injection: ${errorCount} errors out of ${CONFIG.SEQUENTIAL_COUNT}`);
      },
    },
  ];
}

/**
 * Run a single stress test
 */
async function runStressTest(test) {
  console.log(`\n[Stress Test] Running: ${test.name}`);
  console.log(`[Stress Test] Description: ${test.description}`);
  
  const startTime = Date.now();
  let error = null;
  
  try {
    await Promise.race([
      test.run(),
      new Promise((_, reject) =>
        setTimeout(() => reject(new Error('Test timeout')), CONFIG.TIMEOUT_MS)
      ),
    ]);
  } catch (e) {
    error = e;
  }
  
  const duration = Date.now() - startTime;
  const slaReport = getSLAReport();
  
  return {
    name: test.name,
    description: test.description,
    duration,
    error: error?.message || null,
    slaReport,
  };
}

/**
 * Run all stress tests
 */
export async function runAllStressTests() {
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('JS→Erlang→JS Roundtrip Stress Tests');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  
  // Reset SLA stats before tests
  resetSLAStats();
  
  const tests = generatePermutations();
  const results = [];
  
  for (const test of tests) {
    const result = await runStressTest(test);
    results.push(result);
    
    // Print summary
    console.log(`[Stress Test] Duration: ${result.duration}ms`);
    if (result.error) {
      console.log(`[Stress Test] ❌ Error: ${result.error}`);
    } else {
      console.log(`[Stress Test] ✅ Completed`);
    }
    
    // Print SLA stats
    const emitStats = getSLAStats(OPERATION_TYPES.EMIT_EVENT);
    const hookStats = getSLAStats(OPERATION_TYPES.REGISTER_HOOK);
    const intentStats = getSLAStats(OPERATION_TYPES.PROCESS_INTENT);
    
    console.log(`[Stress Test] SLA Stats:`);
    console.log(`  emit_event: ${emitStats.count} ops, ${emitStats.averageLatency.toFixed(2)}ms avg, ${(emitStats.errorRate * 100).toFixed(2)}% error`);
    console.log(`  register_hook: ${hookStats.count} ops, ${hookStats.averageLatency.toFixed(2)}ms avg, ${(hookStats.errorRate * 100).toFixed(2)}% error`);
    console.log(`  process_intent: ${intentStats.count} ops, ${intentStats.averageLatency.toFixed(2)}ms avg, ${(intentStats.errorRate * 100).toFixed(2)}% error`);
    
    // Small delay between tests
    await new Promise(resolve => setTimeout(resolve, 100));
  }
  
  // Final summary
  console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('Stress Test Summary');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  
  const totalDuration = results.reduce((sum, r) => sum + r.duration, 0);
  const errorCount = results.filter(r => r.error).length;
  const finalSLAReport = getSLAReport();
  
  console.log(`Total Tests: ${results.length}`);
  console.log(`Passed: ${results.length - errorCount}`);
  console.log(`Failed: ${errorCount}`);
  console.log(`Total Duration: ${totalDuration}ms`);
  console.log(`\nFinal SLA Report:`);
  console.log(`  Overall Compliant: ${finalSLAReport.overall.overallCompliant ? '✅' : '❌'}`);
  console.log(`  Total Roundtrips: ${finalSLAReport.overall.totalRoundtrips}`);
  console.log(`  Total Errors: ${finalSLAReport.overall.totalErrors}`);
  console.log(`  Overall Error Rate: ${(finalSLAReport.overall.overallErrorRate * 100).toFixed(2)}%`);
  console.log(`  Violations: ${finalSLAReport.violations.length}`);
  
  if (finalSLAReport.violations.length > 0) {
    console.log(`\nSLA Violations:`);
    finalSLAReport.violations.forEach(v => {
      console.log(`  - ${v.operationType}: ${v.reason}`);
    });
  }
  
  return {
    results,
    summary: {
      totalTests: results.length,
      passed: results.length - errorCount,
      failed: errorCount,
      totalDuration,
      slaReport: finalSLAReport,
    },
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllStressTests()
    .then(() => {
      console.log('\n✅ Stress tests completed');
      process.exit(0);
    })
    .catch(error => {
      console.error('\n❌ Stress tests failed:', error);
      process.exit(1);
    });
}

