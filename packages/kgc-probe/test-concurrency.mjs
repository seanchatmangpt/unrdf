#!/usr/bin/env node
/**
 * @fileoverview Test runner for concurrency probe
 * Validates that probeConcurrency implements all required features
 */

import { probeConcurrency } from './src/probes/concurrency.mjs';

async function main() {
  console.log('='.repeat(70));
  console.log('KGC Probe - Concurrency Surface Test');
  console.log('Agent 4 - Concurrency Surface Probe');
  console.log('='.repeat(70));
  console.log();

  try {
    console.log('📊 Running concurrency probe...\n');

    const config = {
      timeout: 5000,
      maxWorkers: 8,
      samples: 10,
      budgetMs: 30000,
    };

    const startTime = Date.now();
    const observations = await probeConcurrency(config);
    const endTime = Date.now();

    console.log(`✅ Probe completed in ${endTime - startTime}ms\n`);
    console.log(`📋 Total observations: ${observations.length}\n`);

    // Display each observation
    observations.forEach((obs, idx) => {
      console.log(`${idx + 1}. ${obs.method}`);
      console.log(`   Timestamp: ${new Date(obs.timestamp).toISOString()}`);
      console.log(`   Guard Decision: ${obs.guardDecision || 'N/A'}`);
      console.log(`   Outputs:`);
      Object.entries(obs.outputs).forEach(([key, value]) => {
        if (typeof value === 'object' && value !== null) {
          console.log(`     ${key}: ${JSON.stringify(value)}`);
        } else {
          console.log(`     ${key}: ${value}`);
        }
      });
      if (obs.metadata && Object.keys(obs.metadata).length > 0) {
        console.log(`   Metadata: ${JSON.stringify(obs.metadata)}`);
      }
      console.log();
    });

    // Validation checks
    console.log('='.repeat(70));
    console.log('VALIDATION CHECKS');
    console.log('='.repeat(70));
    console.log();

    const requiredMethods = [
      'concurrency.worker_threads_available',
      'concurrency.shared_array_buffer',
      'concurrency.atomics',
      'concurrency.thread_pool_size',
      'concurrency.event_loop_latency',
      'concurrency.worker_spawn_time',
      'concurrency.message_passing_overhead',
      'concurrency.max_concurrent_workers',
      'concurrency.parallel_io_contention',
      'concurrency.event_loop_ordering',
      'concurrency.stack_depth',
      'concurrency.async_local_storage',
      'concurrency.microtask_queue_depth',
      'concurrency.max_concurrent_promises',
      'concurrency.stream_backpressure',
    ];

    const foundMethods = observations.map(o => o.method);
    const missing = requiredMethods.filter(m => !foundMethods.includes(m));

    if (missing.length === 0) {
      console.log('✅ All required probe methods implemented');
    } else {
      console.log('❌ Missing probe methods:');
      missing.forEach(m => console.log(`   - ${m}`));
    }

    // Check all observations have required fields
    const allHaveRequiredFields = observations.every(
      obs =>
        obs.method &&
        obs.inputs !== undefined &&
        obs.outputs !== undefined &&
        obs.timestamp &&
        obs.guardDecision !== undefined
    );

    if (allHaveRequiredFields) {
      console.log('✅ All observations have required fields (method, inputs, outputs, timestamp, guardDecision)');
    } else {
      console.log('❌ Some observations missing required fields');
    }

    // Check guard constraints
    const allAllowedOrUnknown = observations.every(
      obs => obs.guardDecision === 'allowed' || obs.guardDecision === 'unknown'
    );

    if (allAllowedOrUnknown) {
      console.log('✅ All guard decisions are "allowed" or "unknown" (no "denied")');
    } else {
      console.log('⚠️  Some operations were denied by guards');
    }

    console.log();
    console.log('='.repeat(70));
    console.log('TEST SUMMARY');
    console.log('='.repeat(70));
    console.log(`Total observations: ${observations.length}`);
    console.log(`Required methods: ${requiredMethods.length}`);
    console.log(`Found methods: ${foundMethods.length}`);
    console.log(`Execution time: ${endTime - startTime}ms`);
    console.log(`Budget: ${config.budgetMs}ms`);
    console.log(`Within budget: ${endTime - startTime <= config.budgetMs ? '✅' : '❌'}`);
    console.log();

    process.exit(0);
  } catch (error) {
    console.error('\n❌ Error running concurrency probe:');
    console.error(`   ${error.message}`);
    console.error('\n   Stack trace:');
    console.error(`   ${error.stack}`);
    process.exit(1);
  }
}

main();
