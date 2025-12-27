#!/usr/bin/env node
/**
 * @file Runtime Probe Demo - Agent 2 Delivery
 * @description Demonstrates runtime surface probe with sample observations
 */

import { probeRuntime } from '../src/probes/runtime.mjs';

async function main() {
  console.log('═══════════════════════════════════════════════════════════════');
  console.log('  KGC Probe - Agent 2 (Runtime Surface) - Demonstration');
  console.log('═══════════════════════════════════════════════════════════════\n');

  const config = { samples: 100, budgetMs: 5000 };

  console.log(`Configuration:`);
  console.log(`  Samples: ${config.samples}`);
  console.log(`  Budget: ${config.budgetMs}ms\n`);

  const start = Date.now();
  const observations = await probeRuntime(config);
  const duration = Date.now() - start;

  console.log(`✅ Probe completed in ${duration}ms`);
  console.log(`📊 Collected ${observations.length} observations\n`);

  console.log('─────────────────────────────────────────────────────────────');
  console.log('SAMPLE OBSERVATIONS:');
  console.log('─────────────────────────────────────────────────────────────\n');

  // Show first 3 observations as examples
  for (let i = 0; i < Math.min(3, observations.length); i++) {
    const obs = observations[i];
    console.log(`[${i + 1}] Method: ${obs.method}`);
    console.log(`    Hash: ${obs.hash}`);
    console.log(`    Guard: ${obs.guardDecision}`);
    console.log(`    Inputs: ${JSON.stringify(obs.inputs)}`);
    console.log(`    Outputs: ${JSON.stringify(obs.outputs, null, 2)}`);
    if (obs.metadata) {
      console.log(`    Metadata: ${JSON.stringify(obs.metadata, null, 2)}`);
    }
    console.log();
  }

  console.log('─────────────────────────────────────────────────────────────');
  console.log('ALL METHODS PROBED:');
  console.log('─────────────────────────────────────────────────────────────\n');

  const methodList = observations.map(o => o.method).sort();
  methodList.forEach((method, idx) => {
    console.log(`  ${(idx + 1).toString().padStart(2, ' ')}. ${method}`);
  });

  console.log('\n─────────────────────────────────────────────────────────────');
  console.log('DETERMINISM PROOF:');
  console.log('─────────────────────────────────────────────────────────────\n');

  // Run probe twice and compare deterministic methods
  console.log('Running probe twice to verify hash stability...');
  const obs1 = await probeRuntime(config);
  const obs2 = await probeRuntime(config);

  const deterministicMethods = [
    'runtime.available_globals',
    'runtime.module_system',
    'runtime.node_version',
    'runtime.platform_arch',
    'runtime.v8_version',
  ];

  console.log('\nDeterministic methods (same hash on repeated calls):\n');

  let hashMatches = 0;
  for (const method of deterministicMethods) {
    const o1 = obs1.find(o => o.method === method);
    const o2 = obs2.find(o => o.method === method);

    const match = o1.hash === o2.hash;
    hashMatches += match ? 1 : 0;

    console.log(`  ${method}`);
    console.log(`    Run 1: ${o1.hash.substring(0, 16)}...`);
    console.log(`    Run 2: ${o2.hash.substring(0, 16)}...`);
    console.log(`    Match: ${match ? '✅ YES' : '❌ NO'}\n`);
  }

  console.log(`Determinism Score: ${hashMatches}/${deterministicMethods.length} (${(hashMatches / deterministicMethods.length * 100).toFixed(1)}%)\n`);

  console.log('─────────────────────────────────────────────────────────────');
  console.log('GUARD VERIFICATION:');
  console.log('─────────────────────────────────────────────────────────────\n');

  const denied = observations.filter(o => o.guardDecision === 'denied');
  const allowed = observations.filter(o => o.guardDecision === 'allowed');

  console.log(`  Allowed: ${allowed.length}`);
  console.log(`  Denied: ${denied.length}`);
  console.log(`  Total: ${observations.length}\n`);

  if (denied.length > 0) {
    console.log('❌ GUARD VIOLATIONS DETECTED:');
    denied.forEach(obs => {
      console.log(`  - ${obs.method}: ${obs.metadata?.reason || 'No reason'}`);
    });
  } else {
    console.log('✅ All operations allowed (no guard violations)');
  }

  console.log('\n─────────────────────────────────────────────────────────────');
  console.log('KEY FINDINGS:');
  console.log('─────────────────────────────────────────────────────────────\n');

  const nodeVersion = observations.find(o => o.method === 'runtime.node_version');
  const v8Version = observations.find(o => o.method === 'runtime.v8_version');
  const platform = observations.find(o => o.method === 'runtime.platform_arch');
  const wasm = observations.find(o => o.method === 'runtime.wasm_support');
  const workers = observations.find(o => o.method === 'runtime.worker_threads');
  const globals = observations.find(o => o.method === 'runtime.available_globals');
  const timer = observations.find(o => o.method === 'runtime.timer_resolution');
  const eventLoop = observations.find(o => o.method === 'runtime.event_loop_latency');
  const memory = observations.find(o => o.method === 'runtime.memory_limits');

  console.log(`  Node.js Version: ${nodeVersion.outputs.version}`);
  console.log(`  V8 Version: ${v8Version.outputs.version}`);
  console.log(`  Platform: ${platform.outputs.platform}`);
  console.log(`  Architecture: ${platform.outputs.arch}`);
  console.log(`  WebAssembly: ${wasm.outputs.instantiate ? 'Supported' : 'Not supported'}`);
  console.log(`  Worker Threads: ${workers.outputs.available ? 'Available' : 'Not available'}`);
  console.log(`  Available Globals: ${Object.keys(globals.outputs).filter(k => globals.outputs[k]).length}/${Object.keys(globals.outputs).length}`);
  console.log(`  Timer Resolution: ${(timer.outputs.mean_ns / 1000).toFixed(3)}µs (mean)`);
  console.log(`  Event Loop Latency: ${eventLoop.outputs.mean_ms.toFixed(3)}ms (mean), ${eventLoop.outputs.p99_ms.toFixed(3)}ms (p99)`);
  console.log(`  Memory Usage: ${(memory.outputs.heap_used_bytes / 1024 / 1024).toFixed(2)}MB heap used, ${(memory.outputs.rss_bytes / 1024 / 1024).toFixed(2)}MB RSS`);

  console.log('\n═══════════════════════════════════════════════════════════════');
  console.log('  Agent 2 (Runtime Surface) - Probe Complete');
  console.log('═══════════════════════════════════════════════════════════════\n');
}

main().catch(console.error);
