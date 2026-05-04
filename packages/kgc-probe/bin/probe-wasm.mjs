#!/usr/bin/env node
/**
 * @file WASM Probe CLI
 * @description Command-line tool to run WASM capability and performance probe
 */

import { probeWasm } from '../src/probes/wasm.mjs';

async function main() {
  const args = process.argv.slice(2);
  
  // Parse CLI arguments
  const config = {
    samples: 100,
    timeout: 5000,
    maxMemoryMB: 1024,
    detectSIMD: true,
    detectThreads: true,
  };

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (arg === '--samples' && args[i + 1]) {
      config.samples = parseInt(args[i + 1], 10);
      i++;
    } else if (arg === '--timeout' && args[i + 1]) {
      config.timeout = parseInt(args[i + 1], 10);
      i++;
    } else if (arg === '--max-memory' && args[i + 1]) {
      config.maxMemoryMB = parseInt(args[i + 1], 10);
      i++;
    } else if (arg === '--no-simd') {
      config.detectSIMD = false;
    } else if (arg === '--no-threads') {
      config.detectThreads = false;
    } else if (arg === '--help') {
      console.log(`
WASM Probe CLI - WebAssembly Capability Detection & Benchmarking

Usage: probe-wasm.mjs [options]

Options:
  --samples <n>       Number of benchmark samples (default: 100)
  --timeout <ms>      Timeout per operation in ms (default: 5000)
  --max-memory <mb>   Max memory for growth test in MB (default: 1024)
  --no-simd           Disable SIMD detection
  --no-threads        Disable threads detection
  --help              Show this help message

Example:
  node bin/probe-wasm.mjs --samples 50 --timeout 3000
      `);
      process.exit(0);
    }
  }

  console.log('🔍 KGC WASM Probe - Starting...\n');
  console.log('Configuration:', JSON.stringify(config, null, 2), '\n');

  try {
    const observations = await probeWasm(config);

    console.log(`✅ Probe completed successfully!\n`);
    console.log(`Total observations: ${observations.length}\n`);

    // Group observations by category
    const categories = {
      support: observations.filter(o => o.metric.startsWith('wasm.support')),
      compile: observations.filter(o => o.metric.startsWith('wasm.compile')),
      instantiate: observations.filter(o => o.metric.startsWith('wasm.instantiate')),
      call: observations.filter(o => o.metric.startsWith('wasm.call')),
      memory: observations.filter(o => o.metric.startsWith('wasm.memory')),
      environment: observations.filter(o => o.metric === 'wasm.environment'),
    };

    // Print summary
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('WASM CAPABILITY SUPPORT');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

    categories.support.forEach(obs => {
      const icon = obs.value === true ? '✅' : '❌';
      const name = obs.metric.replace('wasm.support.', '');
      console.log(`${icon} ${name.padEnd(20)} ${obs.value}`);
    });

    console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('PERFORMANCE BENCHMARKS');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

    // Compile time
    const compileMean = categories.compile.find(o => o.metric === 'wasm.compile.time.mean');
    if (compileMean) {
      console.log(`Compile Time (mean):     ${compileMean.value.toFixed(4)} ms`);
    }

    // Instantiate time
    const instantiateMean = categories.instantiate.find(o => o.metric === 'wasm.instantiate.time.mean');
    if (instantiateMean) {
      console.log(`Instantiate Time (mean): ${instantiateMean.value.toFixed(4)} ms`);
    }

    // Call overhead
    const callMean = categories.call.find(o => o.metric === 'wasm.call.overhead.mean');
    if (callMean) {
      console.log(`Call Overhead (mean):    ${callMean.value.toFixed(6)} ms`);
    }

    console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('MEMORY CHARACTERISTICS');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

    const initialPages = categories.memory.find(o => o.metric === 'wasm.memory.initial.pages');
    const maxPages = categories.memory.find(o => o.metric === 'wasm.memory.max.pages');

    if (initialPages) {
      console.log(`Initial Pages:  ${initialPages.value}`);
    }
    if (maxPages) {
      console.log(`Max Pages:      ${maxPages.value}`);
    }

    // Print errors if any
    const errors = observations.filter(o => o.status === 'error');
    if (errors.length > 0) {
      console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
      console.log('ERRORS');
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

      errors.forEach(obs => {
        console.log(`❌ ${obs.metric}: ${obs.error}`);
      });
    }

    // Print full JSON output for programmatic use
    if (process.env.JSON_OUTPUT === 'true') {
      console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
      console.log('JSON OUTPUT');
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
      console.log(JSON.stringify(observations, null, 2));
    }

    process.exit(0);
  } catch (error) {
    console.error('\n❌ Probe failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

main();
