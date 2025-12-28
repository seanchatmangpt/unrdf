#!/usr/bin/env node
/**
 * KGC Multiverse - 10k Universe CLI Tool
 * Command-line interface for running 10k universe benchmarks
 *
 * Usage:
 *   node --max-old-space-size=2048 cli-10k.mjs [options]
 *
 * @module @unrdf/kgc-multiverse/cli-10k
 */

import { performance } from 'node:perf_hooks';
import { createParallelExecutor } from './parallel-executor.mjs';
import { TaskType } from './worker-task.mjs';

/**
 * CLI Configuration
 */
const CLI_CONFIG = {
  universeCount: 10000,
  workerCount: 10,
  batchSize: 100,
  maxDurationMs: 120000,
  maxMemoryMB: 512,
  minThroughput: 83,
  verbose: false,
};

/**
 * ANSI color codes
 */
const colors = {
  reset: '\x1b[0m',
  bold: '\x1b[1m',
  dim: '\x1b[2m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
};

/**
 * Get current memory usage in MB
 */
function getMemoryMB() {
  return process.memoryUsage().heapUsed / (1024 * 1024);
}

/**
 * Format duration
 */
function formatDuration(ms) {
  if (ms < 1000) return `${ms.toFixed(0)}ms`;
  if (ms < 60000) return `${(ms / 1000).toFixed(1)}s`;
  const mins = Math.floor(ms / 60000);
  const secs = Math.floor((ms % 60000) / 1000);
  return `${mins}m ${secs}s`;
}

/**
 * Render animated progress bar
 */
function renderProgress(label, current, total, startTime, emoji = '🚀') {
  const percent = Math.floor((current / total) * 100);
  const filled = Math.floor(percent / 4);
  const empty = 25 - filled;

  // Animation characters
  const spinners = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];
  const spinner = spinners[Math.floor(Date.now() / 100) % spinners.length];

  const bar = `${colors.green}${'█'.repeat(filled)}${colors.dim}${'░'.repeat(empty)}${colors.reset}`;
  const elapsed = formatDuration(Date.now() - startTime);

  // Estimate remaining time
  const rate = current / ((Date.now() - startTime) / 1000);
  const remaining = rate > 0 ? formatDuration(((total - current) / rate) * 1000) : '...';

  const line = `${emoji} ${label.padEnd(25)} ${bar} ${percent.toString().padStart(3)}% ` +
    `(${current.toLocaleString()}/${total.toLocaleString()}) ` +
    `${colors.cyan}${elapsed}${colors.reset} elapsed, ~${remaining} remaining ${spinner}`;

  process.stdout.write(`\r\x1b[K${line}`);
}

/**
 * Print completion line
 */
function printComplete(label, count, duration, throughput, emoji = '✅') {
  const line = `${emoji} ${label.padEnd(25)} ${colors.green}COMPLETE${colors.reset} - ` +
    `${count.toLocaleString()} items in ${formatDuration(duration)} ` +
    `(${colors.cyan}${throughput.toFixed(1)}/sec${colors.reset})`;
  console.log(`\r\x1b[K${line}`);
}

/**
 * Run the 10k benchmark with nice CLI output
 */
async function run10kBenchmark() {
  console.log(`
${colors.bold}╔════════════════════════════════════════════════════════════════╗
║         ${colors.cyan}KGC Multiverse - 10k Universe CLI${colors.reset}${colors.bold}                   ║
╚════════════════════════════════════════════════════════════════╝${colors.reset}
`);

  console.log(`${colors.dim}Configuration:${colors.reset}`);
  console.log(`  Universes:     ${colors.bold}${CLI_CONFIG.universeCount.toLocaleString()}${colors.reset}`);
  console.log(`  Workers:       ${colors.bold}${CLI_CONFIG.workerCount}${colors.reset}`);
  console.log(`  Batch size:    ${colors.bold}${CLI_CONFIG.batchSize}${colors.reset}`);
  console.log(`  Target time:   ${colors.bold}<${CLI_CONFIG.maxDurationMs / 1000}s${colors.reset}`);
  console.log(`  Target memory: ${colors.bold}<${CLI_CONFIG.maxMemoryMB}MB${colors.reset}`);
  console.log('');

  const totalStartTime = performance.now();
  const results = {
    phases: [],
    errors: [],
  };

  // Initialize executor
  console.log(`${colors.yellow}⚡ Initializing worker pool...${colors.reset}`);
  const executor = createParallelExecutor({
    workerCount: CLI_CONFIG.workerCount,
    batchSize: CLI_CONFIG.batchSize,
  });

  try {
    await executor.initialize();
    console.log(`${colors.green}✓ Worker pool ready (${CLI_CONFIG.workerCount} threads)${colors.reset}\n`);

    // Phase 1: Create Universes
    {
      const count = CLI_CONFIG.universeCount;
      const startTime = Date.now();
      let created = 0;
      let peakMemory = 0;
      const universes = [];

      for await (const universe of executor.createUniverses(count, {
        batchSize: CLI_CONFIG.batchSize,
        onProgress: (current) => {
          created = current;
          const mem = getMemoryMB();
          if (mem > peakMemory) peakMemory = mem;
          if (current % 50 === 0 || current === count) {
            renderProgress('Creating universes', current, count, startTime, '🚀');
          }
        },
      })) {
        universes.push(universe);
      }

      const duration = Date.now() - startTime;
      const throughput = (created / duration) * 1000;
      printComplete('Creating universes', created, duration, throughput);

      results.universes = universes;
      results.phases.push({
        name: 'universe_creation',
        count: created,
        duration_ms: duration,
        throughput: throughput,
        peak_memory_mb: peakMemory,
      });
    }

    // Phase 2: Apply Morphisms
    {
      const universes = results.universes;
      const count = universes.length;
      const startTime = Date.now();
      let processed = 0;
      let peakMemory = 0;
      const morphResults = [];

      const morphismConfig = {
        id: 'Φ_cli_benchmark',
        type: 'SCHEMA',
        name: 'cli-transform',
      };

      for await (const result of executor.applyMorphismsParallel(universes, morphismConfig, {
        batchSize: CLI_CONFIG.batchSize,
        onProgress: (current) => {
          processed = current;
          const mem = getMemoryMB();
          if (mem > peakMemory) peakMemory = mem;
          if (current % 50 === 0 || current === count) {
            renderProgress('Applying morphisms', current, count, startTime, '⚙️');
          }
        },
      })) {
        morphResults.push(result);
      }

      const duration = Date.now() - startTime;
      const throughput = (processed / duration) * 1000;
      printComplete('Applying morphisms', processed, duration, throughput);

      results.morphResults = morphResults;
      results.phases.push({
        name: 'morphism_application',
        count: processed,
        duration_ms: duration,
        throughput: throughput,
        peak_memory_mb: peakMemory,
      });
    }

    // Phase 3: Generate Receipts
    {
      const operations = results.morphResults;
      const count = operations.length;
      const startTime = Date.now();
      let generated = 0;
      let peakMemory = 0;
      const receipts = [];

      for await (const receipt of executor.generateReceiptsParallel(operations, {
        batchSize: CLI_CONFIG.batchSize,
        operationType: 'morphism',
        onProgress: (current) => {
          generated = current;
          const mem = getMemoryMB();
          if (mem > peakMemory) peakMemory = mem;
          if (current % 50 === 0 || current === count) {
            renderProgress('Generating receipts', current, count, startTime, '🧾');
          }
        },
      })) {
        receipts.push(receipt);
      }

      const duration = Date.now() - startTime;
      const throughput = (generated / duration) * 1000;
      printComplete('Generating receipts', generated, duration, throughput);

      results.receipts = receipts;
      results.phases.push({
        name: 'receipt_generation',
        count: generated,
        duration_ms: duration,
        throughput: throughput,
        peak_memory_mb: peakMemory,
      });
    }

    // Phase 4: Freeze Universes
    {
      const universes = results.universes;
      const count = universes.length;
      const startTime = Date.now();
      let frozen = 0;
      let peakMemory = 0;

      for await (const _frozenUniverse of executor.freezeUniversesParallel(universes, {
        batchSize: CLI_CONFIG.batchSize,
        onProgress: (current) => {
          frozen = current;
          const mem = getMemoryMB();
          if (mem > peakMemory) peakMemory = mem;
          if (current % 50 === 0 || current === count) {
            renderProgress('Freezing universes', current, count, startTime, '❄️');
          }
        },
      })) {
        // Just count, don't store to save memory
      }

      const duration = Date.now() - startTime;
      const throughput = (frozen / duration) * 1000;
      printComplete('Freezing universes', frozen, duration, throughput);

      results.phases.push({
        name: 'universe_freezing',
        count: frozen,
        duration_ms: duration,
        throughput: throughput,
        peak_memory_mb: peakMemory,
      });
    }
  } catch (error) {
    console.error(`\n${colors.red}✗ Error: ${error.message}${colors.reset}`);
    results.errors.push(error.message);
  } finally {
    console.log(`\n${colors.yellow}⏳ Shutting down worker pool...${colors.reset}`);
    await executor.shutdown();
    console.log(`${colors.green}✓ Worker pool terminated${colors.reset}`);
  }

  // Calculate totals
  const totalDuration = performance.now() - totalStartTime;
  const totalOperations = results.phases.reduce((sum, p) => sum + p.count, 0);
  const peakMemory = Math.max(...results.phases.map((p) => p.peak_memory_mb));
  const avgThroughput = (CLI_CONFIG.universeCount / totalDuration) * 1000;

  // Print summary
  console.log(`
${colors.bold}╔════════════════════════════════════════════════════════════════╗
║                          SUMMARY                               ║
╚════════════════════════════════════════════════════════════════╝${colors.reset}
`);

  // Phase summary table
  console.log('Phase                    │ Count     │ Duration  │ Throughput  │ Memory');
  console.log('─'.repeat(75));

  for (const phase of results.phases) {
    const name = phase.name.replace(/_/g, ' ').padEnd(24);
    const count = phase.count.toLocaleString().padStart(9);
    const duration = formatDuration(phase.duration_ms).padStart(9);
    const throughput = `${phase.throughput.toFixed(0)}/s`.padStart(11);
    const memory = `${phase.peak_memory_mb.toFixed(0)}MB`.padStart(6);
    console.log(`${name} │ ${count} │ ${duration} │ ${throughput} │ ${memory}`);
  }

  console.log('─'.repeat(75));
  const totalName = 'TOTAL'.padEnd(24);
  const totalCount = totalOperations.toLocaleString().padStart(9);
  const totalDur = formatDuration(totalDuration).padStart(9);
  const totalThroughput = `${avgThroughput.toFixed(0)}/s`.padStart(11);
  const totalMem = `${peakMemory.toFixed(0)}MB`.padStart(6);
  console.log(`${colors.bold}${totalName} │ ${totalCount} │ ${totalDur} │ ${totalThroughput} │ ${totalMem}${colors.reset}`);

  // Pass/Fail checks
  console.log('');
  const timeOk = totalDuration < CLI_CONFIG.maxDurationMs;
  const memoryOk = peakMemory < CLI_CONFIG.maxMemoryMB;
  const throughputOk = avgThroughput >= CLI_CONFIG.minThroughput;
  const passed = timeOk && memoryOk && throughputOk && results.errors.length === 0;

  console.log(`${colors.bold}Checks:${colors.reset}`);
  console.log(`  Time:       ${timeOk ? colors.green + '✓ PASS' : colors.red + '✗ FAIL'}${colors.reset} ` +
    `(${formatDuration(totalDuration)} < ${CLI_CONFIG.maxDurationMs / 1000}s)`);
  console.log(`  Memory:     ${memoryOk ? colors.green + '✓ PASS' : colors.red + '✗ FAIL'}${colors.reset} ` +
    `(${peakMemory.toFixed(0)}MB < ${CLI_CONFIG.maxMemoryMB}MB)`);
  console.log(`  Throughput: ${throughputOk ? colors.green + '✓ PASS' : colors.red + '✗ FAIL'}${colors.reset} ` +
    `(${avgThroughput.toFixed(1)}/s >= ${CLI_CONFIG.minThroughput}/s)`);

  if (results.errors.length > 0) {
    console.log(`  Errors:     ${colors.red}✗ ${results.errors.length} error(s)${colors.reset}`);
  }

  console.log('');
  if (passed) {
    console.log(`${colors.green}${colors.bold}════════════════════════════════════════════════════════════════`);
    console.log(`                     ✅ BENCHMARK PASSED                        `);
    console.log(`════════════════════════════════════════════════════════════════${colors.reset}`);
  } else {
    console.log(`${colors.red}${colors.bold}════════════════════════════════════════════════════════════════`);
    console.log(`                     ❌ BENCHMARK FAILED                        `);
    console.log(`════════════════════════════════════════════════════════════════${colors.reset}`);
  }

  // Output JSON result
  const jsonResult = {
    test_run: '10k_big_bangs',
    timestamp: new Date().toISOString(),
    config: {
      universeCount: CLI_CONFIG.universeCount,
      workerCount: CLI_CONFIG.workerCount,
      batchSize: CLI_CONFIG.batchSize,
    },
    phases: results.phases,
    total_duration_ms: Math.round(totalDuration),
    throughput_per_sec: Math.round(avgThroughput),
    memory_peak_mb: Math.round(peakMemory),
    passed,
    errors: results.errors,
  };

  if (CLI_CONFIG.verbose) {
    console.log(`\n${colors.dim}JSON Result:${colors.reset}`);
    console.log(JSON.stringify(jsonResult, null, 2));
  }

  return jsonResult;
}

/**
 * Parse command line arguments
 */
function parseArgs() {
  const args = process.argv.slice(2);

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];

    if ((arg === '--count' || arg === '-c') && args[i + 1]) {
      CLI_CONFIG.universeCount = parseInt(args[++i], 10);
    } else if ((arg === '--workers' || arg === '-w') && args[i + 1]) {
      CLI_CONFIG.workerCount = parseInt(args[++i], 10);
    } else if ((arg === '--batch' || arg === '-b') && args[i + 1]) {
      CLI_CONFIG.batchSize = parseInt(args[++i], 10);
    } else if (arg === '--verbose' || arg === '-v') {
      CLI_CONFIG.verbose = true;
    } else if (arg === '--help' || arg === '-h') {
      console.log(`
${colors.bold}KGC Multiverse - 10k Universe CLI${colors.reset}

${colors.dim}Usage:${colors.reset}
  node --max-old-space-size=2048 cli-10k.mjs [options]

${colors.dim}Options:${colors.reset}
  -c, --count <n>     Number of universes to create (default: 10000)
  -w, --workers <n>   Number of worker threads (default: 10)
  -b, --batch <n>     Batch size for operations (default: 100)
  -v, --verbose       Show JSON output
  -h, --help          Show this help message

${colors.dim}Examples:${colors.reset}
  node cli-10k.mjs                     # Run with defaults
  node cli-10k.mjs -c 1000 -w 8        # 1000 universes, 8 workers
  node cli-10k.mjs --verbose           # Show JSON output

${colors.dim}Performance Targets:${colors.reset}
  Time:       < 120s
  Memory:     < 512MB
  Throughput: >= 83 ops/sec
`);
      process.exit(0);
    }
  }
}

// Main entry point
parseArgs();
run10kBenchmark()
  .then((result) => {
    process.exit(result.passed ? 0 : 1);
  })
  .catch((error) => {
    console.error(`${colors.red}Fatal error: ${error.message}${colors.reset}`);
    process.exit(1);
  });

export { run10kBenchmark, CLI_CONFIG };
