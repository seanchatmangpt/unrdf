#!/usr/bin/env node
/**
 * Performance Profiling Tool
 * CPU and memory profiling with flame graph generation
 *
 * Usage:
 *   node scripts/profile.mjs cpu <benchmark>     # CPU profile
 *   node scripts/profile.mjs mem <benchmark>     # Memory profile
 *   node scripts/profile.mjs both <benchmark>    # Both CPU and memory
 *
 * Examples:
 *   node scripts/profile.mjs cpu hook-execution
 *   node scripts/profile.mjs mem receipt-generation
 *   node scripts/profile.mjs both sparql-query
 */

import { spawn } from 'child_process';
import { writeFileSync, mkdirSync, existsSync } from 'fs';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';
import v8Profiler from 'v8-profiler-next';
import { performance } from 'perf_hooks';

const __dirname = dirname(fileURLToPath(import.meta.url));
const projectRoot = resolve(__dirname, '..');
const profilesDir = resolve(projectRoot, 'profiles');

// Ensure profiles directory exists
if (!existsSync(profilesDir)) {
  mkdirSync(profilesDir, { recursive: true });
}

/**
 * Parse command line arguments
 */
function parseArgs() {
  const args = process.argv.slice(2);
  return {
    mode: args[0] || 'cpu', // cpu, mem, both
    benchmark: args[1] || 'hook-execution',
  };
}

/**
 * CPU profiling using V8 profiler
 */
async function profileCPU(benchmarkName) {
  console.log(`Starting CPU profiling for ${benchmarkName}...`);

  const benchmarkPath = resolve(projectRoot, 'benchmarks', `${benchmarkName}-bench.mjs`);

  if (!existsSync(benchmarkPath)) {
    throw new Error(`Benchmark not found: ${benchmarkPath}`);
  }

  return new Promise((resolve, reject) => {
    const timestamp = Date.now();
    const cpuprofileFile = resolve(profilesDir, `${benchmarkName}-${timestamp}.cpuprofile`);

    // Use --cpu-prof flag for Node.js built-in profiling
    const proc = spawn('node', [
      '--cpu-prof',
      `--cpu-prof-dir=${profilesDir}`,
      `--cpu-prof-name=${benchmarkName}-${timestamp}.cpuprofile`,
      benchmarkPath
    ], {
      cwd: projectRoot,
      env: { ...process.env, NODE_ENV: 'production' },
      stdio: 'inherit',
    });

    proc.on('close', (code) => {
      if (code !== 0) {
        reject(new Error(`Benchmark failed with code ${code}`));
        return;
      }

      console.log(`\n✅ CPU profile saved: ${cpuprofileFile}`);
      console.log(`\nTo visualize:`);
      console.log(`  1. Open Chrome DevTools`);
      console.log(`  2. Go to "Performance" tab`);
      console.log(`  3. Click "Load Profile"`);
      console.log(`  4. Select: ${cpuprofileFile}`);
      console.log(`\nOr use speedscope: npx speedscope ${cpuprofileFile}`);

      resolve({ cpuprofile: cpuprofileFile });
    });

    proc.on('error', reject);
  });
}

/**
 * Memory profiling using heap snapshots
 */
async function profileMemory(benchmarkName) {
  console.log(`Starting memory profiling for ${benchmarkName}...`);

  const benchmarkPath = resolve(projectRoot, 'benchmarks', `${benchmarkName}-bench.mjs`);

  if (!existsSync(benchmarkPath)) {
    throw new Error(`Benchmark not found: ${benchmarkPath}`);
  }

  return new Promise((resolve, reject) => {
    const timestamp = Date.now();
    const heapsnapshotFile = resolve(profilesDir, `${benchmarkName}-${timestamp}.heapsnapshot`);

    // Use --heap-prof flag for Node.js built-in memory profiling
    const proc = spawn('node', [
      '--heap-prof',
      `--heap-prof-dir=${profilesDir}`,
      `--heap-prof-name=${benchmarkName}-${timestamp}.heapprofile`,
      '--expose-gc',
      benchmarkPath
    ], {
      cwd: projectRoot,
      env: { ...process.env, NODE_ENV: 'production' },
      stdio: 'inherit',
    });

    proc.on('close', (code) => {
      if (code !== 0) {
        reject(new Error(`Benchmark failed with code ${code}`));
        return;
      }

      const heapprofileFile = resolve(profilesDir, `${benchmarkName}-${timestamp}.heapprofile`);

      console.log(`\n✅ Memory profile saved: ${heapprofileFile}`);
      console.log(`\nTo visualize:`);
      console.log(`  1. Open Chrome DevTools`);
      console.log(`  2. Go to "Memory" tab`);
      console.log(`  3. Click "Load" (bottom left)`);
      console.log(`  4. Select: ${heapprofileFile}`);

      resolve({ heapprofile: heapprofileFile });
    });

    proc.on('error', reject);
  });
}

/**
 * Continuous memory monitoring
 */
async function monitorMemory(benchmarkName, duration = 30000) {
  console.log(`Monitoring memory for ${benchmarkName} (${duration}ms)...`);

  const benchmarkPath = resolve(projectRoot, 'benchmarks', `${benchmarkName}-bench.mjs`);

  if (!existsSync(benchmarkPath)) {
    throw new Error(`Benchmark not found: ${benchmarkPath}`);
  }

  const samples = [];
  const startTime = Date.now();

  return new Promise((resolve, reject) => {
    const proc = spawn('node', [benchmarkPath], {
      cwd: projectRoot,
      env: { ...process.env, NODE_ENV: 'production' },
      stdio: ['inherit', 'inherit', 'inherit'],
    });

    // Sample memory every 100ms
    const interval = setInterval(() => {
      const usage = process.memoryUsage();
      samples.push({
        timestamp: Date.now() - startTime,
        rss: usage.rss,
        heapTotal: usage.heapTotal,
        heapUsed: usage.heapUsed,
        external: usage.external,
      });
    }, 100);

    proc.on('close', (code) => {
      clearInterval(interval);

      if (code !== 0) {
        reject(new Error(`Benchmark failed with code ${code}`));
        return;
      }

      // Analyze memory samples
      const analysis = analyzeMemorySamples(samples);

      const timestamp = Date.now();
      const reportFile = resolve(profilesDir, `${benchmarkName}-memory-${timestamp}.json`);

      writeFileSync(reportFile, JSON.stringify({
        benchmark: benchmarkName,
        duration: Date.now() - startTime,
        samples,
        analysis,
      }, null, 2));

      console.log(`\n✅ Memory monitoring report: ${reportFile}`);
      console.log(`\nMemory Analysis:`);
      console.log(`  Peak RSS: ${(analysis.peakRSS / 1024 / 1024).toFixed(2)} MB`);
      console.log(`  Peak Heap Used: ${(analysis.peakHeapUsed / 1024 / 1024).toFixed(2)} MB`);
      console.log(`  Growth Rate: ${(analysis.growthRate / 1024 / 1024).toFixed(2)} MB/s`);
      console.log(`  Potential Leak: ${analysis.potentialLeak ? 'YES ⚠️' : 'NO'}`);

      resolve({ report: reportFile, analysis });
    });

    proc.on('error', (error) => {
      clearInterval(interval);
      reject(error);
    });
  });
}

/**
 * Analyze memory samples for leaks and trends
 */
function analyzeMemorySamples(samples) {
  if (samples.length === 0) {
    return null;
  }

  const peakRSS = Math.max(...samples.map(s => s.rss));
  const peakHeapUsed = Math.max(...samples.map(s => s.heapUsed));
  const peakHeapTotal = Math.max(...samples.map(s => s.heapTotal));

  // Calculate growth rate (linear regression)
  const n = samples.length;
  const sumX = samples.reduce((sum, s, i) => sum + i, 0);
  const sumY = samples.reduce((sum, s) => sum + s.heapUsed, 0);
  const sumXY = samples.reduce((sum, s, i) => sum + i * s.heapUsed, 0);
  const sumX2 = samples.reduce((sum, s, i) => sum + i * i, 0);

  const slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX);
  const growthRate = slope * 10; // per second (samples are 100ms apart)

  // Detect potential memory leak (sustained growth >1MB/s)
  const potentialLeak = growthRate > 1024 * 1024;

  return {
    peakRSS,
    peakHeapUsed,
    peakHeapTotal,
    growthRate,
    potentialLeak,
    sampleCount: n,
  };
}

/**
 * Generate flame graph from CPU profile
 */
async function generateFlameGraph(cpuprofileFile) {
  console.log('\nGenerating flame graph...');

  // Check if speedscope is available
  return new Promise((resolve, reject) => {
    const proc = spawn('npx', ['speedscope', cpuprofileFile], {
      stdio: 'inherit',
    });

    proc.on('close', (code) => {
      if (code === 0) {
        console.log('✅ Flame graph opened in browser');
        resolve();
      } else {
        console.log('ℹ️  Install speedscope: npm install -g speedscope');
        resolve(); // Don't fail if speedscope not available
      }
    });

    proc.on('error', (error) => {
      console.log('ℹ️  Speedscope not available, skipping flame graph');
      resolve(); // Don't fail
    });
  });
}

/**
 * Main execution
 */
async function main() {
  const opts = parseArgs();

  console.log('='.repeat(70));
  console.log('PERFORMANCE PROFILING');
  console.log('='.repeat(70));
  console.log(`Mode: ${opts.mode}`);
  console.log(`Benchmark: ${opts.benchmark}`);
  console.log(`Profiles directory: ${profilesDir}`);
  console.log('');

  try {
    if (opts.mode === 'cpu' || opts.mode === 'both') {
      const cpuResult = await profileCPU(opts.benchmark);

      // Optionally generate flame graph
      if (cpuResult.cpuprofile) {
        await generateFlameGraph(cpuResult.cpuprofile);
      }
    }

    if (opts.mode === 'mem' || opts.mode === 'both') {
      await profileMemory(opts.benchmark);

      // Also run continuous monitoring
      console.log('\n');
      await monitorMemory(opts.benchmark);
    }

    console.log('\n' + '='.repeat(70));
    console.log('Profiling complete!');
    console.log('='.repeat(70));
  } catch (error) {
    console.error('Profiling failed:', error);
    process.exit(1);
  }
}

main().catch((error) => {
  console.error('Fatal error:', error);
  process.exit(1);
});
