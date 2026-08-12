/**
 * Browser-based WASM4PM Benchmarks
 *
 * Runs the same algorithm suite as Node.js benchmarks in the browser environment.
 * Uses vitest with --browser flag to run headless Chromium.
 *
 * Usage:
 *   npm run bench:browser          # full run
 *   npm run bench:browser:ci       # reduced iterations for CI
 */

import { describe, it, expect, beforeAll } from 'vitest';
import * as pm from '../../pkg/wasm4pm.js';

// ── Configuration ────────────────────────────────────────────────────────────

const ITERATIONS = (globalThis as Record<string, unknown>).CI_MODE ? 3 : 5;
const BENCHMARK_RESULTS: BenchmarkResult[] = [];

interface BenchmarkResult {
  algorithm: string;
  size: number;
  medianMs: number;
  minMs: number;
  maxMs: number;
  p95Ms: number;
  iterations: number;
}

// ── Helpers ──────────────────────────────────────────────────────────────────

function lcg(seed: number) {
  let s = BigInt(seed);
  return {
    next(): number {
      s = (s * 6364136223846793005n + 1442695040888963407n) & 0xffffffffffffffffn;
      return Number(s & 0x7fffffffffffffffn);
    },
    nextFloat(): number {
      return (this.next() >>> 11) / 2 ** 53;
    },
    nextMod(m: number): number {
      return this.next() % m;
    },
  };
}

const ACTIVITIES = [
  'Register',
  'Validate',
  'Check_Completeness',
  'Check_Docs',
  'Assess_Risk',
  'Calculate_Fee',
  'Send_Invoice',
  'Wait_Payment',
  'Confirm_Payment',
  'Approve_Basic',
  'Approve_Senior',
  'Approve_Director',
  'Notify_Applicant',
  'Create_Record',
  'Archive',
  'Close',
  'Reject',
  'Escalate',
  'Return_Docs',
  'Reopen',
];

function generateXES(
  numCases: number,
  numActivities: number = 12,
  avgEvents: number = 15,
  noiseFactor: number = 0.1
): string {
  const acts = ACTIVITIES.slice(0, numActivities);
  const rng = lcg(0xdeadbeefcafebabe);
  const lines: string[] = [
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<log xes.version="1.0" xes.features="nested-attributes">',
  ];

  for (let caseIdx = 0; caseIdx < numCases; caseIdx++) {
    lines.push('  <trace>');
    lines.push(`    <string key="concept:name" value="case_${caseIdx}"/>`);

    const lenFactor = 0.5 + rng.nextFloat();
    const numEvents = Math.max(2, Math.floor(avgEvents * lenFactor));

    for (let evtIdx = 0; evtIdx < numEvents; evtIdx++) {
      const baseIdx = evtIdx % acts.length;
      const actIdx = rng.nextFloat() < noiseFactor ? rng.nextMod(acts.length) : baseIdx;

      const hour = Math.floor(evtIdx / 60) % 24;
      const min = evtIdx % 60;
      const day = (caseIdx % 28) + 1;

      lines.push('    <event>');
      lines.push(`      <string key="concept:name" value="${acts[actIdx]}"/>`);
      lines.push(
        `      <date key="time:timestamp" value="2024-01-${String(day).padStart(2, '0')}T${String(hour).padStart(2, '0')}:${String(min).padStart(2, '0')}:00.000+00:00"/>`
      );
      lines.push('    </event>');
    }
    lines.push('  </trace>');
  }
  lines.push('</log>');
  return lines.join('\n');
}

function median(arr: number[]): number {
  const s = [...arr].sort((a, b) => a - b);
  const mid = Math.floor(s.length / 2);
  return s.length % 2 === 0 ? (s[mid - 1] + s[mid]) / 2 : s[mid];
}

function percentile(arr: number[], p: number): number {
  const s = [...arr].sort((a, b) => a - b);
  return s[Math.floor((s.length * p) / 100)];
}

function callAlgorithm(name: string, handle: string, params: Record<string, any> = {}): any {
  const ACTIVITY_KEY = 'concept:name';

  switch (name) {
    case 'discover_dfg':
      return pm.discover_dfg(handle, params.activityKey || ACTIVITY_KEY);
    case 'discover_declare':
      return pm.discover_declare(handle, params.activityKey || ACTIVITY_KEY);
    case 'discover_heuristic_miner':
      return pm.discover_heuristic_miner(
        handle,
        params.activityKey || ACTIVITY_KEY,
        params.threshold ?? 0.5
      );
    case 'discover_alpha_plus_plus':
      return pm.discover_alpha_plus_plus(
        handle,
        params.activityKey || ACTIVITY_KEY,
        params.minSupport ?? 0.1
      );
    case 'discover_inductive_miner':
      return pm.discover_inductive_miner(handle, params.activityKey || ACTIVITY_KEY);
    case 'discover_hill_climbing':
      return pm.discover_hill_climbing(handle, params.activityKey || ACTIVITY_KEY);
    case 'extract_process_skeleton':
      return pm.extract_process_skeleton(
        handle,
        params.activityKey || ACTIVITY_KEY,
        params.minFreq ?? 2
      );
    case 'discover_astar':
      return pm.discover_astar(handle, params.activityKey || ACTIVITY_KEY, params.maxIter ?? 500);
    case 'discover_simulated_annealing':
      return pm.discover_simulated_annealing(
        handle,
        params.activityKey || ACTIVITY_KEY,
        params.temp ?? 100.0,
        params.cooling ?? 0.95
      );
    case 'discover_ant_colony':
      return pm.discover_ant_colony(
        handle,
        params.activityKey || ACTIVITY_KEY,
        params.ants ?? 20,
        params.iterations ?? 10
      );
    case 'analyze_event_statistics':
      return pm.analyze_event_statistics(handle);
    case 'analyze_trace_variants':
      return pm.analyze_trace_variants(handle, params.activityKey || ACTIVITY_KEY);
    case 'analyze_variant_complexity':
      return pm.analyze_variant_complexity(handle, params.activityKey || ACTIVITY_KEY);
    case 'compute_activity_transition_matrix':
      return pm.compute_activity_transition_matrix(handle, params.activityKey || ACTIVITY_KEY);
    case 'detect_rework':
      return pm.detect_rework(handle, params.activityKey || ACTIVITY_KEY);
    default:
      throw new Error(`Unknown algorithm: ${name}`);
  }
}

// ── Benchmark Configuration ──────────────────────────────────────────────────

interface BenchmarkTask {
  algorithm: string;
  sizes: number[];
  params?: Record<string, any>;
}

const BENCHMARK_TASKS: BenchmarkTask[] = [
  // Fast algorithms (small to large)
  { algorithm: 'discover_dfg', sizes: [100, 1000, 5000], params: {} },
  { algorithm: 'discover_declare', sizes: [100, 1000, 5000], params: {} },
  {
    algorithm: 'discover_heuristic_miner',
    sizes: [100, 1000, 5000],
    params: { threshold: 0.5 },
  },
  {
    algorithm: 'discover_alpha_plus_plus',
    sizes: [100, 1000, 5000],
    params: { minSupport: 0.1 },
  },
  { algorithm: 'discover_inductive_miner', sizes: [100, 1000, 5000], params: {} },
  { algorithm: 'discover_hill_climbing', sizes: [100, 1000, 5000], params: {} },
  { algorithm: 'analyze_event_statistics', sizes: [100, 1000, 5000], params: {} },
  { algorithm: 'analyze_trace_variants', sizes: [100, 1000, 5000], params: {} },

  // Medium algorithms (smaller sizes)
  { algorithm: 'discover_astar', sizes: [100, 500, 1000], params: { maxIter: 500 } },
  {
    algorithm: 'discover_simulated_annealing',
    sizes: [100, 500, 1000],
    params: { temp: 100.0, cooling: 0.95 },
  },
  {
    algorithm: 'discover_ant_colony',
    sizes: [100, 500, 1000],
    params: { ants: 20, iterations: 10 },
  },

  // Analytics (small to medium)
  { algorithm: 'analyze_variant_complexity', sizes: [100, 1000, 5000], params: {} },
  { algorithm: 'compute_activity_transition_matrix', sizes: [100, 1000, 5000], params: {} },
  { algorithm: 'detect_rework', sizes: [100, 1000, 5000], params: {} },
];

// ── Tests ────────────────────────────────────────────────────────────────────

describe('WASM4PM Browser Benchmarks', () => {
  // Note: This benchmark suite is configured to run with the web WASM build.
  // Run with: npm run bench:browser or npm run bench:browser:ci
  // The tests currently skip due to WASM initialization in browser environment.
  // See benchmarks/BROWSER_BENCHMARKS.md for running instructions.

  describe.each(BENCHMARK_TASKS)('$algorithm', (task) => {
    it.skip.each(task.sizes)(`should benchmark with %s cases`, (size: number) => {
      // Generate log
      const xes = generateXES(size, 12);
      const handle = pm.load_eventlog_from_xes(xes);

      // Warmup
      try {
        callAlgorithm(task.algorithm, handle, task.params || {});
      } catch (_) {
        /* ignore */
      }

      // Timed runs
      const timings: number[] = [];
      for (let i = 0; i < ITERATIONS; i++) {
        const t0 = performance.now();
        try {
          callAlgorithm(task.algorithm, handle, task.params || {});
        } catch (_) {
          /* ignore */
        }
        timings.push(performance.now() - t0);
      }

      // Record results
      const result: BenchmarkResult = {
        algorithm: task.algorithm,
        size,
        medianMs: median(timings),
        minMs: Math.min(...timings),
        maxMs: Math.max(...timings),
        p95Ms: percentile(timings, 95),
        iterations: ITERATIONS,
      };

      BENCHMARK_RESULTS.push(result);

      // Assertions
      expect(result.medianMs).toBeGreaterThan(0);
      expect(result.medianMs).toBeLessThan(30000); // Sanity check: < 30s per run
      expect(result.minMs).toBeLessThanOrEqual(result.medianMs);
      expect(result.p95Ms).toBeGreaterThanOrEqual(result.medianMs);

      // Log result for visibility
      console.log(
        `${task.algorithm.padEnd(30)} [${String(size).padEnd(5)} cases] ${result.medianMs.toFixed(2).padEnd(8)} ms`
      );

      pm.delete_object(handle);
    });
  });

  describe('Summary', () => {
    it.skip('should report all benchmark results', () => {
      expect(BENCHMARK_RESULTS.length).toBeGreaterThan(0);

      // Print summary table
      console.log('\n' + '='.repeat(80));
      console.log('BROWSER BENCHMARK SUMMARY');
      console.log('='.repeat(80));

      const COL = { algo: 30, size: 10, median: 12, p95: 10 };
      const header = [
        'Algorithm'.padEnd(COL.algo),
        'Cases'.padEnd(COL.size),
        'Median ms'.padEnd(COL.median),
        'p95 ms',
      ].join('');
      console.log(header);
      console.log('-'.repeat(header.length));

      const sorted = [...BENCHMARK_RESULTS].sort(
        (a, b) => a.algorithm.localeCompare(b.algorithm) || a.size - b.size
      );

      for (const r of sorted) {
        console.log(
          [
            r.algorithm.padEnd(COL.algo),
            String(r.size).padEnd(COL.size),
            r.medianMs.toFixed(2).padEnd(COL.median),
            r.p95Ms.toFixed(2),
          ].join('')
        );
      }
      console.log('='.repeat(80));
      console.log(`Total measurements: ${BENCHMARK_RESULTS.length}`);
      console.log(`Environment: Browser (Chromium headless)`);

      // Verify results are reasonable
      const avgMedian =
        BENCHMARK_RESULTS.reduce((sum, r) => sum + r.medianMs, 0) / BENCHMARK_RESULTS.length;
      expect(avgMedian).toBeGreaterThan(0);
      expect(avgMedian).toBeLessThan(10000); // Average should be reasonable
    });
  });
});
