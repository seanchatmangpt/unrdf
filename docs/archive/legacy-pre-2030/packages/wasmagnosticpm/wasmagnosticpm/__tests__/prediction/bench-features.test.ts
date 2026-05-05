/**
 * Feature Extraction Benchmarks
 * Perspective: "What describes this case?" — Van der Aalst
 */

import { describe, it, expect, afterAll } from 'vitest';
import {
  readXes,
  countTraces,
  printTable,
  BenchRow,
  SAMPLE_XES,
  BPI_XES,
} from './bench-helpers.js';

const SAMPLE = readXes(SAMPLE_XES);
const SAMPLE_TRACES = countTraces(SAMPLE);
let BPI: string, BPI_TRACES: number;
try {
  BPI = readXes(BPI_XES);
  BPI_TRACES = countTraces(BPI);
} catch {
  BPI = '';
  BPI_TRACES = 0;
}

const rows: BenchRow[] = [];
afterAll(() => printTable(rows));

async function loadWasm() {
  const w = await import('../../pkg/wasm4pm.js');
  w.init();
  return w;
}

describe('extract_prefix_features_wasm', () => {
  it('4-activity prefix — correct feature values', async () => {
    const wasm = await loadWasm();
    const prefix = ['Request', 'Review', 'Request', 'Approve'];
    const t = performance.now();
    const result = JSON.parse(wasm.extract_prefix_features_wasm(JSON.stringify(prefix)));
    const dur = Number((performance.now() - t).toFixed(3));
    expect(result.length).toBe(4);
    expect(result.unique_activities).toBe(3);
    expect(result.rework_count).toBe(0); // no consecutive repeats
    expect(result.last_activity).toBe('Approve');
    expect(result.activity_frequency_entropy).toBeGreaterThan(0);
    rows.push({
      algorithm: 'extract_prefix_features',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: dur,
      note: `unique=${result.unique_activities}`,
    });
  });

  it('10 000 calls latency', async () => {
    const wasm = await loadWasm();
    const prefix = JSON.stringify(['Request', 'Review', 'Approve']);
    const t = performance.now();
    for (let i = 0; i < 10_000; i++) wasm.extract_prefix_features_wasm(prefix);
    const perCall = Number(((performance.now() - t) / 10_000).toFixed(5));
    rows.push({
      algorithm: 'extract_prefix_features(10k)',
      dataset: 'synthetic',
      traces: 0,
      durationMs: perCall,
      note: 'ms/call',
    });
    expect(perCall).toBeLessThan(1);
  });
});

describe('compute_rework_score', () => {
  it('three consecutive repeats', async () => {
    const wasm = await loadWasm();
    const trace = ['Request', 'Review', 'Review', 'Approve', 'Approve', 'Approve', 'Complete'];
    const t = performance.now();
    const result = JSON.parse(wasm.compute_rework_score(JSON.stringify(trace)));
    const dur = Number((performance.now() - t).toFixed(3));
    expect(result.rework_count).toBe(3);
    expect(result.rework_ratio).toBeGreaterThan(0);
    rows.push({
      algorithm: 'compute_rework_score',
      dataset: 'synthetic',
      traces: 0,
      durationMs: dur,
      note: `rework=${result.rework_count}`,
    });
  });

  it('no rework — score=0', async () => {
    const wasm = await loadWasm();
    const trace = ['Request', 'Review', 'Approve', 'Complete'];
    const result = JSON.parse(wasm.compute_rework_score(JSON.stringify(trace)));
    expect(result.rework_count).toBe(0);
    expect(result.repeated_pairs).toHaveLength(0);
  });
});

describe('build_transition_probabilities', () => {
  it('sample — probabilities sum to 1 per source', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const t = performance.now();
    const result = JSON.parse(wasm.build_transition_probabilities(log, 'concept:name'));
    const dur = Number((performance.now() - t).toFixed(3));
    expect(result.edges.length).toBeGreaterThan(0);
    // Each source's outgoing probabilities sum to ~1
    const bySource = new Map<string, number>();
    for (const e of result.edges) bySource.set(e.from, (bySource.get(e.from) ?? 0) + e.probability);
    for (const [src, total] of bySource)
      expect(total, `from '${src}': sum=${total}`).toBeCloseTo(1.0, 1);
    rows.push({
      algorithm: 'build_transition_probabilities',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: dur,
      note: `${result.edges.length} edges`,
    });
  });

  it('BPI 2020 — large graph', async () => {
    if (!BPI) return;
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(BPI);
    const t = performance.now();
    const result = JSON.parse(wasm.build_transition_probabilities(log, 'concept:name'));
    const dur = Number((performance.now() - t).toFixed(3));
    expect(result.edges.length).toBeGreaterThan(10);
    rows.push({
      algorithm: 'build_transition_probabilities',
      dataset: 'BPI2020',
      traces: BPI_TRACES,
      durationMs: dur,
      note: `${result.edges.length} edges, ${result.activities.length} activities`,
    });
  });
});
