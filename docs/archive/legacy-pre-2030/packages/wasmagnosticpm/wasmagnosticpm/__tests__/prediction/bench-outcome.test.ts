/**
 * Outcome Prediction Benchmarks
 * Perspective: "Does this case complete normally?" — Van der Aalst
 *
 * Algorithms:
 *   score_anomaly             — DFG-based anomaly score [0,1]
 *   compute_boundary_coverage — fraction of matching traces that complete normally
 *   compute_trace_likelihood  — structured log-likelihood { ll, normalized }
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

// ─── score_anomaly ────────────────────────────────────────────────────────────

describe('score_anomaly', () => {
  it('sample — normal trace scores low', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const dfg = wasm.discover_dfg_handle(log, 'concept:name');
    const t = performance.now();
    const result = JSON.parse(
      wasm.score_anomaly(dfg, JSON.stringify(['Request', 'Review', 'Approve', 'Complete']))
    );
    const dur = Number((performance.now() - t).toFixed(3));
    expect(result.score).toBeGreaterThanOrEqual(0);
    expect(result.score).toBeLessThanOrEqual(1);
    expect(result).toHaveProperty('is_anomalous');
    rows.push({
      algorithm: 'score_anomaly',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: dur,
      note: `score=${result.score?.toFixed(3)} anomalous=${result.is_anomalous}`,
    });
  });

  it('sample — all-missing-edges trace is anomalous', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const dfg = wasm.discover_dfg_handle(log, 'concept:name');
    const result = JSON.parse(wasm.score_anomaly(dfg, JSON.stringify(['ZZZ_X', 'ZZZ_Y', 'ZZZ_Z'])));
    expect(result.is_anomalous).toBe(true);
    expect(result.score).toBeGreaterThan(0.7);
  });

  it('sample — 1 000 calls latency', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const dfg = wasm.discover_dfg_handle(log, 'concept:name');
    const trace = JSON.stringify(['Request', 'Review', 'Approve', 'Complete']);
    const t = performance.now();
    for (let i = 0; i < 1000; i++) wasm.score_anomaly(dfg, trace);
    const perCall = Number(((performance.now() - t) / 1000).toFixed(4));
    rows.push({
      algorithm: 'score_anomaly(1k)',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: perCall,
      note: 'ms/call',
    });
    expect(perCall).toBeLessThan(5);
  });

  it('BPI 2020 — known good sequence scores low', async () => {
    if (!BPI) return;
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(BPI);
    const dfg = wasm.discover_dfg_handle(log, 'concept:name');
    const prefix = JSON.stringify([
      'Declaration SUBMITTED by EMPLOYEE',
      'Declaration APPROVED by ADMINISTRATION',
    ]);
    const t = performance.now();
    const result = JSON.parse(wasm.score_anomaly(dfg, prefix));
    const dur = Number((performance.now() - t).toFixed(3));
    rows.push({
      algorithm: 'score_anomaly',
      dataset: 'BPI2020',
      traces: BPI_TRACES,
      durationMs: dur,
      note: `score=${result.score?.toFixed(3)}`,
    });
    expect(result.score).toBeGreaterThanOrEqual(0);
    expect(result.score).toBeLessThanOrEqual(1);
  });
});

// ─── compute_boundary_coverage ───────────────────────────────────────────────

describe('compute_boundary_coverage', () => {
  it('sample — prefix [Request, Review]', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const t = performance.now();
    const result = JSON.parse(
      wasm.compute_boundary_coverage(log, JSON.stringify(['Request', 'Review']), 'concept:name')
    );
    const dur = Number((performance.now() - t).toFixed(3));
    expect(result.coverage).toBeGreaterThanOrEqual(0);
    expect(result.coverage).toBeLessThanOrEqual(1);
    rows.push({
      algorithm: 'compute_boundary_coverage',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: dur,
      note: `coverage=${result.coverage?.toFixed(3)}`,
    });
  });

  it('sample — empty prefix returns full coverage', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const result = JSON.parse(
      wasm.compute_boundary_coverage(log, JSON.stringify([]), 'concept:name')
    );
    expect(result).toHaveProperty('coverage');
    expect(result).toHaveProperty('matching_traces');
  });

  it('sample — prefix with no matching traces returns 0', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const result = JSON.parse(
      wasm.compute_boundary_coverage(log, JSON.stringify(['ZZZ_NONEXISTENT']), 'concept:name')
    );
    expect(result.coverage).toBe(0);
    expect(result.matching_traces).toBe(0);
  });

  it('BPI 2020 — single-activity prefix', async () => {
    if (!BPI) return;
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(BPI);
    const t = performance.now();
    const result = JSON.parse(
      wasm.compute_boundary_coverage(
        log,
        JSON.stringify(['Declaration SUBMITTED by EMPLOYEE']),
        'concept:name'
      )
    );
    const dur = Number((performance.now() - t).toFixed(3));
    rows.push({
      algorithm: 'compute_boundary_coverage',
      dataset: 'BPI2020',
      traces: BPI_TRACES,
      durationMs: dur,
      note: `coverage=${result.coverage?.toFixed(3)} matching=${result.matching_traces}`,
    });
    expect(result.coverage).toBeGreaterThan(0);
  });
});

describe('compute_trace_likelihood', () => {
  it('sample — complete normal trace has negative ll', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const ngram = wasm.build_ngram_predictor(log, 'concept:name', 2);
    const trace = ['Request', 'Review', 'Approve', 'Complete'];
    const t = performance.now();
    const result = JSON.parse(wasm.compute_trace_likelihood(ngram, JSON.stringify(trace)));
    const dur = Number((performance.now() - t).toFixed(3));
    expect(result.log_likelihood).toBeLessThan(0); // log-probabilities < 0
    expect(result).toHaveProperty('normalized');
    rows.push({
      algorithm: 'compute_trace_likelihood',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: dur,
      note: `ll=${result.log_likelihood?.toFixed(3)}`,
    });
  });

  it('sample — normal trace more likely than reversed (anomalous)', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const ngram = wasm.build_ngram_predictor(log, 'concept:name', 2);
    const normal = JSON.parse(
      wasm.compute_trace_likelihood(
        ngram,
        JSON.stringify(['Request', 'Review', 'Approve', 'Complete'])
      )
    );
    const anomal = JSON.parse(
      wasm.compute_trace_likelihood(
        ngram,
        JSON.stringify(['Complete', 'Approve', 'Review', 'Request'])
      )
    );
    expect(normal.log_likelihood).toBeGreaterThan(anomal.log_likelihood);
  });

  it('sample — 1 000 calls latency', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const ngram = wasm.build_ngram_predictor(log, 'concept:name', 2);
    const trace = JSON.stringify(['Request', 'Review', 'Approve', 'Complete']);
    const t = performance.now();
    for (let i = 0; i < 1000; i++) wasm.compute_trace_likelihood(ngram, trace);
    const perCall = Number(((performance.now() - t) / 1000).toFixed(4));
    rows.push({
      algorithm: 'compute_trace_likelihood(1k)',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: perCall,
      note: 'ms/call',
    });
    expect(perCall).toBeLessThan(5);
  });
});
