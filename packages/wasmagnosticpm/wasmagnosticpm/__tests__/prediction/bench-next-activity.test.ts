/**
 * Next Activity Prediction Benchmarks
 * Perspective: "What happens next?" — Van der Aalst
 *
 * Algorithms:
 *   predict_next_activity  — basic n-gram prediction (returns [{activity, probability}])
 *   score_trace_likelihood — n-gram log-probability of a full trace (plain float)
 *   predict_next_k         — top-k with confidence + entropy
 *   predict_beam_paths     — beam search over future continuations
 *
 * Each test is self-contained: loads its own log and builds its own handles.
 * setup.ts calls clear_all_objects() before each test, so handles cannot be
 * shared across tests via beforeAll.
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

// Read files once at module level (synchronous FS reads, not WASM handles)
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

// ─── predict_next_activity ───────────────────────────────────────────────────

describe('predict_next_activity', () => {
  it('sample — returns ranked [{activity, probability}] array', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const model = wasm.build_ngram_predictor(log, 'concept:name', 2);
    const t = performance.now();
    const result = JSON.parse(wasm.predict_next_activity(model, JSON.stringify(['Request'])));
    const dur = Number((performance.now() - t).toFixed(3));
    expect(Array.isArray(result)).toBe(true);
    expect(result.length).toBeGreaterThan(0);
    expect(result[0]).toHaveProperty('activity');
    expect(result[0]).toHaveProperty('probability');
    rows.push({
      algorithm: 'predict_next_activity',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: dur,
      note: `top=${result[0]?.activity}`,
    });
  });

  it('sample — probabilities sum to ≤1', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const model = wasm.build_ngram_predictor(log, 'concept:name', 2);
    const result = JSON.parse(wasm.predict_next_activity(model, JSON.stringify(['Request'])));
    const total = result.reduce((s: number, r: any) => s + r.probability, 0);
    expect(total).toBeCloseTo(1.0, 5);
  });

  it('sample — 1 000 calls latency', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const model = wasm.build_ngram_predictor(log, 'concept:name', 2);
    const prefix = JSON.stringify(['Request', 'Review']);
    const t = performance.now();
    for (let i = 0; i < 1000; i++) wasm.predict_next_activity(model, prefix);
    const perCall = Number(((performance.now() - t) / 1000).toFixed(4));
    rows.push({
      algorithm: 'predict_next_activity(1k)',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: perCall,
      note: 'ms/call',
    });
    expect(perCall).toBeLessThan(5);
  });

  it('BPI 2020 — single-step prediction', async () => {
    if (!BPI) return;
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(BPI);
    const model = wasm.build_ngram_predictor(log, 'concept:name', 2);
    const prefix = JSON.stringify(['Declaration SUBMITTED by EMPLOYEE']);
    const t = performance.now();
    const result = JSON.parse(wasm.predict_next_activity(model, prefix));
    const dur = Number((performance.now() - t).toFixed(3));
    rows.push({
      algorithm: 'predict_next_activity',
      dataset: 'BPI2020',
      traces: BPI_TRACES,
      durationMs: dur,
      note: `top=${result[0]?.activity}`,
    });
    expect(Array.isArray(result)).toBe(true);
    expect(result.length).toBeGreaterThan(0);
  });
});

// ─── score_trace_likelihood ───────────────────────────────────────────────────

describe('score_trace_likelihood', () => {
  it('sample — returns negative log-probability (plain float)', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const model = wasm.build_ngram_predictor(log, 'concept:name', 2);
    const t = performance.now();
    const score = wasm.score_trace_likelihood(
      model,
      JSON.stringify(['Request', 'Review', 'Approve', 'Complete'])
    );
    const dur = Number((performance.now() - t).toFixed(3));
    expect(typeof score).toBe('number');
    expect(score).toBeLessThan(0); // log-probability is always negative
    rows.push({
      algorithm: 'score_trace_likelihood',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: dur,
      note: `ll=${score?.toFixed(3)}`,
    });
  });

  it('sample — normal trace more likely than reversed (anomalous) trace', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const model = wasm.build_ngram_predictor(log, 'concept:name', 2);
    const normal = wasm.score_trace_likelihood(
      model,
      JSON.stringify(['Request', 'Review', 'Approve', 'Complete'])
    );
    const anomal = wasm.score_trace_likelihood(
      model,
      JSON.stringify(['Complete', 'Approve', 'Review', 'Request'])
    );
    expect(normal).toBeGreaterThan(anomal);
  });

  it('sample — 1 000 calls latency', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const model = wasm.build_ngram_predictor(log, 'concept:name', 2);
    const trace = JSON.stringify(['Request', 'Review', 'Approve', 'Complete']);
    const t = performance.now();
    for (let i = 0; i < 1000; i++) wasm.score_trace_likelihood(model, trace);
    const perCall = Number(((performance.now() - t) / 1000).toFixed(4));
    rows.push({
      algorithm: 'score_trace_likelihood(1k)',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: perCall,
      note: 'ms/call',
    });
    expect(perCall).toBeLessThan(5);
  });

  it('BPI 2020 — score a known process sequence', async () => {
    if (!BPI) return;
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(BPI);
    const model = wasm.build_ngram_predictor(log, 'concept:name', 2);
    const trace = JSON.stringify([
      'Declaration SUBMITTED by EMPLOYEE',
      'Declaration APPROVED by ADMINISTRATION',
      'Declaration FINAL_APPROVED by SUPERVISOR',
    ]);
    const t = performance.now();
    const score = wasm.score_trace_likelihood(model, trace);
    const dur = Number((performance.now() - t).toFixed(3));
    rows.push({
      algorithm: 'score_trace_likelihood',
      dataset: 'BPI2020',
      traces: BPI_TRACES,
      durationMs: dur,
      note: `ll=${score?.toFixed(3)}`,
    });
    expect(typeof score).toBe('number');
  });
});

// ─── predict_next_k ───────────────────────────────────────────────────────────

describe('predict_next_k', () => {
  it('sample — top-3, single-activity prefix', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const model = wasm.build_ngram_predictor(log, 'concept:name', 2);
    const t = performance.now();
    const result = JSON.parse(wasm.predict_next_k(model, JSON.stringify(['Request']), 3));
    const dur = Number((performance.now() - t).toFixed(3));
    expect(result.activities.length).toBeGreaterThan(0);
    expect(result.confidence).toBeGreaterThan(0);
    expect(result).toHaveProperty('entropy');
    rows.push({
      algorithm: 'predict_next_k(k=3)',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: dur,
      note: `top=${result.activities[0]}`,
    });
  });

  it('sample — 1 000 calls latency', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const model = wasm.build_ngram_predictor(log, 'concept:name', 2);
    const prefix = JSON.stringify(['Request', 'Review']);
    const t = performance.now();
    for (let i = 0; i < 1000; i++) wasm.predict_next_k(model, prefix, 3);
    const perCall = Number(((performance.now() - t) / 1000).toFixed(4));
    rows.push({
      algorithm: 'predict_next_k(1k)',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: perCall,
      note: 'ms/call',
    });
    expect(perCall).toBeLessThan(5);
  });

  it('BPI 2020 — 1 000 calls throughput', async () => {
    if (!BPI) return;
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(BPI);
    const model = wasm.build_ngram_predictor(log, 'concept:name', 2);
    const prefix = JSON.stringify(['Declaration SUBMITTED by EMPLOYEE']);
    const t = performance.now();
    for (let i = 0; i < 1000; i++) wasm.predict_next_k(model, prefix, 3);
    const perCall = Number(((performance.now() - t) / 1000).toFixed(4));
    rows.push({
      algorithm: 'predict_next_k(1k)',
      dataset: 'BPI2020',
      traces: BPI_TRACES,
      durationMs: perCall,
      note: 'ms/call',
    });
    expect(perCall).toBeLessThan(10);
  });
});

// ─── predict_beam_paths ───────────────────────────────────────────────────────

describe('predict_beam_paths', () => {
  it('sample — beam=3 steps=4', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const model = wasm.build_ngram_predictor(log, 'concept:name', 2);
    const t = performance.now();
    const result = JSON.parse(wasm.predict_beam_paths(model, JSON.stringify(['Request']), 3, 4));
    const dur = Number((performance.now() - t).toFixed(3));
    expect(Array.isArray(result)).toBe(true);
    if (result.length > 1) {
      expect(result[0].probability).toBeGreaterThanOrEqual(result[1].probability);
    }
    rows.push({
      algorithm: 'predict_beam_paths(w=3,s=4)',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: dur,
      note: `${result.length} paths`,
    });
  });

  it('BPI 2020 — beam=5 steps=5', async () => {
    if (!BPI) return;
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(BPI);
    const model = wasm.build_ngram_predictor(log, 'concept:name', 2);
    const prefix = JSON.stringify(['Declaration SUBMITTED by EMPLOYEE']);
    const t = performance.now();
    const result = JSON.parse(wasm.predict_beam_paths(model, prefix, 5, 5));
    const dur = Number((performance.now() - t).toFixed(3));
    rows.push({
      algorithm: 'predict_beam_paths(w=5,s=5)',
      dataset: 'BPI2020',
      traces: BPI_TRACES,
      durationMs: dur,
      note: `${result.length} paths`,
    });
    expect(Array.isArray(result)).toBe(true);
  });
});
