/**
 * Drift Detection Benchmarks
 * Perspective: "Has the process changed?" — Van der Aalst
 *
 * Note: detect_drift and compute_ewma return JS objects via serde_wasm_bindgen
 * (not JSON strings), so results are used directly without JSON.parse.
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

describe('detect_drift', () => {
  it('sample — window=2', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const t = performance.now();
    const result = JSON.parse(wasm.detect_drift(log, 'concept:name', 2));
    const dur = Number((performance.now() - t).toFixed(3));
    expect(result).toHaveProperty('drifts_detected');
    expect(result).toHaveProperty('method');
    rows.push({
      algorithm: 'detect_drift(w=2)',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: dur,
      note: `${result.drifts_detected} drifts`,
    });
  });

  it('BPI 2020 — window=50', async () => {
    if (!BPI) return;
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(BPI);
    const t = performance.now();
    const result = JSON.parse(wasm.detect_drift(log, 'concept:name', 50));
    const dur = Number((performance.now() - t).toFixed(3));
    rows.push({
      algorithm: 'detect_drift(w=50)',
      dataset: 'BPI2020',
      traces: BPI_TRACES,
      durationMs: dur,
      note: `${result.drifts_detected} drifts`,
    });
    expect(result.drifts_detected).toBeGreaterThanOrEqual(0);
  });

  it('BPI 2020 — window=100', async () => {
    if (!BPI) return;
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(BPI);
    const t = performance.now();
    const result = JSON.parse(wasm.detect_drift(log, 'concept:name', 100));
    const dur = Number((performance.now() - t).toFixed(3));
    rows.push({
      algorithm: 'detect_drift(w=100)',
      dataset: 'BPI2020',
      traces: BPI_TRACES,
      durationMs: dur,
      note: `${result.drifts_detected} drifts`,
    });
    expect(result.drifts_detected).toBeGreaterThanOrEqual(0);
  });
});

describe('compute_ewma', () => {
  it('stable series — trend=stable', async () => {
    const wasm = await loadWasm();
    const values = [100, 102, 98, 101, 99, 100];
    const t = performance.now();
    const result = JSON.parse(wasm.compute_ewma(JSON.stringify(values), 0.3));
    const dur = Number((performance.now() - t).toFixed(3));
    expect(result.smoothed.length).toBe(values.length);
    expect(result).toHaveProperty('trend');
    rows.push({
      algorithm: 'compute_ewma(stable)',
      dataset: 'synthetic',
      traces: 0,
      durationMs: dur,
      note: `trend=${result.trend}`,
    });
  });

  it('spike series — EWMA reacts at index 3', async () => {
    const wasm = await loadWasm();
    const values = [100, 105, 102, 500, 510, 490, 150, 110];
    const result = JSON.parse(wasm.compute_ewma(JSON.stringify(values), 0.5));
    expect(result.smoothed[3]).toBeGreaterThan(result.smoothed[2] * 1.5);
  });

  it('10 000 values throughput', async () => {
    const wasm = await loadWasm();
    const values = Array.from({ length: 10_000 }, (_, i) => 100 + Math.sin(i) * 10);
    const t = performance.now();
    JSON.parse(wasm.compute_ewma(JSON.stringify(values), 0.3));
    const dur = Number((performance.now() - t).toFixed(3));
    rows.push({ algorithm: 'compute_ewma(10k)', dataset: 'synthetic', traces: 0, durationMs: dur });
    expect(dur).toBeLessThan(200);
  });
});
