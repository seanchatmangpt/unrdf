/**
 * Remaining Time Prediction Benchmarks
 * Perspective: "When will this case complete?" — Van der Aalst
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

describe('build_remaining_time_model', () => {
  it('sample — build latency', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const t = performance.now();
    wasm.build_remaining_time_model(log, 'concept:name', 'time:timestamp');
    const dur = Number((performance.now() - t).toFixed(3));
    rows.push({
      algorithm: 'build_remaining_time_model',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: dur,
    });
    expect(dur).toBeLessThan(1000);
  });

  it('BPI 2020 — build latency', async () => {
    if (!BPI) return;
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(BPI);
    const t = performance.now();
    wasm.build_remaining_time_model(log, 'concept:name', 'time:timestamp');
    const dur = Number((performance.now() - t).toFixed(3));
    rows.push({
      algorithm: 'build_remaining_time_model',
      dataset: 'BPI2020',
      traces: BPI_TRACES,
      durationMs: dur,
    });
    expect(dur).toBeLessThan(30000);
  });
});

describe('predict_case_duration', () => {
  it('sample — two-activity prefix', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const model = wasm.build_remaining_time_model(log, 'concept:name', 'time:timestamp');
    const t = performance.now();
    const result = JSON.parse(
      wasm.predict_case_duration(model, JSON.stringify(['Request', 'Review']))
    );
    const dur = Number((performance.now() - t).toFixed(3));
    expect(result).toHaveProperty('remaining_ms');
    expect(result).toHaveProperty('confidence');
    expect(result.remaining_ms).toBeGreaterThanOrEqual(0);
    rows.push({
      algorithm: 'predict_case_duration',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: dur,
      note: `method=${result.method}`,
    });
  });

  it('sample — 1 000 calls latency', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const model = wasm.build_remaining_time_model(log, 'concept:name', 'time:timestamp');
    const prefix = JSON.stringify(['Request']);
    const t = performance.now();
    for (let i = 0; i < 1000; i++) wasm.predict_case_duration(model, prefix);
    const perCall = Number(((performance.now() - t) / 1000).toFixed(4));
    rows.push({
      algorithm: 'predict_case_duration(1k)',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: perCall,
      note: 'ms/call',
    });
    expect(perCall).toBeLessThan(5);
  });

  it('BPI 2020 — real prefix remaining time', async () => {
    if (!BPI) return;
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(BPI);
    const model = wasm.build_remaining_time_model(log, 'concept:name', 'time:timestamp');
    const prefix = JSON.stringify([
      'Declaration SUBMITTED by EMPLOYEE',
      'Declaration APPROVED by ADMINISTRATION',
    ]);
    const t = performance.now();
    const result = JSON.parse(wasm.predict_case_duration(model, prefix));
    const dur = Number((performance.now() - t).toFixed(3));
    const remainingH = (result.remaining_ms ?? 0) / 3_600_000;
    rows.push({
      algorithm: 'predict_case_duration',
      dataset: 'BPI2020',
      traces: BPI_TRACES,
      durationMs: dur,
      note: `${remainingH.toFixed(1)}h remaining`,
    });
    expect(result.remaining_ms).toBeGreaterThanOrEqual(0);
  });
});

describe('predict_hazard_rate', () => {
  it('sample — Weibull h(t) at 2h elapsed', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const model = wasm.build_remaining_time_model(log, 'concept:name', 'time:timestamp');
    const t = performance.now();
    const result = JSON.parse(wasm.predict_hazard_rate(model, 7_200_000));
    const dur = Number((performance.now() - t).toFixed(3));
    expect(result).toHaveProperty('hazard_rate');
    expect(result).toHaveProperty('survival_probability');
    expect(result.survival_probability).toBeGreaterThanOrEqual(0);
    expect(result.survival_probability).toBeLessThanOrEqual(1);
    rows.push({
      algorithm: 'predict_hazard_rate',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: dur,
      note: `h(2h)=${result.hazard_rate?.toFixed(6)}`,
    });
  });

  it('sample — 1 000 hazard evaluations latency', async () => {
    const wasm = await loadWasm();
    const log = wasm.load_eventlog_from_xes(SAMPLE);
    const model = wasm.build_remaining_time_model(log, 'concept:name', 'time:timestamp');
    const t = performance.now();
    for (let i = 0; i < 1000; i++) wasm.predict_hazard_rate(model, i * 3_600_000);
    const perCall = Number(((performance.now() - t) / 1000).toFixed(4));
    rows.push({
      algorithm: 'predict_hazard_rate(1k)',
      dataset: 'sample',
      traces: SAMPLE_TRACES,
      durationMs: perCall,
      note: 'ms/call',
    });
    expect(perCall).toBeLessThan(5);
  });
});
