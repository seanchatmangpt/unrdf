/**
 * Shared helpers for Van der Aalst prediction benchmarks.
 *
 * Loads and caches real event log data. All timing uses performance.now().
 * Each bench-*.test.ts reloads its own WASM state — no cross-file handle sharing.
 */

import { readFileSync } from 'fs';
import { join } from 'path';

export const SAMPLE_XES = join(__dirname, '../data/fixtures/sample.xes');
export const BPI_XES = join(__dirname, '../../tests/fixtures/BPI_2020_Travel_Permits_Actual.xes');

export function readXes(path: string): string {
  return readFileSync(path, 'utf8');
}

export function countTraces(xes: string): number {
  return (xes.match(/<trace/g) || []).length;
}

export function ms(start: number): number {
  return Number((performance.now() - start).toFixed(3));
}

export interface BenchRow {
  algorithm: string;
  dataset: string;
  traces: number;
  durationMs: number;
  note?: string;
}

/** Pretty-print a table of benchmark results */
export function printTable(rows: BenchRow[]): void {
  console.log('\n' + '='.repeat(72));
  console.log(
    'Algorithm'.padEnd(38) +
      'Dataset'.padEnd(10) +
      'Traces'.padEnd(8) +
      'Time(ms)'.padEnd(10) +
      'Note'
  );
  console.log('-'.repeat(72));
  for (const r of rows) {
    console.log(
      r.algorithm.padEnd(38) +
        r.dataset.padEnd(10) +
        String(r.traces).padEnd(8) +
        String(r.durationMs).padEnd(10) +
        (r.note ?? '')
    );
  }
  console.log('='.repeat(72) + '\n');
}
