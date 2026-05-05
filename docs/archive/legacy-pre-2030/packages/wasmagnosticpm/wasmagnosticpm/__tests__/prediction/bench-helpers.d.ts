/**
 * Shared helpers for Van der Aalst prediction benchmarks.
 *
 * Loads and caches real event log data. All timing uses performance.now().
 * Each bench-*.test.ts reloads its own WASM state — no cross-file handle sharing.
 */
export declare const SAMPLE_XES: string;
export declare const BPI_XES: string;
export declare function readXes(path: string): string;
export declare function countTraces(xes: string): number;
export declare function ms(start: number): number;
export interface BenchRow {
  algorithm: string;
  dataset: string;
  traces: number;
  durationMs: number;
  note?: string;
}
/** Pretty-print a table of benchmark results */
export declare function printTable(rows: BenchRow[]): void;
//# sourceMappingURL=bench-helpers.d.ts.map
