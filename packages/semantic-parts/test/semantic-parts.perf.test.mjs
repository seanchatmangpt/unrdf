/**
 * Regression bound + replay falsifier for the semantic-parts benchmark.
 * Absolute bounds are ~20x the recorded medians in bench/receipt-v26.9.26.json
 * (floor 4000/1000 ms) because the workspace matrix runs 4 packages at once on
 * 2-CPU runners (act on colima measured build 1476 ms vs 160 ms local). The
 * load-independent structural falsifier is the scaling ratio test. Each test
 * carries an explicit timeout: vitest's 5000 ms default killed the scaling
 * test on that runner (introduced failure, fixed here).
 */
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { performance } from 'node:perf_hooks';

import { describe, expect, it } from 'vitest';

import { findAlternatives, fromCodeGraphTables } from '../src/index.mjs';
import { syntheticTables } from '../bench/fixture.mjs';

const PERF_TIMEOUT_MS = 120000;
const receipt = JSON.parse(readFileSync(new URL('../bench/receipt-v26.9.26.json', import.meta.url), 'utf8'));

describe('semantic-parts performance and replay', () => {
  const tables = syntheticTables({ files: receipt.input.files });

  it('replays the benchmarked graph byte-identically (digest matches receipt)', () => {
    const graph = fromCodeGraphTables(tables);
    const digest = createHash('sha256').update(JSON.stringify(graph)).digest('hex');
    expect(digest).toBe(receipt.graph_sha256);
  }, PERF_TIMEOUT_MS);

  it('builds 10k parts and answers a 2-axis query inside the regression bound', () => {
    const t0 = performance.now();
    const graph = fromCodeGraphTables(tables);
    const buildMs = performance.now() - t0;
    const t1 = performance.now();
    const candidates = findAlternatives(graph, '1', { requiredAxes: ['algorithm', 'domain'] });
    const queryMs = performance.now() - t1;
    expect(Array.isArray(candidates)).toBe(true);
    expect(buildMs).toBeLessThan(Math.max(4000, receipt.fromCodeGraphTables.median_ms * 20));
    expect(queryMs).toBeLessThan(Math.max(1000, receipt.findAlternatives_2axis.median_ms * 20));
  }, PERF_TIMEOUT_MS);

  it('scales build time sub-quadratically (8x input stays under 40x time)', () => {
    // Sparse shape (1 edge per file per axis) keeps the retained heap small:
    // above ~8k dense files V8 major-GC cost grows with the heap and makes wall
    // time superlinear for reasons unrelated to the algorithm (measured on
    // 19e2413a and here alike: 4k->8k->16k dense = 50/216/646 ms).
    // Measured ratio 1000->8000: 6.6-10.3x as written; 71.8x with an injected
    // O(parts^2) lookup (mutation check), so the 40x bound separates them.
    // Samples are interleaved so both sizes see the same runner load.
    const small = syntheticTables({ files: 1000, edgesPerFile: 1 });
    const large = syntheticTables({ files: 8000, edgesPerFile: 1 });
    fromCodeGraphTables(small);
    fromCodeGraphTables(large);
    let bestSmall = Infinity;
    let bestLarge = Infinity;
    for (let i = 0; i < 7; i += 1) {
      let s = performance.now();
      fromCodeGraphTables(small);
      bestSmall = Math.min(bestSmall, performance.now() - s);
      s = performance.now();
      fromCodeGraphTables(large);
      bestLarge = Math.min(bestLarge, performance.now() - s);
    }
    expect(bestLarge / bestSmall).toBeLessThan(40);
  }, PERF_TIMEOUT_MS);
});
