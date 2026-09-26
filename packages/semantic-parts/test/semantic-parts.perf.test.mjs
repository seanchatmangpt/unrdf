/**
 * Regression bound + replay falsifier for the semantic-parts benchmark.
 * Bounds are ~10x the recorded medians in bench/receipt-v26.9.26.json so a
 * slow CI runner passes while an accidental quadratic path fails.
 */
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { performance } from 'node:perf_hooks';

import { describe, expect, it } from 'vitest';

import { findAlternatives, fromCodeGraphTables } from '../src/index.mjs';
import { syntheticTables } from '../bench/fixture.mjs';

const receipt = JSON.parse(readFileSync(new URL('../bench/receipt-v26.9.26.json', import.meta.url), 'utf8'));

describe('semantic-parts performance and replay', () => {
  const tables = syntheticTables({ files: receipt.input.files });

  it('replays the benchmarked graph byte-identically (digest matches receipt)', () => {
    const graph = fromCodeGraphTables(tables);
    const digest = createHash('sha256').update(JSON.stringify(graph)).digest('hex');
    expect(digest).toBe(receipt.graph_sha256);
  });

  it('builds 10k parts and answers a 2-axis query inside the regression bound', () => {
    const t0 = performance.now();
    const graph = fromCodeGraphTables(tables);
    const buildMs = performance.now() - t0;
    const t1 = performance.now();
    const candidates = findAlternatives(graph, '1', { requiredAxes: ['algorithm', 'domain'] });
    const queryMs = performance.now() - t1;
    expect(Array.isArray(candidates)).toBe(true);
    expect(buildMs).toBeLessThan(Math.max(1500, receipt.fromCodeGraphTables.median_ms * 10));
    expect(queryMs).toBeLessThan(Math.max(500, receipt.findAlternatives_2axis.median_ms * 10));
  });

  it('scales build time sub-quadratically (4x input stays under 12x time)', () => {
    const small = syntheticTables({ files: 2500 });
    const large = syntheticTables({ files: 10000 });
    const time = (t) => {
      fromCodeGraphTables(t);
      const samples = [];
      for (let i = 0; i < 3; i += 1) {
        const s = performance.now();
        fromCodeGraphTables(t);
        samples.push(performance.now() - s);
      }
      return Math.min(...samples);
    };
    expect(time(large) / time(small)).toBeLessThan(12);
  });
});
