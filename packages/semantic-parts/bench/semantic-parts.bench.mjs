#!/usr/bin/env node
/**
 * Deterministic timing benchmark for @unrdf/semantic-parts.
 * Usage: node bench/semantic-parts.bench.mjs [--files=N] [--out=path.json]
 * Emits a JSON receipt (median/p95 ms per operation, input shape, output digest).
 */
import { createHash } from 'node:crypto';
import { writeFileSync } from 'node:fs';
import { performance } from 'node:perf_hooks';

import {
  buildSemanticIndex,
  findAlternatives,
  findAlternativesIndexed,
  fromCodeGraphTables,
  indexedCandidatePartIds,
} from '../src/index.mjs';
import { syntheticTables } from './fixture.mjs';

const args = Object.fromEntries(process.argv.slice(2).map((a) => a.replace(/^--/, '').split('=')));
const files = Number(args.files ?? 10000);
const iterations = Number(args.iterations ?? 7);

function stats(samples) {
  const sorted = [...samples].sort((a, b) => a - b);
  const pick = (q) => sorted[Math.min(sorted.length - 1, Math.floor(q * sorted.length))];
  return { median_ms: +pick(0.5).toFixed(3), p95_ms: +pick(0.95).toFixed(3), min_ms: +sorted[0].toFixed(3) };
}

const tables = syntheticTables({ files });
const buildSamples = [];
let graph;
for (let i = 0; i < iterations; i += 1) {
  const t0 = performance.now();
  graph = fromCodeGraphTables(tables);
  buildSamples.push(performance.now() - t0);
}
const indexBuildSamples = [];
let index;
for (let i = 0; i < iterations; i += 1) {
  const t0 = performance.now();
  index = buildSemanticIndex(graph);
  indexBuildSamples.push(performance.now() - t0);
}
const querySamples = [];
const indexedQuerySamples = [];
const indexedRankedQuerySamples = [];
let result;
let indexedResult;
let indexedRankedResult;
for (let i = 0; i < iterations; i += 1) {
  const t0 = performance.now();
  result = findAlternatives(graph, String((i * 997) % files + 1), { requiredAxes: ['algorithm', 'domain'] });
  querySamples.push(performance.now() - t0);

  const t1 = performance.now();
  indexedResult = indexedCandidatePartIds(graph, index, String((i * 997) % files + 1), {
    requiredAxes: ['algorithm', 'domain'],
  });
  indexedQuerySamples.push(performance.now() - t1);

  const t2 = performance.now();
  indexedRankedResult = findAlternativesIndexed(
    graph,
    index,
    String((i * 997) % files + 1),
    { requiredAxes: ['algorithm', 'domain'] },
  );
  indexedRankedQuerySamples.push(performance.now() - t2);
}
const digest = createHash('sha256').update(JSON.stringify(graph)).digest('hex');
const receipt = {
  schema: 'unrdf.semantic-parts.bench.v1',
  node: process.version,
  platform: `${process.platform}-${process.arch}`,
  input: { files, edges: Object.values(tables.edges).reduce((n, r) => n + r.length, 0), iterations },
  graph_sha256: digest,
  fromCodeGraphTables: stats(buildSamples),
  buildSemanticIndex: stats(indexBuildSamples),
  findAlternatives_2axis: { ...stats(querySamples), last_candidates: result.length },
  indexedCandidatePartIds_2axis: {
    ...stats(indexedQuerySamples),
    last_candidates: indexedResult.length,
  },
  findAlternativesIndexed_2axis: {
    ...stats(indexedRankedQuerySamples),
    last_candidates: indexedRankedResult.length,
  },
};
if (args.out) writeFileSync(args.out, `${JSON.stringify(receipt, null, 2)}\n`);
process.stdout.write(`${JSON.stringify(receipt, null, 2)}\n`);
