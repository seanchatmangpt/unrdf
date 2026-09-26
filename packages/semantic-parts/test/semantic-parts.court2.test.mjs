/**
 * Second court falsifiers for unrdf#110/#113 at 8d17d36a. Chicago: real
 * graphs and real indexes; no doubles. Each test kills a mutant the suite at
 * 8d17d36a let survive:
 *   M06 admitSemanticIndex caches an unfrozen (index, graph) pair, so a later
 *       mutation of that index skips re-admission;
 *   M08 duplicate-part refusal removed, so a duplicate posting can offset a
 *       dropped posting and pass the count-based completeness check;
 *   M13 findAlternativesIndexed ignores minimumShared.
 */
import { describe, expect, it } from 'vitest';

import {
  admitSemanticIndex,
  buildSemanticIndex,
  findAlternatives,
  findAlternativesIndexed,
  fromCodeGraphTables,
  indexedCandidatePartIds,
} from '../src/index.mjs';

const tables = () => ({
  files: [
    { file_id: 1, language: 'Python' },
    { file_id: 2, language: 'Rust' },
    { file_id: 3, language: 'Go' },
    { file_id: 4, language: 'Java' },
  ],
  concepts: {
    algorithms: [
      { concept_id: 10, name: 'dijkstra', wikidata_qid: 'Q12105' },
      { concept_id: 11, name: 'bellman ford', wikidata_qid: 'Q294195' },
    ],
    domains: [{ concept_id: 20, name: 'graphs' }],
  },
  edges: {
    file_algorithm: [
      { file_id: 1, concept_id: 10 },
      { file_id: 1, concept_id: 11 },
      { file_id: 2, concept_id: 10 },
      { file_id: 2, concept_id: 11 },
      { file_id: 3, concept_id: 10 },
      { file_id: 4, concept_id: 11 },
    ],
    file_domain: [
      { file_id: 1, concept_id: 20 },
      { file_id: 2, concept_id: 20 },
      { file_id: 3, concept_id: 20 },
      { file_id: 4, concept_id: 20 },
    ],
  },
});

describe('court2: admission cache is only for immutable pairs', () => {
  it('re-admits a mutable index after it is mutated post-admission (kills M06)', () => {
    const graph = structuredClone(fromCodeGraphTables(tables()));
    const index = structuredClone(buildSemanticIndex(graph));
    expect(admitSemanticIndex(index, graph)).toBe(index);
    // Drop file 3 from the dijkstra posting and add it to bellman ford: counts
    // are preserved, so only per-query re-admission (soundness) can refuse it.
    index.axes.algorithm['wikidata:Q12105'] = ['codegraph:file:1', 'codegraph:file:2'];
    index.axes.algorithm['wikidata:Q294195'] = [
      'codegraph:file:1',
      'codegraph:file:2',
      'codegraph:file:3',
      'codegraph:file:4',
    ];
    expect(() => admitSemanticIndex(index, graph)).toThrow('REFUSED_INDEX_STALE');
    expect(() =>
      indexedCandidatePartIds(graph, index, '3', { requiredAxes: ['algorithm'] })
    ).toThrow('REFUSED_INDEX_STALE');
  });
});

describe('court2: duplicates cannot pay for a dropped posting', () => {
  it('refuses a duplicate posting that offsets a missing one (kills M08)', () => {
    const graph = fromCodeGraphTables(tables());
    const forged = structuredClone(buildSemanticIndex(graph));
    // Drop file 4 from bellman ford, duplicate file 1: per-axis count unchanged.
    forged.axes.algorithm['wikidata:Q294195'] = [
      'codegraph:file:1',
      'codegraph:file:1',
      'codegraph:file:2',
    ];
    expect(() => admitSemanticIndex(forged, graph)).toThrow(
      'REFUSED_INDEX_DUPLICATE_PART:algorithm:wikidata:Q294195:codegraph:file:1'
    );
  });
});

describe('court2: indexed ranking honors the full scan contract', () => {
  it('applies minimumShared exactly like findAlternatives (kills M13)', () => {
    const graph = fromCodeGraphTables(tables());
    const index = buildSemanticIndex(graph);
    const requiredAxes = ['algorithm', 'domain'];
    for (const minimumShared of [1, 2, 3, 4]) {
      const scan = findAlternatives(graph, '1', { requiredAxes, minimumShared });
      const indexed = findAlternativesIndexed(graph, index, '1', { requiredAxes, minimumShared });
      expect(indexed).toEqual(scan);
    }
    // Subject 1 shares {dijkstra, bellman ford, graphs} with file 2 only.
    expect(
      findAlternativesIndexed(graph, index, '1', { requiredAxes, minimumShared: 3 }).map(
        c => c.part_id
      )
    ).toEqual(['codegraph:file:2']);
    expect(findAlternativesIndexed(graph, index, '1', { requiredAxes, minimumShared: 4 })).toEqual(
      []
    );
  });
});
