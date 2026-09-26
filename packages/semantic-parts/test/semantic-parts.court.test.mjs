/**
 * Court falsifiers for the semantic-parts index/reader hardening
 * (unrdf#110 follow-up). Chicago: real graphs, real indexes, real reader
 * objects; no doubles. Each test kills a named mutant that the earlier suite
 * let survive (soundness removed, mutable graph/index cached, boxed string).
 */
import { describe, expect, it } from 'vitest';

import {
  admitSemanticIndex,
  buildSemanticIndex,
  findAlternatives,
  fromCodeGraphReader,
  fromCodeGraphTables,
  indexedCandidatePartIds,
} from '../src/index.mjs';

const concepts = {
  algorithms: [
    { concept_id: 10, name: 'dijkstra', wikidata_qid: 'Q12105' },
    { concept_id: 11, name: 'bellman ford', wikidata_qid: 'Q294195' },
  ],
};
const files = [
  { file_id: 1, language: 'Python' },
  { file_id: 2, language: 'Rust' },
  { file_id: 3, language: 'Go' },
];
const t = edges => ({ files, concepts, edges: { file_algorithm: edges } });

describe('court: count-preserving stale index', () => {
  it('A1 refuses a posting swap that preserves every per-axis count', () => {
    const graph = fromCodeGraphTables(
      t([
        { file_id: 1, concept_id: 10 },
        { file_id: 2, concept_id: 10 },
        { file_id: 3, concept_id: 11 },
      ])
    );
    const swapped = structuredClone(buildSemanticIndex(graph));
    swapped.axes.algorithm['wikidata:Q12105'] = ['codegraph:file:1', 'codegraph:file:3'];
    swapped.axes.algorithm['wikidata:Q294195'] = ['codegraph:file:2'];
    expect(() => admitSemanticIndex(swapped, graph)).toThrow('REFUSED_INDEX_STALE');
  });

  it('A2 refuses a frozen index from a same-shape graph with swapped edges', () => {
    const before = fromCodeGraphTables(
      t([
        { file_id: 1, concept_id: 10 },
        { file_id: 2, concept_id: 10 },
        { file_id: 3, concept_id: 11 },
      ])
    );
    const after = fromCodeGraphTables(
      t([
        { file_id: 1, concept_id: 10 },
        { file_id: 2, concept_id: 11 },
        { file_id: 3, concept_id: 10 },
      ])
    );
    const stale = buildSemanticIndex(before);
    expect(() => indexedCandidatePartIds(after, stale, '1')).toThrow('REFUSED_INDEX_STALE');
  });
});

describe('court: admission cache must not outlive a mutable graph', () => {
  it('A3 re-admits an index built from a mutable graph after that graph changes', () => {
    const graph = structuredClone(
      fromCodeGraphTables(
        t([
          { file_id: 1, concept_id: 10 },
          { file_id: 2, concept_id: 10 },
          { file_id: 3, concept_id: 11 },
        ])
      )
    );
    const index = buildSemanticIndex(graph);
    graph.parts[1].semantics.algorithm = [];
    let answer;
    try {
      answer = indexedCandidatePartIds(graph, index, '1');
    } catch (error) {
      expect(String(error.message)).toMatch(/REFUSED_INDEX_STALE/);
      return;
    }
    expect(answer).toEqual(findAlternatives(graph, '1').map(c => c.part_id));
  });

  it('A4 a cached graph cannot be mutated to carry authority', () => {
    const graph = fromCodeGraphTables(t([{ file_id: 1, concept_id: 10 }]));
    expect(() => {
      graph.authority = 'DO';
    }).toThrow(TypeError);
    expect(() => {
      graph.parts[0].semantics.algorithm.push({ semantic_id: 'x' });
    }).toThrow(TypeError);
  });
});

describe('court: reader payload boundary', () => {
  it('A5 refuses a boxed string payload the same way as a primitive string', async () => {
    const reader = { readRows: name => (name === 'files' ? new String('') : []) };
    await expect(fromCodeGraphReader(reader)).rejects.toThrow('REFUSED_READER_ROWS:files');
  });
});

describe('court: reader rows must be decoded records', () => {
  it('A6 refuses a non-empty boxed string, a byte view and scalar rows', async () => {
    const only = payload => ({ readRows: name => (name === 'files' ? payload : []) });
    await expect(fromCodeGraphReader(only(new String('file_id\n1')))).rejects.toThrow(
      'REFUSED_READER_ROWS:files'
    );
    await expect(fromCodeGraphReader(only(new Uint8Array([1, 2])))).rejects.toThrow(
      'REFUSED_READER_ROWS:files'
    );
    await expect(fromCodeGraphReader(only([1, 2]))).rejects.toThrow('REFUSED_READER_ROWS:files');
    await expect(fromCodeGraphReader(only(new Set(['a'])))).rejects.toThrow(
      'REFUSED_READER_ROWS:files'
    );
  });

  it('A7 still admits rows from an async iterable of records', async () => {
    async function* rows() {
      yield { file_id: 1, language: 'Python' };
    }
    const graph = await fromCodeGraphReader({ readRows: name => (name === 'files' ? rows() : []) });
    expect(graph.parts.map(p => p.part_id)).toEqual(['codegraph:file:1']);
  });
});

describe('court: graph admission is not cached for mutable graphs', () => {
  it('A8 re-admits a mutable graph on every call, so a later forgery is refused', () => {
    const graph = structuredClone(
      fromCodeGraphTables(
        t([
          { file_id: 1, concept_id: 10 },
          { file_id: 2, concept_id: 10 },
        ])
      )
    );
    expect(findAlternatives(graph, '1').map(c => c.part_id)).toEqual(['codegraph:file:2']);
    graph.parts[1].authority = 'DO';
    expect(() => findAlternatives(graph, '1')).toThrow('REFUSED_PART_AUTHORITY:codegraph:file:2');
  });
});
