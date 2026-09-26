/**
 * Adversarial falsifiers for the inverted index, reader protocol and RDF
 * projection added on top of PR #110 (Chicago style: real graphs, real index,
 * real reader objects; no doubles).
 */
import { describe, expect, it } from 'vitest';

import {
  admitSemanticIndex,
  buildSemanticIndex,
  findAlternatives,
  findAlternativesIndexed,
  fromCodeGraphReader,
  fromCodeGraphTables,
  indexedCandidatePartIds,
  toSemanticPartsTurtle,
} from '../src/index.mjs';
import { syntheticTables } from '../bench/fixture.mjs';

function tables(extraAlgorithmEdges = []) {
  return {
    files: [
      { file_id: 1, language: 'Python' },
      { file_id: 2, language: 'Rust' },
      { file_id: 3, language: 'Go' },
    ],
    concepts: {
      algorithms: [
        { concept_id: 10, name: 'dijkstra', wikidata_qid: 'Q12105' },
        { concept_id: 11, name: 'bellman ford', wikidata_qid: 'Q294195' },
      ],
    },
    edges: {
      file_algorithm: [
        { file_id: 1, concept_id: 10 },
        { file_id: 2, concept_id: 10 },
        { file_id: 3, concept_id: 11 },
        ...extraAlgorithmEdges,
      ],
    },
  };
}

describe('stale or forged index is refused', () => {
  it('refuses an index built from a different graph with the same part count', () => {
    const before = fromCodeGraphTables(tables());
    const staleIndex = buildSemanticIndex(before);
    const after = fromCodeGraphTables(tables([{ file_id: 3, concept_id: 10 }]));
    expect(after.parts).toHaveLength(before.parts.length);
    expect(() => indexedCandidatePartIds(after, staleIndex, '1')).toThrow('REFUSED_INDEX_STALE');
    // the fresh index answers what the scan answers
    const fresh = buildSemanticIndex(after);
    expect(findAlternativesIndexed(after, fresh, '1')).toEqual(findAlternatives(after, '1'));
  });

  it('refuses an index with a posting removed (incomplete) or injected (unsound)', () => {
    const graph = fromCodeGraphTables(tables());
    const dropped = structuredClone(buildSemanticIndex(graph));
    dropped.axes.algorithm['wikidata:Q12105'] = ['codegraph:file:1'];
    expect(() => admitSemanticIndex(dropped, graph)).toThrow('REFUSED_INDEX_STALE');
    const injected = structuredClone(buildSemanticIndex(graph));
    injected.axes.algorithm['wikidata:Q294195'].push('codegraph:file:1');
    expect(() => admitSemanticIndex(injected, graph)).toThrow('REFUSED_INDEX_STALE');
  });

  it('refuses an index that grants authority or carries an unknown axis', () => {
    const graph = fromCodeGraphTables(tables());
    expect(() => admitSemanticIndex({ ...buildSemanticIndex(graph), authority: 'DO' }, graph))
      .toThrow('REFUSED_INDEX_AUTHORITY');
    const extra = structuredClone(buildSemanticIndex(graph));
    extra.axes.language = { x: ['codegraph:file:1'] };
    expect(() => admitSemanticIndex(extra, graph)).toThrow('REFUSED_INDEX_UNKNOWN_AXIS:language');
  });

  it('indexed and scan queries agree on every subject of a synthetic release', () => {
    const graph = fromCodeGraphTables(syntheticTables({ files: 150, conceptsPerAxis: 20 }));
    const index = buildSemanticIndex(graph);
    for (const part of graph.parts) {
      for (const requiredAxes of [['algorithm'], ['algorithm', 'domain'], ['paradigm', 'design_pattern']]) {
        expect(findAlternativesIndexed(graph, index, part.part_id, { requiredAxes }))
          .toEqual(findAlternatives(graph, part.part_id, { requiredAxes }));
      }
    }
  }, 120000);
});

describe('immutable admission cache', () => {
  it('returns a deep-frozen graph and index that cannot be mutated into a stale state', () => {
    const graph = fromCodeGraphTables(tables());
    const index = buildSemanticIndex(graph);
    expect(Object.isFrozen(graph.parts[0].semantics.algorithm[0])).toBe(true);
    expect(() => { graph.parts[0].authority = 'DO'; }).toThrow(TypeError);
    expect(() => { index.axes.algorithm['wikidata:Q12105'].push('codegraph:file:3'); }).toThrow(TypeError);
    expect(graph.parts[0].authority).toBe('NONE');
  });

  it('does not freeze or alias the caller input rows', () => {
    const t = tables();
    fromCodeGraphTables(t);
    expect(Object.isFrozen(t.files[0])).toBe(false);
    t.files[0].language = 'Changed';
    expect(t.files[0].language).toBe('Changed');
  });

  it('still re-admits a mutable copy, so a forged clone is refused', () => {
    const graph = fromCodeGraphTables(tables());
    const index = buildSemanticIndex(graph);
    const forged = structuredClone(graph);
    forged.parts[0].authority = 'DO';
    expect(() => indexedCandidatePartIds(forged, index, '1')).toThrow('REFUSED_PART_AUTHORITY');
    const staleCopy = structuredClone(index);
    staleCopy.axes.algorithm['wikidata:Q12105'] = ['codegraph:file:1'];
    expect(() => indexedCandidatePartIds(graph, staleCopy, '1')).toThrow('REFUSED_INDEX_STALE');
  });
});

describe('reader protocol boundaries', () => {
  function readerFrom(t) {
    const names = {
      files: t.files,
      concepts_algorithms: t.concepts.algorithms ?? [],
      edges_file_algorithm: t.edges.file_algorithm ?? [],
    };
    return { readRows: (name) => names[name] ?? [] };
  }

  it('matches fromCodeGraphTables for the same rows', async () => {
    expect(await fromCodeGraphReader(readerFrom(tables()))).toEqual(fromCodeGraphTables(tables()));
  });

  it('refuses a table delivered as a string instead of rows', async () => {
    const reader = { readRows: (name) => (name === 'files' ? 'file_id\n1' : []) };
    await expect(fromCodeGraphReader(reader)).rejects.toThrow('REFUSED_READER_ROWS:files');
  });

  it('refuses duplicate delivery of a file row through the reader', async () => {
    const t = tables();
    const reader = readerFrom({ ...t, files: [...t.files, t.files[0]] });
    await expect(fromCodeGraphReader(reader)).rejects.toThrow('REFUSED_DUPLICATE_FILE:1');
  });
});

describe('RDF projection grounding', () => {
  it('grounds Wikidata concepts in the canonical http entity namespace', () => {
    const turtle = toSemanticPartsTurtle(fromCodeGraphTables(tables()));
    expect(turtle).toContain('sp:groundedIn <http://www.wikidata.org/entity/Q12105>');
    expect(turtle).not.toContain('https://www.wikidata.org/entity/');
  });

  it('escapes hostile labels so the literal cannot close early', () => {
    const t = tables();
    t.concepts.algorithms[0].label = 'x" ; a <urn:evil> .\n\\';
    const turtle = toSemanticPartsTurtle(fromCodeGraphTables(t));
    expect(turtle).toContain('rdfs:label "x\\" ; a <urn:evil> .\\n\\\\"');
  });
});
