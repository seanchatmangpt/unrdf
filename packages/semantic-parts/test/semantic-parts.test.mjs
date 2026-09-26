import { describe, expect, it } from 'vitest';

import {
  admitSemanticPartsGraph,
  admitSemanticIndex,
  buildSemanticIndex,
  comparePartSemantics,
  exactSemanticClasses,
  findAlternatives,
  findAlternativesIndexed,
  indexedCandidatePartIds,
  toSemanticPartsTurtle,
  fromCodeGraphReader,
  fromCodeGraphTables,
  substitutionSurvivesSemanticFalsifier,
} from '../src/index.mjs';

const tables = {
  files: [
    { file_id: 1, sample_id: 'py-1', language: 'Python' },
    { file_id: 2, sample_id: 'rs-1', language: 'Rust' },
    { file_id: 3, sample_id: 'js-1', language: 'JavaScript' },
  ],
  concepts: {
    algorithms: [
      { concept_id: 10, name: 'dijkstra algorithm', wikidata_qid: 'Q12105', label: "Dijkstra's algorithm" },
      { concept_id: 11, name: 'bellman ford algorithm', wikidata_qid: 'Q294195', label: 'Bellman–Ford algorithm' },
    ],
    domains: [{ concept_id: 20, name: 'graph theory', wikidata_qid: 'Q131476', label: 'graph theory' }],
    paradigms: [{ concept_id: 30, name: 'imperative programming', wikidata_qid: 'Q275596', label: 'imperative programming' }],
    design_patterns: [],
  },
  edges: {
    file_algorithm: [
      { file_id: 1, concept_id: 10 },
      { file_id: 2, concept_id: 10 },
      { file_id: 3, concept_id: 11 },
    ],
    file_domain: [
      { file_id: 1, concept_id: 20 },
      { file_id: 2, concept_id: 20 },
      { file_id: 3, concept_id: 20 },
    ],
    file_paradigm: [
      { file_id: 1, concept_id: 30, confidence: 0.95 },
      { file_id: 2, concept_id: 30, confidence: 0.91 },
    ],
    file_design_pattern: [],
  },
};

describe('semantic parts graph', () => {
  it('discovers a same-algorithm alternative across programming languages', () => {
    const graph = fromCodeGraphTables(tables);
    const candidates = findAlternatives(graph, 'codegraph:file:1', { requiredAxes: ['algorithm'] });

    expect(candidates).toHaveLength(1);
    expect(candidates[0]).toMatchObject({
      part_id: 'codegraph:file:2',
      language: 'Rust',
      shared: { algorithm: ['wikidata:Q12105'] },
      authority: 'NONE',
      standing: 'CANDIDATE',
    });
    expect(substitutionSurvivesSemanticFalsifier(graph, '1', 'codegraph:file:2')).toBe(true);
    expect(substitutionSurvivesSemanticFalsifier(graph, '1', 'codegraph:file:3')).toBe(false);
  });

  it('can require a semantic cross-product without turning it into authority', () => {
    const graph = fromCodeGraphTables(tables);
    const candidates = findAlternatives(graph, '1', {
      requiredAxes: ['algorithm', 'domain', 'paradigm'],
    });

    expect(candidates).toHaveLength(1);
    expect(candidates[0].coverage).toBe(1);
    expect(candidates[0].authority).toBe('NONE');
  });


  it('is deterministic under source edge ordering', () => {
    const graph = fromCodeGraphTables(tables);
    const reordered = fromCodeGraphTables({
      ...tables,
      edges: Object.fromEntries(
        Object.entries(tables.edges).map(([name, rows]) => [name, [...rows].reverse()]),
      ),
    });

    expect(reordered).toEqual(graph);
  });


  it('indexes required-axis candidate membership without scanning semantics at query time', () => {
    const graph = fromCodeGraphTables(tables);
    const index = buildSemanticIndex(graph);

    expect(index.schema).toBe('unrdf.semantic-parts.index.v1');
    expect(index.authority).toBe('NONE');
    expect(indexedCandidatePartIds(graph, index, '1', {
      requiredAxes: ['algorithm', 'domain', 'paradigm'],
    })).toEqual(['codegraph:file:2']);
  });

  it('refuses forged or stale semantic indexes', () => {
    const graph = fromCodeGraphTables(tables);
    const index = buildSemanticIndex(graph);

    expect(() => admitSemanticIndex({ ...index, authority: 'DO' }, graph))
      .toThrow('REFUSED_INDEX_AUTHORITY');
    expect(() => admitSemanticIndex({ ...index, part_count: 999 }, graph))
      .toThrow('REFUSED_INDEX_PART_COUNT');

    const forged = structuredClone(index);
    forged.axes.algorithm['wikidata:Q12105'].push('codegraph:file:404');
    expect(() => admitSemanticIndex(forged, graph))
      .toThrow('REFUSED_INDEX_DANGLING_PART:algorithm:codegraph:file:404');
  });



  it('indexed ranked retrieval is contract-equivalent to the scan path', () => {
    const graph = fromCodeGraphTables(tables);
    const index = buildSemanticIndex(graph);
    const options = {
      requiredAxes: ['algorithm', 'domain', 'paradigm'],
      minimumShared: 3,
    };

    expect(findAlternativesIndexed(graph, index, '1', options))
      .toEqual(findAlternatives(graph, '1', options));
  });

  it('distinguishes exact observed semantics from mere adjacency', () => {
    const graph = fromCodeGraphTables(tables);

    expect(comparePartSemantics(graph, '1', '2', {
      axes: ['algorithm', 'domain', 'paradigm'],
    })).toMatchObject({
      exact_observed_semantics: true,
      authority: 'NONE',
      standing: 'CANDIDATE',
    });

    const nonEquivalent = comparePartSemantics(graph, '1', '3', {
      axes: ['algorithm', 'domain'],
    });
    expect(nonEquivalent.exact_observed_semantics).toBe(false);
    expect(nonEquivalent.delta.algorithm.only_left).toEqual(['wikidata:Q12105']);
    expect(nonEquivalent.delta.algorithm.only_right).toEqual(['wikidata:Q294195']);
    expect(nonEquivalent.standing).toBe('OBSERVED');
  });

  it('manufactures exact semantic-signature classes without treating unknown as equal', () => {
    const graph = fromCodeGraphTables(tables);
    const classes = exactSemanticClasses(graph, {
      axes: ['algorithm', 'domain', 'paradigm'],
    });

    expect(classes).toHaveLength(1);
    expect(classes[0].members).toEqual(['codegraph:file:1', 'codegraph:file:2']);
    expect(classes[0].authority).toBe('NONE');
    expect(classes[0].standing).toBe('CANDIDATE');
  });



  it('consumes CodeGraph exact release table names through an async reader', async () => {
    const requested = [];
    const releaseTables = {
      files: tables.files,
      concepts_algorithms: tables.concepts.algorithms,
      concepts_domains: tables.concepts.domains,
      concepts_paradigms: tables.concepts.paradigms,
      concepts_design_patterns: tables.concepts.design_patterns,
      edges_file_algorithm: tables.edges.file_algorithm,
      edges_file_domain: tables.edges.file_domain,
      edges_file_paradigm: tables.edges.file_paradigm,
      edges_file_design_pattern: tables.edges.file_design_pattern,
    };
    const reader = {
      async *readRows(name) {
        requested.push(name);
        for (const row of releaseTables[name]) yield row;
      },
    };

    const graph = await fromCodeGraphReader(reader);

    expect(graph).toEqual(fromCodeGraphTables(tables));
    expect(requested).toEqual([
      'files',
      'concepts_algorithms',
      'concepts_domains',
      'concepts_paradigms',
      'concepts_design_patterns',
      'edges_file_algorithm',
      'edges_file_domain',
      'edges_file_paradigm',
      'edges_file_design_pattern',
    ]);
  });

  it('refuses a reader that does not implement the release-row protocol', async () => {
    await expect(fromCodeGraphReader({})).rejects.toThrow('REFUSED_CODEGRAPH_READER');
  });

  it('projects the admitted graph into the marketplace semantic-parts RDF vocabulary', () => {
    const graph = fromCodeGraphTables(tables);
    const turtle = toSemanticPartsTurtle(graph);

    expect(turtle).toContain('@prefix sp: <https://chatmangpt.com/ontology/semantic-parts#> .');
    expect(turtle).toContain('a sp:SoftwarePart');
    expect(turtle).toContain('sp:authorityBoundary "NONE"');
    expect(turtle).toContain('sp:standing sp:Observed');
    expect(turtle).toContain('sp:implementsAlgorithm');
    expect(turtle).toContain('sp:groundedIn <https://www.wikidata.org/entity/Q12105>');
    expect(turtle).toContain('sp:language "Rust"');
    expect(toSemanticPartsTurtle(graph)).toBe(turtle);
  });

  it('refuses dangling CodeGraph edges', () => {
    expect(() => fromCodeGraphTables({
      ...tables,
      edges: { ...tables.edges, file_algorithm: [{ file_id: 999, concept_id: 10 }] },
    })).toThrow('REFUSED_DANGLING_FILE_EDGE');
  });

  it('refuses a graph that smuggles ambient authority', () => {
    const graph = fromCodeGraphTables(tables);
    expect(() => admitSemanticPartsGraph({ ...graph, authority: 'DO' }))
      .toThrow('REFUSED_AMBIENT_AUTHORITY');
  });
});
