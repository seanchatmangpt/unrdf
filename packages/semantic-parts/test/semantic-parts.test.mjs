import { describe, expect, it } from 'vitest';

import {
  admitSemanticPartsGraph,
  findAlternatives,
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
