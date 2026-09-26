/**
 * Adversarial/boundary falsifiers for the semantic-parts graph (Chicago style:
 * every assertion runs the real admission and discovery functions on real
 * table rows; no doubles). Each case pins a defect class found on PR #110 head
 * 19e2413a or a refusal the capability claims to make.
 */
import { describe, expect, it } from 'vitest';

import {
  admitSemanticPartsGraph,
  findAlternatives,
  fromCodeGraphTables,
  substitutionSurvivesSemanticFalsifier,
} from '../src/index.mjs';

function tables() {
  return {
    files: [
      { file_id: 1, sample_id: 'py-1', language: 'Python' },
      { file_id: 2, sample_id: 'rs-1', language: 'Rust' },
      { file_id: 3, sample_id: 'go-1', language: 'Go' },
    ],
    concepts: {
      algorithms: [
        { concept_id: 10, name: 'dijkstra', wikidata_qid: 'Q12105' },
        { concept_id: 12, name: 'dijkstra alias', wikidata_qid: 'Q12105' },
        { concept_id: 13, name: 'ungrounded local concept' },
      ],
      domains: [{ concept_id: 20, name: 'graph theory', wikidata_qid: 'Q131476' }],
    },
    edges: {
      file_algorithm: [
        { file_id: 1, concept_id: 10, confidence: 0.9 },
        { file_id: 2, concept_id: 12, confidence: 0.8 },
        { file_id: 3, concept_id: 13 },
      ],
      file_domain: [
        { file_id: 1, concept_id: 20 },
        { file_id: 2, concept_id: 20 },
      ],
    },
  };
}

describe('duplicate delivery and reordering', () => {
  it('collapses an identical duplicate edge delivery idempotently', () => {
    const t = tables();
    const once = fromCodeGraphTables(t);
    const twice = fromCodeGraphTables({
      ...t,
      edges: {
        ...t.edges,
        file_algorithm: [
          ...t.edges.file_algorithm,
          { file_id: 1, concept_id: 10, confidence: 0.9 },
        ],
      },
    });
    expect(twice).toEqual(once);
  });

  it('refuses a duplicate edge whose evidence conflicts (no order-dependent winner)', () => {
    const t = tables();
    const conflicting = [
      { file_id: 1, concept_id: 10, confidence: 0.1 },
      ...t.edges.file_algorithm,
    ];
    for (const rows of [conflicting, [...conflicting].reverse()]) {
      expect(() =>
        fromCodeGraphTables({ ...t, edges: { ...t.edges, file_algorithm: rows } })
      ).toThrow('REFUSED_CONFLICTING_DUPLICATE_EDGE:algorithm:1:10');
    }
  });

  it('is invariant under file, concept and edge row permutation', () => {
    const t = tables();
    const canonical = JSON.stringify(fromCodeGraphTables(t));
    const permuted = {
      files: [...t.files].reverse(),
      concepts: Object.fromEntries(
        Object.entries(t.concepts).map(([k, v]) => [k, [...v].reverse()])
      ),
      edges: Object.fromEntries(Object.entries(t.edges).map(([k, v]) => [k, [...v].reverse()])),
    };
    expect(JSON.stringify(fromCodeGraphTables(permuted))).toBe(canonical);
  });

  it('orders parts by code point, not host locale', () => {
    const graph = fromCodeGraphTables({
      files: [{ file_id: 'b' }, { file_id: 'B' }, { file_id: 'a' }, { file_id: '_' }],
    });
    expect(graph.parts.map(p => p.source.file_id)).toEqual(['B', '_', 'a', 'b']);
  });

  it('treats two concept ids with one Wikidata QID as one semantic identity', () => {
    const graph = fromCodeGraphTables(tables());
    const [candidate] = findAlternatives(graph, '1');
    expect(candidate.part_id).toBe('codegraph:file:2');
    expect(candidate.shared.algorithm).toEqual(['wikidata:Q12105']);
  });
});

describe('malformed input is refused with a typed reason', () => {
  it('refuses non-scalar identities instead of stringifying them', () => {
    expect(() => fromCodeGraphTables({ files: [{ file_id: {} }] })).toThrow(
      'REFUSED_NON_SCALAR_IDENTITY:file_id'
    );
    expect(() => fromCodeGraphTables({ files: [{ file_id: [1] }] })).toThrow(
      'REFUSED_NON_SCALAR_IDENTITY'
    );
    expect(() => fromCodeGraphTables({ files: [{ file_id: Number.NaN }] })).toThrow(
      'REFUSED_NON_SCALAR_IDENTITY'
    );
    expect(() => fromCodeGraphTables({ files: [{ file_id: true }] })).toThrow(
      'REFUSED_NON_SCALAR_IDENTITY'
    );
  });

  it('refuses a numeric and string file id that collide canonically', () => {
    expect(() => fromCodeGraphTables({ files: [{ file_id: 1 }, { file_id: '1' }] })).toThrow(
      'REFUSED_DUPLICATE_FILE:1'
    );
  });

  it('refuses missing or non-object table containers', () => {
    expect(() => fromCodeGraphTables(null)).toThrow('tables must be an object');
    expect(() => fromCodeGraphTables({ files: [], concepts: null })).toThrow(
      'concepts must be an object'
    );
    expect(() => fromCodeGraphTables({ files: 'x' })).toThrow('files must be an array');
  });

  it('refuses duplicate concepts and dangling concept edges', () => {
    const t = tables();
    expect(() =>
      fromCodeGraphTables({
        ...t,
        concepts: { ...t.concepts, domains: [{ concept_id: 20 }, { concept_id: '20' }] },
      })
    ).toThrow('REFUSED_DUPLICATE_CONCEPT:domain:20');
    expect(() =>
      fromCodeGraphTables({
        ...t,
        edges: { ...t.edges, file_domain: [{ file_id: 1, concept_id: 99 }] },
      })
    ).toThrow('REFUSED_DANGLING_CONCEPT_EDGE:domain:99');
  });

  it('refuses forged graphs whose semantic values are not objects', () => {
    const graph = structuredClone(fromCodeGraphTables(tables()));
    graph.parts[0].semantics.algorithm.push(null);
    expect(() => admitSemanticPartsGraph(graph)).toThrow(
      'REFUSED_SEMANTIC_VALUE_NOT_OBJECT:codegraph:file:1:algorithm'
    );
    const graph2 = structuredClone(fromCodeGraphTables(tables()));
    graph2.parts.push(null);
    expect(() => admitSemanticPartsGraph(graph2)).toThrow('REFUSED_PART_NOT_OBJECT');
  });

  it('refuses wrong schema, duplicate parts and duplicate semantic ids', () => {
    const graph = fromCodeGraphTables(tables());
    expect(() => admitSemanticPartsGraph({ ...graph, schema: 'unrdf.semantic-parts.v0' })).toThrow(
      'REFUSED_SCHEMA'
    );
    expect(() =>
      admitSemanticPartsGraph({ ...graph, parts: [...graph.parts, graph.parts[0]] })
    ).toThrow('REFUSED_DUPLICATE_PART:codegraph:file:1');
    const forged = structuredClone(graph);
    forged.parts[0].semantics.algorithm.push({ ...forged.parts[0].semantics.algorithm[0] });
    expect(() => admitSemanticPartsGraph(forged)).toThrow(
      'REFUSED_DUPLICATE_SEMANTIC_ID:codegraph:file:1:wikidata:Q12105'
    );
  });
});

describe('unauthorized action and stale subject', () => {
  it('refuses a part that was mutated to carry authority after construction', () => {
    const graph = structuredClone(fromCodeGraphTables(tables()));
    graph.parts[1].authority = 'DO';
    expect(() => findAlternatives(graph, '1')).toThrow('REFUSED_PART_AUTHORITY:codegraph:file:2');
  });

  it('never emits a candidate with authority other than NONE', () => {
    const graph = fromCodeGraphTables(tables());
    for (const c of findAlternatives(graph, '1', { requiredAxes: ['algorithm', 'domain'] })) {
      expect(c.authority).toBe('NONE');
      expect(c.standing).toBe('CANDIDATE');
    }
  });

  it('refuses an unknown subject and an unknown candidate rather than answering false', () => {
    const graph = fromCodeGraphTables(tables());
    expect(() => findAlternatives(graph, '404')).toThrow('REFUSED_UNKNOWN_SUBJECT:404');
    expect(() => substitutionSurvivesSemanticFalsifier(graph, '1', '404')).toThrow(
      'REFUSED_UNKNOWN_CANDIDATE:404'
    );
  });

  it('resolves the candidate by file id the same way as the subject', () => {
    const graph = fromCodeGraphTables(tables());
    expect(substitutionSurvivesSemanticFalsifier(graph, '1', '2')).toBe(true);
    expect(substitutionSurvivesSemanticFalsifier(graph, '1', 'codegraph:file:2')).toBe(true);
    expect(substitutionSurvivesSemanticFalsifier(graph, '1', '3')).toBe(false);
    expect(substitutionSurvivesSemanticFalsifier(graph, '1', '1')).toBe(false);
  });

  it('refuses a file-id reference that is ambiguous in a forged graph', () => {
    const graph = structuredClone(fromCodeGraphTables(tables()));
    graph.parts[2].source.file_id = '2';
    expect(() => findAlternatives(graph, '2')).toThrow('REFUSED_AMBIGUOUS_PART_REFERENCE:2');
  });

  it('refuses a subject whose required axis is empty', () => {
    const graph = fromCodeGraphTables(tables());
    expect(() => findAlternatives(graph, '3', { requiredAxes: ['domain'] })).toThrow(
      'REFUSED_SUBJECT_AXIS_EMPTY:domain'
    );
  });
});

describe('query parameter boundaries', () => {
  const graph = fromCodeGraphTables(tables());

  it('refuses duplicate, unknown and empty required axes', () => {
    expect(() =>
      findAlternatives(graph, '1', { requiredAxes: ['algorithm', 'algorithm'] })
    ).toThrow('REFUSED_DUPLICATE_REQUIRED_AXIS');
    expect(() => findAlternatives(graph, '1', { requiredAxes: ['language'] })).toThrow(
      'REFUSED_UNKNOWN_AXIS:language'
    );
    expect(() => findAlternatives(graph, '1', { requiredAxes: [] })).toThrow(
      'REFUSED_REQUIRED_AXES_EMPTY'
    );
    expect(() => findAlternatives(graph, '1', { requiredAxes: 'algorithm' })).toThrow(
      'requiredAxes must be an array'
    );
  });

  it('refuses non-positive or non-integer minimumShared', () => {
    for (const bad of [0, -1, 1.5, Number.NaN, '1']) {
      expect(() => findAlternatives(graph, '1', { minimumShared: bad })).toThrow(
        'REFUSED_MINIMUM_SHARED'
      );
    }
  });

  it('excludes candidates below minimumShared', () => {
    expect(
      findAlternatives(graph, '1', { requiredAxes: ['algorithm', 'domain'], minimumShared: 2 })
    ).toHaveLength(1);
    expect(
      findAlternatives(graph, '1', { requiredAxes: ['algorithm', 'domain'], minimumShared: 3 })
    ).toHaveLength(0);
  });
});
