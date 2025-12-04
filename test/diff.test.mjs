/**
 * @fileoverview Comprehensive TDD tests for diff.mjs
 * Uses London School (mock-driven) approach with strict isolation
 * @vitest-environment node
 */

import { describe, it, expect, _beforeEach, vi } from 'vitest';
import { z } from 'zod';
import {
  diffGraphFromStores,
  diffGraphFromDelta,
  diffOntologyFromGraphDiff,
  diffOntologyFromStores,
  diffOntologyFromDelta,
  summarizeChangesByKind,
  changesForEntity,
  quadToDiffTriple,
  diffTripleKey,
  collectDiffTriplesFromStore,
  DiffTripleSchema,
  GraphDiffSchema,
  OntologyChangeSchema,
  OntologyDiffSchema,
  DeltaLikeSchema,
} from '../src/diff.mjs';

/* ========================================================================= */
/* Test Fixtures & Mocks                                                    */
/* ========================================================================= */

/**
 * Create a mock RDF/JS Quad
 */
function createQuad(subjectValue, predicateValue, objectValue) {
  return {
    subject: { value: subjectValue },
    predicate: { value: predicateValue },
    object: { value: objectValue },
    graph: { value: '' },
  };
}

/**
 * Create a mock store with controlled quads
 */
function createMockStore(quads = []) {
  return {
    getQuads: vi.fn((_s, _p, _o, _g) => quads),
  };
}

/**
 * Simple ontology lens for testing
 */
function testLens(triple, direction) {
  const { subject, predicate, object } = triple;

  // Rule 1: Feature additions/removals
  if (
    predicate === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
    object === 'http://example.org/ontology#Feature'
  ) {
    return {
      kind: direction === 'added' ? 'FeatureAdded' : 'FeatureRemoved',
      entity: subject,
    };
  }

  // Rule 2: Role assignments
  if (predicate === 'http://example.org/ontology#hasRole') {
    return {
      kind: direction === 'added' ? 'RoleAdded' : 'RoleRemoved',
      entity: subject,
      role: object,
    };
  }

  return null;
}

/**
 * Lens that ignores everything
 */
function noOpLens() {
  return null;
}

/* ========================================================================= */
/* Unit Tests: Internal Helpers                                             */
/* ========================================================================= */

describe('diff.mjs - Internal Helpers', () => {
  describe('quadToDiffTriple', () => {
    it('converts a quad to DiffTriple with correct IRI values', () => {
      const quad = createQuad(
        'http://example.org/s',
        'http://example.org/p',
        'http://example.org/o'
      );
      const triple = quadToDiffTriple(quad);

      expect(triple).toEqual({
        subject: 'http://example.org/s',
        predicate: 'http://example.org/p',
        object: 'http://example.org/o',
      });
    });
  });

  describe('diffTripleKey', () => {
    it('creates stable key from DiffTriple components', () => {
      const triple = {
        subject: 'http://example.org/s',
        predicate: 'http://example.org/p',
        object: 'http://example.org/o',
      };
      const key = diffTripleKey(triple);

      expect(key).toBe('http://example.org/s http://example.org/p http://example.org/o');
    });

    it('creates different keys for different triples', () => {
      const triple1 = {
        subject: 'http://example.org/alice',
        predicate: 'http://example.org/knows',
        object: 'http://example.org/bob',
      };
      const triple2 = {
        subject: 'http://example.org/alice',
        predicate: 'http://example.org/knows',
        object: 'http://example.org/charlie',
      };

      const key1 = diffTripleKey(triple1);
      const key2 = diffTripleKey(triple2);

      expect(key1).not.toBe(key2);
    });
  });

  describe('collectDiffTriplesFromStore', () => {
    it('collects all quads from store as DiffTriples', () => {
      const quads = [
        createQuad('http://ex.org/alice', 'http://ex.org/name', 'Alice'),
        createQuad('http://ex.org/bob', 'http://ex.org/name', 'Bob'),
      ];
      const store = createMockStore(quads);

      const triples = collectDiffTriplesFromStore(store);

      expect(triples).toHaveLength(2);
      expect(triples[0].subject).toBe('http://ex.org/alice');
      expect(triples[1].subject).toBe('http://ex.org/bob');
    });

    it('throws error if store lacks getQuads method', () => {
      const invalidStore = {};

      expect(() => {
        collectDiffTriplesFromStore(invalidStore);
      }).toThrow(TypeError);
    });
  });
});

/* ========================================================================= */
/* Unit Tests: Graph Diff                                                   */
/* ========================================================================= */

describe('diff.mjs - Graph Diff', () => {
  describe('diffGraphFromStores', () => {
    it('detects added and removed triples', () => {
      const before = createMockStore([
        createQuad('http://ex.org/alice', 'http://ex.org/name', 'Alice'),
        createQuad('http://ex.org/alice', 'http://ex.org/oldProp', 'oldValue'),
      ]);
      const after = createMockStore([
        createQuad('http://ex.org/alice', 'http://ex.org/name', 'Alice'),
        createQuad('http://ex.org/alice', 'http://ex.org/newProp', 'newValue'),
      ]);

      const diff = diffGraphFromStores(before, after);

      expect(diff.added).toHaveLength(1);
      expect(diff.removed).toHaveLength(1);
      expect(diff.added[0].predicate).toBe('http://ex.org/newProp');
      expect(diff.removed[0].predicate).toBe('http://ex.org/oldProp');
    });

    it('validates result against GraphDiffSchema', () => {
      const before = createMockStore([]);
      const after = createMockStore([
        createQuad('http://ex.org/s', 'http://ex.org/p', 'http://ex.org/o'),
      ]);

      const diff = diffGraphFromStores(before, after);

      // Should not throw
      expect(() => GraphDiffSchema.parse(diff)).not.toThrow();
    });
  });

  describe('diffGraphFromDelta', () => {
    it('creates diff from delta with both additions and removals', () => {
      const delta = {
        additions: [createQuad('http://ex.org/alice', 'http://ex.org/newProp', 'newValue')],
        removals: [createQuad('http://ex.org/alice', 'http://ex.org/oldProp', 'oldValue')],
      };

      const diff = diffGraphFromDelta(delta);

      expect(diff.added).toHaveLength(1);
      expect(diff.removed).toHaveLength(1);
    });

    it('validates delta schema before processing', () => {
      const invalidDelta = {
        additions: [{ incomplete: 'object' }],
        removals: [],
      };

      expect(() => {
        diffGraphFromDelta(invalidDelta);
      }).toThrow(z.ZodError);
    });
  });
});

/* ========================================================================= */
/* Unit Tests: Ontology Diff                                                */
/* ========================================================================= */

describe('diff.mjs - Ontology Diff', () => {
  describe('diffOntologyFromGraphDiff', () => {
    it('applies lens to added triples', () => {
      const graphDiff = {
        added: [
          {
            subject: 'http://ex.org/feature1',
            predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
            object: 'http://example.org/ontology#Feature',
          },
        ],
        removed: [],
      };

      const ontologyDiff = diffOntologyFromGraphDiff(graphDiff, testLens);

      expect(ontologyDiff.changes).toHaveLength(1);
      expect(ontologyDiff.changes[0].kind).toBe('FeatureAdded');
      expect(ontologyDiff.changes[0].entity).toBe('http://ex.org/feature1');
    });

    it('applies lens to removed triples', () => {
      const graphDiff = {
        added: [],
        removed: [
          {
            subject: 'http://ex.org/feature1',
            predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
            object: 'http://example.org/ontology#Feature',
          },
        ],
      };

      const ontologyDiff = diffOntologyFromGraphDiff(graphDiff, testLens);

      expect(ontologyDiff.changes).toHaveLength(1);
      expect(ontologyDiff.changes[0].kind).toBe('FeatureRemoved');
    });

    it('filters triples that lens returns null for', () => {
      const graphDiff = {
        added: [
          {
            subject: 'http://ex.org/anything',
            predicate: 'http://ex.org/irrelevant',
            object: 'http://ex.org/value',
          },
        ],
        removed: [],
      };

      const ontologyDiff = diffOntologyFromGraphDiff(graphDiff, noOpLens);

      expect(ontologyDiff.changes).toHaveLength(0);
    });

    it('includes triples in ontology diff even when lens finds nothing', () => {
      const graphDiff = {
        added: [
          {
            subject: 'http://ex.org/s',
            predicate: 'http://ex.org/p',
            object: 'http://ex.org/o',
          },
        ],
        removed: [],
      };

      const ontologyDiff = diffOntologyFromGraphDiff(graphDiff, noOpLens);

      expect(ontologyDiff.triples.added).toHaveLength(1);
      expect(ontologyDiff.changes).toHaveLength(0);
    });

    it('processes both additions and removals with lens', () => {
      const graphDiff = {
        added: [
          {
            subject: 'http://ex.org/feature1',
            predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
            object: 'http://example.org/ontology#Feature',
          },
        ],
        removed: [
          {
            subject: 'http://ex.org/feature2',
            predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
            object: 'http://example.org/ontology#Feature',
          },
        ],
      };

      const ontologyDiff = diffOntologyFromGraphDiff(graphDiff, testLens);

      expect(ontologyDiff.changes).toHaveLength(2);
      expect(ontologyDiff.changes[0].kind).toBe('FeatureAdded');
      expect(ontologyDiff.changes[1].kind).toBe('FeatureRemoved');
    });

    it('validates result against OntologyDiffSchema', () => {
      const graphDiff = {
        added: [],
        removed: [],
      };

      const ontologyDiff = diffOntologyFromGraphDiff(graphDiff, noOpLens);

      // Should not throw
      expect(() => OntologyDiffSchema.parse(ontologyDiff)).not.toThrow();
    });

    it('lens receives correct triple direction parameter', () => {
      const lensWithTracking = vi.fn((triple, direction) => {
        if (direction === 'added') {
          return { kind: 'Tracked' };
        }
        return null;
      });

      const graphDiff = {
        added: [
          {
            subject: 'http://ex.org/s',
            predicate: 'http://ex.org/p',
            object: 'http://ex.org/o',
          },
        ],
        removed: [],
      };

      diffOntologyFromGraphDiff(graphDiff, lensWithTracking);

      expect(lensWithTracking).toHaveBeenCalledWith(expect.any(Object), 'added');
    });
  });

  describe('diffOntologyFromStores', () => {
    it('combines graph diff and ontology lens', () => {
      const before = createMockStore([]);
      const after = createMockStore([
        createQuad(
          'http://ex.org/feature1',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://example.org/ontology#Feature'
        ),
      ]);

      const ontologyDiff = diffOntologyFromStores(before, after, testLens);

      expect(ontologyDiff.triples.added).toHaveLength(1);
      expect(ontologyDiff.changes).toHaveLength(1);
      expect(ontologyDiff.changes[0].kind).toBe('FeatureAdded');
    });

    it('applies lens after computing graph diff', () => {
      const quads = [createQuad('http://ex.org/alice', 'http://ex.org/knows', 'http://ex.org/bob')];
      const before = createMockStore([]);
      const after = createMockStore(quads);

      const ontologyDiff = diffOntologyFromStores(before, after, testLens);

      // The triple is detected but lens returns null for it
      expect(ontologyDiff.triples.added).toHaveLength(1);
      expect(ontologyDiff.changes).toHaveLength(0);
    });
  });

  describe('diffOntologyFromDelta', () => {
    it('combines delta and ontology lens', () => {
      const delta = {
        additions: [
          createQuad(
            'http://ex.org/feature1',
            'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
            'http://example.org/ontology#Feature'
          ),
        ],
        removals: [],
      };

      const ontologyDiff = diffOntologyFromDelta(delta, testLens);

      expect(ontologyDiff.triples.added).toHaveLength(1);
      expect(ontologyDiff.changes).toHaveLength(1);
      expect(ontologyDiff.changes[0].kind).toBe('FeatureAdded');
    });

    it('processes delta without re-scanning store', () => {
      // This test verifies the efficiency benefit
      const delta = {
        additions: [
          createQuad('http://ex.org/s1', 'http://ex.org/p1', 'http://ex.org/o1'),
          createQuad('http://ex.org/s2', 'http://ex.org/p2', 'http://ex.org/o2'),
        ],
        removals: [],
      };

      const ontologyDiff = diffOntologyFromDelta(delta, noOpLens);

      // Should have 2 added triples
      expect(ontologyDiff.triples.added).toHaveLength(2);
    });
  });
});

/* ========================================================================= */
/* Unit Tests: Summarization Functions                                      */
/* ========================================================================= */

describe('diff.mjs - Summarization', () => {
  describe('summarizeChangesByKind', () => {
    it('counts changes by kind', () => {
      const ontologyDiff = {
        triples: { added: [], removed: [] },
        changes: [
          { kind: 'FeatureAdded', entity: 'f1' },
          { kind: 'FeatureAdded', entity: 'f2' },
          { kind: 'FeatureRemoved', entity: 'f3' },
        ],
      };

      const summary = summarizeChangesByKind(ontologyDiff);

      expect(summary.FeatureAdded).toBe(2);
      expect(summary.FeatureRemoved).toBe(1);
    });

    it('validates ontology diff before summarizing', () => {
      const invalidDiff = {
        triples: { added: [], removed: [] },
        changes: [{ incomplete: 'object' }],
      };

      expect(() => {
        summarizeChangesByKind(invalidDiff);
      }).toThrow(z.ZodError);
    });
  });

  describe('changesForEntity', () => {
    it('filters changes for specific entity', () => {
      const ontologyDiff = {
        triples: { added: [], removed: [] },
        changes: [
          { kind: 'FeatureAdded', entity: 'http://ex.org/feature1' },
          { kind: 'RoleAdded', entity: 'http://ex.org/feature1' },
          { kind: 'FeatureRemoved', entity: 'http://ex.org/feature2' },
        ],
      };

      const changes = changesForEntity(ontologyDiff, 'http://ex.org/feature1');

      expect(changes).toHaveLength(2);
      expect(changes.every(c => c.entity === 'http://ex.org/feature1')).toBe(true);
    });
  });
});

/* ========================================================================= */
/* Schema Validation Tests                                                   */
/* ========================================================================= */

describe('diff.mjs - Schema Validation', () => {
  describe('DiffTripleSchema', () => {
    it('validates correct DiffTriple', () => {
      const triple = {
        subject: 'http://ex.org/s',
        predicate: 'http://ex.org/p',
        object: 'http://ex.org/o',
      };

      expect(() => DiffTripleSchema.parse(triple)).not.toThrow();
    });

    it('rejects missing subject', () => {
      const triple = {
        predicate: 'http://ex.org/p',
        object: 'http://ex.org/o',
      };

      expect(() => DiffTripleSchema.parse(triple)).toThrow(z.ZodError);
    });
  });

  describe('GraphDiffSchema', () => {
    it('validates correct GraphDiff', () => {
      const diff = {
        added: [
          {
            subject: 'http://ex.org/s',
            predicate: 'http://ex.org/p',
            object: 'http://ex.org/o',
          },
        ],
        removed: [],
      };

      expect(() => GraphDiffSchema.parse(diff)).not.toThrow();
    });
  });

  describe('OntologyChangeSchema', () => {
    it('validates minimal change (kind only)', () => {
      const change = { kind: 'FeatureAdded' };

      expect(() => OntologyChangeSchema.parse(change)).not.toThrow();
    });
  });

  describe('DeltaLikeSchema', () => {
    it('validates correct Delta structure', () => {
      const delta = {
        additions: [createQuad('http://ex.org/s', 'http://ex.org/p', 'http://ex.org/o')],
        removals: [],
      };

      expect(() => DeltaLikeSchema.parse(delta)).not.toThrow();
    });
  });
});

/* ========================================================================= */
/* Integration Tests: End-to-End Scenarios                                  */
/* ========================================================================= */

describe('diff.mjs - Integration', () => {
  it('complete flow: stores -> graph diff -> ontology diff', () => {
    const before = createMockStore([
      createQuad('http://ex.org/feature1', 'http://ex.org/status', 'active'),
    ]);
    const after = createMockStore([
      createQuad('http://ex.org/feature1', 'http://ex.org/status', 'active'),
      createQuad(
        'http://ex.org/feature2',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://example.org/ontology#Feature'
      ),
    ]);

    const ontologyDiff = diffOntologyFromStores(before, after, testLens);

    expect(ontologyDiff.triples.added).toHaveLength(1);
    expect(ontologyDiff.changes).toHaveLength(1);
    expect(ontologyDiff.changes[0].kind).toBe('FeatureAdded');
  });

  it('complete flow: delta -> ontology diff', () => {
    const delta = {
      additions: [
        createQuad(
          'http://ex.org/feature1',
          'http://example.org/ontology#hasRole',
          'http://ex.org/admin'
        ),
      ],
      removals: [],
    };

    const ontologyDiff = diffOntologyFromDelta(delta, testLens);

    expect(ontologyDiff.changes).toHaveLength(1);
    expect(ontologyDiff.changes[0].kind).toBe('RoleAdded');
    expect(ontologyDiff.changes[0].role).toBe('http://ex.org/admin');
  });

  it('summarize and filter changes from ontology diff', () => {
    const delta = {
      additions: [
        createQuad(
          'http://ex.org/feature1',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://example.org/ontology#Feature'
        ),
        createQuad(
          'http://ex.org/feature1',
          'http://example.org/ontology#hasRole',
          'http://ex.org/role1'
        ),
        createQuad(
          'http://ex.org/feature2',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://example.org/ontology#Feature'
        ),
      ],
      removals: [],
    };

    const ontologyDiff = diffOntologyFromDelta(delta, testLens);
    const summary = summarizeChangesByKind(ontologyDiff);
    const feature1Changes = changesForEntity(ontologyDiff, 'http://ex.org/feature1');

    expect(summary.FeatureAdded).toBe(2);
    expect(summary.RoleAdded).toBe(1);
    expect(feature1Changes).toHaveLength(2);
  });

  it('handles complex multi-entity diff with all operations', () => {
    const before = createMockStore([
      createQuad(
        'http://ex.org/feature1',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://example.org/ontology#Feature'
      ),
      createQuad('http://ex.org/feature1', 'http://ex.org/version', '1.0'),
    ]);
    const after = createMockStore([
      createQuad(
        'http://ex.org/feature1',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://example.org/ontology#Feature'
      ),
      createQuad('http://ex.org/feature1', 'http://ex.org/version', '2.0'),
      createQuad(
        'http://ex.org/feature2',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://example.org/ontology#Feature'
      ),
    ]);

    const ontologyDiff = diffOntologyFromStores(before, after, testLens);

    // Triple-level: 2 added (new version + new feature type), 1 removed (old version)
    // Ontology-level: 1 FeatureAdded (only feature2 matches the lens, version properties don't)
    expect(ontologyDiff.triples.added).toHaveLength(2);
    expect(ontologyDiff.triples.removed).toHaveLength(1);
    expect(ontologyDiff.changes).toHaveLength(1);
    expect(ontologyDiff.changes[0].kind).toBe('FeatureAdded');
    expect(ontologyDiff.changes[0].entity).toBe('http://ex.org/feature2');
  });
});

/* ========================================================================= */
/* Edge Cases & Error Handling                                              */
/* ========================================================================= */

describe('diff.mjs - Edge Cases', () => {
  it('handles large diffs efficiently', () => {
    // Create 1000 quads
    const largeQuadSet = Array.from({ length: 1000 }, (_, i) =>
      createQuad(`http://ex.org/s${i}`, 'http://ex.org/p', `http://ex.org/o${i}`)
    );

    const before = createMockStore([]);
    const after = createMockStore(largeQuadSet);

    const start = performance.now();
    const diff = diffGraphFromStores(before, after);
    const duration = performance.now() - start;

    expect(diff.added).toHaveLength(1000);
    expect(duration).toBeLessThan(1000); // Should complete in <1s
  });
});
