/**
 * Impact Set Computation - Test Suite
 *
 * Verifies:
 * - Correct impact set computation
 * - Deterministic serialization
 * - Set operations (merge, intersection)
 * - Edge cases (empty capsules, malformed input)
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import {
  computeImpactSet,
  mergeImpactSets,
  impactSetIntersection,
  formatImpactSummary,
  impactSetToJSON,
  impactSetFromJSON,
} from '../src/index.mjs';

/**
 * Helper: Create a test quad
 * @param {string} subject - Subject IRI
 * @param {string} predicate - Predicate IRI
 * @param {string} objectValue - Object value
 * @param {string} [graph] - Graph IRI (optional)
 * @returns {Object} Quad object
 */
function quad(subject, predicate, objectValue, graph) {
  const q = {
    subject: { termType: 'NamedNode', value: subject },
    predicate: { termType: 'NamedNode', value: predicate },
    object: { termType: 'Literal', value: objectValue },
  };

  if (graph) {
    q.graph = { termType: 'NamedNode', value: graph };
  }

  return q;
}

describe('Impact Set Computation', () => {
  it('should compute impact set for simple add operation', () => {
    const capsule = {
      delta: {
        add: [
          {
            subject: { termType: 'NamedNode', value: 'http://ex.org/s1' },
            predicate: { termType: 'NamedNode', value: 'http://ex.org/p1' },
            object: { termType: 'Literal', value: 'value1' },
            graph: { termType: 'NamedNode', value: 'http://ex.org/g1' },
          },
        ],
        del: [],
      },
    };

    const impact = computeImpactSet(capsule);

    assert.equal(impact.subjects.size, 1);
    assert.ok(impact.subjects.has('http://ex.org/s1'));
    assert.ok(impact.predicates.has('http://ex.org/p1'));
    assert.ok(impact.graphs.has('http://ex.org/g1'));
    assert.equal(impact.cardinality.quadsAdded, 1);
    assert.equal(impact.cardinality.quadsDeleted, 0);
    assert.equal(impact.cardinality.netChange, 1);
    assert.equal(impact.resourcesAffected.directAffected, 1);
  });

  it('should handle multiple quads across add and delete', () => {
    const capsule = {
      delta: {
        add: [
          quad('http://ex.org/s1', 'http://ex.org/p1', 'v1', 'http://ex.org/g1'),
          quad('http://ex.org/s2', 'http://ex.org/p2', 'v2', 'http://ex.org/g1'),
          quad('http://ex.org/s1', 'http://ex.org/p3', 'v3', 'http://ex.org/g2'),
        ],
        del: [
          quad('http://ex.org/s3', 'http://ex.org/p1', 'old', 'http://ex.org/g1'),
        ],
      },
    };

    const impact = computeImpactSet(capsule);

    assert.equal(impact.subjects.size, 3); // s1, s2, s3
    assert.equal(impact.predicates.size, 3); // p1, p2, p3
    assert.equal(impact.graphs.size, 2); // g1, g2
    assert.equal(impact.cardinality.quadsAdded, 3);
    assert.equal(impact.cardinality.quadsDeleted, 1);
    assert.equal(impact.cardinality.netChange, 2);
    assert.equal(impact.resourcesAffected.directAffected, 3);
  });

  it('should handle empty capsule', () => {
    const capsule = {
      delta: { add: [], del: [] },
    };

    const impact = computeImpactSet(capsule);

    assert.equal(impact.subjects.size, 0);
    assert.equal(impact.predicates.size, 0);
    assert.equal(impact.graphs.size, 0);
    assert.equal(impact.cardinality.quadsAdded, 0);
    assert.equal(impact.cardinality.quadsDeleted, 0);
    assert.equal(impact.cardinality.netChange, 0);
    assert.equal(impact.resourcesAffected.directAffected, 0);
  });

  it('should use default graph for quads without graph', () => {
    const capsule = {
      delta: {
        add: [
          {
            subject: { termType: 'NamedNode', value: 'http://ex.org/s1' },
            predicate: { termType: 'NamedNode', value: 'http://ex.org/p1' },
            object: { termType: 'Literal', value: 'value1' },
            // No graph specified
          },
        ],
        del: [],
      },
    };

    const impact = computeImpactSet(capsule);

    assert.equal(impact.graphs.size, 1);
    assert.ok(impact.graphs.has('http://kgc.io/Universe'));
  });

  it('should throw error for malformed capsule', () => {
    const malformedCapsule = {
      // Missing delta
      id: 'test',
    };

    assert.throws(
      () => computeImpactSet(malformedCapsule),
      /Invalid capsule/
    );
  });
});

describe('Deterministic Serialization', () => {
  it('should produce deterministic JSON output', () => {
    const capsule = {
      delta: {
        add: [
          quad('http://ex.org/s2', 'http://ex.org/p2', 'v2', 'http://ex.org/g2'),
          quad('http://ex.org/s1', 'http://ex.org/p1', 'v1', 'http://ex.org/g1'),
          quad('http://ex.org/s3', 'http://ex.org/p3', 'v3', 'http://ex.org/g3'),
        ],
        del: [],
      },
    };

    const impact1 = computeImpactSet(capsule);
    const json1 = impactSetToJSON(impact1);

    const impact2 = computeImpactSet(capsule);
    const json2 = impactSetToJSON(impact2);

    // JSON serialization should be identical
    assert.equal(JSON.stringify(json1), JSON.stringify(json2));

    // Arrays should be sorted
    assert.deepEqual(json1.subjects, [...json1.subjects].sort());
    assert.deepEqual(json1.predicates, [...json1.predicates].sort());
    assert.deepEqual(json1.graphs, [...json1.graphs].sort());
  });

  it('should correctly round-trip through JSON', () => {
    const capsule = {
      delta: {
        add: [
          quad('http://ex.org/s1', 'http://ex.org/p1', 'v1', 'http://ex.org/g1'),
          quad('http://ex.org/s2', 'http://ex.org/p2', 'v2', 'http://ex.org/g2'),
        ],
        del: [
          quad('http://ex.org/s3', 'http://ex.org/p1', 'old', 'http://ex.org/g1'),
        ],
      },
    };

    const original = computeImpactSet(capsule);
    const json = impactSetToJSON(original);
    const restored = impactSetFromJSON(json);

    // Restored should have Set objects
    assert.ok(restored.subjects instanceof Set);
    assert.ok(restored.predicates instanceof Set);
    assert.ok(restored.graphs instanceof Set);

    // Content should match
    assert.equal(restored.subjects.size, original.subjects.size);
    assert.deepEqual(
      [...restored.subjects].sort(),
      [...original.subjects].sort()
    );
    assert.deepEqual(
      [...restored.predicates].sort(),
      [...original.predicates].sort()
    );
    assert.deepEqual(
      [...restored.graphs].sort(),
      [...original.graphs].sort()
    );

    // Cardinality should match
    assert.equal(
      restored.cardinality.quadsAdded,
      original.cardinality.quadsAdded
    );
    assert.equal(
      restored.cardinality.quadsDeleted,
      original.cardinality.quadsDeleted
    );
    assert.equal(
      restored.cardinality.netChange,
      original.cardinality.netChange
    );
  });
});

describe('Impact Set Merging', () => {
  it('should merge multiple impact sets', () => {
    const capsule1 = {
      delta: {
        add: [quad('http://ex.org/s1', 'http://ex.org/p1', 'v1', 'http://ex.org/g1')],
        del: [],
      },
    };

    const capsule2 = {
      delta: {
        add: [quad('http://ex.org/s2', 'http://ex.org/p2', 'v2', 'http://ex.org/g2')],
        del: [quad('http://ex.org/s3', 'http://ex.org/p1', 'old', 'http://ex.org/g1')],
      },
    };

    const impact1 = computeImpactSet(capsule1);
    const impact2 = computeImpactSet(capsule2);

    const merged = mergeImpactSets([impact1, impact2]);

    // Union of all subjects
    assert.ok(merged.subjects.size >= Math.max(impact1.subjects.size, impact2.subjects.size));
    assert.equal(merged.subjects.size, 3); // s1, s2, s3

    // Cardinality is summed
    assert.equal(
      merged.cardinality.quadsAdded,
      impact1.cardinality.quadsAdded + impact2.cardinality.quadsAdded
    );
    assert.equal(
      merged.cardinality.quadsDeleted,
      impact1.cardinality.quadsDeleted + impact2.cardinality.quadsDeleted
    );
    assert.equal(merged.cardinality.quadsAdded, 2);
    assert.equal(merged.cardinality.quadsDeleted, 1);
    assert.equal(merged.cardinality.netChange, 1);
  });

  it('should handle empty merge', () => {
    const merged = mergeImpactSets([]);

    assert.equal(merged.subjects.size, 0);
    assert.equal(merged.predicates.size, 0);
    assert.equal(merged.graphs.size, 0);
    assert.equal(merged.cardinality.quadsAdded, 0);
    assert.equal(merged.cardinality.quadsDeleted, 0);
  });
});

describe('Impact Set Intersection', () => {
  it('should compute intersection of impact sets', () => {
    const capsule1 = {
      delta: {
        add: [
          quad('http://ex.org/s1', 'http://ex.org/p1', 'v1', 'http://ex.org/g1'),
          quad('http://ex.org/s2', 'http://ex.org/p2', 'v2', 'http://ex.org/g2'),
          quad('http://ex.org/s3', 'http://ex.org/p3', 'v3', 'http://ex.org/g3'),
        ],
        del: [],
      },
    };

    const capsule2 = {
      delta: {
        add: [
          quad('http://ex.org/s2', 'http://ex.org/p2', 'v2', 'http://ex.org/g2'),
          quad('http://ex.org/s3', 'http://ex.org/p3', 'v3', 'http://ex.org/g3'),
          quad('http://ex.org/s4', 'http://ex.org/p4', 'v4', 'http://ex.org/g4'),
        ],
        del: [],
      },
    };

    const impact1 = computeImpactSet(capsule1);
    const impact2 = computeImpactSet(capsule2);
    const intersection = impactSetIntersection(impact1, impact2);

    // Only s2 and s3 are in common
    assert.equal(intersection.subjects.size, 2);
    assert.ok(intersection.subjects.has('http://ex.org/s2'));
    assert.ok(intersection.subjects.has('http://ex.org/s3'));

    // Only p2 and p3 are in common
    assert.equal(intersection.predicates.size, 2);
    assert.ok(intersection.predicates.has('http://ex.org/p2'));
    assert.ok(intersection.predicates.has('http://ex.org/p3'));

    // Only g2 and g3 are in common
    assert.equal(intersection.graphs.size, 2);
    assert.ok(intersection.graphs.has('http://ex.org/g2'));
    assert.ok(intersection.graphs.has('http://ex.org/g3'));
  });

  it('should handle empty intersection', () => {
    const capsule1 = {
      delta: {
        add: [quad('http://ex.org/s1', 'http://ex.org/p1', 'v1', 'http://ex.org/g1')],
        del: [],
      },
    };

    const capsule2 = {
      delta: {
        add: [quad('http://ex.org/s2', 'http://ex.org/p2', 'v2', 'http://ex.org/g2')],
        del: [],
      },
    };

    const impact1 = computeImpactSet(capsule1);
    const impact2 = computeImpactSet(capsule2);
    const intersection = impactSetIntersection(impact1, impact2);

    assert.equal(intersection.subjects.size, 0);
    assert.equal(intersection.predicates.size, 0);
    assert.equal(intersection.graphs.size, 0);
  });
});

describe('Human-Readable Formatting', () => {
  it('should format impact set as human-readable text', () => {
    const capsule = {
      delta: {
        add: [
          quad('http://ex.org/s1', 'http://ex.org/p1', 'v1', 'http://ex.org/g1'),
          quad('http://ex.org/s2', 'http://ex.org/p2', 'v2', 'http://ex.org/g2'),
        ],
        del: [
          quad('http://ex.org/s3', 'http://ex.org/p1', 'old', 'http://ex.org/g1'),
        ],
      },
    };

    const impact = computeImpactSet(capsule);
    const summary = formatImpactSummary(impact);

    assert.ok(summary.includes('Impact Set Summary'));
    assert.ok(summary.includes('Resources Affected:'));
    assert.ok(summary.includes('quads added'));
    assert.ok(summary.includes('quads deleted'));
    assert.equal(typeof summary, 'string');
    assert.ok(summary.includes('http://ex.org/s1'));
    assert.ok(summary.includes('http://ex.org/p1'));
    assert.ok(summary.includes('http://ex.org/g1'));
  });

  it('should limit displayed items based on maxItems option', () => {
    const capsule = {
      delta: {
        add: Array.from({ length: 20 }, (_, i) =>
          quad(`http://ex.org/s${i}`, 'http://ex.org/p1', 'v', 'http://ex.org/g1')
        ),
        del: [],
      },
    };

    const impact = computeImpactSet(capsule);
    const summary = formatImpactSummary(impact, { maxItems: 5 });

    assert.ok(summary.includes('... and 15 more'));
  });
});
