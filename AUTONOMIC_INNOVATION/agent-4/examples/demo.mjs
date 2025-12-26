/**
 * Impact Set API Demonstration
 *
 * Shows how to use the impact set extraction API to analyze RDF diffs.
 */

import {
  computeImpactSet,
  mergeImpactSets,
  impactSetIntersection,
  formatImpactSummary,
  impactSetToJSON,
} from '../src/index.mjs';

// Helper to create quads
function quad(subject, predicate, object, graph) {
  return {
    subject: { termType: 'NamedNode', value: subject },
    predicate: { termType: 'NamedNode', value: predicate },
    object: { termType: 'Literal', value: object },
    graph: { termType: 'NamedNode', value: graph },
  };
}

console.log('=== Impact Set Extraction Demo ===\n');

// Example 1: Simple capsule with adds and deletes
console.log('Example 1: Computing impact set from a capsule\n');

const capsule1 = {
  delta: {
    add: [
      quad('http://example.org/Person/alice', 'http://xmlns.com/foaf/0.1/name', 'Alice', 'http://example.org/graph1'),
      quad('http://example.org/Person/alice', 'http://xmlns.com/foaf/0.1/age', '30', 'http://example.org/graph1'),
      quad('http://example.org/Person/bob', 'http://xmlns.com/foaf/0.1/name', 'Bob', 'http://example.org/graph2'),
    ],
    del: [
      quad('http://example.org/Person/charlie', 'http://xmlns.com/foaf/0.1/name', 'Charles', 'http://example.org/graph1'),
    ],
  },
  id: 'capsule-001',
};

const impact1 = computeImpactSet(capsule1);
console.log(formatImpactSummary(impact1));

// Example 2: Deterministic JSON serialization
console.log('\nExample 2: Deterministic JSON serialization\n');

const json1 = impactSetToJSON(impact1);
console.log('JSON representation:');
console.log(JSON.stringify(json1, null, 2));

// Example 3: Merging impact sets
console.log('\n\nExample 3: Merging multiple impact sets\n');

const capsule2 = {
  delta: {
    add: [
      quad('http://example.org/Person/david', 'http://xmlns.com/foaf/0.1/name', 'David', 'http://example.org/graph2'),
      quad('http://example.org/Person/david', 'http://xmlns.com/foaf/0.1/email', 'david@example.org', 'http://example.org/graph2'),
    ],
    del: [],
  },
  id: 'capsule-002',
};

const impact2 = computeImpactSet(capsule2);
const mergedImpact = mergeImpactSets([impact1, impact2]);

console.log('Merged impact set:');
console.log(`  Total subjects affected: ${mergedImpact.resourcesAffected.directAffected}`);
console.log(`  Total quads added: ${mergedImpact.cardinality.quadsAdded}`);
console.log(`  Total quads deleted: ${mergedImpact.cardinality.quadsDeleted}`);
console.log(`  Net change: ${mergedImpact.cardinality.netChange}`);

// Example 4: Conflict detection via intersection
console.log('\n\nExample 4: Conflict detection (intersection)\n');

const capsule3 = {
  delta: {
    add: [
      quad('http://example.org/Person/alice', 'http://xmlns.com/foaf/0.1/email', 'alice@example.org', 'http://example.org/graph1'),
    ],
    del: [],
  },
  id: 'capsule-003',
};

const impact3 = computeImpactSet(capsule3);
const intersection = impactSetIntersection(impact1, impact3);

console.log(`Capsule 1 affects ${impact1.subjects.size} subjects: ${[...impact1.subjects].join(', ')}`);
console.log(`Capsule 3 affects ${impact3.subjects.size} subjects: ${[...impact3.subjects].join(', ')}`);
console.log(`Overlapping subjects: ${intersection.subjects.size} (${[...intersection.subjects].join(', ')})`);

if (intersection.subjects.size > 0) {
  console.log('\n⚠️  Potential conflict detected: Both capsules modify the same subject(s)');
}

console.log('\n=== Demo Complete ===');
