/**
 * @file Graph Diff Example
 * @description Demonstrates diffGraphFromStores, diffGraphFromDelta, diffOntologyFromGraphDiff
 *
 * API demonstrated: packages/core/src/diff.mjs
 * Docs: packages/core/README.md — "Graph Diff" section
 *
 * The diff functions compute triple-level and ontology-level differences between
 * RDF graphs. They are pure: no network, no async, no side effects.
 *
 * EXPECTED OUTPUT:
 *   [diffGraphFromStores] added N, removed M when adding/removing triples
 *   [diffGraphFromDelta] converts delta {additions, removals} to GraphDiff
 *   [diffOntologyFromGraphDiff] applies a lens to classify changes by kind
 *   [idempotent] diffing a store against itself returns empty diff
 */

import {
  diffGraphFromStores,
  diffGraphFromDelta,
  diffOntologyFromGraphDiff,
} from '../packages/core/src/diff.mjs';

const FOAF_NAME = 'http://xmlns.com/foaf/0.1/name';
const FOAF_KNOWS = 'http://xmlns.com/foaf/0.1/knows';
const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';
const FOAF_PERSON = 'http://xmlns.com/foaf/0.1/Person';
const ALICE = 'https://example.org/Alice';
const BOB = 'https://example.org/Bob';
const CAROL = 'https://example.org/Carol';

// diffGraphFromStores requires store.match(null,null,null,null) → iterable of quads
// Use a duck-typed mock store: no oxigraph dependency needed for pure diff logic
function makeStore(quads) {
  return {
    match: () => quads,
    getQuads: () => quads,
  };
}

function q(s, p, o, isLiteral = false) {
  return {
    subject: { termType: 'NamedNode', value: s },
    predicate: { termType: 'NamedNode', value: p },
    object: isLiteral
      ? { termType: 'Literal', value: o }
      : { termType: 'NamedNode', value: o },
    graph: { termType: 'DefaultGraph', value: '' },
  };
}

// ─── Setup: before and after stores ──────────────────────────────────────────

const before = makeStore([
  q(ALICE, RDF_TYPE, FOAF_PERSON),
  q(ALICE, FOAF_NAME, 'Alice', true),
  q(ALICE, FOAF_KNOWS, BOB),
]);

const after = makeStore([
  q(ALICE, RDF_TYPE, FOAF_PERSON),   // unchanged
  q(ALICE, FOAF_NAME, 'Alice', true), // unchanged
  // removed: ALICE foaf:knows BOB
  q(ALICE, FOAF_KNOWS, CAROL),       // new: ALICE foaf:knows CAROL
  q(BOB, RDF_TYPE, FOAF_PERSON),     // new
]);

// ─── 1. diffGraphFromStores ───────────────────────────────────────────────────

const diff = diffGraphFromStores(before, after);

if (!Array.isArray(diff.added)) throw new Error('diff.added must be an array');
if (!Array.isArray(diff.removed)) throw new Error('diff.removed must be an array');
if (diff.added.length !== 2) throw new Error(`Expected 2 added triples, got ${diff.added.length}`);
if (diff.removed.length !== 1) throw new Error(`Expected 1 removed triple, got ${diff.removed.length}`);

const removedTriple = diff.removed[0];
if (removedTriple.predicate !== FOAF_KNOWS) {
  throw new Error(`Expected removed predicate foaf:knows, got ${removedTriple.predicate}`);
}
if (removedTriple.object !== BOB) {
  throw new Error(`Expected removed object Bob, got ${removedTriple.object}`);
}

console.log(`[diffGraphFromStores] ✅ added=${diff.added.length}, removed=${diff.removed.length}`);
console.log(`[diffGraphFromStores]   removed: ${removedTriple.subject} → foaf:knows → ${removedTriple.object}`);

// ─── 2. diffGraphFromStores — idempotent (diff of store against itself) ───────

const identityDiff = diffGraphFromStores(before, before);
if (identityDiff.added.length !== 0 || identityDiff.removed.length !== 0) {
  throw new Error(`Diff of store against itself must be empty, got added=${identityDiff.added.length}, removed=${identityDiff.removed.length}`);
}
console.log(`[diffGraphFromStores] ✅ idempotent: diff(store, store) → {added:0, removed:0}`);

// ─── 3. diffGraphFromDelta ────────────────────────────────────────────────────

const delta = {
  additions: [q(BOB, FOAF_KNOWS, CAROL)],
  removals: [q(ALICE, FOAF_KNOWS, BOB)],
};

const deltaResult = diffGraphFromDelta(delta);
if (deltaResult.added.length !== 1) throw new Error(`Expected 1 added, got ${deltaResult.added.length}`);
if (deltaResult.removed.length !== 1) throw new Error(`Expected 1 removed, got ${deltaResult.removed.length}`);

const addedTriple = deltaResult.added[0];
if (addedTriple.subject !== BOB || addedTriple.object !== CAROL) {
  throw new Error(`Expected Bob→Carol in added, got ${addedTriple.subject}→${addedTriple.object}`);
}

console.log(`[diffGraphFromDelta] ✅ added=${deltaResult.added.length}, removed=${deltaResult.removed.length}`);

// ─── 4. diffOntologyFromGraphDiff ────────────────────────────────────────────

// A lens classifies changes by predicate semantics
function typeLens(triple, direction) {
  if (triple.predicate === RDF_TYPE) {
    return { kind: direction === 'added' ? 'class-assertion-added' : 'class-assertion-removed', triple };
  }
  if (triple.predicate === FOAF_KNOWS) {
    return { kind: direction === 'added' ? 'relationship-added' : 'relationship-removed', triple };
  }
  return null; // ignore this triple
}

const ontologyDiff = diffOntologyFromGraphDiff(diff, typeLens);

if (!ontologyDiff.changes) throw new Error('Expected ontologyDiff.changes array');
if (!ontologyDiff.triples) throw new Error('Expected ontologyDiff.triples (the underlying GraphDiff)');

// diff has 2 added (Bob type + Alice knowsCarol) and 1 removed (Alice knowsBob)
// type lens: Bob type → class-assertion-added; Alice knowsCarol → relationship-added; Alice knowsBob removed → relationship-removed
// Alice name is ignored by the lens (no match)
const kinds = ontologyDiff.changes.map(c => c.kind);
if (!kinds.includes('class-assertion-added')) throw new Error(`Expected class-assertion-added, got: ${kinds}`);
if (!kinds.includes('relationship-removed')) throw new Error(`Expected relationship-removed, got: ${kinds}`);

console.log(`[diffOntologyFromGraphDiff] ✅ ${ontologyDiff.changes.length} classified changes:`);
for (const c of ontologyDiff.changes) {
  console.log(`  - ${c.kind}`);
}

console.log('\n✅ All graph diff assertions passed.');
