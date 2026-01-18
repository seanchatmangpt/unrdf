/**
 * Composition C04: RDF Canonicalization + Store
 * Atoms: A04 (rdf-canonicalize) + A01 (rdf-store-create)
 *
 * Proof: Canonical RDF graphs with consistent hashing
 */

import { createStore, canonicalize, isIsomorphic, namedNode, literal, quad } from '@unrdf/core';

console.log('=== C04: RDF Canonicalization + Store Proof ===\n');

async function prove() {
  try {
    // A01: Create two stores with isomorphic but differently ordered triples
    const store1 = createStore();
    const store2 = createStore();

    console.log('✅ A01: Created two RDF stores');

    // Add same data in different order
    const alice = namedNode('http://example.org/alice');
    const bob = namedNode('http://example.org/bob');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');

    // Store 1: Alice then Bob
    store1.add(quad(alice, name, literal('Alice')));
    store1.add(quad(bob, name, literal('Bob')));

    // Store 2: Bob then Alice (same data, different order)
    store2.add(quad(bob, name, literal('Bob')));
    store2.add(quad(alice, name, literal('Alice')));

    console.log('✅ Added isomorphic data in different orders');

    // A04: Canonicalize both stores
    const quads1 = Array.from(store1.match());
    const quads2 = Array.from(store2.match());

    const canonical1 = canonicalize(quads1);
    const canonical2 = canonicalize(quads2);

    console.log('✅ A04: Canonicalized both graphs');
    console.log(`   Canonical1: ${canonical1.substring(0, 50)}...`);
    console.log(`   Canonical2: ${canonical2.substring(0, 50)}...`);

    // Verify they're identical
    const areEqual = canonical1 === canonical2;
    const areIsomorphic = isIsomorphic(quads1, quads2);

    console.log('\n✅ COMPOSITION VERIFIED');
    console.log(`   Canonical strings match: ${areEqual}`);
    console.log(`   Graphs are isomorphic: ${areIsomorphic}`);
    console.log('   Value: Deterministic graph hashing for deduplication');

    process.exit(areEqual && areIsomorphic ? 0 : 1);
  } catch (error) {
    console.error('❌ COMPOSITION FAILED:', error.message);
    process.exit(1);
  }
}

prove();
