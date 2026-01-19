/**
 * @fileoverview Minimal Core Example - Basic RDF Operations
 *
 * Demonstrates the minimal core system using @unrdf/core:
 * - Creating an RDF store
 * - Adding and querying triples
 * - Basic SPARQL queries
 *
 * @version 1.0.0
 * @license MIT
 */

import { createStore, namedNode, literal, executeSelectSync, FOAF, RDF } from '@unrdf/core';

console.log('🚀 Minimal Core System Example');
console.log('='.repeat(50));

// 1) Initialize store
const store = createStore();
console.log('✅ Store initialized');

// 2) Add some basic RDF data
const alice = namedNode('http://example.org/alice');
const bob = namedNode('http://example.org/bob');

store.addQuad(alice, RDF.type, FOAF.Person);
store.addQuad(alice, FOAF.name, literal('Alice'));
store.addQuad(alice, FOAF.mbox, literal('alice@example.com'));

store.addQuad(bob, RDF.type, FOAF.Person);
store.addQuad(bob, FOAF.name, literal('Bob'));

console.log(`✅ Added ${store.size} triples to the store`);

// 3) Query the data
const results = executeSelectSync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name ?email WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
    OPTIONAL { ?person foaf:mbox ?email }
  }
  ORDER BY ?name
`);

console.log('\n📊 Query Results:');
for (const binding of results) {
  const name = binding.get('name')?.value || 'unknown';
  const email = binding.get('email')?.value || 'no email';
  console.log(`  ${name}: ${email}`);
}

console.log('\n✅ Minimal core example complete!');
console.log('Next: Try examples/01-hello-rdf.mjs for more features');
