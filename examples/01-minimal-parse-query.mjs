/**
 * 01-minimal-parse-query.mjs
 *
 * The simplest UNRDF example: createStore() and executeSelectSync()
 * Demonstrates basic RDF parsing and SPARQL queries.
 *
 * Run: node examples/01-minimal-parse-query.mjs
 */

import { createStore, namedNode, executeSelectSync } from '@unrdf/core';

console.log('=== Minimal Parse & Query Example ===\n');

// Create an RDF store
const store = createStore();

// Add some simple triples (Alice knows Bob, Bob knows Charlie, etc.)
const ex = (name) => namedNode(`http://example.org/${name}`);
const knows = namedNode('http://example.org/knows');

store.addQuad(ex('Alice'), knows, ex('Bob'));
store.addQuad(ex('Bob'), knows, ex('Charlie'));
store.addQuad(ex('Charlie'), knows, ex('Diana'));

console.log(`✅ Added ${store.size} triples to the store\n`);

// Query the store using SPARQL
const results = executeSelectSync(store, `
  SELECT ?person ?knows WHERE {
    ?person <http://example.org/knows> ?knows .
  }
`);

console.log('--- Query Results ---');
for (const binding of results) {
  console.log(`  ${binding.get('person').value} knows ${binding.get('knows').value}`);
}

console.log('\n✅ Example complete!\n');
console.log('Next: Try examples/01-hello-rdf.mjs for more comprehensive examples');
