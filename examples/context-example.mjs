/**
 * @fileoverview Example demonstrating RDF store operations
 *
 * This example shows how to create and query an RDF store
 * using the current @unrdf/core API.
 *
 * @version 1.0.0
 * @license MIT
 */

import { createStore, namedNode, literal, executeSelectSync } from '@unrdf/core';

// Example 1: Basic store usage
console.log('=== Example 1: Basic Store Usage ===');

// Create an RDF store
const store = createStore();

// Create some RDF data
const subject = namedNode('http://example.org/person1');
const predicate = namedNode('http://example.org/name');
const object = literal('John Doe');

// Add to the store
store.addQuad(subject, predicate, object);

console.log(`Store size: ${store.size}`);

// Query the data
const results = executeSelectSync(store, `
  PREFIX ex: <http://example.org/>
  SELECT ?s ?p ?o WHERE { ?s ?p ?o }
`);

console.log('Query results:');
for (const binding of results) {
  console.log(`  ${binding.get('s').value} ${binding.get('p').value} ${binding.get('o').value}`);
}

// Example 2: Adding multiple triples
console.log('\n=== Example 2: Multiple Triples ===');

const person2 = namedNode('http://example.org/person2');
const age = namedNode('http://example.org/age');
const ageValue = literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'));

store.addQuad(person2, age, ageValue);

console.log(`Store size after adding: ${store.size}`);

// Example 3: Querying specific patterns
console.log('\n=== Example 3: Pattern Matching ===');

const allPeople = executeSelectSync(store, `
  PREFIX ex: <http://example.org/>
  SELECT ?person ?name WHERE {
    ?person ex:name ?name .
  }
`);

console.log('People found:');
for (const binding of allPeople) {
  console.log(`  ${binding.get('person').value}: ${binding.get('name').value}`);
}

console.log('\n✅ Example complete!');
