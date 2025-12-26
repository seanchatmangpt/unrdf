/**
 * Example 01: Hello RDF - Basic RDF Operations
 *
 * This example demonstrates:
 * - Creating an RDF store
 * - Adding triples (quads)
 * - Querying data
 * - Using common RDF namespaces
 *
 * Run: node examples/01-hello-rdf.mjs
 */

import { createStore, namedNode, literal, FOAF, RDF } from '@unrdf/core';

console.log('=== Example 01: Hello RDF ===\n');

// Step 1: Create an RDF store
const store = createStore();
console.log('✅ Created RDF store\n');

// Step 2: Add some basic triples
// Triple: Alice is a Person
store.addQuad(
  namedNode('http://example.org/Alice'),
  RDF.type,
  FOAF.Person
);

// Triple: Alice's name is "Alice Smith"
store.addQuad(
  namedNode('http://example.org/Alice'),
  FOAF.name,
  literal('Alice Smith')
);

// Triple: Alice's email
store.addQuad(
  namedNode('http://example.org/Alice'),
  FOAF.mbox,
  literal('alice@example.com')
);

// Triple: Alice knows Bob
store.addQuad(
  namedNode('http://example.org/Alice'),
  FOAF.knows,
  namedNode('http://example.org/Bob')
);

// Triple: Bob is a Person
store.addQuad(
  namedNode('http://example.org/Bob'),
  RDF.type,
  FOAF.Person
);

// Triple: Bob's name
store.addQuad(
  namedNode('http://example.org/Bob'),
  FOAF.name,
  literal('Bob Johnson')
);

console.log(`✅ Added ${store.size} triples to the store\n`);

// Step 3: Query the data
console.log('--- Alice\'s Properties ---');
const aliceQuads = store.getQuads(
  namedNode('http://example.org/Alice'),
  null,  // any predicate
  null   // any object
);

for (const quad of aliceQuads) {
  const predicate = quad.predicate.value.split('/').pop();
  console.log(`  ${predicate}: ${quad.object.value}`);
}

console.log('\n--- All People ---');
const peopleQuads = store.getQuads(
  null,      // any subject
  RDF.type,  // predicate: rdf:type
  FOAF.Person // object: foaf:Person
);

for (const quad of peopleQuads) {
  const person = quad.subject;
  const nameQuads = store.getQuads(person, FOAF.name, null);
  if (nameQuads.length > 0) {
    console.log(`  - ${nameQuads[0].object.value} (${person.value})`);
  }
}

console.log('\n--- Social Network ---');
const knowsQuads = store.getQuads(
  null,
  FOAF.knows,
  null
);

for (const quad of knowsQuads) {
  const subject = quad.subject;
  const object = quad.object;

  const subjectName = store.getQuads(subject, FOAF.name, null)[0]?.object.value || subject.value;
  const objectName = store.getQuads(object, FOAF.name, null)[0]?.object.value || object.value;

  console.log(`  ${subjectName} knows ${objectName}`);
}

console.log('\n--- Summary ---');
console.log(`Total triples: ${store.size}`);
console.log(`Total people: ${peopleQuads.length}`);
console.log(`Total relationships: ${knowsQuads.length}`);

console.log('\n✅ Example complete!');
console.log('\nNext: Try examples/02-sparql-queries.mjs for more powerful queries');
