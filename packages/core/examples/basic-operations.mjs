/**
 * @unrdf/core - Basic RDF Operations Example
 *
 * Demonstrates fundamental store operations:
 * - Creating a store
 * - Adding quads
 * - Querying quads
 * - SPARQL queries
 * - Canonicalization
 */

import {
  createStore,
  addQuad,
  getQuads,
  iterateQuads,
  countQuads,
  namedNode,
  literal,
  defaultGraph,
  executeSelect,
  executeAsk,
  canonicalize,
  createTerms,
  RDF,
  FOAF,
  XSD,
} from '@unrdf/core';

async function main() {
  // Create a new RDF store
  const store = createStore();
  console.log('✅ Created new RDF store');

  // Define some data using namedNode and literal
  const alice = namedNode('http://example.com/alice');
  const bob = namedNode('http://example.com/bob');

  // Add quads to the store
  addQuad(store, {
    subject: alice,
    predicate: RDF.type,
    object: FOAF.Person,
    graph: defaultGraph(),
  });

  addQuad(store, {
    subject: alice,
    predicate: FOAF.name,
    object: literal('Alice'),
    graph: defaultGraph(),
  });

  addQuad(store, {
    subject: alice,
    predicate: FOAF.age,
    object: literal('30', XSD.integer),
    graph: defaultGraph(),
  });

  addQuad(store, {
    subject: bob,
    predicate: RDF.type,
    object: FOAF.Person,
    graph: defaultGraph(),
  });

  addQuad(store, {
    subject: bob,
    predicate: FOAF.name,
    object: literal('Bob'),
    graph: defaultGraph(),
  });

  console.log('✅ Added 5 quads to store');

  // Count quads
  console.log(`\n📊 Total quads in store: ${countQuads(store)}`);

  // Query all quads
  const allQuads = getQuads(store);
  console.log(`📊 Retrieved ${allQuads.length} quads`);

  // Query by subject
  const aliceQuads = getQuads(store, alice);
  console.log(`\n👤 Quads for Alice: ${aliceQuads.length}`);

  // Query by predicate
  const names = getQuads(store, null, FOAF.name);
  console.log(`\n📝 Quads with foaf:name predicate: ${names.length}`);
  names.forEach((quad) => {
    console.log(`   - ${quad.subject.value} => ${quad.object.value}`);
  });

  // Iterate all quads
  console.log('\n🔄 All quads:');
  for (const quad of iterateQuads(store)) {
    console.log(`   ${quad.subject.value}`);
    console.log(`     ${quad.predicate.value}`);
    console.log(`     ${quad.object.value}`);
  }

  // Execute SPARQL SELECT query
  console.log('\n🔍 Executing SPARQL SELECT query:');
  const selectResults = await executeSelect(
    store,
    `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?person ?name WHERE {
      ?person foaf:name ?name
    }
  `
  );

  console.log(`   Found ${selectResults.length} results:`);
  selectResults.forEach((row) => {
    console.log(`   - ${row.person.value}: ${row.name.value}`);
  });

  // Execute SPARQL ASK query
  console.log('\n❓ Executing SPARQL ASK query:');
  const askResult = await executeAsk(
    store,
    `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    ASK {
      ?s foaf:name "Alice"
    }
  `
  );

  console.log(`   Does Alice exist? ${askResult ? 'Yes ✓' : 'No ✗'}`);

  // Canonicalize the store
  console.log('\n🔒 Canonicalizing store:');
  const canonical = await canonicalize(store);
  console.log(`   Canonical form (${canonical.length} chars):`);
  console.log(canonical.split('\n').slice(0, 3).join('\n') + '\n   ...');

  // Create terms from data
  console.log('\n🏗️  Creating terms from data:');
  const terms = createTerms({
    uri: 'http://example.com/charlie',
    name: 'Charlie',
    age: 25,
    active: true,
  });

  console.log(`   URI: ${terms.uri.value} (${terms.uri.termType})`);
  console.log(`   Name: ${terms.name.value} (${terms.name.termType})`);
  console.log(`   Age: ${terms.age.value} (datatype: ${terms.age.datatype.value})`);
  console.log(`   Active: ${terms.active.value} (datatype: ${terms.active.datatype.value})`);

  console.log('\n✅ Example complete!');
}

main().catch((error) => {
  console.error('❌ Error:', error.message);
  process.exit(1);
});
