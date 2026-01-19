/**
 * @file Dark Matter 80/20 Example - Query Optimization
 * @module dark-matter-80-20-example
 *
 * @description
 * Example demonstrating query optimization using the 80/20 principle:
 * Focus on the 20% of optimizations that deliver 80% of the performance gains.
 */

import { createStore, namedNode, literal, executeSelectSync, FOAF, RDF } from '@unrdf/core';

console.log('🌌 Dark Matter 80/20 Query Optimization Example');
console.log('='.repeat(50));

// Create a larger dataset for optimization testing
console.log('\n📊 Creating test dataset...');
const store = createStore();

// Add 100 people with various properties
for (let i = 0; i < 100; i++) {
  const person = namedNode(`http://example.org/person${i}`);
  store.addQuad(person, RDF.type, FOAF.Person);
  store.addQuad(person, FOAF.name, literal(`Person ${i}`));
  store.addQuad(person, namedNode('http://example.org/age'), literal(String(20 + (i % 50))));

  // Add social connections (20% of people know 80% of others)
  if (i < 20) {
    for (let j = 0; j < 80; j++) {
      const friend = namedNode(`http://example.org/person${j}`);
      store.addQuad(person, FOAF.knows, friend);
    }
  }
}

console.log(`✅ Created dataset with ${store.size} triples`);

// 80/20 Optimization 1: Selective Queries (20% of queries handle 80% of use cases)
console.log('\n🎯 Optimization 1: Core Queries (80/20 Principle)');

const startTime1 = performance.now();
const coreQuery = executeSelectSync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
  }
  LIMIT 10
`);
const duration1 = performance.now() - startTime1;

console.log(`   Query executed in ${duration1.toFixed(2)}ms`);
console.log(`   Results: ${coreQuery.length} people`);

// 80/20 Optimization 2: Indexed Lookups (Most common access pattern)
console.log('\n🎯 Optimization 2: Filtered Queries (Common Pattern)');

const startTime2 = performance.now();
const filteredQuery = executeSelectSync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  SELECT ?person ?name ?age WHERE {
    ?person a foaf:Person ;
            foaf:name ?name ;
            ex:age ?age .
    FILTER(?age >= "30")
  }
  LIMIT 20
`);
const duration2 = performance.now() - startTime2;

console.log(`   Query executed in ${duration2.toFixed(2)}ms`);
console.log(`   Results: ${filteredQuery.length} matching people`);

// 80/20 Optimization 3: Social Network Queries (Most valuable insights)
console.log('\n🎯 Optimization 3: Social Network Analysis');

const startTime3 = performance.now();
const socialQuery = executeSelectSync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name (COUNT(?friend) AS ?connections) WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
    OPTIONAL { ?person foaf:knows ?friend }
  }
  GROUP BY ?person ?name
  ORDER BY DESC(?connections)
  LIMIT 10
`);
const duration3 = performance.now() - startTime3;

console.log(`   Query executed in ${duration3.toFixed(2)}ms`);
console.log('   Top connectors:');
for (const binding of socialQuery.slice(0, 5)) {
  const name = binding.get('name')?.value;
  const connections = binding.get('connections')?.value || '0';
  console.log(`     ${name}: ${connections} connections`);
}

// Display 80/20 summary
console.log('\n📈 80/20 Performance Summary:');
console.log(`   Core query: ${duration1.toFixed(2)}ms (most frequent)`);
console.log(`   Filtered query: ${duration2.toFixed(2)}ms (common pattern)`);
console.log(`   Social analysis: ${duration3.toFixed(2)}ms (high value)`);
console.log(`   Total dataset: ${store.size} triples`);

console.log('\n✅ Dark Matter 80/20 example complete!');
console.log('💡 Focus on the 20% of queries that deliver 80% of the value');
