/**
 * Example 02: SPARQL Queries
 *
 * This example demonstrates:
 * - SELECT queries (retrieve data)
 * - ASK queries (boolean checks)
 * - CONSTRUCT queries (create new graphs)
 * - SPARQL patterns and filters
 *
 * Run: node examples/02-sparql-queries.mjs
 */

import {
  createStore,
  namedNode,
  literal,
  executeSelectSync,
  executeAskSync,
  executeConstructSync,
  FOAF,
  RDF,
  RDFS
} from '@unrdf/core';

console.log('=== Example 02: SPARQL Queries ===\n');

// Create store and add sample data
const store = createStore();
const ex = (name) => namedNode(`http://example.org/${name}`);

// Add team data
const teamData = [
  { id: 'alice', name: 'Alice Smith', role: 'Backend Developer', experience: 5 },
  { id: 'bob', name: 'Bob Johnson', role: 'Frontend Developer', experience: 3 },
  { id: 'carol', name: 'Carol White', role: 'DevOps Engineer', experience: 7 },
  { id: 'dave', name: 'Dave Brown', role: 'Backend Developer', experience: 2 }
];

for (const person of teamData) {
  store.addQuad(ex(person.id), RDF.type, FOAF.Person);
  store.addQuad(ex(person.id), FOAF.name, literal(person.name));
  store.addQuad(ex(person.id), ex('role'), literal(person.role));
  store.addQuad(ex(person.id), ex('experience'), literal(String(person.experience)));
}

// Add relationships
store.addQuad(ex('alice'), FOAF.knows, ex('bob'));
store.addQuad(ex('alice'), FOAF.knows, ex('carol'));
store.addQuad(ex('bob'), FOAF.knows, ex('dave'));
store.addQuad(ex('carol'), FOAF.knows, ex('alice'));

console.log(`✅ Loaded ${store.size} triples\n`);

// ============================================================================
// 1. Basic SELECT Query
// ============================================================================
console.log('--- 1. All Team Members (SELECT) ---');
const allMembers = executeSelectSync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  SELECT ?name ?role WHERE {
    ?person a foaf:Person ;
            foaf:name ?name ;
            ex:role ?role .
  }
  ORDER BY ?name
`);

allMembers.forEach(row => {
  console.log(`  ${row.get('name').value} - ${row.get('role').value}`);
});

// ============================================================================
// 2. SELECT with FILTER
// ============================================================================
console.log('\n--- 2. Senior Team Members (Experience >= 5) ---');
const seniorMembers = executeSelectSync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  SELECT ?name ?experience WHERE {
    ?person foaf:name ?name ;
            ex:experience ?experience .
    FILTER(?experience >= 5)
  }
  ORDER BY DESC(?experience)
`);

seniorMembers.forEach(row => {
  console.log(`  ${row.get('name').value} - ${row.get('experience').value} years`);
});

// ============================================================================
// 3. SELECT with Pattern Matching
// ============================================================================
console.log('\n--- 3. Backend Developers ---');
const backendDevs = executeSelectSync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  SELECT ?name WHERE {
    ?person foaf:name ?name ;
            ex:role ?role .
    FILTER(REGEX(?role, "Backend", "i"))
  }
`);

backendDevs.forEach(row => {
  console.log(`  ${row.get('name').value}`);
});

// ============================================================================
// 4. ASK Query (Boolean Check)
// ============================================================================
console.log('\n--- 4. ASK Queries (Boolean Checks) ---');

const aliceKnowsBob = executeAskSync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  ASK {
    ex:alice foaf:knows ex:bob .
  }
`);
console.log(`  Does Alice know Bob? ${aliceKnowsBob}`);

const hasDevOps = executeAskSync(store, `
  PREFIX ex: <http://example.org/>

  ASK {
    ?person ex:role "DevOps Engineer" .
  }
`);
console.log(`  Do we have a DevOps Engineer? ${hasDevOps}`);

// ============================================================================
// 5. Friends of Friends (FOAF)
// ============================================================================
console.log('\n--- 5. Friends of Friends ---');
const foafResults = executeSelectSync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT DISTINCT ?person ?friendOfFriend WHERE {
    ?person foaf:knows ?friend .
    ?friend foaf:knows ?friendOfFriend .
    FILTER(?person != ?friendOfFriend)
  }
`);

for (const row of foafResults) {
  const personName = store.getQuads(row.get('person'), FOAF.name, null)[0].object.value;
  const foafName = store.getQuads(row.get('friendOfFriend'), FOAF.name, null)[0].object.value;
  console.log(`  ${personName} might know ${foafName}`);
}

// ============================================================================
// 6. OPTIONAL Pattern
// ============================================================================
console.log('\n--- 6. Team Members with Optional Experience ---');
const optionalResults = executeSelectSync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  SELECT ?name ?experience WHERE {
    ?person foaf:name ?name .
    OPTIONAL { ?person ex:experience ?experience }
  }
  ORDER BY ?name
`);

optionalResults.forEach(row => {
  const name = row.get('name').value;
  const exp = row.get('experience')?.value || 'N/A';
  console.log(`  ${name}: ${exp} years`);
});

// ============================================================================
// 7. CONSTRUCT Query (Create New Graph)
// ============================================================================
console.log('\n--- 7. CONSTRUCT New Graph ---');
const colleaguesGraph = executeConstructSync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  CONSTRUCT {
    ?person ex:colleague ?colleague .
  }
  WHERE {
    ?person foaf:knows ?colleague .
  }
`);

console.log(`  Created new graph with ${colleaguesGraph.size} triples`);
console.log('  Sample triples:');

let count = 0;
for (const quad of colleaguesGraph) {
  if (count++ >= 3) break;
  const personName = store.getQuads(quad.subject, FOAF.name, null)[0]?.object.value || quad.subject.value;
  const colleagueName = store.getQuads(quad.object, FOAF.name, null)[0]?.object.value || quad.object.value;
  console.log(`    ${personName} is colleague of ${colleagueName}`);
}

// ============================================================================
// 8. Aggregation (COUNT)
// ============================================================================
console.log('\n--- 8. Count Connections ---');
const connectionCounts = executeSelectSync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person (COUNT(?friend) as ?connections) WHERE {
    ?person foaf:knows ?friend .
  }
  GROUP BY ?person
  ORDER BY DESC(?connections)
`);

console.log('  Social network ranking:');
connectionCounts.forEach(row => {
  const personName = store.getQuads(row.get('person'), FOAF.name, null)[0]?.object.value;
  console.log(`    ${personName}: ${row.get('connections').value} connections`);
});

console.log('\n--- Summary ---');
console.log(`  SELECT queries: Retrieve specific data`);
console.log(`  ASK queries: Check if pattern exists`);
console.log(`  CONSTRUCT queries: Create new graphs`);
console.log(`  FILTER: Add conditions to queries`);
console.log(`  OPTIONAL: Include optional patterns`);
console.log(`  Aggregation: COUNT, SUM, AVG, etc.`);

console.log('\n✅ Example complete!');
console.log('\nNext: Try examples/03-knowledge-hooks.mjs for reactive behaviors');
