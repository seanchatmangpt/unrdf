/**
 * README Example 1: Simple Knowledge Graph (lines 332-377)
 * Tests parseTurtle + SPARQL query with prefixes
 */

import { createDarkMatterCore, parseTurtle } from 'unrdf';

console.log('🧪 Testing Simple Knowledge Graph Example...\n');

try {
  const system = await createDarkMatterCore();

  // Parse and load data
  const ttl = `
    @prefix ex: <http://example.org/> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .

    ex:alice a foaf:Person ;
             foaf:name "Alice" ;
             foaf:knows ex:bob .

    ex:bob a foaf:Person ;
           foaf:name "Bob" .
  `;

  const store = await parseTurtle(ttl);

  // Add to knowledge graph
  await system.executeTransaction({
    additions: [...store],
    removals: [],
    actor: 'importer'
  });

  // Query social network
  const friends = await system.query({
    query: `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT ?person ?name ?friend ?friendName
      WHERE {
        ?person foaf:knows ?friend .
        ?person foaf:name ?name .
        ?friend foaf:name ?friendName .
      }
    `,
    type: 'sparql-select'
  });

  console.log('Query results:', JSON.stringify(friends, null, 2));

  // Verify expected output
  const hasAliceKnowsBob = friends.some(r =>
    r.name?.value === 'Alice' && r.friendName?.value === 'Bob'
  );

  if (hasAliceKnowsBob && friends.length === 1) {
    console.log('✅ Simple Knowledge Graph example PASSED');
    console.log('   - Turtle parsing successful');
    console.log('   - SPARQL query with prefixes executed');
    console.log('   - Found Alice knows Bob relationship');
  } else {
    console.log('❌ Simple Knowledge Graph example FAILED');
    console.log('   - Expected: Alice knows Bob');
    console.log('   - Got:', friends);
  }

  await system.cleanup();

  process.exit(hasAliceKnowsBob && friends.length === 1 ? 0 : 1);
} catch (error) {
  console.log('❌ Simple Knowledge Graph example FAILED with error:');
  console.error(error);
  process.exit(1);
}
