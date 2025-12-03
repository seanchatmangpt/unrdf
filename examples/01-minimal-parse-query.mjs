/**
 * 01-minimal-parse-query.mjs
 *
 * The recommended UNRDF entry point: createKnowledgeSubstrateCore()
 * One function gives you everything.
 *
 * Run: node examples/01-minimal-parse-query.mjs
 */

import { createKnowledgeSubstrateCore } from 'unrdf';

// One function gives you: transactions, hooks, sandboxing, audit trails, etc.
const core = await createKnowledgeSubstrateCore();

// Parse RDF Turtle using the core's parseTurtle method
const ttl = `
  @prefix ex: <http://example.org/> .
  ex:Alice ex:knows ex:Bob .
  ex:Bob ex:knows ex:Charlie .
  ex:Charlie ex:knows ex:Diana .
`;

const store = core.parseTurtle(ttl);

// Query the store using SPARQL
const results = core.query(
  store,
  `
  SELECT ?person ?knows WHERE {
    ?person <http://example.org/knows> ?knows .
  }
`
);

console.log('Query results:');
for (const binding of results) {
  console.log(`  ${binding.get('person').value} knows ${binding.get('knows').value}`);
}

// Access components for advanced operations
const txManager = core.getComponent('TransactionManager');
console.log('\nTransaction manager ready:', txManager !== null);

// Get core status
console.log('Core status:', core.getStatus());

// Cleanup when done
await core.cleanup();
