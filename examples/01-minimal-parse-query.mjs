/**
 * 01-minimal-parse-query.mjs
 *
 * The simplest UNRDF example: parse RDF and query it.
 * This is all most users need.
 *
 * Run: node examples/01-minimal-parse-query.mjs
 */

import { initStore, useGraph, useTurtle } from 'unrdf';

// Initialize context (call once)
await initStore();

// Get composables
const turtle = useTurtle();
const graph = useGraph();

// Parse RDF Turtle
const store = turtle.parse(`
  @prefix ex: <http://example.org/> .
  ex:Alice ex:knows ex:Bob .
  ex:Bob ex:knows ex:Charlie .
  ex:Charlie ex:knows ex:Diana .
`);

// Query with SPARQL
const results = graph.select(`
  PREFIX ex: <http://example.org/>
  SELECT ?person ?friend WHERE {
    ?person ex:knows ?friend
  }
`);

console.log('Query results:');
console.log(results);

// Serialize back to Turtle
const output = turtle.serialize(store);
console.log('\nSerialized RDF:');
console.log(output);
