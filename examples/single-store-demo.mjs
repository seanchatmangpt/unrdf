/**
 * @fileoverview Example demonstrating the new single-store RdfEngine implementation
 *
 * This example shows how the RdfEngine now works with a single global store
 * using proper async context support with AsyncLocalStorage.
 *
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { RdfEngine } from '../src/engines/rdf-engine.mjs';
import { initStore } from '../src/context/index.mjs';

// Example Turtle data
const turtleData = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice a foaf:Person ;
    foaf:name "Alice" ;
    foaf:knows ex:bob .

ex:bob a foaf:Person ;
    foaf:name "Bob" ;
    foaf:knows ex:alice .
`;

// Example JSON-LD data
const jsonldData = {
  '@context': {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/',
  },
  '@id': 'ex:charlie',
  '@type': 'foaf:Person',
  'foaf:name': 'Charlie',
  'foaf:knows': {
    '@id': 'ex:alice',
  },
};

async function demonstrateSingleStoreRdfEngine() {
  console.log('=== Single Store RdfEngine Demo ===\n');

  // Initialize the store context with async support
  const runApp = initStore([], {
    baseIRI: 'http://example.org/',
    deterministic: true,
  });

  await runApp(async () => {
    const engine = new RdfEngine();

    console.log('1. Parsing Turtle into global store:');
    const store = engine.parseTurtle(turtleData);
    console.log(`   Global store now has ${store.size} quads`);

    console.log('\n2. Parsing JSON-LD into global store:');
    await engine.fromJSONLD(jsonldData);
    console.log(`   Global store now has ${store.size} quads after JSON-LD`);

    console.log('\n3. Querying the global store:');
    const queryResult = await engine.query(`
      PREFIX ex: <http://example.org/>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT ?name WHERE {
        ?person foaf:name ?name .
      }
    `);

    console.log(`   Query found ${queryResult.results.length} people:`);
    queryResult.results.forEach(row => {
      console.log(`     - ${row.name.value}`);
    });

    console.log('\n4. Serializing the global store:');
    const turtleOutput = await engine.serializeTurtle();
    console.log(`   Serialized store (first 200 chars):`);
    console.log(`   ${turtleOutput.substring(0, 200)}...`);

    console.log('\n5. Getting store statistics:');
    const stats = engine.getStats();
    console.log(`   Quads: ${stats.quads}`);
    console.log(`   Subjects: ${stats.subjects}`);
    console.log(`   Predicates: ${stats.predicates}`);
    console.log(`   Objects: ${stats.objects}`);
    console.log(`   Graphs: ${stats.graphs}`);

    console.log('\n6. Canonicalizing the global store:');
    const canonical = await engine.canonicalize();
    console.log(`   Canonical form (first 200 chars):`);
    console.log(`   ${canonical.substring(0, 200)}...`);

    console.log('\n=== Demo Complete ===');
  });
}

// Run the demonstration
demonstrateSingleStoreRdfEngine().catch(console.error);
