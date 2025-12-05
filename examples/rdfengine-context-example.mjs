/**
 * @fileoverview Example demonstrating RdfEngine integration with store context
 *
 * This example shows how the RdfEngine now works seamlessly with the store context
 * system, providing both traditional methods and context-aware convenience methods.
 *
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { RdfEngine } from '../packages/knowledge-engine/src/engines/rdf-engine.mjs';
import { initStore } from '../packages/composables/src/context/index.mjs';

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

async function demonstrateRdfEngineContext() {
  console.log('=== RdfEngine Store Context Integration Demo ===\n');

  // Initialize the store context
  const runApp = initStore([], {
    baseIRI: 'http://example.org/',
    deterministic: true,
  });

  // Run everything within the context
  await runApp(async () => {
    const engine = new RdfEngine();

    console.log('1. Traditional RdfEngine usage (creates new stores):');
    const traditionalStore = engine.parseTurtle(turtleData);
    console.log(`   Created store with ${traditionalStore.size} quads`);

    // This store is separate from the context store
    console.log(`   Context store size: ${engine.getContextStore()?.size || 0} quads\n`);

    console.log('2. Context-aware RdfEngine usage:');

    // Parse Turtle directly into the context store
    const contextStore = engine.parseTurtleToContext(turtleData);
    console.log(`   Context store now has ${contextStore.size} quads`);

    // Parse JSON-LD into the context store
    await engine.fromJSONLDToContext(jsonldData);
    console.log(`   Context store now has ${contextStore.size} quads after JSON-LD`);

    // Check context availability before query
    const currentContextStore = engine.getContextStore();
    console.log(`   Current context store available: ${currentContextStore ? 'yes' : 'no'}`);

    if (currentContextStore) {
      // Query the context store
      const queryResult = await engine.queryContext(`
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
    } else {
      console.log(`   ✗ Context store not available for query`);
    }

    // Serialize the context store
    const turtleOutput = await engine.serializeContextToTurtle();
    console.log(`\n3. Serialized context store (first 200 chars):`);
    console.log(`   ${turtleOutput.substring(0, 200)}...`);

    // Get context store statistics
    const stats = engine.getContextStats();
    console.log(`\n4. Context store statistics:`);
    console.log(`   Quads: ${stats.quads}`);
    console.log(`   Subjects: ${stats.subjects}`);
    console.log(`   Predicates: ${stats.predicates}`);
    console.log(`   Objects: ${stats.objects}`);
    console.log(`   Graphs: ${stats.graphs}`);

    // Canonicalize the context store
    const canonical = await engine.canonicalizeContext();
    console.log(`\n5. Canonical form (first 200 chars):`);
    console.log(`   ${canonical.substring(0, 200)}...`);

    console.log('\n=== Demo Complete ===');
  });
}

// Run the demonstration
demonstrateRdfEngineContext().catch(console.error);
