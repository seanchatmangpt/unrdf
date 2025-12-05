/**
 * README Example: Simple Knowledge Graph (lines 336-379)
 * Tests creating and querying a social network
 */

import { createDarkMatterCore, parseTurtle } from '../../packages/knowledge-engine/index.mjs';

/**
 *
 */
async function testSimpleGraph() {
  console.log('🧪 Testing Simple Knowledge Graph Example...');

  const system = await createDarkMatterCore();

  try {
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
    console.log('✅ Parsed Turtle');

    // Add to knowledge graph
    await system.executeTransaction({
      additions: [...store],
      removals: [],
      actor: 'importer',
    });
    console.log('✅ Added to knowledge graph');

    // Query social network (note: README example has incomplete query)
    // The query uses unbound prefixes - let's use full URIs
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
      type: 'sparql-select',
    });
    console.log('✅ Query results:', friends);

    if (!friends || friends.length === 0) {
      throw new Error('Query returned no results');
    }

    await system.cleanup();
    console.log('\n✅ Simple Knowledge Graph Example: PASSED\n');
    return true;
  } catch (error) {
    await system.cleanup();
    console.error('❌ Simple Knowledge Graph Example FAILED:', error.message);
    console.error(error.stack);
    return false;
  }
}

// Run test if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const success = await testSimpleGraph();
  process.exit(success ? 0 : 1);
}

export { testSimpleGraph };
