/**
 * README Example: SPARQL Queries (lines 182-208)
 * Tests SELECT, ASK, and CONSTRUCT queries
 */

import { createDarkMatterCore, DataFactory } from '../../src/knowledge-engine/index.mjs';
const { namedNode, quad, literal } = DataFactory;

/**
 *
 */
async function testSparqlQueries() {
  console.log('🧪 Testing SPARQL Queries Example...');

  const system = await createDarkMatterCore();

  try {
    // Add test data
    await system.executeTransaction({
      additions: [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/knows'),
          namedNode('http://example.org/bob')
        ),
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice')
        )
      ],
      removals: [],
      actor: 'system'
    });
    console.log('✅ Added test data');

    // SELECT query
    const results = await system.query({
      query: `
        SELECT ?person ?friend
        WHERE {
          ?person <http://xmlns.com/foaf/0.1/knows> ?friend .
        }
      `,
      type: 'sparql-select'
    });
    console.log('✅ SELECT query executed:', results.length, 'results');

    if (!results || results.length === 0) {
      throw new Error('SELECT query returned no results');
    }

    // ASK query
    const exists = await system.query({
      query: 'ASK { ?s ?p ?o }',
      type: 'sparql-ask'
    });
    console.log('✅ ASK query executed:', exists);

    if (typeof exists !== 'boolean') {
      throw new Error('ASK query should return boolean');
    }

    // CONSTRUCT query
    const graph = await system.query({
      query: `
        CONSTRUCT { ?s ?p ?o }
        WHERE { ?s ?p ?o }
      `,
      type: 'sparql-construct'
    });
    console.log('✅ CONSTRUCT query executed:', graph?.size || 0, 'quads');

    if (!graph || graph.size === 0) {
      throw new Error('CONSTRUCT query returned no quads');
    }

    await system.cleanup();
    console.log('\n✅ SPARQL Queries Example: PASSED\n');
    return true;
  } catch (error) {
    await system.cleanup();
    console.error('❌ SPARQL Queries Example FAILED:', error.message);
    console.error(error.stack);
    return false;
  }
}

// Run test if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const success = await testSparqlQueries();
  process.exit(success ? 0 : 1);
}

export { testSparqlQueries };
