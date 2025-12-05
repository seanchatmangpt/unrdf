/**
 * README Example: Quick Start (lines 66-102)
 * Tests the 5-minute tutorial
 */

import { createDarkMatterCore, DataFactory } from '../../packages/knowledge-engine/index.mjs';
const { namedNode, quad, literal } = DataFactory;

/**
 *
 */
async function testQuickStart() {
  console.log('🧪 Testing Quick Start Example...');

  try {
    // 1. Create the knowledge engine
    const system = await createDarkMatterCore();
    console.log('✅ Created knowledge engine');

    // 2. Add some RDF data
    await system.executeTransaction({
      additions: [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice')
        ),
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/knows'),
          namedNode('http://example.org/bob')
        ),
      ],
      removals: [],
      actor: 'system',
    });
    console.log('✅ Added RDF data');

    // 3. Query the data
    const results = await system.query({
      query: 'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }',
      type: 'sparql-select',
    });

    console.log('✅ Query results:', results);

    // Validate results
    if (!results || results.length === 0) {
      throw new Error('Expected query results, got none');
    }

    const hasAlice = results.some(r => r.name?.value === 'Alice' || r.name === 'Alice');
    if (!hasAlice) {
      throw new Error(`Expected to find 'Alice', got: ${JSON.stringify(results)}`);
    }

    console.log('✅ Found Alice in results');

    // 4. Cleanup
    await system.cleanup();
    console.log('✅ Cleanup complete');

    console.log('\n✅ Quick Start Example: PASSED\n');
    return true;
  } catch (error) {
    console.error('❌ Quick Start Example FAILED:', error.message);
    console.error(error.stack);
    return false;
  }
}

// Run test if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const success = await testQuickStart();
  process.exit(success ? 0 : 1);
}

export { testQuickStart };
