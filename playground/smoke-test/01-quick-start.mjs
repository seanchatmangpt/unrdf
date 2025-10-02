/**
 * README Example: Quick Start (lines 64-100)
 * Tests basic createDarkMatterCore + query functionality
 */

import { createDarkMatterCore } from 'unrdf';
import { namedNode, quad, literal } from '@rdfjs/data-model';

console.log('🧪 Testing Quick Start Example...\n');

try {
  // 1. Create the knowledge engine
  const system = await createDarkMatterCore();

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
      )
    ],
    removals: [],
    actor: 'system'
  });

  // 3. Query the data
  const results = await system.query({
    query: 'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }',
    type: 'sparql-select'
  });

  console.log('Query results:', JSON.stringify(results, null, 2));

  // Verify expected output
  const hasAlice = results.some(r => r.name && r.name.value === 'Alice');

  if (hasAlice && results.length === 1) {
    console.log('✅ Quick Start example PASSED');
    console.log('   - Query executed successfully');
    console.log('   - Returned expected result: { name: "Alice" }');
  } else {
    console.log('❌ Quick Start example FAILED');
    console.log('   - Expected: [{ name: "Alice" }]');
    console.log('   - Got:', results);
  }

  // 4. Cleanup
  await system.cleanup();

  process.exit(hasAlice && results.length === 1 ? 0 : 1);
} catch (error) {
  console.log('❌ Quick Start example FAILED with error:');
  console.error(error);
  process.exit(1);
}
