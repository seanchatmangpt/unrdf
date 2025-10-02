/**
 * UNRDF Starter Project - Main Entry Point
 */

import { initStore } from 'unrdf';
import { useStoreContext, useTurtle, useGraph } from 'unrdf/composables';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';

console.log('🚀 UNRDF Starter Project\n');

// Initialize UNRDF with configuration
const runApp = initStore([], {
  baseIRI: 'http://example.org/'
});

await runApp(async () => {
  const store = useStoreContext();
  const turtle = await useTurtle();
  const graph = useGraph();

  try {
    // Load sample data
    console.log('📁 Loading sample data...');
    const dataPath = join(process.cwd(), 'data', 'sample.ttl');
    const data = await readFile(dataPath, 'utf-8');
    const quads = await turtle.parse(data);
    store.add(...quads);
    console.log(`✅ Loaded ${quads.length} triples\n`);

    // Example SPARQL query
    console.log('🔍 Executing SPARQL query...');
    const query = `
      PREFIX ex: <http://example.org/>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>

      SELECT ?person ?name WHERE {
        ?person a foaf:Person ;
                foaf:name ?name .
      }
    `;

    const results = await graph.select(query);
    console.log('Query Results:');
    console.table(results);

    console.log('\n✅ Starter project completed successfully!');
    console.log('\n📚 Next steps:');
    console.log('  1. Add your own data in data/');
    console.log('  2. Create Knowledge Hooks in src/hooks/');
    console.log('  3. Write tests in test/');
    console.log('  4. Run npm test to validate');

  } catch (error) {
    console.error('❌ Error:', error.message);
    process.exit(1);
  }
});
