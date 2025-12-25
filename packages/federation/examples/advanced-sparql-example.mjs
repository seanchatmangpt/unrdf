/**
 * Advanced SPARQL Federation Example
 * Demonstrates federated querying across multiple SPARQL endpoints
 */

import { createAdvancedFederationEngine, federatedQuery } from '../src/advanced-sparql-federation.mjs';

/**
 * Example 1: Simple federated query
 */
async function simpleExample() {
  console.log('\n=== Simple Federated Query ===\n');

  const results = await federatedQuery(
    [
      'https://dbpedia.org/sparql',
      'https://query.wikidata.org/sparql'
    ],
    `
      SELECT DISTINCT ?person ?name
      WHERE {
        ?person a dbo:Person ;
                rdfs:label ?name .
        FILTER (lang(?name) = 'en')
      }
      LIMIT 5
    `
  );

  console.log(`Found ${results.bindings.length} results`);
  console.log(`Execution time: ${results.metadata.executionTime}ms`);
  console.log('Results:', JSON.stringify(results.bindings, null, 2));
}

/**
 * Example 2: Advanced engine with streaming
 */
async function streamingExample() {
  console.log('\n=== Streaming Federated Query ===\n');

  const engine = await createAdvancedFederationEngine({
    sources: [
      { url: 'https://dbpedia.org/sparql', type: 'sparql' },
      { url: 'https://query.wikidata.org/sparql', type: 'sparql' }
    ],
    streaming: true,
    optimization: 'aggressive',
    timeout: 30000
  });

  let resultCount = 0;

  const results = await engine.query(
    `
      SELECT ?s ?p ?o
      WHERE {
        ?s ?p ?o .
      }
      LIMIT 20
    `,
    {
      onBinding: (binding) => {
        resultCount++;
        console.log(`[Stream ${resultCount}]`, binding);
      }
    }
  );

  console.log(`\nTotal results: ${results.bindings.length}`);
  console.log(`Execution time: ${results.metadata.executionTime}ms`);

  await engine.close();
}

/**
 * Example 3: Source metadata discovery
 */
async function metadataExample() {
  console.log('\n=== Source Metadata Discovery ===\n');

  const engine = await createAdvancedFederationEngine({
    sources: [
      { url: 'https://dbpedia.org/sparql', type: 'sparql' },
      { url: 'https://query.wikidata.org/sparql', type: 'sparql' }
    ]
  });

  const metadata = await engine.getSourcesMetadata();

  console.log('Source Metadata:');
  metadata.forEach((source) => {
    console.log(`\n  ${source.url}`);
    console.log(`    Type: ${source.type}`);
    console.log(`    Status: ${source.status}`);
    if (source.tripleCount) {
      console.log(`    Triple Count: ${source.tripleCount}`);
    }
    if (source.error) {
      console.log(`    Error: ${source.error}`);
    }
  });

  await engine.close();
}

/**
 * Run all examples
 */
async function main() {
  try {
    console.log('Advanced SPARQL Federation Examples');
    console.log('====================================');

    // Uncomment to run examples (requires network access)
    // await simpleExample();
    // await streamingExample();
    // await metadataExample();

    console.log('\n✅ All examples completed successfully');
    console.log('\nNote: Examples are commented out by default to avoid network calls');
    console.log('Uncomment the example calls in main() to run them');
  } catch (error) {
    console.error('❌ Example failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export { simpleExample, streamingExample, metadataExample };
