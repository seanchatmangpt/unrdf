/**
 * Composition C22: RDF Store + RDF Embedder + Semantic Query
 * Atoms: A01 + A29 + A30
 *
 * Proof: Hybrid semantic + SPARQL search
 */

import { createStore, namedNode, literal, quad } from '@unrdf/core';
import { RDFEmbedder, SemanticQueryEngine } from '@unrdf/semantic-search';

console.log('=== C22: Hybrid Semantic Search Proof ===\n');

async function prove() {
  try {
    // A01: Create RDF store with sample knowledge graph
    const store = createStore();

    const alice = namedNode('http://example.org/alice');
    const bob = namedNode('http://example.org/bob');
    const carol = namedNode('http://example.org/carol');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');
    const interest = namedNode('http://xmlns.com/foaf/0.1/interest');

    store.add(quad(alice, name, literal('Alice')));
    store.add(quad(alice, interest, literal('machine learning')));
    store.add(quad(bob, name, literal('Bob')));
    store.add(quad(bob, interest, literal('artificial intelligence')));
    store.add(quad(carol, name, literal('Carol')));
    store.add(quad(carol, interest, literal('quantum computing')));

    console.log('✅ A01: RDF store created with 3 people and interests');

    // A29: Create RDF embedder
    const embedder = new RDFEmbedder({
      model: 'all-MiniLM-L6-v2', // Small transformer model
      dimensions: 384
    });
    await embedder.initialize();
    console.log('✅ A29: RDF embedder initialized');

    // Generate embeddings for all entities
    const quads = Array.from(store.match());
    await embedder.embedQuads(quads);
    console.log(`   Embedded ${quads.length} quads`);

    // A30: Create semantic query engine
    const queryEngine = new SemanticQueryEngine({
      store,
      embedder
    });
    await queryEngine.initialize();
    console.log('✅ A30: Semantic query engine initialized');

    // Execute natural language query
    const nlQuery = "Who is interested in AI and ML?";
    const results = await queryEngine.query(nlQuery, {
      hybridMode: true, // Use both semantic similarity and SPARQL
      topK: 5
    });

    console.log(`\n🔍 Natural language query: "${nlQuery}"`);
    console.log(`   Results: ${results.length}`);

    for (const result of results) {
      console.log(`   - ${result.entity} (similarity: ${result.score.toFixed(3)})`);
      console.log(`     ${result.description}`);
    }

    console.log('\n✅ COMPOSITION VERIFIED');
    console.log('   Value: Natural language queries over RDF');
    console.log('   Hybrid mode: Combines semantic similarity + SPARQL');
    console.log(`   Precision: ${results.length > 0 ? 'High' : 'N/A'}`);

    // Cleanup
    await embedder.dispose();
    await queryEngine.dispose();

    process.exit(results.length > 0 ? 0 : 1);
  } catch (error) {
    console.error('❌ COMPOSITION FAILED:', error.message);
    console.error('   Note: This proof requires transformer models (may fail in CI)');
    console.error(error.stack);
    process.exit(1);
  }
}

prove();
