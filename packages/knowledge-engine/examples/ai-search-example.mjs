/**
 * AI-Enhanced Knowledge Graph Search Example
 * Demonstrates semantic search using transformer models
 */

import { createAISearchEngine, semanticSearch, batchEmbed } from '../src/ai-enhanced-search.mjs';
import { createKnowledgeSubstrateCore } from '../src/knowledge-substrate-core.mjs';

/**
 * Example 1: Simple semantic search
 */
async function simpleSearchExample() {
  console.log('\n=== Simple Semantic Search ===\n');

  // Create knowledge substrate with sample data
  const core = await createKnowledgeSubstrateCore();

  // Add sample triples
  const sampleData = `
    @prefix ex: <http://example.org/> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

    ex:ml rdfs:label "Machine Learning" ;
           rdfs:comment "A branch of artificial intelligence" .

    ex:ai rdfs:label "Artificial Intelligence" ;
          rdfs:comment "Intelligence demonstrated by machines" .

    ex:nn rdfs:label "Neural Networks" ;
          rdfs:comment "Computing systems inspired by biological neural networks" .

    ex:dl rdfs:label "Deep Learning" ;
          rdfs:comment "Part of machine learning based on artificial neural networks" .
  `;

  await core.importTurtle(sampleData);

  // Perform semantic search
  const results = await semanticSearch(
    core.store,
    'artificial intelligence algorithms',
    {
      topK: 3,
      threshold: 0.5
    }
  );

  console.log(`Found ${results.length} results:\n`);
  results.forEach((result, idx) => {
    console.log(`${idx + 1}. Score: ${result.score.toFixed(4)}`);
    console.log(`   Subject: ${result.triple.subject}`);
    console.log(`   Object: ${result.triple.object}\n`);
  });
}

/**
 * Example 2: Advanced search with custom engine
 */
async function advancedSearchExample() {
  console.log('\n=== Advanced Search Engine ===\n');

  const core = await createKnowledgeSubstrateCore();

  // Add sample data
  const sampleData = `
    @prefix ex: <http://example.org/> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

    ex:python rdfs:label "Python Programming" ;
              rdfs:comment "High-level programming language for general-purpose programming" .

    ex:javascript rdfs:label "JavaScript" ;
                  rdfs:comment "Programming language for web development" .

    ex:rust rdfs:label "Rust" ;
            rdfs:comment "Systems programming language focused on safety and performance" .
  `;

  await core.importTurtle(sampleData);

  // Create search engine
  const engine = await createAISearchEngine(core.store, {
    model: 'Xenova/all-MiniLM-L6-v2',
    topK: 5,
    threshold: 0.6,
    cache: true
  });

  // Search
  const results = await engine.search('web development languages');

  console.log(`Found ${results.length} results:\n`);
  results.forEach((result, idx) => {
    console.log(`${idx + 1}. Score: ${result.score.toFixed(4)}`);
    console.log(`   ${result.triple.subject} -> ${result.triple.object}\n`);
  });

  // Show stats
  const stats = engine.getStats();
  console.log('Engine Statistics:');
  console.log(`  Cache size: ${stats.cacheSize}`);
  console.log(`  Model loaded: ${stats.modelLoaded}`);
  console.log(`  Config:`, stats.config);

  engine.clearCache();
}

/**
 * Example 3: Find similar triples
 */
async function findSimilarExample() {
  console.log('\n=== Find Similar Triples ===\n');

  const core = await createKnowledgeSubstrateCore();

  const sampleData = `
    @prefix ex: <http://example.org/> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

    ex:cat rdfs:label "Cat" ;
           rdfs:comment "Small carnivorous mammal" .

    ex:dog rdfs:label "Dog" ;
           rdfs:comment "Domesticated carnivorous mammal" .

    ex:car rdfs:label "Car" ;
           rdfs:comment "Wheeled motor vehicle" .
  `;

  await core.importTurtle(sampleData);

  const engine = await createAISearchEngine(core.store, {
    topK: 2,
    threshold: 0.5
  });

  // Find triples similar to "dog"
  const similar = await engine.findSimilar(
    {
      subject: 'http://example.org/dog',
      predicate: 'http://www.w3.org/2000/01/rdf-schema#comment',
      object: 'Domesticated carnivorous mammal'
    },
    { topK: 2 }
  );

  console.log('Triples similar to "dog":\n');
  similar.forEach((result, idx) => {
    console.log(`${idx + 1}. Score: ${result.score.toFixed(4)}`);
    console.log(`   ${result.triple.subject}`);
    console.log(`   ${result.triple.object}\n`);
  });

  engine.clearCache();
}

/**
 * Example 4: Batch embedding generation
 */
async function batchEmbedExample() {
  console.log('\n=== Batch Embedding Generation ===\n');

  const texts = [
    'machine learning',
    'artificial intelligence',
    'neural networks',
    'deep learning',
    'natural language processing'
  ];

  console.log('Generating embeddings for:');
  texts.forEach((text, idx) => console.log(`  ${idx + 1}. ${text}`));

  const embeddings = await batchEmbed(texts, {
    batchSize: 2
  });

  console.log(`\nGenerated ${embeddings.length} embeddings`);
  console.log(`Embedding dimension: ${embeddings[0].length}`);
  console.log(`First embedding (first 5 dims):`, embeddings[0].slice(0, 5));
}

/**
 * Example 5: Clustering triples
 */
async function clusteringExample() {
  console.log('\n=== Clustering Triples ===\n');

  const core = await createKnowledgeSubstrateCore();

  const sampleData = `
    @prefix ex: <http://example.org/> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

    ex:apple rdfs:label "Apple" .
    ex:banana rdfs:label "Banana" .
    ex:car rdfs:label "Car" .
    ex:bike rdfs:label "Bicycle" .
    ex:orange rdfs:label "Orange" .
  `;

  await core.importTurtle(sampleData);

  const engine = await createAISearchEngine(core.store);

  const clusters = await engine.cluster(2);

  console.log(`Created ${clusters.length} clusters:\n`);
  clusters.forEach((cluster, idx) => {
    console.log(`Cluster ${idx + 1}: ${cluster.length} triples`);
    cluster.forEach((item) => {
      console.log(`  - ${item.triple.subject}`);
    });
    console.log('');
  });

  engine.clearCache();
}

/**
 * Run all examples
 */
async function main() {
  try {
    console.log('AI-Enhanced Knowledge Graph Search Examples');
    console.log('============================================');

    // Note: These examples require the transformer model to be downloaded
    // Uncomment to run (requires ~50MB model download on first run)
    // await simpleSearchExample();
    // await advancedSearchExample();
    // await findSimilarExample();
    // await batchEmbedExample();
    // await clusteringExample();

    console.log('\n✅ All examples completed successfully');
    console.log('\nNote: Examples are commented out by default to avoid model downloads');
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

export {
  simpleSearchExample,
  advancedSearchExample,
  findSimilarExample,
  batchEmbedExample,
  clusteringExample
};
