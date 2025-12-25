/**
 * Semantic Search Demo
 * Demonstrates AI-powered semantic search over RDF knowledge graphs
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { SemanticQueryEngine, RDFEmbedder, KnowledgeRecommender } from '../src/index.mjs';

const { namedNode, literal, triple } = dataFactory;

/**
 * Create sample knowledge graph about programming languages
 */
function createSampleKnowledgeGraph() {
  const store = createStore();

  // Namespaces
  const ex = (local) => namedNode(`http://example.org/${local}`);
  const schema = (local) => namedNode(`http://schema.org/${local}`);
  const rdf = (local) => namedNode(`http://www.w3.org/1999/02/22-rdf-syntax-ns#${local}`);

  // Programming languages
  const languages = [
    {
      uri: ex('JavaScript'),
      name: 'JavaScript',
      paradigm: 'multi-paradigm',
      typing: 'dynamic',
      description: 'High-level programming language for web development',
      usedFor: 'web applications',
      features: ['async/await', 'closures', 'prototypes'],
    },
    {
      uri: ex('Python'),
      name: 'Python',
      paradigm: 'multi-paradigm',
      typing: 'dynamic',
      description: 'High-level programming language for data science and AI',
      usedFor: 'data science',
      features: ['list comprehensions', 'decorators', 'generators'],
    },
    {
      uri: ex('Rust'),
      name: 'Rust',
      paradigm: 'multi-paradigm',
      typing: 'static',
      description: 'Systems programming language focused on safety and performance',
      usedFor: 'systems programming',
      features: ['ownership', 'borrowing', 'zero-cost abstractions'],
    },
    {
      uri: ex('Java'),
      name: 'Java',
      paradigm: 'object-oriented',
      typing: 'static',
      description: 'Object-oriented programming language for enterprise applications',
      usedFor: 'enterprise applications',
      features: ['garbage collection', 'strong typing', 'portability'],
    },
    {
      uri: ex('Haskell'),
      name: 'Haskell',
      paradigm: 'functional',
      typing: 'static',
      description: 'Pure functional programming language with strong type system',
      usedFor: 'functional programming',
      features: ['lazy evaluation', 'type classes', 'monads'],
    },
  ];

  // Add triples to store
  languages.forEach(lang => {
    store.add(triple(lang.uri, rdf('type'), ex('ProgrammingLanguage')));
    store.add(triple(lang.uri, schema('name'), literal(lang.name)));
    store.add(triple(lang.uri, ex('paradigm'), literal(lang.paradigm)));
    store.add(triple(lang.uri, ex('typingSystem'), literal(lang.typing)));
    store.add(triple(lang.uri, schema('description'), literal(lang.description)));
    store.add(triple(lang.uri, ex('usedFor'), literal(lang.usedFor)));

    lang.features.forEach(feature => {
      store.add(triple(lang.uri, ex('hasFeature'), literal(feature)));
    });
  });

  // Add relationships
  store.add(triple(ex('JavaScript'), ex('influencedBy'), ex('Java')));
  store.add(triple(ex('Python'), ex('similarTo'), ex('JavaScript')));
  store.add(triple(ex('Rust'), ex('improves'), ex('C++')));

  console.log(`Created knowledge graph with ${store.size} triples`);
  return store;
}

/**
 * Demo 1: Semantic Search
 */
async function demoSemanticSearch(engine) {
  console.log('\n=== DEMO 1: Semantic Search ===\n');

  const queries = [
    'language for building websites',
    'safe systems programming',
    'data science and machine learning',
    'functional programming with types',
    'enterprise backend development',
  ];

  for (const query of queries) {
    console.log(`Query: "${query}"`);
    const results = await engine.search(query, { limit: 3, threshold: 0.3 });

    results.forEach((result, i) => {
      console.log(`  ${i + 1}. ${result.text}`);
      console.log(`     Score: ${result.score.toFixed(3)}`);
    });
    console.log('');
  }
}

/**
 * Demo 2: Hybrid Search (Semantic + SPARQL)
 */
async function demoHybridSearch(engine) {
  console.log('\n=== DEMO 2: Hybrid Search (Semantic + SPARQL) ===\n');

  const nlQuery = 'language for web development';
  const sparqlPattern = '?s <http://example.org/typingSystem> "dynamic"';

  console.log(`Natural Language: "${nlQuery}"`);
  console.log(`SPARQL Filter: ${sparqlPattern}\n`);

  const results = await engine.hybridSearch(nlQuery, sparqlPattern, {
    limit: 5,
    threshold: 0.2,
  });

  results.forEach((result, i) => {
    console.log(`  ${i + 1}. ${result.text}`);
    console.log(`     Semantic Score: ${result.score.toFixed(3)}`);
    console.log(`     Hybrid Score: ${result.hybridScore.toFixed(3)}`);
    console.log(`     Matches SPARQL: ${result.matchesSparql}`);
  });
}

/**
 * Demo 3: Knowledge Discovery
 */
async function demoKnowledgeDiscovery(recommender) {
  console.log('\n\n=== DEMO 3: Knowledge Discovery ===\n');

  const entity = 'http://example.org/JavaScript';

  console.log(`Finding entities similar to: JavaScript\n`);

  const similar = await recommender.findSimilarEntities(entity, {
    limit: 3,
    threshold: 0.5,
  });

  similar.forEach((result, i) => {
    console.log(`  ${i + 1}. ${result.label}`);
    console.log(`     Similarity: ${result.score.toFixed(3)}`);
    console.log(`     URI: ${result.entity}`);
  });
}

/**
 * Demo 4: Concept Recommendations
 */
async function demoConcepRecommendations(recommender) {
  console.log('\n\n=== DEMO 4: Concept Recommendations ===\n');

  const queries = [
    'memory safety programming',
    'dynamic scripting language',
  ];

  for (const query of queries) {
    console.log(`Query: "${query}"`);
    const recommendations = await recommender.recommendConcepts(query, {
      limit: 3,
      threshold: 0.4,
    });

    recommendations.forEach((rec, i) => {
      console.log(`  ${i + 1}. ${rec.label}`);
      console.log(`     Score: ${rec.score.toFixed(3)}`);
      console.log(`     Triples: ${rec.tripleCount}`);
    });
    console.log('');
  }
}

/**
 * Demo 5: Autocomplete
 */
async function demoAutocomplete(engine) {
  console.log('\n=== DEMO 5: Autocomplete Suggestions ===\n');

  const partialQueries = [
    'program',
    'safe',
    'web',
  ];

  for (const partial of partialQueries) {
    console.log(`Partial: "${partial}"`);
    const suggestions = await engine.autocomplete(partial, 3);

    suggestions.forEach((suggestion, i) => {
      console.log(`  ${i + 1}. ${suggestion}`);
    });
    console.log('');
  }
}

/**
 * Main demo execution
 */
async function main() {
  console.log('🚀 Semantic Search Demo - UNRDF\n');
  console.log('Creating sample knowledge graph...');

  const store = createSampleKnowledgeGraph();

  console.log('\nInitializing semantic search engine...');
  const engine = new SemanticQueryEngine(store);
  await engine.initialize();

  console.log('Building vector index...');
  const indexed = await engine.indexStore();
  console.log(`Indexed ${indexed} triples`);

  console.log('\nInitializing knowledge recommender...');
  const recommender = new KnowledgeRecommender(store);
  await recommender.initialize();

  // Run demos
  await demoSemanticSearch(engine);
  await demoHybridSearch(engine);
  await demoKnowledgeDiscovery(recommender);
  await demoConcepRecommendations(recommender);
  await demoAutocomplete(engine);

  // Show statistics
  console.log('\n=== Statistics ===\n');
  console.log('Engine Stats:', engine.getStats());
  console.log('Recommender Stats:', recommender.getStats());

  console.log('\n✅ Demo completed successfully!');
}

// Run demo
main().catch(console.error);
