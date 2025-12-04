// @ts-check
const { namedNode, literal  } = dataFactory;

/**
 * Mock QueryAnalyzer for demonstration
 * (In real usage, import from @unrdf/dark-matter)
 */
class QueryAnalyzer {
  constructor(store) {
    this.store = store;
  }

  analyzePattern(pattern) {
    const results = this.store.getQuads(
      pattern.subject,
      pattern.predicate,
      pattern.object
    );

    const cardinality = results.length;
    const selectivity = this.store.size > 0 ? cardinality / this.store.size : 0;

    return { cardinality, selectivity };
  }
}

/**
 * Query Optimization Example
 *
 * Demonstrates how to use @unrdf/dark-matter to:
 * - Analyze SPARQL query patterns
 * - Estimate cardinality and selectivity
 * - Optimize query execution order
 * - Measure performance improvements
 */

/**
 * Create sample RDF dataset
 * @returns {Store} Populated N3 Store
 */
function createSampleDataset() {
  const store = createStore();

  // Add 1000 person triples
  for (let i = 1; i <= 1000; i++) {
    const person = namedNode(`http://example.org/person/${i}`);
    store.addQuad(person, namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), namedNode('http://example.org/Person'));
    store.addQuad(person, namedNode('http://example.org/name'), literal(`Person ${i}`));
    store.addQuad(person, namedNode('http://example.org/age'), literal(20 + (i % 60)));

    // 10% have email
    if (i % 10 === 0) {
      store.addQuad(person, namedNode('http://example.org/email'), literal(`person${i}@example.org`));
    }

    // 30% work at companies
    if (i % 3 === 0) {
      const company = namedNode(`http://example.org/company/${Math.floor(i / 10)}`);
      store.addQuad(person, namedNode('http://example.org/worksAt'), company);
      store.addQuad(company, namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), namedNode('http://example.org/Company'));
      store.addQuad(company, namedNode('http://example.org/companyName'), literal(`Company ${Math.floor(i / 10)}`));
    }
  }

  console.log(`\n📊 Dataset created: ${store.size} triples`);
  return store;
}

/**
 * Example 1: Analyze query patterns and estimate cardinality
 * @param {Store} store - RDF store
 */
function example1_analyzeQueryPatterns(store) {
  console.log('\n\n🔍 Example 1: Query Pattern Analysis\n');
  console.log('=' .repeat(60));

  const analyzer = new QueryAnalyzer(store);

  // Pattern 1: Find all people (high cardinality)
  const pattern1 = {
    subject: null,
    predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    object: namedNode('http://example.org/Person')
  };

  console.log('\n📋 Pattern 1: Find all people');
  console.log('   Pattern: ?person rdf:type ex:Person');
  const analysis1 = analyzer.analyzePattern(pattern1);
  console.log(`   Estimated cardinality: ${analysis1.cardinality}`);
  console.log(`   Selectivity: ${(analysis1.selectivity * 100).toFixed(2)}%`);

  // Pattern 2: Find people with email (low cardinality)
  const pattern2 = {
    subject: null,
    predicate: namedNode('http://example.org/email'),
    object: null
  };

  console.log('\n📋 Pattern 2: Find people with email');
  console.log('   Pattern: ?person ex:email ?email');
  const analysis2 = analyzer.analyzePattern(pattern2);
  console.log(`   Estimated cardinality: ${analysis2.cardinality}`);
  console.log(`   Selectivity: ${(analysis2.selectivity * 100).toFixed(2)}%`);

  // Pattern 3: Find people working at companies (medium cardinality)
  const pattern3 = {
    subject: null,
    predicate: namedNode('http://example.org/worksAt'),
    object: null
  };

  console.log('\n📋 Pattern 3: Find people working at companies');
  console.log('   Pattern: ?person ex:worksAt ?company');
  const analysis3 = analyzer.analyzePattern(pattern3);
  console.log(`   Estimated cardinality: ${analysis3.cardinality}`);
  console.log(`   Selectivity: ${(analysis3.selectivity * 100).toFixed(2)}%`);

  return { pattern1: analysis1, pattern2: analysis2, pattern3: analysis3 };
}

/**
 * Example 2: Optimize join order based on cardinality
 * @param {Store} store - RDF store
 */
function example2_optimizeJoinOrder(store) {
  console.log('\n\n⚡ Example 2: Join Order Optimization\n');
  console.log('=' .repeat(60));

  const analyzer = new QueryAnalyzer(store);

  // Original query (bad order): Start with high cardinality pattern
  const badOrderPatterns = [
    {
      subject: null,
      predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      object: namedNode('http://example.org/Person')
    },
    {
      subject: null,
      predicate: namedNode('http://example.org/email'),
      object: null
    }
  ];

  console.log('\n❌ Original Query (Bad Order):');
  console.log('   1. ?person rdf:type ex:Person  (1000 results)');
  console.log('   2. ?person ex:email ?email     (100 results)');
  console.log('   Expected intermediate results: ~1000');

  // Optimized query: Start with low cardinality pattern
  const goodOrderPatterns = [
    {
      subject: null,
      predicate: namedNode('http://example.org/email'),
      object: null
    },
    {
      subject: null,
      predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      object: namedNode('http://example.org/Person')
    }
  ];

  console.log('\n✅ Optimized Query (Good Order):');
  console.log('   1. ?person ex:email ?email     (100 results)');
  console.log('   2. ?person rdf:type ex:Person  (already filtered)');
  console.log('   Expected intermediate results: ~100');

  // Calculate cost estimates
  const badOrderCost = badOrderPatterns.reduce((cost, pattern) => {
    const analysis = analyzer.analyzePattern(pattern);
    return cost + analysis.cardinality;
  }, 0);

  const goodOrderCost = goodOrderPatterns.reduce((cost, pattern) => {
    const analysis = analyzer.analyzePattern(pattern);
    return cost + analysis.cardinality;
  }, 0);

  console.log(`\n📊 Cost Comparison:`);
  console.log(`   Bad order cost:  ${badOrderCost} intermediate results`);
  console.log(`   Good order cost: ${goodOrderCost} intermediate results`);
  console.log(`   Improvement:     ${((1 - goodOrderCost / badOrderCost) * 100).toFixed(1)}% reduction`);

  return { badOrderCost, goodOrderCost };
}

/**
 * Example 3: Measure actual query performance
 * @param {Store} store - RDF store
 */
function example3_measurePerformance(store) {
  console.log('\n\n⏱️  Example 3: Performance Measurement\n');
  console.log('=' .repeat(60));

  // Bad order query: Filter broad first, then narrow
  console.log('\n❌ Testing Bad Order Query...');
  const badStart = performance.now();

  let badResults = store.getQuads(
    null,
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://example.org/Person')
  );

  badResults = badResults.filter(quad => {
    const hasEmail = store.getQuads(
      quad.subject,
      namedNode('http://example.org/email'),
      null
    );
    return hasEmail.length > 0;
  });

  const badDuration = performance.now() - badStart;
  console.log(`   Results: ${badResults.length}`);
  console.log(`   Time: ${badDuration.toFixed(3)}ms`);

  // Good order query: Filter narrow first, then verify
  console.log('\n✅ Testing Optimized Query...');
  const goodStart = performance.now();

  let goodResults = store.getQuads(
    null,
    namedNode('http://example.org/email'),
    null
  );

  goodResults = goodResults.filter(quad => {
    const isPerson = store.getQuads(
      quad.subject,
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode('http://example.org/Person')
    );
    return isPerson.length > 0;
  });

  const goodDuration = performance.now() - goodStart;
  console.log(`   Results: ${goodResults.length}`);
  console.log(`   Time: ${goodDuration.toFixed(3)}ms`);

  console.log(`\n📈 Performance Improvement:`);
  console.log(`   Speedup: ${(badDuration / goodDuration).toFixed(2)}x faster`);
  console.log(`   Time saved: ${(badDuration - goodDuration).toFixed(3)}ms`);

  return { badDuration, goodDuration, speedup: badDuration / goodDuration };
}

/**
 * Example 4: Recommend query optimizations
 * @param {Store} store - RDF store
 */
function example4_recommendOptimizations(store) {
  console.log('\n\n💡 Example 4: Optimization Recommendations\n');
  console.log('=' .repeat(60));

  const analyzer = new QueryAnalyzer(store);

  const patterns = [
    {
      subject: null,
      predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      object: namedNode('http://example.org/Person')
    },
    {
      subject: null,
      predicate: namedNode('http://example.org/email'),
      object: null
    },
    {
      subject: null,
      predicate: namedNode('http://example.org/worksAt'),
      object: null
    }
  ];

  console.log('\n📋 Query Patterns to Analyze:');
  patterns.forEach((pattern, idx) => {
    const predLabel = pattern.predicate.value.split('/').pop();
    console.log(`   ${idx + 1}. ?s ${predLabel} ?o`);
  });

  // Analyze each pattern
  const analyses = patterns.map(pattern => analyzer.analyzePattern(pattern));

  // Sort by selectivity (most selective first)
  const sortedAnalyses = analyses
    .map((analysis, idx) => ({ ...analysis, originalIndex: idx }))
    .sort((a, b) => a.selectivity - b.selectivity);

  console.log('\n✅ Recommended Execution Order (by selectivity):');
  sortedAnalyses.forEach((analysis, idx) => {
    const pattern = patterns[analysis.originalIndex];
    const predLabel = pattern.predicate.value.split('/').pop();
    console.log(`   ${idx + 1}. ?s ${predLabel} ?o`);
    console.log(`      Selectivity: ${(analysis.selectivity * 100).toFixed(2)}%`);
    console.log(`      Cardinality: ${analysis.cardinality}`);
  });

  return { originalOrder: patterns, optimizedOrder: sortedAnalyses };
}

/**
 * Main execution
 */
export function run() {
  console.log('🚀 Query Optimization with @unrdf/dark-matter');
  console.log('=' .repeat(60));

  const store = createSampleDataset();

  const results = {
    patternAnalysis: example1_analyzeQueryPatterns(store),
    joinOptimization: example2_optimizeJoinOrder(store),
    performance: example3_measurePerformance(store),
    recommendations: example4_recommendOptimizations(store)
  };

  console.log('\n\n✨ Summary\n');
  console.log('=' .repeat(60));
  console.log('✅ Demonstrated query pattern analysis');
  console.log('✅ Optimized join order based on cardinality');
  console.log(`✅ Achieved ${results.performance.speedup.toFixed(2)}x performance improvement`);
  console.log('✅ Generated optimization recommendations');
  console.log('\n');

  return results;
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  run();
}
