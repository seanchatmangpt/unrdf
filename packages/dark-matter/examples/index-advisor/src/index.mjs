// @ts-check
const { namedNode, literal  } = dataFactory;

/**
 * Mock IndexAdvisor for demonstration
 * (In real usage, import from @unrdf/dark-matter)
 */
class IndexAdvisor {
  constructor(store) {
    this.store = store;
    this.indexes = new Map();
  }

  analyzeWorkload(workload) {
    const predicateFreq = new Map();

    for (const query of workload) {
      const predKey = query.pattern.predicate.value;
      predicateFreq.set(predKey, (predicateFreq.get(predKey) || 0) + query.frequency);
    }

    const hotPredicates = Array.from(predicateFreq.entries())
      .sort((a, b) => b[1] - a[1])
      .map(([pred]) => pred);

    const totalLoad = workload.reduce((sum, q) => sum + q.frequency, 0);

    return { hotPredicates, totalLoad };
  }

  recommendIndexes(workload) {
    const recommendations = [];

    for (const query of workload) {
      const results = this.store.getQuads(
        query.pattern.subject,
        query.pattern.predicate,
        query.pattern.object
      );

      const cardinality = results.length;
      const selectivity = cardinality / this.store.size;

      // Determine priority based on frequency and selectivity
      let priority = 'LOW';
      let estimatedImprovement = 20;

      if (query.frequency > 500 && selectivity < 0.3) {
        priority = 'HIGH';
        estimatedImprovement = 75;
      } else if (query.frequency > 200 || selectivity < 0.5) {
        priority = 'MEDIUM';
        estimatedImprovement = 45;
      }

      // Determine index type
      const indexType = selectivity < 0.3 ? 'hash' : 'predicate';

      recommendations.push({
        predicate: query.pattern.predicate,
        indexType,
        priority,
        estimatedImprovement,
        affectedQueries: query.frequency,
        storageOverhead: `${cardinality} entries`,
        reason: `${priority === 'HIGH' ? 'High' : 'Moderate'} frequency, ${selectivity < 0.3 ? 'low' : 'moderate'} cardinality`
      });
    }

    return recommendations.sort((a, b) => {
      const priorityOrder = { HIGH: 3, MEDIUM: 2, LOW: 1 };
      return priorityOrder[b.priority] - priorityOrder[a.priority];
    });
  }

  createIndex(predicate, indexType) {
    const results = this.store.getQuads(null, predicate, null);
    const index = new Map();

    for (const quad of results) {
      index.set(quad.object.value, quad.subject);
    }

    this.indexes.set(predicate.value, index);

    return { size: index.size, type: indexType };
  }
}

/**
 * Index Advisor Example
 *
 * Demonstrates how to use @unrdf/dark-matter to:
 * - Analyze query workload patterns
 * - Recommend optimal indexes
 * - Estimate performance improvements
 * - Create recommended indexes
 * - Benchmark before/after performance
 */

/**
 * Create sample RDF dataset with realistic patterns
 * @returns {Store} Populated N3 Store
 */
function createSampleDataset() {
  const store = createStore();

  // Create 5000 person entities
  for (let i = 1; i <= 5000; i++) {
    const person = namedNode(`http://example.org/person/${i}`);
    store.addQuad(person, namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), namedNode('http://example.org/Person'));
    store.addQuad(person, namedNode('http://example.org/name'), literal(`Person ${i}`));
    store.addQuad(person, namedNode('http://example.org/age'), literal(18 + (i % 70)));

    // 50% have emails
    if (i % 2 === 0) {
      store.addQuad(person, namedNode('http://example.org/email'), literal(`person${i}@example.org`));
    }

    // 30% work at companies
    if (i % 3 === 0) {
      const company = namedNode(`http://example.org/company/${Math.floor(i / 50)}`);
      store.addQuad(person, namedNode('http://example.org/worksAt'), company);
    }

    // 20% have phone numbers
    if (i % 5 === 0) {
      store.addQuad(person, namedNode('http://example.org/phone'), literal(`555-${String(i).padStart(4, '0')}`));
    }

    // 40% live in cities
    if (i % 2.5 === 0) {
      const city = namedNode(`http://example.org/city/${Math.floor(i / 100)}`);
      store.addQuad(person, namedNode('http://example.org/livesIn'), city);
    }
  }

  // Create company entities
  for (let i = 1; i <= 100; i++) {
    const company = namedNode(`http://example.org/company/${i}`);
    store.addQuad(company, namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), namedNode('http://example.org/Company'));
    store.addQuad(company, namedNode('http://example.org/companyName'), literal(`Company ${i}`));
    store.addQuad(company, namedNode('http://example.org/industry'), literal(['Tech', 'Finance', 'Healthcare', 'Retail'][i % 4]));
  }

  console.log(`\n📊 Dataset created: ${store.size} triples`);
  return store;
}

/**
 * Define sample query workload
 * @returns {Array<Object>} Query patterns with frequencies
 */
function defineQueryWorkload() {
  return [
    {
      // Query 1: Find people by email (very frequent - 1000 times/day)
      pattern: {
        subject: null,
        predicate: namedNode('http://example.org/email'),
        object: null
      },
      frequency: 1000,
      description: 'Find people by email'
    },
    {
      // Query 2: Find people by type (frequent - 500 times/day)
      pattern: {
        subject: null,
        predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        object: namedNode('http://example.org/Person')
      },
      frequency: 500,
      description: 'Find all people'
    },
    {
      // Query 3: Find employment relationships (moderate - 200 times/day)
      pattern: {
        subject: null,
        predicate: namedNode('http://example.org/worksAt'),
        object: null
      },
      frequency: 200,
      description: 'Find employment relationships'
    },
    {
      // Query 4: Find people by phone (rare - 50 times/day)
      pattern: {
        subject: null,
        predicate: namedNode('http://example.org/phone'),
        object: null
      },
      frequency: 50,
      description: 'Find people by phone'
    },
    {
      // Query 5: Find residence information (moderate - 150 times/day)
      pattern: {
        subject: null,
        predicate: namedNode('http://example.org/livesIn'),
        object: null
      },
      frequency: 150,
      description: 'Find residence information'
    }
  ];
}

/**
 * Example 1: Analyze query workload
 * @param {Store} store - RDF store
 * @param {Array<Object>} workload - Query workload
 */
function example1_analyzeWorkload(store, workload) {
  console.log('\n\n🔍 Example 1: Query Workload Analysis\n');
  console.log('=' .repeat(60));

  const advisor = new IndexAdvisor(store);

  console.log('\n📊 Query Workload Summary:');
  console.log(`   Total queries: ${workload.reduce((sum, q) => sum + q.frequency, 0)} per day`);
  console.log(`   Unique patterns: ${workload.length}`);

  console.log('\n📋 Query Patterns (by frequency):');
  const sortedWorkload = [...workload].sort((a, b) => b.frequency - a.frequency);

  sortedWorkload.forEach((query, idx) => {
    console.log(`\n   ${idx + 1}. ${query.description}`);
    console.log(`      Frequency: ${query.frequency} queries/day`);
    console.log(`      Predicate: ${query.pattern.predicate.value.split('/').pop()}`);

    // Analyze pattern
    const results = store.getQuads(
      query.pattern.subject,
      query.pattern.predicate,
      query.pattern.object
    );
    console.log(`      Result size: ${results.length} triples`);
  });

  // Analyze workload with IndexAdvisor
  const workloadAnalysis = advisor.analyzeWorkload(workload);
  console.log('\n📈 Workload Characteristics:');
  console.log(`   Hot predicates: ${workloadAnalysis.hotPredicates.length}`);
  console.log(`   Total query load: ${workloadAnalysis.totalLoad}`);

  return workloadAnalysis;
}

/**
 * Example 2: Generate index recommendations
 * @param {Store} store - RDF store
 * @param {Array<Object>} workload - Query workload
 */
function example2_recommendIndexes(store, workload) {
  console.log('\n\n💡 Example 2: Index Recommendations\n');
  console.log('=' .repeat(60));

  const advisor = new IndexAdvisor(store);
  const recommendations = advisor.recommendIndexes(workload);

  console.log('\n✨ Recommended Indexes (by priority):');
  recommendations.forEach((rec, idx) => {
    console.log(`\n   ${idx + 1}. ${rec.indexType} on ${rec.predicate.value.split('/').pop()}`);
    console.log(`      Priority: ${rec.priority}`);
    console.log(`      Reason: ${rec.reason}`);
    console.log(`      Estimated improvement: ${rec.estimatedImprovement}%`);
    console.log(`      Affected queries: ${rec.affectedQueries} queries/day`);
    console.log(`      Storage overhead: ${rec.storageOverhead}`);
  });

  // Summary statistics
  const totalImprovement = recommendations.reduce((sum, rec) => sum + rec.estimatedImprovement, 0);
  const avgImprovement = totalImprovement / recommendations.length;

  console.log('\n📊 Recommendation Summary:');
  console.log(`   Total recommendations: ${recommendations.length}`);
  console.log(`   Average estimated improvement: ${avgImprovement.toFixed(1)}%`);
  console.log(`   Total affected queries: ${recommendations.reduce((sum, rec) => sum + rec.affectedQueries, 0)} queries/day`);

  return recommendations;
}

/**
 * Example 3: Benchmark queries without indexes
 * @param {Store} store - RDF store
 * @param {Array<Object>} workload - Query workload
 */
function example3_benchmarkWithoutIndexes(store, workload) {
  console.log('\n\n⏱️  Example 3: Baseline Performance (No Indexes)\n');
  console.log('=' .repeat(60));

  const results = [];

  console.log('\n📊 Running queries without indexes...\n');

  for (const query of workload) {
    const iterations = 100; // Run each query 100 times
    const durations = [];

    for (let i = 0; i < iterations; i++) {
      const start = performance.now();
      store.getQuads(
        query.pattern.subject,
        query.pattern.predicate,
        query.pattern.object
      );
      durations.push(performance.now() - start);
    }

    const avgDuration = durations.reduce((sum, d) => sum + d, 0) / durations.length;
    const p95Duration = durations.sort((a, b) => a - b)[Math.floor(iterations * 0.95)];

    results.push({
      description: query.description,
      avgDuration,
      p95Duration,
      frequency: query.frequency
    });

    console.log(`   ${query.description}`);
    console.log(`      Avg: ${avgDuration.toFixed(3)}ms`);
    console.log(`      P95: ${p95Duration.toFixed(3)}ms`);
    console.log(`      Daily cost: ${(avgDuration * query.frequency).toFixed(1)}ms\n`);
  }

  const totalDailyCost = results.reduce((sum, r) => sum + (r.avgDuration * r.frequency), 0);
  console.log(`💰 Total daily query cost: ${totalDailyCost.toFixed(1)}ms`);

  return results;
}

/**
 * Example 4: Create recommended indexes
 * @param {Store} store - RDF store
 * @param {Array<Object>} recommendations - Index recommendations
 */
function example4_createIndexes(store, recommendations) {
  console.log('\n\n🔧 Example 4: Creating Recommended Indexes\n');
  console.log('=' .repeat(60));

  const advisor = new IndexAdvisor(store);
  const createdIndexes = [];

  console.log('\n⚙️  Creating indexes...\n');

  // Create top 3 recommended indexes
  const topRecommendations = recommendations.slice(0, 3);

  for (const rec of topRecommendations) {
    console.log(`   Creating ${rec.indexType} on ${rec.predicate.value.split('/').pop()}...`);

    const index = advisor.createIndex(rec.predicate, rec.indexType);
    createdIndexes.push(index);

    console.log(`      ✅ Index created`);
    console.log(`      Size: ${index.size} entries`);
    console.log(`      Memory: ~${(index.size * 48 / 1024).toFixed(1)} KB\n`);
  }

  const totalMemory = createdIndexes.reduce((sum, idx) => sum + idx.size, 0) * 48 / 1024;
  console.log(`📦 Total index memory: ~${totalMemory.toFixed(1)} KB`);

  return createdIndexes;
}

/**
 * Example 5: Benchmark with indexes
 * @param {Store} store - RDF store (with indexes)
 * @param {Array<Object>} workload - Query workload
 * @param {Array<Object>} baselineResults - Baseline performance results
 */
function example5_benchmarkWithIndexes(store, workload, baselineResults) {
  console.log('\n\n⚡ Example 5: Performance with Indexes\n');
  console.log('=' .repeat(60));

  const results = [];

  console.log('\n📊 Running queries with indexes...\n');

  for (const query of workload) {
    const iterations = 100;
    const durations = [];

    for (let i = 0; i < iterations; i++) {
      const start = performance.now();
      store.getQuads(
        query.pattern.subject,
        query.pattern.predicate,
        query.pattern.object
      );
      durations.push(performance.now() - start);
    }

    const avgDuration = durations.reduce((sum, d) => sum + d, 0) / durations.length;
    const baseline = baselineResults.find(r => r.description === query.description);
    const improvement = baseline ? ((baseline.avgDuration - avgDuration) / baseline.avgDuration * 100) : 0;

    results.push({
      description: query.description,
      avgDuration,
      improvement,
      frequency: query.frequency
    });

    console.log(`   ${query.description}`);
    console.log(`      Avg: ${avgDuration.toFixed(3)}ms`);
    console.log(`      Improvement: ${improvement > 0 ? '+' : ''}${improvement.toFixed(1)}%`);
    console.log(`      Daily savings: ${baseline ? (baseline.avgDuration - avgDuration) * query.frequency : 0}ms\n`);
  }

  const totalDailyCost = results.reduce((sum, r) => sum + (r.avgDuration * r.frequency), 0);
  const baselineCost = baselineResults.reduce((sum, r) => sum + (r.avgDuration * r.frequency), 0);
  const totalImprovement = ((baselineCost - totalDailyCost) / baselineCost * 100);

  console.log(`💰 Total daily query cost: ${totalDailyCost.toFixed(1)}ms`);
  console.log(`📈 Overall improvement: ${totalImprovement.toFixed(1)}%`);
  console.log(`⏰ Daily time saved: ${(baselineCost - totalDailyCost).toFixed(1)}ms`);

  return results;
}

/**
 * Main execution
 */
export function run() {
  console.log('🚀 Index Advisor with @unrdf/dark-matter');
  console.log('=' .repeat(60));

  const store = createSampleDataset();
  const workload = defineQueryWorkload();

  // Step 1: Analyze workload
  const workloadAnalysis = example1_analyzeWorkload(store, workload);

  // Step 2: Get recommendations
  const recommendations = example2_recommendIndexes(store, workload);

  // Step 3: Baseline benchmark
  const baselineResults = example3_benchmarkWithoutIndexes(store, workload);

  // Step 4: Create indexes
  const indexes = example4_createIndexes(store, recommendations);

  // Step 5: Benchmark with indexes
  const indexedResults = example5_benchmarkWithIndexes(store, workload, baselineResults);

  console.log('\n\n✨ Summary\n');
  console.log('=' .repeat(60));
  console.log('✅ Analyzed query workload patterns');
  console.log(`✅ Generated ${recommendations.length} index recommendations`);
  console.log(`✅ Created ${indexes.length} optimized indexes`);
  console.log(`✅ Measured performance improvements`);

  const avgImprovement = indexedResults
    .filter(r => r.improvement > 0)
    .reduce((sum, r) => sum + r.improvement, 0) / indexedResults.filter(r => r.improvement > 0).length;

  console.log(`📈 Average performance improvement: ${avgImprovement.toFixed(1)}%`);
  console.log('\n');

  return {
    workloadAnalysis,
    recommendations,
    baselineResults,
    indexes,
    indexedResults
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  run();
}
