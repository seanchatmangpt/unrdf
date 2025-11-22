#!/usr/bin/env node
/**
 * @file Dark Matter Query Optimization Example
 * @description Demonstrates query analysis, critical path identification, and optimization
 */

import { createDarkMatterQuerySystem } from '../src/knowledge-engine/dark-matter/index.mjs';

console.log('\n🌌 Dark Matter 80/20 Query Optimization Example\n');

// Create the integrated system
const system = createDarkMatterQuerySystem({
  complexityThreshold: 50,
  enableAutoOptimization: true,
});

// Example queries
const queries = [
  {
    id: 'simple-lookup',
    query: `
      SELECT ?name WHERE {
        <http://example.org/alice> <http://example.org/name> ?name .
      }
    `,
    executionTime: 5,
  },
  {
    id: 'complex-join',
    query: `
      SELECT ?name ?friendName WHERE {
        ?person <http://example.org/name> ?name .
        ?person <http://example.org/knows> ?friend .
        ?friend <http://example.org/name> ?friendName .
      }
    `,
    executionTime: 150,
  },
  {
    id: 'expensive-scan',
    query: `
      SELECT ?s ?p ?o WHERE {
        ?s ?p ?o .
        FILTER(?p = <http://example.org/type>)
      }
    `,
    executionTime: 450,
  },
  {
    id: 'aggregation-query',
    query: `
      SELECT (COUNT(?person) as ?count) ?name WHERE {
        ?person <http://example.org/name> ?name .
      }
      GROUP BY ?name
    `,
    executionTime: 200,
  },
];

console.log('📊 Step 1: Analyzing Queries\n');

for (const { id, query, executionTime } of queries) {
  console.log(`\nQuery ID: ${id}`);
  console.log(`Execution Time: ${executionTime}ms`);

  // Analyze query
  const analysis = system.analyze(query, id);

  console.log(`  - Type: ${analysis.type}`);
  console.log(`  - Complexity Score: ${analysis.complexity.score}`);
  console.log(`  - Patterns: ${analysis.patterns.length}`);
  console.log(`  - Filters: ${analysis.filters.length}`);
  console.log(`  - JOINs: ${analysis.joins.length}`);
  console.log(`  - Expensive Operations: ${analysis.expensiveOperations.length}`);

  if (analysis.expensiveOperations.length > 0) {
    console.log('  - Most Expensive:');
    for (const op of analysis.expensiveOperations.slice(0, 2)) {
      console.log(`    * ${op.type}: ${op.reason} (cost: ${op.cost})`);
    }
  }

  // Log execution
  system.logExecution(id, query, executionTime);
}

console.log('\n\n🎯 Step 2: Identifying Critical Path (80/20 Analysis)\n');

try {
  const criticalPath = system.identifyCriticalQueries();

  console.log('Metrics:');
  console.log(`  - Total Queries: ${criticalPath.metrics.totalQueries}`);
  console.log(
    `  - Critical Queries: ${criticalPath.metrics.criticalQueryCount} (${criticalPath.metrics.criticalQueryPercentage.toFixed(1)}%)`
  );
  console.log(`  - Impact Ratio: ${(criticalPath.metrics.impactRatio * 100).toFixed(1)}%`);
  console.log(`  - P50 Latency: ${criticalPath.metrics.p50.toFixed(2)}ms`);
  console.log(`  - P90 Latency: ${criticalPath.metrics.p90.toFixed(2)}ms`);
  console.log(`  - P99 Latency: ${criticalPath.metrics.p99.toFixed(2)}ms`);

  console.log('\nCritical Queries (Top Impact):');
  for (const query of criticalPath.criticalQueries) {
    console.log(`  ${query.rank}. ${query.queryId}`);
    console.log(`     Occurrences: ${query.occurrences}`);
    console.log(`     Avg Time: ${query.executionTime.toFixed(2)}ms`);
    console.log(`     Total Time: ${query.totalTime.toFixed(2)}ms`);
    console.log(`     % of Total: ${query.percentageOfTotal.toFixed(1)}%`);
  }
} catch (error) {
  console.log(`  ⚠️ ${error.message}`);
}

console.log('\n\n⚡ Step 3: Optimizing Queries\n');

for (const { id, query } of queries.slice(1)) {
  // Skip simple queries
  console.log(`\nOptimizing: ${id}`);

  const optimization = system.optimize(query);

  console.log(`  - Rules Applied: ${optimization.rules.filter(r => r.applied).length}`);
  console.log(
    `  - Estimated Improvement: ${optimization.estimatedImprovement.percentageGain.toFixed(1)}%`
  );

  if (optimization.rules.some(r => r.applied)) {
    console.log('  - Applied Rules:');
    for (const rule of optimization.rules.filter(r => r.applied)) {
      console.log(`    * ${rule.name} (${rule.impact} impact): ${rule.description}`);
    }
  }

  if (optimization.skipped) {
    console.log(`  - Skipped: ${optimization.reason}`);
  }
}

console.log('\n\n📈 Step 4: System Statistics\n');

const stats = system.getStats();

console.log('Analyzer Statistics:');
console.log(`  - Total Analyzed: ${stats.analyzer.totalAnalyzed}`);
console.log(`  - Complex Queries: ${stats.analyzer.complexQueries}`);
console.log(`  - Simple Queries: ${stats.analyzer.simpleQueries}`);
console.log(`  - Avg Complexity: ${stats.analyzer.avgComplexity.toFixed(2)}`);

console.log('\nOptimizer Statistics:');
console.log(`  - Total Optimizations: ${stats.optimizer.totalOptimizations}`);
console.log('  - Rules Applied:');
for (const [rule, count] of Object.entries(stats.optimizer.rulesApplied)) {
  console.log(`    * ${rule}: ${count}`);
}

console.log('\nCritical Path Statistics:');
if (stats.criticalPath.error) {
  console.log(`  - Status: ${stats.criticalPath.error}`);
} else {
  console.log(`  - Impact Ratio: ${(stats.criticalPath.impactRatio * 100).toFixed(1)}%`);
  console.log(`  - Critical Queries: ${stats.criticalPath.criticalQueryCount}`);
}

console.log('\n\n📄 Step 5: Comprehensive Report\n');
console.log('─'.repeat(80));
console.log(system.getReport());

console.log('\n✅ Dark Matter 80/20 Query Optimization Demo Complete!\n');
console.log('Key Takeaways:');
console.log('  • Focus optimization on the critical path (20% of queries)');
console.log('  • These critical queries account for 80% of performance impact');
console.log('  • Complexity scoring helps identify expensive operations');
console.log('  • Query rewrite rules provide automatic optimizations');
console.log('  • Continuous monitoring adapts to changing query patterns\n');
