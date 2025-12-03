/**
 * @unrdf/dark-matter - Basic Example
 *
 * Demonstrates query optimization and performance analysis.
 */

import { Store } from 'n3';
import {
  analyzeSparqlQuery,
  estimateComplexity,
  identifyBottlenecks,
  optimizeQuery,
  suggestIndexes,
  createMetricsCollector,
  analyzeIndexNeeds,
  suggestIndexForPattern,
} from '../src/index.mjs';

console.log('🌑 @unrdf/dark-matter - Query Optimization Example\n');

// Example SPARQL query
const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name ?email WHERE {
    ?person foaf:knows ?friend .
    ?person foaf:name ?name .
    ?person foaf:mbox ?email .
    FILTER(regex(?name, "Alice"))
  }
`;

console.log('📊 1. Analyze Query Structure');
console.log('=============================');
const analysis = analyzeSparqlQuery(query);
console.log('Query type:', analysis.type);
console.log('Variables:', analysis.variables);
console.log('Patterns:', analysis.patterns.length);
console.log('Joins:', analysis.joins.length);
console.log('Estimated results:', analysis.estimatedResults);
console.log('Critical path patterns:', analysis.criticalPath);
console.log();

console.log('📈 2. Estimate Complexity');
console.log('=========================');
const complexity = estimateComplexity(query);
console.log('Complexity score:', complexity, '/ 10');
console.log();

console.log('🔍 3. Identify Bottlenecks');
console.log('==========================');
const bottlenecks = identifyBottlenecks(query);
if (bottlenecks.length > 0) {
  bottlenecks.forEach((b) => {
    console.log(`- ${b.type} (${b.severity}): ${b.description}`);
    console.log(`  Recommendation: ${b.recommendation}`);
  });
} else {
  console.log('No major bottlenecks detected');
}
console.log();

console.log('⚡ 4. Optimize Query');
console.log('===================');
const optimization = optimizeQuery(query);
console.log('Estimated speedup:', optimization.estimatedSpeedup + 'x');
console.log('Changes applied:');
optimization.changes.forEach((change) => {
  console.log(`- ${change.type}: ${change.description}`);
});
console.log();
console.log('Optimized query:');
console.log(optimization.optimizedQuery);
console.log();

console.log('📇 5. Index Recommendations');
console.log('===========================');
const store = new Store();
const indexSuggestions = suggestIndexes(store, query);
console.log('Suggested indexes:');
indexSuggestions.forEach((suggestion) => {
  console.log(`- ${suggestion.type} (${suggestion.priority}): ${suggestion.reason}`);
});
console.log();

console.log('📊 6. Performance Metrics');
console.log('=========================');
const metrics = createMetricsCollector();

// Simulate query executions
metrics.recordQuery(query, 150, 42);
metrics.recordQuery(query, 120, 38);
metrics.recordQuery('SELECT ?s WHERE { ?s ?p ?o }', 50, 1000);

const stats = metrics.analyzePerformance();
console.log('Total queries:', stats.totalQueries);
console.log('Average execution time:', stats.averageExecutionTime + 'ms');
console.log();
console.log('Slowest queries:');
stats.slowestQueries.forEach((q, i) => {
  console.log(`${i + 1}. ${q.executionTime}ms - ${q.resultCount} results`);
});
console.log();

console.log('🔧 7. Index Analysis');
console.log('====================');
const queryLog = [
  'SELECT ?name WHERE { ?s <http://xmlns.com/foaf/0.1/name> ?name }',
  'SELECT ?name WHERE { ?s <http://xmlns.com/foaf/0.1/name> ?name }',
  'SELECT ?email WHERE { ?s <http://xmlns.com/foaf/0.1/mbox> ?email }',
];

const indexRecommendations = analyzeIndexNeeds(store, queryLog);
console.log('Index recommendations based on query log:');
indexRecommendations.slice(0, 3).forEach((rec) => {
  console.log(`- ${rec.type} (${rec.priority}): ${rec.reason}`);
  console.log(`  Estimated benefit: ${rec.estimatedBenefit}%`);
});
console.log();

console.log('🎯 8. Pattern-Specific Index');
console.log('=============================');
const pattern = {
  subject: '?s',
  predicate: '<http://xmlns.com/foaf/0.1/name>',
  object: '?name',
};

const patternSuggestion = suggestIndexForPattern(pattern);
console.log('Pattern:', JSON.stringify(pattern));
console.log('Recommendation:', patternSuggestion.type);
console.log('Priority:', patternSuggestion.priority);
console.log('Estimated benefit:', patternSuggestion.estimatedBenefit + '%');
console.log('Reason:', patternSuggestion.reason);
console.log();

console.log('✅ Dark Matter optimization complete!');
console.log();
console.log('💡 Key Takeaways:');
console.log('- Analyze queries before execution');
console.log('- Optimize pattern order for better performance');
console.log('- Track metrics to identify slow queries');
console.log('- Add indexes for frequently-queried predicates');
console.log('- Apply 80/20 principle: optimize the 20% that matters');
