# Dark Matter 80/20 Query Optimization

## Overview

The Dark Matter 80/20 Query Optimization system implements intelligent query analysis and optimization based on the Pareto principle: **20% of queries typically account for 80% of performance impact**.

This system focuses optimization efforts on the critical path - the small number of queries that have the largest impact on overall system performance.

## Architecture

The system consists of three main components:

### 1. Query Analyzer (`query-analyzer.mjs`)

Analyzes SPARQL queries to extract patterns and calculate complexity scores.

**Features:**
- Triple pattern extraction
- Complexity scoring
- Expensive operation identification
- JOIN detection
- Filter and aggregation analysis

**Complexity Factors:**
- Variable positions (subject, predicate, object)
- JOIN operations (shared variables)
- FILTER clauses
- OPTIONAL clauses (left outer joins)
- UNION operations
- Aggregations (COUNT, SUM, AVG, etc.)
- DISTINCT, GROUP BY, ORDER BY modifiers

### 2. Critical Path Identifier (`critical-path.mjs`)

Implements the 80/20 algorithm to identify the top 20% slowest queries.

**Features:**
- Query execution logging
- 80/20 analysis
- Percentile calculation (P50, P90, P99)
- Impact ratio calculation
- Markdown report generation

**Algorithm:**
1. Log all query executions with timing data
2. Group queries by query ID
3. Calculate total execution time per query
4. Sort queries by total time (descending)
5. Identify top 20% of queries that account for 80% of total time
6. Generate optimization recommendations

### 3. Query Optimizer (`optimizer.mjs`)

Applies query rewrite rules to optimize SPARQL queries.

**Optimization Rules:**

#### High Impact Rules (80% of optimization value)

1. **Filter Pushdown**
   - Push FILTER clauses closer to triple patterns
   - Reduces intermediate result sizes
   - Impact: HIGH

2. **Join Reordering**
   - Reorder triple patterns by selectivity
   - Most selective patterns first (concrete > variable)
   - Minimizes intermediate results
   - Impact: HIGH

3. **Early LIMIT**
   - Add LIMIT clause to unbounded queries
   - Prevents runaway result sets
   - Impact: HIGH

#### Medium Impact Rules (remaining 20%)

4. **OPTIONAL Optimization**
   - Replace OPTIONAL with FILTER EXISTS when possible
   - Reduces left join overhead
   - Impact: MEDIUM

5. **UNION Optimization**
   - Optimize UNION clauses
   - Consider alternatives like VALUES or property paths
   - Impact: MEDIUM

6. **Index Hints**
   - Add hints for common access patterns
   - Database-specific
   - Impact: MEDIUM

## Usage

### Basic Analysis

```javascript
import { createQueryAnalyzer } from './query-analyzer.mjs';

const analyzer = createQueryAnalyzer();

const query = `
  SELECT ?name ?age WHERE {
    ?person <http://example.org/name> ?name .
    ?person <http://example.org/age> ?age .
  }
`;

const analysis = analyzer.analyze(query, 'user-query-1');

console.log('Complexity Score:', analysis.complexity.score);
console.log('Expensive Operations:', analysis.expensiveOperations);
console.log('JOIN Count:', analysis.joins.length);
```

### Critical Path Identification

```javascript
import { createCriticalPathIdentifier } from './critical-path.mjs';

const criticalPath = createCriticalPathIdentifier();

// Log query executions
criticalPath.logExecution('query-1', queryString, 150); // 150ms
criticalPath.logExecution('query-2', queryString, 50);  // 50ms
criticalPath.logExecution('query-1', queryString, 200); // 200ms (same query, different execution)

// Identify critical queries
const analysis = criticalPath.identify();

console.log('Critical Queries:', analysis.criticalQueries.length);
console.log('Impact Ratio:', analysis.metrics.impactRatio);
console.log('P99 Latency:', analysis.metrics.p99);

// Generate report
const report = criticalPath.getReport();
console.log(report);
```

### Query Optimization

```javascript
import { createDarkMatterOptimizer } from './optimizer.mjs';

const optimizer = createDarkMatterOptimizer();

const query = `
  SELECT ?name WHERE {
    ?person <http://example.org/knows> ?friend .
    ?friend <http://example.org/name> ?name .
    OPTIONAL { ?person <http://example.org/age> ?age }
    FILTER(?age > 18)
  }
`;

const result = optimizer.optimize(query);

console.log('Original Query:', result.original);
console.log('Optimized Query:', result.optimized);
console.log('Rules Applied:', result.rules.filter(r => r.applied));
console.log('Estimated Improvement:', result.estimatedImprovement.percentageGain);
```

### Integrated System

```javascript
import { createDarkMatterQuerySystem } from './index.mjs';

const system = createDarkMatterQuerySystem({
  complexityThreshold: 100,
  enableAutoOptimization: true
});

// Process query execution
const result = system.processExecution(query, executionTime);

// Get critical queries
const critical = system.identifyCriticalQueries();

// Generate comprehensive report
const report = system.getReport();
console.log(report);
```

## Performance Targets

Based on Dark Matter 80/20 principles:

- **P50 (median)**: < 50ms for optimized queries
- **P90**: < 200ms
- **P99**: < 500ms
- **Critical path**: Top 20% of queries identified and optimized
- **Impact ratio**: 80%+ of performance from critical path optimization

## Query Complexity Scoring

Complexity is calculated based on:

```
score = patternCost + filterCost + joinCost + aggregationCost

Where:
- patternCost = Σ(pattern.complexity)
- filterCost = filterCount × 5
- joinCost = Σ(join.estimatedCost)
- aggregationCost = aggregationCount × 8

Modifiers:
- GROUP BY: score × 1.5
- ORDER BY: score × 1.2
- DISTINCT: score × 1.3
```

Pattern complexity:
- Variable in subject: +5
- Variable in predicate: +10 (very expensive - full graph scan)
- Variable in object: +3

## Best Practices

### 1. Focus on Critical Path

Don't optimize everything - focus on the 20% that matters:

```javascript
// Identify critical queries first
const critical = system.identifyCriticalQueries();

// Optimize only critical queries
for (const query of critical.criticalQueries) {
  const optimized = system.optimize(query.query);
  // Deploy optimized version
}
```

### 2. Monitor Continuously

Keep logging query executions to adapt to changing patterns:

```javascript
// Log every query execution
function executeQuery(query, queryId) {
  const startTime = Date.now();
  const result = await runQuery(query);
  const executionTime = Date.now() - startTime;

  system.processExecution(query, executionTime, queryId);

  return result;
}
```

### 3. Set Appropriate Thresholds

Configure thresholds based on your workload:

```javascript
const system = createDarkMatterQuerySystem({
  // Only optimize queries with complexity > 100
  complexityThreshold: 100,

  // Target 80% impact from top 20% queries
  targetImpactRatio: 0.8,
  targetQueryRatio: 0.2,

  // Require at least 50 samples before analysis
  minSampleSize: 50
});
```

### 4. Validate Optimizations

Always validate that optimizations actually improve performance:

```javascript
const result = optimizer.optimize(query);

// Test both versions
const originalTime = await benchmark(result.original);
const optimizedTime = await benchmark(result.optimized);

if (optimizedTime < originalTime) {
  // Deploy optimized version
  console.log('Improvement:', ((originalTime - optimizedTime) / originalTime) * 100, '%');
} else {
  // Stick with original
  console.warn('Optimization did not improve performance');
}
```

## Integration with UNRDF

The Dark Matter query optimizer integrates with the existing UNRDF infrastructure:

```javascript
import { DarkMatterCore } from './dark-matter-core.mjs';
import { createDarkMatterQuerySystem } from './dark-matter/index.mjs';

const darkMatter = new DarkMatterCore({
  enablePerformanceOptimizer: true
});

await darkMatter.initialize();

// Get performance optimizer component
const perfOptimizer = darkMatter.getComponent('performanceOptimizer');

// Create query system
const querySystem = createDarkMatterQuerySystem();

// Integrate with transaction execution
async function executeTransaction(store, delta) {
  const queryId = crypto.randomUUID();
  const startTime = Date.now();

  const result = await darkMatter.executeTransaction(store, delta);

  const executionTime = Date.now() - startTime;

  // Log for critical path analysis
  querySystem.logExecution(queryId, JSON.stringify(delta), executionTime);

  return result;
}
```

## Observability

The system integrates with UNRDF's observability framework:

```javascript
const observability = darkMatter.getComponent('observability');

// Track query optimization metrics
observability.recordMetric('query.optimization.applied', {
  queryId,
  rules: result.rules.filter(r => r.applied).length,
  improvement: result.estimatedImprovement.percentageGain
});

// Track critical path changes
observability.recordMetric('query.critical_path.size', {
  criticalQueries: critical.criticalQueries.length,
  impactRatio: critical.metrics.impactRatio
});
```

## Testing

Comprehensive test coverage ensures correctness:

```bash
# Run Dark Matter query optimization tests
npm run test:dark-matter

# Run with coverage
npm test -- test/knowledge-engine/dark-matter/
```

## Future Enhancements

1. **ML Pattern Recognition** (optional, high value)
   - Train models on query execution patterns
   - Predict slow queries before execution
   - Recommend optimizations based on historical data

2. **Adaptive Optimization**
   - Automatically adjust thresholds based on workload
   - Learn which optimizations work best for specific query patterns

3. **Query Plan Caching**
   - Cache optimized queries for reuse
   - Invalidate cache when data distribution changes

4. **Distributed Query Optimization**
   - Optimize queries across federated sources
   - Consider network latency in cost estimation

## References

- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [Query Optimization in RDF Databases](https://link.springer.com/chapter/10.1007/978-3-642-25073-6_10)
- [Pareto Principle (80/20 Rule)](https://en.wikipedia.org/wiki/Pareto_principle)
