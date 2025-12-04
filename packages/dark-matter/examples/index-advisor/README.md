# Index Advisor Example

This example demonstrates how to use `@unrdf/dark-matter` to analyze query workloads, recommend optimal indexes, and measure performance improvements.

## Features

- **Workload Analysis**: Analyze query patterns and frequencies
- **Index Recommendations**: Generate prioritized index recommendations
- **Performance Benchmarking**: Measure query performance before/after indexing
- **Index Creation**: Create recommended indexes automatically
- **Cost Analysis**: Calculate query costs and savings

## Running the Example

```bash
# From the example directory
pnpm install
pnpm start

# Run tests
pnpm test
```

## What You'll Learn

### 1. Query Workload Analysis

Learn how to analyze your application's query patterns:
- Identify hot predicates (frequently queried)
- Calculate query frequencies
- Measure result set sizes
- Assess workload characteristics

```javascript
const advisor = new IndexAdvisor(store);
const workload = [
  {
    pattern: { subject: null, predicate: emailPred, object: null },
    frequency: 1000,  // queries per day
    description: 'Find people by email'
  },
  // ... more patterns
];

const analysis = advisor.analyzeWorkload(workload);
console.log(analysis.hotPredicates);  // Most frequently queried predicates
console.log(analysis.totalLoad);      // Total query volume
```

### 2. Index Recommendations

Discover how the advisor generates optimal index recommendations:
- Prioritize by query frequency and cardinality
- Estimate performance improvements
- Calculate storage overhead
- Identify affected queries

```javascript
const recommendations = advisor.recommendIndexes(workload);

recommendations.forEach(rec => {
  console.log(`${rec.indexType} on ${rec.predicate}`);
  console.log(`Priority: ${rec.priority}`);
  console.log(`Estimated improvement: ${rec.estimatedImprovement}%`);
  console.log(`Affected queries: ${rec.affectedQueries}/day`);
});
```

### 3. Performance Benchmarking

Measure actual query performance:
- Baseline measurements (no indexes)
- Indexed measurements (with indexes)
- Calculate improvements and savings

```javascript
// Baseline
const start = performance.now();
const results = store.getQuads(null, predicate, null);
const duration = performance.now() - start;

// With index
const indexedDuration = measureWithIndex();
const improvement = (duration - indexedDuration) / duration * 100;
console.log(`Improvement: ${improvement}%`);
```

### 4. Index Creation

Create recommended indexes automatically:
- Hash indexes for equality lookups
- Tree indexes for range queries
- Predicate indexes for pattern matching

```javascript
const advisor = new IndexAdvisor(store);

// Create index from recommendation
const index = advisor.createIndex(
  recommendation.predicate,
  recommendation.indexType
);

console.log(`Index created: ${index.size} entries`);
```

## Key Concepts

### Index Types

**Hash Index**
- Best for: Equality lookups (`?s ex:email "user@example.org"`)
- Characteristics: O(1) lookup, no range queries
- Use when: High-frequency exact match queries

**Tree Index**
- Best for: Range queries (`?s ex:age ?age . FILTER(?age > 30)`)
- Characteristics: O(log n) lookup, supports ranges
- Use when: Numerical or sorted data queries

**Predicate Index**
- Best for: Pattern matching (`?s ?p ?o` with variable predicate)
- Characteristics: Groups by predicate, fast filtering
- Use when: Queries scan multiple predicates

### Recommendation Priority

The advisor prioritizes indexes based on:
1. **Query Frequency**: How often the pattern is queried
2. **Cardinality**: Size of result sets (lower = higher priority)
3. **Selectivity**: How much the pattern filters (lower = higher priority)
4. **Storage Cost**: Memory overhead (lower = higher priority)

### Cost Analysis

Query cost is measured as:
```
Cost = avg_query_time × query_frequency
```

Total savings from indexes:
```
Savings = baseline_cost - indexed_cost
Improvement = (savings / baseline_cost) × 100%
```

## Example Output

```
🔍 Example 1: Query Workload Analysis
============================================================

📊 Query Workload Summary:
   Total queries: 1900 per day
   Unique patterns: 5

📋 Query Patterns (by frequency):

   1. Find people by email
      Frequency: 1000 queries/day
      Result size: 2500 triples

   2. Find all people
      Frequency: 500 queries/day
      Result size: 5000 triples

💡 Example 2: Index Recommendations
============================================================

✨ Recommended Indexes (by priority):

   1. hash on email
      Priority: HIGH
      Reason: High frequency, low cardinality
      Estimated improvement: 75%
      Affected queries: 1000 queries/day
      Storage overhead: 2500 entries

   2. predicate on type
      Priority: MEDIUM
      Reason: Moderate frequency, high cardinality
      Estimated improvement: 45%
      Affected queries: 500 queries/day
      Storage overhead: 5000 entries

⏱️  Example 3: Baseline Performance (No Indexes)
============================================================

   Find people by email
      Avg: 1.234ms
      P95: 1.567ms
      Daily cost: 1234.0ms

   Find all people
      Avg: 2.345ms
      P95: 2.789ms
      Daily cost: 1172.5ms

💰 Total daily query cost: 2406.5ms

⚡ Example 5: Performance with Indexes
============================================================

   Find people by email
      Avg: 0.312ms
      Improvement: +74.7%
      Daily savings: 922ms

   Find all people
      Avg: 1.234ms
      Improvement: +47.4%
      Daily savings: 555ms

💰 Total daily query cost: 1429.5ms
📈 Overall improvement: 40.6%
⏰ Daily time saved: 977.0ms
```

## Integration with Your Application

```javascript
import { IndexAdvisor } from '@unrdf/dark-matter';

// 1. Define your query workload
const workload = [
  {
    pattern: { subject: null, predicate: emailPred, object: null },
    frequency: 1000,
    description: 'Email lookup'
  },
  // ... your query patterns
];

// 2. Get recommendations
const advisor = new IndexAdvisor(store);
const recommendations = advisor.recommendIndexes(workload);

// 3. Create top recommendations
const topRecs = recommendations.slice(0, 3);
for (const rec of topRecs) {
  advisor.createIndex(rec.predicate, rec.indexType);
}

// 4. Use indexed store normally
const results = store.getQuads(null, emailPred, null);
// Automatically uses index if available
```

## Best Practices

1. **Analyze Before Indexing**: Always analyze your actual query workload
2. **Prioritize High-Impact**: Create indexes for high-frequency, low-cardinality queries first
3. **Monitor Storage**: Track index memory usage and balance with performance gains
4. **Benchmark Regularly**: Measure actual improvements to validate recommendations
5. **Update Workload**: Re-analyze as query patterns change

## Further Reading

- [Database Indexing Fundamentals](https://en.wikipedia.org/wiki/Database_index)
- [Query Optimization](https://en.wikipedia.org/wiki/Query_optimization)
- [Cardinality Estimation](https://en.wikipedia.org/wiki/Cardinality_(SQL_statements))


## Testing

Run the test suite:

```bash
pnpm test
```

Run tests in watch mode:

```bash
pnpm test:watch
```

Generate coverage report:

```bash
pnpm test:coverage
```

Test coverage: 80%+ (minimum requirement)
