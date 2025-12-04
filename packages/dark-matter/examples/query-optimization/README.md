# Query Optimization Example

This example demonstrates how to use `@unrdf/dark-matter` to optimize SPARQL query execution through pattern analysis, cardinality estimation, and join order optimization.

## Features

- **Pattern Analysis**: Analyze query patterns to estimate cardinality and selectivity
- **Join Order Optimization**: Reorder query patterns for optimal execution
- **Performance Measurement**: Measure actual query performance improvements
- **Optimization Recommendations**: Generate actionable optimization suggestions

## Running the Example

```bash
# From the example directory
pnpm install
pnpm start

# Run tests
pnpm test
```

## What You'll Learn

### 1. Query Pattern Analysis

Learn how to analyze individual query patterns to estimate:
- **Cardinality**: How many results a pattern will match
- **Selectivity**: What percentage of the dataset matches (0-1, lower is better)

```javascript
const analyzer = new QueryAnalyzer(store);
const analysis = analyzer.analyzePattern({
  subject: null,
  predicate: namedNode('ex:email'),
  object: null
});

console.log(analysis.cardinality);  // 100
console.log(analysis.selectivity);  // 0.1 (10% of triples)
```

### 2. Join Order Optimization

Discover how to optimize multi-pattern queries by:
- Starting with the most selective patterns (lowest cardinality)
- Reducing intermediate result sets
- Minimizing overall query cost

**Bad Order** (start with high cardinality):
```sparql
?person rdf:type ex:Person .     # 1000 results
?person ex:email ?email .         # Filter to 100
```

**Good Order** (start with low cardinality):
```sparql
?person ex:email ?email .         # 100 results
?person rdf:type ex:Person .      # Already filtered
```

### 3. Performance Measurement

Measure actual query performance to validate optimizations:
- Execute queries with different pattern orders
- Compare execution times
- Calculate speedup ratios

Typical results show **2-10x** performance improvements for optimized queries.

### 4. Optimization Recommendations

Generate optimization recommendations:
- Rank patterns by selectivity
- Suggest optimal execution order
- Identify potential bottlenecks

## Key Concepts

### Cardinality Estimation

Cardinality is the estimated number of results for a pattern. Lower cardinality patterns should be executed first to reduce intermediate result sizes.

### Selectivity

Selectivity measures how much a pattern filters the dataset (0-1 scale):
- **0.01** = Very selective (1% of triples match) → Execute first
- **0.5** = Moderately selective (50% match)
- **0.9** = Poorly selective (90% match) → Execute last

### Join Order

The order of pattern execution significantly impacts performance:
- **Cost = Sum of intermediate result sizes**
- Start with low cardinality patterns to minimize cost
- Each subsequent pattern filters the results further

## Example Output

```
🔍 Example 1: Query Pattern Analysis
============================================================

📋 Pattern 1: Find all people
   Pattern: ?person rdf:type ex:Person
   Estimated cardinality: 1000
   Selectivity: 33.33%

📋 Pattern 2: Find people with email
   Pattern: ?person ex:email ?email
   Estimated cardinality: 100
   Selectivity: 3.33%

⚡ Example 2: Join Order Optimization
============================================================

📊 Cost Comparison:
   Bad order cost:  1100 intermediate results
   Good order cost: 200 intermediate results
   Improvement:     81.8% reduction

⏱️  Example 3: Performance Measurement
============================================================

❌ Testing Bad Order Query...
   Results: 100
   Time: 5.234ms

✅ Testing Optimized Query...
   Results: 100
   Time: 1.829ms

📈 Performance Improvement:
   Speedup: 2.86x faster
   Time saved: 3.405ms
```

## Integration with Your Application

```javascript
import { QueryAnalyzer } from '@unrdf/dark-matter';
import { Store } from 'n3';

// Create analyzer for your store
const analyzer = new QueryAnalyzer(store);

// Analyze your query patterns
const patterns = [/* your SPARQL patterns */];
const analyses = patterns.map(p => analyzer.analyzePattern(p));

// Sort by selectivity (most selective first)
const optimizedOrder = analyses
  .map((a, idx) => ({ analysis: a, pattern: patterns[idx] }))
  .sort((a, b) => a.analysis.selectivity - b.analysis.selectivity);

// Execute patterns in optimized order
for (const { pattern } of optimizedOrder) {
  // Execute pattern...
}
```

## Further Reading

- [Query Optimization Basics](https://en.wikipedia.org/wiki/Query_optimization)
- [Join Order Optimization](https://en.wikipedia.org/wiki/Join_(SQL)#Join_ordering)
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
