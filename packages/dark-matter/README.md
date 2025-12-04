# @unrdf/dark-matter

**Query Optimization and Performance Analysis** *(Optional Extension)*

Optimize SPARQL queries and analyze RDF graph performance. Implements the 80/20 principle for query optimization.

## Installation

```bash
pnpm add @unrdf/dark-matter
```

## 📚 Examples

See these examples that demonstrate @unrdf/dark-matter:

- **[dark-matter-80-20.mjs](../../examples/dark-matter-80-20.mjs)** - Query optimization basics (30 min, 5-10x speedup)
- **[dark-matter-query-optimization.mjs](../../examples/dark-matter-query-optimization.mjs)** - Advanced optimization strategies
- **[profiling-example.mjs](../../examples/profiling-example.mjs)** - Performance profiling and analysis
- **[lockchain-dark-matter-test.mjs](../../examples/lockchain-dark-matter-test.mjs)** - Combined optimization + audit

**Need faster queries?** Start with [dark-matter-80-20.mjs](../../examples/dark-matter-80-20.mjs).

## Quick Start

```javascript
import { optimizeQuery, analyzePerformance } from '@unrdf/dark-matter'

// Optimize a SPARQL query
const optimized = optimizeQuery(
  'SELECT ?name WHERE { ?s foaf:name ?name. ?s foaf:age ?age }'
)

// Analyze graph performance
const metrics = await analyzePerformance(store)
console.log('Query execution time:', metrics.queryTime)
```

## Features

- ✅ SPARQL query optimization
- ✅ Critical path analysis
- ✅ Performance metrics collection
- ✅ Query plan analysis
- ✅ Index recommendations
- ✅ Bottleneck identification

## Use Cases

- **Performance tuning**: Optimize slow queries
- **Capacity planning**: Understand resource usage
- **Monitoring**: Track graph performance
- **Optimization**: Find and fix bottlenecks
- **Benchmarking**: Compare query strategies

## Documentation

- **[API Reference](./docs/API.md)** - Complete API documentation
- **[User Guide](./docs/GUIDE.md)** - Optimization guide
- **[Examples](./examples/)** - Code examples
- **[Contributing](./docs/CONTRIBUTING.md)** - How to contribute

## Status

**Optional Extension** - Use only if you need query optimization.
Most applications don't need this.

## Depends On

- `@unrdf/core` - RDF substrate

## VOC Usage

- Performance-critical applications

## License

MIT
