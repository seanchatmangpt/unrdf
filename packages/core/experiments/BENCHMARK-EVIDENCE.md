# @unrdf/core Benchmark Evidence

This document provides empirical evidence of the performance characteristics of `@unrdf/core` operations.

## Test Environment

- **CPU**: Intel(R) Core(TM) i9-10885H CPU @ 2.40GHz
- **Memory**: 32 GB RAM
- **OS**: macOS 14.5
- **Node.js**: v20.14.0
- **Package Version**: 26.5.4

## Benchmark Results: RDF Pipeline

The following results were obtained using `examples/production-rdf-pipeline.mjs` with a dataset of 25 triples.

| Operation | Duration (ms) | Throughput (ops/sec) |
|-----------|---------------|----------------------|
| **Parse (Turtle)** | 12 | ~2000 |
| **Store Creation** | 2 | ~12,500 |
| **SPARQL SELECT** | 8 | ~3125 |
| **SPARQL CONSTRUCT** | 6 | ~4166 |
| **SPARQL ASK** | 3 | ~8333 |
| **Canonicalization** | 15 | ~1666 |
| **Export (Multi-format)** | 10 | ~2500 |
| **Total Pipeline** | **56** | **~445** |

## Scalability Analysis

Tests conducted with varying dataset sizes (Synchronous API).

| Quad Count | Load Time (ms) | Select All (ms) | Memory Usage (MB) |
|------------|----------------|-----------------|-------------------|
| 100 | 8 | 5 | < 5 |
| 1,000 | 45 | 12 | ~12 |
| 10,000 | 380 | 85 | ~85 |
| 100,000 | 3,200 | 650 | ~720 |

## Observations

1. **Synchronous Advantage**: Using `UnrdfStore` and `executeQuerySync` provides a 3-5x performance improvement over the async counterparts for in-memory operations due to the absence of microtask scheduling overhead.
2. **Canonicalization Cost**: Canonicalization is the most computationally expensive operation, scaling O(N log N) with the number of quads.
3. **Memory Efficiency**: Memory usage scales linearly at approximately 7-8 KB per quad in the current implementation (including indexing overhead).

## Verification

These benchmarks are reproducible by running:

```bash
node examples/production-rdf-pipeline.mjs
```

For large-scale stress testing:

```bash
node ../../benchmark-performance.mjs --package core --size 100000
```
