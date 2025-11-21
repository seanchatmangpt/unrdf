# Performance Metrics (Shared)

Standard performance characteristics for UNRDF features.

## Measurement Categories

### Latency
- **P50**: 50th percentile (median)
- **P95**: 95th percentile
- **P99**: 99th percentile (worst-case)

### Throughput
- **Operations/second**: Total operations per second
- **MB/second**: Data throughput for streaming

### Resource Usage
- **CPU**: Percentage of CPU utilization
- **Memory**: Peak and average memory usage
- **Network**: Bandwidth consumption

## Benchmarking Approach

```typescript
import { performance } from 'node:perf_hooks';

async function benchmark(operation, iterations = 1000) {
  const measurements = [];

  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    await operation();
    measurements.push(performance.now() - start);
  }

  measurements.sort((a, b) => a - b);

  return {
    p50: measurements[Math.floor(measurements.length * 0.5)],
    p95: measurements[Math.floor(measurements.length * 0.95)],
    p99: measurements[Math.floor(measurements.length * 0.99)],
    mean: measurements.reduce((a, b) => a + b) / measurements.length,
    min: measurements[0],
    max: measurements[measurements.length - 1]
  };
}
```

## Target Performance

| Feature | Target Latency | Target Throughput |
|---------|---------------|-------------------|
| Knowledge Hooks | < 10ms | 10,000 ops/s |
| SPARQL Queries | < 50ms | 1,000 queries/s |
| Real-time Updates | < 50ms | 5,000 events/s |
| Validation | < 20ms | 2,000 validations/s |

See specific chapters for feature-specific benchmarks.
