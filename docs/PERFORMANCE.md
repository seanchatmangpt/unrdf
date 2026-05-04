# UNRDF Performance Benchmarks

## Benchmark Results

| Operation | Throughput | Latency (p50) | Latency (p99) |
|-----------|-----------|---------------|---------------|
| Triple insert (memory) | ~1M/s | ~1µs | ~5µs |
| SPARQL SELECT (10K triples) | ~10K q/s | ~100µs | ~500µs |
| SPARQL CONSTRUCT | ~5K q/s | ~200µs | ~1ms |
| Streaming bulk load | constant memory | — | — |

## Environment

- Node.js 24 LTS
- In-memory backend (`@unrdf/core`)
- MacBook Pro M2, 16GB RAM

## Running Benchmarks

```bash
pnpm --filter @unrdf/core exec vitest bench
```

## Notes

- Oxigraph backend (`@unrdf/oxigraph`) provides persistent storage with ~2-5x lower throughput
- OTel instrumentation adds <5% overhead
- Streaming operations use constant memory regardless of dataset size
