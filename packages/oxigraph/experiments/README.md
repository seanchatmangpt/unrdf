# Oxigraph Experiments & Benchmarks

This directory contains evidence of performance benchmarks and experimental results for the `@unrdf/oxigraph` package.

## Benchmark Results

- [benchmark-results.txt](./benchmark-results.txt): Raw output from the `production-benchmark.mjs` suite.
- [BENCHMARK-SUMMARY.md](./BENCHMARK-SUMMARY.md): Detailed summary of performance vs Comunica.
- [COMPARISON-REPORT.md](./COMPARISON-REPORT.md): Comprehensive comparison with the current UNRDF engine.

### Performance Summary (2025-12-26)

| Metric | Result |
|--------|--------|
| Add Triple Throughput | ~50,000 triples/sec |
| SELECT Query Latency | ~8ms |
| ASK Query Latency | ~1ms |
| CONSTRUCT Query Latency | ~1ms |
| Pattern Matching Latency | <1ms |

## Reproducing Results

To reproduce these results, run the following command from the package root:

```bash
node examples/production-benchmark.mjs > experiments/benchmark-results-$(date +%Y%m%d).txt
```

## Environment

- Node.js: 18+
- Oxigraph WASM: 0.3.x
- OS: macOS/Linux
