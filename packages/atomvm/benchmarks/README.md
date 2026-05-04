# AtomVM Performance Benchmarks

## Overview

Real performance benchmarks with actual measurements to validate performance claims. All benchmarks use `performance.now()` for high-precision timing and run multiple iterations for statistical validity.

## Benchmark Methodology

### 1. Pattern Matching Benchmark
**File**: `pattern-match-benchmark.mjs`

**Purpose**: Measure RDF triple pattern matching performance

**Method**:
- Creates 1000 triples in-memory using Oxigraph store
- Executes 100 pattern matching operations with different query patterns
- Measures operations per second, P50 latency, P99 latency
- Tests both specific queries and wildcard queries

**Metrics**:
- **Throughput**: Operations per second
- **P50 Latency**: Median query time in milliseconds
- **P99 Latency**: 99th percentile query time in milliseconds
- **Total Time**: End-to-end benchmark duration

### 2. Serialization Benchmark
**File**: `serialization-benchmark.mjs`

**Purpose**: Measure triple serialization/deserialization performance

**Method**:
- Creates 1000 test triples
- Serializes triples to JSON format
- Deserializes back to triple objects
- Simulates BEAM binary format conversion
- Measures roundtrip time and throughput

**Metrics**:
- **JSON Serialization**: Throughput in triples/second
- **JSON Deserialization**: Throughput in triples/second
- **Roundtrip Time**: Total time for serialize + deserialize
- **Format Comparison**: JSON vs simulated BEAM format

### 3. Batch Throughput Benchmark
**File**: `batch-throughput-benchmark.mjs`

**Purpose**: Measure batching performance and find optimal batch size

**Method**:
- Uses TripleStreamBatcher to process triples
- Tests multiple batch sizes: 10, 50, 100, 200, 500, 1000
- Measures triples/second throughput for each batch size
- Identifies optimal batch size for maximum throughput
- Tests backpressure handling

**Metrics**:
- **Throughput by Batch Size**: Triples/second for each batch size
- **Optimal Batch Size**: Batch size with highest throughput
- **Average Batch Latency**: Mean time to process a batch
- **Backpressure Events**: Number of backpressure activations

## Running Benchmarks

### Run All Benchmarks
```bash
node benchmarks/pattern-match-benchmark.mjs
node benchmarks/serialization-benchmark.mjs
node benchmarks/batch-throughput-benchmark.mjs
```

### Individual Benchmarks
```bash
# Pattern matching only
node benchmarks/pattern-match-benchmark.mjs

# Serialization only
node benchmarks/serialization-benchmark.mjs

# Batch throughput only
node benchmarks/batch-throughput-benchmark.mjs
```

## Expected Performance Targets

Based on documented claims, benchmarks should validate:

- **Pattern Matching**: >= 10,000 ops/sec for simple queries
- **Batch Throughput**: >= 10,000 triples/sec (per TripleStreamBatcher docs)
- **Serialization**: >= 5,000 roundtrips/sec
- **Latency**: P99 < 10ms for pattern matching

## Output Format

All benchmarks output structured results in this format:

```
=== Benchmark Name ===
- Metric 1: <value> <unit>
- Metric 2: <value> <unit>
...
- P50 Latency: <value>ms
- P99 Latency: <value>ms
- Throughput: <value> ops/sec
=====================
```

## Statistical Validity

- **Warmup**: Each benchmark runs warmup iterations (not measured)
- **Sample Size**: Minimum 100 operations per benchmark
- **Percentiles**: Calculated from sorted latency arrays
- **Averaging**: Mean calculated from all valid measurements

## Poka-Yoke (Error Prevention)

- All benchmarks validate dependencies before running
- Failed operations are counted and reported separately
- Out-of-memory scenarios are detected and handled
- Benchmarks clean up resources after completion

## Reproducibility

To reproduce benchmark results:

1. Ensure Node.js 20+ is installed
2. Run `pnpm install` in the atomvm package directory
3. Execute benchmarks in a quiet environment (no other heavy processes)
4. Results may vary by ±10% due to system load and GC timing

## Adversarial PM Compliance

These benchmarks follow Adversarial PM principles:

- **Did you RUN it?**: Yes - every benchmark must be executed to completion
- **Can you PROVE it?**: Yes - actual numbers with timestamps
- **What BREAKS if you're wrong?**: Performance claims become unverified assertions
- **What's the EVIDENCE?**: Console output with measurable metrics

## Extending Benchmarks

To add new benchmarks:

1. Create `benchmarks/new-benchmark.mjs`
2. Use `performance.now()` for timing
3. Run minimum 100 iterations
4. Calculate P50, P99, mean, throughput
5. Output structured results
6. Update this README with methodology
