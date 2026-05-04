#!/bin/bash
#
# Run All AtomVM Benchmarks
#
# Executes all standalone benchmarks and displays results
#

echo "========================================"
echo "AtomVM Performance Benchmark Suite"
echo "========================================"
echo ""
echo "Running all benchmarks..."
echo ""

# Pattern Match Benchmark
echo "1/3: Pattern Match Benchmark"
echo "----------------------------"
node benchmarks/pattern-match-benchmark-standalone.mjs
echo ""

# Serialization Benchmark
echo "2/3: Serialization Benchmark"
echo "----------------------------"
node benchmarks/serialization-benchmark-standalone.mjs
echo ""

# Batch Throughput Benchmark
echo "3/3: Batch Throughput Benchmark"
echo "--------------------------------"
node benchmarks/batch-throughput-benchmark-standalone.mjs
echo ""

echo "========================================"
echo "All benchmarks completed!"
echo "========================================"
echo ""
echo "See benchmarks/RESULTS.md for detailed analysis"
echo ""
