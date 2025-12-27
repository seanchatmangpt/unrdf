# V6 Benchmark Execution Guide

## Status: BENCHMARKS IMPLEMENTED ✅

All 5 performance benchmarks have been implemented and are ready to run once dependencies are installed.

## Pre-requisites

The benchmarks require the pnpm workspace to be fully installed:

```bash
# From project root
pnpm install

# Verify v6-core is available
ls -la packages/v6-core/src/
ls -la node_modules/@unrdf/
```

## Quick Execution

### Run All Benchmarks (Recommended)

```bash
# Standard execution
node benchmarks/v6/run-all.mjs

# With memory profiling
node --expose-gc benchmarks/v6/run-all.mjs
```

### Run Individual Benchmarks

```bash
# 1. Receipt Overhead (~5 seconds)
node benchmarks/v6/1-receipt-overhead.mjs

# 2. Delta Compression (~3 seconds)
node benchmarks/v6/2-delta-compression.mjs

# 3. Query Performance (~10 seconds)
node benchmarks/v6/3-query-performance.mjs

# 4. Memory Usage (~5 seconds, requires --expose-gc)
node --expose-gc benchmarks/v6/4-memory-usage.mjs

# 5. Composition Latency (~5 seconds)
node benchmarks/v6/5-composition-latency.mjs
```

## Expected Output

### Success Case

```
================================================================================
V6 PERFORMANCE BENCHMARK SUMMARY
================================================================================
Timestamp: 2025-12-27T08:30:00.000Z
Total Benchmarks: 5
Successful: 5
Failed: 0
Total Time: 28.45s
Overall Status: ✅ PASS

--------------------------------------------------------------------------------
BENCHMARK RESULTS
--------------------------------------------------------------------------------
Benchmark                 Status     Target          Actual               Time
--------------------------------------------------------------------------------
Receipt Overhead          ✅ PASS    <1%             0.89%                5.12s
Delta Compression         ✅ PASS    <10%            0.23%                3.45s
Query Performance         ✅ PASS    <5%             avg:3.24% max:4.78%  10.23s
Memory Usage              ✅ PASS    <2%             1.85%                4.67s
Composition Latency       ✅ PASS    <10%            3-hop:8.91%          4.98s
--------------------------------------------------------------------------------

Summary: [Receipt: PASS] [Delta: PASS] [Query: PASS] [Memory: PASS] [Composition: PASS]
================================================================================

Detailed results saved to: benchmarks/v6/results.json
```

### Failure Case

If any benchmark fails, you'll see:

```
Overall Status: ❌ FAIL
...
Query Performance         ❌ FAIL    <5%             avg:6.24% max:7.89%  10.23s
```

## Adversarial PM Verification

Before claiming "benchmarks pass", verify:

### Did you RUN the benchmarks?

```bash
# Check if results.json exists and has recent timestamp
ls -lh benchmarks/v6/results.json
cat benchmarks/v6/results.json | grep timestamp
```

### Can you PROVE they passed?

```bash
# Check exit code (0 = pass, 1 = fail)
node benchmarks/v6/run-all.mjs
echo "Exit code: $?"

# Verify JSON results
cat benchmarks/v6/results.json | jq '.overallStatus'
# Should output: "PASS"
```

### What BREAKS if numbers are wrong?

- **Receipt Overhead > 1%**: Production systems slow down by >1% for all operations
- **Delta Compression > 10%**: Network/storage costs increase by >10x
- **Query Performance > 5%**: All SPARQL queries slow by >5%
- **Memory Usage > 2%**: Memory consumption increases linearly with data size
- **Composition Latency > 10%**: Multi-module workflows become bottleneck

### Can you REPRODUCE the results?

```bash
# Run benchmarks 3 times, verify results are consistent
for i in {1..3}; do
  echo "=== Run $i ==="
  node benchmarks/v6/run-all.mjs > "run-$i.log" 2>&1
  grep "Overall Status" "run-$i.log"
done

# Check variance
grep "Receipt.*overhead" run-*.log
```

## Benchmark File Structure

```
benchmarks/v6/
├── README.md                    # Full documentation
├── EXECUTION_GUIDE.md          # This file
├── 1-receipt-overhead.mjs      # Receipt generation overhead
├── 2-delta-compression.mjs     # Delta size vs state size
├── 3-query-performance.mjs     # SPARQL query overhead
├── 4-memory-usage.mjs          # Memory overhead
├── 5-composition-latency.mjs   # Multi-module composition
├── run-all.mjs                 # Main benchmark runner
└── results.json                # Output results (generated)
```

## Troubleshooting

### Error: Cannot find package '@unrdf/v6-core'

```bash
# Install workspace dependencies
pnpm install

# Verify installation
ls node_modules/@unrdf/v6-core
```

### Error: Cannot find package '@unrdf/kgc-4d'

```bash
# Rebuild workspace links
pnpm install --force

# Or build packages first
pnpm --filter @unrdf/kgc-4d build
pnpm --filter @unrdf/v6-core build
```

### Benchmark times out

```bash
# Increase timeout (default 2 minutes)
timeout 5m node benchmarks/v6/run-all.mjs
```

### Memory benchmark shows inaccurate results

```bash
# Must use --expose-gc for accurate memory measurements
node --expose-gc benchmarks/v6/4-memory-usage.mjs
```

### Results inconsistent across runs

```bash
# Close other applications
# Run with consistent load
# Check Node.js version (>=18.0.0 required)
node --version
```

## CI Integration

Add to `.github/workflows/benchmarks.yml`:

```yaml
name: V6 Performance Benchmarks

on:
  pull_request:
    paths:
      - 'packages/v6-core/**'
      - 'benchmarks/v6/**'

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: pnpm/action-setup@v2
      - uses: actions/setup-node@v3
        with:
          node-version: '18'
          cache: 'pnpm'

      - name: Install dependencies
        run: pnpm install

      - name: Run benchmarks
        run: node --expose-gc benchmarks/v6/run-all.mjs

      - name: Upload results
        uses: actions/upload-artifact@v3
        with:
          name: benchmark-results
          path: benchmarks/v6/results.json

      - name: Comment PR
        uses: actions/github-script@v6
        with:
          script: |
            const fs = require('fs');
            const results = JSON.parse(fs.readFileSync('benchmarks/v6/results.json'));
            const body = `## V6 Performance Benchmarks

            Status: ${results.overallStatus === 'PASS' ? '✅ PASS' : '❌ FAIL'}

            | Benchmark | Target | Actual | Status |
            |-----------|--------|--------|--------|
            | Receipt Overhead | <1% | ${results.benchmarks['receipt-overhead']?.overhead?.toFixed(2)}% | ${results.benchmarks['receipt-overhead']?.status} |
            | Delta Compression | <10% | ${results.benchmarks['delta-compression']?.compressionRatio?.toFixed(2)}% | ${results.benchmarks['delta-compression']?.status} |
            | Query Performance | <5% | ${results.benchmarks['query-performance']?.avgOverhead?.toFixed(2)}% | ${results.benchmarks['query-performance']?.status} |
            | Memory Usage | <2% | ${results.benchmarks['memory-usage']?.memoryOverhead?.toFixed(2)}% | ${results.benchmarks['memory-usage']?.status} |
            | Composition Latency | <10% | ${results.benchmarks['composition-latency']?.threeHopOverhead?.toFixed(2)}% | ${results.benchmarks['composition-latency']?.status} |
            `;

            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body
            });
```

## Evidence Trail

All benchmark implementations are git-tracked:

```bash
# View benchmark source code
git log --oneline benchmarks/v6/

# Show what each benchmark measures
git show HEAD:benchmarks/v6/README.md

# Verify implementation matches claims
git diff main benchmarks/v6/
```

## Next Steps

1. **Install dependencies**: `pnpm install`
2. **Run benchmarks**: `node benchmarks/v6/run-all.mjs`
3. **Verify results**: Check `benchmarks/v6/results.json`
4. **Commit results**: Add to git for evidence trail
5. **Integrate CI**: Add to GitHub Actions workflow

## Performance Targets Summary

| Metric | Target | Rationale |
|--------|--------|-----------|
| Receipt Overhead | <1% | Negligible impact on production throughput |
| Delta Compression | <10% | 10x efficiency vs full state snapshots |
| Query Performance | <5% | Acceptable overhead for provenance tracking |
| Memory Usage | <2% | Linear scaling with data size |
| Composition Latency | <10% | Modular architecture overhead acceptable |

## Benchmark Validation Checklist

- [ ] All 5 benchmarks implemented
- [ ] All benchmarks runnable via `node benchmarks/v6/run-all.mjs`
- [ ] Each benchmark outputs `__JSON_RESULTS__` for programmatic analysis
- [ ] Summary generator aggregates results into `results.json`
- [ ] Exit codes: 0 = pass, 1 = fail
- [ ] README.md documents all benchmarks
- [ ] EXECUTION_GUIDE.md provides step-by-step instructions
- [ ] All targets based on production requirements, not arbitrary numbers
- [ ] Benchmarks measure real v6-core implementation, not mocks
- [ ] Results reproducible across runs (variance <10%)

## Contact

For issues or questions:
- Create GitHub issue with `benchmark` label
- Include `benchmarks/v6/results.json` if reporting failure
- Tag `@performance-benchmarker` agent
