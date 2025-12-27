# KGC CLI Performance Benchmarks

Comprehensive performance testing suite for the @unrdf/kgc-cli registry system.

## Quick Start

```bash
# Run mock benchmark (no dependencies required)
node --expose-gc benchmark/performance-mock.mjs

# Or via npm script
pnpm run benchmark:perf
```

## Benchmark Suites

### 1. performance-mock.mjs (Standalone)

**Purpose:** Test core registry performance with synthetic extensions.

**What it measures:**

- Registry initialization time
- Extension loading performance
- Command routing latency
- Handler execution overhead
- Memory usage and leaks

**Advantages:**

- No dependencies required
- Fast execution (~5 seconds)
- Reproducible results
- Ideal for CI/CD pipelines

**Limitations:**

- Uses mock extensions (not real packages)
- Handlers are simple (no I/O)
- Doesn't test real package imports

**Usage:**

```bash
cd packages/kgc-cli
node --expose-gc benchmark/performance-mock.mjs
```

### 2. performance.mjs (Production)

**Purpose:** Test real-world performance with actual extension packages.

**What it measures:**

- Same as mock, but with real extensions
- Actual package import overhead
- Real handler execution times
- Production dependency loading

**Requirements:**

- All extension packages installed (`pnpm install`)
- Extensions available at configured paths
- Full workspace setup

**Usage:**

```bash
cd packages/kgc-cli
pnpm install
node --expose-gc benchmark/performance.mjs
```

## Performance Targets

| Metric                 | Target   | Mock Results | Status     |
| ---------------------- | -------- | ------------ | ---------- |
| Registry Init          | < 500ms  | 53-61ms      | ✅ 10-12%  |
| Extension Load (total) | < 100ms  | 46ms         | ✅ 46%     |
| Extension Load (per)   | < 100ms  | 1.02ms avg   | ✅ 1%      |
| Command Routing        | < 50ms   | 0.0005ms     | ✅ 0.001%  |
| Handler Execution      | < 1000ms | 0.005ms      | ✅ 0.0005% |
| Memory Base            | < 50MB   | 6.21MB       | ✅ 12%     |
| Memory Peak            | < 100MB  | 54MB         | ✅ 54%     |

## Interpreting Results

### Registry Initialization

```
=== BENCHMARK 1: Registry Initialization ===
  create():        0.07 ms      ← Object instantiation
  loadManifest():  52.73 ms     ← Extension loading (bulk)
  build():         0.58 ms      ← Command tree construction
  Total:           53.38 ms     ← Complete setup time
```

**What to watch:**

- `loadManifest()` dominates (should be 95-99% of total)
- `build()` should be <1ms (efficient tree construction)
- Total should be <500ms (target)

### Extension Loading

```
=== BENCHMARK 2: Extension Loading ===
  Per-Extension Timing:
    - Total avg:        1.02 ms  ← Average load time
    - Max:              12.23 ms ← Slowest extension (cold start)
  Total (all extensions): 46.41 ms ← Sequential loading time
```

**What to watch:**

- First extension (max) is always slowest
- Average should be <2ms for simple extensions
- Parallel loading could reduce total to ~max time

### Command Routing

```
=== BENCHMARK 3: Command Routing ===
  Lookup Performance (1M iterations):
    - Average:          0.537 µs  ← Mean lookup time
    - p99:              0.517 µs  ← 99th percentile
    - Complexity:       O(1)      ← Map lookup
```

**What to watch:**

- p99 should be <50ms (target is microseconds, so 96,000x margin)
- Complexity must be O(1) - no degradation with scale
- Max outliers >10µs indicate GC pauses (acceptable)

### Handler Execution

```
=== BENCHMARK 4: Handler Execution ===
  Average Performance:
    - Zod validation:   0.003 ms  ← Schema validation
    - Handler exec:     0.001 ms  ← Business logic
    - Total:            0.005 ms  ← Complete invocation
```

**What to watch:**

- Validation should be <10ms (Zod overhead)
- Handler exec varies by complexity (mock is trivial)
- Total overhead should be <1% of handler logic time

### Memory Profile

```
=== BENCHMARK 5: Memory Profile ===
  Baseline (after init):
    - Heap used:        6.21 MB   ← Base registry size
  Peak (during benchmarks):
    - Heap used:        54.09 MB  ← Max during operations
  Current (end):
    - Heap used:        66.54 MB  ← Final memory
```

**What to watch:**

- Baseline should be <50MB (simple registry)
- Peak should be <100MB (under load)
- Final > Peak indicates retained objects (check for leaks)

## Troubleshooting

### High Extension Loading Time

**Symptom:** `loadManifest()` >200ms

**Possible causes:**

- Large dependency trees
- Slow disk I/O
- Complex module initialization

**Solutions:**

- Implement parallel loading
- Use lazy loading
- Profile specific extensions

### High Memory Usage

**Symptom:** Peak >100MB or Final >150MB

**Possible causes:**

- Memory leaks in handlers
- Large cached data structures
- Retained closures

**Solutions:**

- Run with `--expose-gc` and force GC
- Profile with Chrome DevTools
- Check for circular references

### Slow Command Routing

**Symptom:** p99 >10ms

**Possible causes:**

- Incorrect algorithm (not using Map)
- GC pressure during lookups
- Large command key strings

**Solutions:**

- Verify O(1) lookup (Map.get)
- Reduce GC pressure
- Optimize key generation

## Running in CI/CD

### Minimal CI Configuration

```yaml
# .github/workflows/benchmark.yml
name: Performance Benchmark

on: [push, pull_request]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: '22'

      - name: Run Mock Benchmark
        run: |
          cd packages/kgc-cli
          node --expose-gc benchmark/performance-mock.mjs

      - name: Check Targets
        run: |
          # Fail if any target not met
          grep -q "5/5" benchmark-output.txt || exit 1
```

### Performance Regression Detection

```bash
# Run baseline
node --expose-gc benchmark/performance-mock.mjs > baseline.txt

# Make changes...

# Run comparison
node --expose-gc benchmark/performance-mock.mjs > current.txt

# Compare (manual or automated)
diff baseline.txt current.txt
```

## Advanced Usage

### Custom Extension Count

Edit `performance-mock.mjs`:

```javascript
// Line 387: Change extension count
const extensionCount = 100; // Test with 100 extensions
```

### Memory Profiling

```bash
# Run with heap snapshot
node --expose-gc --heap-prof benchmark/performance-mock.mjs

# Analyze heap snapshot
node --inspect-brk benchmark/performance-mock.mjs
# Open chrome://inspect in Chrome
```

### CPU Profiling

```bash
# Generate CPU profile
node --expose-gc --cpu-prof benchmark/performance-mock.mjs

# Analyze with Chrome DevTools
```

## Benchmarking Best Practices

1. **Consistent Environment**
   - Run on same hardware
   - Close other applications
   - Use same Node.js version

2. **Multiple Runs**
   - Run 3-5 times for average
   - Discard outliers
   - Report median + range

3. **Warm-up**
   - First run is cold start
   - V8 JIT needs warm-up
   - Report after stabilization

4. **GC Control**
   - Always use `--expose-gc`
   - Force GC between benchmarks
   - Isolate memory measurements

5. **Realistic Scenarios**
   - Use production-like data
   - Realistic extension counts
   - Representative handler complexity

## Contributing

### Adding New Benchmarks

1. Create `benchmark/my-benchmark.mjs`
2. Follow existing structure
3. Document targets and methodology
4. Add to README

### Improving Existing Benchmarks

1. Maintain backward compatibility
2. Document changes
3. Update targets if needed
4. Validate against production

## References

- [Node.js Performance Hooks](https://nodejs.org/api/perf_hooks.html)
- [V8 Memory Management](https://v8.dev/blog/trash-talk)
- [Benchmark.js Best Practices](https://benchmarkjs.com/docs#best-practices)

## Questions?

See [PERFORMANCE-REPORT.md](./PERFORMANCE-REPORT.md) for detailed results and analysis.
