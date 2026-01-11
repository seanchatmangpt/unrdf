# YAWL Daemon Performance Benchmarks

Comprehensive performance benchmarking suite for YAWL daemon integration.

## Overview

This benchmark suite measures the performance of the YAWL daemon across five critical areas:

1. **Daemon Startup** - Initialization and ready state latency
2. **IPC Throughput** - Inter-process communication performance
3. **Workflow Execution** - Single workflow execution latency
4. **Concurrent Workflows** - Parallel execution and scalability
5. **Memory Footprint** - Memory usage and leak detection

## Performance Targets

| Metric | Target | Rationale |
|--------|--------|-----------|
| Daemon startup (P95) | <500ms | Fast cold starts |
| IPC round-trip (P95) | <10ms | Low-latency messaging |
| Message throughput | >1000 msg/s | High-volume workloads |
| Simple workflow (P95) | <100ms | Responsive execution |
| Memory baseline | <50MB | Efficient resource usage |

## Running Benchmarks

### Run all benchmarks
```bash
node benchmarks/yawl-daemon/runner.mjs
```

### Run specific suite
```bash
# Startup benchmarks only
node benchmarks/yawl-daemon/runner.mjs --suite=startup

# IPC benchmarks only
node benchmarks/yawl-daemon/runner.mjs --suite=ipc

# Workflow execution benchmarks
node benchmarks/yawl-daemon/runner.mjs --suite=workflow

# Concurrent execution benchmarks
node benchmarks/yawl-daemon/runner.mjs --suite=concurrent

# Memory benchmarks
node benchmarks/yawl-daemon/runner.mjs --suite=memory
```

### Options

- `--verbose` or `-v` - Verbose output with detailed metrics
- `--save-baseline` - Save results as new baseline for regression detection
- `--quick` - Run with reduced iterations (faster)
- `--suite=<name>` - Run specific benchmark suite

### Memory profiling

For accurate memory measurements, run with GC exposed:

```bash
node --expose-gc benchmarks/yawl-daemon/runner.mjs
```

## Benchmark Details

### 1. Daemon Startup (`daemon-startup.bench.mjs`)

Measures daemon initialization latency:

- **Cold startup** - First initialization (50 runs)
- **Warm startup** - Subsequent initializations (30 runs)
- **Startup with load** - With pre-scheduled operations (10 runs, 100 ops)

**Metrics**: P50, P95, P99 latency, memory footprint

### 2. IPC Throughput (`ipc-throughput.bench.mjs`)

Measures inter-process communication performance:

- **Round-trip latency** - Message send + receive (1000 messages, 5 runs)
- **Message throughput** - Messages per second (5 second duration, 3 runs)
- **Queue depth impact** - Latency vs queue size (depths: 10, 50, 100, 500)

**Metrics**: P50, P95, P99 latency, messages/sec, scalability factor

### 3. Workflow Execution (`workflow-execution.bench.mjs`)

Measures YAWL workflow execution performance:

- **Simple workflow** - Single task (100 workflows, 5 runs)
- **Multi-step workflow** - Sequential tasks (50 workflows, 5 steps, 3 runs)
- **Conditional workflow** - Branching logic (50 workflows, 3 branches, 3 runs)
- **Task transitions** - Transition overhead (30 workflows, 10 transitions)

**Metrics**: P50, P95, P99 execution latency

### 4. Concurrent Workflows (`concurrent-workflows.bench.mjs`)

Measures performance under concurrent load:

- **Concurrent execution** - Scalability test (levels: 1, 5, 10, 20, 50)
- **Resource contention** - Latency under load (20 concurrent, 5s duration)
- **Burst handling** - Sudden load spikes (50 workflows burst, 10 max concurrent)

**Metrics**: Throughput, latency, scalability factor, P99/P50 ratio

### 5. Memory Footprint (`memory-footprint.bench.mjs`)

Measures memory usage and leak detection:

- **Baseline memory** - Daemon footprint (5 runs)
- **Memory growth** - Under workflow load (1000 workflows, sampling every 100)
- **Memory stability** - Over time (10s duration, 50 wf/s)
- **GC pressure** - Impact of garbage collection (500 workflows)

**Metrics**: Heap usage (MB), RSS, growth rate, leak detection

## Baseline Management

### Save current results as baseline

```bash
node benchmarks/yawl-daemon/runner.mjs --save-baseline
```

This saves results to `baselines/baseline.json` for future regression detection.

### Regression Detection

When a baseline exists, benchmarks are compared automatically:

- **Regression**: >20% performance degradation vs baseline (⚠)
- **Improvement**: >20% performance improvement vs baseline (↑)

Regressions cause the benchmark suite to exit with error code 1 (CI failure).

## Output Files

- `yawl-daemon-benchmarks-<timestamp>.json` - Full benchmark results
- `baselines/baseline.json` - Saved baseline for regression detection

## Interpreting Results

### Latency Percentiles

- **P50 (median)** - Typical case performance
- **P95** - 95% of requests faster than this
- **P99** - 99% of requests faster than this

Good performance: P99/P50 ratio < 3 (low variance)

### Throughput

Messages or workflows processed per second. Higher is better.

### Memory

- **Baseline** - Memory used by daemon at rest
- **Growth rate** - Memory increase per operation (should be near zero)
- **Leak detection** - Memory not released after GC (should be minimal)

### Scalability Factor

Ratio of throughput at max concurrency vs baseline.

- Linear scaling: Factor ~= concurrency level
- Sub-linear: Factor < concurrency level (some contention)
- Super-linear: Factor > concurrency level (unlikely, check measurement)

## CI Integration

Add to `.github/workflows/performance.yml`:

```yaml
- name: Run YAWL Daemon Benchmarks
  run: node --expose-gc benchmarks/yawl-daemon/runner.mjs --verbose
```

Benchmarks will fail CI if:
- Any benchmark fails to meet target
- Performance regression detected (>20% vs baseline)

## Troubleshooting

### High memory baseline (>50MB)

- Check for resource leaks in daemon initialization
- Review event listener cleanup
- Profile with `--expose-gc` and check heap snapshots

### Low throughput (<1000 msg/s)

- Verify maxConcurrent is set appropriately
- Check for I/O bottlenecks
- Profile CPU usage during execution

### High latency variance (P99/P50 > 5)

- Investigate GC pauses (use `--expose-gc`)
- Check for blocking operations
- Review event loop lag

### Regressions

- Review recent changes to daemon or workflow engine
- Compare detailed metrics between baseline and current
- Run with `--verbose` for full diagnostics

## Future Enhancements

- [ ] Distributed benchmarks (multi-node)
- [ ] Network latency simulation
- [ ] Fault injection testing
- [ ] Long-running stability tests (hours)
- [ ] Custom workload patterns
