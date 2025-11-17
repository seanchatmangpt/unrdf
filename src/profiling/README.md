# UNRDF Performance Profiling

Comprehensive performance profiling tools for UNRDF v3.1.0 with OpenTelemetry integration.

## Features

- ⚡ **Latency Profiling**: High-resolution timing with percentile calculations (p50, p75, p90, p95, p99, p999)
- 💾 **Memory Profiling**: Heap tracking, leak detection, and trend analysis
- 🔥 **CPU Profiling**: V8 Inspector integration with hot function analysis (Node.js)
- 📊 **Rich Reporting**: JSON, HTML, and terminal output formats
- 🎯 **Performance Budgets**: Automatic budget enforcement and violation detection
- 📈 **Regression Detection**: Compare profiles to detect performance regressions
- 🔍 **OTEL Integration**: Automatic metric export to OpenTelemetry

## Quick Start

```javascript
import { createProfiler } from './profiling/profiler.mjs';

const profiler = createProfiler();

const { result, profile } = await profiler.profile('my-operation', async () => {
  // Your code here
  return await performWork();
});

console.log('Duration:', profile.latency.duration, 'ms');
console.log('p95:', profile.latency.p95, 'ms');
console.log('Memory delta:', profile.memory.heapUsedDelta, 'bytes');
```

## Modules

### Main Profiler (`profiler.mjs`)

Orchestrates all profiling activities and manages OTEL integration.

```javascript
const profiler = createProfiler({
  enableLatency: true,
  enableMemory: true,
  enableCpu: false,
  enableOtel: true,
  labels: ['production', 'api-v1']
});

await profiler.profile('operation', async () => work());
```

### Latency Profiler (`latency-profiler.mjs`)

High-resolution latency measurements with statistical analysis.

```javascript
import { LatencyProfiler } from './latency-profiler.mjs';

const profiler = new LatencyProfiler();
const sessionId = profiler.start('operation');

// ... do work ...

const metrics = profiler.stop(sessionId);
console.log('p95 latency:', metrics.p95, 'ms');
```

### Memory Profiler (`memory-profiler.mjs`)

Track memory usage and detect leaks.

```javascript
import { MemoryProfiler } from './memory-profiler.mjs';

const profiler = new MemoryProfiler();
const { metrics } = await profiler.profile('operation', async () => work());

if (metrics.leakDetected) {
  console.warn('Memory leak detected!');
}
```

### CPU Profiler (`cpu-profiler.mjs`)

Profile CPU usage and identify hot functions (Node.js only).

```javascript
import { CpuProfiler } from './cpu-profiler.mjs';

if (CpuProfiler.isAvailable()) {
  const profiler = new CpuProfiler({ saveProfiles: true });
  const { metrics } = await profiler.profile('operation', async () => work());

  console.log('Hot functions:', metrics.hotFunctions);
}
```

### Reporter (`reporter.mjs`)

Generate reports and compare profiles.

```javascript
import { Reporter } from './reporter.mjs';

// Terminal output
console.log(Reporter.toTerminal(profile));

// Save as HTML
Reporter.save(profile, 'report.html', { format: 'html' });

// Compare profiles
const comparison = Reporter.compare(baseline, current);
if (comparison.regression) {
  console.error('Performance regression detected!');
}
```

## Performance Targets

UNRDF v3.1.0 performance targets (p95 latency):

| Operation | Target |
|-----------|--------|
| Parse operations | < 50ms |
| Query operations | < 100ms |
| Hook execution | < 80ms |
| Transaction commit | < 120ms |

## Usage Examples

See `/examples/profiling-example.mjs` for comprehensive examples:

```bash
node examples/profiling-example.mjs
```

## Testing

Run the test suite:

```bash
npm test test/profiling/profiler.test.mjs
```

## Documentation

See `/docs/profiling-guide.md` for complete documentation.

## OTEL Integration

The profiler automatically exports metrics to OpenTelemetry:

- `profiler.operation.latency` - Latency histogram
- `profiler.memory.heap_used` - Memory gauge
- `profiler.operations.total` - Operation counter
- `profiler.operations.errors` - Error counter

## Performance Overhead

- Latency profiling: ~0.1-0.5% overhead
- Memory profiling: ~1-2% overhead
- CPU profiling: ~5-10% overhead

## License

Same as UNRDF project license.
