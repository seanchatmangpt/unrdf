# Performance Monitor API Reference

Complete API for `PerformanceMonitor` exported from `@unrdf/streaming`.

---

## createPerformanceMonitor(config?)

```typescript
createPerformanceMonitor(config?: PerformanceMonitorConfig): PerformanceMonitor
```

Factory function. Equivalent to `new PerformanceMonitor(config)`.

```javascript
import { createPerformanceMonitor } from '@unrdf/streaming';

const monitor = createPerformanceMonitor({
  sampleInterval: 1000,
  windowSize: 60,
  thresholds: { throughputMin: 5000 },
});
```

---

## PerformanceMonitorConfig

All fields are optional.

| Field                      | Type           | Default | Description                                                 |
| -------------------------- | -------------- | ------- | ----------------------------------------------------------- |
| `sampleInterval`           | number (ms)    | `1000`  | How often to collect a metric sample                        |
| `windowSize`               | number         | `60`    | Maximum samples to keep per metric array                    |
| `enableMemoryTracking`     | boolean        | `true`  | Collect `process.memoryUsage()` each sample                 |
| `enableLatencyTracking`    | boolean        | `true`  | Record per-quad latency from `recordQuad(latency)`          |
| `enableThroughputTracking` | boolean        | `true`  | Calculate quads/sec each sample                             |
| `thresholds.throughputMin` | number         | —       | Emit `'threshold-violation'` if throughput drops below this |
| `thresholds.memoryMax`     | number (bytes) | —       | Emit `'threshold-violation'` if `heapUsed` exceeds this     |

---

## PerformanceMonitor Class

`PerformanceMonitor` extends Node.js `EventEmitter`.

### constructor(config?)

```typescript
new PerformanceMonitor(config?: PerformanceMonitorConfig)
```

Initializes metric arrays and state. Does not start collecting until `start()` is called.

---

### start()

```typescript
start(): void
```

Begin metric collection. Sets `isMonitoring = true`, records `startTime`, and starts the internal `setInterval` at `sampleInterval`. Emits `'started'`.

If already monitoring, does nothing.

```javascript
monitor.start();
```

---

### stop()

```typescript
stop(): void
```

Stop metric collection. Clears the interval, collects a final sample, sets `isMonitoring = false`. Emits `'stopped'` with the full `getReport()` result as the event argument.

```javascript
monitor.stop();
monitor.on('stopped', report => console.log(report.summary));
```

---

### recordQuad(latency?)

```typescript
recordQuad(latency?: number): void
```

Increment the quad counter by 1. If `latency` is provided and `enableLatencyTracking` is `true`, the value (in ms) is appended to the latency window.

```javascript
const start = performance.now();
processQuad(quad);
monitor.recordQuad(performance.now() - start);
```

---

### recordBytes(bytes)

```typescript
recordBytes(bytes: number): void
```

Add `bytes` to `state.bytesProcessed`.

---

### recordChunk()

```typescript
recordChunk(): void
```

Increment `state.chunksProcessed` by 1. Used to calculate `backpressure.rate` in the report.

---

### recordError(error)

```typescript
recordError(error: Error): void
```

Increment `state.errorsCount`, append the error to the errors window, and emit `'error-recorded'` with the error as the event argument.

---

### recordBackpressure()

```typescript
recordBackpressure(): void
```

Increment `state.backpressureEvents`, append a backpressure sample, and emit `'backpressure'`.

---

### getCurrentMetrics()

```typescript
getCurrentMetrics(): CurrentMetrics
```

Point-in-time snapshot of counters. Safe to call while `isMonitoring` is `true`.

```typescript
{
  quadsProcessed: number; // total calls to recordQuad()
  bytesProcessed: number; // total bytes passed to recordBytes()
  chunksProcessed: number; // total calls to recordChunk()
  errorsCount: number; // total calls to recordError()
  backpressureEvents: number; // total calls to recordBackpressure()
  duration: number; // ms since start()
  averageThroughput: number; // quadsProcessed / duration * 1000
}
```

---

### getReport()

```typescript
getReport(): PerformanceReport
```

Full statistical report. Typically called after `stop()`.

```typescript
{
  summary: CurrentMetrics;

  throughput: {
    mean: number;
    min: number;
    max: number;
    p50: number;
    p95: number;
    p99: number;
    unit: 'quads/sec';
  }

  latency: {
    mean: number;
    min: number;
    max: number;
    p50: number;
    p95: number;
    p99: number;
    unit: 'ms';
  }

  memory: {
    rss: {
      (mean, min, max, p50, p95, p99);
    }
    heapUsed: {
      (mean, min, max, p50, p95, p99);
    }
    heapTotal: {
      (mean, min, max, p50, p95, p99);
    }
    unit: 'bytes';
  }

  backpressure: {
    events: number; // total backpressure events
    rate: number; // backpressureEvents / chunksProcessed (0 if no chunks)
  }

  errors: {
    count: number;
    recent: Array<{ timestamp: number; message: string; stack: string }>;
    // recent contains last 10 errors
  }
}
```

Statistics are calculated over the rolling window of samples (up to `windowSize`). If no samples have been collected yet (e.g., `stop()` called before the first `sampleInterval` fires), numeric statistics default to `0`.

---

### reset()

```typescript
reset(): void
```

Clear all metric arrays and counters. Resets `startTime` and `lastSample` to `null`. Does not stop or restart the monitoring interval. Emits `'reset'`.

---

## Events

| Event                   | When emitted                      | Listener argument                                      |
| ----------------------- | --------------------------------- | ------------------------------------------------------ |
| `'started'`             | `start()` called                  | none                                                   |
| `'stopped'`             | `stop()` called                   | `PerformanceReport`                                    |
| `'threshold-violation'` | A threshold is breached on sample | `{ metric: string, value: number, threshold: number }` |
| `'error-recorded'`      | `recordError()` called            | the `Error` object                                     |
| `'backpressure'`        | `recordBackpressure()` called     | none                                                   |
| `'reset'`               | `reset()` called                  | none                                                   |

---

## State Properties (read-only)

| Property       | Type    | Description                                                                    |
| -------------- | ------- | ------------------------------------------------------------------------------ |
| `isMonitoring` | boolean | Whether the monitor is currently collecting                                    |
| `config`       | object  | Validated configuration (Zod-parsed)                                           |
| `metrics`      | object  | Raw metric arrays: `throughput`, `latency`, `memory`, `backpressure`, `errors` |
| `state`        | object  | Running counters: `quadsProcessed`, `bytesProcessed`, etc.                     |
