# How To: Handle Backpressure

Prevent memory overruns and stalled streams when your downstream consumer cannot keep up with the rate of incoming data.

---

## Understand When Backpressure Matters

Backpressure occurs when the producer (file read or network socket) emits data faster than the consumer (quad processor, database writer) can handle it. Without flow control, Node.js buffers the excess internally until you run out of heap or the process crashes.

Two places where backpressure can occur in `@unrdf/streaming`:

1. **`RDFStreamParser`** — the Transform stream's internal buffer fills if you do not drain it
2. **`ChangeFeeed` + `StreamProcessor`** — a slow subscriber accumulates pending events in the processor's internal batch buffer

---

## Configure the RDFStreamParser for Backpressure

The parser has backpressure enabled by default (`enableBackpressure: true`). Tune `highWaterMark` and `chunkSize` to match your consumer's throughput:

```javascript
import { createReadStream } from 'fs';
import { createRDFStreamParser } from '@unrdf/streaming';

const parser = createRDFStreamParser({
  format: 'n-triples',
  highWaterMark: 8192, // input buffer: 8 KB before pausing the source
  chunkSize: 200, // quads per emitted chunk — smaller = more responsive consumer
  enableBackpressure: true,
});

parser.on('data', async chunk => {
  if (chunk.type === 'quads') {
    // If your handler is async, pause the stream while you process
    parser.pause();
    await writeToDatabase(chunk.data); // slow operation
    parser.resume();
  }
});

await new Promise((resolve, reject) => {
  createReadStream('large.nt').pipe(parser);
  parser.on('end', resolve);
  parser.on('error', reject);
});
```

**Key parameters:**

| Parameter            | Default         | Effect                                                         |
| -------------------- | --------------- | -------------------------------------------------------------- |
| `highWaterMark`      | `16384` (bytes) | Input buffer size before the readable source is paused         |
| `chunkSize`          | `1000` (quads)  | Number of quads buffered before emitting a `'quads'` chunk     |
| `enableBackpressure` | `true`          | Whether the parser emits `'progress'` signals for flow control |

---

## Detect Backpressure Events

Check `parser.getMetrics().backpressureEvents` after parsing to see how often the buffer was full:

```javascript
import { createReadStream } from 'fs';
import { createRDFStreamParser } from '@unrdf/streaming';

const parser = createRDFStreamParser({
  format: 'n-triples',
  enableBackpressure: true,
  chunkSize: 500,
});

await new Promise((resolve, reject) => {
  parser.resume();
  createReadStream('large.nt').pipe(parser);
  parser.on('end', resolve);
  parser.on('error', reject);
});

const m = parser.getMetrics();
console.log('Backpressure events:', m.backpressureEvents);
console.log('Backpressure rate:', m.backpressureRate.toFixed(3), 'events/chunk');

// If backpressureRate > 0.1, consider:
//   - Reducing chunkSize
//   - Reducing highWaterMark
//   - Processing fewer quads per handler invocation
```

---

## Batch Changes in a StreamProcessor

If your subscriber cannot handle every individual event, use the `batch` operator to accumulate changes and process them in bulk:

```javascript
import { createChangeFeed, createStreamProcessor } from '@unrdf/streaming';

const feed = createChangeFeed();
const processor = createStreamProcessor(feed);

// Collect 50 changes before calling the subscriber
const batchStream = processor.batch(50);

batchStream.subscribe(async changes => {
  // Receive an array of 50 changes, not one at a time
  await bulkWrite(changes);
});
```

The batch buffer accumulates events internally. If you need to flush partial batches (e.g., on timeout), combine with `debounce`:

```javascript
// Debounce rapid bursts: if no change arrives for 200 ms, flush whatever is pending
const processor2 = createStreamProcessor(feed);
processor2.debounce(200).subscribe(change => {
  // Called at most once per 200 ms quiet window
  console.log('Debounced change:', change.type);
});
```

---

## Limit Change History to Avoid Memory Growth

By default, the feed stores up to 10 000 changes in a ring buffer. For long-running processes emitting thousands of events per second, this can grow large. Set a lower limit:

```javascript
import { createChangeFeed } from '@unrdf/streaming';

// Keep only the last 100 changes (ring buffer evicts oldest automatically)
const feed = createChangeFeed(null, { maxHistorySize: 100 });

// Or disable history entirely
const feedNoHistory = createChangeFeed(null, { maxHistorySize: 0 });

// Or allow unbounded growth (use only for short-lived feeds)
const feedUnbounded = createChangeFeed(null, { maxHistorySize: Infinity });
```

---

## Monitor Backpressure with PerformanceMonitor

Attach a `PerformanceMonitor` to count and surface backpressure events across a streaming pipeline:

```javascript
import { createRDFStreamParser, createPerformanceMonitor } from '@unrdf/streaming';
import { createReadStream } from 'fs';

const monitor = createPerformanceMonitor({
  sampleInterval: 500,
  thresholds: { throughputMin: 1000 }, // quads/sec minimum
});

const parser = createRDFStreamParser({ format: 'n-triples', chunkSize: 100 });

monitor.start();

monitor.on('threshold-violation', violation => {
  console.warn(
    `Throughput dropped below ${violation.threshold}: ${violation.value.toFixed(0)} quads/sec`
  );
});

parser.on('data', chunk => {
  if (chunk.type === 'quads') {
    chunk.data.forEach(() => monitor.recordQuad());
    monitor.recordChunk();
  }
  if (chunk.type === 'progress' && chunk.backpressure) {
    monitor.recordBackpressure();
  }
});

await new Promise((resolve, reject) => {
  createReadStream('large.nt').pipe(parser);
  parser.on('end', resolve);
  parser.on('error', reject);
});

monitor.stop();
const report = monitor.getReport();
console.log('Backpressure events:', report.backpressure.events);
console.log('Backpressure rate:  ', report.backpressure.rate.toFixed(3));
```

## See Also

- [How-To: Monitor Stream Performance](./03-monitor-stream-performance.md)
- [Explanation: Backpressure and Flow Control](../explanation/02-backpressure-and-flow-control.md)
- [Reference: Stream Protocol](../reference/stream-protocol.md)
