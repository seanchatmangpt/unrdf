# Tutorial: Stream a Large RDF Graph

In this tutorial you will use `RDFStreamParser` to parse a large RDF file as a Node.js stream, receive quads in chunks, track progress, and collect a final metrics report. By the end you will understand chunked streaming and be ready to wire the parser into a change feed pipeline.

**Time**: 20–30 minutes
**Prerequisite**: `pnpm add @unrdf/streaming`

---

## Part 1: Parse a Turtle File

Create `parse-turtle.mjs`:

```javascript
import { createReadStream } from 'fs';
import { createRDFStreamParser } from '@unrdf/streaming';

// createRDFStreamParser returns a Node.js Transform stream in object mode
const parser = createRDFStreamParser({ format: 'turtle' });

let totalQuads = 0;

parser.on('data', chunk => {
  if (chunk.type === 'quads') {
    totalQuads += chunk.count;
    console.log(`Received chunk: ${chunk.count} quads (total so far: ${totalQuads})`);
  }

  if (chunk.type === 'complete') {
    console.log('Parsing complete. Final metrics:', chunk.metrics);
  }
});

parser.on('error', err => {
  console.error('Parse error:', err.message);
});

// Pipe a file into the parser
createReadStream('data.ttl').pipe(parser);
```

The parser emits two event types on `'data'`:

| `chunk.type` | Contains                                                        |
| ------------ | --------------------------------------------------------------- |
| `'quads'`    | `chunk.data` (array of quads), `chunk.count`, `chunk.timestamp` |
| `'progress'` | Backpressure signal (emitted per input chunk)                   |
| `'complete'` | `chunk.metrics` — final stats after the last flush              |

---

## Part 2: Collect All Quads with parseRDFStream

When you want all quads at once rather than processing chunks incrementally, use the `parseRDFStream` helper:

```javascript
import { createReadStream } from 'fs';
import { parseRDFStream } from '@unrdf/streaming';

// Resolves with an array of all parsed quads
const quads = await parseRDFStream(createReadStream('data.ttl'), {
  format: 'turtle',
  chunkSize: 1000, // quads per internal chunk
});

console.log(`Parsed ${quads.length} quads total`);
console.log('First quad:', quads[0].subject.value);
```

`parseRDFStream` wraps the streaming machinery and returns a `Promise<Quad[]>`. Use it when the entire dataset fits comfortably in memory. For datasets larger than available heap, use the chunk-by-chunk approach from Part 1.

---

## Part 3: Parse N-Triples with Progress Callbacks

N-Triples is faster to parse than Turtle because it has no prefixes. For large datasets (100 000+ quads), use `'n-triples'` format and track progress with the `onProgress` callback:

```javascript
import { createReadStream } from 'fs';
import { createRDFStreamParser } from '@unrdf/streaming';

const parser = createRDFStreamParser({
  format: 'n-triples',
  chunkSize: 500, // emit a chunk every 500 quads
  onProgress: progress => {
    process.stdout.write(
      `\rProcessed ${progress.quadsProcessed} quads, ` +
        `${(progress.bytesProcessed / 1024).toFixed(1)} KB ` +
        `(${progress.duration}ms)`
    );
  },
  onQuad: quad => {
    // Called for every individual quad — use for streaming inserts
    // (Do NOT store all quads here if dataset is very large)
  },
});

const quads = [];

parser.on('data', chunk => {
  if (chunk.type === 'quads') {
    quads.push(...chunk.data);
  }
});

await new Promise((resolve, reject) => {
  createReadStream('large-dataset.nt').pipe(parser);
  parser.on('end', resolve);
  parser.on('error', reject);
});

console.log(`\nDone: ${quads.length} quads`);
```

---

## Part 4: Read Metrics After Parsing

Every `RDFStreamParser` instance tracks internal metrics. Call `getMetrics()` after the `'end'` event:

```javascript
import { createReadStream } from 'fs';
import { createRDFStreamParser } from '@unrdf/streaming';

const parser = createRDFStreamParser({ format: 'n-triples' });

await new Promise((resolve, reject) => {
  parser.resume(); // flowing mode — no 'data' listener needed
  createReadStream('data.nt').pipe(parser);
  parser.on('end', resolve);
  parser.on('error', reject);
});

const metrics = parser.getMetrics();
console.log('Quads parsed:   ', metrics.quadsProcessed);
console.log('Bytes read:     ', metrics.bytesProcessed);
console.log('Chunks emitted: ', metrics.chunksEmitted);
console.log('Duration (ms):  ', metrics.duration);
console.log('Throughput:     ', metrics.throughput.toFixed(0), 'quads/sec');
console.log('Backpressure:   ', metrics.backpressureEvents, 'events');
```

**Important:** Call `parser.resume()` before piping if you do not attach a `'data'` listener. A Transform stream in object mode stalls if its internal buffer fills with pushed objects and there is no consumer draining it.

---

## Part 5: Feed Parsed Quads into a Change Feed

Connect the parser output to a `ChangeFeed` so downstream subscribers react to each parsed quad in real time:

```javascript
import { createReadStream } from 'fs';
import { createRDFStreamParser, createChangeFeed } from '@unrdf/streaming';

const feed = createChangeFeed();
const parser = createRDFStreamParser({ format: 'n-triples', chunkSize: 100 });

// Subscribe to the feed
const unsubscribe = feed.subscribe(change => {
  // Called for every quad as it arrives from the file
  // console.log('Loaded:', change.quad.subject.value);
});

// Wire parser output into the feed
parser.on('data', chunk => {
  if (chunk.type === 'quads') {
    for (const quad of chunk.data) {
      feed.emitChange({ type: 'add', quad });
    }
  }
});

await new Promise((resolve, reject) => {
  createReadStream('data.nt').pipe(parser);
  parser.on('end', resolve);
  parser.on('error', reject);
});

unsubscribe();

const metrics = parser.getMetrics();
console.log('Streamed into feed:', metrics.quadsProcessed, 'quads');
console.log('Feed history size:', feed.getChanges().length);
```

---

## What You Learned

1. **Create a parser** — `createRDFStreamParser({ format, chunkSize })` returns a Transform stream
2. **Pipe data** — `readStream.pipe(parser)` drives the parse; always await `'end'`
3. **Receive chunks** — listen for `chunk.type === 'quads'` on `'data'`; `'complete'` has the final metrics
4. **Collect all quads** — `parseRDFStream(stream, options)` for a one-shot Promise
5. **Read metrics** — `parser.getMetrics()` after `'end'` for throughput and backpressure stats
6. **Resume if no consumer** — call `parser.resume()` when you only care about the end event

## Next Steps

- [How-To: Handle Backpressure](../how-to/02-handle-backpressure.md) — tune `highWaterMark` and `chunkSize` for your dataset
- [How-To: Monitor Stream Performance](../how-to/03-monitor-stream-performance.md) — attach a `PerformanceMonitor` for continuous metrics
- [Reference: Stream Protocol](../reference/stream-protocol.md) — full parser API and chunk shape definitions
