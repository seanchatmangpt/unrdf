# Explanation: Backpressure and Flow Control

**Why backpressure matters for RDF streaming and how `@unrdf/streaming` manages it.**

---

## What Backpressure Is

Backpressure is what happens when a producer generates data faster than a consumer can process it. Without backpressure management, the runtime buffers the excess data in memory. If the imbalance continues long enough, you either run out of heap or the process becomes so slow under GC pressure that it appears to hang.

In an RDF streaming context, the producer is typically:

- A file read stream delivering 50 MB/s of N-Triples
- A network socket streaming live quad events from a federation peer

The consumer is typically:

- An Oxigraph store writing quads to disk (I/O bound)
- A validation pipeline checking SHACL shapes (CPU bound)
- A remote HTTP endpoint receiving batched updates (network bound)

The difference in speed between producer and consumer is the pressure.

---

## Node.js Stream Mechanics

Node.js readable streams implement backpressure through the `highWaterMark` threshold and a two-state model: **flowing** and **paused**.

```
Readable (file/network)
        │
        │ push()
        ↓
  Internal Buffer
        │
        │ When buffer.length >= highWaterMark:
        │   push() returns false → source should pause
        │
        ↓
  Transform / Writable (your consumer)
        │
        │ When consumer drains:
        │   emits 'drain' → source may resume
```

`Transform` streams participate on both sides: they have a writable side (receiving from the source) and a readable side (pushing to the downstream consumer). If the downstream is slow, the readable buffer fills, `push()` returns `false`, and the transform should stop calling `push()` until `'drain'` fires.

The `RDFStreamParser` extends `Transform` with `objectMode: true`. This means each item it pushes is a chunk object (`{ type: 'quads', data: [...] }`), not a byte buffer. The `highWaterMark` in object mode is measured in object count, not bytes.

---

## Why the Parser Buffers Input, Not Quads

The N3 parser used internally (`@unrdf/core/rdf/n3-justified-only`) requires the complete input string before it can produce quads — it does not support incremental chunk-by-chunk parsing. Therefore `RDFStreamParser._transform()` accumulates incoming byte chunks into a `buffer` string, and `_flush()` does the actual parsing all at once when the stream ends.

This design means:

1. **Memory growth is proportional to input size**: The entire RDF text must fit in memory during `_flush`. For 100 MB files, 100 MB of string is held.
2. **The `chunkSize` parameter controls output chunking**: Once parsing is complete, quads are emitted in groups of `chunkSize` to give the consumer a chance to drain between groups.
3. **`enableBackpressure` controls `'progress'` emissions**: During `_transform`, a `{ type: 'progress', quads: 0 }` object is pushed to signal the downstream that data is being received. If `push()` returns `false`, the `backpressureActive` flag is set and `backpressureEvents` is incremented.

For truly large datasets that cannot fit in memory, the recommended approach is to split the source into smaller files before streaming, or use a format that supports incremental parsing (future work).

---

## How `chunkSize` and `highWaterMark` Interact

```
N-Triples file (100 000 quads)
         │
   _transform()  ← accumulates input string
         │
   _flush()      ← N3 parses all 100 000 quads
         │
   chunkBuffer   ← 100-quad chunks queued
         │
   _emitChunk()  ← push({ type: 'quads', data: [...100 quads] })
         │
   downstream 'data' listener
```

If `chunkSize` is `1000`, the parser emits 100 chunks of 1000 quads each. If the downstream listener processes each chunk synchronously in 10 ms, the total processing time is 1 second. If the downstream is async (e.g., writing to a database), the listener can call `parser.pause()` between chunks to prevent the Transform's object-mode buffer from filling.

The `highWaterMark` default of `16384` bytes applies to the writable side (incoming byte chunks from the source). Once the readable buffer (object-mode) has more than `highWaterMark` objects queued, Node.js signals the source to pause.

---

## The Resume Problem

A common mistake with object-mode Transform streams is forgetting to consume the output:

```javascript
// WRONG: no 'data' listener and no .resume() — the stream stalls
const parser = createRDFStreamParser({ format: 'n-triples' });
createReadStream('data.nt').pipe(parser);
await new Promise(resolve => parser.on('end', resolve)); // never fires
```

When there is no consumer draining the readable side, each `push()` in `_emitChunk()` fills the internal buffer. Once it is full, the Transform stops calling its `_transform` callback, which means `_flush` never runs, which means `'end'` never fires.

**Fix**: Add a `'data'` listener or call `parser.resume()` to put the stream in flowing mode:

```javascript
// Option A: data listener
parser.on('data', chunk => {
  /* consume */
});

// Option B: resume (discard output)
parser.resume();
```

This is documented in the CLAUDE.md Streaming / Transform Stream Gotchas section.

---

## Change Feed Backpressure: A Different Problem

The `ChangeFeed` and `StreamProcessor` are synchronous event dispatchers — they do not use Node.js streams. Backpressure here is conceptual rather than mechanical:

- When `emitChange` is called, it synchronously calls all subscribers before returning
- A slow subscriber blocks the caller until it completes
- There is no built-in mechanism to pause emission if subscribers are overloaded

Strategies for managing feed backpressure:

| Strategy                 | Mechanism                                | Trade-off                                      |
| ------------------------ | ---------------------------------------- | ---------------------------------------------- |
| Batch                    | `processor.batch(N)`                     | Reduces call frequency; adds N-event latency   |
| Debounce                 | `processor.debounce(ms)`                 | Collapses bursts; only last event per window   |
| Async subscriber         | Use `feed.subscribe` with async callback | Does not block emitter, but callbacks overlap  |
| Ring buffer limit        | `maxHistorySize: N`                      | Caps memory; oldest changes are lost           |
| Separate feed per domain | Multiple `createChangeFeed` instances    | Independent queues; no cross-domain contention |

The `PerformanceMonitor.recordBackpressure()` call is a manual marker. The feed does not call it automatically — it is intended for the developer to call when they detect that their own subscriber is falling behind (e.g., when a write queue depth exceeds a threshold).

---

## PerformanceMonitor Sampling

The `PerformanceMonitor` collects samples on a fixed `sampleInterval` timer. Each sample computes throughput as:

```
throughput = (quadsProcessed_total / elapsed_since_last_sample) * 1000
```

This is an instantaneous rate, not a rolling average. The `windowSize` parameter limits how many samples are retained for the statistical report (`mean`, `p95`, etc.). Once the window is full, old samples are evicted from the front of the array (FIFO, same pattern as the change feed ring buffer).

The `threshold-violation` event fires synchronously inside the `_collectSample` call. If your violation handler is slow, it blocks the monitoring interval — keep it lightweight.

---

## See Also

- [How-To: Handle Backpressure](../how-to/02-handle-backpressure.md)
- [How-To: Monitor Stream Performance](../how-to/03-monitor-stream-performance.md)
- [Reference: Stream Protocol](../reference/stream-protocol.md)
