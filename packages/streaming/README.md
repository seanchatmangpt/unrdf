# @unrdf/streaming

![Version](https://img.shields.io/badge/version-[VERSION]--beta.1-blue) ![Production Ready](https://img.shields.io/badge/production-ready-green)

**Real-time Change Feeds and Synchronization**

Stream RDF changes in real-time. Subscribe to changes, process deltas, and maintain consistency.

## Installation

```bash
pnpm add @unrdf/streaming
```

## 📚 Examples

See these examples that demonstrate @unrdf/streaming:

- **[production-change-feed.mjs](./examples/production-change-feed.mjs)** - Production-ready change feed with validation
- **[streaming/basic-stream.mjs](../../examples/streaming/basic-stream.mjs)** - Real-time change feeds basics
- **[streaming/advanced-filters.mjs](../../examples/streaming/advanced-filters.mjs)** - Stream filtering and transformation
- **[streaming-admission.mjs](../../examples/streaming-admission.mjs)** - StreamingAdmission: delta admission with BLAKE3 receipt chaining and schema validation
- **[validate-shacl.mjs](../../examples/validate-shacl.mjs)** - validateShacl/validateQuad: API surface + stub status (violation detection not yet implemented)
- **[sync-protocol.mjs](../../examples/sync-protocol.mjs)** - Sync protocol: create, validate, merge, and apply sync messages
- **[rdf-stream-parser.mjs](../../examples/rdf-stream-parser.mjs)** - RDFStreamParser: chunk-based streaming with backpressure and format error handling
- **[performance-monitor.mjs](../../examples/performance-monitor.mjs)** - PerformanceMonitor: throughput/latency/memory tracking with threshold violation events

**Need real-time updates?** Start with [QUICKSTART-STREAMING.md](./QUICKSTART-STREAMING.md).

## 🚀 Quick Start

Get started in 5 minutes with our **[Quick Start Guide](./QUICKSTART-STREAMING.md)**.

```javascript
import { subscribeToChanges } from '@unrdf/streaming';

// Subscribe to all changes
const unsubscribe = subscribeToChanges(store, change => {
  console.log('Change:', change.quad);
  console.log('Type:', change.type); // 'add' or 'remove'
});

// Unsubscribe when done
unsubscribe();
```

## 🏗️ Architecture

@unrdf/streaming is designed as a modular pipeline for RDF data in motion.

```text
  [ RDF Data Source ]
          │
          ▼
┌───────────────────┐
│   Change Feed     │ ───► Event Emitters (add/remove/update)
└───────────────────┘
          │
          ▼
┌───────────────────┐
│Stream Processor   │ ───► Filtering, Mapping, Aggregation
└───────────────────┘
          │
          ▼
┌───────────────────┐
│Real-Time Validator│ ───► Incremental SHACL Validation
└───────────────────┘
          │
          ▼
┌───────────────────┐
│  Sync Protocol    │ ───► WebSocket / Remote Sync
└───────────────────┘
          │
          ▼
  [ RDF Consumers ] (UI, Search Index, Remote Store)
```

### Key Components:

- **Change Feed**: Wraps an RDF store to emit events for every atomic change.
- **Stream Processor**: A functional pipeline for transforming RDF streams with backpressure support.
- **Real-Time Validator**: Provides high-performance SHACL validation by only checking deltas.
- **Sync Protocol**: A robust mechanism for synchronizing state across distributed nodes.

## 🛠️ Troubleshooting

### High Latency in Validation

If validation is slow, ensure you are using `ValidationMode.DELTA`. For large complex graphs, `ValidationMode.INCREMENTAL` is often the best balance between speed and correctness.

### Missed Events

Ensure the `store` you are monitoring is passed to `createChangeFeed` _before_ you start making changes. The feed monkey-patches the store's mutation methods to capture events.

### Out of Memory

The Change Feed maintains a ring buffer of recent changes. If you are processing millions of changes, reduce the `maxHistorySize` (default: 10,000) or disable history entirely if not needed for replay.

### WebSocket Connection Drops

The Sync Protocol includes built-in heartbeat support, but you should implement your own reconnection logic at the application level if using raw WebSockets.

## Features

- ✅ Real-time change subscriptions
- ✅ Guaranteed delivery semantics
- ✅ Delta computation and streaming
- ✅ Change filtering (by subject, predicate, etc)
- ✅ Batch operations
- ✅ WebSocket support
- ✅ SHACL validation for streaming data
- ✅ Built-in observability with OpenTelemetry

## Use Cases

- **Real-time UI updates**: Push RDF changes to clients
- **Data synchronization**: Keep distributed stores in sync
- **Audit trails**: Track all changes to RDF graphs
- **Change streams**: Process changes downstream
- **Reactive applications**: Build reactive RDF apps

## API Reference

### Streaming Admission (`stream-admit.mjs`)

Validates RDF delta operations and produces a tamper-evident BLAKE3 receipt chain.
Each `admit()` call computes input/output/delta hashes, links to the previous receipt,
and either applies the delta to the store or rolls it back — returning a validated receipt.
The chain proves complete provenance: every change has a signed, linked receipt.

See **[examples/streaming-admission.mjs](../../examples/streaming-admission.mjs)** for a full runnable demonstration.

```javascript
import {
  StreamingAdmission,
  createStreamingAdmission,
  DeltaConditionSchema,
  ReceiptSchema,
} from '@unrdf/streaming/stream-admit';

const admission = createStreamingAdmission(store, {
  nodeId: 'node-A',
  emitReceipts: true,
  rollbackOnFailure: true,
  condition: { kind: 'delta', checksum: 'blake3' }, // optional gate
});

// Admit a single delta (additions + removals)
const { receipt, admitted } = await admission.admit({
  additions: [myQuad],
  removals: [],
});
// receipt: { id, inputHash, outputHash, deltaHash, previousReceiptHash, receiptHash, status, ... }

// Batch — returns array of { receipt, admitted } per delta
const results = await admission.admitStream([delta1, delta2, delta3]);

// Verify the chain hasn't been tampered with
const { valid, mismatches } = admission.verifyChain();

// Stats
const { total, admitted: admittedCount, rejected, chainLength } = admission.getStats();
```

**`DeltaConditionSchema`** — Zod schema for validating condition objects before passing them in.
Accepts `{ kind: 'delta', checksum: 'blake3'|'sha256'|'crc32', hash?, expectedDeltaHash?, expectedInputHash? }`.

**`ReceiptSchema`** — Zod schema for the receipt shape. Useful for validating receipts received from other nodes before trusting them. `admit()` validates its own output against this schema before returning.

### Sync Protocol (`sync-protocol.mjs`)

Serialize RDF changes into tamper-detectable messages for distribution across nodes.
Each message carries a SHA-256 checksum; `parseSyncMessage` rejects any message whose
checksum doesn't match, catching corruption and replay attacks before changes touch the store.

See **[examples/sync-protocol.mjs](../../examples/sync-protocol.mjs)** for a runnable demonstration of all six functions.

```javascript
import {
  createSyncMessage,
  parseSyncMessage,
  calculateChecksum,
  mergeSyncMessages,
  applySyncMessage,
  createSyncMessageFromFeed,
} from '@unrdf/streaming/sync-protocol';

// Create a message from an array of change events
const message = createSyncMessage([{ type: 'add', quad: myQuad, timestamp: Date.now() }], {
  source: 'node-A',
  sequence: 1,
});
// → { version: '1.0', changes: [...], checksum: '<sha256>', timestamp, source, sequence }

// Validate a received message (throws if checksum mismatches)
const validated = parseSyncMessage(message);

// Merge messages from multiple nodes — deduplicates by quad+type+timestamp
const merged = mergeSyncMessages([messageFromA, messageFromB]);

// Apply a message to a local change feed
const applied = applySyncMessage(feed, message); // returns count of applied changes

// Build a message from feed history (supports since/limit options)
const syncMsg = createSyncMessageFromFeed(feed, { since: lastSyncTimestamp });
```

**`createSyncMessage(changes, options?)`**

- `changes` — `Array<{ type: 'add'|'remove'|'update', quad, timestamp, metadata? }>`
- `options.source` — identifier for the originating node
- `options.sequence` — monotonic counter for ordering
- Returns a validated sync message object with SHA-256 checksum

**`parseSyncMessage(message)`**

- Validates the message schema and recomputes the checksum
- Throws `Error('Checksum mismatch: message may be corrupted')` if tampered

**`mergeSyncMessages(messages[])`**

- Collects all changes from all messages, sorts by timestamp, deduplicates
- Returns a new sync message containing the union

**`applySyncMessage(feed, message)`**

- Calls `feed.emitChange(change)` for each validated change
- Returns the count of changes applied

**`createSyncMessageFromFeed(feed, options?)`**

- `options.since` — only include changes after this timestamp
- `options.limit` — max changes to include
- Calls `feed.getHistory(options)` and wraps the result in a sync message

### RDF Stream Parser (`rdf-stream-parser.mjs`)

A Node.js Transform stream that parses RDF text into quad objects with automatic backpressure,
configurable chunk sizes, and per-operation OTel tracing. Designed for large datasets where
loading all quads at once would exhaust memory.

See **[examples/rdf-stream-parser.mjs](../../examples/rdf-stream-parser.mjs)** for a runnable demonstration.

```javascript
import {
  RDFStreamParser,
  createRDFStreamParser,
  parseRDFStream,
} from '@unrdf/streaming/rdf-stream-parser';

// Transform stream — pipe RDF text in, get quad chunks out
const parser = createRDFStreamParser({
  format: 'n-triples', // 'turtle' | 'n-triples' | 'n-quads' | 'trig'
  chunkSize: 1000, // quads per emitted chunk (default 1000)
  highWaterMark: 16384, // bytes (default 16KB)
  enableBackpressure: true,
  onProgress: stats => console.log(`${stats.quadsProcessed} quads processed`),
  onError: err => console.error('Parse error:', err.message),
});

parser.on('data', chunk => {
  if (chunk.type === 'quads') {
    // chunk.data: Quad[], chunk.count: number, chunk.timestamp: number
    console.log(`Received ${chunk.count} quads`);
  }
  if (chunk.type === 'complete') {
    // chunk.metrics: { quadsProcessed, chunksEmitted, bytesProcessed, duration, ... }
    console.log('Done:', chunk.metrics);
  }
});

readableStream.pipe(parser);

// Convenience wrapper — collects all quads into an array
const quads = await parseRDFStream(readableStream, { format: 'turtle' });
```

**Constructor options:**

- `format` — RDF serialization (default `'turtle'`)
- `chunkSize` — quads to buffer before emitting a chunk (default 1000)
- `highWaterMark` — Transform stream buffer size in bytes (default 16384)
- `enableBackpressure` — pause the N3 parser when downstream is full (default true)
- `onQuad(quad)` — called for each parsed quad (before chunking)
- `onError(err)` — called on parse errors
- `onProgress(stats)` — called every 10,000 quads

**`parser.getMetrics()`** — returns `{ quadsProcessed, chunksEmitted, bytesProcessed, duration, throughput, backpressureRate }`

### Performance Monitor (`performance-monitor.mjs`)

Monitors streaming throughput, latency, memory usage, and backpressure in a rolling time window.
Fires `'threshold-violation'` events when a metric exceeds its configured bound.

See **[examples/performance-monitor.mjs](../../examples/performance-monitor.mjs)** for a runnable demonstration.

```javascript
import { createPerformanceMonitor } from '@unrdf/streaming/performance-monitor';

const monitor = createPerformanceMonitor({
  sampleInterval: 1000, // collect a sample every 1s
  windowSize: 60, // keep 60 samples (1 minute)
  thresholds: {
    throughputMin: 500, // quads/sec — fires 'threshold-violation' if below
    memoryMax: 256 * 1024 * 1024, // bytes — fires if heap exceeds 256MB
  },
});

monitor.on('threshold-violation', ({ metric, value, threshold }) => {
  console.warn(`${metric} threshold breached: ${value} vs ${threshold}`);
});

monitor.start();

// Record operations as your stream processes data
monitor.recordQuad(latencyMs); // track a quad + optional latency
monitor.recordBytes(chunkBytes); // track bytes processed
monitor.recordChunk(); // count a chunk emitted
monitor.recordError(error); // emits 'error-recorded'
monitor.recordBackpressure(); // emits 'backpressure'

const metrics = monitor.getCurrentMetrics();
// → { quadsProcessed, bytesProcessed, chunksProcessed, errorsCount, backpressureEvents, duration, averageThroughput }

monitor.stop(); // emits 'stopped' with full report

const report = monitor.getReport();
// → { summary, throughput: {mean,min,max,p50,p95,p99}, latency, memory, backpressure, errors }

monitor.reset(); // clears all metrics, emits 'reset'
```

**Events:** `'started'`, `'stopped'` (with report), `'backpressure'`, `'error-recorded'`, `'threshold-violation'`, `'reset'`

### SHACL Validation (`validate.mjs`)

> **⚠️ Implementation status: stub/placeholder.**
> `validateShacl` extracts shape metadata from the shapes store but does not yet evaluate constraints
> (`sh:minCount`, `sh:datatype`, `sh:pattern`, etc.). It always returns `{ conforms: true, results: [] }`.
> This will be a complete SHACL validator in a future release.
> **Do not depend on `validateShacl` detecting violations in production — it cannot.**

The API surface is stable and safe to use for structure, but violation detection is not yet implemented:

```javascript
import { validateShacl, validateQuad } from '@unrdf/streaming/validate';

// API is callable — returns { conforms, results, warnings, timestamp }
// conforms is always true in the current stub implementation
const result = await validateShacl(dataStore, shapesStore);
// → { conforms: true, results: [], warnings: [], timestamp: <number> }

// validateQuad wraps a single quad in a temp store and calls validateShacl
const quadResult = await validateQuad(myQuad, shapesStore);
// → { conforms: true, results: [], ... }
```

**Options** (accepted and validated, but `strict`/`maxViolations` have no effect yet):

- `strict` (boolean): Reserved — will throw on first violation once implemented
- `maxViolations` (number): Reserved — will cap violation count once implemented

### Observability (`observability.mjs`)

Built-in OpenTelemetry instrumentation for monitoring streaming operations:

```javascript
import { createObservabilityManager } from '@unrdf/streaming/observability';

const obs = createObservabilityManager({
  serviceName: 'my-streaming-service',
  version: '[VERSION]',
});

// Record operations
obs.recordOperation('stream_started', { subject: 'ex:Alice' });
obs.recordError('validation_failed', error, { context: 'streaming' });

// Use spans for tracing
await obs.withSpan('process-changes', async span => {
  // Your streaming logic here
  span.setAttribute('quad.count', changes.length);
});
```

**Metrics Tracked:**

- `streaming.operations` (counter): Total operations
- `streaming.errors` (counter): Total errors
- `streaming.duration` (histogram): Operation duration
- `streaming.cache.hits/misses` (counter): Cache statistics

## Documentation

- **[API Reference](./docs/API.md)** - Complete API documentation
- **[User Guide](./docs/GUIDE.md)** - Change feed patterns
- **[Examples](./examples/)** - Code examples
- **[Contributing](./docs/CONTRIBUTING.md)** - How to contribute

## Depends On

- `@unrdf/core` - RDF substrate
- `@unrdf/hooks` - Policy enforcement

## VOC Usage

- VOC-2: Sync Agent (stream changes for sync)
- VOC-4: Audit Agent (subscribe to changes)
- VOC-6: App Developer (real-time UI updates)

## License

MIT
