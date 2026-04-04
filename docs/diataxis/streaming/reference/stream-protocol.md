# Stream Protocol Reference

Complete API for `RDFStreamParser`, `parseRDFStream`, and the sync protocol functions exported from `@unrdf/streaming`.

---

## RDFStreamParser

A Node.js `Transform` stream (object mode) that parses RDF text and emits quad chunks.

### createRDFStreamParser(options?)

```typescript
createRDFStreamParser(options?: RDFStreamParserOptions): RDFStreamParser
```

Factory function. Equivalent to `new RDFStreamParser(options)`.

### RDFStreamParserOptions

| Field                | Type                                                   | Default    | Description                                         |
| -------------------- | ------------------------------------------------------ | ---------- | --------------------------------------------------- |
| `format`             | `'turtle'` \| `'n-triples'` \| `'n-quads'` \| `'trig'` | `'turtle'` | Input RDF serialization format                      |
| `baseIRI`            | string                                                 | —          | Base IRI for resolving relative IRIs                |
| `blankNodePrefix`    | string                                                 | —          | Prefix string for blank node identifiers            |
| `highWaterMark`      | number (bytes)                                         | `16384`    | Stream read buffer size                             |
| `chunkSize`          | number                                                 | `1000`     | Quads accumulated before emitting a `'quads'` chunk |
| `enableBackpressure` | boolean                                                | `true`     | Emit `'progress'` chunks to signal flow             |
| `onQuad`             | `(quad: Quad) => void`                                 | —          | Called synchronously for each parsed quad           |
| `onError`            | `(err: Error) => void`                                 | —          | Called for parse errors (does not stop parsing)     |
| `onProgress`         | `(progress: ProgressInfo) => void`                     | —          | Called every 10 000 quads                           |

Throws a Zod `ZodError` if options fail validation.

### ProgressInfo (onProgress argument)

```typescript
{
  quadsProcessed: number;
  bytesProcessed: number;
  chunksEmitted: number;
  duration: number; // ms since parser creation
}
```

---

### Piping

`RDFStreamParser` is a standard Node.js Transform stream. Pipe a readable into it:

```javascript
import { createReadStream } from 'fs';
import { createRDFStreamParser } from '@unrdf/streaming';

const parser = createRDFStreamParser({ format: 'n-triples' });

createReadStream('data.nt').pipe(parser);
```

---

### Output Chunks

The parser emits three chunk types on its `'data'` event:

| `chunk.type` | Fields                                               | When emitted                                    |
| ------------ | ---------------------------------------------------- | ----------------------------------------------- |
| `'quads'`    | `data: Quad[]`, `count: number`, `timestamp: number` | When `chunkSize` quads have been accumulated    |
| `'progress'` | `quads: number`                                      | Per input chunk when `enableBackpressure: true` |
| `'complete'` | `metrics: ParserMetrics`                             | Once, after all data has been flushed           |

The `'complete'` chunk is always the last item on the stream before `'end'` fires.

---

### RDFStreamParser.getMetrics()

```typescript
getMetrics(): ParserMetrics
```

Available after the `'end'` event.

```typescript
{
  quadsProcessed: number; // total quads parsed
  chunksEmitted: number; // number of 'quads' chunks pushed
  bytesProcessed: number; // total input bytes
  backpressureEvents: number; // times backpressure was triggered
  errors: number; // parse errors (non-fatal, counted via onError)
  duration: number; // ms from parser creation to flush end
  throughput: number; // quadsProcessed / duration * 1000 (quads/sec)
  backpressureRate: number; // backpressureEvents / chunksEmitted (0 if no chunks)
}
```

**Note:** Call `parser.resume()` before piping if you do not attach a `'data'` listener. Without a consumer, the Transform stream stalls when its internal object-mode buffer fills.

---

## parseRDFStream(stream, options?)

```typescript
parseRDFStream(stream: Readable, options?: RDFStreamParserOptions): Promise<Quad[]>
```

Convenience function. Parses the entire stream and resolves with an array of all quads.

```javascript
import { parseRDFStream } from '@unrdf/streaming';
import { createReadStream } from 'fs';

const quads = await parseRDFStream(createReadStream('data.nt'), {
  format: 'n-triples',
  chunkSize: 500,
});
console.log(quads.length);
```

Rejects with a `Error` if any non-recoverable stream error occurs. Individual parse errors (via `onError`) are not rejections.

---

## Sync Protocol

Functions for creating, validating, merging, and applying checksummed sync messages between nodes.

### createSyncMessage(changes, options?)

```typescript
createSyncMessage(
  changes: ChangeRecord[],
  options?: { source?: string; sequence?: number }
): SyncMessage
```

Produce a sync message from an array of change records. The checksum is a SHA-256 hex string of the canonical change representations (sorted by timestamp).

```javascript
import { createSyncMessage } from '@unrdf/streaming';

const message = createSyncMessage(
  [{ type: 'add', quad: { subject, predicate, object }, timestamp: Date.now() }],
  { source: 'node-1', sequence: 42 }
);
// { version: '1.0', changes: [...], checksum: '<64-char hex>', timestamp: <ms>, source: 'node-1', sequence: 42 }
```

### SyncMessage shape

```typescript
{
  version:   '1.0';
  changes:   ChangeRecord[];
  checksum:  string;  // 64-char SHA-256 hex
  timestamp: number;  // Unix ms, set at creation
  source?:   string;
  sequence?: number;
}
```

### ChangeRecord shape

```typescript
{
  type:      'add' | 'remove' | 'update';
  quad:      { subject: any; predicate: any; object: any; graph?: any };
  timestamp: number;
  metadata?: Record<string, any>;
}
```

---

### parseSyncMessage(message)

```typescript
parseSyncMessage(message: unknown): SyncMessage
```

Validate and verify a sync message. Throws:

- `ZodError` — if the message fails schema validation
- `Error('Checksum mismatch: message may be corrupted')` — if the checksum does not match the recalculated value

```javascript
import { parseSyncMessage } from '@unrdf/streaming';

const validated = parseSyncMessage(receivedJson); // throws if tampered
```

---

### calculateChecksum(changes)

```typescript
calculateChecksum(changes: ChangeRecord[]): string
```

Compute a 64-character SHA-256 hex checksum of `changes`. Changes are sorted by `timestamp` before hashing to produce a stable, order-independent result.

```javascript
import { calculateChecksum } from '@unrdf/streaming';

const cs = calculateChecksum(changes);
// Always returns the same string for the same set of changes
```

---

### mergeSyncMessages(messages)

```typescript
mergeSyncMessages(messages: SyncMessage[]): SyncMessage
```

Merge multiple sync messages into one. Each input message is validated (checksum checked). All changes are collected, sorted by timestamp, and deduplicated by `type + subject + predicate + object + timestamp`. Returns a new `SyncMessage` with `source: 'merged'`.

```javascript
import { mergeSyncMessages } from '@unrdf/streaming';

const merged = mergeSyncMessages([messageA, messageB, messageC]);
```

---

### applySyncMessage(feed, message)

```typescript
applySyncMessage(feed: ChangeFeed, message: SyncMessage): number
```

Validate the message and call `feed.emitChange()` for each change in it. Returns the number of changes applied.

```javascript
import { applySyncMessage } from '@unrdf/streaming';

const applied = applySyncMessage(feed, receivedMessage);
console.log(`Applied ${applied} changes`);
```

---

### createSyncMessageFromFeed(feed, options?)

```typescript
createSyncMessageFromFeed(
  feed: ChangeFeed,
  options?: { since?: number; limit?: number }
): SyncMessage
```

Build a sync message from a feed's history. Equivalent to `createSyncMessage(feed.getHistory(options), { source: 'feed' })`.

```javascript
import { createSyncMessageFromFeed } from '@unrdf/streaming';

// Send only changes since last sync
const msg = createSyncMessageFromFeed(feed, { since: lastSyncTimestamp });
await sendToRemoteNode(JSON.stringify(msg));
```
