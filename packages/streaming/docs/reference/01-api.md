# API Reference: @unrdf/streaming

Complete reference for streaming RDF data.

---

## streamRdf(source, options?)

Create a readable stream of RDF quads from a source.

**Signature:**
```javascript
function streamRdf(
  source: string | ReadableStream,
  options?: StreamOptions
): AsyncIterable<Quad>
```

**Parameters:**
- `source` - File path or readable stream
- `options.format?` - RDF format: `'turtle'` | `'ntriples'` | `'nquads'`
- `options.encoding?` - Text encoding (default: `'utf-8'`)
- `options.highWaterMark?` - Chunk size in bytes (default: 65536)

**Returns:** Async iterable of quads

**Example:**
```javascript
import { streamRdf } from '@unrdf/streaming';

for await (const quad of streamRdf('data.nt')) {
  console.log(quad);
}
```

---

## Backpressure Handling

### pauseStream(stream)

Pause streaming to prevent buffer overflow.

**Signature:**
```javascript
function pauseStream(stream: AsyncIterable<Quad>): void
```

### resumeStream(stream)

Resume paused stream.

**Signature:**
```javascript
function resumeStream(stream: AsyncIterable<Quad>): void
```

---

## Buffering

### createBuffer(size?)

Create a buffer for collecting quads.

**Signature:**
```javascript
function createBuffer(size?: number): QuadBuffer
```

**Example:**
```javascript
const buffer = createBuffer(1000);
for await (const quad of stream) {
  buffer.add(quad);

  if (buffer.isFull()) {
    processBatch(buffer.drain());
  }
}
```

---

## Statistics

### StreamStats

Get streaming statistics.

```javascript
interface StreamStats {
  quadsProcessed: number
  quadsPerSecond: number
  bytesProcessed: number
  memoryUsage: number
  elapsed: number
}
```

**Example:**
```javascript
const stats = stream.getStats();
console.log(`${stats.quadsPerSecond} quads/sec`);
```

---

## Error Handling

Streams emit errors for:
- Invalid RDF syntax
- File not found
- Encoding issues
- Stream interruption

**Pattern:**
```javascript
stream.on('error', (error) => {
  console.error('Stream error:', error);
});
```

---

## Next Reading

- **TYPES.md** (Reference) - Type definitions
- **CONFIGURATION.md** (Reference) - Streaming options
- **optimize-memory** (How-To) - Memory optimization
