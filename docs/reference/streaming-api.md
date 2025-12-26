# Streaming API Reference

**Version**: 5.0.0+
**Package**: `@unrdf/core/rdf/n3-justified-only`, `@unrdf/streaming`
**Stability**: Stable

---

## Overview

UNRDF provides two levels of streaming APIs:

1. **Low-level N3 Streaming** (`@unrdf/core/rdf/n3-justified-only`)
   - SAX-like RDF parsing
   - Backpressure-aware streaming
   - Memory-efficient serialization

2. **High-level Change Feeds** (`@unrdf/streaming`)
   - Real-time change notifications
   - Multi-client subscriptions
   - Sync protocol for distributed systems

---

## Table of Contents

### N3 Streaming API
1. [streamingParse](#streamingparse)
2. [streamingWrite](#streamingwrite)
3. [createStreamParser](#createstreamparser)
4. [createStreamWriter](#createstreamwriter)
5. [UnrdfDataFactory](#unrdfdatafactory)

### Change Feed API
6. [createChangeFeed](#createchangefeed)
7. [createSubscriptionManager](#createsubscriptionmanager)
8. [createStreamProcessor](#createstreamprocessor)

### Sync Protocol
9. [createSyncMessage](#createsyncmessage)
10. [parseSyncMessage](#parsesyncmessage)
11. [calculateChecksum](#calculatechecksum)
12. [mergeSyncMessages](#mergesyncmessages)

---

## N3 Streaming API

### streamingParse

Parse RDF data using N3's streaming SAX-like parser.

**Signature**:
```javascript
async function streamingParse(
  input: string,
  options?: ParseOptions
): Promise<Quad[]>
```

**Parameters**:

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `input` | `string` | Yes | - | RDF content to parse |
| `options.format` | `string` | No | `'text/turtle'` | RDF format (turtle, ntriples, nquads, trig) |
| `options.baseIRI` | `string` | No | - | Base IRI for relative URIs |

**Returns**: `Promise<Quad[]>` - Array of parsed quads

**Throws**:
- `Error` - If parsing fails or invalid RDF syntax

**Example**:
```javascript
import { streamingParse } from '@unrdf/core/rdf/n3-justified-only';

const turtle = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:alice a foaf:Person ;
    foaf:name "Alice" ;
    foaf:knows ex:bob .
`;

const quads = await streamingParse(turtle, {
  format: 'text/turtle',
  baseIRI: 'http://example.org/'
});

console.log(`Parsed ${quads.length} quads`);
```

**Performance**:
- Memory: O(1) during parsing (streaming)
- Speed: 10,000-50,000 quads/sec
- Suitable for: Large files (>100MB)

**Version**: 5.0.0 | **Stability**: Stable

---

### streamingWrite

Serialize quads to RDF using N3's streaming writer.

**Signature**:
```javascript
async function streamingWrite(
  quads: Quad[],
  options?: WriteOptions
): Promise<string>
```

**Parameters**:

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `quads` | `Quad[]` | Yes | - | Quads to serialize |
| `options.format` | `string` | No | `'text/turtle'` | Output format |
| `options.prefixes` | `Object` | No | `{}` | Namespace prefixes |

**Returns**: `Promise<string>` - Serialized RDF string

**Example**:
```javascript
import { streamingWrite } from '@unrdf/core/rdf/n3-justified-only';
import { namedNode, literal } from '@unrdf/core';

const quads = [
  {
    subject: namedNode('http://example.org/alice'),
    predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
    object: literal('Alice'),
    graph: { value: '' }
  }
];

const turtle = await streamingWrite(quads, {
  format: 'text/turtle',
  prefixes: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/'
  }
});

console.log(turtle);
// @prefix ex: <http://example.org/> .
// @prefix foaf: <http://xmlns.com/foaf/0.1/> .
// ex:alice foaf:name "Alice" .
```

**Version**: 5.0.0 | **Stability**: Stable

---

### createStreamParser

Create a Node.js stream parser for large files with backpressure handling.

**Signature**:
```javascript
function createStreamParser(
  options?: StreamParserOptions
): StreamParser
```

**Parameters**:

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `options.format` | `string` | No | `'text/turtle'` | RDF format |
| `options.baseIRI` | `string` | No | - | Base IRI |

**Returns**: `StreamParser` - N3 StreamParser instance (implements Transform stream)

**Example**:
```javascript
import { createStreamParser } from '@unrdf/core/rdf/n3-justified-only';
import { createReadStream } from 'fs';

const parser = createStreamParser({
  format: 'text/turtle',
  baseIRI: 'http://example.org/'
});

const fileStream = createReadStream('large-dataset.ttl');
const quads = [];

parser.on('data', (quad) => {
  quads.push(quad);
  console.log(`Parsed quad: ${quad.subject.value}`);
});

parser.on('end', () => {
  console.log(`Total quads: ${quads.length}`);
});

parser.on('error', (error) => {
  console.error('Parse error:', error);
});

fileStream.pipe(parser);
```

**Backpressure Handling**:
```javascript
import { createStreamParser, createStreamWriter } from '@unrdf/core/rdf/n3-justified-only';
import { createReadStream, createWriteStream } from 'fs';

const parser = createStreamParser({ format: 'turtle' });
const writer = createStreamWriter({ format: 'ntriples' });

createReadStream('input.ttl')
  .pipe(parser)
  .pipe(writer)
  .pipe(createWriteStream('output.nt'));
```

**Version**: 5.0.0 | **Stability**: Stable

---

### createStreamWriter

Create a Node.js stream writer for large datasets with backpressure handling.

**Signature**:
```javascript
function createStreamWriter(
  options?: StreamWriterOptions
): StreamWriter
```

**Returns**: `StreamWriter` - N3 StreamWriter instance (implements Transform stream)

**Example**:
```javascript
import { createStreamWriter } from '@unrdf/core/rdf/n3-justified-only';
import { createWriteStream } from 'fs';

const writer = createStreamWriter({
  format: 'text/turtle',
  prefixes: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/'
  }
});

const outputFile = createWriteStream('output.ttl');
writer.pipe(outputFile);

// Stream quads
for (const quad of largeQuadArray) {
  writer.write(quad);
}

writer.end();
```

**Version**: 5.0.0 | **Stability**: Stable

---

### UnrdfDataFactory

Isolated RDF term factory (N3 implementation).

**Signature**:
```javascript
const UnrdfDataFactory = {
  namedNode: (iri: string) => NamedNode,
  blankNode: (label?: string) => BlankNode,
  literal: (value: string, languageOrDatatype?: string | NamedNode) => Literal,
  variable: (name: string) => Variable,
  defaultGraph: () => DefaultGraph,
  quad: (subject: Term, predicate: NamedNode, object: Term, graph?: Term) => Quad,
  triple: (subject: Term, predicate: NamedNode, object: Term) => Quad
}
```

**Example**:
```javascript
import { UnrdfDataFactory } from '@unrdf/core/rdf/n3-justified-only';

const subject = UnrdfDataFactory.namedNode('http://example.org/alice');
const predicate = UnrdfDataFactory.namedNode('http://xmlns.com/foaf/0.1/name');
const object = UnrdfDataFactory.literal('Alice');

const quad = UnrdfDataFactory.quad(subject, predicate, object);
```

**Note**: For new code, prefer importing from `@unrdf/core`:

```javascript
import { namedNode, literal, blankNode } from '@unrdf/core';
```

**Version**: 5.0.0 | **Stability**: Stable

---

## Change Feed API

### createChangeFeed

Create a real-time change feed for store mutations.

**Signature**:
```javascript
function createChangeFeed(
  store: Store,
  options?: ChangeFeedOptions
): ChangeFeed
```

**Parameters**:

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `store` | `Store` | Yes | - | Store to monitor |
| `options.bufferSize` | `number` | No | `100` | Max buffered changes |
| `options.includeInitial` | `boolean` | No | `false` | Emit current store state |

**Returns**: `ChangeFeed` - Change feed object

**Example**:
```javascript
import { createChangeFeed } from '@unrdf/streaming';
import { createStore } from '@unrdf/core';

const store = createStore();
const feed = createChangeFeed(store, {
  bufferSize: 1000,
  includeInitial: false
});

feed.on('change', (event) => {
  console.log('Change type:', event.type); // 'add' | 'remove'
  console.log('Quad:', event.quad);
  console.log('Timestamp:', event.timestamp);
});

// Mutations trigger events
store.add(subject, predicate, object);
// -> Emits 'change' event
```

**Version**: 5.0.0 | **Stability**: Stable

---

### createSubscriptionManager

Manage multi-client subscriptions to change feeds.

**Signature**:
```javascript
function createSubscriptionManager(
  options?: SubscriptionManagerOptions
): SubscriptionManager
```

**Example**:
```javascript
import { createSubscriptionManager } from '@unrdf/streaming';

const manager = createSubscriptionManager();

// Client subscribes
const subscription = manager.subscribe({
  pattern: {
    subject: 'http://example.org/alice',
    predicate: null,
    object: null
  },
  callback: (event) => {
    console.log('Alice changed:', event);
  }
});

// Later: unsubscribe
subscription.unsubscribe();
```

**Version**: 5.0.0 | **Stability**: Stable

---

### createStreamProcessor

Create a stream processor for transforming change feeds.

**Signature**:
```javascript
function createStreamProcessor(
  options?: StreamProcessorOptions
): StreamProcessor
```

**Example**:
```javascript
import { createStreamProcessor, createChangeFeed } from '@unrdf/streaming';

const processor = createStreamProcessor({
  transform: (event) => {
    // Transform or filter events
    if (event.quad.predicate.value.includes('name')) {
      return { ...event, priority: 'high' };
    }
    return event;
  }
});

const feed = createChangeFeed(store);
feed.pipe(processor);

processor.on('data', (transformed) => {
  console.log('Transformed:', transformed);
});
```

**Version**: 5.0.0 | **Stability**: Stable

---

## Sync Protocol API

### createSyncMessage

Create a sync protocol message for distributed synchronization.

**Signature**:
```javascript
function createSyncMessage(
  delta: Delta,
  options?: SyncMessageOptions
): SyncMessage
```

**Parameters**:

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `delta` | `Object` | Yes | Changes to sync |
| `delta.additions` | `Quad[]` | Yes | Quads to add |
| `delta.removals` | `Quad[]` | Yes | Quads to remove |
| `options.source` | `string` | No | Source identifier |
| `options.timestamp` | `number` | No | Message timestamp |

**Returns**: `SyncMessage` - Sync protocol message

**Example**:
```javascript
import { createSyncMessage } from '@unrdf/streaming';

const message = createSyncMessage({
  additions: [quad1, quad2],
  removals: [quad3]
}, {
  source: 'node-1',
  timestamp: Date.now()
});

console.log('Checksum:', message.checksum);
console.log('Message:', message);
```

**Version**: 5.0.0 | **Stability**: Stable

---

### parseSyncMessage

Parse and validate sync protocol message.

**Signature**:
```javascript
function parseSyncMessage(
  message: string | Object
): SyncMessage
```

**Example**:
```javascript
import { parseSyncMessage } from '@unrdf/streaming';

const received = JSON.parse(networkMessage);
const syncMsg = parseSyncMessage(received);

if (syncMsg.valid) {
  // Apply changes
  applyDelta(store, syncMsg.delta);
}
```

**Version**: 5.0.0 | **Stability**: Stable

---

### calculateChecksum

Calculate checksum for delta verification.

**Signature**:
```javascript
function calculateChecksum(
  delta: Delta
): string
```

**Returns**: `string` - SHA-256 checksum

**Example**:
```javascript
import { calculateChecksum } from '@unrdf/streaming';

const checksum = calculateChecksum({
  additions: [quad1],
  removals: []
});

console.log('Checksum:', checksum);
// "a1b2c3d4..."
```

**Version**: 5.0.0 | **Stability**: Stable

---

### mergeSyncMessages

Merge multiple sync messages into single delta.

**Signature**:
```javascript
function mergeSyncMessages(
  messages: SyncMessage[]
): Delta
```

**Example**:
```javascript
import { mergeSyncMessages } from '@unrdf/streaming';

const merged = mergeSyncMessages([msg1, msg2, msg3]);

console.log('Total additions:', merged.additions.length);
console.log('Total removals:', merged.removals.length);
```

**Version**: 5.0.0 | **Stability**: Stable

---

## Performance Characteristics

### Streaming Parser

| File Size | Memory Usage | Parse Time |
|-----------|--------------|------------|
| 10MB | ~50MB | 0.5s |
| 100MB | ~50MB | 5s |
| 1GB | ~50MB | 50s |

**Constant memory** due to streaming architecture.

### Change Feeds

| Operation | Latency |
|-----------|---------|
| Event emission | <1ms |
| Subscription match | <1ms |
| Network propagation | 10-100ms |

---

## Best Practices

### 1. Use Streaming for Large Files

**Bad** (loads entire file in memory):
```javascript
import { readFileSync } from 'fs';
import { createStore } from '@unrdf/core';

const content = readFileSync('large.ttl', 'utf-8');
const quads = await streamingParse(content); // 1GB in memory!
```

**Good** (streams with constant memory):
```javascript
import { createStreamParser } from '@unrdf/core/rdf/n3-justified-only';
import { createReadStream } from 'fs';

const parser = createStreamParser();
createReadStream('large.ttl').pipe(parser);

parser.on('data', (quad) => {
  processQuad(quad); // Process one at a time
});
```

### 2. Handle Backpressure

```javascript
const parser = createStreamParser();
const writer = createStreamWriter();

parser.on('data', (quad) => {
  const canContinue = writer.write(quad);
  if (!canContinue) {
    parser.pause();
    writer.once('drain', () => parser.resume());
  }
});
```

---

## Related Documentation

- [Core API](/docs/reference/core-rdf-api.md)
- [Store API](/docs/reference/oxigraph-store-api.md)
- [Performance Guide](/docs/guides/performance-optimization.md)

---

**Document Version**: 1.0.0
**Last Updated**: 2025-12-25
**Maintainer**: UNRDF Team
