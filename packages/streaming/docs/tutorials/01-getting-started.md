# Your First Stream: Processing RDF Data

**Time estimate:** 6-8 hours (if you code along)
**Difficulty:** Intermediate
**Prerequisites:** Understand @unrdf/core basics
**What you'll learn:** Stream large RDF files without memory overhead

---

## What You'll Do

In this tutorial, you'll:
1. Create a streaming parser for large RDF files
2. Process quads one at a time instead of loading all
3. Handle backpressure (producer faster than consumer)
4. Build a real pipeline from file → parser → store → output

By the end, you'll process multi-gigabyte RDF files in constant memory.

---

## Part 1: The Problem

### Traditional Approach (Doesn't Scale)

```javascript
import fs from 'fs';
import { createUnrdfStore } from '@unrdf/core';

// ❌ PROBLEM: Load entire file into memory first
const data = fs.readFileSync('huge-1gb.ttl', 'utf-8');
const store = createUnrdfStore();

// Parsing 1GB file into quads
// Could use 5-10GB RAM temporarily!
// Machine crashes if data > available RAM
```

### Streaming Approach (Scalable)

```javascript
import { createReadStream } from 'fs';
import { Readable } from 'stream';

// ✅ SOLUTION: Read file in chunks, process each chunk
const stream = createReadStream('huge-1gb.ttl');
stream.on('data', (chunk) => {
  // Process small chunk (e.g., 64KB)
  // Each chunk individually parsed
  // Constant memory usage!
});
```

---

## Part 2: Streaming Basics

### Create a File Stream

```javascript
import { createReadStream } from 'fs';

const stream = createReadStream('data.ttl', {
  encoding: 'utf-8',
  highWaterMark: 64 * 1024  // 64KB chunks
});

// Listen for data events
stream.on('data', (chunk) => {
  console.log(`Got ${chunk.length} bytes`);
});

stream.on('end', () => {
  console.log('File finished');
});

stream.on('error', (error) => {
  console.error('Read error:', error);
});
```

**Key points:**
- Stream emits `'data'` events with chunks
- `highWaterMark` = chunk size (64KB default)
- `'end'` fires when file done
- `'error'` for problems

---

## Part 3: Parse RDF from Stream

### Line-by-Line Processing (N-Triples)

```javascript
import { createReadStream } from 'fs';
import { createUnrdfStore } from '@unrdf/core';

const store = createUnrdfStore();
let buffer = '';

createReadStream('data.nt')
  .on('data', (chunk) => {
    // Add to buffer
    buffer += chunk;

    // Process complete lines
    const lines = buffer.split('\n');
    buffer = lines.pop();  // Keep incomplete line

    lines.forEach(line => {
      if (!line.trim()) return;

      // Parse N-Triples line
      const quad = parseNTriplesLine(line);
      if (quad) {
        store.addQuad(quad);
      }
    });

    console.log(`Loaded ${store.size} quads so far...`);
  })
  .on('end', () => {
    // Process final line
    if (buffer.trim()) {
      const quad = parseNTriplesLine(buffer);
      if (quad) store.addQuad(quad);
    }

    console.log(`Total: ${store.size} quads`);
  });

function parseNTriplesLine(line) {
  // Simple N-Triples parser
  const match = line.match(/^<(.+?)>\s+<(.+?)>\s+(.+?)\s+\.$/);
  if (!match) return null;

  const [, subject, predicate, objectStr] = match;

  return {
    subject: { type: 'NamedNode', value: subject },
    predicate: { type: 'NamedNode', value: predicate },
    object: parseObject(objectStr)
  };
}

function parseObject(str) {
  if (str.startsWith('<') && str.endsWith('>')) {
    return { type: 'NamedNode', value: str.slice(1, -1) };
  }
  if (str.startsWith('"')) {
    return { type: 'Literal', value: str.slice(1, -1) };
  }
  return null;
}
```

---

## Part 4: Memory-Efficient Pipeline

### Monitor Memory Usage

```javascript
function monitorStream(stream, store) {
  let processed = 0;

  stream.on('data', (chunk) => {
    processed += chunk.length;
    const mem = process.memoryUsage();

    const mbUsed = (mem.heapUsed / 1024 / 1024).toFixed(2);
    const mbTotal = (mem.heapTotal / 1024 / 1024).toFixed(2);

    console.log(`
      Processed: ${(processed / 1024 / 1024).toFixed(1)} MB
      Heap: ${mbUsed} / ${mbTotal} MB
      Quads: ${store.size}
    `);
  });
}
```

### Real-World Example

```javascript
import { createReadStream } from 'fs';
import { createUnrdfStore, namedNode, literal } from '@unrdf/core';

const store = createUnrdfStore({
  indexing: 'predicate',
  maxSize: Infinity
});

let buffer = '';
let quadsAdded = 0;

const stream = createReadStream('1gb-dataset.nt');

stream
  .on('data', (chunk) => {
    buffer += chunk.toString();

    // Split by newlines
    const lines = buffer.split('\n');
    buffer = lines.pop();  // Keep incomplete line

    // Process each complete line
    lines.forEach(line => {
      if (!line.trim()) return;

      const quad = parseNTriplesLine(line);
      if (quad) {
        store.addQuad(quad);
        quadsAdded++;

        // Log progress every 100k quads
        if (quadsAdded % 100000 === 0) {
          const mem = (process.memoryUsage().heapUsed / 1024 / 1024).toFixed(1);
          console.log(`Loaded ${quadsAdded} quads (${mem} MB)`);
        }
      }
    });
  })
  .on('end', () => {
    // Final line
    if (buffer.trim()) {
      const quad = parseNTriplesLine(buffer);
      if (quad) store.addQuad(quad);
    }

    console.log(`Done! Total: ${store.size} quads`);
    console.log(`Memory: ${(process.memoryUsage().heapUsed / 1024 / 1024).toFixed(1)} MB`);
  })
  .on('error', (error) => {
    console.error('Stream error:', error);
  });
```

---

## Summary

You've learned:
- ✅ Streaming vs loading entire files
- ✅ Process RDF line-by-line
- ✅ Monitor memory during parsing
- ✅ Build streaming pipelines
- ✅ Handle multi-gigabyte files efficiently

### Key takeaway: Stream = Constant Memory

```
Traditional: 1GB file → 5GB RAM spike → done
Streaming: 1GB file → 50MB RAM constant → done
```

---

## Next Reading

- **02-backpressure** (Tutorial) - Handle producer faster than consumer
- **03-real-time-sync** (Tutorial) - Continuous RDF data streams
- **optimize-memory** (How-To) - Further memory optimizations
- **API.md** (Reference) - Streaming function reference
