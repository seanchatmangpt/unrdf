# @unrdf/streaming - Quick Start Guide

**80/20 Guide**: Get production-grade RDF change feeds and real-time validation running in 5 minutes.

## Prerequisites

- Node.js 18+
- Basic knowledge of RDF (Quads/Triples)

## One-Command Demo

```bash
node examples/production-change-feed.mjs
```

**What it does:**
1. ✅ Initializes an RDF Store (Oxigraph)
2. ✅ Configures a real-time SHACL Validator
3. ✅ Subscribes to changes via a Change Feed
4. ✅ Demonstrates valid data ingestion
5. ✅ Catches and reports SHACL violations in real-time
6. ✅ Shows observability metrics for the streaming pipeline

**Expected output:**
```
--- Starting Production Change Feed Demo ---

Adding valid person...
[Feed] ADD: http://example.org/alice http://www.w3.org/1999/02/22-rdf-syntax-ns#type http://xmlns.com/foaf/0.1/Person
[Feed] ADD: http://example.org/alice http://xmlns.com/foaf/0.1/name Alice
[Validator] Data conforms to shapes.

Adding invalid person (missing name)...
[Feed] ADD: http://example.org/bob http://www.w3.org/1999/02/22-rdf-syntax-ns#type http://xmlns.com/foaf/0.1/Person
[Validator] SHACL Violation: Less than 1 values on foaf:name

--- Demo Metrics ---
{
  "validationsPerformed": 3,
  "violationsDetected": 1,
  "cacheHits": 0,
  "cacheMisses": 3,
  "errorCount": 0,
  "avgLatency": 1.2,
  "p95Latency": 2.5,
  "cacheHitRate": 0
}

Demo complete.
```

## Manual Setup (Step-by-Step)

### 1. Install Dependencies

```bash
pnpm add @unrdf/streaming @unrdf/oxigraph
```

### 2. Initialize your Store

```javascript
import { createStore } from '@unrdf/oxigraph';
const store = createStore();
```

### 3. Create a Change Feed

```javascript
import { createChangeFeed } from '@unrdf/streaming';

const feed = createChangeFeed(store);
feed.subscribe((change) => {
  console.log(`Change detected: ${change.type} on ${change.quad.subject.value}`);
});
```

### 4. Add Real-time Validation

```javascript
import { createRealTimeValidator, ValidationMode } from '@unrdf/streaming';

const shapes = `... (your SHACL shapes in Turtle) ...`;
const validator = createRealTimeValidator({
  shapes,
  mode: ValidationMode.DELTA
});

// Hook validator into feed
feed.subscribe(async (change) => {
  if (change.type === 'add') {
    const result = await validator.validateDelta({ additions: [change.quad] }, store);
    if (!result.conforms) {
      console.error('Invalid data added!');
    }
  }
});
```

## Core Concepts

### Change Feed
The `Change Feed` is an EventTarget-based emitter that wraps your RDF store. It tracks every `add` and `remove` operation, allowing you to react to changes as they happen.

### Real-time Validator
Unlike traditional validators that check the entire database, the `RealTimeValidator` optimizes for performance by checking only the "delta" (what changed). It supports three modes:
- `FULL`: Validates the entire store on every change.
- `INCREMENTAL`: Validates only the affected subgraphs.
- `DELTA`: Validates only the new quads in isolation.

### Sync Protocol
The Sync Protocol allows two stores to stay in sync over a network (e.g., via WebSockets). It handles serialization, checksums, and merging of changes.

## Architecture

```
RDF Store (Oxigraph)
      │
      ▼
┌───────────────┐
│  Change Feed  │───► Subscribers (UI, Loggers, Sync)
└───────────────┘
      │
      ▼
┌───────────────────┐
│ Real-Time Validator│───► Rejects invalid transactions
└───────────────────┘
      │
      ▼
┌──────────────────────┐
│ Observability Manager │───► Metrics & Traces (OTel)
└──────────────────────┘
```

## Troubleshooting

### "Validator is slow"
- **Solution**: Ensure you are using `ValidationMode.DELTA` or `INCREMENTAL`. `FULL` validation is expensive on large datasets.
- **Solution**: Check if caching is enabled (`enableCaching: true`).

### "Changes not being detected"
- **Solution**: Ensure you passed the `store` instance to `createChangeFeed(store)`. The feed needs to wrap the store's methods.
- **Solution**: If using a custom store, ensure it implements `addQuad` and `removeQuad`.

### "Memory usage is high"
- **Solution**: Adjust `maxHistorySize` in `createChangeFeed`. By default, it keeps the last 10,000 changes in memory.

## Production Checklist

- [ ] Use `createObservabilityManager` to track latency and error rates.
- [ ] Configure `ValidationMode.INCREMENTAL` for complex SHACL constraints.
- [ ] Implement backpressure if consuming streams at high velocity.
- [ ] Use `destroy()` on feeds and `cleanup()` on validators to prevent memory leaks.
- [ ] Secure your WebSocket sync endpoints with authentication.

## Support

- Issues: https://github.com/unrdf/unrdf/issues
- Main README: `README.md`
- Full API Docs: `docs/API.md`
