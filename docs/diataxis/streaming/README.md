# @unrdf/streaming Documentation

Real-time RDF streaming, change feeds, and synchronization across nodes.

## Documentation Structure

This documentation follows the [Diataxis framework](https://diataxis.fr/), organizing content into four categories:

### Tutorials

Learning-oriented guides that take you step by step through building with the streaming package:

- [Subscribe to a Change Feed](./tutorials/01-subscribe-to-change-feed.md)
- [Stream a Large RDF Graph](./tutorials/02-stream-large-rdf-graph.md)

### How-To Guides

Task-oriented guides for specific operations:

- [Filter Change Feed Events](./how-to/01-filter-change-feed-events.md)
- [Handle Backpressure](./how-to/02-handle-backpressure.md)
- [Monitor Stream Performance](./how-to/03-monitor-stream-performance.md)

### Reference

Technical API documentation:

- [Change Feed API](./reference/change-feed-api.md)
- [Performance Monitor API](./reference/performance-monitor-api.md)
- [Stream Protocol](./reference/stream-protocol.md)

### Explanation

Conceptual deep-dives and architecture:

- [Change Feed Architecture](./explanation/01-change-feed-architecture.md)
- [Backpressure and Flow Control](./explanation/02-backpressure-and-flow-control.md)

---

## What is @unrdf/streaming?

The streaming package provides real-time event distribution, large-graph parsing, and cross-node synchronization for RDF knowledge graphs. Its core primitives are:

- **ChangeFeed** — emits validated change events whenever quads are added, removed, or updated
- **SubscriptionManager** — subscribes to a feed with optional quad-pattern filters
- **StreamProcessor** — chainable operators (filter, map, batch, debounce) over a change feed
- **RDFStreamParser** — a Node.js Transform stream that parses Turtle/N-Triples/N-Quads/TriG with built-in backpressure and metrics
- **PerformanceMonitor** — collects throughput, latency, memory, and backpressure metrics during streaming operations
- **Sync Protocol** — creates, parses, merges, and applies SHA-256-checksummed sync messages for distributing changes across nodes

```javascript
import { createChangeFeed, createSubscriptionManager } from '@unrdf/streaming';

const feed = createChangeFeed();
const manager = createSubscriptionManager(feed);

const subId = manager.subscribe(change => {
  console.log(change.type, change.quad.subject.value);
}, {});

feed.emitChange({ type: 'add', quad: { subject, predicate, object } });
```

## Quick Navigation

1. New to streaming? Start with [Subscribe to a Change Feed](./tutorials/01-subscribe-to-change-feed.md)
2. Need a specific task done? Check the [How-To Guides](./how-to/)
3. Looking up an API? See the [Reference](./reference/)
4. Want to understand the design? Read the [Explanation](./explanation/)
