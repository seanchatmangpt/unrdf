# @unrdf/streaming

![Version](https://img.shields.io/badge/version-5.0.0--beta.1-blue) ![Production Ready](https://img.shields.io/badge/production-ready-green)


**Real-time Change Feeds and Synchronization**

Stream RDF changes in real-time. Subscribe to changes, process deltas, and maintain consistency.

## Installation

```bash
pnpm add @unrdf/streaming
```

## 📚 Examples

See these examples that demonstrate @unrdf/streaming:

- **[streaming/basic-stream.mjs](../../examples/streaming/basic-stream.mjs)** - Real-time change feeds basics
- **[streaming/advanced-filters.mjs](../../examples/streaming/advanced-filters.mjs)** - Stream filtering and transformation
- **[comprehensive-feature-test.mjs](../../examples/comprehensive-feature-test.mjs)** - Streaming integration

**Need real-time updates?** Start with [streaming/basic-stream.mjs](../../examples/streaming/basic-stream.mjs).

## Quick Start

```javascript
import { subscribeToChanges } from '@unrdf/streaming'

// Subscribe to all changes
const unsubscribe = subscribeToChanges(store, (change) => {
  console.log('Change:', change.quad)
  console.log('Type:', change.type) // 'add' or 'remove'
})

// Unsubscribe when done
unsubscribe()
```

## Features

- ✅ Real-time change subscriptions
- ✅ Guaranteed delivery semantics
- ✅ Delta computation and streaming
- ✅ Change filtering (by subject, predicate, etc)
- ✅ Batch operations
- ✅ WebSocket support

## Use Cases

- **Real-time UI updates**: Push RDF changes to clients
- **Data synchronization**: Keep distributed stores in sync
- **Audit trails**: Track all changes to RDF graphs
- **Change streams**: Process changes downstream
- **Reactive applications**: Build reactive RDF apps

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
