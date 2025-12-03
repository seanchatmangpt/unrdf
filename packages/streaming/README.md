# @unrdf/streaming

**Real-time Change Feeds and Synchronization**

Stream RDF changes in real-time. Subscribe to changes, process deltas, and maintain consistency.

## Installation

```bash
pnpm add @unrdf/streaming
```

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
