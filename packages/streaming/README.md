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

**Need real-time updates?** Start with [QUICKSTART-STREAMING.md](./QUICKSTART-STREAMING.md).

## 🚀 Quick Start

Get started in 5 minutes with our **[Quick Start Guide](./QUICKSTART-STREAMING.md)**.

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
Ensure the `store` you are monitoring is passed to `createChangeFeed` *before* you start making changes. The feed monkey-patches the store's mutation methods to capture events.

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

### SHACL Validation (`validate.mjs`)

Validate RDF data against SHACL shapes during streaming operations:

```javascript
import { validateShacl, validateQuad } from '@unrdf/streaming/validate'

// Validate entire store against SHACL shapes
const result = await validateShacl(dataStore, shapesStore, {
  strict: true,
  maxViolations: 10
})

console.log(`Conforms: ${result.conforms}`)
console.log(`Violations: ${result.results.length}`)

// Validate individual quad
const quadResult = validateQuad(quad, shapesStore)
if (!quadResult.valid) {
  console.error('Validation failed:', quadResult.violations)
}
```

**Options:**
- `strict` (boolean): Throw on first violation (default: false)
- `maxViolations` (number): Maximum violations to collect (default: 100)

**Returns:**
- `conforms` (boolean): Whether data conforms to shapes
- `results` (array): Array of validation violations
- `warnings` (array): Non-critical validation warnings
- `timestamp` (number): Validation timestamp

### Observability (`observability.mjs`)

Built-in OpenTelemetry instrumentation for monitoring streaming operations:

```javascript
import { createObservabilityManager } from '@unrdf/streaming/observability'

const obs = createObservabilityManager({
  serviceName: 'my-streaming-service',
  version: '[VERSION]'
})

// Record operations
obs.recordOperation('stream_started', { subject: 'ex:Alice' })
obs.recordError('validation_failed', error, { context: 'streaming' })

// Use spans for tracing
await obs.withSpan('process-changes', async (span) => {
  // Your streaming logic here
  span.setAttribute('quad.count', changes.length)
})
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
