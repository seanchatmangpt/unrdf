# @unrdf/streaming

![Version](https://img.shields.io/badge/version-6.0.0--rc.3-blue) ![Production Ready](https://img.shields.io/badge/production-ready-green)


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
  version: '1.0.0'
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
