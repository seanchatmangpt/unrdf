# Real-time Streaming and WebSocket Subscriptions for UNRDF

This module provides real-time streaming capabilities for RDF knowledge graphs, including WebSocket subscriptions, change feeds, stream processing, and incremental SHACL validation.

## Features

- **WebSocket Subscription Manager**: Real-time subscriptions to graph changes with automatic reconnection
- **Change Feed**: Capture and broadcast RDF graph changes with history tracking
- **Stream Processor**: Process RDF streams with windowing operations (tumbling, sliding, session, count)
- **Real-time Validator**: Incremental SHACL validation on streaming updates
- **OTEL Observability**: Comprehensive tracing and metrics for all operations
- **Performance**: <100ms p95 latency for change notifications

## Installation

The streaming module is included in the main UNRDF package:

```bash
pnpm add unrdf
```

The WebSocket library is included as a dependency:

```bash
pnpm add ws  # Automatically included
```

## Quick Start

### Basic Subscription

```javascript
import { createSubscriptionManager, SubscriptionPatternType } from 'unrdf/streaming';

// Create subscription manager
const manager = createSubscriptionManager({
  url: 'ws://localhost:8080/graph-updates'
});

// Connect to WebSocket server
await manager.connect();

// Subscribe to property changes
const subId = manager.subscribe({
  pattern: SubscriptionPatternType.PROPERTY_CHANGE,
  subject: 'http://example.org/alice',
  predicate: 'http://example.org/age'
}, (data) => {
  console.log('Property changed:', data);
});
```

### Change Feed Integration

```javascript
import { createChangeFeed, createChangeFeedHook } from 'unrdf/streaming';
import { TransactionManager } from 'unrdf';

// Create change feed
const feed = createChangeFeed({
  enableHistory: true,
  historySize: 1000
});

feed.start();

// Listen for changes
feed.on('change', (change) => {
  console.log('Change:', change.type, change.delta);
});

// Integrate with transactions
const txManager = new TransactionManager();
const hook = createChangeFeedHook(feed);
txManager.addHook(hook);
```

### Stream Processing

```javascript
import { createStreamProcessor, WindowType, Aggregators } from 'unrdf/streaming';

// Create processor with windowing
const processor = createStreamProcessor({
  enableWindowing: true,
  enableAggregation: true
});

// Configure tumbling window
processor.configureWindowing({
  type: WindowType.TUMBLING,
  size: 5000  // 5 second windows
});

// Register aggregators
processor.registerAggregator('count', Aggregators.count);
processor.registerAggregator('avg', Aggregators.avg('value'));

processor.start();

// Process events
await processor.process({
  id: 'evt-1',
  value: 42,
  timestamp: Date.now()
});
```

### Real-time Validation

```javascript
import { createRealTimeValidator, ValidationMode } from 'unrdf/streaming';

// Create validator with SHACL shapes
const validator = createRealTimeValidator({
  mode: ValidationMode.DELTA,
  shapes: shaclShapesTurtle,
  enableCaching: true
});

// Listen for violations
validator.on('violation', (result) => {
  console.error('Validation failed:', result.violations);
});

// Validate delta
const result = await validator.validateDelta(delta);

// Integrate with transactions
const txManager = new TransactionManager();
const hook = validator.createValidationHook();
txManager.addHook(hook);
```

### Complete Pipeline

```javascript
import { createStreamingPipeline } from 'unrdf/streaming';

// Create integrated pipeline
const pipeline = createStreamingPipeline({
  subscriptionManager: {
    url: 'ws://localhost:8080/graph-updates'
  },
  changeFeed: {
    enableHistory: true,
    historySize: 1000
  },
  streamProcessor: {
    enableWindowing: true,
    enableAggregation: true
  },
  validator: {
    mode: ValidationMode.DELTA,
    shapes: shaclShapes
  }
});

// Configure windowing
pipeline.streamProcessor.configureWindowing({
  type: WindowType.COUNT,
  size: 100,
  count: 10
});

// Start pipeline
pipeline.start();

// Get metrics
const metrics = pipeline.getMetrics();
console.log('Pipeline metrics:', metrics);
```

## API Reference

### SubscriptionManager

#### Methods

- `connect(url?)`: Connect to WebSocket server
- `disconnect()`: Disconnect from server
- `subscribe(config, callback?)`: Create a subscription
- `unsubscribe(id)`: Remove a subscription
- `unsubscribeAll()`: Remove all subscriptions
- `getSubscription(id)`: Get subscription by ID
- `getAllSubscriptions()`: Get all subscriptions
- `getMetrics()`: Get performance metrics

#### Events

- `connected`: WebSocket connection established
- `disconnected`: WebSocket connection closed
- `change`: Change notification received
- `error`: Error occurred
- `max-reconnects`: Maximum reconnection attempts reached

### ChangeFeed

#### Methods

- `start()`: Start the change feed
- `stop()`: Stop the change feed
- `recordChange(delta, metadata?)`: Record a change event
- `getHistory(options?)`: Get change history
- `getChange(id)`: Get change by ID
- `clearHistory()`: Clear history
- `getMetrics()`: Get performance metrics

#### Events

- `started`: Feed started
- `stopped`: Feed stopped
- `change`: Single change event
- `batch`: Batch of changes (in batch mode)

### StreamProcessor

#### Methods

- `configureWindowing(config)`: Configure windowing
- `start()`: Start processing
- `stop()`: Stop processing
- `process(event)`: Process a single event
- `processBatch(events)`: Process multiple events
- `registerAggregator(name, fn)`: Register aggregator function
- `unregisterAggregator(name)`: Remove aggregator
- `getWindow(id)`: Get window by ID
- `getAllWindows()`: Get all windows
- `getActiveWindow()`: Get active window
- `getMetrics()`: Get performance metrics

#### Events

- `started`: Processor started
- `stopped`: Processor stopped
- `processed`: Event processed
- `window-created`: New window created
- `window-closed`: Window closed

### RealTimeValidator

#### Methods

- `validateDelta(delta, store?)`: Validate a delta change
- `validateDebounced(delta, store?)`: Validate with debouncing
- `createValidationHook(options?)`: Create transaction hook
- `clearCache()`: Clear validation cache
- `getMetrics()`: Get performance metrics

#### Events

- `validated`: Validation completed
- `violation`: Validation violation detected

## Subscription Patterns

### SPARQL SELECT

Subscribe to results of a SPARQL SELECT query:

```javascript
manager.subscribe({
  pattern: SubscriptionPatternType.SPARQL_SELECT,
  query: 'SELECT * WHERE { ?s ?p ?o }'
});
```

### Property Change

Subscribe to changes in a specific property:

```javascript
manager.subscribe({
  pattern: SubscriptionPatternType.PROPERTY_CHANGE,
  subject: 'http://example.org/alice',
  predicate: 'http://example.org/age'
});
```

### Entity Update

Subscribe to all changes for an entity:

```javascript
manager.subscribe({
  pattern: SubscriptionPatternType.ENTITY_UPDATE,
  subject: 'http://example.org/alice'
});
```

### Wildcard

Subscribe to all changes:

```javascript
manager.subscribe({
  pattern: SubscriptionPatternType.WILDCARD
});
```

## Window Types

### Tumbling Window

Fixed-size, non-overlapping windows:

```javascript
processor.configureWindowing({
  type: WindowType.TUMBLING,
  size: 5000  // 5 seconds
});
```

### Sliding Window

Overlapping windows with configurable slide interval:

```javascript
processor.configureWindowing({
  type: WindowType.SLIDING,
  size: 10000,  // 10 seconds
  slide: 2000   // Slide every 2 seconds
});
```

### Session Window

Windows based on inactivity timeout:

```javascript
processor.configureWindowing({
  type: WindowType.SESSION,
  size: 5000,
  timeout: 10000  // Close after 10s of inactivity
});
```

### Count Window

Windows based on event count:

```javascript
processor.configureWindowing({
  type: WindowType.COUNT,
  size: 100,
  count: 10  // Window closes after 10 events
});
```

## Aggregators

Built-in aggregators:

- `Aggregators.count`: Count events
- `Aggregators.sum(field)`: Sum numeric values
- `Aggregators.avg(field)`: Average numeric values
- `Aggregators.min(field)`: Find minimum value
- `Aggregators.max(field)`: Find maximum value
- `Aggregators.groupBy(field)`: Group events by field

Custom aggregators:

```javascript
processor.registerAggregator('custom', (events, window) => {
  // Custom aggregation logic
  return events.reduce((acc, event) => acc + event.value, 0);
});
```

## Validation Modes

### DELTA Mode

Validate only the changes in the delta (fastest):

```javascript
const validator = createRealTimeValidator({
  mode: ValidationMode.DELTA,
  shapes
});
```

### INCREMENTAL Mode

Validate only affected subgraphs (balanced):

```javascript
const validator = createRealTimeValidator({
  mode: ValidationMode.INCREMENTAL,
  shapes
});
```

### FULL Mode

Validate entire store (most thorough):

```javascript
const validator = createRealTimeValidator({
  mode: ValidationMode.FULL,
  shapes
});
```

## Performance

The streaming module is designed for high performance:

- **Latency**: <100ms p95 for change notifications
- **Throughput**: Handles 1000+ events/second
- **Caching**: Intelligent validation caching
- **Debouncing**: Configurable debouncing for batching
- **Observability**: Full OTEL tracing and metrics

### Performance Tips

1. Use DELTA validation mode for best performance
2. Enable caching for validators
3. Use batch mode for high-frequency changes
4. Configure appropriate window sizes
5. Use debouncing for rapid updates

## Observability

All streaming modules include comprehensive OTEL observability:

```javascript
const manager = createSubscriptionManager({
  observability: {
    serviceName: 'my-streaming-service',
    enableTracing: true,
    enableMetrics: true,
    endpoint: 'http://localhost:4318'
  }
});
```

Metrics tracked:

- Subscription count
- Messages received/sent
- Latency (average, p95, p99)
- Cache hit rate
- Validation performance
- Window statistics

## Examples

See the `/examples/streaming/` directory for complete examples:

- `basic-subscription.mjs`: Basic WebSocket subscription
- `change-feed-integration.mjs`: Change feed with transactions
- `stream-processing-pipeline.mjs`: Complete streaming pipeline
- `real-time-validation.mjs`: Real-time SHACL validation

## Integration with Knowledge Hooks

The streaming module integrates seamlessly with UNRDF's Knowledge Hooks:

```javascript
import { KnowledgeHookManager } from 'unrdf';
import { createChangeFeed, createChangeFeedHook } from 'unrdf/streaming';

const manager = new KnowledgeHookManager();
const feed = createChangeFeed();

// Add change feed hook
const hook = createChangeFeedHook(feed);
manager.addHook(hook);

// Changes will now be tracked automatically
```

## Browser Support

The streaming module works in both Node.js and browser environments:

```javascript
// Node.js - uses 'ws' package
import { createSubscriptionManager } from 'unrdf/streaming';

// Browser - uses native WebSocket
import { createSubscriptionManager } from 'unrdf/streaming';
```

## Testing

Run the streaming tests:

```bash
pnpm test test/streaming/
```

Run specific test suites:

```bash
pnpm vitest test/streaming/subscription-manager.test.mjs
pnpm vitest test/streaming/change-feed.test.mjs
pnpm vitest test/streaming/stream-processor.test.mjs
pnpm vitest test/streaming/real-time-validator.test.mjs
```

## License

MIT
