# ADR-003: Real-time Streaming System

**Status:** Proposed
**Date:** 2025-11-18
**Deciders:** System Architecture Team
**Technical Story:** Enable real-time RDF stream processing and live subscriptions

## Context and Problem Statement

Modern applications require real-time updates from knowledge graphs - monitoring changes, reacting to events, and processing continuous RDF streams. How do we add streaming capabilities while maintaining the existing Knowledge Hooks architecture and ensuring low latency?

## Decision Drivers

- **Latency**: Sub-10ms stream processing for in-memory windows
- **Throughput**: Handle 1M+ events/second
- **Integration**: Seamless integration with Knowledge Hooks
- **Flexibility**: Support both push (WebSocket) and pull (polling) models
- **Scalability**: Horizontal scaling for distributed streams
- **Reliability**: At-least-once delivery semantics

## Considered Options

### Option 1: External Stream Processor (Kafka, Flink)
- **Pros**: Battle-tested, high throughput, rich ecosystem
- **Cons**: Heavy dependencies, complex setup, deployment overhead

### Option 2: Built-in Lightweight Streaming
- **Pros**: No external dependencies, simple deployment, RDF-native
- **Cons**: Lower throughput than specialized systems

### Option 3: Hybrid Approach (Recommended)
- **Pros**: Lightweight by default, optional external integration
- **Cons**: More code paths, testing complexity

## Decision Outcome

**Chosen option:** Option 3 - Hybrid Approach

Implement a lightweight RDF stream processor with optional adapters for external systems (Kafka, Redis Streams, NATS) for high-scale scenarios.

### Architecture Design

#### Component Structure

```
src/streaming/
├── core/
│   ├── stream-manager.mjs          # Main orchestrator
│   ├── stream-registry.mjs         # Stream catalog
│   ├── event-schema.mjs            # Event schemas (Zod)
│   └── schemas.mjs
│
├── processor/
│   ├── rdf-stream-processor.mjs    # Core stream processor
│   ├── window-manager.mjs          # Windowing operations
│   ├── operators/
│   │   ├── filter.mjs              # Filter operator
│   │   ├── map.mjs                 # Map/transform operator
│   │   ├── reduce.mjs              # Reduce/aggregate operator
│   │   ├── join.mjs                # Stream join operator
│   │   └── window.mjs              # Window operator
│   ├── query-engine.mjs            # Continuous SPARQL queries
│   └── index.mjs
│
├── subscription/
│   ├── subscription-manager.mjs    # Subscription lifecycle
│   ├── subscription-registry.mjs   # Active subscriptions
│   ├── transports/
│   │   ├── websocket-transport.mjs # WebSocket transport
│   │   ├── sse-transport.mjs       # Server-Sent Events
│   │   ├── polling-transport.mjs   # HTTP polling
│   │   └── grpc-transport.mjs      # gRPC streaming
│   └── index.mjs
│
├── events/
│   ├── event-bus.mjs               # In-memory event bus
│   ├── change-feed.mjs             # Change data capture
│   ├── event-store.mjs             # Event persistence
│   └── hook-integration.mjs        # Knowledge Hooks bridge
│
├── adapters/
│   ├── kafka-adapter.mjs           # Apache Kafka
│   ├── redis-adapter.mjs           # Redis Streams
│   ├── nats-adapter.mjs            # NATS JetStream
│   └── mqtt-adapter.mjs            # MQTT (IoT)
│
├── windows/
│   ├── tumbling-window.mjs         # Fixed-size windows
│   ├── sliding-window.mjs          # Overlapping windows
│   ├── session-window.mjs          # Gap-based windows
│   └── count-window.mjs            # Count-based windows
│
└── index.mjs
```

#### Core Interfaces

```javascript
/**
 * @typedef {Object} StreamConfig
 * @property {string} name - Stream identifier
 * @property {'memory'|'kafka'|'redis'|'nats'} backend
 * @property {Object} window - Windowing configuration
 * @property {number} bufferSize - Event buffer size
 */

/**
 * Real-time RDF Stream Manager
 */
export class StreamManager {
  constructor(config = {}) {
    this.config = {
      backend: config.backend || 'memory',
      bufferSize: config.bufferSize || 10000,
      ...config
    };

    this.processor = new RDFStreamProcessor(this.config);
    this.subscriptionManager = new SubscriptionManager();
    this.eventBus = new EventBus();
    this.changeFeed = new ChangeFeed();
  }

  /**
   * Create a new RDF stream
   * @param {string} name - Stream name
   * @param {Object} config - Stream configuration
   * @returns {Promise<Stream>} Stream instance
   */
  async createStream(name, config = {}) {
    const span = tracer.startSpan('streaming.stream.create');

    try {
      span.setAttribute('stream.name', name);
      span.setAttribute('stream.backend', config.backend || this.config.backend);

      const stream = await this.processor.createStream(name, {
        ...this.config,
        ...config
      });

      span.setStatus({ code: SpanStatusCode.OK });
      return stream;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Subscribe to stream events
   * @param {string} streamName - Stream to subscribe to
   * @param {Function} handler - Event handler
   * @param {Object} options - Subscription options
   * @returns {Promise<string>} Subscription ID
   */
  async subscribe(streamName, handler, options = {}) {
    const span = tracer.startSpan('streaming.subscribe');

    try {
      span.setAttribute('stream.name', streamName);
      span.setAttribute('subscription.transport', options.transport || 'memory');

      const subscriptionId = await this.subscriptionManager.subscribe({
        streamName,
        handler,
        filter: options.filter,
        transport: options.transport || 'memory',
        ...options
      });

      span.setAttribute('subscription.id', subscriptionId);
      span.setStatus({ code: SpanStatusCode.OK });

      return subscriptionId;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Publish event to stream
   * @param {string} streamName - Target stream
   * @param {Object} event - Event data
   * @returns {Promise<void>}
   */
  async publish(streamName, event) {
    const span = tracer.startSpan('streaming.publish');

    try {
      span.setAttribute('stream.name', streamName);
      span.setAttribute('event.type', event.type);

      // Validate event schema
      const validated = await this._validateEvent(event);

      // Publish to event bus
      await this.eventBus.publish(streamName, validated);

      span.setStatus({ code: SpanStatusCode.OK });

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Execute continuous SPARQL query
   * @param {string} sparqlQuery - SPARQL query
   * @param {Function} handler - Result handler
   * @returns {Promise<string>} Query ID
   */
  async continuousQuery(sparqlQuery, handler) {
    const span = tracer.startSpan('streaming.continuous_query');

    try {
      const queryId = await this.processor.registerContinuousQuery(
        sparqlQuery,
        handler
      );

      span.setAttribute('query.id', queryId);
      span.setStatus({ code: SpanStatusCode.OK });

      return queryId;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }
}
```

#### RDF Stream Processor

```javascript
/**
 * Core RDF stream processor with windowing support
 */
export class RDFStreamProcessor {
  constructor(config) {
    this.config = config;
    this.streams = new Map();
    this.continuousQueries = new Map();
    this.windowManager = new WindowManager();
  }

  /**
   * Create a new RDF stream
   * @param {string} name - Stream name
   * @param {Object} config - Stream configuration
   * @returns {Promise<Stream>}
   */
  async createStream(name, config) {
    const span = tracer.startSpan('streaming.processor.create_stream');

    try {
      const stream = new RDFStream(name, {
        backend: config.backend || 'memory',
        window: config.window || { type: 'tumbling', size: 1000 },
        bufferSize: config.bufferSize || 10000,
        ...config
      });

      // Initialize stream backend
      await stream.initialize();

      this.streams.set(name, stream);

      span.setAttribute('stream.name', name);
      span.setAttribute('stream.backend', stream.backend);
      span.setStatus({ code: SpanStatusCode.OK });

      return stream;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Register a continuous SPARQL query
   * @param {string} sparqlQuery - SPARQL query
   * @param {Function} handler - Result handler
   * @returns {Promise<string>} Query ID
   */
  async registerContinuousQuery(sparqlQuery, handler) {
    const queryId = crypto.randomUUID();

    const query = {
      id: queryId,
      sparql: sparqlQuery,
      handler,
      lastExecution: 0,
      resultSet: new Set()
    };

    this.continuousQueries.set(queryId, query);

    // Start continuous evaluation
    this._startContinuousEvaluation(query);

    return queryId;
  }

  async _startContinuousEvaluation(query) {
    // Evaluate query on each window close
    this.windowManager.on('window:close', async (window) => {
      const span = tracer.startSpan('streaming.continuous_query.evaluate');

      try {
        // Create temporary store from window data
        const store = this._createStoreFromWindow(window);

        // Execute SPARQL query
        const results = await this._executeSparql(store, query.sparql);

        // Detect changes from last execution
        const changes = this._detectChanges(query.resultSet, results);

        if (changes.length > 0) {
          // Invoke handler with changes
          await query.handler(changes);

          // Update result set
          query.resultSet = new Set(results.map(r => JSON.stringify(r)));
        }

        span.setStatus({ code: SpanStatusCode.OK });

      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR });
      } finally {
        span.end();
      }
    });
  }
}
```

#### Window Manager

```javascript
/**
 * Manages different windowing strategies for stream processing
 */
export class WindowManager {
  constructor() {
    this.windows = new Map();
    this.eventEmitter = new EventEmitter();
  }

  /**
   * Create a tumbling window (fixed-size, non-overlapping)
   * @param {string} streamName - Stream name
   * @param {Object} config - Window configuration
   * @returns {TumblingWindow}
   */
  createTumblingWindow(streamName, config) {
    const window = new TumblingWindow({
      size: config.size || 1000,  // Events per window
      timespan: config.timespan,  // Or time-based (ms)
      evictionPolicy: config.evictionPolicy || 'count'
    });

    window.on('close', (data) => {
      this.eventEmitter.emit('window:close', {
        stream: streamName,
        type: 'tumbling',
        data
      });
    });

    this.windows.set(`${streamName}:tumbling`, window);
    return window;
  }

  /**
   * Create a sliding window (overlapping windows)
   * @param {string} streamName - Stream name
   * @param {Object} config - Window configuration
   * @returns {SlidingWindow}
   */
  createSlidingWindow(streamName, config) {
    const window = new SlidingWindow({
      size: config.size || 1000,
      slide: config.slide || 100,  // Window slides by this amount
      timespan: config.timespan,
      slideTime: config.slideTime
    });

    window.on('slide', (data) => {
      this.eventEmitter.emit('window:slide', {
        stream: streamName,
        type: 'sliding',
        data
      });
    });

    this.windows.set(`${streamName}:sliding`, window);
    return window;
  }

  /**
   * Create a session window (gap-based)
   * @param {string} streamName - Stream name
   * @param {Object} config - Window configuration
   * @returns {SessionWindow}
   */
  createSessionWindow(streamName, config) {
    const window = new SessionWindow({
      gap: config.gap || 5000,  // Close window after 5s of inactivity
      timeout: config.timeout || 300000  // Max session duration: 5 minutes
    });

    window.on('close', (data) => {
      this.eventEmitter.emit('window:close', {
        stream: streamName,
        type: 'session',
        data
      });
    });

    this.windows.set(`${streamName}:session`, window);
    return window;
  }

  on(event, handler) {
    this.eventEmitter.on(event, handler);
  }
}
```

#### Change Feed Integration

```javascript
/**
 * Change Data Capture for RDF stores
 * Publishes all store mutations as stream events
 */
export class ChangeFeed {
  constructor(store, streamManager) {
    this.store = store;
    this.streamManager = streamManager;
    this.changeStream = null;
  }

  /**
   * Start capturing changes
   * @returns {Promise<void>}
   */
  async start() {
    const span = tracer.startSpan('streaming.change_feed.start');

    try {
      // Create change stream
      this.changeStream = await this.streamManager.createStream('store:changes', {
        backend: 'memory',
        window: { type: 'tumbling', size: 100 }
      });

      // Hook into store operations
      this._instrumentStore();

      span.setStatus({ code: SpanStatusCode.OK });

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  _instrumentStore() {
    // Wrap store.add
    const originalAdd = this.store.add.bind(this.store);
    this.store.add = (quad) => {
      const result = originalAdd(quad);

      // Publish change event
      this.streamManager.publish('store:changes', {
        type: 'quad:added',
        quad: this._serializeQuad(quad),
        timestamp: Date.now()
      });

      return result;
    };

    // Wrap store.delete
    const originalDelete = this.store.delete.bind(this.store);
    this.store.delete = (quad) => {
      const result = originalDelete(quad);

      // Publish change event
      this.streamManager.publish('store:changes', {
        type: 'quad:deleted',
        quad: this._serializeQuad(quad),
        timestamp: Date.now()
      });

      return result;
    };
  }

  _serializeQuad(quad) {
    return {
      subject: quad.subject.value,
      predicate: quad.predicate.value,
      object: quad.object.value,
      graph: quad.graph?.value
    };
  }
}
```

### Integration with Knowledge Hooks

```javascript
// Streaming hooks
defineHook('stream.event.received', {
  phase: 'pre',
  condition: async ({ event }) => event.type === 'quad:added',
  effect: async ({ event }) => {
    // Validate new quad before adding to stream
    await validator.validate(event.quad);
  }
});

defineHook('stream.window.closed', {
  phase: 'post',
  condition: async ({ window }) => window.data.length > 100,
  effect: async ({ window }) => {
    // Analyze large windows for patterns
    await patternAnalyzer.analyze(window.data);
  }
});

defineHook('stream.subscription.created', {
  phase: 'post',
  effect: async ({ subscription }) => {
    // Log new subscription
    await logger.info(`New subscription: ${subscription.id}`);
  }
});
```

### Usage Examples

```javascript
// Example 1: Real-time change feed
import { StreamManager, ChangeFeed } from 'unrdf/streaming';
import { useGraph } from 'unrdf';

const { store } = useGraph();
const streamManager = new StreamManager();

// Start change feed
const changeFeed = new ChangeFeed(store, streamManager);
await changeFeed.start();

// Subscribe to changes
await streamManager.subscribe('store:changes', (event) => {
  console.log(`Change detected: ${event.type}`, event.quad);
});

// Example 2: Continuous SPARQL query
const queryId = await streamManager.continuousQuery(
  `
  SELECT ?person ?action WHERE {
    ?person <http://example.org/action> ?action .
    FILTER (?action = "login")
  }
  `,
  (results) => {
    console.log('New login events:', results);
  }
);

// Example 3: Windowed aggregation
const stream = await streamManager.createStream('sensor-data', {
  window: { type: 'tumbling', timespan: 60000 }  // 1-minute windows
});

streamManager.subscribe('sensor-data', async (window) => {
  // Compute average temperature per minute
  const avgTemp = window.data.reduce((sum, event) => {
    return sum + event.temperature;
  }, 0) / window.data.length;

  console.log(`Average temperature (last minute): ${avgTemp}°C`);
});

// Example 4: WebSocket subscription
import { createServer } from 'http';
import { WebSocketTransport } from 'unrdf/streaming';

const server = createServer();
const wsTransport = new WebSocketTransport({ server });

await streamManager.subscribe('store:changes', (event) => {
  // Broadcast to all connected clients
  wsTransport.broadcast(event);
}, { transport: 'websocket' });
```

## Consequences

### Positive

- **Real-time Updates**: Instant notification of graph changes
- **Scalability**: Handle high-throughput event streams
- **Flexibility**: Multiple windowing and transport options
- **Integration**: Seamless integration with Knowledge Hooks
- **Standard Support**: SPARQL-based continuous queries

### Negative

- **Complexity**: Stream processing adds architectural complexity
- **Resource Usage**: In-memory windows consume memory
- **Event Ordering**: Distributed streams may have ordering issues
- **Latency Variance**: Network-based transports add latency

## Performance Targets

| Operation | Target |
|-----------|--------|
| In-memory Event Processing | <10ms |
| WebSocket Event Delivery | <50ms |
| Window Aggregation (1k events) | <100ms |
| Continuous Query Evaluation | <200ms |
| Throughput (in-memory) | 1M events/sec |
| Throughput (Kafka) | 100k events/sec |

## References

- [RDF Stream Processing](https://www.w3.org/community/rsp/)
- [Apache Kafka Streams](https://kafka.apache.org/documentation/streams/)
- [Reactive Streams Specification](https://www.reactive-streams.org/)
