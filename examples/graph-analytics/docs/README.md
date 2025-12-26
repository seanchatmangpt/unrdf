# Real-Time Graph Analytics

Real-time analytics engine for RDF graphs with pattern detection and visualization.

## Features

- **Live graph metrics**: Real-time statistics on triples, entities, predicates
- **Pattern detection**: Identify hubs, chains, cycles, and communities
- **Anomaly alerts**: Detect unusual graph changes
- **Interactive visualization**: WebSocket-based dashboard

## Quick Start

```bash
pnpm install
node src/analytics-engine.mjs
```

## Usage

```javascript
import { AnalyticsEngine } from './src/analytics-engine.mjs';
import { createStore } from '@unrdf/oxigraph';

const store = createStore();
const analytics = new AnalyticsEngine({ store });

// Start analytics
await analytics.start();

// Subscribe to events
analytics.subscribe((event) => {
  if (event.type === 'metrics') {
    console.log('Total triples:', event.metrics.triples.total);
    console.log('Unique entities:', event.metrics.entities.total);
  }

  if (event.type === 'patterns') {
    console.log('Detected patterns:', event.patterns);
  }

  if (event.type === 'anomaly') {
    console.log('Anomaly detected:', event.anomaly);
  }
});

// Add data to store (analytics will detect changes)
await store.load('./data/graph.ttl', 'turtle');

// Get current metrics
const metrics = analytics.getMetrics();
console.log(metrics);
```

## Visualization Dashboard

```bash
# Start visualizer
node src/visualizer.mjs

# Connect via WebSocket
ws://localhost:4000
```

## Docker

```bash
docker build -t graph-analytics .
docker run -p 4000:4000 graph-analytics
```

## Testing

```bash
pnpm test
pnpm test:coverage
```

## Pattern Detection

Detects:
- **Hub nodes**: Nodes with high degree (>10 edges)
- **Chains**: Linear sequences of connected nodes
- **Cycles**: Circular dependencies
- **Communities**: Dense subgraphs

## Performance

- Metrics update: Every 1 second
- Pattern detection: Every 10 seconds
- Anomaly detection: Real-time
- Dashboard latency: <50ms
