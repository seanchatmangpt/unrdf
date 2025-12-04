# Distributed Queries Example

This example demonstrates distributed query execution and result aggregation across a federation of SPARQL endpoints.

## Features

- **Federation Coordinator**: Manage multiple peers in a unified federation
- **Distributed Queries**: Execute SPARQL queries across multiple peers
- **Result Aggregation**: Combine and deduplicate results from multiple sources
- **Fault Tolerance**: Continue querying even when some peers fail
- **Query Routing**: Broadcast, selective, or first-available strategies
- **Health Monitoring**: Periodic health checks with automatic status updates
- **Synchronization**: Coordinate graph updates across federation

## Usage

```bash
# Install dependencies
pnpm install

# Run the example
pnpm start

# Run tests
pnpm test
```

## Example Code

```javascript
import { createCoordinator } from '@unrdf/federation';

// Create federation coordinator
const coordinator = createCoordinator({
  peers: [
    { id: 'movies', endpoint: 'http://localhost:8001/sparql' },
    { id: 'books', endpoint: 'http://localhost:8002/sparql' },
  ],
  strategy: 'broadcast', // Query all peers
  timeout: 30000,
});

// Execute distributed query
const result = await coordinator.query(`
  SELECT ?title WHERE {
    ?item :title ?title .
  } LIMIT 10
`);

console.log('Results:', result.results);
console.log('Success rate:', result.successCount, '/', result.peerResults.length);
```

## Query Strategies

### Broadcast (Default)
Query all healthy peers and aggregate results:
```javascript
const result = await coordinator.query(sparql, { strategy: 'broadcast' });
```

### First Available
Query only the first healthy peer:
```javascript
const result = await coordinator.query(sparql, { strategy: 'first-available' });
```

### Selective
Query only peers matching certain criteria:
```javascript
const result = await coordinator.query(sparql, { strategy: 'selective' });
```

## Result Aggregation

The coordinator automatically:
- Combines results from all successful peers
- Removes duplicate bindings
- Preserves all unique data
- Handles different result formats (SPARQL JSON, arrays, objects)

## Fault Handling

```javascript
// Federation continues even if some peers fail
const result = await coordinator.query(sparql);

console.log('Successful:', result.successCount);
console.log('Failed:', result.failureCount);
console.log('Results:', result.results); // Only successful results
```

## Health Monitoring

```javascript
// Manual health check
const health = await coordinator.healthCheck();
console.log('Healthy peers:', health.healthyPeers);

// Periodic health checks
coordinator.startHealthChecks(); // Runs every 60 seconds

// Stop monitoring
coordinator.stopHealthChecks();
```

## Statistics

```javascript
const stats = coordinator.getStats();
console.log('Total queries:', stats.totalQueries);
console.log('Error rate:', stats.errorRate);
console.log('Healthy peers:', stats.healthyPeers);
```

## Graph Synchronization

For write operations across the federation:

1. **Broadcast** UPDATE query to all peers
2. **Wait** for acknowledgment from quorum (majority)
3. **Commit** if successful, **rollback** on failure
4. **Update** peer status based on sync result

```javascript
// Conceptual workflow
const updateResult = await coordinator.query(`
  DELETE { ?movie :title "Old Title" }
  INSERT { ?movie :title "New Title" }
  WHERE { ?movie :title "Old Title" }
`);

if (updateResult.successCount >= updateResult.peerResults.length / 2) {
  console.log('Update committed across federation');
} else {
  console.log('Update failed - not enough peers responded');
}
```

## API Reference

### Create Coordinator
```javascript
createCoordinator({
  peers: [{ id, endpoint, metadata }],
  strategy: 'broadcast' | 'selective' | 'first-available',
  timeout: 30000,
  healthCheckInterval: 60000,
})
```

### Add/Remove Peers
```javascript
await coordinator.addPeer(id, endpoint, metadata);
coordinator.removePeer(id);
```

### Query Operations
```javascript
// Distributed query
await coordinator.query(sparql, { strategy, timeout, format });

// Single peer query
await coordinator.queryPeer(peerId, sparql, { timeout, format });
```

### Health & Stats
```javascript
await coordinator.healthCheck();
coordinator.getStats();
coordinator.startHealthChecks();
coordinator.stopHealthChecks();
```

## Learn More

- [Federation Documentation](../../README.md)
- [Peer Discovery Example](../peer-discovery/)
- [UNRDF Core](../../../core/)


## Testing

Run the test suite:

```bash
pnpm test
```

Run tests in watch mode:

```bash
pnpm test:watch
```

Generate coverage report:

```bash
pnpm test:coverage
```

Test coverage: 80%+ (minimum requirement)
