# Daemon Federation Query Integration Guide

## Overview

The Daemon Federation Query Integration enables distributed SPARQL query execution across federated Raft nodes with intelligent scheduling, result aggregation, and automatic failure recovery. This guide covers topology design, query planning, consistency guarantees, and fault tolerance mechanisms.

## Core Concepts

### Distributed Query Execution

Queries are executed across multiple federation nodes in parallel or sequential modes, with results aggregated and deduplicated. The executor maintains topology awareness and selects nodes based on performance metrics.

**Key Components:**
- **DaemonFederationExecutor**: Coordinates query execution across nodes
- **Node Selection**: Strategies for choosing which nodes to query
- **Result Aggregation**: Combines results with deduplication
- **Metrics Tracking**: Monitors node performance and query statistics
- **Failure Recovery**: Automatically fallback when nodes become unavailable

### Federation Topology

The daemon operates within a distributed federation of peers, each representing a SPARQL endpoint in the Raft consensus cluster.

```
┌─────────────────────────────────────────────────────────────┐
│ Daemon Cluster                                              │
├─────────────────────────────────────────────────────────────┤
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │ Node 1       │  │ Node 2       │  │ Node 3       │      │
│  │ (SPARQL)     │  │ (SPARQL)     │  │ (SPARQL)     │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
│        ↑                ↑                    ↑               │
│        └────────────────┼────────────────────┘               │
│                   Federation Query                           │
│                   Executor                                   │
│                         ↑                                    │
│                 DaemonFederationExecutor                     │
└─────────────────────────────────────────────────────────────┘
```

## Query Execution Strategies

### 1. Broadcast Strategy

Executes query on all available nodes and aggregates results.

**Use Cases:**
- Comprehensive result collection from all sources
- Ensuring no data is missed
- Consistency verification across nodes

**Example:**
```javascript
const result = await executor.executeQuery(
  'SELECT ?person ?name WHERE { ?person foaf:name ?name }',
  { strategy: 'broadcast' }
);
```

**Characteristics:**
- Query Count: All healthy nodes
- Latency: Slowest node determines total time
- Consistency: Results merged with deduplication
- Failure Impact: Partial results available even if some nodes fail

### 2. Selective Strategy

Executes query on a subset of best-performing nodes (default 50%).

**Use Cases:**
- Balancing latency and completeness
- Most queries in production scenarios
- Cost-effective execution on large federations

**Example:**
```javascript
const result = await executor.executeQuery(
  'SELECT ?x WHERE { ?x ?p ?o }',
  { strategy: 'selective' }
);
```

**Characteristics:**
- Query Count: ~50% of healthy nodes
- Latency: Faster than broadcast
- Consistency: Results from multiple nodes reduce missing data risk
- Failure Impact: High resilience with fallback nodes available

### 3. Best-Node Strategy

Executes query on single best-performing node.

**Use Cases:**
- Minimizing latency for interactive queries
- Reducing load on federation
- Single-source-of-truth scenarios

**Example:**
```javascript
const result = await executor.executeQuery(
  'SELECT ?person WHERE { ?person rdf:type foaf:Person }',
  { strategy: 'best-node' }
);
```

**Characteristics:**
- Query Count: Single node
- Latency: Minimal
- Consistency: Single source (may be stale if not replicated)
- Failure Impact: No fallback available, query fails if node down

## Node Selection Heuristics

The executor selects nodes based on performance metrics that accumulate over time.

### Selection Criteria

1. **Success Rate**: Percentage of successful queries
2. **Average Duration**: Mean query execution time
3. **Health Status**: Current node availability

### Selection Algorithm

For non-best-node strategies, the executor scores nodes as:

```
score = successRate / (1 + avgDuration/1000)
```

Higher score indicates better performing node.

**For Selective Strategy:**
1. Filter healthy nodes (status = 'healthy')
2. Score each node
3. Select top 50% by score

**For Best-Node Strategy:**
1. Filter healthy nodes
2. Return single highest-scoring node

### Metric Tracking

Metrics update after each query execution:

```javascript
{
  queryCount: 5,           // Total queries executed
  successCount: 5,         // Successful queries
  totalDuration: 15000,    // Total execution time (ms)
  successRate: 1.0,        // successCount / queryCount
  avgDuration: 3000        // totalDuration / queryCount (ms)
}
```

## Result Aggregation and Consistency

### Aggregation Process

1. **Collection**: Gather results from all queried nodes
2. **Extraction**: Convert different formats to unified structure
3. **Deduplication**: Remove duplicate results across nodes
4. **Metadata**: Track source nodes for each result

### Deduplication

Results are considered identical if their non-metadata content matches exactly:

```javascript
{
  id: 'x1',
  name: 'Alice',
  _sources: ['node-0', 'node-1', 'node-2'],  // Metadata
  _replicaCount: 3                            // Metadata
}
```

**Deduplication Toggle:**
```javascript
const executor = new DaemonFederationExecutor(daemon, coordinator, {
  deduplicateResults: true  // Enable (default: true)
});
```

### Replica Information

Each result includes source tracking:

- `_sources`: Array of node IDs that returned this result
- `_replicaCount`: Number of replicas found

**Use Cases:**
- Verify data replication across nodes
- Detect missing replicas
- Monitor consistency

## Consistency Guarantees

### Eventual Consistency

The federation provides **eventual consistency** with the following guarantees:

1. **Replica Visibility**: Results from majority of nodes visible in results
2. **Stable Results**: Multiple queries return consistent base results
3. **Deduplication**: Identical results merged into single entry
4. **Temporal Ordering**: Results ordered by execution start time

### Consistency Boundaries

**Strong Consistency (within single query):**
- All successful nodes queried simultaneously
- Results aggregated from same query execution
- Timestamp indicates execution time

**Weak Consistency (across queries):**
- Different queries may execute at different times
- Node data may change between queries
- Use broadcast strategy for consistent snapshot

## Fault Tolerance

### Node Failure Handling

**Automatic Detection:**
- Failed queries marked as unsuccessful
- Node status updated to 'unreachable'
- Future queries exclude failing nodes

**Graceful Degradation:**
```javascript
// Query executes on remaining nodes
const result = await executor.executeQuery(query, {
  strategy: 'broadcast',
  excludeNodes: ['node-0']  // Exclude known-bad node
});

// Result still available from other nodes
console.log(result.successCount);   // >= 1
console.log(result.aggregatedResults.length); // >= 1
```

### Recovery Protocol

When a node recovers:

1. Health check detects availability
2. Node status changes to 'healthy'
3. Future queries include recovered node
4. Metrics reset to initial state

```javascript
// Node recovers online
coordinator.addPeer('node-0', 'http://node-0.local:8080');

// Subsequent queries include node-0 again
const result = await executor.executeQuery(query);
// node-0 participates again
```

### Failure Scenarios

| Scenario | Broadcast | Selective | Best-Node |
|----------|-----------|-----------|-----------|
| 1 of 3 fails | Partial results | Partial results | Full failure |
| 2 of 3 fail | Partial results | Partial results | Full failure |
| All fail | Error thrown | Error thrown | Error thrown |

## Configuration

### Executor Configuration

```javascript
const executor = new DaemonFederationExecutor(daemon, coordinator, {
  // Execution strategy: 'broadcast', 'selective', 'best-node'
  strategy: 'selective',

  // Default timeout per query (milliseconds)
  timeout: 30000,

  // Maximum retry attempts for failed queries
  maxRetries: 2,

  // Enable result deduplication
  deduplicateResults: true,

  // Enable intelligent node selection
  enableNodeSelection: true,

  // Health check threshold (0.0-1.0)
  healthCheckThreshold: 0.7
});
```

### Per-Query Options

```javascript
const result = await executor.executeQuery(sparqlQuery, {
  // Override default strategy
  strategy: 'best-node',

  // Override default timeout
  timeout: 10000,

  // Exclude specific nodes
  excludeNodes: ['node-0', 'node-1']
});
```

## Monitoring and Observability

### Query Statistics

Track query execution metrics:

```javascript
// Get statistics for specific query
const stats = executor.getStats(queryId);
console.log(stats);
// {
//   queryId: 'query-...',
//   sparql: 'SELECT ...',
//   strategy: 'selective',
//   nodeCount: 2,
//   successCount: 2,
//   failureCount: 0,
//   totalDuration: 245,
//   startTime: 1234567890,
//   endTime: 1234567890
// }

// Get all query statistics
const allStats = executor.getStats();
console.log(allStats.totalQueries);  // Total queries executed
console.log(allStats.stats);         // Array of all stats
```

### Node Metrics

Monitor per-node performance:

```javascript
// Get metrics for specific node
const nodeMetrics = executor.getNodeMetrics('node-0');
console.log(nodeMetrics);
// {
//   queryCount: 10,
//   successCount: 10,
//   totalDuration: 2500,
//   successRate: 1.0,
//   avgDuration: 250
// }

// Get metrics for all nodes
const allMetrics = executor.getNodeMetrics();
```

### Health Status

Get executor health:

```javascript
const health = executor.getHealth();
console.log(health);
// {
//   executorId: 'executor-...',
//   totalQueries: 42,
//   successfulQueries: 42,
//   failedQueries: 0,
//   nodeCount: 3,
//   averageQueryDuration: 215,
//   timestamp: Date(...)
// }
```

### OpenTelemetry Integration

The executor emits OTEL spans for:

- `federation.executeQuery`: Top-level query execution
- `federation.executeOnNode`: Per-node query execution
- Attributes: query ID, node ID, strategy, duration, success/failure

```javascript
// OTEL spans automatically captured
// Trace context flows through all operations
// Query execution visible in trace UI
```

## Best Practices

### 1. Choose Appropriate Strategy

| Scenario | Recommended Strategy | Reason |
|----------|----------------------|--------|
| Analytics/Reporting | Broadcast | Ensure completeness |
| Production API | Selective | Balance latency/completeness |
| Real-time Dashboard | Best-node | Minimize latency |
| Data Verification | Broadcast | Verify consistency |

### 2. Monitor Node Health

```javascript
// Periodically check node metrics
setInterval(() => {
  const metrics = executor.getNodeMetrics();
  Object.entries(metrics).forEach(([nodeId, m]) => {
    if (m.successRate < 0.8) {
      console.warn(`Low success rate on ${nodeId}: ${m.successRate}`);
    }
  });
}, 60000);
```

### 3. Handle Partial Failures

```javascript
const result = await executor.executeQuery(query, {
  strategy: 'broadcast'
});

if (result.failureCount > 0) {
  console.warn(`${result.failureCount} nodes failed`);
  // Results still available from successful nodes
  console.log(`Got ${result.aggregatedResults.length} results`);
}
```

### 4. Exclude Known-Bad Nodes

```javascript
// Cache known-bad nodes
const badNodes = new Set();

try {
  await executor.executeQuery(query);
} catch (error) {
  // Extract failed nodes and cache them
  badNodes.add(failedNodeId);
}

// Use in next query
await executor.executeQuery(anotherQuery, {
  excludeNodes: Array.from(badNodes)
});
```

### 5. Reset Statistics Periodically

```javascript
// Reset daily to avoid memory bloat
setInterval(() => {
  const health = executor.getHealth();
  console.log('Daily health:', health);
  executor.reset();
}, 24 * 60 * 60 * 1000);
```

## Troubleshooting

### All Queries Failing

1. Check coordinator connectivity
2. Verify peer endpoints are reachable
3. Confirm SPARQL endpoint health

```javascript
const peers = coordinator.listPeers();
console.log('Available peers:', peers);
// Verify all peers have status = 'healthy'
```

### Slow Query Execution

1. Check node metrics for bottlenecks
2. Consider switching to best-node strategy
3. Reduce query scope or timeout

```javascript
const metrics = executor.getNodeMetrics();
// Find slowest node
const slowest = Object.entries(metrics)
  .reduce((a, [id, m], b) =>
    m.avgDuration > b[1].avgDuration ? [id, m] : [a[0], a[1]]
  );
```

### Inconsistent Results

1. Use broadcast strategy to compare all nodes
2. Check replica counts in results
3. Verify no nodes are excluded

```javascript
const result = await executor.executeQuery(query, {
  strategy: 'broadcast'
});

// Check replica distribution
const replicaCounts = result.aggregatedResults.map(r => r._replicaCount);
console.log('Min replicas:', Math.min(...replicaCounts));
console.log('Max replicas:', Math.max(...replicaCounts));
```

## Performance Considerations

### Latency

- **Broadcast**: P95 = sum of all node latencies
- **Selective**: P95 = ~50th percentile of node latencies
- **Best-node**: P95 = best node latency

### Throughput

- Federation executes in parallel
- Selective strategy ~2x throughput vs broadcast
- Best-node strategy minimizes queueing

### Memory

- Result aggregation stores all results in memory
- Large result sets may require pagination
- Reset statistics periodically to prevent growth

## Integration with Daemon Operations

The federation query executor integrates seamlessly with daemon scheduled operations:

```javascript
const executor = new DaemonFederationExecutor(daemon, coordinator);

daemon.schedule({
  id: 'federation-sync',
  name: 'Federated Data Sync',
  handler: async () => {
    const result = await executor.executeQuery(`
      SELECT ?s ?p ?o WHERE {
        ?s ?p ?o
        FILTER(BOUND(?o))
      }
    `);

    console.log(`Synced ${result.aggregatedResults.length} triples from ${result.nodeCount} nodes`);
    return result;
  }
});
```

## Security Considerations

1. **Query Validation**: Queries are validated before execution
2. **Timeout Protection**: All queries have configurable timeouts
3. **Node Authentication**: Use secure endpoints in federation
4. **Result Sanitization**: Results inherit node security context

```javascript
// Set conservative timeout
const executor = new DaemonFederationExecutor(daemon, coordinator, {
  timeout: 5000  // Prevent runaway queries
});
```

## Summary

The Daemon Federation Query Integration provides:

- **Distributed Execution**: Parallel query execution across multiple nodes
- **Smart Selection**: Intelligent node selection based on performance
- **Result Aggregation**: Automatic deduplication and merging
- **Failure Recovery**: Graceful handling of node failures
- **Consistency**: Eventual consistency with replica tracking
- **Observability**: Comprehensive metrics and OpenTelemetry integration

Use this integration to build scalable, resilient distributed RDF query systems across federated Raft clusters.
