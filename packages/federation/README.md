# @unrdf/federation

![Version](https://img.shields.io/badge/version-5.0.0--beta.1-blue) ![Production Ready](https://img.shields.io/badge/production-ready-green)

**Peer Discovery and Distributed Query Execution**

Federate RDF graphs across multiple peers. Discover peers, route queries, and execute distributed operations with automatic failover.

## Quick Start

See [QUICKSTART-FEDERATION.md](./QUICKSTART-FEDERATION.md) for a 5-minute guide to production federated queries.

**One-command demo:**
```bash
node examples/production-federation.mjs
```

## Features

### Peer Management
- ✅ Peer discovery (DNS-SD, mDNS)
- ✅ Dynamic peer registration/removal
- ✅ Peer metadata and configuration
- ✅ Connection pooling
- ✅ Automatic reconnection

### Query Execution
- ✅ Remote SPARQL execution
- ✅ Multiple query strategies (broadcast, selective, failover)
- ✅ Query routing and optimization
- ✅ Result aggregation
- ✅ Timeout configuration
- ✅ Retry logic

### Health Monitoring
- ✅ Peer health monitoring
- ✅ Automatic health checks
- ✅ Health scores (0-100)
- ✅ Fallback peer selection
- ✅ Degraded peer handling

### Statistics & Monitoring
- ✅ Query statistics tracking
- ✅ Error rate monitoring
- ✅ Performance metrics
- ✅ Per-peer statistics
- ✅ Response time tracking
- ✅ OpenTelemetry integration (metrics, traces, spans)

## Installation

```bash
pnpm add @unrdf/federation
```

## Usage

### Basic Federation

```javascript
import { createCoordinator } from '@unrdf/federation'

// Create a federation coordinator with initial peers
const coordinator = createCoordinator({
  peers: [
    {
      id: 'dbpedia',
      endpoint: 'https://dbpedia.org/sparql',
      metadata: { description: 'DBpedia SPARQL endpoint' }
    },
    {
      id: 'wikidata',
      endpoint: 'https://query.wikidata.org/sparql',
      metadata: { description: 'Wikidata Query Service' }
    }
  ],
  strategy: 'broadcast', // Query all peers
  timeout: 10000 // 10 second timeout
})

// Execute federated query
const result = await coordinator.query(`
  SELECT DISTINCT ?type WHERE {
    ?s a ?type .
  } LIMIT 10
`)

console.log(`Results: ${result.results.length}`)
console.log(`Success: ${result.successCount}/${result.totalPeers}`)
```

### Dynamic Peer Management

```javascript
// Add peer dynamically
await coordinator.addPeer(
  'local',
  'http://localhost:3030/dataset/sparql',
  { description: 'Local Fuseki instance' }
)

// List registered peers
const peers = coordinator.listPeers()
peers.forEach(peer => {
  console.log(`${peer.id}: ${peer.endpoint} (${peer.status})`)
})

// Remove peer
const removed = coordinator.removePeer('local')
console.log(`Removed: ${removed}`)
```

### Query Strategies

#### Broadcast Strategy
Query ALL peers, aggregate results:

```javascript
const result = await coordinator.query(sparqlQuery, {
  strategy: 'broadcast',
  timeout: 10000
})

// result.peerResults contains results from each peer
// result.results contains aggregated/merged results
```

**Use when:**
- Need comprehensive results from all sources
- Want to compare data across peers
- High availability is critical

#### Selective Strategy
Query HEALTHY peers only, skip degraded/unreachable:

```javascript
const result = await coordinator.query(sparqlQuery, {
  strategy: 'selective',
  timeout: 5000
})
```

**Use when:**
- Want optimal performance
- Can tolerate partial results
- Network conditions vary

#### Failover Strategy
Query ONE peer, try next on failure:

```javascript
const result = await coordinator.query(sparqlQuery, {
  strategy: 'failover',
  timeout: 3000
})
```

**Use when:**
- Need single authoritative source
- Want minimal network usage
- Peers have redundant data

### Health Monitoring

```javascript
// Run health check
const health = await coordinator.healthCheck()
console.log(`Healthy: ${health.healthyPeers}/${health.totalPeers}`)
console.log(`Degraded: ${health.degradedPeers}`)
console.log(`Unreachable: ${health.unreachablePeers}`)

// Query specific peer
const peerResult = await coordinator.queryPeer('dbpedia', sparqlQuery)
console.log(`Success: ${peerResult.success}`)
console.log(`Duration: ${peerResult.duration}ms`)
```

### Statistics

```javascript
// Get federation statistics
const stats = coordinator.getStats()
console.log(`Total queries: ${stats.totalQueries}`)
console.log(`Total errors: ${stats.totalErrors}`)
console.log(`Error rate: ${(stats.errorRate * 100).toFixed(2)}%`)
```

## Architecture

```
Federation Coordinator
│
├── Peer Manager
│   ├── Peer Registration
│   ├── Health Tracking
│   ├── Metadata Management
│   └── Connection Pooling
│
├── Distributed Query Engine
│   ├── Query Routing
│   ├── Strategy Selection
│   │   ├── Broadcast (all peers)
│   │   ├── Selective (healthy only)
│   │   └── Failover (single with fallback)
│   ├── Parallel Execution
│   └── Result Aggregation
│
├── Health Monitor
│   ├── Periodic Health Checks
│   ├── Health Score Calculation
│   ├── Degradation Detection
│   └── Auto-Failover
│
└── Statistics Tracker
    ├── Query Metrics
    ├── Error Tracking
    ├── Performance Monitoring
    └── Per-Peer Statistics

Query Flow:
1. Coordinator receives SPARQL query
2. Strategy determines which peers to query
3. Queries executed in parallel across selected peers
4. Results aggregated and returned
5. Health scores updated
6. Statistics recorded
```

### Peer Lifecycle

```
┌─────────────┐
│   INITIAL   │
└──────┬──────┘
       │
       │ addPeer()
       ▼
┌─────────────┐
│   ACTIVE    │◄────────┐
└──────┬──────┘         │
       │                │
       │ healthCheck()  │
       ▼                │
┌─────────────┐         │
│  DEGRADED   │─────────┘
└──────┬──────┘  recover
       │
       │ continued failure
       ▼
┌─────────────┐
│ UNREACHABLE │
└──────┬──────┘
       │
       │ removePeer()
       ▼
┌─────────────┐
│   REMOVED   │
└─────────────┘
```

## API Reference

### `createCoordinator(config)`

Creates a federation coordinator.

**Parameters:**
- `config.peers` (Array): Initial peers
  - `id` (string): Unique peer identifier
  - `endpoint` (string): SPARQL endpoint URL
  - `metadata` (object, optional): Peer metadata
- `config.strategy` (string): Default query strategy ('broadcast', 'selective', 'failover')
- `config.timeout` (number): Default timeout in milliseconds
- `config.healthCheckInterval` (number, optional): Health check interval (default: 60000ms)
- `config.retryAttempts` (number, optional): Retry attempts for failed queries (default: 3)
- `config.retryDelay` (number, optional): Delay between retries (default: 1000ms)

**Returns:** Coordinator instance

### Coordinator Methods

#### `addPeer(id, endpoint, metadata?)`
Adds a new peer to the federation.

#### `removePeer(id)`
Removes a peer from the federation.

#### `listPeers()`
Returns array of all registered peers.

#### `query(sparqlQuery, options?)`
Executes a federated SPARQL query.

**Options:**
- `strategy`: Query strategy ('broadcast', 'selective', 'failover')
- `timeout`: Query timeout in milliseconds

**Returns:** Query result object with:
- `success`: boolean
- `results`: Array of bindings
- `successCount`: Number of successful peers
- `failureCount`: Number of failed peers
- `totalDuration`: Total query duration
- `peerResults`: Per-peer result details

#### `queryPeer(peerId, sparqlQuery)`
Queries a specific peer.

#### `healthCheck()`
Runs health checks on all peers.

**Returns:** Health status object with:
- `totalPeers`: Total number of peers
- `healthyPeers`: Number of healthy peers
- `degradedPeers`: Number of degraded peers
- `unreachablePeers`: Number of unreachable peers

#### `getStats()`
Returns federation statistics.

**Returns:** Statistics object with:
- `totalQueries`: Total queries executed
- `totalErrors`: Total errors encountered
- `errorRate`: Error rate (0-1)
- `totalDuration`: Total query duration

## Use Cases

### Multi-graph Queries
Query across multiple RDF sources:

```javascript
const result = await coordinator.query(`
  SELECT ?person ?name ?birthPlace WHERE {
    ?person foaf:name ?name .
    ?person dbo:birthPlace ?birthPlace .
  } LIMIT 100
`, { strategy: 'broadcast' })
```

### Distributed Systems
Coordinate RDF operations across distributed nodes:

```javascript
// Add organizational peers
await coordinator.addPeer('hr', 'http://hr.example.com/sparql')
await coordinator.addPeer('finance', 'http://finance.example.com/sparql')

// Query across departments
const employees = await coordinator.query(`
  SELECT ?emp ?dept ?salary WHERE {
    ?emp org:department ?dept .
    ?emp org:salary ?salary .
  }
`, { strategy: 'selective' })
```

### Data Federation
Combine datasets from multiple sources:

```javascript
// Public knowledge bases
await coordinator.addPeer('dbpedia', 'https://dbpedia.org/sparql')
await coordinator.addPeer('wikidata', 'https://query.wikidata.org/sparql')

// Federated query across public + private data
const combined = await coordinator.query(`
  SELECT ?entity ?label ?description WHERE {
    ?entity rdfs:label ?label .
    OPTIONAL { ?entity schema:description ?description }
  }
`, { strategy: 'broadcast' })
```

### High Availability
Route to healthy peers automatically:

```javascript
// Failover automatically if primary is down
const result = await coordinator.query(sparqlQuery, {
  strategy: 'failover'
})
```

## Performance Characteristics

| Metric | Value | Notes |
|--------|-------|-------|
| **Query Latency** | <100ms overhead | Federation coordination |
| **Peer Timeout** | 10s default | Configurable per query |
| **Health Check** | 60s interval | Background monitoring |
| **Failover Time** | <1s | Automatic on failure |
| **Max Peers** | Unlimited | Limited by resources |
| **Parallel Queries** | N peers | Concurrent execution |

## Troubleshooting

### Cannot connect to peer

**Symptom**: `ECONNREFUSED` or timeout errors

**Solution**:
1. Check peer endpoint URL is correct
2. Verify network connectivity: `curl <endpoint>`
3. Check firewall/CORS settings
4. Increase timeout: `{ timeout: 30000 }`

### Query returns empty results

**Symptom**: `results.length === 0` but no errors

**Solution**:
1. Test query on individual peer: `coordinator.queryPeer('peer-id', query)`
2. Check SPARQL syntax
3. Verify peer has data
4. Check peer endpoint accepts HTTP POST

### Slow query performance

**Symptom**: Queries taking >10 seconds

**Solution**:
1. Use `strategy: 'selective'` to skip unhealthy peers
2. Reduce query complexity
3. Add LIMIT clause
4. Query specific peer instead of broadcast

### Too many failed peers

**Symptom**: `health.unreachablePeers > health.healthyPeers`

**Solution**:
1. Run health check: `await coordinator.healthCheck()`
2. Remove unreachable peers
3. Check peer endpoints are accessible
4. Review peer configuration

## Examples

Complete examples are available in the [examples/](./examples/) directory:

- **[basic.mjs](./examples/basic.mjs)** - Basic federation usage (15 min)
- **[production-federation.mjs](./examples/production-federation.mjs)** - Production-ready example with all features (20 min)
- **[distributed-queries/](./examples/distributed-queries/)** - Advanced distributed query patterns
- **[peer-discovery/](./examples/peer-discovery/)** - Peer discovery and management

**New to federation?** Start with [QUICKSTART-FEDERATION.md](./QUICKSTART-FEDERATION.md).

## Observability

The federation package includes built-in OpenTelemetry instrumentation for production monitoring:

```javascript
import { createCoordinator } from '@unrdf/federation'

const coordinator = createCoordinator({
  peers: [...],
  observability: {
    serviceName: 'my-federation',
    version: '1.0.0'
  }
})

// Metrics automatically tracked:
// - federation.queries (counter)
// - federation.errors (counter)
// - federation.query_duration (histogram)
// - federation.peer_health (up/down counter)
// - federation.concurrent_queries (gauge)
```

**Note**: Metrics are exported via `@opentelemetry/api` and can be collected by any OTEL-compatible backend (Prometheus, Jaeger, etc.).

## Dependencies

- `@unrdf/core` - RDF substrate
- `@unrdf/hooks` - Policy enforcement
- `@opentelemetry/api` - Observability instrumentation

## Browser Compatibility

- **Node.js**: 18+ ✅
- **Browser**: Not yet supported (planned)

## VOC Usage

- VOC-1: Knowledge Agent (discover agent peers)
- VOC-2: Sync Agent (peer discovery and sync)
- VOC-5: Data Engineer (federate data sources)

## Documentation

Complete documentation is organized using the [Diataxis](https://diataxis.fr/) framework:

- **Quick Start**: [QUICKSTART-FEDERATION.md](./QUICKSTART-FEDERATION.md) - Get started in 5 minutes
- **Examples**: [examples/](./examples/) - Code examples
- **API Reference**: This README - Complete API documentation
- **User Guide**: See monorepo [examples/](../../examples/) - Federation patterns

## Development

```bash
# Run tests
pnpm test

# Run tests in watch mode
pnpm test:watch

# Lint
pnpm lint

# Format
pnpm format
```

## Requirements

- Node.js >= 18.0.0
- pnpm >= 8.0.0
- Network access to SPARQL endpoints

## License

MIT
